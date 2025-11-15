import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, maybeToList, mapMaybe, listToMaybe)
import Control.Monad (foldM)
import Data.Foldable (foldl', foldr')
import Data.Function (on, (&))
import Control.Arrow ((***), (&&&), first, second)

-- Improved type definitions with newtypes for better type safety
newtype Points = Points Int deriving (Eq, Ord, Show)

-- Unwrap Points to get the Int value
getPoints :: Points -> Int
getPoints (Points p) = p

-- Create Points
points :: Int -> Points
points = Points

-- Tree structure for hierarchical recognition model
data Node = Node {
    nodeId :: String,
    nodeName :: String,
    nodePoints :: Points,
    nodeParent :: Maybe Node,
    nodeChildren :: Map.Map String Node,
    nodeTags :: Set.Set String,         -- Tags for categorization (not used in fulfillment calculations)
    nodeContributors :: Set.Set String, -- Contributors are referenced by their ID (used in fulfillment)
    nodeManualFulfillment :: Maybe Float
} deriving (Show, Eq, Ord)

-- Type aliases for indexes
type TagIndex = Map.Map String (Set.Set Node)
type ContributorIndex = Map.Map String (Set.Set Node)
type Indexes = (TagIndex, ContributorIndex)

-- Create a new tree node with validation for manual fulfillment
createNode :: String -> String -> Points -> Maybe Node -> [String] -> [String] -> Maybe Float -> Node
createNode id name points parent tags contributors manualFulfillment = 
    Node {
        nodeId = id,
        nodeName = name,
        nodePoints = points,
        nodeParent = parent,
        nodeChildren = Map.empty,
        nodeTags = Set.fromList tags,
        nodeContributors = Set.fromList contributors,
        nodeManualFulfillment = validateManualFulfillment manualFulfillment
    }
  where
    -- Ensure manual fulfillment is between 0.0 and 1.0
    validateManualFulfillment Nothing = Nothing
    validateManualFulfillment (Just val)
        | val < 0.0 = Just 0.0
        | val > 1.0 = Just 1.0
        | otherwise = Just val

-- Check if a node is a contributor (root node)
isContributor :: Node -> Bool
isContributor = isNothing . nodeParent
  where
    isNothing = null . maybeToList

-- === Standardized Tree Operations ===

-- Apply a function to each node in the tree
mapTree :: (Node -> Node) -> Node -> Node
mapTree f node = 
    let updatedNode = f node
        updatedChildren = Map.map (mapTree f) (nodeChildren updatedNode)
    in updatedNode { nodeChildren = updatedChildren }

-- Fold a tree from the bottom up
foldTree :: (Node -> b -> b) -> b -> Node -> b
foldTree f initial node =
    -- Start with the accumulated value
    let accumulator = initial
        -- Apply the fold to each child node, starting with our initial value
        childrenFold = foldr (\child acc -> foldTree f acc child) accumulator (Map.elems (nodeChildren node))
    -- Finally, apply f to this node and the accumulated result from children
    in f node childrenFold

-- Filter children of a node
filterNodeChildren :: (Node -> Bool) -> Node -> [Node]
filterNodeChildren pred = filter pred . Map.elems . nodeChildren

-- Test if any child satisfies a predicate
anyNodeChild :: (Node -> Bool) -> Node -> Bool
anyNodeChild pred = any pred . Map.elems . nodeChildren

-- Sum a value over the children of a node
sumOverChildren :: (Node -> Float) -> Node -> Float
sumOverChildren f = sum . map f . Map.elems . nodeChildren

-- === End of Standardized Tree Operations ===

-- Safely get instances of a tag
getTagInstances :: TagIndex -> String -> Set.Set Node
getTagInstances tagIndex tagId = Map.findWithDefault Set.empty tagId tagIndex

-- Safely get instances of a contributor
getContributorInstances :: ContributorIndex -> String -> Set.Set Node
getContributorInstances contribIndex contribId = Map.findWithDefault Set.empty contribId contribIndex

-- Safely get a tag node, returns Maybe to handle empty sets
getTagNode :: TagIndex -> String -> Maybe Node
getTagNode tagIndex tagId = 
    let instances = getTagInstances tagIndex tagId
    in listToMaybe (Set.toList instances)

-- Safely get a contributor node
getContributorNode :: ContributorIndex -> String -> Maybe Node
getContributorNode contribIndex contribId =
    let instances = getContributorInstances contribIndex contribId
    in listToMaybe (Set.toList instances)

-- Check if a node is a contribution (has contributors and a parent)
isContribution :: ContributorIndex -> Node -> Bool
isContribution _ node = not (Set.null $ nodeContributors node)
                     && isJust (nodeParent node)

-- Add a node to the tag index
addNodeToTagIndex :: Node -> String -> TagIndex -> TagIndex
addNodeToTagIndex node tagId tagIndex = 
    Map.alter (Just . Set.insert node . fromMaybe Set.empty) tagId tagIndex

-- Add a node to the contributor index
addNodeToContributorIndex :: Node -> String -> ContributorIndex -> ContributorIndex
addNodeToContributorIndex node contribId contribIndex = 
    Map.alter (Just . Set.insert node . fromMaybe Set.empty) contribId contribIndex

-- Remove a node from a tag in the index
removeNodeFromTag :: Node -> String -> TagIndex -> TagIndex
removeNodeFromTag node tagId tagIndex = 
    Map.adjust (Set.delete node) tagId tagIndex

-- Remove a node from a contributor in the index
removeNodeFromContributor :: Node -> String -> ContributorIndex -> ContributorIndex
removeNodeFromContributor node contribId contribIndex = 
    Map.adjust (Set.delete node) contribId contribIndex

-- Add a child to a tree node
addChild :: Indexes -> Node -> String -> String -> Points -> [String] -> [String] -> Maybe Float -> Either String (Node, Indexes)
addChild (tagIndex, contribIndex) parent childId childName childPoints childTags childContributors childManualFulfillment
    | isJust (nodeParent parent) && isContribution contribIndex parent =
        Left $ "Node " ++ nodeName parent ++ " is an instance of a contributor/contribution and cannot have children."
    | otherwise = Right (updatedParent, (updatedTagIndex, updatedContribIndex))
  where
    -- Validate manual fulfillment before creating the child
    validatedManualFulfillment = case childManualFulfillment of
        Just val | val < 0.0 -> Just 0.0
                | val > 1.0 -> Just 1.0
                | otherwise -> Just val
        Nothing -> Nothing
        
    child = createNode childId childName childPoints (Just parent) childTags childContributors validatedManualFulfillment
    updatedParent = parent { nodeChildren = Map.insert childId child (nodeChildren parent) }
    
    -- Update both indexes
    updatedTagIndex = foldr' (\tagId idx -> addNodeToTagIndex child tagId idx) tagIndex childTags
    updatedContribIndex = foldr' (\contribId idx -> addNodeToContributorIndex child contribId idx) contribIndex childContributors

-- Remove a child from a tree node
removeChild :: Indexes -> Node -> String -> (Node, Indexes)
removeChild (tagIndex, contribIndex) parent childId = case Map.lookup childId (nodeChildren parent) of
    Nothing -> (parent, (tagIndex, contribIndex))
    Just child -> 
        let updatedParent = parent { nodeChildren = Map.delete childId (nodeChildren parent) }
            updatedTagIndex = foldr' (\tagId idx -> removeNodeFromTag child tagId idx) tagIndex (Set.toList $ nodeTags child)
            updatedContribIndex = foldr' (\contribId idx -> removeNodeFromContributor child contribId idx) contribIndex (Set.toList $ nodeContributors child)
        in (updatedParent, (updatedTagIndex, updatedContribIndex))

-- Add a tag to a node
addTag :: Indexes -> Node -> String -> (Node, Indexes)
addTag (tagIndex, contribIndex) node tagId
    | Set.member tagId (nodeTags node) = (node, (tagIndex, contribIndex))
    | otherwise = 
        let updatedNode = node { nodeTags = Set.insert tagId (nodeTags node) }
            updatedTagIndex = addNodeToTagIndex node tagId tagIndex
        in (updatedNode, (updatedTagIndex, contribIndex))

-- Add a contributor to a node
addContributor :: Indexes -> Node -> String -> (Node, Indexes)
addContributor (tagIndex, contribIndex) node contribId
    | Set.member contribId (nodeContributors node) = (node, (tagIndex, contribIndex))
    | otherwise = 
        let updatedNode = node { nodeContributors = Set.insert contribId (nodeContributors node) }
            updatedContribIndex = addNodeToContributorIndex node contribId contribIndex
        in (updatedNode, (tagIndex, updatedContribIndex))

-- Remove a tag from a node
removeTag :: Indexes -> Node -> String -> (Node, Indexes)
removeTag (tagIndex, contribIndex) node tagId
    | not (Set.member tagId (nodeTags node)) = (node, (tagIndex, contribIndex))
    | otherwise = 
        let updatedNode = node { nodeTags = Set.delete tagId (nodeTags node) }
            updatedTagIndex = removeNodeFromTag node tagId tagIndex
        in (updatedNode, (updatedTagIndex, contribIndex))

-- Remove a contributor from a node
removeContributor :: Indexes -> Node -> String -> (Node, Indexes)
removeContributor (tagIndex, contribIndex) node contribId
    | not (Set.member contribId (nodeContributors node)) = (node, (tagIndex, contribIndex))
    | otherwise = 
        let updatedNode = node { nodeContributors = Set.delete contribId (nodeContributors node) }
            updatedContribIndex = removeNodeFromContributor node contribId contribIndex
        in (updatedNode, (tagIndex, updatedContribIndex))

-- Get a node's total child points
totalChildPoints :: Node -> Int
totalChildPoints = sum . map (getPoints . nodePoints) . Map.elems . nodeChildren

-- Calculate a node's weight (proportional importance in the tree)
weight :: Node -> Float
weight node = case nodeParent node of
    Nothing -> 1.0  -- Root node has weight 1.0
    Just parent -> 
        let parentTotalPoints = totalChildPoints parent
            nodePointsVal = getPoints (nodePoints node)
        in if parentTotalPoints == 0
           then 0.0
           else (fromIntegral nodePointsVal / fromIntegral parentTotalPoints) * weight parent

-- Calculate a node's share of its parent's total points
shareOfParent :: Node -> Float
shareOfParent node = case nodeParent node of
    Nothing -> 1.0  -- Root node has 100% share
    Just parent ->
        let parentTotalPoints = totalChildPoints parent
            nodePointsVal = getPoints (nodePoints node)
        in if parentTotalPoints == 0
           then 0.0
           else fromIntegral nodePointsVal / fromIntegral parentTotalPoints

-- Check if a node has direct contribution children
hasDirectContributionChild :: ContributorIndex -> Node -> Bool
hasDirectContributionChild contribIndex = 
    anyNodeChild (isContribution contribIndex)

-- Check if a node has non-contribution children
hasNonContributionChild :: ContributorIndex -> Node -> Bool
hasNonContributionChild contribIndex = 
    anyNodeChild (not . isContribution contribIndex)

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: ContributorIndex -> Node -> Float
contributionChildrenWeight contribIndex node = ratio contributionPoints totalPoints
  where
    contributionPoints = sum $ map (getPoints . nodePoints) $ 
                         filterNodeChildren (isContribution contribIndex) node
    totalPoints = totalChildPoints node
    ratio _ 0 = 0.0
    ratio a b = fromIntegral a / fromIntegral b

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: ContributorIndex -> (Node -> Bool) -> Node -> Float
childrenFulfillment contribIndex pred node =
    sum $ map (\child -> fulfilled contribIndex child * shareOfParent child) $
          filterNodeChildren pred node

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: ContributorIndex -> Node -> Float
contributionChildrenFulfillment contribIndex = 
    childrenFulfillment contribIndex (isContribution contribIndex)

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: ContributorIndex -> Node -> Float
nonContributionChildrenFulfillment contribIndex =
    childrenFulfillment contribIndex (not . isContribution contribIndex)

-- Calculate the fulfillment of a node (core recursive function)
fulfilled :: ContributorIndex -> Node -> Float
fulfilled contribIndex node
    -- Leaf nodes
    | Map.null (nodeChildren node) = 
        if isContribution contribIndex node then 1.0 else 0.0
    
    -- Nodes with manual fulfillment and contributor children
    | isJust (nodeManualFulfillment node) && hasDirectContributionChild contribIndex node = 
        let manualFulfillment = min 1.0 $ max 0.0 $ fromMaybe 0.0 (nodeManualFulfillment node)
        in if not (hasNonContributionChild contribIndex node)
           then manualFulfillment
           else 
               let contribWeight = contributionChildrenWeight contribIndex node
                   nonContribFulfillment = nonContributionChildrenFulfillment contribIndex node
               in manualFulfillment * contribWeight + nonContribFulfillment * (1.0 - contribWeight)
    
    -- Default case: weighted sum of all children's fulfillment
    | otherwise = 
        sumOverChildren (\child -> fulfilled contribIndex child * shareOfParent child) node

-- Calculate the desire (unfulfilled need) of a node
desire :: ContributorIndex -> Node -> Float
desire contribIndex = (1.0 -) . fulfilled contribIndex

-- Calculate a node's share of general fulfillment from another contributor
shareOfGeneralFulfillment :: ContributorIndex -> Node -> Node -> Float
shareOfGeneralFulfillment contribIndex node contribNode =
    getContributorInstances contribIndex (nodeId contribNode)
    & Set.filter (\instance_ -> node == instance_ || any ((==) node) (ancestors instance_))
    & Set.toList
    & map calculateInstanceShare
    & sum
  where
    calculateInstanceShare instance_ = 
        let contributorCount = Set.size (nodeContributors instance_)
            fWeight = fulfilled contribIndex instance_ * weight instance_
        in if contributorCount > 0
           then fWeight / fromIntegral contributorCount
           else fWeight
    ancestors n = case nodeParent n of
            Nothing -> []
            Just p -> p : ancestors p

-- Calculate mutual fulfillment between two nodes
mutualFulfillment :: ContributorIndex -> Node -> Node -> Float
mutualFulfillment contribIndex node1 node2 =
    min (shareOfGeneralFulfillment contribIndex node1 node2)
        (shareOfGeneralFulfillment contribIndex node2 node1)

-- Calculate mutual fulfillment distribution
mutualFulfillmentDistribution :: ContributorIndex -> Node -> Map.Map Node Float
mutualFulfillmentDistribution contribIndex node =
    let validContribIds = filter (not . Set.null . getContributorInstances contribIndex) 
                               (Map.keys contribIndex)
        
        getMutualFulfillment contribId = do
            contribNode <- getContributorNode contribIndex contribId
            let mf = mutualFulfillment contribIndex node contribNode
            if mf > 0 then Just (contribNode, mf) else Nothing
            
        contribsWithValues = mapMaybe getMutualFulfillment validContribIds
        
        total = sum $ map snd contribsWithValues
        
        normalize (contribNode, value) = (contribNode, if total > 0 then value / total else 0)
    in Map.fromList $ map normalize contribsWithValues

-- Create an example tree structure for demonstration
exampleTree :: (Node, Indexes)
exampleTree = (finalRoot, (finalTagIndex, finalContribIndex))
  where
    -- Create root node
    root = createNode "root" "Root" (points 0) Nothing [] [] Nothing
    
    -- Create contributor nodes (like Alice, Bob, etc.)
    alice = createNode "alice" "Alice" (points 0) Nothing [] [] Nothing
    bob = createNode "bob" "Bob" (points 0) Nothing [] [] Nothing
    
    -- Setup indexes
    initialTagIndex = Map.empty
    initialContribIndex = Map.empty
    
    contribIndexWithAlice = addNodeToContributorIndex alice "alice" initialContribIndex
    contribIndexWithBoth = addNodeToContributorIndex bob "bob" contribIndexWithAlice
    
    -- Add children to root with different contributors
    (rootWithChild1, (tagIndex1, contribIndex1)) = 
        case addChild (initialTagIndex, contribIndexWithBoth) root "need1" "Need 1" (points 50) [] ["alice"] Nothing of
            Right result -> result
            Left err -> error err
        
    (finalRoot, (finalTagIndex, finalContribIndex)) = 
        case addChild (tagIndex1, contribIndex1) rootWithChild1 "need2" "Need 2" (points 30) [] ["bob"] Nothing of
            Right result -> result
            Left err -> error err

-- Main function to demonstrate tree-based calculations
main :: IO ()
main = do
    putStrLn "Tree-Based Recognition and Fulfillment:"
    let (tree, (_, contribIndex)) = exampleTree
        need1 = fromMaybe (error "Need1 not found") $ Map.lookup "need1" (nodeChildren tree)
        need2 = fromMaybe (error "Need2 not found") $ Map.lookup "need2" (nodeChildren tree)
    
    putStrLn $ "Need 1 Fulfillment: " ++ show (fulfilled contribIndex need1)
    putStrLn $ "Need 2 Fulfillment: " ++ show (fulfilled contribIndex need2)
    putStrLn $ "Root Total Fulfillment: " ++ show (fulfilled contribIndex tree)

