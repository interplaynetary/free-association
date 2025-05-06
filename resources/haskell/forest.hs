{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.List (foldl')
import Data.Maybe (catMaybes, isJust)
import Data.List (maximumBy)

----------------------
-- Core Data Types --
----------------------

newtype Points = Points Int deriving (Eq, Ord, Show)
getPoints (Points p) = p

data Node = Node {
    nodeId :: String,
    nodeName :: String,
    nodePoints :: Points,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float
} deriving (Show, Eq)

data Ctx = Ctx {
    ctxParent :: Node,
    ctxSiblings :: Map.Map String Node,
    ctxAncestors :: [Ctx]
} deriving (Show)

data TreeZipper = TreeZipper {
    zipperCurrent :: Node,
    zipperContext :: Maybe Ctx
} deriving (Show)

type Forest = Map.Map String TreeZipper
type ContributorIndex = Map.Map String TreeZipper

-------------------------
-- Zipper Navigation --
-------------------------

-- Core navigation
enterChild :: String -> TreeZipper -> Maybe TreeZipper
enterChild name z@(TreeZipper current ctx) = 
    case Map.lookup name (nodeChildren current) of
        Just child -> Just $ TreeZipper {
            zipperCurrent = child,
            zipperContext = Just $ Ctx current (Map.delete name $ nodeChildren current) (maybe [] (\c -> [c]) ctx)
        }
        Nothing -> Nothing

exitToParent :: TreeZipper -> Maybe TreeZipper
exitToParent (TreeZipper current (Just (Ctx parent siblings ancestors))) = 
    Just $ TreeZipper {
        zipperCurrent = parent { nodeChildren = Map.insert (nodeId current) current siblings },
        zipperContext = case ancestors of
            [] -> Nothing
            (a:as) -> Just $ a { ctxAncestors = as }
    }
exitToParent _ = Nothing

-- The -: operator for more readable navigation chains
(-:) :: a -> (a -> b) -> b
x -: f = f x

-- Enhanced navigation functions
enterSibling :: String -> TreeZipper -> Maybe TreeZipper
enterSibling name z = exitToParent z >>= enterChild name

goToRoot :: TreeZipper -> TreeZipper
goToRoot z = case exitToParent z of
    Nothing -> z
    Just parent -> goToRoot parent

-- Modify the current node safely
modifyNode :: (Node -> Node) -> TreeZipper -> TreeZipper
modifyNode f z@(TreeZipper current ctx) = z { zipperCurrent = f current }

-- Get all siblings of the current node
getSiblings :: TreeZipper -> [String]
getSiblings z = case exitToParent z of
    Nothing -> []
    Just parent -> Map.keys $ nodeChildren $ zipperCurrent parent

-- Safe navigation with breadcrumbs
type NavigationPath = [String]

followPath :: NavigationPath -> TreeZipper -> Maybe TreeZipper
followPath [] z = Just z
followPath (x:xs) z = enterChild x z >>= followPath xs

-- Get the current path from root
getCurrentPath :: TreeZipper -> NavigationPath
getCurrentPath z = reverse $ getPath z []
  where
    getPath z acc = case exitToParent z of
        Nothing -> acc
        Just parent -> getPath parent (nodeId (zipperCurrent z) : acc)

----------------------------
-- Tree Modification API --
----------------------------
createRootNode :: String -> String -> Points -> [String] -> Maybe Float -> TreeZipper
createRootNode id name pts contribs manual = TreeZipper {
    zipperCurrent = Node {
        nodeId = id,
        nodeName = name,
        nodePoints = pts,
        nodeChildren = Map.empty,
        nodeContributors = Set.fromList contribs,
        nodeManualFulfillment = clampManual manual
    },
    zipperContext = Nothing
}
  where clampManual = fmap (\v -> max 0 (min 1 v))

addChild :: String -> Points -> [String] -> Maybe Float -> TreeZipper -> TreeZipper
addChild name pts contribs manual z@(TreeZipper current ctx) = z {
    zipperCurrent = current {
        nodeChildren = Map.insert name newChild (nodeChildren current)
    }
}
  where newChild = Node {
            nodeId = name,
            nodeName = name,
            nodePoints = pts,
            nodeChildren = Map.empty,
            nodeContributors = Set.fromList contribs,
            nodeManualFulfillment = clampManual manual
        }
        clampManual = fmap (\v -> max 0 (min 1 v))

-------------------------
-- Core Calculations --
-------------------------
weight :: TreeZipper -> Float
weight z = case zipperContext z of
    Nothing -> 1.0
    Just ctx -> 
        let parentZipper = fromJust $ exitToParent z
            parent = zipperCurrent parentZipper
            total = sum (map (getPoints . nodePoints) $ Map.elems (nodeChildren parent))
            currentPoints = getPoints (nodePoints $ zipperCurrent z)
        in if total == 0 
            then 0 
            else (fromIntegral currentPoints / fromIntegral total) * weight parentZipper
            
fulfilled :: ContributorIndex -> TreeZipper -> Float
fulfilled ci z
    | Map.null (nodeChildren $ zipperCurrent z) = 
        if isContribution ci z then 1 else 0
    | Just mf <- nodeManualFulfillment (zipperCurrent z) = mf
    | otherwise = 
        sum [ fulfilled ci child * shareOfParent child 
            | child <- catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)
            ]

shareOfParent :: TreeZipper -> Float
shareOfParent z = case exitToParent z of
    Just parent -> 
        let total = sum (map (getPoints . nodePoints) $ Map.elems (nodeChildren $ zipperCurrent parent))
            currentPoints = getPoints (nodePoints $ zipperCurrent z)
        in if total == 0 
            then 0 
            else fromIntegral currentPoints / fromIntegral total
    Nothing -> 1

isContribution :: ContributorIndex -> TreeZipper -> Bool
isContribution ci z = not (Set.null (nodeContributors $ zipperCurrent z)) && isJust (zipperContext z)

---------------------------
-- Mutual Fulfillment --
---------------------------
shareOfGeneralFulfillment :: ContributorIndex -> TreeZipper -> TreeZipper -> Float
shareOfGeneralFulfillment ci target contributor =
    case Map.lookup (nodeId $ zipperCurrent contributor) ci of
        Nothing -> 0
        Just rootContributor ->
            let contribId = nodeId $ zipperCurrent rootContributor
                -- Only consider contribution nodes (non-root with contributors)
                contributingNodes = filter 
                    (\node -> 
                        Set.member contribId (nodeContributors (zipperCurrent node)) 
                        && isContribution ci node
                    ) 
                    (getAllDescendants target)
                -- Calculate total contribution from these nodes
                total = sum [weight node * fulfilled ci node | node <- contributingNodes]
            in total

getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z = z : concatMap (maybe [] getAllDescendants) childZippers
  where
    childZippers = map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)

mutualFulfillment :: ContributorIndex -> TreeZipper -> TreeZipper -> Float
mutualFulfillment ci a b = 
    min (shareOfGeneralFulfillment ci a b) (shareOfGeneralFulfillment ci b a)

------------------------------
-- Mutual Fulfillment Utils --
------------------------------

-- Get all contributors that mutually fulfill with the current node
getMutualContributors :: ContributorIndex -> TreeZipper -> [(String, Float)]
getMutualContributors ci z = 
    [(nodeId $ zipperCurrent contributor, mutualFulfillment ci z contributor)
    | contributor <- validContributors]
  where
    contributors = nodeContributors $ zipperCurrent z
    validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]

-- Find the path to the highest mutual fulfillment node
findHighestMutualPath :: ContributorIndex -> TreeZipper -> TreeZipper -> Maybe NavigationPath
findHighestMutualPath ci root target = 
    let allPaths = map getCurrentPath $ getAllDescendants root
        pathScores = [(path, scorePath path) | path <- allPaths]
        scorePath path = case followPath path root of
            Nothing -> 0
            Just node -> mutualFulfillment ci node target
    in fmap fst $ maximumByMay (\a b -> compare (snd a) (snd b)) pathScores

-- Helper for safe maximum with empty lists
maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp xs = Just $ maximumBy cmp xs

-- Get all nodes with mutual fulfillment above a threshold
getHighMutualNodes :: ContributorIndex -> TreeZipper -> Float -> [TreeZipper]
getHighMutualNodes ci z threshold = 
    filter (\node -> mutualFulfillment ci z node > threshold) (getAllDescendants z)

-- Calculate mutual fulfillment between all pairs in a forest
calculateForestMutualFulfillment :: ContributorIndex -> Forest -> [(String, String, Float)]
calculateForestMutualFulfillment ci forest = 
    [(id1, id2, mutualFulfillment ci z1 z2)
    | (id1, z1) <- Map.toList forest
    , (id2, z2) <- Map.toList forest
    , id1 < id2  -- Only calculate each pair once
    ]


------------------------------------------
-- Provider-centric share calculation --
------------------------------------------

type ShareMap = Map.Map String Float
type VisitedSet = Set.Set String

distributeShares :: ContributorIndex -> TreeZipper -> Int -> ShareMap
distributeShares ci provider maxDepth = 
    let initialShares = calculateDirectShares ci provider
        (finalShares, _) = foldl' (processDepth ci) (initialShares, Set.empty) [2..maxDepth]
    in finalShares

calculateDirectShares :: ContributorIndex -> TreeZipper -> ShareMap
calculateDirectShares ci provider =
    let contributors = nodeContributors $ zipperCurrent provider
        -- Look up TreeZippers for each contributor ID
        validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]
        totalMutualRecognition = sum [mutualFulfillment ci provider contributor 
                                   | contributor <- validContributors]
    in if totalMutualRecognition == 0 
        then Map.empty
        else Map.fromList 
            [(nodeId $ zipperCurrent contributor, 
              mutualFulfillment ci provider contributor / totalMutualRecognition)
            | contributor <- validContributors]

processDepth :: ContributorIndex -> (ShareMap, VisitedSet) -> Int -> (ShareMap, VisitedSet)
processDepth ci (shares, visited) _ =
    let recipients = Map.keys shares
        (newShares, newVisited) = foldl' (processRecipient ci shares) (shares, visited) recipients
    in (newShares, newVisited)

processRecipient :: ContributorIndex -> ShareMap -> (ShareMap, VisitedSet) -> String -> (ShareMap, VisitedSet)
processRecipient ci shares (currentShares, visited) recipientId =
    case Map.lookup recipientId shares of
        Nothing -> (currentShares, visited)
        Just recipientShare ->
            case Map.lookup recipientId ci of
                Nothing -> (currentShares, visited)
                Just recipient -> 
                    let newVisited = Set.insert recipientId visited
                        connections = nodeContributors $ zipperCurrent recipient
                        unvisitedConnections = Set.filter (\c -> not $ Set.member c visited) connections
                        validConnections = catMaybes [Map.lookup c ci | c <- Set.toList unvisitedConnections]
                        transitiveShares = Map.fromListWith (+)
                            [(nodeId $ zipperCurrent conn, 
                              recipientShare * directShare ci recipient (nodeId $ zipperCurrent conn))
                            | conn <- validConnections]
                    in (Map.unionWith (+) currentShares transitiveShares, newVisited)

-- Calculate direct share between a provider and a recipient
directShare :: ContributorIndex -> TreeZipper -> String -> Float
directShare ci provider recipientId =
    case Map.lookup recipientId ci of
        Nothing -> 0
        Just recipient ->
            let contributors = nodeContributors $ zipperCurrent provider
                validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]
                totalMutualRecognition = sum [mutualFulfillment ci provider contributor 
                                           | contributor <- validContributors]
            in if totalMutualRecognition == 0 
                then 0
                else mutualFulfillment ci provider recipient / totalMutualRecognition

-- Get total share including all transitive paths up to maxDepth
totalShare :: ContributorIndex -> TreeZipper -> String -> Int -> Float
totalShare ci provider recipientId maxDepth =
    Map.findWithDefault 0 recipientId (distributeShares ci provider maxDepth)
    
-----------------------
-- Forest Management --
-----------------------
addToForest :: Forest -> TreeZipper -> Forest
addToForest forest z = Map.insert (nodeId $ zipperCurrent z) z forest

mergeContributors :: [ContributorIndex] -> ContributorIndex
mergeContributors = foldl' (Map.unionWith (\_ new -> new)) Map.empty

-- Helper operator for safe navigation
(?) :: Maybe a -> a -> a
(?) = flip Maybe.fromMaybe

-----------------------
-- Example Usage --
--------------------
exampleForest :: (Forest, ContributorIndex)
exampleForest = (forest, ci)
  where
    -- Create roots with mutual contributors
    aliceRoot = createRootNode "alice" "Alice" (Points 100) ["bob", "charlie"] Nothing
    bobRoot = createRootNode "bob" "Bob" (Points 100) ["alice", "charlie"] Nothing
    charlieRoot = createRootNode "charlie" "Charlie" (Points 100) ["alice", "bob"] Nothing
    
    -- Build trees with children contributing to both others
    aliceWithChild = addChild "alice_child" (Points 30) ["bob", "charlie"] Nothing aliceRoot
    bobWithChild = addChild "bob_child" (Points 40) ["alice", "charlie"] Nothing bobRoot
    charlieWithChild = addChild "charlie_child" (Points 50) ["alice", "bob"] Nothing charlieRoot
    
    -- Create forest and contributor index
    forest = foldl addToForest Map.empty [aliceWithChild, bobWithChild, charlieWithChild]
    ci = Map.fromList
        [ ("alice", aliceWithChild)
        , ("bob", bobWithChild)
        , ("charlie", charlieWithChild)
        ]

main :: IO ()
main = do
    let (forest, ci) = exampleForest
        alice = fromJust $ Map.lookup "alice" forest
        bob = fromJust $ Map.lookup "bob" forest
        charlie = fromJust $ Map.lookup "charlie" forest
    
    -- Print fulfillment values
    putStrLn $ "Alice's Fulfillment: " ++ show (fulfilled ci alice)
    putStrLn $ "Bob's Fulfillment: " ++ show (fulfilled ci bob)
    putStrLn $ "Charlie's Fulfillment: " ++ show (fulfilled ci charlie)
    
    -- Calculate mutual fulfillment between pairs
    putStrLn $ "\nMutual Fulfillment Scores:"
    putStrLn $ "Alice <-> Bob: " ++ show (mutualFulfillment ci alice bob)
    putStrLn $ "Alice <-> Charlie: " ++ show (mutualFulfillment ci alice charlie)
    putStrLn $ "Bob <-> Charlie: " ++ show (mutualFulfillment ci bob charlie)
    
    -- Calculate and display Alice's share distribution
    let maxDepth = 3
        aliceShares = distributeShares ci alice maxDepth
    putStrLn $ "\nAlice's ShareMap (depth " ++ show maxDepth ++ "):"
    mapM_ (\(k,v) -> putStrLn $ "  " ++ k ++ ": " ++ show v) $ Map.toList aliceShares

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Unexpected Nothing"