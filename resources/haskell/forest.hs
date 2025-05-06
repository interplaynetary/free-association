{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import Data.List (foldl')
import Data.Maybe (catMaybes, isJust)

-------------------
-- Core Data Types --
-------------------
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

-----------------------------
-- Tree Modification API --
-----------------------------
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
    
--------------------
-- Forest Management --
--------------------
addToForest :: Forest -> TreeZipper -> Forest
addToForest forest z = Map.insert (nodeId $ zipperCurrent z) z forest

mergeContributors :: [ContributorIndex] -> ContributorIndex
mergeContributors = foldl' (Map.unionWith (\_ new -> new)) Map.empty

-- Helper operator for safe navigation
(?) :: Maybe a -> a -> a
(?) = flip Maybe.fromMaybe

--------------------
-- Example Usage --
--------------------
exampleForest :: (Forest, ContributorIndex)
exampleForest = (forest, ci)
  where
    -- Create roots
    aliceRoot = createRootNode "alice" "Alice" (Points 100) ["bob"] Nothing
    bobRoot = createRootNode "bob" "Bob" (Points 100) ["alice"] Nothing
    
    -- Build trees
    aliceWithChild = addChild "alice_child" (Points 30) ["bob"] Nothing aliceRoot
    bobWithChild = addChild "bob_child" (Points 40) ["alice"] Nothing bobRoot
    
    -- Create forest
    forest = foldl addToForest Map.empty [aliceWithChild, bobWithChild]
    ci = Map.fromList
        [ ("alice", aliceWithChild)
        , ("bob", bobWithChild)
        ]

main :: IO ()
main = do
    let (forest, ci) = exampleForest
        alice = fromJust $ Map.lookup "alice" forest
        bob = fromJust $ Map.lookup "bob" forest
    
    putStrLn $ "Alice's Fulfillment: " ++ show (fulfilled ci alice)
    putStrLn $ "Bob's Fulfillment: " ++ show (fulfilled ci bob)
    putStrLn $ "Mutual Fulfillment: " ++ show (mutualFulfillment ci alice bob)

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Unexpected Nothing"