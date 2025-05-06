{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, unless)
import Data.List (foldl', maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

----------------------
-- Caching System --
----------------------
{-
  UNIVERSAL CACHING STRATEGY:

  This implementation uses a universal caching abstraction that follows DRY principles:

  1. Unified Cache Type:
     - Generic 'Cache k v' data type with hits/misses tracking
     - Common interface for all cache operations (lookup, insert)
     - Eq instance that focuses on cached values, not statistics

  2. Standardized Key Generation:
     - nodeKey: For node-specific calculations (nodeId + suffix)
     - mutualKey: For calculations between pairs of nodes (ordered pair)

  3. Universal Caching Function:
     - withCache: Generic higher-order function for all caching operations
     - Takes computation, cache access, and update functions as parameters
     - Allows consistent caching behavior across all calculation types

  4. Cache Location Strategy:
     - Node-level caches for calculations tied to specific nodes (weight, fulfillment)
     - Mutual caches for calculations between nodes
     - Global cache for expensive network calculations (provider shares)

  5. Cache Statistics:
     - Built-in tracking of hits and misses
     - Statistics reporting for performance monitoring

  This approach ensures:
   - Consistent caching behavior across the codebase
   - Clear separation between computation and caching logic
   - Easy extension to new calculation types
   - Performance monitoring through built-in statistics
-}

-- Generic cache for any calculation
data Cache k v = Cache
  { cacheMap :: Map.Map k v,
    cacheMisses :: Int,
    cacheHits :: Int
  }
  deriving (Show)

-- Eq instance for Cache that only compares the stored values
instance (Eq k, Eq v) => Eq (Cache k v) where
  c1 == c2 = cacheMap c1 == cacheMap c2

-- Initialize an empty cache
emptyCache :: Cache k v
emptyCache = Cache Map.empty 0 0

-- Lookup a value in the cache
cacheLookup :: (Ord k) => k -> Cache k v -> Maybe (v, Cache k v)
cacheLookup key cache =
  case Map.lookup key (cacheMap cache) of
    Just value -> Just (value, cache {cacheHits = cacheHits cache + 1})
    Nothing -> Nothing

-- Insert a value into the cache
cacheInsert :: (Ord k) => k -> v -> Cache k v -> Cache k v
cacheInsert key value cache =
  cache
    { cacheMap = Map.insert key value (cacheMap cache),
      cacheMisses = cacheMisses cache + 1
    }

-- Create a cache key for node-specific calculations
nodeKey :: String -> String -> String
nodeKey nodeId suffix = nodeId ++ "_" ++ suffix

-- Create a cache key for mutual calculations between two nodes
mutualKey :: String -> String -> String
mutualKey nodeId1 nodeId2 =
  if nodeId1 < nodeId2
    then nodeId1 ++ "_" ++ nodeId2
    else nodeId2 ++ "_" ++ nodeId1

-- Generic caching function for any calculation
-- Takes a key, a computation function, and a cache
withCache :: (Ord k) => k -> (a -> v) -> (a -> Cache k v) -> (a -> Cache k v -> Cache k v) -> a -> (v, Cache k v)
withCache key compute getCache updateCache arg =
  let cache = getCache arg
   in case cacheLookup key cache of
        Just (value, updatedCache) -> (value, updatedCache)
        Nothing ->
          let value = compute arg
              newCache = cacheInsert key value cache
           in (value, updateCache arg newCache)

-- Node cache updater - applies cache updates to a node
updateNodeCache :: (Node -> Cache k v -> Node) -> TreeZipper -> Cache k v -> (TreeZipper, Cache k v)
updateNodeCache updater z cache =
  let current = zipperCurrent z
      newNode = updater current cache
      newZipper = z {zipperCurrent = newNode}
   in (newZipper, cache)

----------------------
-- Core Data Types --
----------------------

newtype Points = Points Int deriving (Eq, Ord, Show)

getPoints (Points p) = p

data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Points,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    -- Cache fields using the new Cache type
    nodeWeightCache :: Cache String Float,
    nodeFulfillmentCache :: Cache String Float,
    nodeMutualCache :: Cache String Float,
    nodeTotalPointsCache :: Maybe Int,
    nodeDescendantsCache :: Cache String [String]
  }
  deriving (Show, Eq)

data Ctx = Ctx
  { ctxParent :: Node,
    ctxSiblings :: Map.Map String Node,
    ctxAncestors :: [Ctx]
  }
  deriving (Show)

data TreeZipper = TreeZipper
  { zipperCurrent :: Node,
    zipperContext :: Maybe Ctx
  }
  deriving (Show)

type Forest = Map.Map String TreeZipper

type ContributorIndex = Map.Map String TreeZipper

-------------------------
-- Zipper Navigation --
-------------------------

-- Core navigation
enterChild :: String -> TreeZipper -> Maybe TreeZipper
enterChild name z@(TreeZipper current ctx) =
  case Map.lookup name (nodeChildren current) of
    Just child ->
      Just $
        TreeZipper
          { zipperCurrent = child,
            zipperContext = Just $ Ctx current (Map.delete name $ nodeChildren current) (maybe [] (\c -> [c]) ctx)
          }
    Nothing -> Nothing

exitToParent :: TreeZipper -> Maybe TreeZipper
exitToParent (TreeZipper current (Just (Ctx parent siblings ancestors))) =
  Just $
    TreeZipper
      { zipperCurrent = parent {nodeChildren = Map.insert (nodeId current) current siblings},
        zipperContext = case ancestors of
          [] -> Nothing
          (a : as) -> Just $ a {ctxAncestors = as}
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
modifyNode f z@(TreeZipper current ctx) = z {zipperCurrent = f current}

-- Get all siblings of the current node
getSiblings :: TreeZipper -> [String]
getSiblings z = case exitToParent z of
  Nothing -> []
  Just parent -> Map.keys $ nodeChildren $ zipperCurrent parent

-- Safe navigation with breadcrumbs
type NavigationPath = [String]

followPath :: NavigationPath -> TreeZipper -> Maybe TreeZipper
followPath [] z = Just z
followPath (x : xs) z = enterChild x z >>= followPath xs

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
createRootNode :: String -> String -> Points -> [String] -> Maybe Float -> Node
createRootNode id name pts contribs manual =
  Node
    { nodeId = id,
      nodeName = name,
      nodePoints = pts,
      nodeChildren = Map.empty,
      nodeContributors = Set.fromList contribs,
      nodeManualFulfillment = clampManual manual,
      nodeCapacities = Map.empty,
      nodeCapacityShares = Map.empty,
      -- Cache fields using the new Cache type
      nodeWeightCache = emptyCache,
      nodeFulfillmentCache = emptyCache,
      nodeMutualCache = emptyCache,
      nodeTotalPointsCache = Nothing,
      nodeDescendantsCache = emptyCache
    }
  where
    clampManual = fmap (\v -> max 0 (min 1 v))

-- Add a child and update caches
addChild :: String -> Points -> [String] -> Maybe Float -> TreeZipper -> TreeZipper
addChild name pts contribs manual z@(TreeZipper current ctx) =
  let newChild =
        Node
          { nodeId = name,
            nodeName = name,
            nodePoints = pts,
            nodeChildren = Map.empty,
            nodeContributors = Set.fromList contribs,
            nodeManualFulfillment = clampManual manual,
            nodeCapacities = Map.empty,
            nodeCapacityShares = Map.empty,
            -- Cache fields
            nodeWeightCache = emptyCache,
            nodeFulfillmentCache = emptyCache,
            nodeMutualCache = emptyCache,
            nodeTotalPointsCache = Nothing,
            nodeDescendantsCache = emptyCache
          }
      clampManual = fmap (\v -> max 0 (min 1 v))
      -- Update parent's cached total
      newCachedTotal = case nodeTotalPointsCache current of
        Nothing -> getPoints pts -- No previous cache
        Just oldTotal -> oldTotal + getPoints pts
      -- Clear all parent caches since structure has changed
      updatedCurrent =
        current
          { nodeChildren = Map.insert name newChild (nodeChildren current),
            nodeTotalPointsCache = Just newCachedTotal,
            -- Clear other caches that depend on children
            nodeWeightCache = emptyCache,
            nodeFulfillmentCache = emptyCache,
            nodeMutualCache = emptyCache,
            nodeDescendantsCache = emptyCache -- Clear descendants cache
          }
   in z {zipperCurrent = updatedCurrent}

-------------------------
-- Core Calculations --
-------------------------

-- Calculate total points from all children
totalChildPoints :: TreeZipper -> Int
totalChildPoints z =
  let current = zipperCurrent z
   in case nodeTotalPointsCache current of
        Nothing ->
          -- Cache is empty, calculate and cache result
          let total = sum $ map (getPoints . nodePoints) $ Map.elems (nodeChildren current)
              updatedZ = modifyNode (\n -> n {nodeTotalPointsCache = Just total}) z
           in total
        Just cachedTotal -> cachedTotal

-- Calculate a node's weight (proportional importance in the tree) with caching
weight :: TreeZipper -> Float
weight z =
  let current = zipperCurrent z
      currentId = nodeId current
      key = currentId
      -- Extract the cache
      cache = nodeWeightCache current
      -- Define the computation
      compute _ =
        case zipperContext z of
          Nothing -> 1.0 -- Root node has weight 1.0
          Just ctx ->
            let parentZipper = fromJust $ exitToParent z
                total = totalChildPoints parentZipper
                currentPoints = getPoints (nodePoints current)
             in if total == 0
                  then 0
                  else (fromIntegral currentPoints / fromIntegral total) * weight parentZipper

      -- Define cache update function
      updateCache _ newCache =
        let updatedNode = current {nodeWeightCache = newCache}
            updatedZipper = z {zipperCurrent = updatedNode}
         in (compute z, newCache) -- Return both the value and the updated cache
   in case cacheLookup key cache of
        Just (value, updatedCache) ->
          let updatedNode = current {nodeWeightCache = updatedCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in value
        Nothing ->
          let value = compute z
              newCache = cacheInsert key value cache
              updatedNode = current {nodeWeightCache = newCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in value

shareOfParent :: TreeZipper -> Float
shareOfParent z = case exitToParent z of
  Nothing -> 1.0 -- Root node has 100% share
  Just parent ->
    let total = totalChildPoints parent
        currentPoints = getPoints (nodePoints $ zipperCurrent z)
     in if total == 0
          then 0
          else fromIntegral currentPoints / fromIntegral total

isContribution :: ContributorIndex -> TreeZipper -> Bool
isContribution ci z = not (Set.null (nodeContributors $ zipperCurrent z)) && isJust (zipperContext z)

-- Check if a node has direct contribution children
hasDirectContributionChild :: ContributorIndex -> TreeZipper -> Bool
hasDirectContributionChild ci z = any (isContribution ci) childZippers
  where
    childZippers = catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)

-- Check if a node has non-contribution children
hasNonContributionChild :: ContributorIndex -> TreeZipper -> Bool
hasNonContributionChild ci z = any (not . isContribution ci) childZippers
  where
    childZippers = catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: ContributorIndex -> TreeZipper -> Float
contributionChildrenWeight ci z = ratio contributionPoints total
  where
    childZippers = catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)
    contributionPoints =
      sum $
        map (getPoints . nodePoints . zipperCurrent) $
          filter (isContribution ci) childZippers
    total = totalChildPoints z
    ratio _ 0 = 0.0
    ratio a b = fromIntegral a / fromIntegral b

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: ContributorIndex -> (TreeZipper -> Bool) -> TreeZipper -> Float
childrenFulfillment ci pred z =
  sum
    [ fulfilled ci child * shareOfParent child
      | child <- catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z),
        pred child
    ]

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: ContributorIndex -> TreeZipper -> Float
contributionChildrenFulfillment ci = childrenFulfillment ci (isContribution ci)

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: ContributorIndex -> TreeZipper -> Float
nonContributionChildrenFulfillment ci = childrenFulfillment ci (not . isContribution ci)

-- Safely get instances of a contributor
getContributorInstances :: ContributorIndex -> String -> Set.Set TreeZipper
getContributorInstances contribIndex contribId = Set.singleton $ Map.findWithDefault emptyZipper contribId contribIndex
  where
    emptyNode = createRootNode contribId contribId (Points 0) [] Nothing
    emptyZipper = TreeZipper emptyNode Nothing

-- Safely get a contributor node
getContributorNode :: ContributorIndex -> String -> Maybe TreeZipper
getContributorNode contribIndex contribId =
  let instances = getContributorInstances contribIndex contribId
   in listToMaybe (Set.toList instances)

-- Calculate the fulfillment of a node with caching
fulfilled :: ContributorIndex -> TreeZipper -> Float
fulfilled ci z =
  let current = zipperCurrent z
      currentId = nodeId current
      contribIdStr = show $ Map.keys ci -- Simple hash of contributor index
      key = nodeKey currentId contribIdStr
      cache = nodeFulfillmentCache current

      -- The computation function
      compute _ =
        -- Leaf nodes
        if Map.null (nodeChildren current)
          then if isContribution ci z then 1.0 else 0.0
          else -- Nodes with manual fulfillment and contributor children

            if isJust (nodeManualFulfillment current) && hasDirectContributionChild ci z
              then
                if not (hasNonContributionChild ci z)
                  then fromJust $ nodeManualFulfillment current
                  else
                    let contribWeight = contributionChildrenWeight ci z
                        nonContribFulfillment = nonContributionChildrenFulfillment ci z
                     in (fromJust $ nodeManualFulfillment current) * contribWeight
                          + nonContribFulfillment * (1.0 - contribWeight)
              else -- Default case: weighted sum of all children's fulfillment

                sum
                  [ fulfilled ci child * shareOfParent child
                    | child <- catMaybes $ map (flip enterChild z) (Map.keys $ nodeChildren current)
                  ]
   in -- Check cache
      case cacheLookup key cache of
        Just (value, updatedCache) ->
          let updatedNode = current {nodeFulfillmentCache = updatedCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in value
        Nothing ->
          -- Compute and update cache
          let value = compute ()
              newCache = cacheInsert key value cache
              updatedNode = current {nodeFulfillmentCache = newCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in value

-- Calculate the desire (unfulfilled need) of a node
desire :: ContributorIndex -> TreeZipper -> Float
desire ci z = 1.0 - fulfilled ci z

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
          contributingNodes =
            filter
              ( \node ->
                  Set.member contribId (nodeContributors (zipperCurrent node))
                    && isContribution ci node
              )
              (getAllDescendantsCached target)
          -- Calculate total contribution from these nodes
          total = sum [weight node * fulfilled ci node | node <- contributingNodes]
          -- Get number of contributors for each node
          contributorCounts = map (Set.size . nodeContributors . zipperCurrent) contributingNodes
          -- Divide each node's contribution by its number of contributors
          weightedTotal =
            sum
              [ w * f / fromIntegral c
                | (w, f, c) <-
                    zip3
                      (map weight contributingNodes)
                      (map (fulfilled ci) contributingNodes)
                      contributorCounts
              ]
       in weightedTotal

-- Get all descendants with caching
getAllDescendantsCached :: TreeZipper -> [TreeZipper]
getAllDescendantsCached z =
  let current = zipperCurrent z
      currentId = nodeId current
      key = currentId
      cache = nodeDescendantsCache current

      -- Computation function
      compute _ =
        let descendants = getAllDescendants z
            descendantIds = map (nodeId . zipperCurrent) descendants
         in descendantIds
   in -- Check cache
      case cacheLookup key cache of
        Just (descendantIds, updatedCache) ->
          -- We have the IDs but not the TreeZippers, so we still need to use getAllDescendants
          -- Update the cache for statistics
          let updatedNode = current {nodeDescendantsCache = updatedCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in z : getAllDescendants z
        Nothing ->
          -- Compute and update cache
          let descendantIds = compute ()
              newCache = cacheInsert key descendantIds cache
              updatedNode = current {nodeDescendantsCache = newCache}
              updatedZipper = z {zipperCurrent = updatedNode}
           in z : getAllDescendants z

getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z = z : concatMap (maybe [] getAllDescendants) childZippers
  where
    childZippers = map (flip enterChild z) (Map.keys $ nodeChildren $ zipperCurrent z)

-- Calculate mutual fulfillment between two nodes with caching
mutualFulfillment :: ContributorIndex -> TreeZipper -> TreeZipper -> Float
mutualFulfillment ci a b =
  let aNode = zipperCurrent a
      bNode = zipperCurrent b
      aId = nodeId aNode
      bId = nodeId bNode
      key = mutualKey aId bId
      aCache = nodeMutualCache aNode
      bCache = nodeMutualCache bNode

      -- The computation function
      compute _ =
        let aToB = shareOfGeneralFulfillment ci a b
            bToA = shareOfGeneralFulfillment ci b a
         in min aToB bToA
   in -- Check both nodes' caches
      case cacheLookup key aCache of
        Just (value, updatedACache) ->
          -- Update a's cache hit counter
          let updatedA = a {zipperCurrent = aNode {nodeMutualCache = updatedACache}}
           in value
        Nothing ->
          -- Check b's cache
          case cacheLookup key bCache of
            Just (value, updatedBCache) ->
              -- Update b's cache hit counter
              let updatedB = b {zipperCurrent = bNode {nodeMutualCache = updatedBCache}}
               in value
            Nothing ->
              -- Compute the value and update both caches
              let value = compute ()
                  newACache = cacheInsert key value aCache
                  newBCache = cacheInsert key value bCache
                  updatedA = a {zipperCurrent = aNode {nodeMutualCache = newACache}}
                  updatedB = b {zipperCurrent = bNode {nodeMutualCache = newBCache}}
               in value

------------------------------
-- Mutual Fulfillment Utils --
------------------------------

-- Get all contributors that mutually fulfill with the current node
getMutualContributors :: ContributorIndex -> TreeZipper -> [(String, Float)]
getMutualContributors ci z =
  [ (nodeId $ zipperCurrent contributor, mutualFulfillment ci z contributor)
    | contributor <- validContributors
  ]
  where
    contributors = nodeContributors $ zipperCurrent z
    validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]

-- Find the path to the highest mutual fulfillment node
findHighestMutualPath :: ContributorIndex -> TreeZipper -> TreeZipper -> Maybe NavigationPath
findHighestMutualPath ci root target =
  let allPaths = map getCurrentPath $ getAllDescendantsCached root
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
  filter (\node -> mutualFulfillment ci z node > threshold) (getAllDescendantsCached z)

-- Calculate mutual fulfillment between all pairs in a forest
calculateForestMutualFulfillment :: ContributorIndex -> Forest -> [(String, String, Float)]
calculateForestMutualFulfillment ci forest =
  [ (id1, id2, mutualFulfillment ci z1 z2)
    | (id1, z1) <- Map.toList forest,
      (id2, z2) <- Map.toList forest,
      id1 < id2 -- Only calculate each pair once
  ]

------------------------------------------
-- Provider-centric share calculation --
------------------------------------------

type ShareMap = Map.Map String Float

type VisitedSet = Set.Set String

-- Global cache for provider shares
data ProviderSharesCache = ProviderSharesCache
  { sharesCache :: Cache (String, Int) ShareMap -- (providerId, depth) -> shares
  }

-- Initialize an empty provider shares cache
emptyProviderSharesCache :: ProviderSharesCache
emptyProviderSharesCache = ProviderSharesCache emptyCache

-- Provider-centric share calculation with caching
providerSharesCached :: ProviderSharesCache -> ContributorIndex -> TreeZipper -> Int -> (ShareMap, ProviderSharesCache)
providerSharesCached cache ci provider maxDepth =
  let providerId = nodeId $ zipperCurrent provider
      key = (providerId, maxDepth)
      currentCache = sharesCache cache
   in -- Check cache
      case cacheLookup key currentCache of
        Just (shares, updatedCache) ->
          (shares, cache {sharesCache = updatedCache})
        Nothing ->
          -- Calculate shares
          let shares = providerShares ci provider maxDepth
              newCache = cacheInsert key shares currentCache
           in (shares, cache {sharesCache = newCache})

-- Function to get cache statistics
getCacheStats :: ProviderSharesCache -> (Int, Int, Int)
getCacheStats cache =
  let c = sharesCache cache
      hits = cacheHits c
      misses = cacheMisses c
      total = hits + misses
   in (hits, misses, total)

-- Helper function to print cache statistics
printCacheStats :: ProviderSharesCache -> IO ()
printCacheStats cache = do
  let (hits, misses, total) = getCacheStats cache
  putStrLn $ "Cache Statistics:"
  putStrLn $ "  Total lookups: " ++ show total
  putStrLn $ "  Cache hits: " ++ show hits ++ " (" ++ show (percentage hits total) ++ "%)"
  putStrLn $ "  Cache misses: " ++ show misses ++ " (" ++ show (percentage misses total) ++ "%)"
  where
    percentage _ 0 = 0
    percentage n total = round $ (fromIntegral n / fromIntegral total) * 100

-- Original function without IO for compatibility
providerShares :: ContributorIndex -> TreeZipper -> Int -> ShareMap
providerShares ci provider maxDepth =
  let initialShares = calculateDirectShares ci provider
      (finalShares, _) = foldl' (processDepth ci) (initialShares, Set.empty) [2 .. maxDepth]
      -- Normalize the final shares to ensure they add up to 100%
      totalShare = sum $ Map.elems finalShares
   in if totalShare > 0
        then Map.map (\share -> share / totalShare) finalShares
        else finalShares

calculateDirectShares :: ContributorIndex -> TreeZipper -> ShareMap
calculateDirectShares ci provider =
  let contributors = nodeContributors $ zipperCurrent provider
      -- Look up TreeZippers for each contributor ID
      validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]
      -- Get mutual fulfillment values
      mutualValues =
        [ ( nodeId $ zipperCurrent contributor,
            mutualFulfillment ci provider contributor
          )
          | contributor <- validContributors
        ]
      -- Calculate total mutual recognition for normalization
      totalMutualRecognition = sum $ map snd mutualValues
   in if totalMutualRecognition == 0
        then Map.empty
        else
          Map.fromList
            [ (contribId, mutualValue / totalMutualRecognition)
              | (contribId, mutualValue) <- mutualValues
            ]

processDepth :: ContributorIndex -> (ShareMap, VisitedSet) -> Int -> (ShareMap, VisitedSet)
processDepth ci (shares, visited) _ =
  -- Create a new map for this depth's calculations
  let recipients = Map.keys shares
      unvisitedRecipients = filter (\r -> not $ Set.member r visited) recipients

      -- Process each unvisited recipient
      processRecipient :: (ShareMap, VisitedSet) -> String -> (ShareMap, VisitedSet)
      processRecipient (currentShares, currentVisited) recipientId =
        case Map.lookup recipientId currentShares of
          Nothing -> (currentShares, currentVisited)
          Just recipientShare ->
            case Map.lookup recipientId ci of
              Nothing -> (currentShares, currentVisited)
              Just recipient ->
                let newVisited = Set.insert recipientId currentVisited
                    -- Process this recipient's connections
                    connections = nodeContributors $ zipperCurrent recipient
                    unvisitedConnections = Set.filter (\c -> not $ Set.member c currentVisited) connections
                    validConnections = catMaybes [Map.lookup c ci | c <- Set.toList unvisitedConnections]

                    -- Calculate direct shares from this recipient
                    directShares = calculateDirectShares ci recipient

                    -- Calculate transitive shares (recipient's share * connection's direct share)
                    transitiveShares =
                      Map.mapWithKey
                        (\connId connShare -> recipientShare * connShare)
                        directShares
                 in (Map.unionWith (+) currentShares transitiveShares, newVisited)

      -- Apply processing to all unvisited recipients
      (newShares, newVisited) = foldl' processRecipient (shares, visited) unvisitedRecipients
   in (newShares, newVisited)

-- Calculate direct share between a provider and a recipient
directShare :: ContributorIndex -> TreeZipper -> String -> Float
directShare ci provider recipientId =
  case Map.lookup recipientId ci of
    Nothing -> 0
    Just recipient ->
      let contributors = nodeContributors $ zipperCurrent provider
          validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]
          mutualValues =
            [ mutualFulfillment ci provider contributor
              | contributor <- validContributors
            ]
          totalMutualRecognition = sum mutualValues
       in if totalMutualRecognition == 0
            then 0
            else mutualFulfillment ci provider recipient / totalMutualRecognition

-- Get a receiver's share from a specific capacity provider
receiverShareFrom :: ContributorIndex -> TreeZipper -> TreeZipper -> Capacity -> Int -> Float
receiverShareFrom ci receiver provider capacity maxDepth =
  let providerShareMap = providerShares ci provider maxDepth
      receiverId = nodeId $ zipperCurrent receiver
   in Map.findWithDefault 0 receiverId providerShareMap

-- Get a person's total share in a specific capacity
getPersonalCapacityShare :: ContributorIndex -> Forest -> TreeZipper -> Capacity -> Float
getPersonalCapacityShare ci forest person capacity =
  -- Find all owners of this capacity
  let capacityOwners =
        [ owner | (_, owner) <- Map.toList forest, Map.member (capacityId capacity) (nodeCapacities $ zipperCurrent owner)
        ]
      -- Calculate direct shares from each owner using provider-centric calculation
      directShares = [receiverShareFrom ci person owner capacity 2 | owner <- capacityOwners]
      -- Take the maximum share (person might have shares from multiple owners)
      totalShare = if null directShares then 0 else maximum directShares
   in totalShare

-- Update computed quantities for all capacity shares in a node
updateComputedQuantities :: TreeZipper -> TreeZipper
updateComputedQuantities z =
  z
    { zipperCurrent =
        (zipperCurrent z)
          { nodeCapacityShares = Map.map updateShare (nodeCapacityShares $ zipperCurrent z)
          }
    }
  where
    updateShare share =
      share
        { computedQuantity = computeQuantityShare (targetCapacity share) (sharePercentage share)
        }

-----------------------
-- Capacity Types --
-----------------------

data RecurrenceUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

data RecurrenceEnd
  = Never
  | EndsOn UTCTime
  | EndsAfter Int
  deriving (Show, Eq)

data CustomRecurrence = CustomRecurrence
  { repeatEvery :: Int,
    repeatUnit :: RecurrenceUnit,
    recurrenceEnd :: RecurrenceEnd
  }
  deriving (Show, Eq)

data LocationType
  = Undefined
  | LiveLocation
  | Specific
  deriving (Show, Eq)

data SpaceTimeCoordinates = SpaceTimeCoordinates
  { locationType :: LocationType,
    allDay :: Bool,
    recurrence :: Maybe String,
    customRecurrence :: Maybe CustomRecurrence,
    startDate :: UTCTime,
    startTime :: UTCTime,
    endDate :: UTCTime,
    endTime :: UTCTime,
    timeZone :: String
  }
  deriving (Show, Eq)

data MaxDivisibility = MaxDivisibility
  { naturalDiv :: Int,
    percentageDiv :: Float
  }
  deriving (Show, Eq)

data Capacity = Capacity
  { capacityId :: String,
    capacityName :: String,
    quantity :: Int,
    unit :: String,
    shareDepth :: Int,
    expanded :: Bool,
    coordinates :: SpaceTimeCoordinates,
    maxDivisibility :: MaxDivisibility,
    hiddenUntilRequestAccepted :: Bool
  }
  deriving (Show, Eq)

-- A share in someone else's capacity
data CapacityShare = CapacityShare
  { targetCapacity :: Capacity,
    sharePercentage :: Float,
    computedQuantity :: Int -- Derived from percentage * capacity quantity, respecting maxDivisibility
  }
  deriving (Show, Eq)

type CapacityInventory = Map.Map String Capacity

type CapacityShares = Map.Map String CapacityShare

-- Helper functions for capacity management
computeQuantityShare :: Capacity -> Float -> Int
computeQuantityShare cap percentage =
  let rawQuantity = round $ (fromIntegral $ quantity cap) * percentage
      maxNatural = naturalDiv $ maxDivisibility cap
      maxPercent = percentageDiv $ maxDivisibility cap
      -- Apply percentage divisibility constraint
      percentConstrained =
        if percentage > maxPercent
          then round $ (fromIntegral $ quantity cap) * maxPercent
          else rawQuantity
      -- Apply natural number divisibility constraint
      naturalConstrained = (percentConstrained `div` maxNatural) * maxNatural
   in naturalConstrained

-- Create a new capacity share
createCapacityShare :: Capacity -> Float -> CapacityShare
createCapacityShare cap percentage =
  CapacityShare
    { targetCapacity = cap,
      sharePercentage = percentage,
      computedQuantity = computeQuantityShare cap percentage
    }

-- Add a capacity to a node's inventory
addCapacity :: Capacity -> TreeZipper -> TreeZipper
addCapacity cap z =
  z
    { zipperCurrent =
        (zipperCurrent z)
          { nodeCapacities = Map.insert (capacityId cap) cap (nodeCapacities $ zipperCurrent z)
          }
    }

-- Add a share in another node's capacity
addCapacityShare :: String -> CapacityShare -> TreeZipper -> TreeZipper
addCapacityShare shareId share z =
  z
    { zipperCurrent =
        (zipperCurrent z)
          { nodeCapacityShares = Map.insert shareId share (nodeCapacityShares $ zipperCurrent z)
          }
    }

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
-----------------------
exampleForest :: (Forest, ContributorIndex)
exampleForest = (forest, ci)
  where
    -- Create example capacities
    roomCapacity =
      Capacity
        { capacityId = "room1",
          capacityName = "Spare Room",
          quantity = 10,
          unit = "room",
          shareDepth = 2,
          expanded = True,
          coordinates =
            SpaceTimeCoordinates
              { locationType = Specific,
                allDay = True,
                recurrence = Nothing,
                customRecurrence = Nothing,
                startDate = undefined, -- Would be actual UTCTime in real usage
                startTime = undefined,
                endDate = undefined,
                endTime = undefined,
                timeZone = "UTC"
              },
          maxDivisibility =
            MaxDivisibility
              { naturalDiv = 1,
                percentageDiv = 0.1 -- Maximum 50% share
              },
          hiddenUntilRequestAccepted = False
        }

    pieCapacity =
      Capacity
        { capacityId = "pie1",
          capacityName = "Apple Pie",
          quantity = 8,
          unit = "slices",
          shareDepth = 3,
          expanded = True,
          coordinates =
            SpaceTimeCoordinates
              { locationType = Specific,
                allDay = True,
                recurrence = Nothing,
                customRecurrence = Nothing,
                startDate = undefined,
                startTime = undefined,
                endDate = undefined,
                endTime = undefined,
                timeZone = "UTC"
              },
          maxDivisibility =
            MaxDivisibility
              { naturalDiv = 1, -- Can't split a slice
                percentageDiv = 0.125 -- Minimum share is one slice (1/8)
              },
          hiddenUntilRequestAccepted = False
        }

    -- Create roots with mutual contributors
    aliceNode = createRootNode "alice" "Alice" (Points 100) ["bob", "charlie"] Nothing
    bobNode = createRootNode "bob" "Bob" (Points 100) ["alice", "charlie"] Nothing
    charlieNode = createRootNode "charlie" "Charlie" (Points 100) ["alice", "bob"] Nothing

    aliceRoot = TreeZipper aliceNode Nothing
    bobRoot = TreeZipper bobNode Nothing
    charlieRoot = TreeZipper charlieNode Nothing

    -- Add capacities to roots
    aliceWithCapacity = addCapacity roomCapacity aliceRoot
    bobWithCapacity = addCapacity pieCapacity bobRoot

    -- Create capacity shares
    aliceRoomShare = createCapacityShare roomCapacity 0.5 -- 50% share of room
    bobPieShare = createCapacityShare pieCapacity 0.25 -- 25% share of pie (2 slices)

    -- Build trees with children and add capacity shares
    aliceWithChild = addChild "alice_child" (Points 30) ["bob", "charlie"] Nothing aliceWithCapacity
    bobWithChild = addChild "bob_child" (Points 40) ["alice", "charlie"] Nothing bobWithCapacity
    bobWithShare = addCapacityShare "alice_room" aliceRoomShare bobWithChild
    charlieWithChild = addChild "charlie_child" (Points 50) ["alice", "bob"] Nothing charlieRoot
    charlieWithShare = addCapacityShare "bob_pie" bobPieShare charlieWithChild

    -- Create forest and contributor index
    forest = foldl addToForest Map.empty [aliceWithChild, bobWithShare, charlieWithShare]
    ci =
      Map.fromList
        [ ("alice", aliceWithChild),
          ("bob", bobWithShare),
          ("charlie", charlieWithShare)
        ]

main :: IO ()
main = do
  let (forest, ci) = exampleForest
      alice = fromJust $ Map.lookup "alice" forest
      bob = fromJust $ Map.lookup "bob" forest
      charlie = fromJust $ Map.lookup "charlie" forest

  -- Print fulfillment values
  putStrLn "Mutual Fulfillment Values:"
  putStrLn $ "Alice <-> Bob: " ++ show (mutualFulfillment ci alice bob)
  putStrLn $ "Alice <-> Charlie: " ++ show (mutualFulfillment ci alice charlie)
  putStrLn $ "Bob <-> Charlie: " ++ show (mutualFulfillment ci bob charlie)

  -- Print capacity information
  putStrLn "\nCapacity Information:"

  -- Print Alice's room capacity
  putStrLn "Alice's Room Capacity:"
  let aliceRoom = Map.lookup "room1" (nodeCapacities $ zipperCurrent alice)
  case aliceRoom of
    Just room -> do
      putStrLn $ "  Quantity: " ++ show (quantity room) ++ " " ++ unit room

      -- Show provider-centric shares (Alice's perspective)
      putStrLn "\nProvider-centric shares (Alice distributing to network):"
      let aliceShares = providerShares ci alice 2
          totalShares = sum $ Map.elems aliceShares
          bobShare = Map.findWithDefault 0 "bob" aliceShares * 100
          charlieShare = Map.findWithDefault 0 "charlie" aliceShares * 100
          otherShares = 100 - bobShare - charlieShare

      putStrLn $ "  Total shares distributed: " ++ show (totalShares * 100) ++ "%"
      putStrLn $ "  Bob's share from Alice: " ++ show bobShare ++ "%"
      putStrLn $ "  Charlie's share from Alice: " ++ show charlieShare ++ "%"
      putStrLn $ "  Remaining shares (distributed to other network members): " ++ show otherShares ++ "%"

      -- Break down where the remaining shares went (if any)
      let remainingRecipients = filter (\(id, _) -> id /= "bob" && id /= "charlie") $ Map.toList aliceShares
      unless (null remainingRecipients) $ do
        putStrLn "  Other recipients:"
        forM_ remainingRecipients $ \(id, share) ->
          putStrLn $ "    " ++ id ++ ": " ++ show (share * 100) ++ "%"

      -- Show receiver-centric shares
      putStrLn "\nReceiver perspective (shares received from Alice):"
      putStrLn $
        "  Bob's share of Alice's room: "
          ++ show (receiverShareFrom ci bob alice room 2 * 100)
          ++ "%"
      putStrLn $
        "  Charlie's share of Alice's room: "
          ++ show (receiverShareFrom ci charlie alice room 2 * 100)
          ++ "%"

      -- Show computed quantities
      putStrLn "\nComputed quantities for capacity shares:"
      let bobRoomQty = round $ (fromIntegral $ quantity room) * receiverShareFrom ci bob alice room 2
      let charlieRoomQty = round $ (fromIntegral $ quantity room) * receiverShareFrom ci charlie alice room 2
      putStrLn $ "  Bob's portion: " ++ show bobRoomQty ++ " " ++ unit room
      putStrLn $ "  Charlie's portion: " ++ show charlieRoomQty ++ " " ++ unit room
    Nothing -> putStrLn "  No room capacity found"

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Unexpected Nothing"