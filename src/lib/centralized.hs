import Control.Monad (forM_, unless)
import Data.List (foldl', maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

----------------------
-- Caching System --
----------------------

-- Generic cache value type to handle all our cache needs
data CacheValue
  = FloatValue Float
  | IntValue Int
  | StringListValue [String]
  | ShareMapValue ShareMap
  deriving (Show, Eq)

-- Unified cache type
data Cache k = Cache
  { cacheMap :: Map.Map k CacheValue,
    cacheMisses :: Int,
    cacheHits :: Int
  }
  deriving (Show, Eq)

-- Initialize an empty cache
emptyCache :: Cache k
emptyCache = Cache Map.empty 0 0

-- Lookup a value in the cache
cacheLookup :: (Ord k) => k -> Cache k -> Maybe (CacheValue, Cache k)
cacheLookup key cache =
  case Map.lookup key (cacheMap cache) of
    Just value -> Just (value, cache {cacheHits = cacheHits cache + 1})
    Nothing -> Nothing

-- Insert a value into the cache
cacheInsert :: (Ord k) => k -> CacheValue -> Cache k -> Cache k
cacheInsert key value cache =
  cache
    { cacheMap = Map.insert key value (cacheMap cache),
      cacheMisses = cacheMisses cache + 1
    }

-- Helper functions to work with CacheValue
fromFloat :: Float -> CacheValue
fromFloat = FloatValue

fromInt :: Int -> CacheValue
fromInt = IntValue

fromStringList :: [String] -> CacheValue
fromStringList = StringListValue

fromShareMap :: ShareMap -> CacheValue
fromShareMap = ShareMapValue

toFloat :: CacheValue -> Maybe Float
toFloat (FloatValue f) = Just f
toFloat _ = Nothing

toInt :: CacheValue -> Maybe Int
toInt (IntValue i) = Just i
toInt _ = Nothing

toStringList :: CacheValue -> Maybe [String]
toStringList (StringListValue l) = Just l
toStringList _ = Nothing

toShareMap :: CacheValue -> Maybe ShareMap
toShareMap (ShareMapValue m) = Just m
toShareMap _ = Nothing

-- Node type with unified cache
data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Points,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    -- Unified cache
    nodeCache :: Cache String
  }
  deriving (Show, Eq)

-- Cache keys
weightCacheKey :: String -> String
weightCacheKey nodeId = nodeId ++ "_weight"

fulfillmentCacheKey :: String -> String
fulfillmentCacheKey nodeId = nodeId ++ "_fulfillment"

mutualCacheKey :: String -> String -> String
mutualCacheKey nodeId1 nodeId2 =
  if nodeId1 < nodeId2
    then nodeId1 ++ "_mutual_" ++ nodeId2
    else nodeId2 ++ "_mutual_" ++ nodeId1

descendantsCacheKey :: String -> String
descendantsCacheKey nodeId = nodeId ++ "_descendants"

totalPointsCacheKey :: String -> String
totalPointsCacheKey nodeId = nodeId ++ "_total_points"

-- Global cache for provider shares
newtype ProviderSharesCache = ProviderSharesCache {sharesCache :: Cache (String, Int)}

-- Initialize an empty provider shares cache
emptyProviderSharesCache :: ProviderSharesCache
emptyProviderSharesCache = ProviderSharesCache emptyCache

-- Generic caching function
withCache ::
  (Ord k) =>
  k ->
  (b -> CacheValue) ->
  (CacheValue -> Maybe b) ->
  (a -> b) ->
  Cache k ->
  a ->
  (b, Cache k)
withCache key constructor extractor compute cache arg =
  case cacheLookup key cache of
    Just (value, updatedCache) ->
      case extractor value of
        Just v -> (v, updatedCache)
        Nothing -> computeAndCache updatedCache
    Nothing -> computeAndCache cache
  where
    computeAndCache currentCache =
      let computed = compute arg
          newValue = constructor computed
          newCache = cacheInsert key newValue currentCache
       in (computed, newCache)

----------------------
-- Core Data Types --
----------------------

newtype Points = Points Int deriving (Eq, Ord, Show)

getPoints (Points p) = p

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

-------------------------
-- Zipper Navigation --
-------------------------

-- Core navigation
enterChild :: String -> TreeZipper -> Maybe TreeZipper
enterChild childId (TreeZipper current ctx) =
  case Map.lookup childId (nodeChildren current) of
    Nothing -> Nothing
    Just child ->
      let siblings = Map.delete childId (nodeChildren current)
          newCtx = Ctx current siblings (maybe [] ctxAncestors ctx)
       in Just $ TreeZipper child (Just newCtx)

exitToParent :: TreeZipper -> Maybe TreeZipper
exitToParent (TreeZipper _ Nothing) = Nothing
exitToParent (TreeZipper current (Just ctx)) =
  let parent = ctxParent ctx
      siblings = ctxSiblings ctx
      ancestors = ctxAncestors ctx
      newParent = parent {nodeChildren = Map.insert (nodeId current) current siblings}
   in Just $ TreeZipper newParent (if null ancestors then Nothing else Just (head ancestors))

-- The -: operator for more readable navigation chains
(-:) :: a -> (a -> b) -> b
x -: f = f x

-- Enhanced navigation functions
enterSibling :: String -> TreeZipper -> Maybe TreeZipper
enterSibling name z = exitToParent z >>= enterChild name

goToRoot :: TreeZipper -> TreeZipper
goToRoot z = maybe z goToRoot (exitToParent z)

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
      -- Unified cache
      nodeCache = emptyCache
    }
  where
    clampManual = fmap (max 0 . min 1)

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
            nodeCache = emptyCache
          }
      clampManual = fmap (max 0 . min 1)
      -- Update parent's cached total
      key = totalPointsCacheKey (nodeId current)
      currentCache = nodeCache current
      currentTotal = case cacheLookup key currentCache of
        Just (value, _) -> Maybe.fromMaybe 0 (toInt value)
        Nothing -> 0
      newTotal = currentTotal + getPoints pts
      newCache = cacheInsert key (fromInt newTotal) currentCache
      -- Clear all parent caches since structure has changed
      clearedCache = emptyCache
      updatedCurrent =
        current
          { nodeChildren = Map.insert name newChild (nodeChildren current),
            nodeCache = clearedCache
          }
   in z {zipperCurrent = updatedCurrent}

-------------------------
-- Core Calculations --
-------------------------

-- Calculate total points from all children
totalChildPoints :: TreeZipper -> Int
totalChildPoints z =
  let current = zipperCurrent z
      cache = nodeCache current
      cacheKey = totalPointsCacheKey (nodeId current)
   in case cacheLookup cacheKey cache of
        Just (value, updatedCache) ->
          case toInt value of
            Just total -> total
            Nothing -> computeAndCache updatedCache cacheKey
        Nothing -> computeAndCache cache cacheKey
  where
    computeAndCache cache key =
      let total = sum $ map (getPoints . nodePoints) $ Map.elems (nodeChildren $ zipperCurrent z)
          newCache = cacheInsert key (fromInt total) cache
       in total

-- Calculate a node's weight with caching
weight :: TreeZipper -> Float
weight z =
  let current = zipperCurrent z
      cacheKey = weightCacheKey (nodeId current)
   in case zipperContext z of
        Nothing -> 1.0 -- Root node has weight 1.0
        Just ctx ->
          case cacheLookup cacheKey (nodeCache current) of
            Just (value, updatedCache) ->
              case toFloat value of
                Just w -> w
                Nothing ->
                  let w = computeWeight current
                      newCache = cacheInsert cacheKey (fromFloat w) updatedCache
                   in w
            Nothing ->
              let w = computeWeight current
                  newCache = cacheInsert cacheKey (fromFloat w) (nodeCache current)
               in w
  where
    computeWeight node
      | total == 0 = 0
      | otherwise = fromIntegral currentPoints / fromIntegral total * parentWeight
      where
        parentZipper = fromJust $ exitToParent z
        total = totalChildPoints parentZipper
        currentPoints = getPoints (nodePoints node)
        parentWeight = weight parentZipper

shareOfParent :: TreeZipper -> Float
shareOfParent z
  | isRoot = 1.0 -- Root node has 100% share
  | total == 0 = 0
  | otherwise = fromIntegral currentPoints / fromIntegral total
  where
    isRoot = isNothing $ exitToParent z
    parent = fromJust $ exitToParent z
    total = totalChildPoints parent
    currentPoints = getPoints (nodePoints $ zipperCurrent z)

isContribution :: Forest -> TreeZipper -> Bool
isContribution ci z = not (Set.null (nodeContributors $ zipperCurrent z)) && isJust (zipperContext z)

-- Check if a node has direct contribution children
hasDirectContributionChild :: Forest -> TreeZipper -> Bool
hasDirectContributionChild ci z = any isContributor childZippers
  where
    current = zipperCurrent z
    childIds = Map.keys (nodeChildren current)
    childZippers = catMaybes [enterChild id z | id <- childIds]
    isContributor = isContribution ci

-- Check if a node has non-contribution children
hasNonContributionChild :: Forest -> TreeZipper -> Bool
hasNonContributionChild ci z = not (all (isContribution ci) childZippers)
  where
    current = zipperCurrent z
    childIds = Map.keys (nodeChildren current)
    childZippers = catMaybes [enterChild id z | id <- childIds]
    isContributor = isContribution ci

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: Forest -> TreeZipper -> Float
contributionChildrenWeight ci z =
  let childZippers = catMaybes [enterChild id z | id <- Map.keys (nodeChildren $ zipperCurrent z)]
      (contribWeight, totalWeight) = foldl' accumWeight (0, 0) childZippers
   in if totalWeight == 0 then 0 else contribWeight / totalWeight
  where
    accumWeight (cw, tw) child =
      let w = weight child
       in if isContribution ci child
            then (cw + w, tw + w)
            else (cw, tw + w)

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: Forest -> (TreeZipper -> Bool) -> TreeZipper -> Float
childrenFulfillment ci pred z =
  sum
    [ fulfilled ci child * shareOfParent child
      | child <- Maybe.mapMaybe (`enterChild` z) (Map.keys $ nodeChildren $ zipperCurrent z),
        pred child
    ]

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: Forest -> TreeZipper -> Float
contributionChildrenFulfillment ci = childrenFulfillment ci (isContribution ci)

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: Forest -> TreeZipper -> Float
nonContributionChildrenFulfillment ci z =
  let childZippers = catMaybes [enterChild id z | id <- Map.keys (nodeChildren $ zipperCurrent z)]
      nonContribChildren = filter (not . isContribution ci) childZippers
      weights = map weight nonContribChildren
      fulfillments = map (fulfilled ci) nonContribChildren
      weightedFulfillments = zipWith (*) weights fulfillments
      totalWeight = sum weights
   in if totalWeight == 0
        then 0
        else sum weightedFulfillments / totalWeight

-- Safely get instances of a contributor
getContributorInstances :: Forest -> String -> Set.Set TreeZipper
getContributorInstances contribIndex contribId = Set.singleton $ Map.findWithDefault emptyZipper contribId contribIndex
  where
    emptyNode = createRootNode contribId contribId (Points 0) [] Nothing
    emptyZipper = TreeZipper emptyNode Nothing

-- Safely get a contributor node
getContributorNode :: Forest -> String -> Maybe TreeZipper
getContributorNode contribIndex contribId =
  let instances = getContributorInstances contribIndex contribId
   in listToMaybe (Set.toList instances)

-- Calculate fulfillment with caching
fulfilled :: Forest -> TreeZipper -> Float
fulfilled ci z =
  let current = zipperCurrent z
      cacheKey = fulfillmentCacheKey (nodeId current)
      cache = nodeCache current
   in case cacheLookup cacheKey cache of
        Just (value, updatedCache) ->
          case toFloat value of
            Just f -> f
            Nothing ->
              let f = computeFulfillment current
                  newCache = cacheInsert cacheKey (fromFloat f) updatedCache
               in f
        Nothing ->
          let f = computeFulfillment current
              newCache = cacheInsert cacheKey (fromFloat f) cache
           in f
  where
    computeFulfillment node
      | Map.null (nodeChildren node) =
          if isContribution ci z then 1.0 else 0.0
      | isJust (nodeManualFulfillment node) && hasDirectContributionChild ci z =
          if not (hasNonContributionChild ci z)
            then fromJust $ nodeManualFulfillment node
            else manualContribShare
      | otherwise =
          sum
            [ fulfilled ci child * shareOfParent child
              | child <- childZippers
            ]
      where
        manualContribShare =
          let contribWeight = contributionChildrenWeight ci z
              nonContribFulfillment = nonContributionChildrenFulfillment ci z
           in fromJust (nodeManualFulfillment node) * contribWeight
                + nonContribFulfillment * (1.0 - contribWeight)
        childZippers = Maybe.mapMaybe (`enterChild` z) (Map.keys $ nodeChildren node)

-- Calculate the desire (unfulfilled need) of a node
desire :: Forest -> TreeZipper -> Float
desire ci z = 1.0 - fulfilled ci z

---------------------------
-- Mutual Fulfillment --
---------------------------
shareOfGeneralFulfillment :: Forest -> TreeZipper -> TreeZipper -> Float
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
      cache = nodeCache current
      cacheKey = descendantsCacheKey currentId
   in case cacheLookup cacheKey cache of
        Just (value, updatedCache) ->
          case toStringList value of
            Just _ -> z : getAllDescendants z
            Nothing -> computeAndCache cache cacheKey
        Nothing -> computeAndCache cache cacheKey
  where
    computeAndCache cache key =
      let descendants = getAllDescendants z
          descendantIds = map (nodeId . zipperCurrent) descendants
          newCache = cacheInsert key (fromStringList descendantIds) cache
       in z : descendants

getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z = z : concatMap (maybe [] getAllDescendants) childZippers
  where
    childZippers = map (`enterChild` z) (Map.keys $ nodeChildren $ zipperCurrent z)

-- Calculate mutual fulfillment between two nodes with caching
mutualFulfillment :: Forest -> TreeZipper -> TreeZipper -> Float
mutualFulfillment ci a b =
  let aNode = zipperCurrent a
      bNode = zipperCurrent b
      cacheKey = mutualCacheKey (nodeId aNode) (nodeId bNode)
   in case cacheLookup cacheKey (nodeCache aNode) of
        Just (value, updatedCache) ->
          case toFloat value of
            Just v -> v
            Nothing ->
              let v = compute ()
                  newCache = cacheInsert cacheKey (fromFloat v) updatedCache
               in v
        Nothing ->
          let v = compute ()
              newCache = cacheInsert cacheKey (fromFloat v) (nodeCache aNode)
           in v
  where
    compute _ =
      let aToB = shareOfGeneralFulfillment ci a b
          bToA = shareOfGeneralFulfillment ci b a
       in min aToB bToA

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
findHighestMutualPath :: Forest -> TreeZipper -> TreeZipper -> Maybe NavigationPath
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
getHighMutualNodes :: Forest -> TreeZipper -> Float -> [TreeZipper]
getHighMutualNodes ci z threshold =
  filter (\node -> mutualFulfillment ci z node > threshold) (getAllDescendantsCached z)

-- Calculate mutual fulfillment between all pairs in a forest
calculateForestMutualFulfillment :: Forest -> Forest -> [(String, String, Float)]
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

-- Core share calculation function that handles all depths
providerShares :: Forest -> TreeZipper -> Int -> ShareMap
providerShares ci provider depth
  | depth <= 1 = initialShares
  | otherwise = normalizeShares finalShares
  where
    initialShares =
      let contributors = nodeContributors $ zipperCurrent provider
          validContributors = catMaybes [Map.lookup c ci | c <- Set.toList contributors]
          mutualValues = [(nodeId $ zipperCurrent c, mutualFulfillment ci provider c) | c <- validContributors]
          total = sum $ map snd mutualValues
       in if total == 0
            then Map.empty
            else Map.fromList [(id, val / total) | (id, val) <- mutualValues]

    (finalShares, _) = foldl' (processDepth ci) (initialShares, Set.empty) [2 .. depth]

    normalizeShares shares =
      let totalShare = sum $ Map.elems shares
       in if totalShare > 0
            then Map.map (/ totalShare) shares
            else shares

-- Process additional depths for transitive share calculation
processDepth :: Forest -> (ShareMap, VisitedSet) -> Int -> (ShareMap, VisitedSet)
processDepth ci (shares, visited) _ = foldl' processRecipient (shares, visited) unvisitedRecipients
  where
    recipients = Map.keys shares
    unvisitedRecipients = filter (not . (`Set.member` visited)) recipients

    processRecipient :: (ShareMap, VisitedSet) -> String -> (ShareMap, VisitedSet)
    processRecipient (currentShares, currentVisited) recipientId
      | not hasShare = (currentShares, currentVisited)
      | not hasRecipient = (currentShares, currentVisited)
      | otherwise = (Map.unionWith (+) currentShares weightedShares, newVisited)
      where
        hasShare = Map.member recipientId currentShares
        recipientShare = fromJust $ Map.lookup recipientId currentShares
        hasRecipient = Map.member recipientId ci
        recipient = fromJust $ Map.lookup recipientId ci
        newVisited = Set.insert recipientId currentVisited
        connections = nodeContributors $ zipperCurrent recipient
        unvisitedConnections = Set.filter (not . (`Set.member` currentVisited)) connections
        transitiveShares = providerShares ci recipient 1
        weightedShares = Map.map (* recipientShare) transitiveShares

-- Provider-centric share calculation with caching
providerSharesCached :: ProviderSharesCache -> Forest -> TreeZipper -> Int -> (ShareMap, ProviderSharesCache)
providerSharesCached cache ci provider maxDepth =
  let providerId = nodeId $ zipperCurrent provider
      key = (providerId, maxDepth)
      currentCache = sharesCache cache
   in case cacheLookup key currentCache of
        Just (value, updatedCache) ->
          case toShareMap value of
            Just shares -> (shares, cache {sharesCache = updatedCache})
            Nothing -> computeAndCache key currentCache
        Nothing -> computeAndCache key currentCache
  where
    computeAndCache key currentCache =
      let shares = providerShares ci provider maxDepth
          newCache = cacheInsert key (fromShareMap shares) currentCache
       in (shares, cache {sharesCache = newCache})

-- Simplified interface functions that use providerShares
directShare :: Forest -> TreeZipper -> String -> Float
directShare ci provider recipientId =
  Map.findWithDefault 0 recipientId $ providerShares ci provider 1

-- Get a receiver's share from a specific capacity provider
receiverShareFrom :: Forest -> TreeZipper -> TreeZipper -> Capacity -> Int -> Float
receiverShareFrom ci receiver provider capacity maxDepth =
  let providerShareMap = providerShares ci provider maxDepth
      receiverId = nodeId $ zipperCurrent receiver
   in Map.findWithDefault 0 receiverId providerShareMap
   
-- Get a person's total share in a specific capacity
getPersonalCapacityShare :: Forest -> TreeZipper -> Capacity -> Float
getPersonalCapacityShare ci person capacity =
  let capacityOwners =
        [ owner | (_, owner) <- Map.toList ci, Map.member (capacityId capacity) (nodeCapacities $ zipperCurrent owner)
        ]
      directShares = [receiverShareFrom ci person owner capacity 2 | owner <- capacityOwners]
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
  let rawQuantity = round $ fromIntegral (quantity cap) * percentage
      maxNatural = naturalDiv $ maxDivisibility cap
      maxPercent = percentageDiv $ maxDivisibility cap
      -- Apply percentage divisibility constraint
      percentConstrained =
        if percentage > maxPercent
          then round $ fromIntegral (quantity cap) * maxPercent
          else rawQuantity
      -- Apply natural number divisibility constraint
      naturalConstrained = percentConstrained `div` maxNatural * maxNatural
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

mergeContributors :: [Forest] -> Forest
mergeContributors = foldl' (Map.unionWith (\_ new -> new)) Map.empty

-- Helper operator for safe navigation
(?) :: Maybe a -> a -> a
(?) = flip Maybe.fromMaybe

-----------------------
-- Example Usage --
-----------------------
exampleForest :: (Forest, Forest)
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
      let bobRoomQty = round $ fromIntegral (quantity room) * receiverShareFrom ci bob alice room 2
      let charlieRoomQty = round $ fromIntegral (quantity room) * receiverShareFrom ci charlie alice room 2
      putStrLn $ "  Bob's portion: " ++ show bobRoomQty ++ " " ++ unit room
      putStrLn $ "  Charlie's portion: " ++ show charlieRoomQty ++ " " ++ unit room
    Nothing -> putStrLn "  No room capacity found"