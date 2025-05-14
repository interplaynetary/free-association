import Control.Monad (forM_, unless)
import Control.Monad.State.Strict (State, get, put, runState)
import Data.Function.Memoize (memoize)
import Data.List (foldl', maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)

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

-- Monadic cache type for cleaner state threading
type CacheM k a = State (Cache k) a

-- Monadic wrapper for cache operations
withCacheM ::
  (Ord k, Cacheable v) =>
  k ->
  (a -> v) ->
  a ->
  CacheM k v
withCacheM key compute arg = do
  cache <- get
  case cacheLookup key cache of
    Just (cv, updated) ->
      case fromCache cv of
        Just v -> do
          put updated
          pure v
        Nothing -> computeAndStore
    Nothing -> computeAndStore
  where
    computeAndStore = do
      cache <- get
      let v = compute arg
          cv = toCache v
      put (cacheInsert key cv cache)
      pure v

-- Helper to run a cache computation on a node
runNodeCache :: CacheM String a -> Node -> (a, Node)
runNodeCache computation node =
  let (result, newCache) = runState computation (nodeCache node)
   in (result, node {nodeCache = newCache})

-- | Helper to run a cache computation on a zipper.
-- This generic function leverages currying to handle functions with any number of parameters:
--
-- For a function with no extra parameters:
--   withNodeCache f z
--   where f :: TreeZipper -> CacheM String a
--
-- For a function with one extra parameter:
--   withNodeCache (f param) z
--   where f :: b -> TreeZipper -> CacheM String a
--
-- For a function with two extra parameters:
--   withNodeCache (\z -> f param1 param2 z) z
--   where f :: b -> c -> TreeZipper -> CacheM String a
withNodeCache :: (TreeZipper -> CacheM String a) -> TreeZipper -> a
withNodeCache computation z =
  let current = zipperCurrent z
      (result, _) = runNodeCache (computation z) current
   in result

-- Helper functions to work with CacheValue
class Cacheable a where
  toCache :: a -> CacheValue
  fromCache :: CacheValue -> Maybe a

toStringList :: CacheValue -> Maybe [String]
toStringList = fromCache

instance Cacheable Int where toCache = IntValue; fromCache (IntValue i) = Just i; fromCache _ = Nothing

instance Cacheable Float where toCache = FloatValue; fromCache (FloatValue f) = Just f; fromCache _ = Nothing

instance Cacheable ShareMap where toCache = ShareMapValue; fromCache (ShareMapValue m) = Just m; fromCache _ = Nothing

instance Cacheable [String] where toCache = StringListValue; fromCache (StringListValue l) = Just l; fromCache _ = Nothing

-- Node type with unified cache
data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Int,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    -- Unified cache - only used for PS and SOGF now
    nodeCache :: Cache String
  }
  deriving (Show, Eq)

-- Cache is now only used for PS and SOGF maps

----------------------
-- Core Data Types --
----------------------

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

-- Enhanced tree traversal helpers

-- | Get the child zippers of the current node
children :: TreeZipper -> [TreeZipper]
children z = catMaybes [enterChild cid z | cid <- Map.keys (nodeChildren $ zipperCurrent z)]

-- | Generic fold over the children
foldChildren :: (b -> TreeZipper -> b) -> b -> TreeZipper -> b
foldChildren f z0 tz = foldl' f z0 (children tz)

-- | Generic map over the children
mapChildren :: (TreeZipper -> a) -> TreeZipper -> [a]
mapChildren f = map f . children

-- | Check if any child satisfies a predicate
anyChild :: (TreeZipper -> Bool) -> TreeZipper -> Bool
anyChild p = any p . children

-- | Check if all children satisfy a predicate
allChildren :: (TreeZipper -> Bool) -> TreeZipper -> Bool
allChildren p = all p . children

-- | Get all descendants (excluding self)
descendants :: TreeZipper -> [TreeZipper]
descendants = tail . getAllDescendantsCached

----------------------------
-- Tree Modification API --
----------------------------
createRootNode :: String -> String -> Int -> [String] -> Maybe Float -> Node
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
addChild :: String -> Int -> [String] -> Maybe Float -> TreeZipper -> Maybe TreeZipper
addChild name pts contribs manual z@(TreeZipper current ctx) =
  -- Don't allow adding children to nodes with contributors
  if not (Set.null (nodeContributors current))
    then Nothing
    else
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

          -- We're making a structural change, so it's best to just invalidate all caches
          updatedCurrent =
            current
              { nodeChildren = Map.insert name newChild (nodeChildren current),
                nodeCache = emptyCache
              }
       in Just z {zipperCurrent = updatedCurrent}

-- Add contributors to a node and recursively delete its subtree
addContributors :: [String] -> TreeZipper -> TreeZipper
addContributors contribs z =
  modifyNode updateNode z
  where
    updateNode node =
      node
        { nodeContributors = Set.fromList contribs,
          nodeChildren = Map.empty,
          nodeCache = emptyCache
        }

-- Helper to recursively delete a subtree
deleteSubtree :: TreeZipper -> TreeZipper
deleteSubtree z =
  modifyNode updateNode z
  where
    updateNode node =
      node
        { nodeChildren = Map.empty,
          nodeCache = emptyCache
        }

-------------------------
-- Core Calculations --
-------------------------

-- Calculate total points from all children
totalChildPoints :: TreeZipper -> Int
totalChildPoints z@(TreeZipper current _) = memoized (nodeId current)
  where
    memoized = memoize computeTotal

    computeTotal :: String -> Int
    computeTotal _ = sum $ mapChildren (nodePoints . zipperCurrent) z

-- Calculate a node's weight with caching
weight :: TreeZipper -> Float
weight z@(TreeZipper current ctx) = case zipperContext z of
  Nothing -> 1.0 -- Root node has weight 1.0
  Just _ -> memoized (nodeId current)
  where
    memoized = memoize computeWeight

    computeWeight :: String -> Float
    computeWeight _
      | total == 0 = 0
      | otherwise = fromIntegral currentPoints / fromIntegral total * parentWeight
      where
        parentZipper = fromJust $ exitToParent z
        total = totalChildPoints parentZipper
        currentPoints = nodePoints current
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
    currentPoints = nodePoints $ zipperCurrent z

-- Predicates for node types

-- | Check if a node is a contribution node (has contributors and is not root)
isContribution :: TreeZipper -> Bool
isContribution z = not (Set.null (nodeContributors $ zipperCurrent z)) && isJust (zipperContext z)

-- Check if a node has direct contribution children
hasDirectContributionChild :: TreeZipper -> Bool
hasDirectContributionChild = anyChild isContribution

-- Check if a node has non-contribution children
hasNonContributionChild :: TreeZipper -> Bool
hasNonContributionChild = not . allChildren isContribution

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: TreeZipper -> Float
contributionChildrenWeight z =
  let (contribWeight, totalWeight) = foldChildren accumWeight (0, 0) z
   in if totalWeight == 0 then 0 else contribWeight / totalWeight
  where
    accumWeight (cw, tw) child =
      let w = weight child
       in if isContribution child
            then (cw + w, tw + w)
            else (cw, tw + w)

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: (TreeZipper -> Bool) -> TreeZipper -> Float
childrenFulfillment pred z =
  sum
    [ fulfilled child * shareOfParent child
      | child <- children z,
        pred child
    ]

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: TreeZipper -> Float
contributionChildrenFulfillment = childrenFulfillment isContribution

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: TreeZipper -> Float
nonContributionChildrenFulfillment z =
  let nonContribChildren = filter (not . isContribution) (children z)
      weights = map weight nonContribChildren
      fulfillments = map fulfilled nonContribChildren
      weightedFulfillments = zipWith (*) weights fulfillments
      totalWeight = sum weights
   in if totalWeight == 0
        then 0
        else sum weightedFulfillments / totalWeight

-- Safely get instances of a contributor
getContributorInstances :: Forest -> String -> Set.Set TreeZipper
getContributorInstances contribIndex contribId = Set.singleton $ Map.findWithDefault emptyZipper contribId contribIndex
  where
    emptyNode = createRootNode contribId contribId 0 [] Nothing
    emptyZipper = TreeZipper emptyNode Nothing

-- Safely get a contributor node
getContributorNode :: Forest -> String -> Maybe TreeZipper
getContributorNode contribIndex contribId =
  let instances = getContributorInstances contribIndex contribId
   in listToMaybe (Set.toList instances)

-- Calculate fulfillment with caching
fulfilled :: TreeZipper -> Float
fulfilled z@(TreeZipper current _) = memoized (nodeId current)
  where
    memoized = memoize computeFulfillment

    computeFulfillment :: String -> Float
    computeFulfillment _
      | Map.null (nodeChildren current) =
          if isContribution z then 1.0 else 0.0
      | isJust (nodeManualFulfillment current) && hasDirectContributionChild z =
          if not (hasNonContributionChild z)
            then fromJust $ nodeManualFulfillment current
            else manualContribShare
      | otherwise =
          sum
            [ fulfilled child * shareOfParent child
              | child <- children z
            ]
      where
        manualContribShare =
          let contribWeight = contributionChildrenWeight z
              nonContribFulfillment = nonContributionChildrenFulfillment z
           in fromJust (nodeManualFulfillment current) * contribWeight
                + nonContribFulfillment * (1.0 - contribWeight)

-- Calculate the desire (unfulfilled need) of a node
desire :: TreeZipper -> Float
desire z = 1.0 - fulfilled z

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
                    && isContribution node
              )
              (target : descendants target)
          -- Calculate total contribution from these nodes
          total = sum [weight node * fulfilled node | node <- contributingNodes]
          -- Get number of contributors for each node
          contributorCounts = map (Set.size . nodeContributors . zipperCurrent) contributingNodes
          -- Divide each node's contribution by its number of contributors
          weightedTotal =
            sum
              [ w * f / fromIntegral c
                | (w, f, c) <-
                    zip3
                      (map weight contributingNodes)
                      (map fulfilled contributingNodes)
                      contributorCounts
              ]
       in weightedTotal

-- Get all descendants with caching
getAllDescendantsCached :: TreeZipper -> [TreeZipper]
getAllDescendantsCached z@(TreeZipper current _) = z : memoized (nodeId current)
  where
    memoized = memoize computeDescendants

    computeDescendants :: String -> [TreeZipper]
    computeDescendants _ = concatMap getAllDescendants (children z)

-- | Get all descendants (including self)
getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z = z : concatMap getAllDescendants (children z)

-- Calculate mutual fulfillment between two nodes with caching
mutualFulfillment :: Forest -> TreeZipper -> TreeZipper -> Float
mutualFulfillment ci a b =
  let aId = nodeId (zipperCurrent a)
      bId = nodeId (zipperCurrent b)
      sharesA = sharesOfGeneralFulfillmentMap ci a
      sharesB = sharesOfGeneralFulfillmentMap ci b
      aToB = Map.findWithDefault 0 bId sharesA
      bToA = Map.findWithDefault 0 aId sharesB
   in min aToB bToA

------------------------------
-- Mutual Fulfillment Utils --
------------------------------

-- Find the path to the highest mutual fulfillment node
findHighestMutualPath :: Forest -> TreeZipper -> TreeZipper -> Maybe NavigationPath
findHighestMutualPath ci root target =
  let allPaths = map getCurrentPath $ root : descendants root
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
  filter (\node -> mutualFulfillment ci z node > threshold) (z : descendants z)

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

-- | Generic function to normalize a map so that its values sum to 1
-- Works for any map with a numeric value type
normalizeMap :: (Ord k, Fractional v, Real v) => Map.Map k v -> Map.Map k v
normalizeMap m =
  let total = sum (Map.elems m)
   in if total == 0 then m else Map.map (/ fromRational (toRational total)) m

-- | Type-specific version of normalizeMap for ShareMap
normalizeShareMap :: ShareMap -> ShareMap
normalizeShareMap = normalizeMap

-------------------------------------------------------
-- Normalized Shares-of-General-Fulfillment Map
-------------------------------------------------------
sharesOfGeneralFulfillmentMap :: Forest -> TreeZipper -> ShareMap
sharesOfGeneralFulfillmentMap ci z =
  withNodeCache (sharesOfGeneralFulfillmentMapM ci) z

sharesOfGeneralFulfillmentMapM :: Forest -> TreeZipper -> CacheM String ShareMap
sharesOfGeneralFulfillmentMapM ci z =
  do
    let cacheKey = "sogf_map"
    cache <- get
    case cacheLookup cacheKey cache of
      Just (cv, upCache) -> case fromCache cv of
        Just sm -> do
          -- Always normalize on read to ensure normalization
          let nsm = normalizeShareMap sm
          put (cacheInsert cacheKey (toCache nsm) upCache)
          pure nsm
        Nothing -> computeAndStore
      Nothing -> computeAndStore
  where
    computeAndStore = do
      let pairs =
            [ (nodeId (zipperCurrent c), shareOfGeneralFulfillment ci z c)
              | (_, c) <- Map.toList ci
            ]
          rawMap = Map.fromList $ filter ((> 0) . snd) pairs
          normMap = normalizeShareMap rawMap
      cache <- get
      put (cacheInsert "sogf_map" (toCache normMap) cache)
      pure normMap

-----------------------------------------------------------------------
-- Simplified Provider-centric shares (direct shares only)
-----------------------------------------------------------------------
providerShares :: Forest -> TreeZipper -> ShareMap
providerShares ci provider = withNodeCache (providerSharesM ci) provider

providerSharesM :: Forest -> TreeZipper -> CacheM String ShareMap
providerSharesM ci z =
  do
    let cacheKey = "ps_direct"
    cache <- get
    case cacheLookup cacheKey cache of
      Just (cv, upCache) -> case fromCache cv of
        Just sm -> do
          -- Always normalize on read to ensure normalization
          let nsm = normalizeShareMap sm
          put (cacheInsert cacheKey (toCache nsm) upCache)
          pure nsm
        Nothing -> computeAndStore
      Nothing -> computeAndStore
  where
    computeAndStore = do
      let result = directShares z
          normResult = normalizeShareMap result
      cache <- get
      put (cacheInsert "ps_direct" (toCache normResult) cache)
      pure normResult

    -- Direct shares based on mutual fulfillment
    directShares p =
      let -- Look for contributors in all descendants
          allContributingNodes = p : descendants p
          -- All unique contributor IDs across the tree
          allContribs = Set.unions [nodeContributors (zipperCurrent node) | node <- allContributingNodes]
          -- Get valid contributors (those that exist in ci)
          validContribs = catMaybes [Map.lookup c ci | c <- Set.toList allContribs]
          -- Calculate mutual fulfillment for each contributor
          mvals = [(nodeId (zipperCurrent c), mutualFulfillment ci p c) | c <- validContribs]
       in Map.fromList $ filter ((> 0) . snd) mvals

-- Simplified interface function that uses providerShares
directShare :: Forest -> TreeZipper -> String -> Float
directShare ci provider recipientId =
  Map.findWithDefault 0 recipientId $ providerShares ci provider

-- Get a receiver's share from a specific capacity provider
receiverShareFrom :: Forest -> TreeZipper -> TreeZipper -> Capacity -> Float
receiverShareFrom ci receiver provider capacity =
  let providerShareMap = providerShares ci provider
      receiverId = nodeId $ zipperCurrent receiver
   in Map.findWithDefault 0 receiverId providerShareMap

-- Get a person's total share in a specific capacity
getPersonalCapacityShare :: Forest -> TreeZipper -> Capacity -> Float
getPersonalCapacityShare ci person capacity =
  let capacityOwners =
        [ owner | (_, owner) <- Map.toList ci, Map.member (capacityId capacity) (nodeCapacities $ zipperCurrent owner)
        ]
      directShares = [receiverShareFrom ci person owner capacity | owner <- capacityOwners]
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
                percentageDiv = 0.1 -- Maximum share is 10%
              },
          hiddenUntilRequestAccepted = False
        }

    pieCapacity =
      Capacity
        { capacityId = "pie1",
          capacityName = "Apple Pie",
          quantity = 8,
          unit = "slices",
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
    aliceNode = createRootNode "alice" "Alice" 100 [] Nothing
    bobNode = createRootNode "bob" "Bob" 100 [] Nothing
    charlieNode = createRootNode "charlie" "Charlie" 100 [] Nothing -- Removed contributors
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
    aliceWithChild = fromJust $ addChild "alice_child" 30 ["bob", "charlie"] Nothing aliceWithCapacity
    bobWithChild = fromJust $ addChild "bob_child" 40 ["alice", "charlie"] Nothing bobWithCapacity
    bobWithShare = addCapacityShare "alice_room" aliceRoomShare bobWithChild
    charlieWithChild = fromJust $ addChild "charlie_child" 50 ["alice", "bob"] Nothing charlieRoot

    -- Add children to the child nodes (second level of the tree)
    aliceWithGrandchild = case enterChild "alice_child" aliceWithChild of
      Just childZ -> case addChild "alice_grandchild" 15 ["bob"] Nothing childZ of
        Just grandchildZ -> fromJust $ exitToParent grandchildZ
        Nothing -> aliceWithChild
      Nothing -> aliceWithChild

    bobWithGrandchild = case enterChild "bob_child" bobWithShare of
      Just childZ -> case addChild "bob_grandchild" 20 ["alice"] Nothing childZ of
        Just grandchildZ -> fromJust $ exitToParent grandchildZ
        Nothing -> bobWithShare
      Nothing -> bobWithShare

    charlieWithGrandchild = case enterChild "charlie_child" charlieWithChild of
      Just childZ -> case addChild "charlie_grandchild" 25 ["alice", "bob"] Nothing childZ of
        Just grandchildZ -> fromJust $ exitToParent grandchildZ
        Nothing -> charlieWithChild
      Nothing -> charlieWithChild

    charlieWithShare = addCapacityShare "bob_pie" bobPieShare charlieWithGrandchild

    -- Create forest and contributor index
    forest = foldl addToForest Map.empty [aliceWithGrandchild, bobWithGrandchild, charlieWithShare]
    ci =
      Map.fromList
        [ ("alice", aliceWithGrandchild),
          ("bob", bobWithGrandchild),
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
      let aliceShares = providerShares ci alice
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
          ++ show (receiverShareFrom ci bob alice room * 100)
          ++ "%"
      putStrLn $
        "  Charlie's share of Alice's room: "
          ++ show (receiverShareFrom ci charlie alice room * 100)
          ++ "%"

      -- Show computed quantities
      putStrLn "\nComputed quantities for capacity shares:"
      let bobRoomQty = round $ fromIntegral (quantity room) * receiverShareFrom ci bob alice room
      let charlieRoomQty = round $ fromIntegral (quantity room) * receiverShareFrom ci charlie alice room
      putStrLn $ "  Bob's portion: " ++ show bobRoomQty ++ " " ++ unit room
      putStrLn $ "  Charlie's portion: " ++ show charlieRoomQty ++ " " ++ unit room
    Nothing -> putStrLn "  No room capacity found"