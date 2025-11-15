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
-- Basic Types --
----------------------

newtype Points = Points Int deriving (Eq, Ord, Show)

getPoints (Points p) = p

-- Convert TreeRef to a string key for caching
treeRefToString :: TreeRef -> String
treeRefToString (TreeRef pid nid) = pid ++ ":" ++ nid

-- Create a TreeRef
createTreeRef :: String -> String -> TreeRef
createTreeRef playerId nodeId = TreeRef playerId nodeId

----------------------
-- Caching System --
----------------------

-- Generic cache value type to handle all our cache needs
data CacheValue
  = FloatValue Float
  | IntValue Int
  | StringListValue [String]
  | ShareMapValue ShareMap
  | MutualFulfillmentValue Float  -- For network caching
  | ProviderShareValue ShareMap   -- For network caching
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

fromMutualFulfillment :: Float -> CacheValue
fromMutualFulfillment = MutualFulfillmentValue

fromProviderShare :: ShareMap -> CacheValue
fromProviderShare = ProviderShareValue

toFloat :: CacheValue -> Maybe Float
toFloat (FloatValue f) = Just f
toFloat (MutualFulfillmentValue f) = Just f
toFloat _ = Nothing

toInt :: CacheValue -> Maybe Int
toInt (IntValue i) = Just i
toInt _ = Nothing

toStringList :: CacheValue -> Maybe [String]
toStringList (StringListValue l) = Just l
toStringList _ = Nothing

toShareMap :: CacheValue -> Maybe ShareMap
toShareMap (ShareMapValue m) = Just m
toShareMap (ProviderShareValue m) = Just m
toShareMap _ = Nothing

toMutualFulfillment :: CacheValue -> Maybe Float
toMutualFulfillment (MutualFulfillmentValue f) = Just f
toMutualFulfillment (FloatValue f) = Just f
toMutualFulfillment _ = Nothing

toProviderShare :: CacheValue -> Maybe ShareMap
toProviderShare (ProviderShareValue m) = Just m
toProviderShare (ShareMapValue m) = Just m
toProviderShare _ = Nothing

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

-- Network cache for remote mutual fulfillment values
newtype NetworkCache = NetworkCache {networkCache :: Cache String}
  deriving (Show, Eq)

-- Initialize an empty network cache
emptyNetworkCache :: NetworkCache
emptyNetworkCache = NetworkCache emptyCache

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

data Ctx = Ctx
  { ctxParent :: Node,
    ctxSiblings :: Map.Map String Node,
    ctxAncestors :: [Ctx]
  }
  deriving (Show)

data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Points,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set TreeRef,  -- Changed from Set.Set String to Set.Set TreeRef
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    -- Unified cache
    nodeCache :: Cache String
  }
  deriving (Show, Eq)

data TreeZipper = TreeZipper
  { zipperCurrent :: Node,
    zipperContext :: Maybe Ctx
  }
  deriving (Show)

-- Define once at the top level
type VisitedSet = Set.Set String

-- Define Forest as a collection of trees owned by a player
type LocalForest = Map.Map String TreeZipper
-- Legacy forest type for backward compatibility - UNUSED
-- type Forest = Map.Map String TreeZipper

-- Player data type to manage local trees
data Player = Player
  { playerId :: String,
    playerName :: String,
    playerLocalTrees :: Map.Map String TreeZipper, -- Local trees owned by this player
    playerNetworkCache :: NetworkCache -- Cache for network-derived values
  }
  deriving (Show)

-- Create a new player
createPlayer :: String -> String -> Player
createPlayer id name =
  Player
    { playerId = id,
      playerName = name,
      playerLocalTrees = Map.empty,
      playerNetworkCache = emptyNetworkCache
    }

-- Add a tree to a player's local forest
addTree :: String -> TreeZipper -> Player -> Player
addTree treeId tree player =
  player {playerLocalTrees = Map.insert treeId tree (playerLocalTrees player)}

-- Helper function to identify local vs remote TreeRefs
isLocalTreeRef :: Player -> TreeRef -> Bool
isLocalTreeRef player ref = treeRefPlayerId ref == playerId player

-- Resolve a TreeRef to a TreeZipper if it's local
resolveLocalTreeRef :: Player -> TreeRef -> Maybe TreeZipper
resolveLocalTreeRef player ref 
  | not (isLocalTreeRef player ref) = Nothing
  | otherwise = Map.lookup (treeRefNodeId ref) (playerLocalTrees player)

-- Add player's tree to local forest
addToLocalForest :: Player -> String -> TreeZipper -> Player
addToLocalForest player treeId z = player { playerLocalTrees = Map.insert treeId z (playerLocalTrees player) }

--------------------------------------------
-- Simplified P2P Network API --
--------------------------------------------

-- A monad-agnostic Network interface with clear separation
-- between local operations and remote calls
class Monad m => Network m where
  -- Core remote operations
  fetchRemoteNode :: Player -> TreeRef -> m (Maybe TreeZipper)
  fetchRemoteMutualFulfillment :: Player -> TreeRef -> TreeRef -> m Float
  fetchRemoteProviderShares :: Player -> TreeRef -> Int -> m ShareMap
  fetchRemoteCapacity :: Player -> TreeRef -> String -> m (Maybe Capacity)
  
  -- Default implementations that can be overridden
  fetchRemoteCapacityShare :: Player -> TreeRef -> TreeRef -> String -> m Float
  fetchRemoteCapacityShare player receiverRef providerRef capId = do
    -- Default implementation using other core operations
    mCapacity <- fetchRemoteCapacity player providerRef capId
    case mCapacity of
      Just capacity -> do
        shares <- fetchRemoteProviderShares player providerRef (shareDepth capacity)
        return $ Map.findWithDefault 0 (treeRefNodeId receiverRef) shares
      Nothing -> return 0

-- Generic resolver that works with any Network monad 
-- Tries local resolution first, then network if needed
resolveNode :: Network m => Player -> TreeRef -> m (Maybe TreeZipper)
resolveNode player ref 
  | isLocalTreeRef player ref = return $ resolveLocalTreeRef player ref
  | otherwise = fetchRemoteNode player ref

-- A pure implementation for testing with local-only data
-- This instance never actually makes network calls
instance Network IO where
  -- Implementation that only works with local data
  fetchRemoteNode player _ = return Nothing
  fetchRemoteMutualFulfillment _ _ _ = return 0
  fetchRemoteProviderShares _ _ _ = return Map.empty
  fetchRemoteCapacity _ _ _ = return Nothing

-- A testing instance for local simulation
newtype LocalSimulation a = LocalSimulation { runLocalSim :: Map.Map String Player -> IO a }

instance Functor LocalSimulation where
  fmap f (LocalSimulation action) = LocalSimulation $ \ps -> fmap f (action ps)

instance Applicative LocalSimulation where
  pure a = LocalSimulation $ \_ -> pure a
  (LocalSimulation f) <*> (LocalSimulation a) = 
    LocalSimulation $ \ps -> f ps <*> a ps

instance Monad LocalSimulation where
  (LocalSimulation a) >>= f = LocalSimulation $ \ps -> do
    result <- a ps
    let (LocalSimulation b) = f result
    b ps

-- Run a local simulation with multiple players
runLocalSimulation :: LocalSimulation a -> Map.Map String Player -> IO a
runLocalSimulation (LocalSimulation action) players = action players

-- Create a test network that simulates P2P interactions
instance Network LocalSimulation where
  fetchRemoteNode player ref = LocalSimulation $ \players -> do
    -- Find the remote player
    let remotePlayerId = treeRefPlayerId ref
        maybeRemotePlayer = Map.lookup remotePlayerId players
    case maybeRemotePlayer of
      Just remotePlayer -> 
        -- Try to resolve in the remote player's forest
        return $ resolveLocalTreeRef remotePlayer ref
      Nothing -> return Nothing
    
  fetchRemoteMutualFulfillment player aRef bRef = LocalSimulation $ \players -> do
    -- Try to resolve both refs across all players
    let aPlayerId = treeRefPlayerId aRef
        bPlayerId = treeRefPlayerId bRef
        maybeAPlayer = Map.lookup aPlayerId players
        maybeBPlayer = Map.lookup bPlayerId players
    
    case (maybeAPlayer, maybeBPlayer) of
      (Just aPlayer, Just bPlayer) -> do
        let aZipper = resolveLocalTreeRef aPlayer aRef
            bZipper = resolveLocalTreeRef bPlayer bRef
        case (aZipper, bZipper) of
          (Just a, Just b) -> do
            -- For simulation, we need to decide which player's context to use
            -- Let's use aPlayer's context if aRef is local to that player
            let calculatingPlayer = if isLocalTreeRef aPlayer aRef then aPlayer else bPlayer
            return $ mutualFulfillmentPure calculatingPlayer a b
          _ -> return 0
      _ -> return 0
      
  fetchRemoteProviderShares player ref depth = LocalSimulation $ \players -> do
    -- Find the provider player
    let providerPlayerId = treeRefPlayerId ref
        maybeProviderPlayer = Map.lookup providerPlayerId players
    
    case maybeProviderPlayer of
      Just providerPlayer -> 
        case resolveLocalTreeRef providerPlayer ref of
          Just zipper -> return $ providerSharesPure providerPlayer zipper depth
          Nothing -> return Map.empty
      Nothing -> return Map.empty
      
  fetchRemoteCapacity player ref capId = LocalSimulation $ \players -> do
    -- Find the capacity owner player
    let ownerPlayerId = treeRefPlayerId ref
        maybeOwnerPlayer = Map.lookup ownerPlayerId players
    
    case maybeOwnerPlayer of
      Just ownerPlayer -> 
        case resolveLocalTreeRef ownerPlayer ref of
          Just zipper -> return $ Map.lookup capId (nodeCapacities $ zipperCurrent zipper)
          Nothing -> return Nothing
      Nothing -> return Nothing

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
-- Updated to use TreeRefs instead of strings for contributors
createRootNode :: String -> String -> Points -> [TreeRef] -> Maybe Float -> Node
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
      nodeCache = emptyCache
    }
  where
    clampManual = fmap (max 0 . min 1)

-- Add a child and update caches, now using TreeRefs
addChild :: String -> Points -> [TreeRef] -> Maybe Float -> TreeZipper -> TreeZipper
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

-- Fix the isContribution function to check if a node has contributors and is not a root
isContribution :: Player -> TreeZipper -> Bool
isContribution player z = 
  not (Set.null (nodeContributors $ zipperCurrent z)) && 
  isJust (zipperContext z)

-- Check if a node has direct contribution children (updated for local calculation)
hasDirectContributionChild :: Player -> TreeZipper -> Bool
hasDirectContributionChild player z = any isContributor childZippers
  where
    current = zipperCurrent z
    childIds = Map.keys (nodeChildren current)
    childZippers = catMaybes [enterChild id z | id <- childIds]
    isContributor = isContribution player

-- Check if a node has non-contribution children
hasNonContributionChild :: Player -> TreeZipper -> Bool
hasNonContributionChild player z = not (all (isContribution player) childZippers)
  where
    current = zipperCurrent z
    childIds = Map.keys (nodeChildren current)
    childZippers = catMaybes [enterChild id z | id <- childIds]

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: Player -> TreeZipper -> Float
contributionChildrenWeight player z =
  let childZippers = catMaybes [enterChild id z | id <- Map.keys (nodeChildren $ zipperCurrent z)]
      (contribWeight, totalWeight) = foldl' accumWeight (0, 0) childZippers
   in if totalWeight == 0 then 0 else contribWeight / totalWeight
  where
    accumWeight (cw, tw) child =
      let w = weight child
       in if isContribution player child
            then (cw + w, tw + w)
            else (cw, tw + w)

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: Player -> (TreeZipper -> Bool) -> TreeZipper -> Float
childrenFulfillment player pred z =
  sum
    [ fulfilled player child * shareOfParent child
      | child <- Maybe.mapMaybe (`enterChild` z) (Map.keys $ nodeChildren $ zipperCurrent z),
        pred child
    ]

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: Player -> TreeZipper -> Float
contributionChildrenFulfillment player = childrenFulfillment player (isContribution player)

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: Player -> TreeZipper -> Float
nonContributionChildrenFulfillment player z =
  let childZippers = catMaybes [enterChild id z | id <- Map.keys (nodeChildren $ zipperCurrent z)]
      nonContribChildren = filter (not . isContribution player) childZippers
      weights = map weight nonContribChildren
      fulfillments = map (fulfilled player) nonContribChildren
      weightedFulfillments = zipWith (*) weights fulfillments
      totalWeight = sum weights
   in if totalWeight == 0
        then 0
        else sum weightedFulfillments / totalWeight

-- We need to ensure the fulfilled function is retained for local calculations
-- Calculate fulfillment with caching (local only, no network)
fulfilled :: Player -> TreeZipper -> Float
fulfilled player z =
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
          if isContribution player z then 1.0 else 0.0
      | isJust (nodeManualFulfillment node) && hasDirectContributionChild player z =
          if not (hasNonContributionChild player z)
            then fromJust $ nodeManualFulfillment node
            else manualContribShare
      | otherwise =
          sum
            [ fulfilled player child * shareOfParent child
              | child <- childZippers
            ]
      where
        manualContribShare =
          let contribWeight = contributionChildrenWeight player z
              nonContribFulfillment = nonContributionChildrenFulfillment player z
           in fromJust (nodeManualFulfillment node) * contribWeight
                + nonContribFulfillment * (1.0 - contribWeight)
        childZippers = Maybe.mapMaybe (`enterChild` z) (Map.keys $ nodeChildren node)

-- Calculate the desire (unfulfilled need) of a node
desire :: Player -> TreeZipper -> Float
desire player z = 1.0 - fulfilled player z

---------------------------
-- Mutual Fulfillment --
---------------------------
-- Network-aware mutual fulfillment calculation (pure version for local trees)
mutualFulfillmentPure :: Player -> TreeZipper -> TreeZipper -> Float
mutualFulfillmentPure player a b =
  let aNode = zipperCurrent a
      bNode = zipperCurrent b
      aId = nodeId aNode
      bId = nodeId bNode
      cacheKey = mutualCacheKey aId bId
   in case cacheLookup cacheKey (nodeCache aNode) of
        Just (value, _) ->
          case toFloat value of
            Just v -> v
            Nothing -> compute ()
        Nothing -> compute ()
  where
    compute _ =
      let aToB = shareOfGeneralFulfillment player a b
          bToA = shareOfGeneralFulfillment player b a
          result = min aToB bToA
       in result

-- Modified to work with Player and local trees
shareOfGeneralFulfillment :: Player -> TreeZipper -> TreeZipper -> Float
shareOfGeneralFulfillment player target contributor =
  let contribId = nodeId $ zipperCurrent contributor
      contribPlayerId = playerId player
      contribRef = TreeRef contribPlayerId contribId
      
      -- Debug contributions for this function
      contributingNodes =
        filter
          ( \node ->
              -- Need to check if any contributor ref matches our contributor
              let nodeContribs = nodeContributors (zipperCurrent node)
                  hasContributor = any (\ref -> 
                    treeRefNodeId ref == contribId) nodeContribs
               in hasContributor && isContribution player node
          )
          (getAllDescendantsCached target)
      
      -- For debugging, count nodes that contribute
      numContribs = length contributingNodes
      
      -- Calculate total contribution from these nodes
      total = sum [weight node * fulfilled player node | node <- contributingNodes]
      
      -- Get number of contributors for each node
      contributorCounts = map (Set.size . nodeContributors . zipperCurrent) contributingNodes
      
      -- Divide each node's contribution by its number of contributors
      weightedTotal =
        sum
          [ w * f / fromIntegral c
            | (w, f, c) <-
                zip3
                  (map weight contributingNodes)
                  (map (fulfilled player) contributingNodes)
                  contributorCounts
          ]
   in if numContribs > 0 then weightedTotal else 0.0

-- Get all descendants with caching (unchanged except for showing full type signature)
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

-- Simplified network-aware mutual fulfillment calculation
mutualFulfillment :: Network m => Player -> TreeRef -> TreeRef -> m Float
mutualFulfillment player aRef bRef = do
  -- Try local cache first
  let cacheKey = mutualCacheKey (treeRefToString aRef) (treeRefToString bRef)
      playerCache = networkCache (playerNetworkCache player)
  
  case cacheLookup cacheKey playerCache of
    Just (value, _) -> 
      case toMutualFulfillment value of
        Just v -> return v
        Nothing -> computeAndCache
    Nothing -> computeAndCache
  where
    computeAndCache = do
      -- If both nodes are local, use pure calculation
      if isLocalTreeRef player aRef && isLocalTreeRef player bRef
      then do
        let aZipper = resolveLocalTreeRef player aRef
            bZipper = resolveLocalTreeRef player bRef
        case (aZipper, bZipper) of
          (Just a, Just b) -> do
            let result = mutualFulfillmentPure player a b
            -- No need to update cache in this simplified version
            return result
          _ -> return 0
      -- Otherwise use network API
      else fetchRemoteMutualFulfillment player aRef bRef

------------------------------------------
-- Provider-centric share calculation --
------------------------------------------


-- Types for capacity management
type ShareMap = Map.Map String Float

-- Core share calculation function for local trees (pure version)
providerSharesPure :: Player -> TreeZipper -> Int -> ShareMap
providerSharesPure player provider depth
  | depth <= 1 = initialShares
  | otherwise = normalizeShares finalShares
  where
    initialShares =
      let current = zipperCurrent provider
          contributors = nodeContributors current
          
          -- Convert contributors to a list of contributor IDs
          contributorIds = [treeRefNodeId ref | ref <- Set.toList contributors]
          
          -- Get all TreeRefs from the current provider node
          contributorRefs = Set.toList contributors
          
          -- For each contributor, calculate mutual fulfillment with provider
          -- This simulates what would happen over the network in a real implementation
          mutualValues = [(nodeId, 1.0) | nodeId <- contributorIds]
          
          -- Sum up all mutual values
          total = sum $ map snd mutualValues
       in if total == 0
            then Map.empty
            else Map.fromList [(id, val / total) | (id, val) <- mutualValues]

    (finalShares, _) = foldl' (processDepthPure player) (initialShares, Set.empty) [2 .. depth]

    normalizeShares shares =
      let totalShare = sum $ Map.elems shares
       in if totalShare > 0
            then Map.map (/ totalShare) shares
            else shares
            
-- Process additional depths for transitive share calculation (local)
processDepthPure :: Player -> (ShareMap, VisitedSet) -> Int -> (ShareMap, VisitedSet)
processDepthPure player (shares, visited) _ = foldl' processRecipient (shares, visited) unvisitedRecipients
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
        recipientRef = createTreeRef (playerId player) recipientId
        hasRecipient = isJust $ resolveLocalTreeRef player recipientRef
        recipient = fromJust $ resolveLocalTreeRef player recipientRef
        newVisited = Set.insert recipientId currentVisited
        
        -- For local tree only consider local contributors
        contributorRefs = nodeContributors $ zipperCurrent recipient
        localContributors = Set.filter (isLocalTreeRef player) contributorRefs
        unvisitedConnections = Set.filter 
          (\ref -> not (Set.member (treeRefNodeId ref) currentVisited)) 
          localContributors
          
        -- Get local shares only
        transitiveShares = providerSharesPure player recipient 1
        weightedShares = Map.map (* recipientShare) transitiveShares

-- Network-aware provider shares calculation
providerShares :: Network m => Player -> TreeRef -> Int -> m ShareMap
providerShares player ref depth = do
  -- Check if ref is local
  if isLocalTreeRef player ref
  then do
    -- Use pure local calculation
    case resolveLocalTreeRef player ref of
      Just zipper -> return $ providerSharesPure player zipper depth
      Nothing -> return Map.empty
  -- Otherwise use network API
  else fetchRemoteProviderShares player ref depth

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

-- TreeRef for referencing nodes in other trees
data TreeRef = TreeRef
  { treeRefPlayerId :: String, -- Owner of the tree
    treeRefNodeId :: String     -- Node within the tree
  }
  deriving (Show, Eq, Ord)

-- Get a receiver's share from a specific capacity provider
receiverShareFrom :: Network m => Player -> TreeRef -> TreeRef -> Capacity -> Int -> m Float
receiverShareFrom player receiver provider capacity maxDepth = do
  shares <- providerShares player provider maxDepth
  return $ Map.findWithDefault 0 (treeRefNodeId receiver) shares

-- Get a person's total share in a specific capacity
getPersonalCapacityShare :: Network m => Player -> TreeRef -> Capacity -> m Float
getPersonalCapacityShare player person capacity = do
  -- Find all capacity owners (would rely on network discovery in real implementation)
  capacityOwners <- findCapacityOwners player (capacityId capacity)
  
  -- Calculate shares from each owner
  directShares <- mapM (\owner -> 
    receiverShareFrom player person owner capacity 2) capacityOwners
    
  -- Return max share
  return $ if null directShares then 0 else maximum directShares

-- Simple implementation that only finds local capacity owners
findCapacityOwners :: Network m => Player -> String -> m [TreeRef]
findCapacityOwners player capId = do
  -- In a real implementation, this would use network discovery
  -- For now, just check local trees
  let allLocalRoots = Map.toList (playerLocalTrees player)
      localOwners = [(tid, tree) | (tid, tree) <- allLocalRoots, 
                     Map.member capId (nodeCapacities $ zipperCurrent tree)]
  return [createTreeRef (playerId player) tid | (tid, _) <- localOwners]


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


---------------------------
-- Helper Functions for Testing --
---------------------------

-- Run a network operation with the local simulation implementation
runLocalTest :: LocalSimulation a -> Map.Map String Player -> IO a
runLocalTest sim players = runLocalSim sim players

-----------------------
-- Example Usage --
-----------------------
-- Modify exampleP2PSetup to add more debugging and create a better example
exampleP2PSetup :: IO ()
exampleP2PSetup = do
  -- Create players
  let alice = createPlayer "alice" "Alice"
      bob = createPlayer "bob" "Bob"
      charlie = createPlayer "charlie" "Charlie"
      
  -- Create mutual contributor references
  let aliceRef = createTreeRef "alice" "alice"
      bobRef = createTreeRef "bob" "bob"
      charlieRef = createTreeRef "charlie" "charlie"
      
  -- Create example capacities
  let roomCapacity =
        Capacity
          { capacityId = "room1",
            capacityName = "Room",
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
                  percentageDiv = 0.1 -- Maximum 10% share
                },
            hiddenUntilRequestAccepted = False
          }

      pieCapacity =
        Capacity
          { capacityId = "pie1",
            capacityName = "Pie",
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
          
      carCapacity =
        Capacity
          { capacityId = "car1",
            capacityName = "Car",
            quantity = 5,
            unit = "seats",
            shareDepth = 2,
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
                { naturalDiv = 1,
                  percentageDiv = 0.2 -- One seat minimum
                },
            hiddenUntilRequestAccepted = False
          }

  -- Create root nodes with mutual contributors (using TreeRefs)
  let aliceNode = createRootNode "alice" "Alice" (Points 100) [bobRef, charlieRef] Nothing
      bobNode = createRootNode "bob" "Bob" (Points 100) [aliceRef, charlieRef] Nothing
      charlieNode = createRootNode "charlie" "Charlie" (Points 100) [aliceRef, bobRef] Nothing

  let aliceRoot = TreeZipper aliceNode Nothing
      bobRoot = TreeZipper bobNode Nothing
      charlieRoot = TreeZipper charlieNode Nothing

  -- Create trees with more meaningful structure for testing
  -- For Alice: Add children that contribute to Bob and Charlie
  let aliceBobChildRef = createTreeRef "alice" "alice_bob_child"
      aliceCharlieChildRef = createTreeRef "alice" "alice_charlie_child"
      
      aliceWithBobChild = addChild "alice_bob_child" (Points 60) [bobRef] Nothing aliceRoot
      aliceWithBothChildren = addChild "alice_charlie_child" (Points 40) [charlieRef] Nothing aliceWithBobChild
  
  -- For Bob: Add children that contribute to Alice and Charlie
  let bobAliceChildRef = createTreeRef "bob" "bob_alice_child"
      bobCharlieChildRef = createTreeRef "bob" "bob_charlie_child"
      
      bobWithAliceChild = addChild "bob_alice_child" (Points 70) [aliceRef] Nothing bobRoot
      bobWithBothChildren = addChild "bob_charlie_child" (Points 30) [charlieRef] Nothing bobWithAliceChild
  
  -- For Charlie: Add children that contribute to Alice and Bob
  let charlieAliceChildRef = createTreeRef "charlie" "charlie_alice_child"
      charlieBobChildRef = createTreeRef "charlie" "charlie_bob_child"
      
      charlieWithAliceChild = addChild "charlie_alice_child" (Points 60) [aliceRef] Nothing charlieRoot
      charlieWithBothChildren = addChild "charlie_bob_child" (Points 40) [bobRef] Nothing charlieWithAliceChild
  
  -- Add capacities to players
  let aliceWithRoomCapacity = addCapacity roomCapacity aliceWithBothChildren
      bobWithPieCapacity = addCapacity pieCapacity bobWithBothChildren
      charlieWithCarCapacity = addCapacity carCapacity charlieWithBothChildren
  
  -- Add capacity shares
  let aliceRoomShareBob = createCapacityShare roomCapacity 0.6 -- 60% share of room for Bob
      aliceRoomShareCharlie = createCapacityShare roomCapacity 0.3 -- 30% share of room for Charlie
      
      bobPieShareAlice = createCapacityShare pieCapacity 0.5 -- 50% share of pie for Alice
      bobPieShareCharlie = createCapacityShare pieCapacity 0.25 -- 25% share of pie for Charlie
      
      charlieCarShareAlice = createCapacityShare carCapacity 0.4 -- 40% share of car for Alice
      charlieCarShareBob = createCapacityShare carCapacity 0.4 -- 40% share of car for Bob
      
  -- Add capacity shares to nodes with meaningful IDs
  let bobWithRoomShare = addCapacityShare "alice_room" aliceRoomShareBob bobWithPieCapacity
      charlieWithRoomShare = addCapacityShare "alice_room" aliceRoomShareCharlie charlieWithCarCapacity
      
      aliceWithPieShare = addCapacityShare "bob_pie" bobPieShareAlice aliceWithRoomCapacity
      charlieWithPieShare = addCapacityShare "bob_pie" bobPieShareCharlie charlieWithRoomShare
      
      aliceWithCarShare = addCapacityShare "charlie_car" charlieCarShareAlice aliceWithPieShare
      bobWithCarShare = addCapacityShare "charlie_car" charlieCarShareBob bobWithRoomShare

  -- Add trees to players
  let aliceWithTree = addToLocalForest alice "alice" aliceWithCarShare
      bobWithTree = addToLocalForest bob "bob" bobWithCarShare
      charlieWithTree = addToLocalForest charlie "charlie" charlieWithPieShare
      
  -- Create a player map for simulation
  let playerMap = Map.fromList [
          ("alice", aliceWithTree),
          ("bob", bobWithTree), 
          ("charlie", charlieWithTree)
        ]

  -- Verify our P2P structure
  putStrLn "==== P2P Structure Verification ===="
  putStrLn $ "Alice can find her own tree: " ++ show (isJust $ resolveLocalTreeRef aliceWithTree aliceRef)
  putStrLn $ "Bob can find his own tree: " ++ show (isJust $ resolveLocalTreeRef bobWithTree bobRef)
  putStrLn $ "Charlie can find his own tree: " ++ show (isJust $ resolveLocalTreeRef charlieWithTree charlieRef)
  
  -- Debug contributions
  putStrLn "\n==== Contribution Structure ===="
  let aliceContributors = Set.toList $ nodeContributors $ zipperCurrent $ fromJust $ resolveLocalTreeRef aliceWithTree aliceRef
      bobContributors = Set.toList $ nodeContributors $ zipperCurrent $ fromJust $ resolveLocalTreeRef bobWithTree bobRef
      charlieContributors = Set.toList $ nodeContributors $ zipperCurrent $ fromJust $ resolveLocalTreeRef charlieWithTree charlieRef
  
  putStrLn "Alice root contributors:"
  forM_ aliceContributors $ \ref -> 
    putStrLn $ "  " ++ treeRefPlayerId ref ++ ":" ++ treeRefNodeId ref
    
  putStrLn "\nBob root contributors:"
  forM_ bobContributors $ \ref -> 
    putStrLn $ "  " ++ treeRefPlayerId ref ++ ":" ++ treeRefNodeId ref
    
  putStrLn "\nCharlie root contributors:"
  forM_ charlieContributors $ \ref -> 
    putStrLn $ "  " ++ treeRefPlayerId ref ++ ":" ++ treeRefNodeId ref

  -- Calculate mutual fulfillment directly first
  putStrLn "\n==== Direct Mutual Fulfillment ===="
  case (resolveLocalTreeRef aliceWithTree aliceRef, resolveLocalTreeRef aliceWithTree bobRef) of
    (Just a, _) -> putStrLn "Alice can't resolve Bob's TreeRef locally (as expected in P2P model)"
    _ -> putStrLn "Failed to resolve Alice's local reference"

  -- Now demonstrate using LocalSimulation to simulate network operations
  putStrLn "\n==== P2P Network Simulation ===="
  
  -- Calculate and display mutual fulfillment between all pairs
  aliceBobMutual <- runLocalSimulation (mutualFulfillment aliceWithTree aliceRef bobRef) playerMap
  aliceCharlieMutual <- runLocalSimulation (mutualFulfillment aliceWithTree aliceRef charlieRef) playerMap
  bobCharlieMutual <- runLocalSimulation (mutualFulfillment bobWithTree bobRef charlieRef) playerMap
  
  putStrLn $ "Alice<->Bob mutual fulfillment: " ++ show aliceBobMutual
  putStrLn $ "Alice<->Charlie mutual fulfillment: " ++ show aliceCharlieMutual
  putStrLn $ "Bob<->Charlie mutual fulfillment: " ++ show bobCharlieMutual
  
  -- Calculate provider shares
  putStrLn "\n==== Provider Shares Calculation ===="
  aliceShares <- runLocalSimulation (providerShares aliceWithTree aliceRef 2) playerMap
  bobShares <- runLocalSimulation (providerShares bobWithTree bobRef 2) playerMap
  charlieShares <- runLocalSimulation (providerShares charlieWithTree charlieRef 2) playerMap
  
  putStrLn "Alice's provider shares:"
  forM_ (Map.toList aliceShares) $ \(id, share) ->
    putStrLn $ "  " ++ id ++ ": " ++ show (share * 100) ++ "%"
    
  putStrLn "\nBob's provider shares:"
  forM_ (Map.toList bobShares) $ \(id, share) ->
    putStrLn $ "  " ++ id ++ ": " ++ show (share * 100) ++ "%"
    
  putStrLn "\nCharlie's provider shares:"
  forM_ (Map.toList charlieShares) $ \(id, share) ->
    putStrLn $ "  " ++ id ++ ": " ++ show (share * 100) ++ "%"
  
  -- Get capacity shares
  putStrLn "\n==== Capacity Share Distribution ===="
  
  -- Actual shares from Alice's room
  bobRoomShare <- runLocalSimulation 
    (receiverShareFrom aliceWithTree bobRef aliceRef roomCapacity 2) playerMap
  charlieRoomShare <- runLocalSimulation 
    (receiverShareFrom aliceWithTree charlieRef aliceRef roomCapacity 2) playerMap
    
  -- Actual shares from Bob's pie  
  alicePieShare <- runLocalSimulation 
    (receiverShareFrom bobWithTree aliceRef bobRef pieCapacity 2) playerMap
  charliePieShare <- runLocalSimulation 
    (receiverShareFrom bobWithTree charlieRef bobRef pieCapacity 2) playerMap
    
  -- Actual shares from Charlie's car
  aliceCarShare <- runLocalSimulation 
    (receiverShareFrom charlieWithTree aliceRef charlieRef carCapacity 2) playerMap
  bobCarShare <- runLocalSimulation 
    (receiverShareFrom charlieWithTree bobRef charlieRef carCapacity 2) playerMap
  
  putStrLn "Alice's room capacity:"
  putStrLn $ "  Total capacity: " ++ show (quantity roomCapacity) ++ " " ++ unit roomCapacity
  putStrLn $ "  Bob's share: " ++ show (bobRoomShare * 100) ++ "% (" ++ 
    show (round $ bobRoomShare * fromIntegral (quantity roomCapacity)) ++ " " ++ unit roomCapacity ++ ")"
  putStrLn $ "  Charlie's share: " ++ show (charlieRoomShare * 100) ++ "% (" ++ 
    show (round $ charlieRoomShare * fromIntegral (quantity roomCapacity)) ++ " " ++ unit roomCapacity ++ ")"
    
  putStrLn "\nBob's pie capacity:"
  putStrLn $ "  Total capacity: " ++ show (quantity pieCapacity) ++ " " ++ unit pieCapacity
  putStrLn $ "  Alice's share: " ++ show (alicePieShare * 100) ++ "% (" ++ 
    show (round $ alicePieShare * fromIntegral (quantity pieCapacity)) ++ " " ++ unit pieCapacity ++ ")"
  putStrLn $ "  Charlie's share: " ++ show (charliePieShare * 100) ++ "% (" ++ 
    show (round $ charliePieShare * fromIntegral (quantity pieCapacity)) ++ " " ++ unit pieCapacity ++ ")"
    
  putStrLn "\nCharlie's car capacity:"
  putStrLn $ "  Total capacity: " ++ show (quantity carCapacity) ++ " " ++ unit carCapacity
  putStrLn $ "  Alice's share: " ++ show (aliceCarShare * 100) ++ "% (" ++ 
    show (round $ aliceCarShare * fromIntegral (quantity carCapacity)) ++ " " ++ unit carCapacity ++ ")"
  putStrLn $ "  Bob's share: " ++ show (bobCarShare * 100) ++ "% (" ++ 
    show (round $ bobCarShare * fromIntegral (quantity carCapacity)) ++ " " ++ unit carCapacity ++ ")"
  
  putStrLn "\n==== P2P Implementation Complete ===="
  putStrLn "The system now uses TreeRefs to reference cross-player nodes"
  putStrLn "Operations use the Network typeclass, which can have multiple implementations"
  putStrLn "The architecture cleanly separates local operations from network operations"

main :: IO ()
main = exampleP2PSetup