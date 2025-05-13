module Main where

import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (State, get, put, runState)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (foldl', intercalate, isPrefixOf, maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)

-------------------------
-- Persistence System --
-------------------------

-- | Error types for persistence operations
data PersistError
  = ConnectionError String
  | ReadError String
  | WriteError String
  | ParseError String
  | NotFoundError String
  | AuthError String
  | EntityNotFound String
  deriving (Show, Eq)

-- | Simple abstraction for a database connection
-- | In our implementation, we'll use an IORef to store an in-memory Map
type Connection = IORef (Map.Map String ByteString)

-- | Generic serialization interface
class Serializable a where
  serialize :: a -> ByteString
  deserialize :: ByteString -> Maybe a

-- | Data source type to abstract storage backend
-- This allows plugging in different storage implementations
data DataSource = DataSource
  { dsName :: String,
    dsConnect :: IO (Either PersistError Connection),
    dsDisconnect :: Connection -> IO (),
    dsRead :: Connection -> String -> IO (Either PersistError (Maybe ByteString)),
    dsWrite :: Connection -> String -> ByteString -> IO (Either PersistError ()),
    dsDelete :: Connection -> String -> IO (Either PersistError ()),
    dsListKeys :: Connection -> String -> IO (Either PersistError [String]),
    dsBatch :: Connection -> [(String, Maybe ByteString)] -> IO (Either PersistError ())
  }

-- | Persistence monad stack combining effects needed for database operations:
-- 1. ReaderT - for accessing the DataSource
-- 2. ExceptT - for handling errors
-- 3. IO - for side effects
newtype PersistM a = PersistM
  { runPersistM :: ReaderT (DataSource, Connection) (ExceptT PersistError IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError PersistError,
      MonadReader (DataSource, Connection)
    )

-- | Run a persistence operation with a given data source
-- This handles connecting to the data source and running the operation
runPersistOp :: DataSource -> PersistM a -> IO (Either PersistError a)
runPersistOp ds op = do
  connectResult <- dsConnect ds
  case connectResult of
    Left err -> pure $ Left err
    Right conn -> do
      result <- runExceptT $ runReaderT (runPersistM op) (ds, conn)
      dsDisconnect ds conn
      pure result

-- | Run a persistence operation with a pre-existing connection
-- This is useful for running multiple operations with the same connection
withConnection :: DataSource -> Connection -> PersistM a -> IO (Either PersistError a)
withConnection ds conn op = runExceptT $ runReaderT (runPersistM op) (ds, conn)

-- | Get the current data source and connection
getDataSourceAndConn :: PersistM (DataSource, Connection)
getDataSourceAndConn = ask

-- | Get just the active connection
getConnection :: PersistM Connection
getConnection = snd <$> ask

-- | Get just the data source
getDataSource :: PersistM DataSource
getDataSource = fst <$> ask

-- | Core operations

-- | Read an entity from storage
readEntity :: (Serializable a) => String -> PersistM (Maybe a)
readEntity key = do
  (ds, conn) <- ask
  result <- liftIO $ dsRead ds conn key
  case result of
    Left err -> throwError err
    Right Nothing -> pure Nothing
    Right (Just bs) ->
      case deserialize bs of
        Nothing -> throwError $ ParseError $ "Failed to parse entity at key: " ++ key
        Just entity -> pure $ Just entity

-- | Write an entity to storage
writeEntity :: (Serializable a) => String -> a -> PersistM ()
writeEntity key entity = do
  (ds, conn) <- ask
  let serialized = serialize entity
  result <- liftIO $ dsWrite ds conn key serialized
  case result of
    Left err -> throwError err
    Right () -> pure ()

-- | Delete an entity from storage
deleteEntity :: String -> PersistM ()
deleteEntity key = do
  (ds, conn) <- ask
  result <- liftIO $ dsDelete ds conn key
  case result of
    Left err -> throwError err
    Right () -> pure ()

-- | List keys with a given prefix
listKeys :: String -> PersistM [String]
listKeys prefix = do
  (ds, conn) <- ask
  result <- liftIO $ dsListKeys ds conn prefix
  case result of
    Left err -> throwError err
    Right keys -> pure keys

-- | Perform a batch operation (useful for transactions)
batchOperations :: [(String, Maybe ByteString)] -> PersistM ()
batchOperations ops = do
  (ds, conn) <- ask
  result <- liftIO $ dsBatch ds conn ops
  case result of
    Left err -> throwError err
    Right () -> pure ()

-- | Serialize multiple entities in a single batch operation
batchEntities :: (Serializable a) => [(String, Maybe a)] -> PersistM ()
batchEntities entities =
  let converted = map convertEntity entities
   in batchOperations converted
  where
    convertEntity (key, Nothing) = (key, Nothing)
    convertEntity (key, Just val) = (key, Just $ serialize val)

-- TODO:
-- Would it be possible for any node to be treated like this (as a rootNode, and all calculations happen up to it, as if it were the root, of its own subtree?)
-- Then we wouldnt need distinction between root and other nodes.

-- What caches do we want to persist and which only make runtime calculations more efficient?

-- How often do we want to be doing certain calculations? Under what conditions?
-- SharesOfGeneralFulfillment (Time-Complexity?  O(n²))
-- ProviderShares [1-5] (Time-complexity? for different depths?)
-- Depth 1: O(c) where c is number of direct contributors
-- Fast, could be recalculated on-demand : Feasible for real-time client calculation (100-10,000 nodes)
-- Depth 2: O(c²) where c is number of contributors
-- More expensive, good candidate for persistence
-- Depth 3: O(c^3)
-- Borderline feasible with aggressive caching (1M nodes)
-- Depth 4-5: O(c^depth)
-- Exponentially more expensive
-- Critical to persist these results

-- Most real-world value likely comes from depths 1-3, as closer connections typically matter more in resource sharing contexts. Consider implementing a "horizon effect" where calculations become increasingly approximated at greater depths.

-- Implementation Recommendations
-- Aggressive Caching:
-- Each node should persistently cache its depth-1 provider shares
-- Time-based invalidation (recalculate every few hours or when relationships change)
-- This makes higher-depth calculations primarily a "map lookup and combine" operation

-- Bandwidth Optimization:
-- Fetch only ShareMaps needed for the current calculation
-- Compress data when transferring over the network
-- Use incremental updates rather than full recalculations when possible
-- Progressive Computation:
-- Show users immediate results with depth-1 or depth-2 calculations
-- Run higher depths in background tasks
-- Only compute higher depths (4-5) when explicitly requested or needed

-- For most practical applications, I'd recommend:
-- Depth 1: Calculate in real-time (milliseconds)
-- Depth 2: Calculate on-demand with caching (seconds)
-- Depth 3: Background calculation with aggressive caching (minutes)
-- Depth 4+: Server-side computation or approximation

-- What data we want to load upon loading? And to persist!
-- tree
-- nodeCache's shareOfGeneralFulfillment, providerShares

-- We dont need to persist the whole nodeCache? only those two. Are they currently stored in nodeCache?

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
  let (result, newCache) = runState computation (nodeTransientCache node)
   in (result, node {nodeTransientCache = newCache})

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

-- Node type with separate persistent and transient caches
data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Int,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    -- Persistent cache (stored in database)
    nodePersistentCache :: PersistentCache,
    -- Transient cache (in-memory only)
    nodeTransientCache :: Cache String
  }
  deriving (Show, Eq)

-- Separate type for persistent cache data
data PersistentCache = PersistentCache
  { pcSogfMap :: Maybe ShareMap,
    pcProviderShares :: Map.Map Int ShareMap -- Depth -> ShareMap
  }
  deriving (Show, Eq)

-- Create an empty persistent cache
emptyPersistentCache :: PersistentCache
emptyPersistentCache =
  PersistentCache
    { pcSogfMap = Nothing,
      pcProviderShares = Map.empty
    }

-- Helpers to access persistent cache
getSogfMap :: PersistentCache -> Maybe ShareMap
getSogfMap = pcSogfMap

getProviderShares :: Int -> PersistentCache -> Maybe ShareMap
getProviderShares depth pc = Map.lookup depth (pcProviderShares pc)

-- Helpers to update persistent cache
setSogfMap :: ShareMap -> PersistentCache -> PersistentCache
setSogfMap sm pc = pc {pcSogfMap = Just sm}

setProviderShares :: Int -> ShareMap -> PersistentCache -> PersistentCache
setProviderShares depth sm pc =
  pc {pcProviderShares = Map.insert depth sm (pcProviderShares pc)}

-- Cache keys
weightCacheKey :: String -> String
weightCacheKey nodeId = nodeId ++ "_weight"

fulfillmentCacheKey :: String -> String
fulfillmentCacheKey nodeId = nodeId ++ "_fulfillment"

descendantsCacheKey :: String -> String
descendantsCacheKey nodeId = nodeId ++ "_descendants"

totalPointsCacheKey :: String -> String
totalPointsCacheKey nodeId = nodeId ++ "_total_points"

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
      -- Empty persistent cache
      nodePersistentCache = emptyPersistentCache,
      -- Empty transient cache
      nodeTransientCache = emptyCache
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
                nodePersistentCache = emptyPersistentCache,
                nodeTransientCache = emptyCache
              }
          clampManual = fmap (max 0 . min 1)

          -- We're making a structural change, so it's best to just invalidate all caches
          updatedCurrent =
            current
              { nodeChildren = Map.insert name newChild (nodeChildren current),
                nodePersistentCache = emptyPersistentCache,
                nodeTransientCache = emptyCache
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
          nodePersistentCache = emptyPersistentCache,
          nodeTransientCache = emptyCache
        }

-- Helper to recursively delete a subtree
deleteSubtree :: TreeZipper -> TreeZipper
deleteSubtree z =
  modifyNode updateNode z
  where
    updateNode node =
      node
        { nodeChildren = Map.empty,
          nodePersistentCache = emptyPersistentCache,
          nodeTransientCache = emptyCache
        }

-------------------------
-- Core Calculations --
-------------------------

-- Calculate total points from all children
totalChildPoints :: TreeZipper -> Int
totalChildPoints = withNodeCache totalChildPointsM

-- Monadic version of totalChildPoints
totalChildPointsM :: TreeZipper -> CacheM String Int
totalChildPointsM z = do
  let current = zipperCurrent z
      cacheKey = totalPointsCacheKey (nodeId current)
      computeTotal _ = sum $ mapChildren (nodePoints . zipperCurrent) z
  withCacheM cacheKey computeTotal ()

-- Calculate a node's weight with caching
weight :: TreeZipper -> Float
weight = withNodeCache weightM

-- Monadic version of weight
weightM :: TreeZipper -> CacheM String Float
weightM z =
  let current = zipperCurrent z
      cacheKey = weightCacheKey (nodeId current)
   in case zipperContext z of
        Nothing -> pure 1.0 -- Root node has weight 1.0
        Just ctx -> do
          let computeWeightVal _ = computeWeight current
          withCacheM cacheKey computeWeightVal ()
  where
    computeWeight node
      | total == 0 = 0
      | otherwise = fromIntegral currentPoints / fromIntegral total * parentWeight
      where
        parentZipper = fromJust $ exitToParent z
        total = totalChildPoints parentZipper
        currentPoints = nodePoints node
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
fulfilled = withNodeCache fulfilledM

-- Monadic version of fulfilled
fulfilledM :: TreeZipper -> CacheM String Float
fulfilledM z = do
  let current = zipperCurrent z
      cacheKey = fulfillmentCacheKey (nodeId current)
      computeFulfillmentVal _ = computeFulfillment current
  withCacheM cacheKey computeFulfillmentVal ()
  where
    computeFulfillment node
      | Map.null (nodeChildren node) =
          if isContribution z then 1.0 else 0.0
      | isJust (nodeManualFulfillment node) && hasDirectContributionChild z =
          if not (hasNonContributionChild z)
            then fromJust $ nodeManualFulfillment node
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
           in fromJust (nodeManualFulfillment node) * contribWeight
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
getAllDescendantsCached z = z : withNodeCache getAllDescendantsCachedM z

-- Monadic version of getAllDescendantsCached
getAllDescendantsCachedM :: TreeZipper -> CacheM String [TreeZipper]
getAllDescendantsCachedM z = do
  let current = zipperCurrent z
      currentId = nodeId current
      cacheKey = descendantsCacheKey currentId

  cache <- get
  case cacheLookup cacheKey cache of
    Just (value, updatedCache) ->
      case toStringList value of
        Just _ -> do
          put updatedCache
          pure (getAllDescendants z)
        Nothing -> computeAndCache
    Nothing -> computeAndCache
  where
    computeAndCache = do
      let descendants = getAllDescendants z
          descendantIds = map (nodeId . zipperCurrent) descendants
      cache <- get
      put (cacheInsert (descendantsCacheKey (nodeId (zipperCurrent z))) (toCache descendantIds) cache)
      pure descendants

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
    let current = zipperCurrent z

    -- Check only the persistent cache
    case getSogfMap (nodePersistentCache current) of
      Just sm -> do
        -- Always normalize on read to ensure normalization
        pure $ normalizeShareMap sm
      Nothing -> do
        -- Compute and store in persistent cache
        let pairs =
              [ (nodeId (zipperCurrent c), shareOfGeneralFulfillment ci z c)
                | (_, c) <- Map.toList ci
              ]
            rawMap = Map.fromList $ filter ((> 0) . snd) pairs
            normMap = normalizeShareMap rawMap
            cacheKey = "sogf_map"

        -- Update the persistent cache directly
        _ <- updatePersistentCacheInCacheM cacheKey normMap z
        pure normMap

-----------------------------------------------------------------------
-- Provider-centric shares respecting cached data at each depth level
-----------------------------------------------------------------------
providerShares :: Forest -> TreeZipper -> Int -> ShareMap
providerShares ci provider depth = withNodeCache (providerSharesM ci depth) provider

-- Modify providerSharesM similarly to use ONLY the persistent cache
providerSharesM :: Forest -> Int -> TreeZipper -> CacheM String ShareMap
providerSharesM ci depth z =
  do
    let current = zipperCurrent z

    -- Check only the persistent cache
    case getProviderShares depth (nodePersistentCache current) of
      Just sm -> do
        -- Always normalize on read to ensure normalization
        pure $ normalizeShareMap sm
      Nothing -> do
        -- Compute shares and store in persistent cache
        let result = computeShares depth
            normResult = normalizeShareMap result
            cacheKey = "ps_" ++ show depth

        -- Update the persistent cache directly
        _ <- updatePersistentCacheInCacheM cacheKey normResult z
        pure normResult
  where
    -- Compute shares for requested depth (existing code)
    computeShares d
      | d <= 1 = depth1Shares z
      | otherwise = depthNShares d

    -- Rest of the implementation remains unchanged
    depth1Shares p =
      let -- Look for contributors in all descendants
          allContributingNodes = p : descendants p
          -- All unique contributor IDs across the tree
          allContribs = Set.unions [nodeContributors (zipperCurrent node) | node <- allContributingNodes]
          -- Get valid contributors (those that exist in ci)
          validContribs = catMaybes [Map.lookup c ci | c <- Set.toList allContribs]
          -- Calculate mutual fulfillment for each contributor
          mvals = [(nodeId (zipperCurrent c), mutualFulfillment ci p c) | c <- validContribs]
       in Map.fromList $ filter ((> 0) . snd) mvals

    -- Existing depthNShares implementation
    depthNShares d = foldl' step initial [2 .. d]
      where
        initial = depth1Shares z
        step acc _ =
          let recipients = Map.keys acc
              accumulate totalShareMap rid =
                case Map.lookup rid ci of
                  Nothing -> totalShareMap
                  Just recipientZ ->
                    let recipientShare = Map.findWithDefault 0 rid acc
                        recipientDepth1 = providerShares ci recipientZ 1 -- cached depth-1
                        weighted = Map.map (* recipientShare) recipientDepth1
                     in Map.unionWith (+) totalShareMap weighted
           in foldl' accumulate acc recipients

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

-- Create a simple test tree with root and children
createTestTree :: IO Node
createTestTree = do
  let rootNode =
        Node
          { nodeId = "root",
            nodeName = "Root Node",
            nodePoints = 100,
            nodeChildren = Map.empty,
            nodeContributors = Set.empty,
            nodeManualFulfillment = Nothing,
            nodeCapacities = Map.empty,
            nodeCapacityShares = Map.empty,
            nodePersistentCache = emptyPersistentCache,
            nodeTransientCache = emptyCache
          }

      child1 =
        Node
          { nodeId = "child1",
            nodeName = "Child 1",
            nodePoints = 30,
            nodeChildren = Map.empty,
            nodeContributors = Set.fromList ["contributor1"],
            nodeManualFulfillment = Nothing,
            nodeCapacities = Map.empty,
            nodeCapacityShares = Map.empty,
            nodePersistentCache = emptyPersistentCache,
            nodeTransientCache = emptyCache
          }

      child2 =
        Node
          { nodeId = "child2",
            nodeName = "Child 2",
            nodePoints = 70,
            nodeChildren = Map.empty,
            nodeContributors = Set.fromList ["contributor2"],
            nodeManualFulfillment = Nothing,
            nodeCapacities = Map.empty,
            nodeCapacityShares = Map.empty,
            nodePersistentCache = emptyPersistentCache,
            nodeTransientCache = emptyCache
          }

      -- Add children to root
      rootWithChildren = rootNode {nodeChildren = Map.fromList [("child1", child1), ("child2", child2)]}

  -- Create a zipper for easy tree manipulation and cache population
  let initialZipper = TreeZipper rootWithChildren Nothing

  -- Create a forest with our test tree
  let forest = Map.fromList [("root", initialZipper)]

  -- Calculate shares to populate caches
  let sharesMap = sharesOfGeneralFulfillmentMap forest initialZipper
      provSharesMap = providerShares forest initialZipper 2
      rootNode' = zipperCurrent initialZipper

  -- Verify cache population (debug)
  putStrLn $ "SOGF Map: " ++ show sharesMap
  putStrLn $ "Provider Shares Map: " ++ show provSharesMap
  putStrLn $ "Root PC: " ++ show (nodePersistentCache rootNode')

  return rootNode'

-- Run persistence tests
testPersistence :: IO ()
testPersistence = do
  putStrLn "=== Testing Persistence Functions ==="

  -- Create in-memory data source for testing
  ds <- createInMemoryDataSource

  -- Create a test tree and populate caches
  putStrLn "Creating test tree..."
  testTree <- createTestTree

  -- Save the tree to persistence
  putStrLn "Saving tree to persistence..."
  saveResult <- persistTreeState ds testTree
  case saveResult of
    Left err -> putStrLn $ "Error saving tree: " ++ show err
    Right () -> do
      putStrLn "Tree saved successfully!"

      -- Load the tree from persistence
      putStrLn "Loading tree from persistence..."
      loadResult <- loadTreeState ds "root"
      case loadResult of
        Left err -> putStrLn $ "Error loading tree: " ++ show err
        Right loadedTree -> do
          putStrLn "Tree loaded successfully!"

          -- Verify persistent cache data
          let originalPC = nodePersistentCache testTree
              loadedPC = nodePersistentCache loadedTree

          putStrLn $ "Original persistent cache: " ++ show originalPC
          putStrLn $ "Loaded persistent cache: " ++ show loadedPC

          if loadedPC == originalPC
            then putStrLn "✓ Persistent cache loaded correctly!"
            else putStrLn "✗ Persistent cache mismatch!"

          -- Verify tree structure
          let originalChildren = Map.keys (nodeChildren testTree)
              loadedChildren = Map.keys (nodeChildren loadedTree)

          putStrLn $ "Original children: " ++ show originalChildren
          putStrLn $ "Loaded children: " ++ show loadedChildren

          if loadedChildren == originalChildren
            then putStrLn "✓ Tree structure loaded correctly!"
            else putStrLn "✗ Tree structure mismatch!"

  putStrLn "=== Testing Complete ==="

-- Update main to call testPersistence
main :: IO ()
main = do
  putStrLn "Running persistence tests..."
  testPersistence

--------------------------------
-- Persistent Cache Functions --
--------------------------------

-- Extract depth from provider shares key
depthFromKey :: String -> Maybe Int
depthFromKey key =
  if isPrefixOf "ps_" key
    then readMaybe (drop 3 key)
    else Nothing
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- Serializable instance for Node
instance Serializable Node where
  serialize node =
    -- A very simple implementation that just stores the ID and name
    -- In a real implementation, we would use a proper serialization library
    BS.pack $
      map (fromIntegral . fromEnum) $
        '{' : "id:" ++ nodeId node ++ ",name:" ++ nodeName node ++ "}"

  deserialize bs =
    -- Simple deserialization just extracts ID and name
    -- In a real implementation we'd use proper parsing
    let str = map (toEnum . fromIntegral) (BS.unpack bs) :: String
        content = case str of
          '{' : rest -> take (length rest - 1) rest -- Remove { and }
          _ -> ""
        idPrefix = "id:"
        namePrefix = ",name:"
        getId = case stripPrefix idPrefix content of
          Just rest -> takeWhile (/= ',') rest
          Nothing -> "unknown"
        getName = case stripPrefix namePrefix (dropWhile (/= ',') content) of
          Just rest -> rest
          Nothing -> "Unknown Node"
     in Just $ createRootNode getId getName 0 [] Nothing

-- Helper function to strip prefix
stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
  | otherwise = Nothing
stripPrefix _ _ = Nothing

-- Serializable instance for ShareMap
instance Serializable ShareMap where
  serialize shareMap =
    -- Basic encoding of a map as a string of key-value pairs
    BS.pack $
      map (fromIntegral . fromEnum) $
        '{' : Data.List.intercalate "," [k ++ ":" ++ show v | (k, v) <- Map.toList shareMap] ++ "}"

  deserialize bs =
    -- Basic decoding, in real implementation would use proper parser
    let str = map (toEnum . fromIntegral) (BS.unpack bs) :: String
        content = case str of
          '{' : rest -> take (length rest - 1) rest -- Remove { and }
          _ -> ""
        pairs = map parsePair (splitOn "," content)
     in Just $ Map.fromList pairs
    where
      parsePair :: String -> (String, Float)
      parsePair s = case splitOn ":" s of
        [k, v] -> (k, read v)
        _ -> ("unknown", 0.0)

      -- Helper to split a string on delimiter
      splitOn :: String -> String -> [String]
      splitOn delim str = case stripPrefix delim str of
        Just rest -> "" : splitOn delim rest
        Nothing -> case str of
          [] -> []
          (c : cs) ->
            let (first : rest) = splitOn delim cs
             in (c : first) : rest

-- Serializable instance for PersistentCache
instance Serializable PersistentCache where
  serialize pc =
    -- Encode SOGF map and provider shares
    BS.pack $
      map (fromIntegral . fromEnum) $
        '{'
          : "sogf:"
          ++ ( case pcSogfMap pc of
                 Nothing -> "null"
                 Just sm -> "{" ++ Data.List.intercalate "," [k ++ ":" ++ show v | (k, v) <- Map.toList sm] ++ "}"
             )
          ++ ",ps:"
          ++ "{"
          ++ Data.List.intercalate
            ","
            [ show depth
                ++ ":"
                ++ "{"
                ++ Data.List.intercalate "," [k ++ ":" ++ show v | (k, v) <- Map.toList sm]
                ++ "}"
              | (depth, sm) <- Map.toList (pcProviderShares pc)
            ]
          ++ "}"
          ++ "}"

  deserialize bs =
    -- Very basic implementation that just returns an empty cache
    -- In a real implementation, would parse the data properly
    Just emptyPersistentCache

-- A function to explicitly save a node's persistent cache
savePersistentCache :: PersistM () -> TreeZipper -> PersistM ()
savePersistentCache saveOp z = do
  let node = zipperCurrent z
      pc = nodePersistentCache node
      nodeKey = "nodes/" ++ nodeId node
      cacheKey = nodeKey ++ "/persistent_cache"

  -- Save the persistent cache
  writeEntity cacheKey pc

  -- Optional additional operation (like saving full node)
  saveOp

-- Function to load a node's persistent cache
loadPersistentCache :: String -> PersistM PersistentCache
loadPersistentCache nodeIdParam = do
  let cacheKey = "nodes/" ++ nodeIdParam ++ "/persistent_cache"
  maybeCache <- readEntity cacheKey
  pure $ Maybe.fromMaybe emptyPersistentCache maybeCache

-- Update a node with fresh persistent cache from database
updateNodeWithPersistentCache :: Node -> PersistM Node
updateNodeWithPersistentCache node = do
  pc <- loadPersistentCache (nodeId node)
  pure $ node {nodePersistentCache = pc}

-- Create a bridge function to update persistent cache within CacheM
updatePersistentCacheInCacheM :: String -> ShareMap -> TreeZipper -> CacheM String TreeZipper
updatePersistentCacheInCacheM key sm z = do
  -- This function runs in CacheM but doesn't actually modify the transient cache
  let node = zipperCurrent z
      pc = nodePersistentCache node

  -- Update the persistent cache based on the key
  let updatedPc =
        if key == "sogf_map"
          then setSogfMap sm pc
          else case depthFromKey key of
            Just depth -> setProviderShares depth sm pc
            Nothing -> pc

  -- Update the node with new persistent cache
  let updatedNode = node {nodePersistentCache = updatedPc}

  -- In a real implementation, we would also call:
  -- liftIO $ runPersistOp someDataSource $ do
  --   let cacheKey = "nodes/" ++ nodeId node ++ "/persistent_cache"
  --   writeEntity cacheKey updatedPc

  -- Return updated zipper
  pure $ z {zipperCurrent = updatedNode}

-- Helper function to create an empty cache of a specific type
emptyCacheOfType :: Cache String
emptyCacheOfType = emptyCache

-- Load a node from the persistence layer
loadNode :: String -> PersistM Node
loadNode nodeId = do
  -- Get node basic data
  let nodeKey = "nodes/" ++ nodeId
  maybeNode <- readEntity nodeKey

  case maybeNode of
    Just node -> do
      -- Load persistent cache for the node
      pc <- loadPersistentCache nodeId

      -- Return node with loaded persistent cache and empty transient cache
      pure $
        node
          { nodePersistentCache = pc,
            nodeTransientCache = emptyCacheOfType
          }
    Nothing -> throwError $ EntityNotFound $ "Node not found: " ++ nodeId

-- Save a node to the persistence layer
saveNode :: Node -> PersistM ()
saveNode node = do
  -- Save node basic data (without caches)
  let nodeKey = "nodes/" ++ nodeId node
      cacheKey = "nodes/" ++ nodeId node ++ "/persistent_cache"

  -- Save the node itself
  writeEntity nodeKey node

  -- Save the persistent cache separately
  writeEntity cacheKey (nodePersistentCache node)

-- Function to load an entire tree structure
loadTree :: String -> PersistM Node
loadTree rootId = do
  -- Load the root node
  rootNode <- loadNode rootId

  -- Load all children
  let childIds = Map.keys (nodeChildren rootNode)
  childMap <- loadChildren childIds

  -- Return the root node with loaded children
  pure $ rootNode {nodeChildren = childMap}
  where
    loadChildren :: [String] -> PersistM (Map.Map String Node)
    loadChildren ids = do
      children <- mapM loadTree ids
      pure $ Map.fromList [(nodeId child, child) | child <- children]

-- Save an entire tree structure
saveTree :: Node -> PersistM ()
saveTree node = do
  -- Save the node
  saveNode node

  -- Save all children recursively
  forM_ (Map.elems $ nodeChildren node) saveTree

-- | High-level function to save the entire tree with its cached state
-- | This would be called periodically or on important state changes
persistTreeState :: DataSource -> Node -> IO (Either PersistError ())
persistTreeState ds rootNode = runPersistOp ds $ saveTree rootNode

-- | High-level function to load the entire tree with its persisted cache
-- | This would be called on application startup
loadTreeState :: DataSource -> String -> IO (Either PersistError Node)
loadTreeState ds rootId = runPersistOp ds $ loadTree rootId

-- | Create a tree zipper from a node, for use after loading from persistence
-- | This initializes the zipper with the persisted node data
createZipperFromPersistedNode :: Node -> TreeZipper
createZipperFromPersistedNode node = TreeZipper node Nothing

-- | Update tree state with calculation results
-- | (Usually after a significant calculation that should be persisted)
updateTreeStateCache :: DataSource -> TreeZipper -> IO (Either PersistError TreeZipper)
updateTreeStateCache ds zipper =
  let node = zipperCurrent zipper
   in runPersistOp ds $ do
        -- Save just the node with its persistent cache
        saveNode node
        -- Return the zipper unchanged
        pure zipper

-- For Testing:

-- | Helper for creating an in-memory data source (useful for testing)
createMemoryDataSource :: IO DataSource
createMemoryDataSource = do
  stateRef <- newIORef Map.empty
  pure $
    DataSource
      { dsName = "memory",
        dsConnect = pure $ Right stateRef,
        dsDisconnect = const $ pure (),
        dsRead = \_ key -> do
          state <- readIORef stateRef
          pure $ Right $ Map.lookup key state,
        dsWrite = \_ key val -> do
          modifyIORef stateRef $ Map.insert key val
          pure $ Right (),
        dsDelete = \_ key -> do
          modifyIORef stateRef $ Map.delete key
          pure $ Right (),
        dsListKeys = \_ prefix -> do
          state <- readIORef stateRef
          pure $ Right $ filter (isPrefixOf prefix) $ Map.keys state,
        dsBatch = \_ ops -> do
          modifyIORef stateRef $ \s ->
            foldl' applyOp s ops
          pure $ Right ()
      }
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    applyOp state (key, Nothing) = Map.delete key state
    applyOp state (key, Just val) = Map.insert key val state

-- | In-memory data source for testing
-- | Useful for testing the persistence layer without a real database
createInMemoryDataSource :: IO DataSource
createInMemoryDataSource = do
  -- Create an empty storage map
  storageRef <- newIORef Map.empty

  -- Return a data source that operates on the in-memory map
  pure $
    DataSource
      { dsName = "test-memory",
        dsConnect = pure $ Right storageRef, -- Connect returns the IORef as connection
        dsDisconnect = \_ -> pure (), -- No-op for disconnect
        dsRead = \conn key -> do
          storage <- readIORef conn
          pure $ Right $ Map.lookup key storage,
        dsWrite = \conn key value -> do
          modifyIORef conn $ Map.insert key value
          pure $ Right (),
        dsDelete = \conn key -> do
          modifyIORef conn $ Map.delete key
          pure $ Right (),
        dsListKeys = \conn prefix -> do
          storage <- readIORef conn
          pure $ Right $ filter (isPrefixOf prefix) $ Map.keys storage,
        dsBatch = \conn ops -> do
          modifyIORef conn $ \s ->
            foldl' applyBatchOp s ops
          pure $ Right ()
      }
  where
    applyBatchOp state (key, Nothing) = Map.delete key state
    applyBatchOp state (key, Just val) = Map.insert key val state

-- | Helper to verify if the persistence layer saved the expected values
-- | Useful for testing or debugging
debugInspectStorage :: Connection -> String -> IO (Either PersistError (Maybe ByteString))
debugInspectStorage conn key = do
  case conn of
    storage -> do
      m <- readIORef storage
      pure $ Right $ Map.lookup key m
