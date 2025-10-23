import Control.Monad (forM_, unless)
import Data.List (foldl', maximumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

----------------------
-- Tree References --
----------------------

data TreeRef = TreeRef
  { refPlayerId :: String,
    refTreeId :: String
  }
  deriving (Show, Eq, Ord)

mkTreeRef :: String -> String -> TreeRef
mkTreeRef = TreeRef

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
    nodeContributors :: Set.Set TreeRef,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    nodeCache :: Cache String
  }
  deriving (Show, Eq)

-- Cache keys
weightCacheKey :: String -> String
weightCacheKey nodeId = nodeId ++ "_weight"

fulfillmentCacheKey :: String -> String
fulfillmentCacheKey nodeId = nodeId ++ "_fulfillment"

mutualCacheKey :: TreeRef -> TreeRef -> String
mutualCacheKey ref1 ref2 =
  let key1 = refPlayerId ref1 ++ ":" ++ refTreeId ref1
      key2 = refPlayerId ref2 ++ ":" ++ refTreeId ref2
   in if key1 < key2
        then key1 ++ "_mutual_" ++ key2
        else key2 ++ "_mutual_" ++ key1

descendantsCacheKey :: String -> String
descendantsCacheKey nodeId = nodeId ++ "_descendants"

totalPointsCacheKey :: String -> String
totalPointsCacheKey nodeId = nodeId ++ "_total_points"

-- Global cache for provider shares
newtype ProviderSharesCache = ProviderSharesCache {sharesCache :: Cache (String, Int)}

-- Initialize an empty provider shares cache
emptyProviderSharesCache :: ProviderSharesCache
emptyProviderSharesCache = ProviderSharesCache emptyCache

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

-- Player type to hold local forest
data Player = Player
  { playerId :: String,
    playerForest :: Map.Map String TreeZipper,
    playerShares :: ShareCache,
    playerMutualFulfillments :: Cache String
  }
  deriving (Show)

-- Share cache type
data ShareCache = ShareCache
  { sharesByProvider :: Map.Map String ShareMap,  -- key: ${providerId}:${depth}
    capacityShares :: Map.Map String CapacityShares  -- key: ${providerId}:${capacityId}
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

-- Add a child and update caches
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
      key = totalPointsCacheKey (nodeId current)
      currentCache = nodeCache current
      currentTotal = case cacheLookup key currentCache of
        Just (value, _) -> Maybe.fromMaybe 0 (toInt value)
        Nothing -> 0
      newTotal = currentTotal + getPoints pts
      newCache = cacheInsert key (fromInt newTotal) currentCache
      clearedCache = emptyCache
      updatedCurrent =
        current
          { nodeChildren = Map.insert name newChild (nodeChildren current),
            nodeCache = clearedCache
          }
   in z {zipperCurrent = updatedCurrent}

-------------------------
-- Player Operations --
-------------------------

-- Create a new player
createPlayer :: String -> Player
createPlayer pid = Player
  { playerId = pid,
    playerForest = Map.empty,
    playerShares = ShareCache Map.empty Map.empty,
    playerMutualFulfillments = emptyCache
  }

-- Add a tree and all its children to a player's forest
addToForest :: Player -> TreeZipper -> Player
addToForest player z =
  let current = zipperCurrent z
      childNodes = Map.toList $ nodeChildren current
      -- First add the root node
      playerWithRoot = player { playerForest = Map.insert (nodeId current) z (playerForest player) }
      -- Then recursively add all children
      playerWithChildren = foldl' addChildToForest playerWithRoot childNodes
  in playerWithChildren
  where
    addChildToForest p (childId, childNode) =
      let childZipper = TreeZipper childNode (Just $ Ctx (zipperCurrent z) Map.empty [])
      in p { playerForest = Map.insert childId childZipper (playerForest p) }

-- Replace a tree in player's forest
replaceInForest :: Player -> TreeZipper -> Player
replaceInForest player z = player
  { playerForest = Map.insert (nodeId $ zipperCurrent z) z (playerForest player)
  }

-- Get a tree from a player's forest
getTree :: Player -> String -> Maybe TreeZipper
getTree player treeId = Map.lookup treeId (playerForest player)

-- Create a TreeRef for a tree in this player's forest
mkLocalTreeRef :: Player -> String -> TreeRef
mkLocalTreeRef player treeId = mkTreeRef (playerId player) treeId

-------------------------
-- Core Calculations --
-------------------------

-- Get a tree using a TreeRef
getTreeByRef :: Player -> TreeRef -> Maybe TreeZipper
getTreeByRef player ref = 
  if refPlayerId ref == playerId player
    then Map.lookup (refTreeId ref) (playerForest player)
    else Nothing -- In a real implementation, this would fetch from the network

-- Calculate total points from all children
totalChildPoints :: Player -> TreeRef -> Int
totalChildPoints player ref =
  case getTreeByRef player ref of
    Nothing -> 0
    Just zipper ->
      let current = zipperCurrent zipper
          cache = nodeCache current
          cacheKey = totalPointsCacheKey (nodeId current)
      in case cacheLookup cacheKey cache of
          Just (value, updatedCache) ->
            case toInt value of
              Just total -> total
              Nothing -> computeAndCache cache cacheKey zipper
          Nothing -> computeAndCache cache cacheKey zipper
  where
    computeAndCache cache key zipper =
      let total = sum $ map (getPoints . nodePoints) $ Map.elems (nodeChildren $ zipperCurrent zipper)
          newCache = cacheInsert key (fromInt total) cache
      in total

-- Calculate a node's weight with caching
weight :: Player -> TreeRef -> Float
weight player ref =
  case getTreeByRef player ref of
    Nothing -> 0
    Just z ->
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
                    let w = computeWeight current ctx
                        newCache = cacheInsert cacheKey (fromFloat w) updatedCache
                    in w
              Nothing ->
                let w = computeWeight current ctx
                    newCache = cacheInsert cacheKey (fromFloat w) (nodeCache current)
                in w
  where
    computeWeight node ctx
      | total == 0 = 0
      | otherwise = fromIntegral currentPoints / fromIntegral total * parentWeight
      where
        parent = ctxParent ctx
        parentRef = mkTreeRef (refPlayerId ref) (nodeId parent)
        total = totalChildPoints player parentRef
        currentPoints = getPoints (nodePoints node)
        parentWeight = weight player parentRef

-- Calculate mutual fulfillment between two nodes with caching
mutualFulfillment :: Player -> TreeRef -> TreeRef -> Float
mutualFulfillment player aRef bRef =
  case getTreeByRef player aRef of
    Nothing -> 0
    Just a ->
      let aNode = zipperCurrent a
          cacheKey = mutualCacheKey aRef bRef
       in case cacheLookup cacheKey (nodeCache aNode) of
            Just (value, updatedCache) ->
              case toFloat value of
                Just v -> v
                Nothing -> 
                  let v = compute ()
                      newNode = aNode { nodeCache = cacheInsert cacheKey (fromFloat v) updatedCache }
                      newZipper = a { zipperCurrent = newNode }
                  in v
            Nothing ->
              let v = compute ()
                  newNode = aNode { nodeCache = cacheInsert cacheKey (fromFloat v) (nodeCache aNode) }
                  newZipper = a { zipperCurrent = newNode }
              in v
  where
    compute _ =
      let aToB = shareOfGeneralFulfillment player aRef bRef
          bToA = shareOfGeneralFulfillment player bRef aRef
       in min aToB bToA

-- Calculate the share of general fulfillment
shareOfGeneralFulfillment :: Player -> TreeRef -> TreeRef -> Float
shareOfGeneralFulfillment player targetRef contributorRef =
  case getTreeByRef player contributorRef of
    Nothing -> 0
    Just contributor ->
      case getTreeByRef player targetRef of
        Nothing -> 0
        Just target ->
          let contribId = refTreeId contributorRef
              -- Only consider contribution nodes (non-root with contributors)
              contributingNodes =
                filter
                  ( \node ->
                      let nodeContribs = nodeContributors (zipperCurrent node)
                          -- Just check if the contributor is in the set
                          isContributor = Set.member contributorRef nodeContribs
                      in isContributor && isContribution player (mkTreeRef (refPlayerId targetRef) (nodeId $ zipperCurrent node))
                  )
                  (getAllDescendantsCached target)
              -- Calculate total contribution from these nodes
              total = sum [weight player (mkTreeRef (refPlayerId targetRef) (nodeId $ zipperCurrent node)) * 
                          fulfilled player (mkTreeRef (refPlayerId targetRef) (nodeId $ zipperCurrent node)) | 
                          node <- contributingNodes]
              -- Get number of contributors for each node
              contributorCounts = map (Set.size . nodeContributors . zipperCurrent) contributingNodes
              -- Divide each node's contribution by its number of contributors
              weightedTotal =
                sum
                  [ w * f / fromIntegral c
                    | (w, f, c) <-
                        zip3
                          (map (weight player . mkTreeRef (refPlayerId targetRef) . nodeId . zipperCurrent) contributingNodes)
                          (map (fulfilled player . mkTreeRef (refPlayerId targetRef) . nodeId . zipperCurrent) contributingNodes)
                          contributorCounts
                  ]
           in if total == 0 then 0 else weightedTotal / total

-- Calculate fulfillment with caching
fulfilled :: Player -> TreeRef -> Float
fulfilled player ref =
    case getTreeByRef player ref of
        Nothing -> 0
        Just z ->
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
                if isContribution player ref then 1.0 else 0.0
            | isJust (nodeManualFulfillment node) && hasDirectContributionChild player ref =
                if not (hasNonContributionChild player ref)
                    then fromJust $ nodeManualFulfillment node
                    else manualContribShare
            | otherwise =
                sum
                    [ fulfilled player childRef * shareOfParent player childRef
                    | childId <- Map.keys (nodeChildren node),
                      let childRef = mkTreeRef (refPlayerId ref) childId
                    ]
            where
                manualContribShare =
                    let contribWeight = contributionChildrenWeight player ref
                        nonContribFulfillment = nonContributionChildrenFulfillment player ref
                    in fromJust (nodeManualFulfillment node) * contribWeight
                         + nonContribFulfillment * (1.0 - contribWeight)

-- Check if a node has direct contribution children
hasDirectContributionChild :: Player -> TreeRef -> Bool
hasDirectContributionChild player ref =
  case getTreeByRef player ref of
    Nothing -> False
    Just z ->
      let current = zipperCurrent z
          childRefs = [mkTreeRef (refPlayerId ref) childId | childId <- Map.keys (nodeChildren current)]
       in any (isContribution player) childRefs

-- Check if a node has non-contribution children
hasNonContributionChild :: Player -> TreeRef -> Bool
hasNonContributionChild player ref =
  case getTreeByRef player ref of
    Nothing -> False
    Just z ->
      let current = zipperCurrent z
          childRefs = [mkTreeRef (refPlayerId ref) childId | childId <- Map.keys (nodeChildren current)]
       in not (all (isContribution player) childRefs)

-- Calculate share of parent
shareOfParent :: Player -> TreeRef -> Float
shareOfParent player ref =
  case getTreeByRef player ref of
    Nothing -> 0
    Just z ->
      case zipperContext z of
        Nothing -> 1.0 -- Root node has 100% share
        Just ctx ->
          let parentRef = mkTreeRef (refPlayerId ref) (nodeId $ ctxParent ctx)
              total = totalChildPoints player parentRef
              currentPoints = getPoints (nodePoints $ zipperCurrent z)
           in if total == 0 then 0 else fromIntegral currentPoints / fromIntegral total

-- Updated to use TreeRef
isContribution :: Player -> TreeRef -> Bool
isContribution player ref =
  case getTreeByRef player ref of
    Nothing -> False
    Just z -> 
      let current = zipperCurrent z
          hasContributors = not (Set.null (nodeContributors current))
          isRoot = isNothing (zipperContext z)
      in hasContributors && not isRoot  -- A node is a contribution if it has contributors and is not the root

-- Calculate the proportion of total child points from contribution children
contributionChildrenWeight :: Player -> TreeRef -> Float
contributionChildrenWeight player ref =
  case getTreeByRef player ref of
    Nothing -> 0
    Just z ->
      let current = zipperCurrent z
          childRefs = [mkTreeRef (refPlayerId ref) childId | childId <- Map.keys (nodeChildren current)]
          (contribWeight, totalWeight) = foldl' accumWeight (0, 0) childRefs
       in if totalWeight == 0 then 0 else contribWeight / totalWeight
  where
    accumWeight (cw, tw) childRef =
      let w = weight player childRef
       in if isContribution player childRef
            then (cw + w, tw + w)
            else (cw, tw + w)

-- Sum fulfillment from children matching a predicate
childrenFulfillment :: Player -> (TreeRef -> Bool) -> TreeRef -> Float
childrenFulfillment player pred ref =
    case getTreeByRef player ref of
        Nothing -> 0
        Just z ->
            sum
                [ fulfilled player childRef * shareOfParent player childRef
                | childId <- Map.keys (nodeChildren $ zipperCurrent z),
                  let childRef = mkTreeRef (refPlayerId ref) childId,
                  pred childRef
                ]

-- Calculate the fulfillment from contribution children
contributionChildrenFulfillment :: Player -> TreeRef -> Float
contributionChildrenFulfillment player = childrenFulfillment player (isContribution player)

-- Calculate the fulfillment from non-contribution children
nonContributionChildrenFulfillment :: Player -> TreeRef -> Float
nonContributionChildrenFulfillment player ref =
  case getTreeByRef player ref of
    Nothing -> 0
    Just z ->
      let current = zipperCurrent z
          childRefs = [mkTreeRef (refPlayerId ref) childId | childId <- Map.keys (nodeChildren current)]
          nonContribChildren = filter (not . isContribution player) childRefs
          weights = map (weight player) nonContribChildren
          fulfillments = map (fulfilled player) nonContribChildren
          weightedFulfillments = zipWith (*) weights fulfillments
          totalWeight = sum weights
       in if totalWeight == 0
            then 0
            else sum weightedFulfillments / totalWeight

-- Calculate the desire (unfulfilled need) of a node
desire :: Player -> TreeRef -> Float
desire player ref = 1.0 - fulfilled player ref

---------------------------
-- Mutual Fulfillment --
---------------------------

-- Get a contributor's root node
getContributorRoot :: Player -> TreeRef -> Maybe TreeZipper
getContributorRoot player ref = 
  if refPlayerId ref == playerId player
    then Map.lookup (refTreeId ref) (playerForest player)
    else Nothing -- In a real implementation, this would fetch from the network

-- Get all descendants with caching
getAllDescendantsCached :: TreeZipper -> [TreeZipper]
getAllDescendantsCached z =
  let current = zipperCurrent z
      currentId = nodeId current
      cache = nodeCache current
      cacheKey = descendantsCacheKey currentId
   in case cacheLookup cacheKey cache of
        Just (value, _) ->
          case toStringList value of
            Just _ -> getAllDescendants z  -- Cache hit, but we still need to rebuild the tree
            Nothing -> computeAndCache cache cacheKey
        Nothing -> computeAndCache cache cacheKey
  where
    computeAndCache cache key =
      let descendants = getAllDescendants z
          descendantIds = map (nodeId . zipperCurrent) descendants
          newCache = cacheInsert key (fromStringList descendantIds) cache
       in descendants

getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z = z : concatMap (maybe [] getAllDescendants) childZippers
  where
    childZippers = map (`enterChild` z) (Map.keys $ nodeChildren $ zipperCurrent z)

------------------------------------------
-- Provider-centric share calculation --
------------------------------------------

type ShareMap = Map.Map String Float

type VisitedSet = Set.Set String

-- Core share calculation function that handles all depths
providerShares :: Player -> TreeRef -> Int -> ShareMap
providerShares player providerRef depth
  | depth <= 1 = initialShares
  | otherwise = normalizeShares finalShares
  where
    initialShares =
      case getTreeByRef player providerRef of
        Nothing -> Map.empty
        Just z ->
          let contributors = nodeContributors $ zipperCurrent z
              mutualValues = [(refTreeId ref, mutualFulfillment player providerRef ref) | ref <- Set.toList contributors]
              total = sum $ map snd mutualValues
          in if total == 0
             then Map.empty
             else Map.fromList [(id, val / total) | (id, val) <- mutualValues]

    (finalShares, _) = foldl' (processDepth player) (initialShares, Set.empty) [2 .. depth]

    normalizeShares shares =
      let totalShare = sum $ Map.elems shares
      in if totalShare > 0
         then Map.map (/ totalShare) shares
         else shares

-- Process additional depths for transitive share calculation
processDepth :: Player -> (ShareMap, VisitedSet) -> Int -> (ShareMap, VisitedSet)
processDepth player (shares, visited) _ = 
    let recipients = Map.keys shares
        unvisitedRecipients = filter (\r -> not (Set.member r visited)) recipients
    in foldl' processRecipient (shares, visited) unvisitedRecipients
    where
        processRecipient :: (ShareMap, VisitedSet) -> String -> (ShareMap, VisitedSet)
        processRecipient (currentShares, currentVisited) recipientId
            | not hasShare = (currentShares, currentVisited)
            | not hasRecipient = (currentShares, currentVisited)
            | otherwise = (Map.unionWith (+) currentShares weightedShares, newVisited)
            where
                hasShare = Map.member recipientId currentShares
                recipientShare = fromJust $ Map.lookup recipientId currentShares
                recipientRef = mkTreeRef (playerId player) recipientId
                hasRecipient = isJust $ getTreeByRef player recipientRef
                newVisited = Set.insert recipientId currentVisited
                transitiveShares = providerShares player recipientRef 1
                weightedShares = Map.map (* recipientShare) transitiveShares

-- Provider-centric share calculation with caching
providerSharesCached :: ProviderSharesCache -> Player -> TreeRef -> Int -> (ShareMap, ProviderSharesCache)
providerSharesCached cache player providerRef maxDepth =
  let key = (refTreeId providerRef, maxDepth)
      currentCache = sharesCache cache
   in case cacheLookup key currentCache of
        Just (value, updatedCache) ->
          case toShareMap value of
            Just shares -> (shares, cache {sharesCache = updatedCache})
            Nothing -> computeAndCache key currentCache
        Nothing -> computeAndCache key currentCache
  where
    computeAndCache key currentCache =
      let shares = providerShares player providerRef maxDepth
          newCache = cacheInsert key (fromShareMap shares) currentCache
       in (shares, cache {sharesCache = newCache})

-- Simplified interface functions that use providerShares
directShare :: Player -> TreeRef -> String -> Float
directShare player provider recipientId =
  Map.findWithDefault 0 recipientId $ providerShares player provider 1

-- Get a receiver's share from a specific capacity provider
receiverShareFrom :: Player -> TreeRef -> TreeRef -> Maybe Capacity -> Int -> Float
receiverShareFrom player receiver provider maybeCapacity maxDepth =
  let providerShareMap = providerShares player provider maxDepth
      receiverId = refTreeId receiver
   in Map.findWithDefault 0 receiverId providerShareMap

-- Get a person's total share in a specific capacity
getPersonalCapacityShare :: Player -> TreeRef -> Maybe Capacity -> Float
getPersonalCapacityShare player personRef maybeCapacity =
    case maybeCapacity of
        Nothing -> 0
        Just capacity ->
            case getTreeByRef player personRef of
                Nothing -> 0
                Just _ ->
                    let capacityOwners =
                            [ ownerRef
                            | (ownerId, owner) <- Map.toList (playerForest player),
                              Map.member (capacityId capacity) (nodeCapacities $ zipperCurrent owner),
                              let ownerRef = mkTreeRef (playerId player) ownerId
                            ]
                        shares = [receiverShareFrom player personRef ownerRef (Just capacity) 2 | ownerRef <- capacityOwners]
                    in if null shares then 0 else maximum shares

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
-- Example Usage --
-----------------------
exampleForest :: (Player, Player, Player)
exampleForest = (finalAlice, finalBob, finalCharlie)
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
                percentageDiv = 0.1 -- Maximum 10% share
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

    -- Create initial players
    alice = createPlayer "alice"
    bob = createPlayer "bob"
    charlie = createPlayer "charlie"

    -- Create TreeRefs for mutual contributors
    aliceRef = mkTreeRef "alice" "alice"
    bobRef = mkTreeRef "bob" "bob"
    charlieRef = mkTreeRef "charlie" "charlie"

    -- Create root nodes with mutual contributors
    aliceNode = createRootNode "alice" "Alice" (Points 100) [bobRef, charlieRef] Nothing
    bobNode = createRootNode "bob" "Bob" (Points 100) [aliceRef, charlieRef] Nothing
    charlieNode = createRootNode "charlie" "Charlie" (Points 100) [aliceRef, bobRef] Nothing

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
    aliceWithChild = addChild "alice_child" (Points 30) [bobRef, charlieRef] (Just 0.8) aliceWithCapacity
    bobWithChild = addChild "bob_child" (Points 40) [aliceRef, charlieRef] (Just 0.7) bobWithCapacity
    charlieWithChild = addChild "charlie_child" (Points 50) [aliceRef, bobRef] (Just 0.9) charlieRoot

    bobWithShare = addCapacityShare "alice_room" aliceRoomShare bobWithChild
    charlieWithShare = addCapacityShare "bob_pie" bobPieShare charlieWithChild

    -- Update players with their final trees
    finalAlice = addToForest alice aliceWithChild
    finalBob = addToForest bob bobWithShare
    finalCharlie = addToForest charlie charlieWithShare

main :: IO ()
main = do
  let (alice, bob, charlie) = exampleForest
      -- Create TreeRefs for the players
      aliceRef = mkTreeRef "alice" "alice"
      bobRef = mkTreeRef "bob" "bob"
      charlieRef = mkTreeRef "charlie" "charlie"
      aliceChildRef = mkTreeRef "alice" "alice_child"
      bobChildRef = mkTreeRef "bob" "bob_child"

  -- Debug output for Alice's tree structure
  putStrLn "Debug: Alice's Tree Structure"
  case getTreeByRef alice aliceRef of
    Nothing -> putStrLn "  Alice's tree not found"
    Just z -> do
      let current = zipperCurrent z
      putStrLn $ "  Root node: " ++ nodeId current
      putStrLn $ "  Contributors: " ++ show (Set.toList $ nodeContributors current)
      putStrLn $ "  Children: " ++ show (Map.keys $ nodeChildren current)
      putStrLn $ "  Is contribution: " ++ show (isContribution alice aliceRef)
      let descendants = getAllDescendantsCached z
      putStrLn $ "  Number of descendants: " ++ show (length descendants)
      putStrLn $ "  Descendant IDs: " ++ show (map (nodeId . zipperCurrent) descendants)
      
      -- Debug child node
      putStrLn "\nDebug: Alice's Child Node"
      putStrLn $ "  Is contribution: " ++ show (isContribution alice aliceChildRef)
      case getTreeByRef alice aliceChildRef of
        Nothing -> putStrLn "  Child node not found"
        Just childZ -> do
          let childNode = zipperCurrent childZ
          putStrLn $ "  Contributors: " ++ show (Set.toList $ nodeContributors childNode)
          putStrLn $ "  Manual fulfillment: " ++ show (nodeManualFulfillment childNode)
          putStrLn $ "  Weight: " ++ show (weight alice aliceChildRef)
          putStrLn $ "  Fulfillment: " ++ show (fulfilled alice aliceChildRef)

  -- Debug output for Bob's tree structure
  putStrLn "\nDebug: Bob's Tree Structure"
  case getTreeByRef bob bobRef of
    Nothing -> putStrLn "  Bob's tree not found"
    Just z -> do
      let current = zipperCurrent z
      putStrLn $ "  Root node: " ++ nodeId current
      putStrLn $ "  Contributors: " ++ show (Set.toList $ nodeContributors current)
      putStrLn $ "  Children: " ++ show (Map.keys $ nodeChildren current)
      putStrLn $ "  Is contribution: " ++ show (isContribution bob bobRef)
      let descendants = getAllDescendantsCached z
      putStrLn $ "  Number of descendants: " ++ show (length descendants)
      putStrLn $ "  Descendant IDs: " ++ show (map (nodeId . zipperCurrent) descendants)
      
      -- Debug child node
      putStrLn "\nDebug: Bob's Child Node"
      putStrLn $ "  Is contribution: " ++ show (isContribution bob bobChildRef)
      case getTreeByRef bob bobChildRef of
        Nothing -> putStrLn "  Child node not found"
        Just childZ -> do
          let childNode = zipperCurrent childZ
          putStrLn $ "  Contributors: " ++ show (Set.toList $ nodeContributors childNode)
          putStrLn $ "  Manual fulfillment: " ++ show (nodeManualFulfillment childNode)
          putStrLn $ "  Weight: " ++ show (weight bob bobChildRef)
          putStrLn $ "  Fulfillment: " ++ show (fulfilled bob bobChildRef)

  -- Print fulfillment values
  putStrLn "\nMutual Fulfillment Values:"
  let aliceBobMutual = mutualFulfillment alice aliceRef bobRef
      aliceToB = shareOfGeneralFulfillment alice aliceRef bobRef
      bToAlice = shareOfGeneralFulfillment bob bobRef aliceRef  -- Changed to use bob's perspective
  putStrLn $ "  Alice -> Bob: " ++ show aliceToB
  putStrLn $ "  Bob -> Alice: " ++ show bToAlice
  putStrLn $ "  Alice <-> Bob (min): " ++ show aliceBobMutual
  putStrLn $ "Alice <-> Bob: " ++ show (mutualFulfillment alice aliceRef bobRef)
  putStrLn $ "Alice <-> Charlie: " ++ show (mutualFulfillment alice aliceRef charlieRef)
  putStrLn $ "Bob <-> Charlie: " ++ show (mutualFulfillment bob bobRef charlieRef)

  -- Print capacity information
  putStrLn "\nCapacity Information:"

  -- Print Alice's room capacity
  putStrLn "Alice's Room Capacity:"
  let aliceRoom = case getTreeByRef alice aliceRef of
                    Just z -> Map.lookup "room1" (nodeCapacities $ zipperCurrent z)
                    Nothing -> Nothing
  case aliceRoom of
    Just room -> do
      putStrLn $ "  Quantity: " ++ show (quantity room) ++ " " ++ unit room

      -- Show provider-centric shares (Alice's perspective)
      putStrLn "\nProvider-centric shares (Alice distributing to network):"
      let aliceShares = providerShares alice aliceRef 2
          totalShares = sum $ Map.elems aliceShares
          bobShare = Map.findWithDefault 0 "bob" aliceShares * 100
          charlieShare = Map.findWithDefault 0 "charlie" aliceShares * 100
          otherShares = 100 - bobShare - charlieShare

      putStrLn $ "  Total shares distributed: " ++ show (totalShares * 100) ++ "%"
      putStrLn $ "  Bob's share from Alice: " ++ show bobShare ++ "%"
      putStrLn $ "  Charlie's share from Alice: " ++ show charlieShare ++ "%"
      putStrLn $ "  Remaining shares (distributed to other network members): " ++ show otherShares ++ "%"

      -- Show receiver perspective
      putStrLn "\nReceiver perspective (shares received from Alice):"
      let bobShare = receiverShareFrom alice bobRef aliceRef aliceRoom 2
          charlieShare = receiverShareFrom alice charlieRef aliceRef aliceRoom 2
      putStrLn $ "  Bob's share of Alice's room: " ++ show (bobShare * 100) ++ "%"
      putStrLn $ "  Charlie's share of Alice's room: " ++ show (charlieShare * 100) ++ "%"

      -- Show computed quantities
      putStrLn "\nComputed quantities for capacity shares:"
      let bobQuantity = case aliceRoom of
                         Just room -> bobShare * fromIntegral (quantity room)
                         Nothing -> 0
          charlieQuantity = case aliceRoom of
                            Just room -> charlieShare * fromIntegral (quantity room)
                            Nothing -> 0
      putStrLn $ "  Bob's portion: " ++ show bobQuantity ++ " " ++ unit room
      putStrLn $ "  Charlie's portion: " ++ show charlieQuantity ++ " " ++ unit room
    Nothing -> putStrLn "  No room capacity found"