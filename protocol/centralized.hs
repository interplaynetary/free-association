import Control.Monad (forM_, unless)
import Data.List (foldl', maximumBy, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Time (UTCTime)

-- Node type with maps for PS and SOGF directly
data Node = Node
  { nodeId :: String,
    nodeName :: String,
    nodePoints :: Int,
    nodeChildren :: Map.Map String Node,
    nodeContributors :: Set.Set String,
    nodeManualFulfillment :: Maybe Float,
    nodeCapacities :: CapacityInventory,
    nodeCapacityShares :: CapacityShares,
    nodeSOGFMap :: Maybe ShareMap,
    nodeProviderSharesMap :: Map.Map Int ShareMap
  }
  deriving (Show, Eq)

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
      -- Safely handle ancestors using pattern matching
      newContext = case ancestors of
        [] -> Nothing
        (ancestor : _) -> Just ancestor
   in Just $ TreeZipper newParent newContext

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
descendants = tail . getAllDescendants

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
      -- Initialize the maps
      nodeSOGFMap = Nothing,
      nodeProviderSharesMap = Map.empty
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
                -- Initialize the maps
                nodeSOGFMap = Nothing,
                nodeProviderSharesMap = Map.empty
              }
          clampManual = fmap (max 0 . min 1)

          -- When making a structural change, reset the maps
          updatedCurrent =
            current
              { nodeChildren = Map.insert name newChild (nodeChildren current),
                nodeSOGFMap = Nothing,
                nodeProviderSharesMap = Map.empty
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
          nodeSOGFMap = Nothing,
          nodeProviderSharesMap = Map.empty
        }

-- Helper to recursively delete a subtree
deleteSubtree :: TreeZipper -> TreeZipper
deleteSubtree z =
  modifyNode updateNode z
  where
    updateNode node =
      node
        { nodeChildren = Map.empty,
          nodeSOGFMap = Nothing,
          nodeProviderSharesMap = Map.empty
        }

-------------------------
-- Core Calculations --
-------------------------

-- Calculate total points from all children (Declarative)
totalChildPoints :: TreeZipper -> Int
totalChildPoints z@(TreeZipper current _) =
  -- Sum the points of all immediate children
  sum $ map (nodePoints . zipperCurrent) $ children z

-- Calculate a node's weight with caching (Declarative)
weight :: TreeZipper -> Float
weight z@(TreeZipper current _) =
  case zipperContext z of
    -- Root node always has weight 1.0
    Nothing -> 1.0
    -- Non-root nodes recursive weight calculation
    Just _ ->
      case exitToParent z of
        -- Guard against missing parent (shouldn't happen with valid zippers)
        Nothing -> 1.0 -- Default to 1.0 (full weight) if parent cannot be found
        Just parentZ ->
          let -- Get parent's total points
              total = totalChildPoints parentZ
              -- Calculate this node's contribution to parent (safely handling division by zero)
              nodeContribution =
                if total == 0
                  then 0
                  else fromIntegral (nodePoints current) / fromIntegral total
              -- Weight is recursive - multiply by parent's weight
              parentWeight = weight parentZ
           in nodeContribution * parentWeight

-- Calculate a node's share of its parent (Declarative)
shareOfParent :: TreeZipper -> Float
shareOfParent z =
  case exitToParent z of
    -- Root node has 100% share
    Nothing -> 1.0
    -- Calculate share for non-root nodes
    Just parent ->
      let parentTotal = totalChildPoints parent
          currentPoints = nodePoints $ zipperCurrent z
       in if parentTotal == 0
            then 0
            else fromIntegral currentPoints / fromIntegral parentTotal

-- Calculate the proportion of total child points from contribution children (Declarative)
contributionChildrenWeight :: TreeZipper -> Float
contributionChildrenWeight z =
  -- Define specific weights for each child category
  contribWeightSum / totalWeightSum
  where
    -- All child zippers
    childZippers = children z

    -- Partition children into contribution and non-contribution nodes
    (contribChildren, nonContribChildren) = partition isContribution childZippers

    -- Calculate weights for each category
    contribWeights = map weight contribChildren
    allWeights = map weight childZippers

    -- Sum the weights (safely handle empty lists)
    contribWeightSum = sum contribWeights
    totalWeightSum = sum allWeights

    -- Handle division by zero
    contribWeightSum / totalWeightSum =
      if totalWeightSum == 0 then 0 else contribWeightSum / totalWeightSum

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

-- Calculate the fulfillment from non-contribution children (Declarative)
nonContributionChildrenFulfillment :: TreeZipper -> Float
nonContributionChildrenFulfillment z =
  -- Weighted average of non-contribution children's fulfillment
  if null nonContribChildren || totalWeight == 0
    then 0
    else weightedSum / totalWeight
  where
    -- Get all child zippers
    childZippers = children z

    -- Filter to non-contribution children only
    nonContribChildren = filter (not . isContribution) childZippers

    -- Get weight and fulfillment for each child
    childWeights = map weight nonContribChildren
    childFulfillments = map fulfilled nonContribChildren

    -- Calculate weighted sum and total weight
    weightedProducts = zipWith (*) childWeights childFulfillments
    weightedSum = sum weightedProducts
    totalWeight = sum childWeights

-- Safely get instances of a contributor
getContributorInstances :: Forest -> String -> Set.Set TreeZipper
getContributorInstances contribIndex contribId =
  let emptyNode = createRootNode contribId contribId 0 [] Nothing
      emptyZipper = TreeZipper emptyNode Nothing
      defaultResult = Set.singleton emptyZipper
   in Set.singleton $ Map.findWithDefault emptyZipper contribId contribIndex

-- Safely get a contributor node
getContributorNode :: Forest -> String -> Maybe TreeZipper
getContributorNode contribIndex contribId =
  let instances = getContributorInstances contribIndex contribId
   in listToMaybe (Set.toList instances)

-- Calculate fulfillment with caching (Declarative)
fulfilled :: TreeZipper -> Float
fulfilled z@(TreeZipper current _) =
  computeFulfillment
  where
    -- Helper predicates and values localized in where clause
    hasManualFulfillment = isJust $ nodeManualFulfillment current
    isLeafNode = Map.null (nodeChildren current)
    isContribNode = isContribution z
    hasContribChildren = hasDirectContributionChild z
    hasNonContribChildren = hasNonContributionChild z

    -- Safely extract manual fulfillment value with a default
    getManualValue = Maybe.fromMaybe 0.0 $ nodeManualFulfillment current

    -- Weight a child's fulfillment by its share
    weightedChildFulfillment :: TreeZipper -> Float
    weightedChildFulfillment child = fulfilled child * shareOfParent child

    -- Calculate the manual contribution share
    manualContribShare :: Float
    manualContribShare =
      let contribWeight = contributionChildrenWeight z
          nonContribFulfillment = nonContributionChildrenFulfillment z
       in getManualValue * contribWeight + nonContribFulfillment * (1.0 - contribWeight)

    -- Main fulfillment calculation with pattern matching
    computeFulfillment
      -- Leaf contribution node
      | isLeafNode && isContribNode = 1.0
      -- Leaf non-contribution node
      | isLeafNode = 0.0
      -- Non-leaf node with manual fulfillment for contribution children only
      | hasManualFulfillment && hasContribChildren && not hasNonContribChildren =
          getManualValue
      -- Non-leaf node with mixed children types and manual fulfillment
      | hasManualFulfillment && hasContribChildren =
          manualContribShare
      -- Other nodes (aggregate from children)
      | otherwise =
          sum $ map weightedChildFulfillment $ children z

-- Calculate the desire (unfulfilled need) of a node
desire :: TreeZipper -> Float
desire z = 1.0 - fulfilled z

---------------------------
-- Mutual Fulfillment (Declarative) --
---------------------------
shareOfGeneralFulfillment :: Forest -> TreeZipper -> TreeZipper -> Float
shareOfGeneralFulfillment ci target contributor =
  -- Use Maybe monad to handle lookup failure gracefully
  Maybe.maybe 0 calculateFulfillment $ Map.lookup contributorId ci
  where
    contributorId = nodeId $ zipperCurrent contributor

    calculateFulfillment :: TreeZipper -> Float
    calculateFulfillment rootContributor =
      -- Sum the weighted contributions for each node
      sum weightedContributions
      where
        contribId = nodeId $ zipperCurrent rootContributor

        -- Find only nodes where this contributor is listed
        contributingNodes = filter isContributingNode (target : descendants target)

        isContributingNode :: TreeZipper -> Bool
        isContributingNode node =
          Set.member contribId (nodeContributors $ zipperCurrent node) && isContribution node

        -- Calculate weighted contribution for each node
        weightedContributions :: [Float]
        weightedContributions =
          [weightedFulfillment node | node <- contributingNodes]

        -- Calculate weighted fulfillment for a single node
        weightedFulfillment :: TreeZipper -> Float
        weightedFulfillment node =
          let nodeWeight = weight node
              nodeFulfillment = fulfilled node
              contributorCount = Set.size $ nodeContributors $ zipperCurrent node
           in nodeWeight * nodeFulfillment / fromIntegral contributorCount

getAllDescendants :: TreeZipper -> [TreeZipper]
getAllDescendants z@(TreeZipper current _) =
  -- This node plus all descendants
  z : concatMap getAllDescendants (children z)

-- Calculate mutual fulfillment between two nodes (Declarative)
mutualFulfillment :: Forest -> TreeZipper -> TreeZipper -> Float
mutualFulfillment ci a b =
  -- Mutual fulfillment is the minimum of shares each gives to the other
  min shareFromAToB shareFromBToA
  where
    -- Get node IDs
    aId = nodeId $ zipperCurrent a
    bId = nodeId $ zipperCurrent b

    -- Get share maps
    sharesFromA = sharesOfGeneralFulfillmentMap ci a
    sharesFromB = sharesOfGeneralFulfillmentMap ci b

    -- Extract shares with safe lookup (defaults to 0)
    shareFromAToB = Map.findWithDefault 0 bId sharesFromA
    shareFromBToA = Map.findWithDefault 0 aId sharesFromB

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
   in ( fst
          <$> maximumByMay (\a b -> compare (snd a) (snd b)) pathScores
      )

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
-- Normalized Shares-of-General-Fulfillment Map (Declarative)
-------------------------------------------------------
sharesOfGeneralFulfillmentMap :: Forest -> TreeZipper -> ShareMap
sharesOfGeneralFulfillmentMap ci z =
  -- Use stored map if available, otherwise calculate fresh
  Maybe.fromMaybe calculateFreshMap $ nodeSOGFMap $ zipperCurrent z
  where
    -- Calculate a fresh map by normalizing the raw shares
    calculateFreshMap = normalizeShareMap rawShareMap

    -- Filter out zero shares for efficiency
    rawShareMap = Map.fromList $ filter ((> 0) . snd) contributorShares

    -- Get all contributor pairs with their general fulfillment share
    contributorShares =
      [(nodeId (zipperCurrent c), shareOfGeneralFulfillment ci z c) | (_, c) <- Map.toList ci]

-----------------------------------------------------------------------
-- Provider-centric shares in a declarative style
-----------------------------------------------------------------------
providerShares :: Forest -> TreeZipper -> Int -> ShareMap
providerShares ci provider depth =
  -- Use stored map if available, otherwise calculate fresh
  Maybe.fromMaybe calculateFreshMap $ Map.lookup depth . nodeProviderSharesMap $ zipperCurrent provider
  where
    -- Calculate a fresh map for the given depth
    calculateFreshMap = normalizeShareMap $ computeSharesForDepth depth

    -- Pattern matching for different depth calculations
    computeSharesForDepth :: Int -> ShareMap
    computeSharesForDepth 1 = directContributorShares
    computeSharesForDepth d = transitiveShares d

    -- Direct contributor shares based on mutual fulfillment
    directContributorShares :: ShareMap
    directContributorShares =
      Map.fromList $ filter ((> 0) . snd) contributorMutualFulfillments
      where
        -- Find all contributors in the entire tree
        allContributingNodes = provider : descendants provider
        allContributorIds = Set.unions $ map (nodeContributors . zipperCurrent) allContributingNodes
        -- Get valid contributors that exist in the forest (safely)
        validContributors = catMaybes [Map.lookup cid ci | cid <- Set.toList allContributorIds]
        -- Calculate mutual fulfillment for each contributor
        contributorMutualFulfillments =
          [(nodeId (zipperCurrent c), mutualFulfillment ci provider c) | c <- validContributors]

    -- Transitive shares for depth > 1
    transitiveShares :: Int -> ShareMap
    transitiveShares d = foldl' combineTransitiveShares directContributorShares [2 .. d]

    -- For each contributor, factor in their direct shares
    combineTransitiveShares :: ShareMap -> Int -> ShareMap
    combineTransitiveShares currentShares _ =
      foldl' addWeightedRecipientShares Map.empty $ Map.toList currentShares
      where
        addWeightedRecipientShares :: ShareMap -> (String, Float) -> ShareMap
        addWeightedRecipientShares accMap (recipientId, share) =
          case Map.lookup recipientId ci of
            Nothing -> accMap
            Just recipientZ ->
              let recipientDirectShares = providerShares ci recipientZ 1
                  weightedShares = Map.map (* share) recipientDirectShares
               in Map.unionWith (+) accMap weightedShares

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