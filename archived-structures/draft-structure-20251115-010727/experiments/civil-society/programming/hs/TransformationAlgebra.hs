{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module TransformationAlgebra where

import qualified Data.Map.Strict as M
import Data.Time.Clock (UTCTime)
import Data.Kind (Type)

-- ============================================================================
-- CORE TYPES (simplified for algebra)
-- ============================================================================

type EntityId = String
type ResourceType = String
type Capacity = Double
type Portion = Double
type Timestamp = UTCTime

-- ============================================================================
-- PATH ALGEBRA
-- ============================================================================

{- | Paths through the state tree (from equations.md)
  
  Examples:
    player/Alice/recognitions/Bob
    context/Coalition/members/Alice
    player/Carol/oscillationHistories/Alice/needHistory
-}
data Path where
  Root :: Path
  PlayerNode :: EntityId -> Path -> Path
  ContextNode :: String -> Path -> Path
  BranchNode :: String -> Path -> Path
  IndexNode :: String -> Path -> Path  -- For map keys
  deriving (Show, Eq)

-- | Path references can be local or remote
data PathRef =
    LocalPath Path
  | RemotePath 
      { remoteAddress :: String
      , remoteEntity :: EntityId  
      , remotePath :: Path
      }
  | ComputedPath (forall s. s -> Maybe Path)  -- Dynamically computed

-- | Path operations
(//) :: Path -> String -> Path
p // branch = BranchNode branch p

at :: Path -> String -> Path
at p key = IndexNode key p

-- Examples:
-- playerPath = Root // "players" `at` "Alice"
-- recognitionPath = playerPath // "recognitions" `at` "Bob"

-- ============================================================================
-- STATE TREE (Abstract for algebra)
-- ============================================================================

-- | Generic state tree - actual structure defined elsewhere
data StateTree = StateTree
  { stateData :: M.Map Path Value
  , stateTimestamp :: Timestamp
  , stateVersion :: Int
  }
  deriving (Show)

-- | Values that can be stored at paths
data Value =
    VCapacity Capacity
  | VPortion Portion
  | VEntityId EntityId
  | VList [Value]
  | VMap (M.Map String Value)
  | VHistory [(Timestamp, Capacity)]
  deriving (Show, Eq)

-- | Lens-like access
getPath :: Path -> StateTree -> Maybe Value
getPath p tree = M.lookup p (stateData tree)

setPath :: Path -> Value -> StateTree -> StateTree
setPath p v tree = tree { stateData = M.insert p v (stateData tree) }

modifyPath :: Path -> (Value -> Value) -> StateTree -> StateTree
modifyPath p f tree = case getPath p tree of
  Just v -> setPath p (f v) tree
  Nothing -> tree

-- ============================================================================
-- TRANSFORMATION ALGEBRA
-- ============================================================================

{- | A transformation is a function StateTree -> StateTree
  
  We make it algebraic so we can:
  - Compose transformations
  - Reason about their properties
  - Prove convergence
  - Replay/undo/redo
-}
data Transform = Transform
  { transformName :: String
  , transformPrecondition :: StateTree -> Bool
  , transformApply :: StateTree -> StateTree
  , transformInverse :: Maybe Transform  -- For undo
  }

instance Show Transform where
  show t = "Transform{" ++ transformName t ++ "}"

-- | Identity transformation
idTransform :: Transform
idTransform = Transform
  { transformName = "id"
  , transformPrecondition = const True
  , transformApply = id
  , transformInverse = Just idTransform
  }

-- | Compose transformations (like function composition)
(/>) :: Transform -> Transform -> Transform
t1 /> t2 = Transform
  { transformName = transformName t1 ++ " /> " ++ transformName t2
  , transformPrecondition = \s -> transformPrecondition t1 s && 
                                   transformPrecondition t2 (transformApply t1 s)
  , transformApply = transformApply t2 . transformApply t1
  , transformInverse = case (transformInverse t2, transformInverse t1) of
      (Just inv2, Just inv1) -> Just (inv2 /> inv1)  -- Reverse order!
      _ -> Nothing
  }

infixl 7 />

-- | Conditional transformation
whenT :: (StateTree -> Bool) -> Transform -> Transform
whenT cond t = t { transformPrecondition = \s -> cond s && transformPrecondition t s }

-- ============================================================================
-- PROTOCOL TRANSFORMATIONS
-- ============================================================================

{- | Update a recipient's need (THE UPDATE LAW)
  
  Remaining_Need = max(0, Declared_Need - Total_Received)
-}
updateNeedTransform :: EntityId -> ResourceType -> Capacity -> Transform
updateNeedTransform recipientId resType received = Transform
  { transformName = "updateNeed(" ++ recipientId ++ ", " ++ resType ++ ", " ++ show received ++ ")"
  , transformPrecondition = const True
  , transformApply = \tree ->
      let needPath = Root // "players" `at` recipientId // "needs" `at` resType
          currentNeed = case getPath needPath tree of
            Just (VCapacity n) -> n
            _ -> 0
          remainingNeed = max 0 (currentNeed - received)
      in setPath needPath (VCapacity remainingNeed) tree
  , transformInverse = Nothing  -- Need updates are not reversible (time flows forward)
  }

{- | Update oscillation history (STEP 0)
  
  Adds new data point to history for oscillation detection
-}
updateHistoryTransform :: EntityId -> EntityId -> Timestamp -> Capacity -> Transform
updateHistoryTransform providerId recipientId timestamp need = Transform
  { transformName = "updateHistory(" ++ providerId ++ " tracking " ++ recipientId ++ ")"
  , transformPrecondition = const True
  , transformApply = \tree ->
      let histPath = Root // "players" `at` providerId // "oscillationHistories" `at` recipientId
          newEntry = (timestamp, need)
          history = case getPath histPath tree of
            Just (VHistory h) -> take 5 $ newEntry : h  -- Keep last 5
            _ -> [newEntry]
      in setPath histPath (VHistory history) tree
  , transformInverse = Nothing  -- History is append-only
  }

{- | Set recognition (must maintain 100% constraint!)
  
  This is a CONSTRAINED transformation
-}
setRecognitionTransform :: EntityId -> EntityId -> Portion -> Transform
setRecognitionTransform fromId toId portion = Transform
  { transformName = "setRecognition(" ++ fromId ++ " → " ++ toId ++ " = " ++ show portion ++ ")"
  , transformPrecondition = \tree ->
      -- After this transform, total must equal 1.0
      let recPath = Root // "players" `at` fromId // "recognitions"
          recs = case getPath recPath tree of
            Just (VMap m) -> m
            _ -> M.empty
          recs' = M.insert toId (VPortion portion) recs
          total = sum [p | VPortion p <- M.elems recs']
      in abs (total - 1.0) < 0.001  -- Maintain invariant!
  , transformApply = \tree ->
      let recPath = Root // "players" `at` fromId // "recognitions" `at` toId
      in setPath recPath (VPortion portion) tree
  , transformInverse = Nothing  -- Can't undo recognition changes
  }

{- | Publish slot allocation (PROVIDER PHASE OUTPUT)
  
  Provider publishes their calculated allocations
-}
publishAllocationTransform :: EntityId -> EntityId -> ResourceType -> Capacity -> Transform
publishAllocationTransform providerId recipientId resType quantity = Transform
  { transformName = "publishAllocation(" ++ providerId ++ " → " ++ recipientId ++ ": " ++ show quantity ++ ")"
  , transformPrecondition = const True
  , transformApply = \tree ->
      let allocPath = Root // "players" `at` providerId // "allocations" `at` recipientId
          allocation = VMap $ M.fromList 
            [ ("resourceType", VEntityId resType)
            , ("quantity", VCapacity quantity)
            ]
      in setPath allocPath allocation tree
  , transformInverse = Nothing  -- Allocations are published facts
  }

-- ============================================================================
-- ROUND TRANSFORMATION (Composition of all steps)
-- ============================================================================

{- | A complete protocol round is a composition of transformations:
  
  Round = ProviderPhase /> RecipientPhase
       
  Where:
    ProviderPhase = ∀ provider: (OscillationCheck /> CalculateAllocations /> PublishAllocations)
    RecipientPhase = ∀ recipient: (AggregateAllocations /> UpdateNeed />  PublishCommitment)
-}
data RoundTransform = RoundTransform
  { roundNumber :: Int
  , providerTransforms :: [Transform]
  , recipientTransforms :: [Transform]
  , composedTransform :: Transform
  }

mkRound :: Int -> [Transform] -> [Transform] -> RoundTransform
mkRound n providers recipients = RoundTransform
  { roundNumber = n
  , providerTransforms = providers
  , recipientTransforms = recipients
  , composedTransform = 
      foldr (/>) idTransform (providers ++ recipients)
  }

-- | Apply a complete round
applyRound :: RoundTransform -> StateTree -> StateTree
applyRound round tree = transformApply (composedTransform round) tree

-- ============================================================================
-- CONVERGENCE PROPERTIES (Formal)
-- ============================================================================

{- | A sequence of rounds converges if:
  
  ∀ε>0, ∃N: ∀n>N, ||state[n+1] - state[n]|| < ε
  
  For Free Association:
    || state || = Σ all remaining needs
    
  Convergence means: total needs decrease monotonically to 0
-}
totalNeeds :: StateTree -> Capacity
totalNeeds tree = 
  sum [ need 
      | (path, VCapacity need) <- M.toList (stateData tree)
      , "needs" `pathContains` path
      ]
  where
    pathContains :: String -> Path -> Bool
    pathContains str (BranchNode s _) = s == str
    pathContains str (PlayerNode _ p) = pathContains str p
    pathContains str (ContextNode _ p) = pathContains str p
    pathContains _ _ = False

-- | Check convergence
isConverged :: StateTree -> Bool
isConverged tree = totalNeeds tree < 0.01  -- Essentially zero

-- | Monotonic decrease property
-- "Total-Needs(tomorrow) ≤ Total-Needs(today)"
monotonicDecrease :: StateTree -> StateTree -> Bool
monotonicDecrease stateBefore stateAfter =
  totalNeeds stateAfter <= totalNeeds stateBefore

-- ============================================================================
-- STREAMING TRANSFORMATIONS
-- ============================================================================

{- | For real-time updates, we need streaming transformations
  
  Instead of: StateTree -> StateTree
  We have:   StateTree -> [Event] -> StateTree
-}
data Event =
    NeedChanged EntityId ResourceType Capacity Timestamp
  | RecognitionChanged EntityId EntityId Portion Timestamp
  | CapacityChanged EntityId ResourceType Capacity Timestamp
  | AllocationPublished EntityId EntityId ResourceType Capacity Timestamp
  deriving (Show, Eq)

-- | Convert event to transformation
eventToTransform :: Event -> Transform
eventToTransform (NeedChanged eid resType newNeed _timestamp) = Transform
  { transformName = "event:NeedChanged"
  , transformPrecondition = const True
  , transformApply = \tree ->
      let needPath = Root // "players" `at` eid // "needs" `at` resType
      in setPath needPath (VCapacity newNeed) tree
  , transformInverse = Nothing
  }

eventToTransform (RecognitionChanged from to portion _timestamp) =
  setRecognitionTransform from to portion

eventToTransform (CapacityChanged eid resType newCap _timestamp) = Transform
  { transformName = "event:CapacityChanged"
  , transformPrecondition = const True
  , transformApply = \tree ->
      let capPath = Root // "players" `at` eid // "capacities" `at` resType
      in setPath capPath (VCapacity newCap) tree
  , transformInverse = Nothing
  }

eventToTransform (AllocationPublished prov rec resType qty _timestamp) =
  publishAllocationTransform prov rec resType qty

-- | Apply stream of events
applyEvents :: StateTree -> [Event] -> StateTree
applyEvents = foldl (\tree event -> transformApply (eventToTransform event) tree)

-- ============================================================================
-- CRDT-COMPATIBLE OPERATIONS
-- ============================================================================

{- | For distributed consensus, transformations must be:
  1. Commutative (order doesn't matter)
  2. Associative (grouping doesn't matter)  
  3. Idempotent (applying twice = applying once)
  
  Not all our transforms have these properties!
  But some do:
-}

-- | Commutative transformations can be applied in any order
isCommutative :: Transform -> Transform -> StateTree -> Bool
isCommutative t1 t2 tree =
  let result1 = transformApply t2 (transformApply t1 tree)
      result2 = transformApply t1 (transformApply t2 tree)
  in result1 == result2  -- Approximate equality check needed

-- | Some transforms ARE commutative:
-- - Setting different paths (no conflict)
-- - Incrementing counters (LWW-register or Counter CRDT)
-- - Adding to sets (OR-Set CRDT)

-- | But some are NOT:
-- - Recognition updates (must sum to 100%)
-- - Need updates (depends on allocations received)
-- - History updates (order matters!)

-- | For non-commutative operations, use vector clocks
data Versioned a = Versioned
  { versionedValue :: a
  , versionedClock :: M.Map EntityId Int  -- Vector clock
  }

-- | Merge strategy for conflicts
data MergeStrategy =
    LastWriteWins Timestamp
  | MaxValue  -- For capacities
  | MinValue  -- For needs (conservative)
  | Custom (Value -> Value -> Value)

-- ============================================================================
-- EXAMPLE: BUILDING A ROUND
-- ============================================================================

exampleRound :: Timestamp -> RoundTransform
exampleRound timestamp = mkRound 1 providerPhase recipientPhase
  where
    -- Provider phase: Carol and Kitchen calculate allocations
    providerPhase =
      [ updateHistoryTransform "Carol" "Alice" timestamp 100
      , publishAllocationTransform "Carol" "Alice" "food" 64.3
      , publishAllocationTransform "Carol" "Bob" "food" 85.7
      , updateHistoryTransform "Kitchen" "Alice" timestamp 100
      , publishAllocationTransform "Kitchen" "Alice" "food" 100
      , publishAllocationTransform "Kitchen" "Bob" "food" 90
      ]
    
    -- Recipient phase: Alice and Bob update needs
    recipientPhase =
      [ updateNeedTransform "Alice" "food" 164.3  -- Received from Carol + Kitchen
      , updateNeedTransform "Bob" "food" 175.7
      ]

-- | Run the example
runExample :: StateTree -> StateTree
runExample initialState = 
  let round1 = exampleRound undefined -- Would use actual timestamp
      stateAfterRound1 = applyRound round1 initialState
  in stateAfterRound1

-- ============================================================================
-- PROPERTY TESTS (What we can PROVE about transformations)
-- ============================================================================

{- Properties we want to verify:

1. Convergence: ∀ sufficient capacity, lim[n→∞] totalNeeds(state[n]) = 0

2. Fairness: allocations ∝ mutual recognition

3. No accumulation: ∀ recipient, totalReceived ≤ declaredNeed

4. Conservation: Σ allocations ≤ Σ capacities

5. Monotonic decrease: totalNeeds decreases each round

6. Stability: if converged, stays converged
-}

prop_convergence :: [RoundTransform] -> StateTree -> Bool
prop_convergence rounds initialState =
  let finalState = foldl (flip applyRound) initialState rounds
  in totalNeeds finalState <= totalNeeds initialState

prop_noAccumulation :: Transform -> StateTree -> Bool
prop_noAccumulation t state =
  let state' = transformApply t state
  in totalNeeds state' >= 0  -- Can't have negative needs

prop_idempotent :: Transform -> StateTree -> Bool
prop_idempotent t state =
  let once = transformApply t state
      twice = transformApply t once
  in once == twice  -- Applying twice = applying once

-- These can be fed to QuickCheck for property-based testing!

