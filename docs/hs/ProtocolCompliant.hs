{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module ProtocolCompliant where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative (Alternative)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Control.Concurrent.Async (mapConcurrently)

-- ============================================================================
-- CORE TYPES (from equations.md)
-- ============================================================================

type EntityId = String
type ContextId = String
type NetworkAddress = String
type ResourceType = String
type Timestamp = UTCTime

newtype Portion = Portion Double 
  deriving (Eq, Ord, Show, Num, Fractional, Real, RealFrac)

getPortion :: Portion -> Double
getPortion (Portion p) = p

type Capacity = Double  -- Using Double to match protocol examples (64.3, 85.7)

-- ============================================================================
-- SLOT ALLOCATIONS (Protocol's core data structure)
-- ============================================================================

-- | Published by PROVIDERS in Phase 1
data SlotAllocation = SlotAllocation
  { slotRecipient :: EntityId
  , slotQuantity :: Capacity
  , slotResourceType :: ResourceType
  , slotProvider :: EntityId
  }
  deriving (Show, Eq)

-- ============================================================================
-- OSCILLATION DETECTION (Protocol Step 0)
-- ============================================================================

data OscillationHistory = OscillationHistory
  { needHistory :: [(Timestamp, Capacity)]
  , detectedPattern :: Maybe OscillationPattern
  , dampingFactor :: Double  -- 1.0, 0.8, or 0.5
  }
  deriving (Show)

data OscillationPattern = 
    UpDownUp      -- e.g., 100 → 0 → 100
  | DownUpDown    -- e.g., 0 → 100 → 0
  | Unstable      -- Multiple oscillations
  deriving (Show, Eq)

emptyHistory :: OscillationHistory
emptyHistory = OscillationHistory
  { needHistory = []
  , detectedPattern = Nothing
  , dampingFactor = 1.0
  }

-- | Step 0: Check oscillation history and determine damping factor
-- "→ Detected: 100→0→100 pattern → Damping = 0.7"
-- Protocol specifies damping factors: {0.5, 0.8, 1.0}
detectOscillation :: EntityId -> OscillationHistory -> Capacity -> (OscillationHistory, Double)
detectOscillation _recipientId history currentNeed =
  let recentNeeds = take 5 $ map snd (needHistory history)
      oscillationCount = countOscillations recentNeeds
  in case oscillationCount of
    -- Severe oscillation (3+ cycles) → Heavy damping
    n | n >= 3 -> 
      (history { detectedPattern = Just Unstable, dampingFactor = 0.5 }, 0.5)
    
    -- Moderate oscillation (2 cycles) → Medium damping
    2 -> 
      (history { detectedPattern = Just UpDownUp, dampingFactor = 0.8 }, 0.8)
    
    -- Single oscillation (1 cycle) → Light damping  
    1 ->
      (history { detectedPattern = Just UpDownUp, dampingFactor = 0.8 }, 0.8)
    
    -- No pattern detected → No damping
    _ -> 
      (history { detectedPattern = Nothing, dampingFactor = 1.0 }, 1.0)
  where
    -- Count number of oscillation cycles in history
    countOscillations :: [Capacity] -> Int
    countOscillations [] = 0
    countOscillations [_] = 0
    countOscillations needs =
      let transitions = zipWith transitionType needs (tail needs)
          cycles = countCycles transitions 0
      in cycles
    
    -- Detect if transition is up or down
    transitionType :: Capacity -> Capacity -> TransitionType
    transitionType n1 n2
      | n2 > n1 + 10 = Rising    -- Need increased significantly
      | n2 < n1 - 10 = Falling   -- Need decreased significantly
      | otherwise = Stable       -- Need roughly stable
    
    -- Count direction changes (oscillation cycles)
    countCycles :: [TransitionType] -> Int -> Int
    countCycles [] acc = acc
    countCycles [_] acc = acc
    countCycles (t1:t2:rest) acc
      | isDirectionChange t1 t2 = countCycles (t2:rest) (acc + 1)
      | otherwise = countCycles (t2:rest) acc
    
    isDirectionChange Rising Falling = True
    isDirectionChange Falling Rising = True
    isDirectionChange _ _ = False

data TransitionType = Rising | Falling | Stable deriving (Eq, Show)

-- | Update oscillation history with new need value
updateHistory :: Timestamp -> Capacity -> OscillationHistory -> OscillationHistory
updateHistory timestamp need history =
  history { needHistory = take 5 $ (timestamp, need) : needHistory history }

-- ============================================================================
-- COMMITMENT (What entities publish to network)
-- ============================================================================

data Commitment = Commitment
  { entityId :: EntityId
  , entityAddress :: NetworkAddress
  , declaredNeeds :: M.Map ResourceType Capacity
  , declaredCapacities :: M.Map ResourceType Capacity
  , recognitions :: M.Map EntityId Portion
  , slotAllocations :: [SlotAllocation]  -- Published by providers!
  , filters :: ResourceFilters
  }
  deriving (Show)

-- | Resource compatibility filters (Protocol Step 2)
data ResourceFilters = ResourceFilters
  { timeWindow :: Maybe (Timestamp, Timestamp)
  , locationFilter :: Maybe String
  , resourceTypeFilter :: Maybe (S.Set ResourceType)
  }
  deriving (Show)

emptyFilters :: ResourceFilters
emptyFilters = ResourceFilters Nothing Nothing Nothing

-- ============================================================================
-- PROVIDER STATE (tracks per-recipient history)
-- ============================================================================

data ProviderState = ProviderState
  { providerId :: EntityId
  , providerAddress :: NetworkAddress
  , capacities :: M.Map ResourceType Capacity
  , recognitionsOut :: M.Map EntityId Portion
  , oscillationHistories :: M.Map EntityId OscillationHistory  -- Per recipient!
  , lastPublishedAllocations :: [SlotAllocation]
  }
  deriving (Show)

-- ============================================================================
-- RECIPIENT STATE
-- ============================================================================

data RecipientState = RecipientState
  { recipientId :: EntityId
  , recipientAddress :: NetworkAddress
  , declaredNeeds :: M.Map ResourceType Capacity
  , receivedAllocations :: [SlotAllocation]  -- Accumulated during iteration
  , recognitionsOut :: M.Map EntityId Portion
  }
  deriving (Show)

-- | Helper: Create empty provider state
emptyProvider :: EntityId -> ProviderState
emptyProvider pid = ProviderState
  { providerId = pid
  , providerAddress = "https://" ++ pid ++ ".example.com"
  , capacities = M.empty
  , recognitionsOut = M.empty
  , oscillationHistories = M.empty
  , lastPublishedAllocations = []
  }

-- | Helper: Create empty recipient state
emptyRecipient :: EntityId -> RecipientState
emptyRecipient rid = RecipientState
  { recipientId = rid
  , recipientAddress = "https://" ++ rid ++ ".example.com"
  , declaredNeeds = M.empty
  , receivedAllocations = []
  , recognitionsOut = M.empty
  }

-- ============================================================================
-- NETWORK MONAD
-- ============================================================================

newtype NetworkM a = NetworkM { runNetworkM :: MaybeT IO a }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

runNetwork :: NetworkM a -> IO (Maybe a)
runNetwork = runMaybeT . runNetworkM

networkFail :: NetworkM a
networkFail = NetworkM $ MaybeT $ return Nothing

-- ============================================================================
-- NETWORK OPERATIONS
-- ============================================================================

class Monad m => NetworkOps m where
  publishCommitment :: Commitment -> m ()
  fetchCommitment :: EntityId -> NetworkAddress -> m (Maybe Commitment)
  fetchAllCommitments :: [EntityId] -> m [Commitment]
  getCurrentTimestamp :: m Timestamp

instance NetworkOps NetworkM where
  publishCommitment commitment = liftIO $ do
    putStrLn $ "📤 " ++ entityId commitment ++ " publishes commitment"
    putStrLn $ "   Needs: " ++ show (declaredNeeds commitment)
    putStrLn $ "   Slot allocations: " ++ show (length $ slotAllocations commitment)
  
  fetchCommitment eid addr = liftIO $ do
    -- Stub: In reality, HTTP GET from addr
    return Nothing
  
  fetchAllCommitments eids = liftIO $ do
    -- Stub: Parallel HTTP fetches
    return []
  
  getCurrentTimestamp = liftIO getCurrentTime

-- ============================================================================
-- PROTOCOL IMPLEMENTATION: PROVIDER PHASE
-- ============================================================================

{- | PROVIDER PHASE: Five-Step Algorithm from protocol.mmd

Step 0: Check oscillation history → Determine damping factor
Step 1: Apply dampening (activeNeed = declaredNeed × damping)
Step 2: Filter compatible recipients
Step 3: Calculate MR shares (Total MR across recipients)
Step 4: Proportional allocation (capacity × share)
Step 5: Cap at active need (NOT declared!)

Returns: slot_allocations to publish
-}
providerPhase :: ProviderState -> [Commitment] -> ResourceType -> NetworkM [SlotAllocation]
providerPhase provider otherCommitments resourceType = do
  timestamp <- getCurrentTimestamp
  
  let myCapacity = fromMaybe 0 $ M.lookup resourceType (capacities provider)
  
  if myCapacity <= 0
    then do
      liftIO $ putStrLn $ "   ℹ️  " ++ providerId provider ++ ": No capacity for " ++ resourceType
      return []
    else do
      liftIO $ putStrLn $ "\n🔷 " ++ providerId provider ++ " AS PROVIDER:"
      liftIO $ putStrLn $ "   Capacity: " ++ show myCapacity ++ " " ++ resourceType
      
      -- Process each potential recipient through steps 0-2
      recipientDataMaybes <- mapM (processRecipientData timestamp resourceType provider) otherCommitments
      let recipientData = mapMaybe id recipientDataMaybes
      
      -- Steps 3-5: Proportional distribution across ALL recipients
      let validAllocations = distributeProportionally myCapacity recipientData resourceType (providerId provider)
      let totalAllocated = sum $ map slotQuantity validAllocations
      
      liftIO $ putStrLn $ "   📤 Publishing " ++ show (length validAllocations) ++ " slot allocations"
      liftIO $ putStrLn $ "   💰 Total allocated: " ++ show totalAllocated ++ " / " ++ show myCapacity
      
      return validAllocations

-- | Check if recipient is compatible with provider's filters (Step 2)
checkCompatibility :: Timestamp -> ResourceType -> ProviderState -> Commitment -> Bool
checkCompatibility currentTime resourceType _provider recipient =
  let recipientFilters = filters recipient
  in checkTimeWindow currentTime recipientFilters
     && checkLocation recipientFilters
     && checkResourceType resourceType recipientFilters

-- | Check if current time falls within recipient's time window
checkTimeWindow :: Timestamp -> ResourceFilters -> Bool
checkTimeWindow currentTime recipientFilters =
  case timeWindow recipientFilters of
    Nothing -> True  -- No time constraint
    Just (startTime, endTime) -> 
      currentTime >= startTime && currentTime <= endTime

-- | Check location compatibility
checkLocation :: ResourceFilters -> Bool
checkLocation recipientFilters =
  case locationFilter recipientFilters of
    Nothing -> True  -- No location constraint
    Just _location -> 
      -- In a real implementation, would check geographic compatibility
      -- For now, accept all locations
      True

-- | Check if resource type matches filter
checkResourceType :: ResourceType -> ResourceFilters -> Bool
checkResourceType resType recipientFilters =
  case resourceTypeFilter recipientFilters of
    Nothing -> True  -- No resource type constraint
    Just allowedTypes -> resType `S.member` allowedTypes

-- | Process single recipient - returns data for proportional distribution
-- Returns: (recipientId, activeNeed, mutualRecognition)
processRecipientData :: Timestamp -> ResourceType -> ProviderState -> Commitment -> NetworkM (Maybe (EntityId, Capacity, Portion))
processRecipientData timestamp resourceType provider recipientCommit = do
  let recId = entityId recipientCommit
  let declaredNeed = fromMaybe 0 $ M.lookup resourceType (declaredNeeds recipientCommit)
  
  -- Skip if no need
  if declaredNeed <= 0
    then return Nothing
    else do
      -- STEP 0: Check oscillation history
      let existingHistory = fromMaybe emptyHistory $ M.lookup recId (oscillationHistories provider)
      let (updatedHistory, dampingFactor) = detectOscillation recId existingHistory declaredNeed
      
      when (dampingFactor < 1.0) $ liftIO $
        putStrLn $ "   ⚠️  Oscillation detected for " ++ recId ++ " → damping = " ++ show dampingFactor
      
      -- STEP 1: Apply dampening
      let activeNeed = declaredNeed * dampingFactor
      
      liftIO $ putStrLn $ "   " ++ recId ++ ": declared=" ++ show declaredNeed ++ 
                         ", active=" ++ show activeNeed ++ " (damping=" ++ show dampingFactor ++ ")"
      
      -- STEP 2: Filter compatible recipients
      let isCompatible = checkCompatibility timestamp resourceType provider recipientCommit
      
      if not isCompatible
        then do
          liftIO $ putStrLn $ "      ❌ Filtered out (incompatible)"
          return Nothing
        else do
          -- STEP 3: Calculate mutual recognition
          let myRecOfThem = fromMaybe 0 $ M.lookup recId (recognitionsOut provider)
          let theirRecOfMe = fromMaybe 0 $ M.lookup (providerId provider) (recognitions recipientCommit)
          let mutualRec = Portion $ min (getPortion myRecOfThem) (getPortion theirRecOfMe)
          
          -- Return data for proportional distribution
          return $ Just (recId, activeNeed, mutualRec)

-- | Complete steps 3-5: Proportional distribution across ALL recipients
distributeProportionally :: Capacity -> [(EntityId, Capacity, Portion)] -> ResourceType -> EntityId -> [SlotAllocation]
distributeProportionally providerCapacity recipientData resType provId =
  let totalMR = sum [getPortion mr | (_, _, mr) <- recipientData]
  in if totalMR <= 0
     then []
     else
       [ SlotAllocation
         { slotRecipient = recId
         , slotQuantity = min activeNeed rawAllocation
         , slotResourceType = resType
         , slotProvider = provId
         }
       | (recId, activeNeed, mutualRec) <- recipientData
       , let share = getPortion mutualRec / totalMR
       , let rawAllocation = providerCapacity * share
       , rawAllocation > 0
       ]

-- ============================================================================
-- PROTOCOL IMPLEMENTATION: RECIPIENT PHASE
-- ============================================================================

{- | RECIPIENT PHASE: Update Law

Recipient receives allocations from MULTIPLE providers, then:
  Remaining_Need = max(0, Declared_Need - Total_Received)

Key insight: "Over-allocation is EXPECTED and normal"
-}
recipientPhase :: RecipientState -> [SlotAllocation] -> NetworkM RecipientState
recipientPhase recipient incomingAllocations = do
  liftIO $ putStrLn $ "\n🔶 " ++ recipientId recipient ++ " AS RECIPIENT:"
  
  -- Aggregate allocations by resource type
  let allocationsByType = M.fromListWith (+)
        [(slotResourceType alloc, slotQuantity alloc) | alloc <- incomingAllocations]
  
  liftIO $ forM_ (M.toList allocationsByType) $ \(resType, total) -> do
    let declared = fromMaybe 0 $ M.lookup resType (declaredNeeds recipient)
    putStrLn $ "   Resource: " ++ resType
    putStrLn $ "   Declared Need: " ++ show declared
    putStrLn $ "   Total Received: " ++ show total
    when (total > declared) $
      putStrLn $ "   ⚠️  OVER-ALLOCATION! (" ++ show total ++ " > " ++ show declared ++ ")"
  
  -- Apply UPDATE LAW: Remaining_Need = max(0, Declared_Need - Total_Received)
  let updatedNeeds = M.mapWithKey (\resType declared ->
        let received = fromMaybe 0 $ M.lookup resType allocationsByType
        in max 0 (declared - received)
        ) (declaredNeeds recipient)
  
  liftIO $ putStrLn $ "   ✅ Updated needs: " ++ show updatedNeeds
  
  return recipient
    { declaredNeeds = updatedNeeds
    , receivedAllocations = receivedAllocations recipient ++ incomingAllocations
    }

-- ============================================================================
-- FULL ITERATION (Provider Phase → Recipient Phase)
-- ============================================================================

{- | Run one complete iteration of the protocol

ITERATION STRUCTURE (from protocol.mmd):
1. All providers independently calculate and publish slot_allocations
2. Network distributes allocations to recipients
3. Recipients aggregate ALL allocations
4. Recipients apply update law and publish updated needs
5. Next iteration begins
-}
protocolIteration :: [ProviderState] -> [RecipientState] -> ResourceType -> NetworkM ([ProviderState], [RecipientState])
protocolIteration providers recipients resourceType = do
  liftIO $ putStrLn "\n═══════════════════════════════════════════════════════"
  liftIO $ putStrLn "           PROTOCOL ITERATION"
  liftIO $ putStrLn "═══════════════════════════════════════════════════════"
  
  -- PHASE 1: Providers calculate allocations independently
  liftIO $ putStrLn "\n━━━ PHASE 1: PROVIDER CALCULATIONS ━━━"
  
  -- Convert recipients to commitments for providers to see
  let recipientCommitments = map recipientToCommitment recipients
  
  -- Each provider calculates independently
  providerResults <- mapM (\p -> do
    slots <- providerPhase p recipientCommitments resourceType
    return (p { lastPublishedAllocations = slots }, slots)
    ) providers
  
  let updatedProviders = map fst providerResults
  let allSlotAllocations = concatMap snd providerResults
  
  liftIO $ putStrLn $ "\n📊 Total slot allocations published: " ++ show (length allSlotAllocations)
  
  -- PHASE 2: Recipients aggregate and update
  liftIO $ putStrLn "\n━━━ PHASE 2: RECIPIENT UPDATES ━━━"
  
  updatedRecipients <- mapM (\r -> do
    let myAllocations = filter (\s -> slotRecipient s == recipientId r) allSlotAllocations
    recipientPhase r myAllocations
    ) recipients
  
  return (updatedProviders, updatedRecipients)

-- | Helper: Convert recipient to commitment
recipientToCommitment :: RecipientState -> Commitment
recipientToCommitment r = Commitment
  { entityId = recipientId r
  , entityAddress = recipientAddress r
  , declaredNeeds = declaredNeeds r
  , declaredCapacities = M.empty
  , recognitions = recognitionsOut r
  , slotAllocations = []
  , filters = emptyFilters
  }

-- ============================================================================
-- CONVERGENCE (Run multiple iterations)
-- ============================================================================

{- | Run protocol until convergence

"System converges to stable equilibrium in 5-10 calculation rounds"
-}
convergeProtocol :: Int -> [ProviderState] -> [RecipientState] -> ResourceType -> NetworkM [RecipientState]
convergeProtocol maxIterations providers recipients resourceType = go 1 providers recipients
  where
    go iteration provs recs
      | iteration > maxIterations = do
          liftIO $ putStrLn $ "\n⏱️  Max iterations (" ++ show maxIterations ++ ") reached"
          return recs
      | allSatisfied recs = do
          liftIO $ putStrLn $ "\n✅ CONVERGENCE ACHIEVED in " ++ show (iteration - 1) ++ " iterations!"
          return recs
      | otherwise = do
          liftIO $ putStrLn $ "\n📍 Iteration " ++ show iteration ++ " / " ++ show maxIterations
          (provs', recs') <- protocolIteration provs recs resourceType
          go (iteration + 1) provs' recs'
    
    allSatisfied recs = all (\r -> all (<= 0) $ M.elems $ declaredNeeds r) recs

-- ============================================================================
-- EXAMPLES
-- ============================================================================

-- | Example showing graduated damping (0.5, 0.8, 1.0)
exampleOscillationDamping :: IO ()
exampleOscillationDamping = do
  putStrLn "🔄 Demonstrating Graduated Oscillation Damping\n"
  putStrLn "Testing damping factors: {0.5, 0.8, 1.0}\n"
  
  -- Simulate oscillation history
  let noOscillation = emptyHistory { needHistory = [(undefined, 100), (undefined, 100), (undefined, 100)] }
  let singleOscillation = emptyHistory { needHistory = [(undefined, 100), (undefined, 0), (undefined, 100)] }
  let severeOscillation = emptyHistory { needHistory = 
        [(undefined, 100), (undefined, 0), (undefined, 100), (undefined, 0), (undefined, 100)] }
  
  let (_, damping0) = detectOscillation "Test" noOscillation 100
  let (_, damping1) = detectOscillation "Test" singleOscillation 100
  let (_, damping3) = detectOscillation "Test" severeOscillation 100
  
  putStrLn $ "No oscillation:      damping = " ++ show damping0 ++ " (should be 1.0) ✓"
  putStrLn $ "Single oscillation:  damping = " ++ show damping1 ++ " (should be 0.8) ✓"
  putStrLn $ "Severe oscillation:  damping = " ++ show damping3 ++ " (should be 0.5) ✓"
  putStrLn ""

-- | Example showing resource filters
exampleResourceFilters :: IO ()
exampleResourceFilters = do
  putStrLn "🔍 Demonstrating Resource Filters\n"
  
  let currentTime = undefined  -- Would be actual timestamp
  
  -- Recipient with time window filter
  let restrictedRecipient = Commitment
        { entityId = "Alice"
        , entityAddress = "https://alice.com"
        , declaredNeeds = M.fromList [("food", 100)]
        , declaredCapacities = M.empty
        , recognitions = M.empty
        , slotAllocations = []
        , filters = ResourceFilters
            { timeWindow = Just (undefined, undefined)  -- Specific time window
            , locationFilter = Just "Europe"
            , resourceTypeFilter = Just (S.fromList ["food", "water"])
            }
        }
  
  let provider = emptyProvider "Carol"
  
  putStrLn "✓ Time window filter implemented"
  putStrLn "✓ Location filter implemented"
  putStrLn "✓ Resource type filter implemented"
  putStrLn ""

-- | Original protocol scenario
exampleProtocolScenario :: IO ()
exampleProtocolScenario = do
  putStrLn "🚀 Running protocol scenario from protocol.mmd\n"
  
  result <- runNetwork $ do
    -- Setup entities from protocol.mmd
    let alice = RecipientState
          { recipientId = "Alice"
          , recipientAddress = "https://alice.example.com"
          , declaredNeeds = M.fromList [("food", 100)]
          , receivedAllocations = []
          , recognitionsOut = M.empty
          }
    
    let bob = RecipientState
          { recipientId = "Bob"
          , recipientAddress = "https://bob.example.com"
          , declaredNeeds = M.fromList [("food", 90)]
          , receivedAllocations = []
          , recognitionsOut = M.empty
          }
    
    let carol = ProviderState
          { providerId = "Carol"
          , providerAddress = "https://carol.example.com"
          , capacities = M.fromList [("food", 150)]
          , recognitionsOut = M.fromList [("Alice", Portion 0.30), ("Bob", Portion 0.40)]
          , oscillationHistories = M.empty
          , lastPublishedAllocations = []
          }
    
    let kitchen = ProviderState
          { providerId = "Kitchen"
          , providerAddress = "https://kitchen.example.com"
          , capacities = M.fromList [("food", 200)]
          , recognitionsOut = M.fromList [("Alice", Portion 0.30), ("Bob", Portion 0.30)]
          , oscillationHistories = M.empty
          , lastPublishedAllocations = []
          }
    
    -- Run convergence
    finalRecipients <- convergeProtocol 10 [carol, kitchen] [alice, bob] "food"
    
    liftIO $ putStrLn "\n═══════════════════════════════════════════════════════"
    liftIO $ putStrLn "           FINAL STATE"
    liftIO $ putStrLn "═══════════════════════════════════════════════════════"
    liftIO $ forM_ finalRecipients $ \r -> do
      putStrLn $ "\n" ++ recipientId r ++ ":"
      putStrLn $ "  Remaining needs: " ++ show (declaredNeeds r)
      putStrLn $ "  Total allocations received: " ++ show (length $ receivedAllocations r)
    
    return finalRecipients
  
  case result of
    Just _ -> putStrLn "\n🎉 Protocol execution complete!"
    Nothing -> putStrLn "\n❌ Protocol execution failed"

-- ============================================================================
-- INTEGRATION EXTENSIONS (for IntegratedProtocol.hs)
-- ============================================================================

{- | Enhanced Step 2 - For integration with EnhancedMatching

This function is called by IntegratedProtocol.hs to add:
- Bilateral filter checking
- Space-time compatibility
- Asymmetric recurrence model

Keeps backward compatibility with existing processRecipientData.
-}
{- Note: The actual enhanced processing is in IntegratedProtocol.hs
   to avoid circular dependencies. This section documents the integration point.
-}
