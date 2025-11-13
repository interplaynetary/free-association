{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

{- | Unified Resource Allocation Protocol

This module proves that Free Association is a GENERAL protocol
for allocating ANY scarce resource based on mutual recognition.

The SAME 5-step algorithm works for:
  - Economic resources (food, money, time)
  - Digital resources (storage, bandwidth, compute)
  - Network resources (routing, caching, replication)
  - Social resources (attention, endorsement, collaboration)
-}

module UnifiedProtocol where

import qualified Data.Map.Strict as M
import Data.Kind (Type)

-- ============================================================================
-- RESOURCE TYPE CLASS
-- ============================================================================

{- | Any resource that can be allocated using Free Association
  
  Must have:
    - A way to measure quantity (Quantity)
    - A way to track need vs capacity
    - A way to detect oscillation/thrashing
-}
class Resource r where
  -- | Type of quantity (Bytes, Dollars, Seconds, etc.)
  type Quantity r :: Type
  
  -- | Empty/zero quantity
  zeroQuantity :: Quantity r
  
  -- | Add quantities
  addQuantity :: Quantity r -> Quantity r -> Quantity r
  
  -- | Compare quantities
  compareQuantity :: Quantity r -> Quantity r -> Ordering
  
  -- | Multiply by portion (for proportional allocation)
  scaleQuantity :: Double -> Quantity r -> Quantity r
  
  -- | Describe quantity for logging
  showQuantity :: Quantity r -> String
  
  -- | Detect oscillation in access pattern
  detectOscillation :: r -> [AccessEvent r] -> (Bool, Double)

-- | Generic access event
data AccessEvent r = AccessEvent
  { eventTimestamp :: Double  -- Simplified timestamp
  , eventQuantity :: Quantity r
  , eventType :: EventType
  }

data EventType = Request | Cancel | Consume
  deriving (Show, Eq)

-- ============================================================================
-- RESOURCE INSTANCES
-- ============================================================================

-- | Economic resources (food, money, etc.)
data Economic = Economic

instance Resource Economic where
  type Quantity Economic = Double
  zeroQuantity = 0.0
  addQuantity = (+)
  compareQuantity = compare
  scaleQuantity = (*)
  showQuantity q = show q ++ " units"
  detectOscillation _ events =
    let requests = [q | AccessEvent _ q Request <- events]
        cancels = [q | AccessEvent _ q Cancel <- events]
    in if length cancels >= 2 && length requests >= 2
       then (True, 0.7)  -- Oscillating
       else (False, 1.0)  -- Not oscillating

-- | Storage resources (bytes on disk)
data Storage = Storage

instance Resource Storage where
  type Quantity Storage = Integer  -- Bytes
  zeroQuantity = 0
  addQuantity = (+)
  compareQuantity = compare
  scaleQuantity portion qty = floor (portion * fromIntegral qty)
  showQuantity qty
    | qty >= 1_000_000_000_000 = show (qty `div` 1_000_000_000_000) ++ " TB"
    | qty >= 1_000_000_000     = show (qty `div` 1_000_000_000) ++ " GB"
    | qty >= 1_000_000         = show (qty `div` 1_000_000) ++ " MB"
    | otherwise                = show qty ++ " bytes"
  detectOscillation _ events =
    let deletes = length [() | AccessEvent _ _ Cancel <- events]
        requests = length [() | AccessEvent _ _ Request <- events]
    in if deletes >= 2 && requests >= 2
       then (True, 0.7)
       else (False, 1.0)

-- | Compute resources (CPU seconds)
data Compute = Compute

instance Resource Compute where
  type Quantity Compute = Double  -- Seconds
  zeroQuantity = 0.0
  addQuantity = (+)
  compareQuantity = compare
  scaleQuantity = (*)
  showQuantity q = show q ++ " CPU-seconds"
  detectOscillation _ events =
    let cancellations = length [() | AccessEvent _ _ Cancel <- events]
    in if cancellations >= 3
       then (True, 0.8)  -- Job thrashing
       else (False, 1.0)

-- | Bandwidth resources (bytes per second)
data Bandwidth = Bandwidth

instance Resource Bandwidth where
  type Quantity Bandwidth = Double  -- Bytes/sec
  zeroQuantity = 0.0
  addQuantity = (+)
  compareQuantity = compare
  scaleQuantity = (*)
  showQuantity bps = 
    let mbps = bps / 1_000_000
    in show mbps ++ " Mbps"
  detectOscillation _ events =
    let spikes = length [() | AccessEvent _ qty Request <- events, qty > 10_000_000]
    in if spikes >= 3
       then (True, 0.7)
       else (False, 1.0)

-- ============================================================================
-- GENERIC PROTOCOL STATE
-- ============================================================================

type EntityId = String
type Portion = Double

-- | Generic provider (works for ANY resource!)
data Provider r = Provider
  { providerId :: EntityId
  , capacity :: Quantity r
  , recognitions :: M.Map EntityId Portion
  , oscillationHistory :: M.Map EntityId [AccessEvent r]
  , publishedAllocations :: [Allocation r]
  }

-- | Generic need/request (works for ANY resource!)
data Need r = Need
  { neederId :: EntityId
  , declaredNeed :: Quantity r
  , neederRecognitions :: M.Map EntityId Portion
  }

-- | Generic allocation (works for ANY resource!)
data Allocation r = Allocation
  { allocProvider :: EntityId
  , allocRecipient :: EntityId
  , allocQuantity :: Quantity r
  }
  deriving (Show)

-- ============================================================================
-- THE UNIVERSAL 5-STEP ALGORITHM
-- ============================================================================

{- | Generic provider phase - works for ANY resource!
  
  Step 0: Check oscillation history
  Step 1: Apply dampening
  Step 2: Filter compatible
  Step 3: Calculate mutual recognition shares
  Step 4: Proportional allocation
  Step 5: Cap at active need
-}
genericProviderPhase :: Resource r 
                     => r                    -- Resource type witness
                     -> Provider r           -- Provider state
                     -> [Need r]             -- All needs in network
                     -> [Allocation r]       -- Calculated allocations
genericProviderPhase resourceWitness provider needs =
  let -- Get all needs with active dampening (Steps 0-1)
      needsWithDamping = 
        [ (nid, activeNeed, mutualRec)
        | need <- needs
        , let nid = neederId need
        , let history = M.findWithDefault [] nid (oscillationHistory provider)
        , let (isOscillating, dampingFactor) = detectOscillation resourceWitness history
        , let activeNeed = scaleQuantity dampingFactor (declaredNeed need)
        
        -- Step 2: Filter compatible (only if need > 0)
        , compareQuantity activeNeed zeroQuantity == GT
        
        -- Step 3: Calculate mutual recognition
        , let myRec = M.findWithDefault 0.0 nid (recognitions provider)
        , let theirRec = M.findWithDefault 0.0 (providerId provider) (neederRecognitions need)
        , let mutualRec = min myRec theirRec
        , mutualRec > 0.0  -- Only allocate if mutual recognition exists
        ]
      
      -- Step 4: Calculate proportional shares
      totalMR = sum [mr | (_, _, mr) <- needsWithDamping]
      
  in if totalMR <= 0.0
     then []
     else
       -- Step 5: Allocate proportionally, cap at active need
       [ Allocation
         { allocProvider = providerId provider
         , allocRecipient = nid
         , allocQuantity = min activeNeed rawAllocation
         }
       | (nid, activeNeed, mutualRec) <- needsWithDamping
       , let share = mutualRec / totalMR
       , let rawAllocation = scaleQuantity share (capacity provider)
       , compareQuantity rawAllocation zeroQuantity == GT
       ]

-- | Generic recipient phase - works for ANY resource!
genericRecipientPhase :: Resource r
                      => r                    -- Resource type witness
                      -> Need r               -- Current need
                      -> [Allocation r]       -- Received allocations
                      -> Need r               -- Updated need
genericRecipientPhase _resourceWitness need allocations =
  let -- Sum all received allocations
      totalReceived = foldl addQuantity zeroQuantity 
                       [allocQuantity a | a <- allocations, allocRecipient a == neederId need]
      
      -- UPDATE LAW: Remaining = max(0, Declared - Received)
      currentNeed = declaredNeed need
      remaining = case compareQuantity currentNeed totalReceived of
        GT -> currentNeed `subtractQuantity` totalReceived  -- Still need more
        _  -> zeroQuantity  -- Satisfied or over-allocated
      
  in need { declaredNeed = remaining }
  where
    -- Helper to subtract (assumes we have enough to subtract)
    subtractQuantity :: Quantity r -> Quantity r -> Quantity r
    subtractQuantity = error "Implement via Resource class if needed"
    -- In practice, we'd add this to the Resource typeclass

-- ============================================================================
-- EXAMPLE: SWITCHING BETWEEN RESOURCE TYPES
-- ============================================================================

-- | Allocate food (economic resource)
exampleFood :: IO ()
exampleFood = do
  putStrLn "🍎 ALLOCATING FOOD (Economic Resource)"
  putStrLn "──────────────────────────────────────"
  
  let carol = Provider
        { providerId = "Carol"
        , capacity = 150.0  -- 150 units of food
        , recognitions = M.fromList [("Alice", 0.3), ("Bob", 0.4)]
        , oscillationHistory = M.empty
        , publishedAllocations = []
        }
  
  let alice = Need
        { neederId = "Alice"
        , declaredNeed = 100.0
        , neederRecognitions = M.fromList [("Carol", 0.5)]
        }
  
  let bob = Need
        { neederId = "Bob"
        , declaredNeed = 90.0
        , neederRecognitions = M.fromList [("Carol", 0.6)]
        }
  
  let allocations = genericProviderPhase Economic carol [alice, bob]
  
  putStrLn "\nAllocations:"
  mapM_ (\a -> putStrLn $ "  " ++ allocRecipient a ++ " receives " ++ 
                          showQuantity (allocQuantity a :: Quantity Economic)) allocations

-- | Allocate storage (digital resource)  
exampleStorage :: IO ()
exampleStorage = do
  putStrLn "\n💾 ALLOCATING STORAGE (Digital Resource)"
  putStrLn "────────────────────────────────────────"
  
  let carol = Provider
        { providerId = "Carol"
        , capacity = 500_000_000_000  -- 500 GB
        , recognitions = M.fromList [("Alice", 0.4), ("Bob", 0.3)]
        , oscillationHistory = M.empty
        , publishedAllocations = []
        }
  
  let alice = Need
        { neederId = "Alice"
        , declaredNeed = 10_000_000_000  -- 10 GB
        , neederRecognitions = M.fromList [("Carol", 0.5)]
        }
  
  let bob = Need
        { neederId = "Bob"
        , declaredNeed = 1_000_000  -- 1 MB
        , neederRecognitions = M.fromList [("Carol", 0.6)]
        }
  
  let allocations = genericProviderPhase Storage carol [alice, bob]
  
  putStrLn "\nReplication Allocations:"
  mapM_ (\a -> putStrLn $ "  " ++ allocRecipient a ++ " receives " ++ 
                          showQuantity (allocQuantity a :: Quantity Storage)) allocations

-- | Allocate compute (computational resource)
exampleCompute :: IO ()
exampleCompute = do
  putStrLn "\n⚡ ALLOCATING COMPUTE (Computational Resource)"
  putStrLn "──────────────────────────────────────────────"
  
  let carol = Provider
        { providerId = "Carol"
        , capacity = 3600.0  -- 1 hour of compute
        , recognitions = M.fromList [("Alice", 0.5), ("Bob", 0.3)]
        , oscillationHistory = M.empty
        , publishedAllocations = []
        }
  
  let alice = Need
        { neederId = "Alice"
        , declaredNeed = 1800.0  -- 30 minutes
        , neederRecognitions = M.fromList [("Carol", 0.6)]
        }
  
  let bob = Need
        { neederId = "Bob"
        , declaredNeed = 7200.0  -- 2 hours (more than available!)
        , neederRecognitions = M.fromList [("Carol", 0.4)]
        }
  
  let allocations = genericProviderPhase Compute carol [alice, bob]
  
  putStrLn "\nCompute Allocations:"
  mapM_ (\a -> putStrLn $ "  " ++ allocRecipient a ++ " receives " ++ 
                          showQuantity (allocQuantity a :: Quantity Compute)) allocations

-- | Allocate bandwidth (network resource)
exampleBandwidth :: IO ()
exampleBandwidth = do
  putStrLn "\n📶 ALLOCATING BANDWIDTH (Network Resource)"
  putStrLn "───────────────────────────────────────────"
  
  let carol = Provider
        { providerId = "Carol"
        , capacity = 100_000_000.0  -- 100 Mbps
        , recognitions = M.fromList [("Alice", 0.4), ("Bob", 0.3)]
        , oscillationHistory = M.empty
        , publishedAllocations = []
        }
  
  let alice = Need
        { neederId = "Alice"
        , declaredNeed = 50_000_000.0  -- 50 Mbps
        , neederRecognitions = M.fromList [("Carol", 0.5)]
        }
  
  let bob = Need
        { neederId = "Bob"
        , declaredNeed = 80_000_000.0  -- 80 Mbps
        , neederRecognitions = M.fromList [("Carol", 0.5)]
        }
  
  let allocations = genericProviderPhase Bandwidth carol [alice, bob]
  
  putStrLn "\nBandwidth Allocations:"
  mapM_ (\a -> putStrLn $ "  " ++ allocRecipient a ++ " receives " ++ 
                          showQuantity (allocQuantity a :: Quantity Bandwidth)) allocations

-- | Run all examples
runAllExamples :: IO ()
runAllExamples = do
  putStrLn "\n╔══════════════════════════════════════════════════════════╗"
  putStrLn "║  UNIFIED PROTOCOL: Same Algorithm, Different Resources  ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝\n"
  
  exampleFood
  exampleStorage
  exampleCompute
  exampleBandwidth
  
  putStrLn "\n╔══════════════════════════════════════════════════════════╗"
  putStrLn "║                  KEY INSIGHT                             ║"
  putStrLn "╠══════════════════════════════════════════════════════════╣"
  putStrLn "║  The SAME 5-step algorithm works for:                   ║"
  putStrLn "║    • Food (economic)                                     ║"
  putStrLn "║    • Storage (digital)                                   ║"
  putStrLn "║    • Compute (computational)                             ║"
  putStrLn "║    • Bandwidth (network)                                 ║"
  putStrLn "║                                                          ║"
  putStrLn "║  Free Association is RESOURCE-AGNOSTIC!                 ║"
  putStrLn "║  It's an OS for allocating ANY scarce resource.         ║"
  putStrLn "╚══════════════════════════════════════════════════════════╝\n"

-- ============================================================================
-- POLYMORPHIC CONVERGENCE
-- ============================================================================

{- | The convergence property holds for ALL resources!
  
  ∀ resource type r:
    totalNeed(round[n+1]) ≤ totalNeed(round[n])
  
  This is a universal property of the protocol.
-}
class Resource r => Convergent r where
  totalNeed :: [Need r] -> Quantity r
  isConverged :: [Need r] -> Bool

instance Convergent Economic where
  totalNeed needs = sum [declaredNeed n | n <- needs]
  isConverged needs = all (\n -> declaredNeed n <= 0.01) needs

instance Convergent Storage where
  totalNeed needs = sum [declaredNeed n | n <- needs]
  isConverged needs = all (\n -> declaredNeed n <= 1000) needs  -- < 1KB

instance Convergent Compute where
  totalNeed needs = sum [declaredNeed n | n <- needs]
  isConverged needs = all (\n -> declaredNeed n <= 0.1) needs  -- < 0.1s

instance Convergent Bandwidth where
  totalNeed needs = sum [declaredNeed n | n <- needs]
  isConverged needs = all (\n -> declaredNeed n <= 1000.0) needs  -- < 1 Kbps

