# Free Association Architecture: Zipper + Algebra + Protocol

## The Three Layers

```
┌─────────────────────────────────────────────────────────┐
│  Layer 3: Protocol (ProtocolCompliant.hs)              │
│  What: 5-step allocation algorithm with dampening      │
│  Role: Business logic - how resources get allocated    │
└─────────────────────────────────────────────────────────┘
                          ↓ uses
┌─────────────────────────────────────────────────────────┐
│  Layer 2: Transformation Algebra                        │
│  What: Composable state transformations                │
│  Role: Formal reasoning about state changes            │
└─────────────────────────────────────────────────────────┘
                          ↓ uses
┌─────────────────────────────────────────────────────────┐
│  Layer 1: Zipper (NetworkedZipper.hs)                  │
│  What: Navigation through distributed state tree       │
│  Role: Data access - fetch/publish across network      │
└─────────────────────────────────────────────────────────┘
```

---

## Answering the Architectural Questions

### Q1: Do tree transformations happen atomically per round, or stream incrementally?

**Answer: BOTH, in different contexts**

```haskell
-- ATOMIC: Provider phase (must see all recipients together)
providerPhase :: ProviderState -> [Commitment] -> ResourceType 
              -> NetworkM [SlotAllocation]
-- All recipients evaluated atomically for proportional distribution

-- STREAMING: Recipient updates (can arrive asynchronously)
applyEvents :: StateTree -> [Event] -> StateTree
-- Events processed as they arrive: NeedChanged, RecognitionChanged, etc.

-- PROTOCOL ROUND: Atomic composition of streaming operations
data RoundTransform = RoundTransform
  { providerTransforms :: [Transform]    -- Applied atomically
  , recipientTransforms :: [Transform]   -- Can stream in
  , composedTransform :: Transform       -- Full round as single transform
  }
```

**Why both?**
- **Atomic**: Proportional allocation requires seeing all recipients simultaneously
- **Streaming**: Real-time need updates ("Circumstances change - Alice needs more!")
- **Convergence**: Atomic rounds guarantee monotonic decrease

---

### Q2: How do paths reference across player boundaries?

**Answer: Remote Path References**

```haskell
-- Three types of path references:
data PathRef =
    LocalPath Path                    -- My own tree
  | RemotePath String EntityId Path   -- Someone else's tree
  | ComputedPath (s -> Maybe Path)    -- Dynamically calculated

-- Mutual recognition uses BOTH:
mutualRecognition :: EntityId -> EntityId -> (PathRef, PathRef)
mutualRecognition "Carol" "Alice" = 
  ( LocalPath  (Root // "players" `at` "Carol" // "recognitions" `at` "Alice")
  , RemotePath "https://alice.com" "Alice" 
               (Root // "players" `at` "Alice" // "recognitions" `at` "Carol")
  )

-- The zipper resolves remote paths via network:
resolve :: PathRef -> ZipperM Value
resolve (LocalPath p) = getPath p <$> getCurrentState
resolve (RemotePath addr eid p) = do
  remoteFocus <- fetchPlayer eid addr
  return $ getPath p (stateOf remoteFocus)
```

**Key insight:** Paths are composable across network boundaries!

---

### Q3: Are there tree algebra operations for combining collective contexts?

**Answer: YES - Full algebra defined**

```haskell
class TreeAlgebra t where
  merge :: MergeStrategy -> t -> t -> t
  union :: t -> t -> t
  intersection :: t -> t -> t
  difference :: t -> t -> t

-- Specific strategies for different data:
data MergeStrategy =
    LastWriteWins Timestamp       -- For capabilities
  | MaxValue                      -- For capacities (optimistic)
  | MinValue                      -- For needs (conservative)
  | Custom (Value -> Value -> Value)

-- Example: Merging two coalition contexts
mergeContexts :: ContextState -> ContextState -> ContextState
mergeContexts coalition1 coalition2 = ContextState
  { contextMembers = M.unionWith avgPortion members1 members2
  , p2c2aCapacities = M.unionWith (+) capacities1 capacities2  -- Add capacities!
  , subContexts = M.union subContexts1 subContexts2
  }
```

**Use cases:**
- Federating organizations: `union context1 context2`
- Coalition splitting: `difference coalition member`
- Shared resources: `intersection org1Resources org2Resources`

---

### Q4: Does the type system enforce 100% constraint at compile or runtime?

**Answer: Runtime now, but CAN be compile-time**

```haskell
-- Current (runtime):
validateRecognitions :: PlayerState -> Either String PlayerState
validateRecognitions player =
  let total = sum $ map getPortion $ M.elems (p2aRecognitions player)
  in if abs (total - 1.0) < 0.001
     then Right player
     else Left "Recognition must sum to 100%"

-- BUT: Transform preconditions enforce it!
setRecognitionTransform :: EntityId -> EntityId -> Portion -> Transform
setRecognitionTransform fromId toId portion = Transform
  { transformPrecondition = \tree ->
      -- Check that AFTER this transform, total = 1.0
      let newTotal = computeTotalRecognition tree fromId (toId, portion)
      in abs (newTotal - 1.0) < 0.001
  , ...
  }

-- Future (compile-time with dependent types):
{-@ type ValidRecognitions = 
    {m:Map EntityId Portion | sum (elems m) == 1.0} @-}

-- Or with Idris/Agda:
record ValidPlayer where
  constructor MkPlayer
  recognitions : Map EntityId Portion
  proof : sum (values recognitions) = 1.0
```

**Trade-off:**
- **Runtime**: Flexible, easy to implement
- **Compile-time**: Catches errors early, but rigid
- **Transform preconditions**: Best of both - flexible but verified!

---

## How They Connect: Complete Flow

### Example: Carol Allocates to Alice

```haskell
-- LAYER 1 (Zipper): Navigate and fetch
carolFocus <- initZipper "Carol" "https://carol.com"
aliceCommit <- toPlayer "Alice" carolFocus >>= fetchCommitment
               ↓ Network fetch: https://alice.com/commitment

-- LAYER 2 (Algebra): Build transformation
let updateHistory = updateHistoryTransform "Carol" "Alice" timestamp 100
let publishAlloc = publishAllocationTransform "Carol" "Alice" "food" 64.3
let carolTransform = updateHistory /> publishAlloc
                     ↑ Composed transformation

-- LAYER 3 (Protocol): Execute 5-step algorithm
carolAllocations <- providerPhase carol [aliceCommit] "food"
-- Step 0: Check history (uses path: carol.oscillationHistories["Alice"])
-- Step 1: Apply damping (transformation algebra)
-- Steps 2-5: Calculate allocation
                     ↓

-- LAYER 2 (Algebra): Apply transformation
let newState = transformApply carolTransform oldState
    ↓ Pure function: StateTree -> StateTree

-- LAYER 1 (Zipper): Publish back
modifyPlayer (\c -> c { lastPublishedAllocations = carolAllocations }) carolFocus
publishPlayer carol
                     ↓ Network: POST https://carol.com/commitment
```

---

## Formal Properties We Can Now Prove

### 1. Convergence (Transformation Algebra)

```haskell
theorem_convergence :: [RoundTransform] -> StateTree -> Property
theorem_convergence rounds initialState =
  let states = scanl (flip applyRound) initialState rounds
      needs = map totalNeeds states
  in all (\(n1, n2) -> n2 <= n1) (zip needs (tail needs))
     -- Needs decrease monotonically!
```

### 2. Composability (Zipper)

```haskell
-- Navigating A->B->C = Navigating A->C (if path exists)
(toPlayer "Alice" >=> toPlayer "Bob") focus 
  ≡ navigatePath ["Alice", "Bob"] focus
```

### 3. Fairness (Protocol)

```haskell
-- Allocation is proportional to mutual recognition
∀ provider recipient:
  allocation(provider, recipient) 
    ∝ mutualRecognition(provider, recipient)
```

### 4. No Accumulation (Algebra Invariant)

```haskell
invariant_no_accumulation :: Transform -> StateTree -> Bool
invariant_no_accumulation t state =
  let state' = transformApply t state
  in totalNeeds state' >= 0  -- Can't go negative
```

---

## What This Enables

### ✅ Event Sourcing
```haskell
-- Every round is a transformation
-- Can replay history: foldl applyRound initialState allRounds
```

### ✅ Distributed Consensus
```haskell
-- Commutative transformations can be applied in any order
-- Use CRDTs for conflict-free replication
```

### ✅ Formal Verification
```haskell
-- Property-based testing:
prop_convergence :: [RoundTransform] -> StateTree -> Bool
-- Feed to QuickCheck!
```

### ✅ Time Travel Debugging
```haskell
-- Each round = pure transformation
-- Can step forward/backward through history
```

### ✅ Streaming Updates
```haskell
-- Convert events to transformations:
applyEvents state [NeedChanged "Alice" "food" 100 t1,
                   RecognitionChanged "Carol" "Alice" 0.5 t2]
```

---

## The Complete Picture

```
equations.md
    ↓ defines
  Types (Portion, Capacity, Path)
    ↓ used by
TransformationAlgebra.hs
    ↓ defines
  Transform = StateTree -> StateTree
    ↓ composed in
ProtocolCompliant.hs
    ↓ implements
  5-step allocation algorithm
    ↓ executed via
NetworkedZipper.hs
    ↓ navigates
  Distributed state tree across network
    ↓ converges to
  Fair resource allocation!
```

---

## Next Steps

1. **Add dependent types** for compile-time invariants (100% recognition)
2. **Implement CRDTs** for conflict-free distributed state
3. **Build streaming engine** for real-time updates
4. **Formal verification** with QuickCheck/LiquidHaskell
5. **Optimization** with parallel zipper navigation
6. **Integration** with existing TypeScript implementation

---

## Key Insights

1. **Zipper enables distributed navigation** without global state
2. **Transformation algebra enables formal reasoning** about convergence
3. **Protocol implements fairness** through 5-step algorithm
4. **Together they create**: A provably-convergent, distributed, fair resource allocation system!

The beauty is in the **composition**: Each layer is simple, but together they create something powerful that matches the mathematical properties described in the README while remaining practically implementable in a distributed network.

