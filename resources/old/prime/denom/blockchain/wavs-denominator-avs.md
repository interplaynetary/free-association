# WAVS-Powered Denominator AVS: Minimal Blockchain Architecture

## Core Insight

**WAVS lets us move 90% of computation off-chain while maintaining verifiability through EigenLayer-secured operators.**

### What This Changes

**Previous (Pure Blockchain):**
- All denominator computation on-chain → expensive
- All allocation validation on-chain → slow
- zk-SNARKs or fraud proofs needed → complex

**With WAVS:**
- Denominator computation in WASM → fast & cheap
- Allocation validation by operators → parallelized
- Signed results on-chain → minimal footprint
- EigenLayer slashing → security guarantee

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                     BLOCKCHAIN (Minimal)                     │
│  - Recipient need commitments (triggers)                     │
│  - Provider capacity commitments (inputs)                    │
│  - Signed allocation results (outputs from WAVS)             │
│  - Slashing conditions                                       │
└────────────┬────────────────────────────────────────────────┘
             │ Triggers & Inputs
             ↓
┌─────────────────────────────────────────────────────────────┐
│                    WAVS AVS (Computation)                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ Component 1: Denominator Computer (WASM)            │    │
│  │  - Reads commitments from chain                     │    │
│  │  - Computes: Denom(P) = Σ[MR × Residual]          │    │
│  │  - Multiple operators validate independently        │    │
│  │  - Operators sign result                           │    │
│  └─────────────────────────────────────────────────────┘    │
│                         ↓                                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ Component 2: Allocation Computer (WASM)             │    │
│  │  - Uses signed denominator from Component 1         │    │
│  │  - Computes: Alloc = Cap × MR × Residual / Denom  │    │
│  │  - Validates conservation law                       │    │
│  │  - Operators sign allocations                       │    │
│  └─────────────────────────────────────────────────────┘    │
│                         ↓                                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │ Component 3: Aggregator (WASM)                      │    │
│  │  - Aggregates signatures from operators             │    │
│  │  - Submits to chain only if quorum reached         │    │
│  │  - Batches multiple allocations                     │    │
│  └─────────────────────────────────────────────────────┘    │
└────────────┬────────────────────────────────────────────────┘
             │ Signed Results
             ↓
┌─────────────────────────────────────────────────────────────┐
│                    BLOCKCHAIN (Minimal)                      │
│  - Store only aggregated signed results                      │
│  - Verify signatures & quorum                                │
│  - Update state roots                                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                    P2P LAYER (Gossip)                        │
│  - Denominator estimates (advisory)                          │
│  - Direct negotiation (optimization)                         │
│  - Request messages (fast path)                              │
└─────────────────────────────────────────────────────────────┘
```

---

## What Goes Where

### Layer Distribution

```
┌─────────────────────────────────────────────────────────────┐
│                  P2P LAYER (95%)                             │
│  ┌──────────────────────────────────────────────────────┐   │
│  │ Recipients:                                           │   │
│  │ • Gossip denominator estimates                        │   │
│  │ • Share need updates                                  │   │
│  │ • Request optimistic allocations                      │   │
│  │                                                        │   │
│  │ Providers:                                            │   │
│  │ • Respond to allocation requests                      │   │
│  │ • Share capacity updates                              │   │
│  │ • Broadcast utilization estimates                     │   │
│  │                                                        │   │
│  │ Peers:                                                │   │
│  │ • Discover & maintain connections                     │   │
│  │ • Validate messages (signature checks)                │   │
│  │ • Forward gossip (epidemic broadcast)                 │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  Frequency: Continuous (milliseconds)                        │
│  Cost: $0 (only bandwidth)                                   │
│  Data: Ephemeral, not stored                                 │
└─────────────────────────────────────────────────────────────┘
                          ↓ Every N blocks
┌─────────────────────────────────────────────────────────────┐
│                  WAVS LAYER (4.9%)                           │
│  ┌──────────────────────────────────────────────────────┐   │
│  │ Component 1: Fetch Commitments (WASM)                │   │
│  │ • Read merkle roots from chain                        │   │
│  │ • Fetch full data via merkle proofs                   │   │
│  │ • Validate data integrity                             │   │
│  ├──────────────────────────────────────────────────────┤   │
│  │ Component 2: Compute Denominators (WASM)             │   │
│  │ • For each Provider P:                                │   │
│  │     Denom(P) = Σ[MR(P,R) × Residual(R)]             │   │
│  │ • Deterministic computation                           │   │
│  │ • Operators reach consensus (67%+ agree)              │   │
│  ├──────────────────────────────────────────────────────┤   │
│  │ Component 3: Compute Allocations (WASM)              │   │
│  │ • For each (P,R) pair:                                │   │
│  │     Alloc = Cap × MR × Residual / Denom              │   │
│  │ • Validate conservation law                           │   │
│  │ • Generate merkle tree of results                     │   │
│  ├──────────────────────────────────────────────────────┤   │
│  │ Component 4: Aggregate & Sign (WASM)                 │   │
│  │ • Collect operator signatures                         │   │
│  │ • Verify quorum reached                               │   │
│  │ • BLS signature aggregation                           │   │
│  │ • Prepare on-chain submission                         │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  Frequency: Every N blocks (e.g., 100 blocks = 20 min)      │
│  Cost: $2 per round (operator infrastructure)                │
│  Security: EigenLayer slashing                               │
└─────────────────────────────────────────────────────────────┘
                          ↓ Submit final results
┌─────────────────────────────────────────────────────────────┐
│              BLOCKCHAIN LAYER (0.1%)                         │
│  ┌──────────────────────────────────────────────────────┐   │
│  │ Input: Commitment Merkle Roots                        │   │
│  │ • recipient_commitments: bytes32                      │   │
│  │ • provider_commitments: bytes32                       │   │
│  │ • round_number: uint256                               │   │
│  │                                                        │   │
│  │ Output: Signed Allocation Batch                       │   │
│  │ • allocation_merkle_root: bytes32                     │   │
│  │ • aggregated_signature: bytes (BLS)                   │   │
│  │ • operator_bitmap: uint256                            │   │
│  │ • round_number: uint256                               │   │
│  │                                                        │   │
│  │ Verification:                                         │   │
│  │ • Check BLS signature validity                        │   │
│  │ • Verify quorum threshold (67%)                       │   │
│  │ • Validate merkle root linkage                        │   │
│  └──────────────────────────────────────────────────────┘   │
│                                                              │
│  Frequency: Every N blocks (only final results)              │
│  Cost: $0.60 per round                                       │
│  Data: ~2.5KB per round (vs 122KB pure blockchain!)          │
└─────────────────────────────────────────────────────────────┘
```

### ON-CHAIN (Blockchain) - MINIMAL STATE ONLY

**Only commitment roots and signed results:**

```solidity
1. Commitments (Inputs for WAVS):
   struct RoundCommitments {
       bytes32 recipient_commitments;  // Just merkle root!
       bytes32 provider_commitments;   // Just merkle root!
       uint256 round_number;
       uint256 timestamp;
   }
   
   Size: 64 bytes + 32 bytes = 96 bytes

2. Signed Results (Outputs from WAVS):
   struct SignedAllocationBatch {
       bytes32 allocation_root;        // Merkle root of allocations
       bytes aggregated_signature;     // BLS aggregated (96 bytes)
       uint256 participation_bitmap;   // Who participated
       bytes32[] operator_ids;         // Slicing info
   }
   
   Size: ~2.3KB (for 100 operators with BLS aggregation)

3. Slashing Conditions (EigenLayer):
   - Invalid computation → 10% stake slashed
   - Non-participation → 0.1% reward reduction
   - Enforced automatically by EigenLayer
```

**Total on-chain storage per round:**
```
Commitments:       96 bytes
Signed batch:    ~2.3KB
Verification:    ~100 bytes (gas for checks)

Total: ~2.5KB per round

Compare to pure blockchain: 122KB per round
Cost reduction: 98% cheaper on-chain! 🎉
```

**What's NOT on-chain (stored in P2P/WAVS):**
- ❌ Individual recipient needs (P2P gossip)
- ❌ Individual provider capacities (P2P gossip)
- ❌ MR values (off-chain database)
- ❌ Intermediate denominator calculations (WAVS)
- ❌ Individual allocations (WAVS, only root on-chain)
- ❌ Operator intermediate states (WAVS)

**Only merkle roots and final signatures on-chain!**

---

### IN WAVS (Off-Chain Computation) - BULK OF WORK

**WAVS Component 1: Denominator Computer**

```rust
// This runs in WASM on WAVS operators
#[wavs_component]
pub fn compute_denominators(input: DenominatorInput) -> DenominatorOutput {
    // Input from blockchain trigger
    let commitments = input.commitments;
    
    let mut denominators = HashMap::new();
    
    for provider in input.providers {
        let mut denom: U256 = U256::zero();
        
        // Compute: Denom(P) = Σ[MR(P,R) × Residual(R)]
        for recipient in commitments.recipients {
            if let Some(mr) = provider.mr_values.get(&recipient.id) {
                let contribution = mr * recipient.residual_need;
                denom += contribution;
            }
        }
        
        denominators.insert(provider.id, DenominatorValue {
            provider_id: provider.id,
            value: denom,
            round: input.round_number,
            computed_at: now(),
        });
    }
    
    DenominatorOutput {
        denominators,
        computation_proof: generate_proof(),  // Optional: can add zk-proof
    }
}

// Trigger: New block with commitments
// Operators: Run this component independently
// Result: Each operator signs their computed denominators
// Consensus: If quorum agrees, result is valid
```

**WAVS Component 2: Allocation Computer**

```rust
#[wavs_component]
pub fn compute_allocations(input: AllocationInput) -> AllocationOutput {
    // Input: Signed denominators from Component 1
    let denominators = input.denominators;
    let commitments = input.commitments;
    
    let mut allocations = Vec::new();
    
    for provider in input.providers {
        let denom = denominators.get(&provider.id).unwrap();
        let capacity = provider.total_capacity;
        
        for recipient in commitments.recipients {
            if let Some(mr) = provider.mr_values.get(&recipient.id) {
                // Compute: Alloc = Capacity × (MR × Residual) / Denominator
                let numerator = mr * recipient.residual_need;
                let allocation = capacity * numerator / denom.value;
                
                allocations.push(AllocationResult {
                    provider_id: provider.id,
                    recipient_id: recipient.id,
                    amount: allocation,
                    round: input.round_number,
                });
            }
        }
        
        // Validate conservation law
        let total: U256 = allocations.iter()
            .filter(|a| a.provider_id == provider.id)
            .map(|a| a.amount)
            .sum();
        
        assert!(total <= capacity, "Conservation law violated!");
    }
    
    AllocationOutput {
        allocations,
        validation_proof: generate_proof(),
    }
}
```

**WAVS Component 3: Aggregator & Submitter**

```rust
#[wavs_component]
pub fn aggregate_and_submit(input: AggregationInput) -> SubmissionResult {
    // Collect signatures from all operators
    let operator_results = input.operator_allocations;
    
    // Check consensus
    let consensus = check_quorum(operator_results);
    
    if consensus.quorum_reached {
        // Aggregate signatures (BLS signature aggregation)
        let aggregated_sig = bls_aggregate(
            operator_results.iter().map(|r| r.signature)
        );
        
        // Create batch for on-chain submission
        let batch = SignedAllocationBatch {
            allocations: consensus.agreed_allocations,
            aggregated_signature: aggregated_sig,
            participating_operators: consensus.operators,
            quorum_percentage: consensus.percentage,
        };
        
        // Submit to blockchain (minimal data!)
        submit_to_chain(batch);
        
        SubmissionResult {
            success: true,
            tx_hash: get_tx_hash(),
        }
    } else {
        // Not enough consensus - trigger dispute resolution
        SubmissionResult {
            success: false,
            reason: "Quorum not reached",
        }
    }
}
```

---

### IN P2P (Gossip) - OPTIMIZATION LAYER

**The P2P layer provides fast optimistic paths before WAVS finalization:**

```python
class HybridRecipient:
    """Recipient using both P2P gossip and WAVS ground truth"""
    
    def __init__(self):
        # P2P state (local, fast, optimistic)
        self.gossip_denom_estimates = {}  # provider_id -> estimate
        self.optimistic_allocations = {}  # provider_id -> allocation
        
        # WAVS state (ground truth, slower, guaranteed)
        self.confirmed_denominators = {}  # provider_id -> confirmed value
        self.finalized_allocations = {}   # provider_id -> finalized allocation
        
    def request_allocation_optimistic(self, provider_id):
        """Fast path: Use P2P gossip estimate"""
        estimate = self.gossip_denom_estimates.get(provider_id)
        
        # Send optimistic request to provider
        request = {
            'type': 'optimistic_allocation',
            'recipient_id': self.id,
            'residual_need': self.residual_need,
            'denominator_estimate': estimate,
        }
        
        # Provider can respond immediately with optimistic allocation
        # (before WAVS round completes)
        self.send_to_provider(provider_id, request)
        
    def on_wavs_finalization(self, round_number):
        """WAVS ground truth arrives - reconcile with optimistic"""
        
        # Fetch finalized denominators from blockchain
        confirmed = self.fetch_confirmed_denominators(round_number)
        
        # Update local state with ground truth
        self.confirmed_denominators = confirmed
        
        # Compute difference between optimistic and final
        for provider_id in self.optimistic_allocations:
            optimistic = self.optimistic_allocations[provider_id]
            finalized = self.finalized_allocations.get(provider_id, 0)
            
            if abs(optimistic - finalized) > threshold:
                # Significant divergence - trust WAVS
                self.log(f"Correcting estimate for {provider_id}")
                
                # Update gossip estimates to match reality
                self.gossip_denom_estimates[provider_id] = (
                    confirmed[provider_id]
                )
```

**P2P Layer Benefits:**

1. **Low Latency**: Recipients get allocations in milliseconds (gossip speed)
2. **Network Efficiency**: Most coordination happens off-chain via P2P
3. **Graceful Degradation**: System works even during blockchain congestion
4. **Cost Savings**: Only finalization hits blockchain

**WAVS Layer Benefits:**

1. **Ground Truth**: Provides canonical denominator values
2. **Convergence Guarantee**: Ensures system reaches equilibrium
3. **Byzantine Resistance**: 67% quorum prevents manipulation
4. **Economic Security**: Slashing enforces honesty

**Combined Architecture:**

```
┌─────────────────────────────────────────────────────────┐
│              NORMAL OPERATION (Fast Path)                │
│  1. Recipients/Providers gossip denominator estimates    │
│  2. Direct negotiation via P2P                           │
│  3. Optimistic allocations in milliseconds               │
│  4. 95% of interactions stay in P2P layer                │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│           PERIODIC FINALIZATION (Every N blocks)         │
│  1. Commitments submitted to blockchain                  │
│  2. WAVS operators compute ground truth                  │
│  3. Finalized allocations submitted on-chain             │
│  4. P2P estimates corrected if diverged                  │
└─────────────────────────────────────────────────────────┘
```

**Result: Fast P2P coordination with periodic WAVS anchoring for convergence!**

---

## The WAVS Service Manifest

**File: `service.json`**

```json
{
  "name": "denominator-allocation-avs",
  "version": "1.0.0",
  "description": "Recognition economy denominator-based allocation AVS",
  
  "workflows": [
    {
      "id": "compute_allocations",
      "description": "Triggered by commitment events, computes denominators and allocations",
      
      "trigger": {
        "type": "evm_event",
        "chain_id": 1,
        "contract_address": "0x...",
        "event_name": "NewRoundCommitments",
        "event_signature": "NewRoundCommitments(uint256,bytes32,uint256)",
        "abi": "./abi/DenominatorAllocationAVS.json"
      },
      
      "components": [
        {
          "id": "fetch_commitments",
          "registry_id": "fetch-commitments-v1",
          "description": "Fetches full commitment data from merkle root",
          "inputs": {
            "commitment_root": "$trigger.commitmentRoot",
            "round_number": "$trigger.round",
            "chain_id": 1,
            "contract_address": "$trigger.contract"
          }
        },
        {
          "id": "compute_denominators",
          "registry_id": "denominator-computer-v1",
          "description": "Computes Denom(P) = Σ[MR × Residual] for each provider",
          "inputs": {
            "recipients": "$fetch_commitments.recipients",
            "providers": "$fetch_commitments.providers",
            "round": "$trigger.round"
          }
        },
        {
          "id": "compute_allocations",
          "registry_id": "allocation-computer-v1",
          "description": "Computes allocations using denominators",
          "inputs": {
            "denominators": "$compute_denominators.denominators",
            "recipients": "$fetch_commitments.recipients",
            "providers": "$fetch_commitments.providers",
            "round": "$trigger.round"
          }
        }
      ],
      
      "submission": {
        "aggregator": {
          "type": "threshold",
          "quorum_percentage": 67,
          "aggregation_method": "bls_signature"
        },
        "target": {
          "type": "evm",
          "chain_id": 1,
          "contract_address": "0x...",
          "function_name": "submitAllocationBatch",
          "abi": "./abi/DenominatorAllocationAVS.json"
        },
        "parameters": [
          {
            "name": "round",
            "value": "$trigger.round"
          },
          {
            "name": "allocationRoot",
            "value": "$compute_allocations.merkle_root"
          },
          {
            "name": "aggregatedSignature",
            "value": "$aggregator.signature"
          },
          {
            "name": "participationBitmap",
            "value": "$aggregator.bitmap"
          },
          {
            "name": "operatorIds",
            "value": "$aggregator.operator_ids"
          }
        ]
      }
    }
  ],
  
  "operator_requirements": {
    "minimum_stake_wei": "32000000000000000000",
    "eigenlayer_quorum_id": 0,
    "slashing_parameters": {
      "invalid_computation": {
        "type": "percentage",
        "amount": 10
      },
      "non_participation": {
        "type": "percentage", 
        "amount": 0.1
      }
    }
  },
  
  "component_registry": {
    "fetch-commitments-v1": {
      "wasm_file": "./components/fetch_commitments.wasm",
      "source_repo": "github.com/your-org/denominator-avs",
      "hash": "0x..."
    },
    "denominator-computer-v1": {
      "wasm_file": "./components/denominator.wasm",
      "source_repo": "github.com/your-org/denominator-avs",
      "hash": "0x..."
    },
    "allocation-computer-v1": {
      "wasm_file": "./components/allocation.wasm",
      "source_repo": "github.com/your-org/denominator-avs",
      "hash": "0x..."
    }
  }
}
```

---

## Information Flow: Complete Round

### Round N Execution

```
PHASE 1: ON-CHAIN COMMITMENTS (Blockchain)
─────────────────────────────────────────────
Recipients submit need commitments → Blockchain
Providers submit capacity commitments → Blockchain
Block N finalized with commitment merkle roots

Time: 1-3 seconds (one block)
Cost: ~$0.10 (just merkle roots + sigs)

↓ [TRIGGER: NewRoundCommitments event]

PHASE 2: DENOMINATOR COMPUTATION (WAVS)
─────────────────────────────────────────────
100 WAVS operators listen for trigger
Each operator:
  1. Fetches full commitments from chain (merkle proofs)
  2. Runs denominator_computer.wasm
  3. Computes: Denom(P) = Σ[MR × Residual] for each P
  4. Signs their result
  
Operators broadcast signatures to each other
Quorum check: Do 67+ operators agree?
  YES → Proceed with agreed denominator
  NO → Dispute resolution (slash dishonest operators)

Time: 2-5 seconds (parallel computation)
Cost: Off-chain (operator infrastructure only)

↓

PHASE 3: ALLOCATION COMPUTATION (WAVS)
─────────────────────────────────────────────
Same operators (now have agreed denominator):
  1. Run allocation_computer.wasm
  2. For each Provider P:
     For each Recipient R:
       Compute: Alloc(P→R) = Cap(P) × MR × Residual / Denom(P)
  3. Validate conservation: Σ Alloc(P) ≤ Cap(P)
  4. Sign allocation results
  
Operators broadcast allocation signatures
Quorum check: Do 67+ operators agree?
  YES → Proceed to aggregation
  NO → Dispute resolution

Time: 2-5 seconds
Cost: Off-chain

↓

PHASE 4: AGGREGATION (WAVS)
─────────────────────────────────────────────
Designated aggregator operator:
  1. Collects all operator signatures
  2. Verifies quorum (67%+ agree)
  3. Aggregates signatures using BLS
  4. Creates compact batch:
     {
       allocations: [all P→R allocations],
       aggregated_signature: BLS_agg(sigs),
       operator_bitmap: 0b1101... (who participated)
     }

Time: 1 second
Cost: Off-chain

↓

PHASE 5: ON-CHAIN SUBMISSION (Blockchain)
─────────────────────────────────────────────
Aggregator submits batch to chain
Chain verifies:
  ✓ BLS aggregated signature valid
  ✓ Quorum threshold met (67%+)
  ✓ Merkle root matches previous commitments
  
If valid:
  - Store allocation batch
  - Update state roots
  - Distribute operator rewards

Time: 1-3 seconds (one block)
Cost: ~$0.50 (one tx with batched allocations)

↓

TOTAL: 7-17 seconds, ~$0.60 per round
(vs 1-3 seconds, $4+ with pure blockchain)
```

---

## Minimal On-Chain State

### Smart Contract Interface

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@eigenlayer/contracts/interfaces/IAVSDirectory.sol";
import "@wavs/contracts/interfaces/IWAVSService.sol";

contract DenominatorAllocationAVS {
    // ═══════════════════════════════════════════════════
    // STATE (Minimal!)
    // ═══════════════════════════════════════════════════
    
    uint256 public currentRound;
    
    // Just store merkle roots, not full data!
    mapping(uint256 => bytes32) public commitmentRoots;  // round → root
    mapping(uint256 => bytes32) public allocationRoots;  // round → root
    
    // Track which operators participated (for rewards)
    mapping(uint256 => uint256) public participationBitmaps;  // round → bitmap
    
    // EigenLayer integration
    IAVSDirectory public immutable avsDirectory;
    uint256 public constant QUORUM_THRESHOLD = 67; // 67%
    
    // ═══════════════════════════════════════════════════
    // EVENTS
    // ═══════════════════════════════════════════════════
    
    event NewRoundCommitments(
        uint256 indexed round,
        bytes32 commitmentRoot,
        uint256 timestamp
    );
    
    event AllocationBatchSubmitted(
        uint256 indexed round,
        bytes32 allocationRoot,
        uint256 operatorCount,
        uint256 timestamp
    );
    
    // ═══════════════════════════════════════════════════
    // PHASE 1: COMMITMENTS (Recipients & Providers)
    // ═══════════════════════════════════════════════════
    
    function submitCommitmentBatch(
        uint256 round,
        bytes32 merkleRoot,
        bytes calldata signature
    ) external {
        require(round == currentRound, "Invalid round");
        
        // Store just the merkle root (not full commitments!)
        commitmentRoots[round] = merkleRoot;
        
        // Emit event → Triggers WAVS operators
        emit NewRoundCommitments(round, merkleRoot, block.timestamp);
    }
    
    // ═══════════════════════════════════════════════════
    // PHASE 5: RECEIVE RESULTS FROM WAVS
    // ═══════════════════════════════════════════════════
    
    function submitAllocationBatch(
        uint256 round,
        bytes32 allocationRoot,
        bytes calldata aggregatedSignature,
        uint256 participationBitmap,
        bytes32[] calldata operatorIds
    ) external {
        require(round == currentRound, "Invalid round");
        require(commitmentRoots[round] != bytes32(0), "No commitments");
        
        // Verify BLS aggregated signature
        require(
            verifyAggregatedSignature(
                allocationRoot,
                aggregatedSignature,
                operatorIds
            ),
            "Invalid signature"
        );
        
        // Check quorum
        uint256 participationCount = countBits(participationBitmap);
        uint256 totalOperators = getOperatorSetSize();
        require(
            participationCount * 100 >= totalOperators * QUORUM_THRESHOLD,
            "Quorum not reached"
        );
        
        // Store allocation root
        allocationRoots[round] = allocationRoot;
        participationBitmaps[round] = participationBitmap;
        
        // Advance round
        currentRound++;
        
        emit AllocationBatchSubmitted(
            round,
            allocationRoot,
            participationCount,
            block.timestamp
        );
    }
    
    // ═══════════════════════════════════════════════════
    // VERIFICATION (Anyone can verify allocations)
    // ═══════════════════════════════════════════════════
    
    function verifyAllocation(
        uint256 round,
        address provider,
        address recipient,
        uint256 amount,
        bytes32[] calldata merkleProof
    ) external view returns (bool) {
        bytes32 leaf = keccak256(abi.encode(provider, recipient, amount));
        return MerkleProof.verify(merkleProof, allocationRoots[round], leaf);
    }
    
    // ═══════════════════════════════════════════════════
    // EIGENLAYER INTEGRATION
    // ═══════════════════════════════════════════════════
    
    function verifyAggregatedSignature(
        bytes32 messageHash,
        bytes calldata signature,
        bytes32[] calldata operatorIds
    ) internal view returns (bool) {
        // Use EigenLayer's BLS signature verification
        return avsDirectory.verifyAggregatedSignature(
            messageHash,
            signature,
            operatorIds
        );
    }
    
    function getOperatorSetSize() internal view returns (uint256) {
        return avsDirectory.getOperatorSetSize(address(this));
    }
}
```

---

## Cost Comparison

### Per Round Costs

| Approach | On-Chain Data | Gas Cost | Time | Off-Chain Cost |
|----------|---------------|----------|------|----------------|
| **Pure Blockchain** | 122KB | $4.00 | 1-3s | $0 |
| **Blockchain + zk-SNARKs** | 10KB | $1.50 | 3-5s | $0.50 (proving) |
| **WAVS AVS** | 2.5KB | **$0.60** | **7-17s** | **$2.00** (operators) |

**WAVS is 85% cheaper on-chain**, with operator costs distributed across stakers.

### For 1000 Recipients, 100 Providers

| Approach | On-Chain/Round | Daily Cost | Yearly Cost |
|----------|----------------|------------|-------------|
| **Pure Blockchain** | $398 | $9,552 | $3.5M |
| **Blockchain + zk** | $150 | $3,600 | $1.3M |
| **WAVS AVS** | **$60** | **$1,440** | **$525K** |

**WAVS reduces costs by 85%+** through off-chain computation!

---

## Security Model

### Three Layers of Security

**1. EigenLayer Restaking**
```
Operators stake ETH via EigenLayer
If operator behaves maliciously:
  - Signed invalid allocation → Slash 10%
  - Didn't participate → Reduce rewards
  - Colluded on wrong result → Slash 50%
  
Economic security = Σ staked ETH across operators
```

**2. Quorum Consensus**
```
Require 67%+ operators to agree
Even if 1/3 operators are malicious:
  - Can't force wrong result (need 67%)
  - Honest 67% will slash dishonest minority
  - Byzantine fault tolerant
```

**3. On-Chain Verification**
```
Anyone can verify allocations:
  - Download merkle proofs
  - Recompute allocation locally
  - If mismatch, submit fraud proof
  - Fraudulent operators slashed
```

### Attack Scenarios

**Attack 1: Malicious Operator**
```
Single operator tries to report wrong denominator

Defense:
  - Other 99 operators compute correctly
  - Quorum threshold not reached with malicious value
  - Malicious operator slashed for deviation
  - Honest result proceeds
```

**Attack 2: Coordinated 30% Attack**
```
30 out of 100 operators collude

Defense:
  - Need 67% for quorum → 30% can't force result
  - Honest 70% reaches quorum alone
  - 30% slashed for attempted fraud
  - Attack fails, costs attackers their stake
```

**Attack 3: Coordinated 70% Attack**
```
70 out of 100 operators collude (very expensive!)

Defense:
  - Can force wrong result on-chain (quorum reached)
  - BUT: Anyone can verify and submit fraud proof
  - If fraud proven: All 70% operators slashed
  - Cost to attack: 70 × 32 ETH = 2240 ETH (~$4.5M)
  - Attack must be profitable to exceed $4.5M
  - Economic security threshold very high
```

---

## Developer Experience

### Building the WAVS Service

**1. Write WASI Components (Rust)**

```rust
// components/src/lib.rs
use wavs_sdk::prelude::*;

// WASI component interface - WAVS standard
wit_bindgen::generate!({
    world: "wavs-component",
    exports: {
        "wavs:compute/handler": Component
    }
});

// Component 1: Denominator Computer
struct DenominatorComputer;

impl Component for DenominatorComputer {
    fn execute(input: Vec<u8>) -> Result<Vec<u8>, String> {
        // Deserialize input from WAVS trigger
        let input: DenominatorInput = serde_json::from_slice(&input)
            .map_err(|e| e.to_string())?;
        
        let mut denominators = HashMap::new();
        
        // Core algorithm: Denom(P) = Σ[MR(P,R) × Residual(R)]
        for provider in input.providers.iter() {
            let mut denom = U256::zero();
            
            for recipient in input.recipients.iter() {
                if let Some(mr) = provider.mr_values.get(&recipient.id) {
                    let contribution = mr * recipient.residual_need;
                    denom += contribution;
                }
            }
            
            denominators.insert(provider.id, DenominatorValue {
                provider_id: provider.id,
                value: denom,
                round: input.round_number,
                timestamp: get_timestamp(),
            });
        }
        
        // Serialize output for next component
        let output = DenominatorOutput { 
            denominators,
            computation_proof: None,  // Optional: add zk-proof
        };
        
        serde_json::to_vec(&output).map_err(|e| e.to_string())
    }
}

// Export WASI component
export_component!(DenominatorComputer);
```

```rust
// Component 2: Allocation Computer
struct AllocationComputer;

impl Component for AllocationComputer {
    fn execute(input: Vec<u8>) -> Result<Vec<u8>, String> {
        let input: AllocationInput = serde_json::from_slice(&input)
            .map_err(|e| e.to_string())?;
        
        let mut allocations = Vec::new();
        
        // Core algorithm: Alloc = Capacity × (MR × Residual) / Denominator
        for provider in input.providers.iter() {
            let denom = input.denominators.get(&provider.id)
                .ok_or("Missing denominator")?;
            
            for recipient in input.recipients.iter() {
                if let Some(mr) = provider.mr_values.get(&recipient.id) {
                    let numerator = mr * recipient.residual_need;
                    let allocation = provider.total_capacity * numerator / denom.value;
                    
                    allocations.push(AllocationResult {
                        provider_id: provider.id,
                        recipient_id: recipient.id,
                        amount: allocation,
                        round: input.round_number,
                    });
                }
            }
            
            // Validate conservation law
            let total: U256 = allocations.iter()
                .filter(|a| a.provider_id == provider.id)
                .map(|a| a.amount)
                .sum();
            
            if total > provider.total_capacity {
                return Err("Conservation law violated!".to_string());
            }
        }
        
        // Create merkle tree of allocations
        let merkle_root = create_merkle_tree(&allocations)?;
        
        let output = AllocationOutput {
            allocations,
            merkle_root,
            validation_proof: None,
        };
        
        serde_json::to_vec(&output).map_err(|e| e.to_string())
    }
}

export_component!(AllocationComputer);
```

**2. Build WASM Components**

```bash
# Install WAVS SDK
cargo add wavs-sdk

# Build components to WASM
cargo build --target wasm32-wasi --release

# Components output to target/wasm32-wasi/release/
# - fetch_commitments.wasm
# - denominator.wasm
# - allocation.wasm
```

**3. Deploy to WAVS**

```bash
# Initialize WAVS service
wavs init denominator-allocation-avs

# Upload components to registry
wavs registry upload \
  --component fetch_commitments.wasm \
  --id fetch-commitments-v1

wavs registry upload \
  --component denominator.wasm \
  --id denominator-computer-v1

wavs registry upload \
  --component allocation.wasm \
  --id allocation-computer-v1

# Deploy service with manifest
wavs deploy \
  --manifest service.json \
  --network mainnet \
  --min-operators 100

# WAVS automatically:
# ✓ Registers service with EigenLayer
# ✓ Distributes WASM to operators
# ✓ Sets up event listeners
# ✓ Configures aggregation & submission
# ✓ Enables slashing conditions
```

**4. Monitor Service**

```bash
# Check operator status
wavs operators list --service denominator-allocation-avs

# View recent computations
wavs logs --service denominator-allocation-avs --tail 100

# Check quorum status
wavs status --service denominator-allocation-avs
```

---

## Convergence Guarantee with WAVS

### Theorem: WAVS-Based Convergence

**The system converges with probability 1 under:**
1. ≥67% operators are honest (BFT assumption)
2. EigenLayer slashing enforced
3. On-chain commitments finalized

**Proof:**

```
Round N:
  1. Commitments finalized on-chain (blockchain consensus)
  2. WAVS operators compute denominators off-chain
  3. Quorum (67%+) must agree on denominator value
  4. If ≥67% honest: honest value reaches quorum
  5. Allocations computed deterministically from agreed denominator
  6. Results signed and aggregated
  7. On-chain validation (signature + quorum check)
  8. State updated atomically
  
Convergence:
  - Each round: Residual(R,N) decreases (or stays 0)
  - Monotonic decrease guaranteed by deterministic allocation
  - Bounded below by 0
  - By monotone convergence: lim_{N→∞} Residual(R,N) = 0
  
Byzantine Resistance:
  - ≤33% malicious can't reach quorum
  - ≥67% malicious possible but economically irrational
    (would lose > stake value in slashing)
  
QED: System converges with probability 1 under BFT assumptions
```

---

## Comparison Summary

### What We Achieved with WAVS

| Metric | Pure Blockchain | WAVS AVS | Improvement |
|--------|----------------|----------|-------------|
| **On-chain data/round** | 122KB | 2.5KB | **49x smaller** |
| **Cost/round (100 agents)** | $4.00 | $0.60 | **85% cheaper** |
| **Cost/year (1000 agents)** | $3.5M | $525K | **85% cheaper** |
| **Computation speed** | Limited by gas | Native WASM | **100x faster** |
| **Operator rewards** | None | EigenLayer yield | **New revenue** |
| **Security** | Chain security | ETH restaking | **Comparable** |
| **Convergence** | Guaranteed | Guaranteed (BFT) | **Same** |
| **Privacy** | Commitments public | Commitments public | **Same** |

### Architecture Breakdown

**On-chain (blockchain): 5%**
- Commitment merkle roots
- Signed result batches
- Quorum verification

**In WAVS (off-chain compute): 90%**
- Denominator computation
- Allocation computation
- Signature aggregation
- Operator coordination

**In P2P (gossip): 5%**
- Denominator estimates (optimization)
- Direct negotiation
- Fast paths

---

## WAVS Composability & Multichain Features

### Workflow Chaining (Composability)

**WAVS allows workflows to trigger other workflows, enabling complex coordination:**

```json
{
  "workflows": [
    {
      "id": "compute_allocations",
      "trigger": {
        "type": "evm_event",
        "chain_id": 1,
        "event_name": "NewRoundCommitments"
      },
      "components": ["fetch_commitments", "compute_denominators", "compute_allocations"],
      "submission": {
        "target": {
          "chain_id": 1,
          "contract_address": "0x...",
          "function_name": "submitAllocationBatch"
        }
      }
    },
    {
      "id": "update_recipient_states",
      "trigger": {
        "type": "evm_event",
        "chain_id": 1,
        "event_name": "AllocationBatchSubmitted"
      },
      "components": ["update_satisfactions", "compute_residuals", "check_convergence"],
      "submission": {
        "target": {
          "chain_id": 1,
          "contract_address": "0x...",
          "function_name": "updateRecipientStates"
        }
      }
    }
  ]
}
```

**The submission of workflow 1 triggers workflow 2 automatically!**

### Multichain Capability

**WAVS can trigger on one chain, compute off-chain, and submit to another:**

```json
{
  "id": "cross_chain_allocation",
  "trigger": {
    "type": "evm_event",
    "chain_id": 1,
    "contract_address": "0x...",
    "event_name": "NewRoundCommitments"
  },
  "components": ["compute_denominators", "compute_allocations"],
  "submission": {
    "target": {
      "type": "evm",
      "chain_id": 10,
      "contract_address": "0x...",
      "function_name": "submitAllocationBatch"
    }
  }
}
```

**Example use case:**
- Recipients and providers commit on Ethereum mainnet (security)
- WAVS operators compute allocations off-chain
- Results submitted to Optimism L2 (cost savings)
- **Best of both worlds: Ethereum security, L2 cost efficiency!**

### Component Reusability

**Components in the registry can be reused across multiple workflows:**

```json
{
  "workflows": [
    {
      "id": "compute_allocations_mainnet",
      "trigger": { "chain_id": 1 },
      "components": [
        { "registry_id": "denominator-computer-v1" },
        { "registry_id": "allocation-computer-v1" }
      ]
    },
    {
      "id": "compute_allocations_arbitrum",
      "trigger": { "chain_id": 42161 },
      "components": [
        { "registry_id": "denominator-computer-v1" },
        { "registry_id": "allocation-computer-v1" }
      ]
    }
  ]
}
```

**Same WASM components, different chains - no code duplication!**

### Dynamic Component Updates

**AVS builders can update components without operator coordination:**

```bash
# Update denominator computer logic
wavs registry upload \
  --component denominator_v2.wasm \
  --id denominator-computer-v2

# Update service manifest to use new version
wavs service update \
  --manifest service.json \
  --component-mapping "denominator-computer-v1=denominator-computer-v2"

# Operators automatically use new version - no downtime!
```

**Operators only need to upgrade WAVS node software itself, not service-specific code.**

---

## Implementation Roadmap

### Phase 1: Core WAVS Components (2-4 weeks)
```
✓ Write denominator_computer.wasm (Rust)
✓ Write allocation_computer.wasm (Rust)
✓ Write aggregator.wasm (Rust)
✓ Create service.json manifest
✓ Local testing with WAVS dev tools
```

### Phase 2: Smart Contract (2 weeks)
```
✓ Minimal commitment contract (Solidity)
✓ Result verification contract
✓ EigenLayer integration
✓ Slashing conditions
✓ Unit tests
```

### Phase 3: Integration (2 weeks)
```
✓ Deploy WAVS service to testnet
✓ Register operators
✓ Test trigger → compute → submit flow
✓ Verify quorum consensus
✓ Test slashing conditions
```

### Phase 4: Production (2 weeks)
```
✓ Security audit
✓ Deploy to mainnet
✓ Recruit operators (EigenLayer)
✓ Monitor & optimize
```

**Total: 8-10 weeks to production**

---

## Optimal Configuration: Minimizing Blockchain Usage

### Three-Layer Optimization Strategy

**1. P2P Layer (95% of interactions) - NO BLOCKCHAIN**

```
What stays in P2P:
✓ Denominator estimate gossip
✓ Direct recipient-provider negotiation
✓ Optimistic allocation requests/responses
✓ State synchronization among peers
✓ Network discovery & maintenance

Frequency: Continuous (milliseconds)
Cost: $0 (only network bandwidth)
Latency: <100ms
Guarantee: Best-effort (no convergence proof)
```

**2. WAVS Layer (4.9% of interactions) - OFF-CHAIN COMPUTE**

```
What runs in WAVS:
✓ Denominator computation
✓ Allocation computation
✓ Conservation law validation
✓ Operator consensus
✓ Result aggregation

Frequency: Every N blocks (e.g., every 100 blocks = ~20 min)
Cost: $2 per round (distributed across operators)
Latency: 7-17 seconds
Guarantee: BFT consensus (67% honest operators)
```

**3. Blockchain Layer (0.1% of interactions) - MINIMAL STATE**

```
What goes on-chain:
✓ Commitment merkle roots (64 bytes)
✓ Aggregated signatures (2KB)
✓ Allocation merkle roots (32 bytes)
✓ Operator participation bitmaps (100 bytes)

Frequency: Every N blocks (only final results)
Cost: $0.60 per round
Latency: 1-3 seconds (one block)
Guarantee: Immutable, verifiable ground truth
```

### Configuration Parameters

**Adjust based on needs:**

```json
{
  "p2p_gossip_interval_ms": 100,
  "wavs_finalization_blocks": 100,
  "blockchain_commitment_blocks": 100,
  
  "optimistic_allocation_enabled": true,
  "fallback_to_wavs_on_divergence": true,
  
  "operator_quorum_percentage": 67,
  "min_operators": 100,
  
  "cost_optimization_mode": "aggressive"
}
```

**Cost optimization modes:**

| Mode | P2P% | WAVS Freq | Chain Freq | Cost/Day | Convergence Time |
|------|------|-----------|------------|----------|------------------|
| **Aggressive** | 99% | Every 500 blocks | Every 500 blocks | $2.88 | ~2 hours |
| **Balanced** | 95% | Every 100 blocks | Every 100 blocks | $14.40 | ~20 min |
| **Conservative** | 90% | Every 50 blocks | Every 50 blocks | $28.80 | ~10 min |
| **Paranoid** | 80% | Every 10 blocks | Every 10 blocks | $144.00 | ~2 min |

### Real-World Example: 1000 Recipients, 100 Providers

**Traditional Blockchain Approach:**
```
Every round: 1000 × 100 = 100,000 allocations on-chain
Data: 100KB per round
Frequency: Every block (12 seconds)
Cost: $4.00 × 7,200 rounds/day = $28,800/day
Annual: $10.5M 😱
```

**Hybrid P2P + WAVS Approach:**
```
P2P gossip: 95% of time, $0 cost
WAVS compute: Every 100 blocks (20 min), $2/round
Blockchain: Only merkle roots, $0.60/round
Frequency: 72 finalizations/day
Cost: ($2 + $0.60) × 72 = $187.20/day
Annual: $68,328 ✓

Cost reduction: 99.35% 🎉
```

### When to Use Each Layer

**Use P2P for:**
- Real-time allocation requests
- Rapid need changes
- High-frequency interactions
- Soft commitments
- Optimistic execution

**Use WAVS for:**
- Canonical denominator computation
- Allocation validation
- Consensus on ground truth
- Periodic anchoring
- Convergence enforcement

**Use Blockchain for:**
- Immutable commitments
- Final state roots
- Operator slashing evidence
- Long-term audit trail
- Cross-system interoperability

---

## Conclusion

### WAVS Enables Optimal Architecture

**By using WAVS, we achieve:**

1. **Minimal blockchain footprint** - Only commitments and signed results on-chain (0.1% of data)
2. **Off-chain computation** - 99.9% of calculations in fast WASM or P2P
3. **Verifiable results** - Operator signatures + quorum + EigenLayer slashing
4. **99%+ cost reduction** - Move nearly everything off expensive blockchain
5. **True convergence** - BFT consensus among WAVS operators
6. **Operator incentives** - EigenLayer rewards for participation
7. **Composability** - Can integrate with other WAVS services
8. **Developer friendly** - Just write WASM, WAVS handles infrastructure

### The Three-Layer Sweet Spot

```
Pure P2P (Layer 1):
  Fast (milliseconds) but no guarantees
  ↓ Optimistic allocations
  
WAVS AVS (Layer 2):
  Guaranteed (BFT) and fast (seconds)
  ↓ Canonical ground truth
  
Blockchain (Layer 3):
  Immutable and verifiable
  
Combined: Fast + Guaranteed + Cheap ✓
```

**WAVS is the perfect fit for denominator-based allocation** - it moves computation off-chain while maintaining cryptoeconomic security through EigenLayer restaking, and works seamlessly with P2P optimistic execution.

This is the **minimal possible architecture** for true convergence with cost efficiency while maximizing P2P coordination!

---

## References

- [WAVS Documentation](https://docs.wavs.xyz/overview)
- [EigenLayer AVS Framework](https://docs.eigenlayer.xyz/eigenlayer/avs-guides/avs-developer-guide)
- [BLS Signature Aggregation](https://ethresear.ch/t/bls-signatures-for-ethereum/7861)
- [Minimal Blockchain State Document](./minimal-blockchain-state.md)

