# Holster User Space Structure

## Overview

The Holster user space is organized as a tree structure rooted at each user's public key. This document describes the canonical organization for declarative reactive computation programs.

```
~{myPubKey}/
├── programs/
│   ├── registry/
│   │   └── {programHash}/
│   │       ├── definition                    # ReactiveComputationGraph
│   │       ├── metadata                      # {version, description, created_at, updated_at}
│   │       └── status                        # {active: boolean, enabled: boolean}
│   │
│   ├── active/
│   │   └── {programHash} → /programs/registry/{programHash}
│   │
│   ├── inactive/
│   │   └── {programHash} → /programs/registry/{programHash}
│   │
│   └── subscribed/
│       └── {peerPubKey}/
│           └── {programHash}/
│               ├── definition                # Peer's program definition
│               └── last_synced               # Timestamp
│
├── compute/
│   └── {programHash}/
│       ├── state/
│       │   ├── variables/
│       │   │   └── {variableName}            # Current value
│       │   ├── computations/
│       │   │   └── {computationId}/
│       │   │       ├── result                # Computation output
│       │   │       ├── last_executed         # Timestamp
│       │   │       └── execution_count       # Number
│       │   └── metadata/
│       │       ├── program_started_at
│       │       ├── last_computation
│       │       └── total_executions
│       │
│       ├── outputs/
│       │   └── {outputKey}/
│       │       ├── value                     # Final output value
│       │       ├── holster_path              # Where it's persisted
│       │       └── updated_at                # Timestamp
│       │
│       └── provenance/
│           └── {provenanceId}/
│               ├── record                    # ComputationProvenance
│               │   ├── id
│               │   ├── itcStamp
│               │   ├── executedBy
│               │   ├── timestamp
│               │   ├── programHash
│               │   ├── computationId
│               │   ├── computationHash
│               │   ├── inputs                # Map of input provenance
│               │   ├── outputs               # Map of output provenance
│               │   ├── deterministicHash
│               │   └── parents
│               └── signature                 # For verification
│
├── subscriptions/
│   ├── outbound/                             # What I'm watching
│   │   ├── local/
│   │   │   └── {holsterPath}/
│   │   │       ├── schema_type
│   │   │       ├── subscribers               # Array of callback IDs
│   │   │       └── last_value
│   │   │
│   │   └── peers/
│   │       └── {peerPubKey}/
│   │           └── {holsterPath}/
│   │               ├── schema_type
│   │               ├── subscribers
│   │               ├── last_value
│   │               └── last_synced
│   │
│   └── inbound/                              # Who's watching me
│       └── {peerPubKey}/
│           └── {holsterPath}                 # List of paths they subscribe to
│
├── nodes/                                     # Tree nodes with storage
│   └── {nodeId}/
│       ├── node                              # RootNode or NonRootNode
│       │   ├── id
│       │   ├── name
│       │   ├── type
│       │   ├── children
│       │   └── ...                          # Node-specific fields
│       │
│       └── storage/                          # NodeDataStorage (if present)
│           ├── data                          # Arbitrary data
│           ├── holster_path
│           ├── data_schema_type
│           ├── data_updated_at
│           ├── is_loading
│           ├── is_persisting
│           ├── last_network_timestamp
│           ├── auto_persist
│           ├── persist_debounce_ms
│           ├── subscribe_to_user
│           └── equality_check
│
├── causality/
│   ├── itc_stamp/                            # My current ITC stamp
│   │   ├── id                                # ITC ID (0, 1, or tree)
│   │   └── event                             # ITC Event (int or tree)
│   │
│   └── peer_stamps/
│       └── {peerPubKey}/
│           ├── itc_stamp                     # Their latest ITC stamp
│           └── last_seen                     # Timestamp
│
├── allocation/                                # Commons-specific data
│   ├── commitment                            # My Commitment
│   │   ├── capacity_slots                    # Array of AvailabilitySlot
│   │   ├── need_slots                        # Array of NeedSlot
│   │   ├── recognition_weights               # Record<pubKey, number>
│   │   ├── mr_values                         # Record<pubKey, number>
│   │   ├── itcStamp
│   │   ├── timestamp
│   │   ├── damping_factor
│   │   └── damping_history
│   │
│   ├── allocation_state                      # My computed allocations
│   │   ├── slot_denominators                 # Per-slot denominators
│   │   ├── slot_allocations                  # Array of SlotAllocationRecord
│   │   ├── recipient_totals                  # Summary by recipient
│   │   ├── converged                         # Boolean
│   │   ├── convergenceHistory                # Array
│   │   ├── itcStamp
│   │   └── timestamp
│   │
│   └── network/                              # Peer allocations
│       └── {peerPubKey}/
│           ├── commitment                    # Their Commitment
│           └── allocation_state              # Their allocations
│
├── trees/                                     # Priority/contribution trees
│   ├── my_tree                               # My RootNode
│   └── network_trees/
│       └── {peerPubKey}                      # Their RootNode
│
└── replication/                               # Encrypted peer data sync
    └── {peerPubKey}/
        └── encrypted_data                     # Peer-encrypted replication data

```

## Key Design Principles

### 1. Program Registry
Programs are registered once in `/programs/registry/{programHash}` and referenced from `/programs/active/` or `/programs/inactive/`. This prevents duplication and ensures consistent program definitions.

### 2. Computation State Isolation
Each program's execution state lives under `/compute/{programHash}/`, isolating variables, results, and provenance by program. This enables:
- Running multiple programs simultaneously
- Independent program lifecycle management
- Clear provenance tracking per program

### 3. Subscriptions are Bidirectional
The `/subscriptions/` namespace tracks both outbound (what I watch) and inbound (who watches me) subscriptions. This enables:
- Efficient subscription cleanup
- Privacy controls (know who's watching what)
- Network topology visibility

### 4. Nodes with Optional Storage
The `/nodes/` namespace stores tree structures where each node can optionally have a `storage` configuration that turns it into a reactive store with Holster subscriptions.

### 5. Causality Tracking
The `/causality/` namespace maintains ITC stamps for both the local user and all peers, enabling:
- Conflict resolution
- Stale update detection
- Distributed coordination without rounds

### 6. Domain-Specific Namespaces
The `/allocation/` and `/trees/` namespaces are specific to the commons allocation protocol. Other applications would have their own top-level namespaces for domain-specific data.

## Path Examples

### Storing a program definition:
```
~{myPubKey}/programs/registry/{programHash}/definition
```

### Storing a computation result:
```
~{myPubKey}/compute/{programHash}/state/computations/{computationId}/result
```

### Storing provenance:
```
~{myPubKey}/compute/{programHash}/provenance/{provenanceId}/record
```

### Subscribing to peer's commitment:
```
~{peerPubKey}/allocation/commitment
```
Tracked in my space at:
```
~{myPubKey}/subscriptions/outbound/peers/{peerPubKey}/allocation/commitment
```

### Node with storage subscribing to capacity:
```
~{myPubKey}/nodes/{nodeId}/storage
```
With `holster_path: "capacity/slot-123"` and `subscribe_to_user: "{peerPubKey}"`

### My current ITC stamp:
```
~{myPubKey}/causality/itc_stamp
```

### Peer's ITC stamp:
```
~{myPubKey}/causality/peer_stamps/{peerPubKey}/itc_stamp
```

## Data Flow

1. **Program Registration**: User defines a `ReactiveComputationGraph` → stored at `/programs/registry/{programHash}/definition`

2. **Program Activation**: Reference created at `/programs/active/{programHash}` → compute engine initializes

3. **Variable Binding**: For each variable with `type: 'subscription'`:
   - Store subscription config at `/subscriptions/outbound/.../`
   - Call `holster.get(holster_path).on(callback)`
   - Store current value at `/compute/{programHash}/state/variables/{variableName}`

4. **Computation Execution**: When subscribed data changes:
   - Increment ITC stamp at `/causality/itc_stamp`
   - Execute computation
   - Store result at `/compute/{programHash}/state/computations/{computationId}/result`
   - Generate provenance at `/compute/{programHash}/provenance/{provenanceId}/record`

5. **Output Persistence**: Based on output binding:
   - `type: 'holster'` → `holster.get(holster_path).put(value)`
   - `type: 'local'` → Store at `/compute/{programHash}/outputs/{outputKey}/value`
   - `type: 'memory'` → Store only in runtime (not persisted)

6. **Cross-Peer Subscription**: When subscribing to peer data:
   - Store subscription at `/subscriptions/outbound/peers/{peerPubKey}/{holsterPath}`
   - Call `holster.get("~{peerPubKey}/{holsterPath}").on(callback)`
   - Merge peer's ITC stamp at `/causality/peer_stamps/{peerPubKey}/itc_stamp`

## Storage Efficiency

- **Deduplication**: Program definitions stored once, referenced multiple times
- **Lazy Loading**: Provenance records only loaded when needed for verification
- **Temporal Pruning**: Old computation states can be pruned while preserving provenance
- **Compressed ITC**: ITC stamps are O(log n) space, much smaller than vector clocks
- **Indexed Subscriptions**: Fast lookup of subscriptions by path or peer

## Security & Privacy

- **User Space Encryption**: All data under `~{myPubKey}/` is encrypted with user's keypair
- **Provenance Signatures**: Computation records cryptographically signed
- **Subscription Privacy**: Inbound subscriptions show who's watching (no stealth monitoring)
- **Selective Replication**: Only explicitly shared paths replicate to peers
- **ITC Causality**: Prevents accepting stale or forged updates from peers

