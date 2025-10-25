# Compute Folder Upgrade Plan
## Migrating to Unified User Space Architecture

---

## Current State: Fragmentation

### Files in `compute/`:
1. **`compute.svelte.ts`** - Runtime using direct Holster access
2. **`node-store.svelte.ts`** - Node stores (partially space-aware)
3. **`program-hash.svelte.ts`** - Program utilities
4. **`schema.ts`** - Compute schemas ✅ (already good)
5. **`space-schemas.ts`** - User space schemas ✅ (already good)
6. **`space-stores.svelte.ts`** - User space stores ✅ (already good)

### Problems:
- `compute.svelte.ts` uses direct Holster paths (not `UserSpacePaths`)
- No integration with program registry
- Provenance written directly, not through space-stores
- Subscriptions not tracked in subscription namespace
- Outputs written directly, not through space-stores

---

## Goal: Unified Architecture

### The Vision:
```
compute.svelte.ts (Runtime)
     ↓
space-stores.svelte.ts (Kernel)
     ↓
UserSpacePaths (Path Convention)
     ↓
Holster (Storage Layer)
```

**All I/O goes through space-stores, following user space structure.**

---

## Migration Steps

### Phase 1: Update `compute.svelte.ts` to use Space Stores

#### 1.1: Import space-stores
```typescript
// ADD to imports
import {
  writeVariable,
  writeComputationResult,
  writeOutput,
  writeProvenance,
  registerLocalSubscription,
  registerPeerSubscription,
  UserSpacePaths
} from './space-stores.svelte';
```

#### 1.2: Update Variable Resolution
**Current** (line ~100-200):
```typescript
// Uses createStore with direct holster paths
case 'subscription': {
  const store = createStore({
    holsterPath: prefixHolsterPath(binding.holster_path, programHash),
    schema: getSchema(binding.schema_type)
  });
  store.initialize();
  // ...
}
```

**New**:
```typescript
case 'subscription': {
  // Use UserSpacePaths for consistency
  const fullPath = UserSpacePaths.computeVariable(
    myPubKey,
    programHash || 'default',
    binding.holster_path
  );
  
  const store = createStore({
    holsterPath: fullPath.replace(/^~[^/]+\//, ''), // Remove user prefix
    schema: getSchema(binding.schema_type)
  });
  store.initialize();
  
  // Track subscription in subscription namespace
  if (binding.subscribe_to_user) {
    registerPeerSubscription(
      binding.subscribe_to_user,
      binding.holster_path,
      binding.schema_type,
      `${programHash}_${binding.holster_path}`
    );
  } else {
    registerLocalSubscription(
      binding.holster_path,
      binding.schema_type,
      `${programHash}_${binding.holster_path}`
    );
  }
  
  // ...
}
```

#### 1.3: Update Output Handling
**Current** (line ~270-318):
```typescript
case 'holster': {
  // Direct Holster write
  const fullPath = programHash 
    ? prefixHolsterPath(binding.holster_path, programHash)
    : binding.holster_path;
    
  holsterUser.get(fullPath).put(value, (err) => {
    // ...
  });
}
```

**New**:
```typescript
case 'holster': {
  // Use space-stores for all writes
  await writeOutput(
    programHash || 'default',
    outputKey,
    value,
    binding.holster_path
  );
  
  // Also write to the actual holster path (for backwards compat)
  const fullPath = programHash 
    ? prefixHolsterPath(binding.holster_path, programHash)
    : binding.holster_path;
    
  holsterUser.get(fullPath).put(value, (err) => {
    if (err) {
      console.error('[REACTIVE-COMPUTE] Error persisting output:', err);
    }
  });
}
```

#### 1.4: Update Provenance Writing
**Current** (scattered in executeComputationInternal):
```typescript
// Direct Holster write to provenance path
const provenancePath = buildProvenancePath(programHash, provenanceId);
holsterUser.get(provenancePath).put(provenance, (err) => {
  // ...
});
```

**New**:
```typescript
// Use space-stores
await writeProvenance(
  programHash || 'default',
  provenanceId,
  {
    record: provenance,
    signature: createProvenanceSignature(provenance)
  }
);

// Also write to program-specific path (for backwards compat)
const provenancePath = buildProvenancePath(programHash, provenanceId);
holsterUser.get(provenancePath).put(provenance, (err) => {
  // ...
});
```

#### 1.5: Track Computation Results
**Add after computation execution**:
```typescript
// Track result in compute namespace
await writeComputationResult(
  programHash || 'default',
  computation.id,
  result
);
```

#### 1.6: Track Variables
**Add in variable resolution**:
```typescript
// Track variable access in compute namespace
await writeVariable(
  programHash || 'default',
  variableName,
  value
);
```

---

### Phase 2: Create ComputeRuntimeManager Class

Create a new class that wraps the runtime with space-store integration:

```typescript
// compute-runtime-manager.svelte.ts

export class ComputeRuntimeManager {
  private programHash: string;
  private runtime: ReactiveComputationRuntime;
  
  constructor(
    graph: ReactiveComputationGraph,
    options?: {
      enableProvenance?: boolean;
      enableLineageTracking?: boolean;
    }
  ) {
    this.programHash = graph.program_hash || getProgramHash(graph);
    
    // Register program in registry
    this.registerInSpace(graph);
    
    // Create runtime
    this.runtime = new ReactiveComputationRuntime(graph, options);
  }
  
  private async registerInSpace(graph: ReactiveComputationGraph) {
    const myPub = get(myPubKey);
    if (!myPub) return;
    
    // Register in program registry
    await registerProgram(this.programHash, {
      definition: graph,
      metadata: {
        version: graph.version || '1.0.0',
        description: graph.description || '',
        created_at: Date.now(),
        updated_at: Date.now()
      },
      status: {
        active: true,
        enabled: true
      }
    });
    
    // Activate program
    await activateProgram(this.programHash);
  }
  
  async start() {
    await this.runtime.start();
  }
  
  async stop() {
    await this.runtime.stop();
  }
  
  // Expose runtime methods with space-store integration
  getResults() {
    return this.runtime.getResults();
  }
}
```

---

### Phase 3: Integrate with Existing Allocation Code

Update `algorithm.svelte.ts` to use the new system:

```typescript
// algorithm.svelte.ts

// Create allocation program graph
const allocationProgramGraph: ReactiveComputationGraph = {
  id: 'allocation_v2',
  version: '2.0.0',
  description: 'Two-tier slot-native allocation with mutual recognition',
  
  variables: {
    myCommitment: {
      type: 'subscription',
      holster_path: 'allocation/commitment',
      schema_type: 'Commitment'
    },
    networkCommitments: {
      type: 'subscription',
      holster_path: 'allocation/network',
      schema_type: 'Object' // Map of commitments
    }
  },
  
  computations: [
    {
      id: 'compute_allocations',
      inputs: {
        myCommitment: { type: 'local', state_path: 'myCommitment' },
        networkCommitments: { type: 'local', state_path: 'networkCommitments' }
      },
      compute_fn: 'twoTierAllocation',
      outputs: {
        allocationState: {
          type: 'holster',
          holster_path: 'allocation/allocation_state'
        }
      }
    }
  ]
};

// Register allocation computation function
registerComputationFunction('twoTierAllocation', (inputs) => {
  const { myCommitment, networkCommitments } = inputs;
  
  // Run allocation algorithm
  const allocationState = computeTwoTierAllocation(
    myCommitment,
    networkCommitments
  );
  
  return { allocationState };
});

// Create runtime manager
const allocationRuntime = new ComputeRuntimeManager(
  allocationProgramGraph,
  {
    enableProvenance: true,
    enableLineageTracking: true
  }
);

// Start reactive computation
await allocationRuntime.start();
```

---

### Phase 4: Update Existing Stores

Modify `stores.svelte.ts` to be a thin wrapper over space-stores:

```typescript
// stores.svelte.ts (REFACTORED)

/**
 * Allocation Stores - V2 Compatibility Layer
 * 
 * This file now re-exports from space-stores for backwards compatibility.
 * New code should import directly from space-stores.svelte.ts
 */

import {
  myCommitmentStore as _myCommitmentStore,
  myAllocationStateStore as _myAllocationStateStore,
  networkAllocationsStore
} from './compute/space-stores.svelte';

// Re-export space stores
export const myCommitmentStore = _myCommitmentStore;
export const myAllocationStateStore = _myAllocationStateStore;

// Adapt networkAllocationsStore to old interface
export const networkCommitments = new Map<string, Commitment>();
export const networkAllocationStates = new Map<string, TwoTierAllocationState>();

// Keep synchronized with networkAllocationsStore
networkAllocationsStore.subscribe(map => {
  networkCommitments.clear();
  networkAllocationStates.clear();
  
  for (const [pubKey, data] of map) {
    if (data.commitment) {
      networkCommitments.set(pubKey, data.commitment);
    }
    if (data.allocation_state) {
      networkAllocationStates.set(pubKey, data.allocation_state);
    }
  }
});

// Re-export subscription functions
export {
  subscribeToPeerAllocation as subscribeToFullParticipant,
  initializeUserSpaceStores as initializeAllocationStores,
  cleanupUserSpaceStores as cleanupAllocationStores
} from './compute/space-stores.svelte';
```

---

## Benefits After Migration

### 1. Single Source of Truth
- All paths use `UserSpacePaths`
- No hardcoded strings
- Refactor-safe

### 2. Automatic Tracking
- Subscriptions tracked in subscription namespace
- Provenance in provenance namespace
- Programs in program registry

### 3. Composability
- Programs can reference each other
- Outputs become inputs automatically
- Network effects

### 4. Observability
```typescript
// Get complete diagnostics
const diagnostics = getUserSpaceDiagnostics();
console.log({
  programs: diagnostics.programs,           // What's running?
  subscriptions: diagnostics.subscriptions, // What's being watched?
  nodes: diagnostics.nodes,                 // What data exists?
  causality: diagnostics.causality          // What's the state?
});
```

### 5. Verifiable Computation
- All I/O goes through space-stores
- Complete provenance chains
- Byzantine fault tolerance

---

## Migration Checklist

### Phase 1: Update compute.svelte.ts ✅
- [ ] Import space-stores utilities
- [ ] Update variable resolution to use UserSpacePaths
- [ ] Track subscriptions in subscription namespace
- [ ] Update output handling to use writeOutput
- [ ] Update provenance to use writeProvenance
- [ ] Track computation results with writeComputationResult
- [ ] Track variable access with writeVariable

### Phase 2: Create Runtime Manager ✅
- [ ] Create compute-runtime-manager.svelte.ts
- [ ] Integrate with program registry
- [ ] Auto-register programs on instantiation
- [ ] Auto-activate programs
- [ ] Expose space-aware API

### Phase 3: Migrate Allocation Code ✅
- [ ] Convert algorithm.svelte.ts to ReactiveComputationGraph
- [ ] Register allocation computation functions
- [ ] Create allocation runtime instance
- [ ] Test backwards compatibility

### Phase 4: Refactor Stores ✅
- [ ] Make stores.svelte.ts re-export from space-stores
- [ ] Create compatibility adapters
- [ ] Update documentation
- [ ] Mark old APIs as deprecated

### Phase 5: Documentation ✅
- [ ] Update README with new architecture
- [ ] Add migration guide for existing code
- [ ] Document space-stores API
- [ ] Create example programs

---

## Testing Strategy

### 1. Unit Tests
- Test space-stores functions in isolation
- Test compute runtime with mock space-stores
- Test path generation

### 2. Integration Tests
- Test full program lifecycle (register → activate → execute)
- Test subscription tracking
- Test provenance generation
- Test cross-program composition

### 3. Migration Tests
- Run old code (stores.svelte.ts) and new code (space-stores) side-by-side
- Verify identical behavior
- Verify data written to same paths

### 4. Backwards Compatibility Tests
- Import from old paths → verify still works
- Call old functions → verify still works
- Verify old UI components still work

---

## Timeline

### Week 1: Foundation
- Phase 1: Update compute.svelte.ts
- Write unit tests

### Week 2: Integration
- Phase 2: Create runtime manager
- Write integration tests

### Week 3: Migration
- Phase 3: Migrate allocation code
- Phase 4: Refactor stores
- Write migration tests

### Week 4: Validation
- Phase 5: Documentation
- Backwards compatibility testing
- Performance testing

---

## Success Criteria

✅ All existing functionality works
✅ All tests pass
✅ No breaking changes to public API
✅ Diagnostics show proper space structure
✅ Provenance chains verifiable
✅ Programs composable
✅ 0 linter errors

---

## Next Steps

**Immediate**: 
1. Start Phase 1 - update compute.svelte.ts variable resolution
2. Add space-stores imports
3. Update one function at a time

**This Week**:
1. Complete Phase 1
2. Start Phase 2 (runtime manager)

**This Month**:
1. Complete all phases
2. Full documentation
3. Example programs

---

## Questions to Resolve

1. **Backwards compatibility**: How long to support old API?
   - Recommendation: 1-2 releases with deprecation warnings

2. **Migration path for existing data**: Do we migrate old Holster paths?
   - Recommendation: Write to both locations during transition

3. **Performance impact**: Does going through space-stores add latency?
   - Recommendation: Benchmark, optimize if needed

4. **Error handling**: How to handle space-store failures?
   - Recommendation: Fallback to direct Holster + error logging

5. **Program versioning**: How to handle program upgrades?
   - Recommendation: Programs registry supports versions, old versions keep running

