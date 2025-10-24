# Program Hashing & Data Namespacing

## Overview

The RDL (Reactive Dataflow Language) implements automatic **program hashing** to namespace all program data in Holster. Every RDL program gets a unique hash based on its structure, and all Holster paths are automatically prefixed with this hash.

## Data Organization

### Format
```
~pubkey/<program_hash>/<holster_path>
```

### Example
```typescript
// You define:
holster_path: "tree"

// Runtime uses:
holster_path: "a1b2c3d4e5f6g7h8/tree"

// Full path in Holster:
~yourPubkey/a1b2c3d4e5f6g7h8/tree
```

---

## Why Program Hashing?

### 1. **No Naming Collisions**
Different programs can use the same path names without conflict:

```
~pubkey/abc123/tree   ← Counter program's tree
~pubkey/def456/tree   ← MR program's tree
~pubkey/789xyz/tree   ← Allocation program's tree
```

### 2. **Program Versioning**
Different versions get different hashes:

```typescript
const programV1 = { ... };  // Hash: abc123
const programV2 = { ... };  // Hash: def456 (different logic)

// Data is isolated:
~pubkey/abc123/counter   ← V1 data
~pubkey/def456/counter   ← V2 data
```

### 3. **Multiple Instances Share Data**
Same program structure = same hash = shared data:

```typescript
const instance1 = new ComputationGraphRuntime(myProgram);
const instance2 = new ComputationGraphRuntime(myProgram);

instance1.getProgramHash(); // "abc123"
instance2.getProgramHash(); // "abc123" (same!)

// Both instances read/write: ~pubkey/abc123/*
```

### 4. **Clear Ownership**
Every data path clearly indicates which program owns it:

```
~pubkey/abc123/tree          ← Program abc123
~pubkey/abc123/nodes/node1   ← Program abc123
~pubkey/abc123/allocation    ← Program abc123
```

### 5. **Easy Cleanup**
Delete all program data by removing one directory:

```
rm -rf ~pubkey/abc123/*
```

---

## How It Works

### Automatic Hashing

The hash is computed from the program's **structure**, not its metadata:

```typescript
const hash = hashProgram(program);
// Hash includes:
// ✓ variables
// ✓ computations
// ✓ logic
// 
// Hash excludes:
// ✗ id
// ✗ version
// ✗ description
```

**Same logic = same hash**, even if metadata differs!

### Hash Computation

```typescript
import { hashProgram, getProgramHash } from '$lib/commons';

const program: ReactiveComputationGraph = {
  id: 'my-program',
  variables: { ... },
  computations: [ ... ]
};

// Compute hash
const hash = hashProgram(program);  // "a1b2c3d4e5f6g7h8"

// Or get from program (uses program.program_hash if set, otherwise computes)
const hash2 = getProgramHash(program);
```

### Automatic Prefixing

All holster paths are automatically prefixed:

```typescript
const program: ReactiveComputationGraph = {
  id: 'counter',
  variables: {
    count: {
      type: 'subscription',
      holster_path: 'counter',  // You define this
      schema_type: 'Number'
    }
  },
  computations: [{
    id: 'increment',
    inputs: {
      value: { type: 'subscription', holster_path: 'counter', schema_type: 'Number' }
    },
    compute_fn: 'increment',
    outputs: {
      result: {
        type: 'holster',
        holster_path: 'counter',  // You define this
        schema_type: 'Number'
      }
    }
  }]
};

// Runtime automatically creates stores at:
// - Input: ~pubkey/<hash>/counter
// - Output: ~pubkey/<hash>/counter
```

### Initialization Logging

The runtime logs the program hash and namespace:

```
[REACTIVE-COMPUTE] Created runtime for program: counter (hash: a1b2c3d4)
[REACTIVE-COMPUTE] Initializing graph: counter
[REACTIVE-COMPUTE] Program hash: a1b2c3d4
[REACTIVE-COMPUTE] All data will be namespaced under: a1b2c3d4/*
```

---

## Manual Hash Override

You can specify a custom hash for semantic versioning or readability:

```typescript
const program: ReactiveComputationGraph = {
  id: 'counter',
  
  // Manual hash override
  program_hash: 'counter-v1.0.0',
  
  variables: { ... },
  computations: [ ... ]
};

// Runtime will use: ~pubkey/counter-v1.0.0/*
```

**Benefits:**
- Readable data paths
- Semantic versioning
- Custom migration strategies
- Stable paths across code refactoring

**Trade-offs:**
- Must manage versions manually
- Risk of collision if not careful
- Loses automatic versioning

---

## Cross-User Data Access

Program hashing works with cross-user subscriptions:

```typescript
const program: ReactiveComputationGraph = {
  id: 'mutual-recognition',
  
  variables: {
    // My tree
    myTree: {
      type: 'subscription',
      holster_path: 'tree',
      schema_type: 'TreeNode'
    },
    
    // Their tree (cross-user)
    theirTree: {
      type: 'subscription',
      holster_path: 'tree',
      schema_type: 'TreeNode',
      subscribe_to_user: 'their-pubkey'  // ← Cross-user
    }
  },
  
  computations: [ ... ]
};

// Data paths:
// My tree:    ~me/<hash>/tree
// Their tree: ~their-pubkey/<hash>/tree
```

**Key insight**: Both users run the **same program** (same hash), but each has their own data namespace!

---

## Program Registry

Programs are automatically registered when a runtime is created:

```typescript
import { 
  registerProgram, 
  getProgramByHash, 
  listRegisteredPrograms,
  unregisterProgram
} from '$lib/commons';

// Register manually
const hash = registerProgram(myProgram);

// Retrieve by hash
const program = getProgramByHash(hash);

// List all
const allHashes = listRegisteredPrograms();

// Unregister
unregisterProgram(hash);
```

**Use cases:**
- Program discovery
- Hot-reload
- Dynamic program loading
- Debugging

---

## Path Manipulation Utilities

```typescript
import { 
  prefixHolsterPath,
  unprefixHolsterPath,
  extractProgramHash,
  buildProgramDataPath
} from '$lib/commons';

// Prefix a path
prefixHolsterPath('abc123', 'tree');
// → "abc123/tree"

// Unprefix a path
unprefixHolsterPath('abc123/tree');
// → "tree"

// Extract hash
extractProgramHash('abc123/tree/nodes/node1');
// → "abc123"

// Build full path
buildProgramDataPath('user-pubkey', 'abc123', 'tree');
// → ["user-pubkey", "abc123/tree"]
// Usage: holsterUser.get(["user-pubkey", "abc123/tree"])

buildProgramDataPath(null, 'abc123', 'tree');
// → "abc123/tree"
// Usage: holsterUser.get("abc123/tree")
```

---

## Best Practices

### 1. **Let the Runtime Handle Prefixing**
Don't manually prefix paths in your program definitions:

```typescript
// ❌ BAD
holster_path: 'abc123/tree'

// ✅ GOOD
holster_path: 'tree'
```

The runtime adds the prefix automatically!

### 2. **Use Descriptive Base Paths**
Even though they're prefixed, base paths should be descriptive:

```typescript
// ❌ BAD
holster_path: 'd1'
holster_path: 'x'

// ✅ GOOD
holster_path: 'tree'
holster_path: 'allocation_result'
holster_path: 'mutual_recognition'
```

### 3. **Structure Data Hierarchically**
Use path separators for related data:

```typescript
holster_path: 'tree'
holster_path: 'tree/root'
holster_path: 'tree/nodes/node1'
holster_path: 'tree/metadata'

// Results in:
// ~pubkey/<hash>/tree
// ~pubkey/<hash>/tree/root
// ~pubkey/<hash>/tree/nodes/node1
// ~pubkey/<hash>/tree/metadata
```

### 4. **Use Manual Hashing for Stable APIs**
If other programs depend on your data paths, use manual hashing:

```typescript
const stableProgram: ReactiveComputationGraph = {
  id: 'public-api',
  program_hash: 'public-api-v1',  // Stable hash
  // ...
};

// Other programs can reliably find data at:
// ~pubkey/public-api-v1/endpoint
```

### 5. **Document Cross-User Dependencies**
When accessing another user's data, document the expected program:

```typescript
variables: {
  peerData: {
    type: 'subscription',
    holster_path: 'shared_data',
    schema_type: 'SharedData',
    subscribe_to_user: peerPubkey
    // Note: Expects peer to run program with hash: abc123
  }
}
```

---

## Migration Guide

### From Non-Hashed to Hashed

If you have existing data without program hashing:

#### Option 1: Manual Hash for Backward Compatibility
```typescript
const program: ReactiveComputationGraph = {
  id: 'legacy-app',
  program_hash: '',  // Empty string = no prefix
  // ...
};
```

#### Option 2: Migrate Data
```typescript
// 1. Read old data
const oldData = await holsterUser.get('tree');

// 2. Create new program (gets hash)
const runtime = new ComputationGraphRuntime(newProgram);
const hash = runtime.getProgramHash();

// 3. Write to new location
await holsterUser.get(`${hash}/tree`).put(oldData);

// 4. Clean up old location (optional)
await holsterUser.get('tree').put(null);
```

---

## Examples

See complete examples in:
- `examples/program-hash-example.ts`
  - Automatic hashing
  - Multiple instances
  - Program versioning
  - Manual hash override
  - Cross-user access
  - Registry operations

---

## Architecture Diagram

```
┌─────────────────────────────────────────────────────┐
│ ReactiveComputationGraph                            │
│                                                     │
│ id: "counter"                                       │
│ variables:                                          │
│   count: { holster_path: "counter", ... }          │
│ computations: [ ... ]                               │
└──────────────┬──────────────────────────────────────┘
               │
               │ hashProgram()
               ├──────────────────────────┐
               ▼                          │
         ┌──────────┐                    │
         │  Hash    │                    │
         │ abc123   │                    │
         └─────┬────┘                    │
               │                          │
               │ registerProgram()        │
               │                          │
               ▼                          │
    ┌─────────────────┐                  │
    │ Program Registry│                  │
    │  abc123 → prog  │                  │
    └─────────────────┘                  │
                                          │
                                          │
    ┌─────────────────────────────────────┤
    │ ComputationGraphRuntime             │
    │                                     │
    │ programHash: "abc123"               │
    │                                     │
    │ All paths prefixed:                 │
    │   "counter" → "abc123/counter"      │
    │                                     │
    │ Holster operations:                 │
    │   holsterUser.get("abc123/counter") │
    └─────────────────────────────────────┘
                    │
                    │
                    ▼
          ┌──────────────────┐
          │  Holster Storage │
          │                  │
          │ ~pubkey/abc123/  │
          │   ├─ counter     │
          │   ├─ tree        │
          │   └─ nodes/      │
          │       └─ node1   │
          └──────────────────┘
```

---

## API Reference

### Hashing Functions

#### `hashProgram(program: ReactiveComputationGraph): string`
Compute deterministic hash from program structure.

#### `getProgramHash(program: ReactiveComputationGraph): string`
Get program hash (uses `program.program_hash` if set, otherwise computes).

#### `verifyProgramHash(program: ReactiveComputationGraph): boolean`
Verify that `program.program_hash` matches computed hash.

### Path Functions

#### `prefixHolsterPath(programHash: string, holsterPath: string): string`
Prefix a holster path with program hash.

#### `unprefixHolsterPath(prefixedPath: string): string`
Remove program hash prefix from path.

#### `extractProgramHash(prefixedPath: string): string | null`
Extract program hash from prefixed path.

#### `buildProgramDataPath(pubkey: string | null, programHash: string, dataPath: string): string | [string, string]`
Build full Holster path for program data.

### Registry Functions

#### `registerProgram(program: ReactiveComputationGraph): string`
Register program in global registry. Returns hash.

#### `getProgramByHash(hash: string): ReactiveComputationGraph | null`
Retrieve program by hash.

#### `listRegisteredPrograms(): string[]`
List all registered program hashes.

#### `unregisterProgram(hash: string): boolean`
Remove program from registry.

#### `clearProgramRegistry(): void`
Clear all registered programs.

### Runtime Methods

#### `runtime.getProgramHash(): string`
Get the program hash for this runtime instance.

---

## Conclusion

Program hashing provides a robust, scalable data organization model for RDL programs. It enables:

- ✅ **Clean data isolation** between programs
- ✅ **Automatic versioning** for program evolution
- ✅ **Predictable data layout** for debugging
- ✅ **Safe multi-program execution** without collisions
- ✅ **Cross-user data sharing** with clear namespaces

All with **zero boilerplate** - just define your program and let the runtime handle the rest!

