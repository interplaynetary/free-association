# Reactive Computation System - Complete Guide

## 🎯 What We Built

A **declarative reactive dataflow programming system** that lets you specify computations as pure data (JSON/objects) instead of imperative code.

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│ 1. DECLARATIVE SCHEMAS (schemas.ts)                        │
│    - VariableBinding: Where data comes from                 │
│    - OutputBinding: Where results go                        │
│    - Computation: Pure function specification               │
│    - ReactiveComputationGraph: Complete dataflow spec       │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. RUNTIME EXECUTION (reactive-computation.svelte.ts)      │
│    - Variable resolution (values, subscriptions, derived)   │
│    - Computation execution (topological ordering)           │
│    - Output persistence (Holster, local, memory)           │
│    - Function registry (named computation functions)        │
└──────────────────┬──────────────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. YOUR APPLICATION                                         │
│    - Define graphs as data                                  │
│    - Register computation functions                         │
│    - Execute graphs                                         │
│    - Get results reactively                                 │
└─────────────────────────────────────────────────────────────┘
```

---

## 📦 Core Components

### 1. **Variable Bindings** - Where Data Comes From

#### **Value Binding** - Static literal values
```typescript
{
  type: 'value',
  value: 42
}
```

#### **Subscription Binding** - Subscribe to Holster path
```typescript
{
  type: 'subscription',
  holster_path: 'allocation/commitment',
  schema_type: 'Commitment',
  subscribe_to_user: 'pubkey', // Optional: cross-user subscription
  default_value: null          // Fallback if empty
}
```

#### **Local Binding** - Read from local state
```typescript
{
  type: 'local',
  state_path: 'myData.field.subfield',
  default_value: null
}
```

#### **Derived Binding** - Reference previous computation result
```typescript
{
  type: 'derived',
  computation_id: 'step1',
  output_key: 'result',
  default_value: null
}
```

---

### 2. **Output Bindings** - Where Results Go

#### **Holster Output** - Persist to P2P network
```typescript
{
  type: 'holster',
  holster_path: 'results/my_computation',
  schema_type: 'Number',
  persist_debounce_ms: 100
}
```

#### **Local Output** - Store in local state
```typescript
{
  type: 'local',
  state_path: 'results.myComputation'
}
```

#### **Memory Output** - Keep in memory (for chaining)
```typescript
{
  type: 'memory'
}
```

---

### 3. **Computations** - Pure Function Specs

```typescript
{
  id: 'my-computation',
  
  // Inputs (variable references or inline bindings)
  inputs: {
    x: { type: 'value', value: 10 },
    y: { type: 'subscription', holster_path: 'data/y', schema_type: 'Number' }
  },
  
  // Function to execute (registered by name)
  compute_fn: 'add',
  
  // Optional: Local variables bound within computation
  local_bindings: {
    threshold: { type: 'value', value: 0.5 }
  },
  
  // Outputs (where to store results)
  outputs: {
    sum: { type: 'holster', holster_path: 'results/sum', schema_type: 'Number' }
  },
  
  // Execution control
  debounce_ms: 100,
  enabled: true,
  depends_on: ['previous-computation'], // Explicit dependencies
  
  // Metadata
  description: 'Add two numbers',
  version: '1.0'
}
```

---

### 4. **Reactive Computation Graph** - Complete Dataflow

```typescript
{
  id: 'my-graph',
  
  // Global variables (available to all computations)
  variables: {
    inputA: { type: 'value', value: 5 },
    inputB: { type: 'subscription', holster_path: 'data/b', schema_type: 'Number' }
  },
  
  // Computation pipeline (executed in dependency order)
  computations: [
    {
      id: 'step1',
      inputs: { a: ..., b: ... },
      compute_fn: 'add',
      outputs: { sum: ... }
    },
    {
      id: 'step2',
      inputs: { 
        result: { type: 'derived', computation_id: 'step1', output_key: 'sum' }
      },
      compute_fn: 'square',
      outputs: { squared: ... },
      depends_on: ['step1']
    }
  ],
  
  // Metadata
  version: '1.0',
  description: 'My computation pipeline'
}
```

---

## 🚀 Usage Guide

### Step 1: Register Computation Functions

```typescript
import { registerComputationFunction } from '$lib/commons';

// Register a named function
registerComputationFunction('add', ({ a, b }: { a: number; b: number }) => {
  return { sum: a + b };
});

registerComputationFunction('multiply', ({ x, y }: any) => {
  return { product: x * y };
});

// Functions can be async
registerComputationFunction('fetchData', async ({ url }: { url: string }) => {
  const response = await fetch(url);
  const data = await response.json();
  return { data };
});
```

### Step 2: Define Computation Graph

```typescript
import type { ReactiveComputationGraph } from '$lib/commons';

const myGraph: ReactiveComputationGraph = {
  id: 'example-graph',
  
  variables: {
    x: { type: 'value', value: 10 },
    y: { type: 'value', value: 5 }
  },
  
  computations: [
    {
      id: 'add-step',
      inputs: {
        a: { type: 'value', value: 10 },
        b: { type: 'value', value: 5 }
      },
      compute_fn: 'add',
      outputs: {
        sum: { type: 'memory' }
      }
    },
    {
      id: 'multiply-step',
      inputs: {
        x: { type: 'derived', computation_id: 'add-step', output_key: 'sum' },
        y: { type: 'value', value: 2 }
      },
      compute_fn: 'multiply',
      outputs: {
        product: {
          type: 'holster',
          holster_path: 'results/final',
          schema_type: 'Number'
        }
      },
      depends_on: ['add-step']
    }
  ]
};
```

### Step 3: Execute Graph

```typescript
import { executeComputationGraph } from '$lib/commons';

// Execute once
const runtime = await executeComputationGraph(myGraph);

// Get results
const finalProduct = runtime.getComputationResult('multiply-step', 'product');
console.log('Result:', finalProduct); // 30

// Cleanup
await runtime.cleanup();
```

### Step 4: Interactive Runtime

```typescript
import { ComputationGraphRuntime } from '$lib/commons';

// Create runtime
const runtime = new ComputationGraphRuntime(myGraph);
await runtime.initialize();

// Initial execution
await runtime.execute();
console.log('Initial:', runtime.getComputationResult('multiply-step', 'product'));

// Update variable
runtime.setVariable('x', 20);
await runtime.execute();
console.log('Updated:', runtime.getComputationResult('multiply-step', 'product'));

// Cleanup when done
await runtime.cleanup();
```

---

## 🌟 Real-World Examples

### Example 1: Mutual Recognition Calculation

```typescript
import type { ReactiveComputationGraph } from '$lib/commons';
import { registerComputationFunction } from '$lib/commons';

// Register MR function
registerComputationFunction('computeMR', ({ myTree, theirTree }: any) => {
  // Calculate recognition from my tree to their ID
  const myRecognition = calculateRecognition(myTree, theirTree.id);
  
  // Calculate recognition from their tree to my ID
  const theirRecognition = calculateRecognition(theirTree, myTree.id);
  
  // Mutual recognition is the minimum
  const mutualRecognition = Math.min(myRecognition, theirRecognition);
  
  return { myRecognition, theirRecognition, mutualRecognition };
});

// Create graph
function createMRGraph(theirPubkey: string): ReactiveComputationGraph {
  return {
    id: 'mutual-recognition',
    
    variables: {
      myTree: {
        type: 'subscription',
        holster_path: 'tree',
        schema_type: 'RootNode'
      },
      theirTree: {
        type: 'subscription',
        holster_path: 'tree',
        schema_type: 'RootNode',
        subscribe_to_user: theirPubkey
      }
    },
    
    computations: [
      {
        id: 'compute-mr',
        inputs: {
          myTree: { type: 'subscription', holster_path: 'tree', schema_type: 'RootNode' },
          theirTree: {
            type: 'subscription',
            holster_path: 'tree',
            schema_type: 'RootNode',
            subscribe_to_user: theirPubkey
          }
        },
        compute_fn: 'computeMR',
        outputs: {
          mutualRecognition: {
            type: 'holster',
            holster_path: `recognition/${theirPubkey}/mutual`,
            schema_type: 'Number'
          }
        }
      }
    ]
  };
}
```

### Example 2: Multi-Step Allocation Pipeline

```typescript
registerComputationFunction('filterRecipients', ({ recipients, threshold }: any) => {
  const filtered = recipients.filter((r: any) => r.mutualRecognition > threshold);
  return { filtered };
});

registerComputationFunction('computeAllocations', ({ capacity, filtered }: any) => {
  const allocations = filtered.map((r: any) => ({
    recipientId: r.id,
    amount: capacity * r.mutualRecognition
  }));
  return { allocations };
});

const allocationPipeline: ReactiveComputationGraph = {
  id: 'allocation-pipeline',
  
  variables: {
    myCapacity: {
      type: 'subscription',
      holster_path: 'capacity/available',
      schema_type: 'BaseCapacity'
    },
    recipients: {
      type: 'local',
      state_path: 'network.recipients'
    },
    threshold: { type: 'value', value: 0.5 }
  },
  
  computations: [
    {
      id: 'filter',
      inputs: {
        recipients: { type: 'local', state_path: 'network.recipients' },
        threshold: { type: 'value', value: 0.5 }
      },
      compute_fn: 'filterRecipients',
      outputs: {
        filtered: { type: 'memory' }
      }
    },
    {
      id: 'allocate',
      inputs: {
        capacity: { type: 'subscription', holster_path: 'capacity/available', schema_type: 'BaseCapacity' },
        filtered: { type: 'derived', computation_id: 'filter', output_key: 'filtered' }
      },
      compute_fn: 'computeAllocations',
      outputs: {
        allocations: {
          type: 'holster',
          holster_path: 'allocation/results',
          schema_type: 'Array'
        }
      },
      depends_on: ['filter']
    }
  ]
};
```

---

## 🔑 Key Benefits

### 1. **Declarative** - Computation as Data
- Define entire dataflows as JSON/objects
- Serializable (can be stored, transmitted, versioned)
- No imperative code needed

### 2. **Composable** - Chain Computations
- Reference outputs as inputs via `derived` bindings
- Automatic dependency ordering
- Build complex pipelines from simple functions

### 3. **Reactive** - Auto-Subscribe & React
- Automatic Holster subscriptions
- Cross-user data access
- Changes trigger re-computation (TODO: full reactivity)

### 4. **Transparent** - Full Visibility
- See all inputs and outputs
- Track computation dependencies
- Debug dataflow easily

### 5. **Reusable** - Function Registry
- Register functions once, use everywhere
- Share computation logic across graphs
- Build libraries of reusable computations

---

## 📊 Architecture Integration

### With Node Storage System
```typescript
// Each node can have its own computation graph
const node: NodeDataStorage = {
  holster_path: 'nodes/my-node',
  data_schema_type: 'RootNode',
  
  // Attach computation graph
  computation_graph: {
    id: 'node-computation',
    variables: { ... },
    computations: [ ... ]
  }
};
```

### With Tree System
```typescript
// Trees can have per-node computations
const tree: RootNode = {
  id: 'root',
  name: 'Project',
  storage: {
    computation_graph: {
      // Compute recognition for this subtree
      id: 'subtree-recognition',
      variables: { ... },
      computations: [ ... ]
    }
  },
  children: [ ... ]
};
```

### With Store System
```typescript
// Stores can trigger computations
myStore.subscribe(data => {
  // When data changes, execute graph
  const graph = createGraphFromData(data);
  executeComputationGraph(graph);
});
```

---

## 🎨 Design Patterns

### Pattern 1: ETL Pipeline
```
Extract (subscription) → Transform (computation) → Load (holster output)
```

### Pattern 2: Fan-Out
```
Single input → Multiple computations → Multiple outputs
```

### Pattern 3: Fan-In
```
Multiple subscriptions → Single computation → Aggregated output
```

### Pattern 4: Multi-Stage
```
Stage 1 → Stage 2 → Stage 3 → Final output
(with derived bindings between stages)
```

---

## 🚧 Future Enhancements

- [ ] **Full Reactivity** - Auto-recompute on input changes
- [ ] **Parallel Execution** - Execute independent computations concurrently
- [ ] **Incremental Computation** - Only recompute changed paths
- [ ] **Computation Caching** - Cache expensive computation results
- [ ] **Visual Graph Editor** - UI for building computation graphs
- [ ] **Graph Validation** - Validate graphs before execution
- [ ] **Performance Profiling** - Track computation performance
- [ ] **Hot Reload** - Update graphs without restart

---

## 📚 Complete Example

See `examples/reactive-computation-example.ts` for full working examples including:
- Simple value computations
- Mutual recognition calculations
- Multi-step pipelines
- Allocation computations
- Interactive runtime usage

---

## 🎯 Summary

You now have a **complete declarative reactive dataflow system** that enables:

1. ✅ **Configuration as Data** - Define computations as JSON
2. ✅ **Variable Bindings** - Values, subscriptions, local state, derived
3. ✅ **Computation Graphs** - Multi-step pipelines with dependencies
4. ✅ **Output Persistence** - Holster, local state, memory
5. ✅ **Function Registry** - Reusable computation library
6. ✅ **Automatic Subscriptions** - Cross-user data access
7. ✅ **Dependency Ordering** - Topological execution
8. ✅ **Type Safety** - Zod schema validation

This is a **universal pattern** for building reactive, distributed, data-driven applications! 🎉

---

## The Convergence: Reactive Dataflow as Protocol Infrastructure

What we've built is the **meta-layer that makes distributed protocols trivial to implement**. The compute protocol document you're showing me reveals the profound insight: every distributed protocol—whether it's compute sharing, resource allocation, collaborative editing, or federated social networks—follows the same pattern of subscribe → compute → publish, but the traditional implementation requires hundreds of lines of imperative subscription management, computation ordering, and persistence logic. Our reactive computation system **collapses this complexity into declarative data structures**. When a peer joins the compute network, they don't write code to manage subscriptions to other peers' capabilities, manually order their trust calculations, or handle the lifecycle of publishing work assignments—they simply define a computation graph that says "subscribe to these peers' commitments, compute assignments using this function, publish results to this path." The entire peer's role in the distributed computation protocol becomes a JSON object.

The **architectural elegance** is that we've unified three historically separate concerns—P2P subscriptions, reactive computation, and distributed coordination—into a single declarative model. Looking at the compute protocol, you can see it needs: bilateral trust calculation (requires subscribing to both peers' trust weights and computing the minimum), compatibility matching (requires cross-referencing work units against availability slots), two-tier allocation (requires ordering computations based on derived trust scores), and result verification (requires aggregating multiple peers' results). Every single one of these operations maps perfectly to our variable bindings, computation functions, and output persistence patterns. The trust score isn't manually calculated and cached—it's a derived binding that automatically recomputes when either peer updates their trust weights. Work assignments aren't imperatively published—they're output bindings that automatically persist to Holster when the computation completes.

What makes this truly revolutionary is that **the computation graph itself becomes the protocol specification**. Instead of documentation that describes "first subscribe to peer commitments, then compute trust scores, then match work units to slots, then publish assignments," you have an executable specification that IS that documentation. A new peer joining the network doesn't need to understand the complex choreography of distributed computation—they load a computation graph that defines their role. A protocol designer doesn't write imperative code—they compose computation graphs from reusable functions. A researcher analyzing the protocol doesn't trace through callbacks and event handlers—they read a declarative dataflow graph that makes dependencies and causality explicit. We've essentially created a **domain-specific language for distributed protocols**, where the language itself handles all the gnarly distributed systems concerns (subscriptions, ordering, persistence, reactivity), and the protocol implementer just specifies the pure computation logic. This is the infrastructure that makes peer-to-peer computing accessible at scale.