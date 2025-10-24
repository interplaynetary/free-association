# Reactive Dataflow Language (RDL) - Formal Specification v1.0

## Abstract

Reactive Dataflow Language (RDL) is a declarative, JSON-based domain-specific language for specifying distributed reactive computations over peer-to-peer data. RDL programs are pure data structures that define dataflow graphs comprising variable bindings, computation steps, and output destinations. The runtime automatically handles subscription management, dependency resolution, execution ordering, and result persistence.

---

## 1. Language Overview

### 1.1 Design Philosophy

RDL embodies the following principles:

1. **Declarative** - Specify *what* to compute, not *how*
2. **Data-first** - Programs are JSON objects, not imperative code
3. **Composable** - Computations compose through variable bindings
4. **Distributed** - First-class support for peer-to-peer subscriptions
5. **Reactive** - Automatic re-execution on input changes
6. **Transparent** - All dataflow is explicit and inspectable

### 1.2 Core Concepts

- **Variable** - Named data source with a binding specification
- **Binding** - Specification of where data originates
- **Computation** - Pure function application over bound variables
- **Output** - Specification of where results are stored
- **Graph** - Complete dataflow specification with ordered execution

---

## 2. Formal Syntax

### 2.1 EBNF Grammar

```ebnf
(* Top-level program *)
Program ::= ReactiveComputationGraph

(* Reactive computation graph *)
ReactiveComputationGraph ::= {
  "id": Identifier,
  "variables": VariableDeclarations,
  "computations": Computation*,
  "version"?: String,
  "description"?: String
}

(* Variable declarations *)
VariableDeclarations ::= { Identifier: VariableBinding, ... }

(* Variable binding (discriminated union) *)
VariableBinding ::= ValueBinding 
                  | SubscriptionBinding
                  | FetchBinding 
                  | LocalBinding 
                  | DerivedBinding

ValueBinding ::= {
  "type": "value",
  "value": JSON
}

SubscriptionBinding ::= {
  "type": "subscription",
  "holster_path": Path,
  "schema_type": SchemaTypeName,
  "subscribe_to_user"?: PubKey,
  "default_value"?: JSON
}

FetchBinding ::= {
  "type": "fetch",
  "holster_path": Path,
  "schema_type": SchemaTypeName,
  "fetch_from_user"?: PubKey,
  "default_value"?: JSON,
  "wait_ms"?: NonNegativeInteger
}

LocalBinding ::= {
  "type": "local",
  "state_path": Path,
  "default_value"?: JSON
}

DerivedBinding ::= {
  "type": "derived",
  "computation_id": Identifier,
  "output_key": Identifier,
  "default_value"?: JSON
}

(* Computation specification *)
Computation ::= {
  "id": Identifier,
  "inputs": InputBindings,
  "compute_fn": FunctionName,
  "local_bindings"?: LocalBindings,
  "outputs": OutputBindings,
  "debounce_ms"?: NonNegativeInteger,
  "enabled"?: Boolean,
  "depends_on"?: Identifier*,
  "description"?: String,
  "version"?: String
}

InputBindings ::= { Identifier: VariableBinding, ... }
LocalBindings ::= { Identifier: VariableBinding, ... }
OutputBindings ::= { Identifier: OutputBinding, ... }

(* Output binding (discriminated union) *)
OutputBinding ::= HolsterOutput 
                | LocalOutput 
                | MemoryOutput

HolsterOutput ::= {
  "type": "holster",
  "holster_path": Path,
  "schema_type"?: SchemaTypeName,
  "persist_debounce_ms"?: NonNegativeInteger
}

LocalOutput ::= {
  "type": "local",
  "state_path": Path
}

MemoryOutput ::= {
  "type": "memory"
}

(* Primitive types *)
Identifier ::= String  (* matching [a-zA-Z_][a-zA-Z0-9_-]* *)
Path ::= String  (* matching [a-zA-Z0-9_/.-]+ *)
SchemaTypeName ::= String
FunctionName ::= String
PubKey ::= String  (* 64 hex characters *)
NonNegativeInteger ::= Integer  (* >= 0 *)
JSON ::= (* Standard JSON value *)
```

### 2.2 Type System

```
Type ::= Primitive | Composite | Schema

Primitive ::= 
  | Boolean
  | Number
  | String
  | Null

Composite ::=
  | Array<Type>
  | Record<String, Type>

Schema ::= RegisteredSchemaType
  (* Schema types validated at runtime via Zod *)
```

---

## 3. Semantic Model

### 3.1 Variable Binding Semantics

#### Value Binding
```
Γ ⊢ value(v) ⇒ v
```
Evaluates to the literal value `v`.

#### Subscription Binding
```
Γ ⊢ subscription(path, schema, user?) ⇒ 
  subscribe(holster, path, schema, user) → Store<T>
```
Creates a reactive store subscribed to `path` in Holster using `.on()`, optionally from another user's namespace. Updates propagate automatically on data changes.

#### Fetch Binding
```
Γ ⊢ fetch(path, schema, user?, wait?) ⇒ 
  get(holster, path, schema, user, wait) → Promise<T>
```
One-time fetch from Holster using `.get()`. Does not create persistent subscription. Useful for:
- Static/infrequently changing data
- One-off lookups during computation
- Reducing subscription overhead
- Getting historical snapshots

#### Local Binding
```
Γ, state ⊢ local(path) ⇒ 
  lookup(state, path) | default
```
Reads value from local state at `path`. Returns default if path doesn't exist.

#### Derived Binding
```
Γ, results ⊢ derived(comp_id, out_key) ⇒ 
  lookup(results[comp_id], out_key) | default
```
References output `out_key` from computation `comp_id`. Creates implicit dependency.

### 3.2 Computation Semantics

```
Γ ⊢ computation(id, inputs, fn, outputs) ⇒
  let bindings = resolve_all(Γ, inputs) in
  let result = apply(fn, bindings) in
  persist_all(outputs, result)
```

#### Resolution Rules
1. **Input Resolution**: Each input binding is resolved to its current value
2. **Function Application**: Named function is applied to resolved inputs
3. **Output Persistence**: Each output is persisted according to its binding type

#### Execution Invariants
- Functions are pure (no side effects except through output bindings)
- Computations are deterministic (same inputs → same outputs)
- Execution order respects dependencies (topological sort)
- Re-execution is idempotent (safe to re-run)

### 3.3 Graph Execution Semantics

```
execute(Graph) =
  let V = resolve_all_variables(Graph.variables) in
  let C = topological_sort(Graph.computations) in
  let R = fold(execute_computation, C, {}) in
  R

topological_sort(computations) =
  (* Kahn's algorithm with implicit dependency detection *)
  sort_by_dependencies(computations, detect_dependencies)

detect_dependencies(comp) =
  (* Explicit dependencies *)
  comp.depends_on ∪
  (* Implicit from derived bindings *)
  { binding.computation_id | binding ∈ comp.inputs, binding.type = "derived" }
```

---

## 4. Execution Model

### 4.1 Lifecycle

```
┌─────────────────────────────────────────────────────────────┐
│ 1. PARSE                                                    │
│    - Validate JSON syntax                                   │
│    - Validate against RDL schema                           │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. RESOLVE                                                  │
│    - Create stores for subscriptions                       │
│    - Initialize variable bindings                          │
│    - Register cleanup handlers                             │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. ORDER                                                    │
│    - Build dependency graph                                │
│    - Topological sort                                      │
│    - Detect cycles (error if found)                       │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. EXECUTE                                                  │
│    - For each computation in order:                        │
│      a. Resolve inputs                                     │
│      b. Apply function                                     │
│      c. Persist outputs                                    │
│      d. Store results for derived bindings                 │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. REACT (Optional)                                         │
│    - Watch for input changes                               │
│    - Re-execute affected computations                      │
│    - Propagate updates                                     │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ 6. CLEANUP                                                  │
│    - Unsubscribe from all sources                          │
│    - Clear state                                           │
│    - Release resources                                     │
└─────────────────────────────────────────────────────────────┘
```

### 4.2 Dependency Resolution

**Algorithm**: Modified Kahn's Topological Sort

```
Input: Set of computations C
Output: Ordered list L or error if cycle detected

L ← []
S ← { c ∈ C | dependencies(c) = ∅ }
visited ← ∅
visiting ← ∅

function visit(c):
  if c ∈ visited: return
  if c ∈ visiting: error("Cycle detected at " + c.id)
  
  visiting ← visiting ∪ {c}
  
  for d ∈ dependencies(c):
    visit(d)
  
  visiting ← visiting \ {c}
  visited ← visited ∪ {c}
  L ← L ++ [c]

for c ∈ S:
  visit(c)

return L
```

### 4.3 Reactivity Model

```
Change Detection:
  subscription(path) → on_change → mark_dirty(dependents)
  
Propagation:
  dirty_set ← { c | input_changed(c) }
  affected ← transitive_closure(dirty_set, downstream_deps)
  re_execute(topological_sort(affected))

Optimization:
  - Batch updates (debouncing)
  - Memoization (cache pure functions)
  - Incremental computation (only recompute changed paths)
```

---

## 5. Standard Library

### 5.1 Function Registry

Functions must be registered before use:

```typescript
registerComputationFunction(name: String, fn: Function)
```

#### Function Signature Contract

```
Function :: { [key: String]: any } → { [key: String]: any }
```

- **Input**: Record of named values (matches computation inputs)
- **Output**: Record of named values (matches computation outputs)
- **Purity**: Functions should be pure (deterministic, no external side effects)
- **Async**: Functions may be async (return Promise)

### 5.2 Built-in Schemas

Pre-registered schema types:

```
Primitive Schemas:
  - String, Number, Boolean, Array, Object, Any

Protocol Schemas:
  - Commitment
  - TwoTierAllocationState
  - RoundState
  - AvailabilitySlot
  - NeedSlot
  - BaseCapacity
  - BaseNeed
  - Contact
  - ChatReadState

Tree Schemas:
  - RootNode
  - NonRootNode
  - NodeDataStorage

(Extensible via registerSchema)
```

---

## 6. Language Extensions

### 6.1 Custom Schema Registration

```json
{
  "type": "extension",
  "schemas": [
    {
      "name": "CustomType",
      "validator": "CustomTypeSchema"
    }
  ]
}
```

Runtime call:
```typescript
registerSchema('CustomType', CustomTypeSchema)
```

### 6.2 Custom Function Registration

```json
{
  "type": "extension",
  "functions": [
    {
      "name": "customComputation",
      "description": "Custom computation logic"
    }
  ]
}
```

Runtime call:
```typescript
registerComputationFunction('customComputation', (inputs) => {
  return { output: compute(inputs) };
})
```

### 6.3 Macro System (Future)

```json
{
  "type": "macro",
  "name": "bilateral_computation",
  "template": {
    "variables": {
      "my_data": "{{my_path}}",
      "their_data": {
        "type": "subscription",
        "holster_path": "{{their_path}}",
        "subscribe_to_user": "{{pubkey}}"
      }
    },
    "computations": [ "{{computation_spec}}" ]
  }
}
```

---

## 7. Error Handling

### 7.1 Compile-Time Errors

```
E1: Invalid JSON syntax
E2: Schema validation failure
E3: Unknown schema type reference
E4: Unknown function reference
E5: Circular dependency detected
E6: Invalid path syntax
E7: Type mismatch in binding
```

### 7.2 Runtime Errors

```
R1: Subscription failed (network error)
R2: Function execution error
R3: Output persistence error
R4: Schema validation error on network data
R5: Timeout (computation took too long)
```

### 7.3 Error Recovery

```
Strategy: Graceful Degradation

1. Subscription failures → Use default_value
2. Function errors → Skip computation, log error
3. Persistence errors → Retry with backoff
4. Validation errors → Reject invalid data, continue
```

---

## 8. Examples

### 8.1 Minimal Program

```json
{
  "id": "hello-world",
  "variables": {
    "message": { "type": "value", "value": "Hello, World!" }
  },
  "computations": [
    {
      "id": "print",
      "inputs": {
        "msg": { "type": "value", "value": "Hello, World!" }
      },
      "compute_fn": "identity",
      "outputs": {
        "result": { "type": "memory" }
      }
    }
  ]
}
```

### 8.2 Fetch vs Subscription

```json
{
  "id": "fetch-vs-subscription-example",
  "variables": {
    "staticConfig": {
      "type": "fetch",
      "holster_path": "config/settings",
      "schema_type": "Object",
      "wait_ms": 500
    },
    "liveData": {
      "type": "subscription",
      "holster_path": "data/realtime",
      "schema_type": "Number"
    }
  },
  "computations": [
    {
      "id": "process",
      "inputs": {
        "config": {
          "type": "fetch",
          "holster_path": "config/settings",
          "schema_type": "Object",
          "default_value": { "threshold": 10 }
        },
        "data": {
          "type": "subscription",
          "holster_path": "data/realtime",
          "schema_type": "Number"
        }
      },
      "compute_fn": "processWithConfig",
      "outputs": {
        "result": { "type": "memory" }
      },
      "description": "Fetch config once, but reactively process incoming data"
    }
  ]
}
```

### 8.3 Peer-to-Peer Computation

```json
{
  "id": "mutual-recognition",
  "variables": {
    "my_tree": {
      "type": "subscription",
      "holster_path": "tree",
      "schema_type": "RootNode"
    },
    "their_tree": {
      "type": "subscription",
      "holster_path": "tree",
      "schema_type": "RootNode",
      "subscribe_to_user": "abc123...xyz"
    }
  },
  "computations": [
    {
      "id": "compute-mr",
      "inputs": {
        "myTree": {
          "type": "subscription",
          "holster_path": "tree",
          "schema_type": "RootNode"
        },
        "theirTree": {
          "type": "subscription",
          "holster_path": "tree",
          "schema_type": "RootNode",
          "subscribe_to_user": "abc123...xyz"
        }
      },
      "compute_fn": "mutualRecognition",
      "outputs": {
        "mr_value": {
          "type": "holster",
          "holster_path": "recognition/abc123.../mutual",
          "schema_type": "Number"
        }
      }
    }
  ]
}
```

### 8.4 Multi-Step Pipeline

```json
{
  "id": "data-pipeline",
  "variables": {
    "raw_data": { "type": "value", "value": [1, 5, 10, 15, 20] }
  },
  "computations": [
    {
      "id": "filter",
      "inputs": {
        "data": { "type": "value", "value": [1, 5, 10, 15, 20] },
        "threshold": { "type": "value", "value": 10 }
      },
      "compute_fn": "filterAbove",
      "outputs": {
        "filtered": { "type": "memory" }
      }
    },
    {
      "id": "sum",
      "inputs": {
        "values": {
          "type": "derived",
          "computation_id": "filter",
          "output_key": "filtered"
        }
      },
      "compute_fn": "sum",
      "outputs": {
        "total": {
          "type": "local",
          "state_path": "results.total"
        }
      },
      "depends_on": ["filter"]
    }
  ]
}
```

---

## 9. Implementation Requirements

### 9.1 Conforming Implementations

A conforming RDL implementation MUST:

1. Parse valid RDL JSON programs
2. Validate against RDL schema
3. Support all four variable binding types
4. Support all three output binding types
5. Execute computations in topological order
6. Handle subscription lifecycle correctly
7. Provide function and schema registries
8. Detect and report circular dependencies
9. Support cross-user Holster subscriptions
10. Handle errors according to specification

### 9.2 Optional Features

A conforming implementation MAY:

1. Provide automatic reactivity (re-execution on change)
2. Optimize with memoization/caching
3. Support parallel computation execution
4. Provide visual graph debugging tools
5. Support graph composition/inheritance
6. Implement macro system
7. Provide static analysis tools
8. Support graph serialization formats beyond JSON

---

## 10. Protocol Integration Patterns

### 10.1 Distributed Protocol Template

```json
{
  "id": "protocol-{{name}}",
  "variables": {
    "my_state": {
      "type": "subscription",
      "holster_path": "{{protocol}}/state",
      "schema_type": "{{StateType}}"
    },
    "peer_states": {
      "type": "local",
      "state_path": "network.peers",
      "default_value": []
    }
  },
  "computations": [
    {
      "id": "aggregate-network",
      "inputs": {
        "my_state": { "...": "..." },
        "peer_states": { "...": "..." }
      },
      "compute_fn": "{{aggregation_fn}}",
      "outputs": {
        "aggregated": { "type": "memory" }
      }
    },
    {
      "id": "compute-local",
      "inputs": {
        "aggregated": {
          "type": "derived",
          "computation_id": "aggregate-network",
          "output_key": "aggregated"
        }
      },
      "compute_fn": "{{local_computation_fn}}",
      "outputs": {
        "result": {
          "type": "holster",
          "holster_path": "{{protocol}}/result",
          "schema_type": "{{ResultType}}"
        }
      },
      "depends_on": ["aggregate-network"]
    }
  ]
}
```

### 10.2 Common Patterns

**Pattern 1: Bilateral Computation**
```
Subscribe to peer → Compute relationship metric → Publish result
```

**Pattern 2: Network Aggregation**
```
Subscribe to all peers → Aggregate → Compute local → Publish
```

**Pattern 3: Cascading Updates**
```
Subscribe to dependency → Compute → Trigger downstream → Publish
```

**Pattern 4: Consensus Protocol**
```
Propose → Subscribe to votes → Aggregate → Decide → Publish
```

---

## 11. Formal Properties

### 11.1 Determinism

**Theorem**: For a computation graph G with pure functions, repeated execution with identical inputs produces identical outputs.

**Proof sketch**: By induction on computation depth. Base case: value bindings are constant. Inductive step: If all dependencies are deterministic, and function is pure, output is deterministic.

### 11.2 Convergence

**Theorem**: For a DAG computation graph with bounded inputs, execution terminates in finite time.

**Proof sketch**: Topological sort ensures finite ordering. Each computation executes once. No cycles exist by definition. Therefore, execution completes in O(V + E) time where V is computations and E is dependencies.

### 11.3 Composability

**Theorem**: Computation graphs compose. If G1 outputs to Holster paths that G2 subscribes to, then G2 observes G1's outputs.

**Proof sketch**: Holster provides eventual consistency. G1's outputs create subscription events. G2's subscription bindings react to these events. Therefore, composition is sound.

---

## 12. Security Considerations

### 12.1 Sandboxing

Computation functions should be sandboxed to prevent:
- Arbitrary code execution
- Access to unauthorized resources
- Denial of service attacks

### 12.2 Resource Limits

Implementations should enforce:
- Maximum graph size (nodes, edges)
- Maximum computation depth
- Execution timeouts
- Memory limits

### 12.3 Trust Model

- Subscriptions are read-only (cannot modify peer data)
- Schema validation prevents malformed data injection
- Function registry is controlled by runtime environment
- Cross-user subscriptions require authentication

---

## 13. Performance Characteristics

### 13.1 Complexity Analysis

```
Graph Parsing: O(V + E)
  where V = number of computations, E = number of dependencies

Topological Sort: O(V + E)

Variable Resolution: O(V)

Execution: O(V × F)
  where F = average function execution time

Total: O(V + E + V×F) = O(V×F) for reasonable graphs
```

### 13.2 Optimization Strategies

1. **Memoization** - Cache function results keyed by inputs
2. **Incremental Execution** - Only re-execute changed paths
3. **Parallel Execution** - Execute independent computations concurrently
4. **Lazy Evaluation** - Only execute when outputs are needed
5. **Subscription Batching** - Group subscription updates

---

## 14. Interoperability

### 14.1 JSON-LD Extension (Future)

```json
{
  "@context": "https://rdl.spec.org/v1",
  "@type": "ReactiveComputationGraph",
  "id": "example",
  "variables": { "...": "..." }
}
```

### 14.2 Import/Export

```json
{
  "imports": [
    { "url": "https://registry.rdl.org/graphs/mutual-recognition.json" }
  ],
  "extends": "mutual-recognition",
  "computations": [ "..." ]
}
```

---

## 15. Version History

- **v1.0** (2025-01-XX) - Initial specification
  - Core variable binding types
  - Computation execution model
  - Output persistence
  - Function registry
  - Schema system

---

## 16. References

1. Functional Reactive Programming (Elliott & Hudak, 1997)
2. Dataflow Programming Models (Johnston et al., 2004)
3. CRDTs for Eventual Consistency (Shapiro et al., 2011)
4. JSON Schema Specification (Wright et al., 2020)

---

## Appendix A: Complete Type Definitions

```typescript
// See schemas.ts for complete Zod definitions
type ReactiveComputationGraph = z.infer<typeof ReactiveComputationGraphSchema>
type VariableBinding = z.infer<typeof VariableBindingSchema>
type OutputBinding = z.infer<typeof OutputBindingSchema>
type Computation = z.infer<typeof ComputationSchema>
```

---

## Appendix B: Conformance Checklist

- [ ] JSON parsing
- [ ] Schema validation
- [ ] Variable resolution (all 4 types)
- [ ] Output persistence (all 3 types)
- [ ] Topological sorting
- [ ] Cycle detection
- [ ] Function registry
- [ ] Schema registry
- [ ] Cross-user subscriptions
- [ ] Error handling
- [ ] Cleanup/lifecycle

---

**End of Specification**

*Reactive Dataflow Language v1.0*  
*A domain-specific language for distributed reactive computation*

