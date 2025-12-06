# Elegant Lambda Calculus API Reference

Complete reference for the elegant, fully-curried lambda calculus API.

## Table of Contents

1. [Combinators](#combinators)
2. [Recognition](#recognition)
3. [Filters](#filters)
4. [Limits](#limits)
5. [Collective](#collective)
6. [Commons](#commons)
7. [Allocation](#allocation)
8. [System](#system)

---

## Combinators

### Basic Combinators

```typescript
// S combinator: S x y z = x z (y z)
S<A, B, C>(x: (a: A) => (b: B) => C): (y: (a: A) => B) => (z: A) => C

// K combinator: K x y = x
K<A, B>(x: A): (y: B) => A

// I combinator: I x = x
I<A>(x: A): A

// B combinator: B f g x = f (g x)
B<A, B, C>(f: (b: B) => C): (g: (a: A) => B) => (x: A) => C

// C combinator: C f x y = f y x
C<A, B, C>(f: (a: A) => (b: B) => C): (x: B) => (y: A) => C
```

### Function Composition

```typescript
// Pipe: left-to-right composition
pipe<A, B, C>(x: A, ...fns: Array<(x: any) => any>): any

// Compose: right-to-left composition
compose2<A, B, C>(f: (b: B) => C, g: (a: A) => B): (a: A) => C
```

### Currying

```typescript
curry2<A, B, C>(f: (a: A, b: B) => C): (a: A) => (b: B) => C
curry3<A, B, C, D>(f: (a: A, b: B, c: C) => D): (a: A) => (b: B) => (c: C) => D
curry4<A, B, C, D, E>(f: (a: A, b: B, c: C, d: D) => E): (a: A) => (b: B) => (c: C) => (d: D) => E
curryN(f: Function): Function
```

### Monads

```typescript
// Maybe Monad
type Maybe<T> = { type: 'just'; value: T } | { type: 'nothing' }

just<T>(value: T): Maybe<T>
nothing<T>(): Maybe<T>
bindMaybe<A, B>(f: (a: A) => Maybe<B>, ma: Maybe<A>): Maybe<B>
fmapMaybe<A, B>(f: (a: A) => B, ma: Maybe<A>): Maybe<B>

// Reader Monad
type Reader<R, A> = (ctx: R) => A

returnReader<R, A>(value: A): Reader<R, A>
bindReader<R, A, B>(f: (a: A) => Reader<R, B>, ra: Reader<R, A>): Reader<R, B>
runReader<R, A>(ctx: R): (reader: Reader<R, A>) => A

// State Monad
type State<S, A> = (state: S) => [A, S]

runState<S, A>(initialState: S): (state: State<S, A>) => [A, S]
```

### Church Encodings

```typescript
// Church Booleans
type ChurchBoolean = <T>(t: T, f: T) => T

churchTrue<T>(t: T, f: T): T
churchFalse<T>(t: T, f: T): T
churchIf<T>(cond: ChurchBoolean, t: T, f: T): T

// Church Numerals
type ChurchNumeral = <T>(f: (x: T) => T, x: T) => T

churchZero<T>(f: (x: T) => T, x: T): T
churchSucc(n: ChurchNumeral): ChurchNumeral
churchAdd(m: ChurchNumeral, n: ChurchNumeral): ChurchNumeral
churchMult(m: ChurchNumeral, n: ChurchNumeral): ChurchNumeral
```

---

## Recognition

### Core Operations

```typescript
// Recognition: (RecognitionMatrix → String → String → Real)
recognition(matrix: RecognitionMatrix): (recognizer: string) => (recognized: string) => Real

// Mutual Recognition: (RecognitionMatrix → String → String → Real)
mutual(matrix: RecognitionMatrix): (entity1: string) => (entity2: string) => Real

// Total Mutual Recognition: (RecognitionMatrix → String → Set String → Real)
tmr(matrix: RecognitionMatrix): (entity: string) => (universe: Set<string>) => Real

// Mutual Recognition Share: (RecognitionMatrix → String → Set String → Distribution)
mrs(matrix: RecognitionMatrix): (entity: string) => (universe: Set<string>) => Distribution

// Mutual Recognition Density: (RecognitionMatrix → String → Set String → Real)
mrd(matrix: RecognitionMatrix): (entity: string) => (universe: Set<string>) => Real
```

### Matrix Operations

```typescript
// Update recognition
updateRecognitionR(matrix: RecognitionMatrix): (recognizer: string) => (recognized: string) => (amount: Real) => unknown

// Create empty matrix
emptyMatrix(): RecognitionMatrix

// Create uniform matrix
uniformRecognitionMatrix(entities: Set<string>): (value: Real) => unknown
```

---

## Filters

### Basic Filters

```typescript
// Attribute filter
attr<T>(predicate: (x: T) => boolean): (set: Set<T>) => Set<T>

// MRD filter
mrdFilter(threshold: Real): (matrix: RecognitionMatrix) => (set: Set<Entity>) => Set<Entity>

// Time filter
timeFilter(minTimestamp: Real): (set: Set<Entity>) => Set<Entity>

// ID filter
idFilter(ids: Set<string>): (set: Set<Entity>) => Set<Entity>

// Metadata filter
metadataFilter(key: string): (value: unknown) => (set: Set<Entity>) => Set<Entity>

// Name pattern filter
nameFilter(pattern: RegExp): (set: Set<Entity>) => Set<Entity>
```

### Rank Filters

```typescript
// Top N by score
topN(n: number): (scoreFn: (entity: Entity) => Real) => (set: Set<Entity>) => Set<Entity>

// Bottom N by score
bottomN(n: number): (scoreFn: (entity: Entity) => Real) => (set: Set<Entity>) => Set<Entity>

// Percentile range
percentile(minPct: Real): (maxPct: Real) => (scoreFn: (entity: Entity) => Real) => (set: Set<Entity>) => Set<Entity>
```

### Logical Combinators

```typescript
// AND: intersection
andFilter<T>(f1: (s: Set<T>) => Set<T>): (f2: (s: Set<T>) => Set<T>) => (set: Set<T>) => Set<T>

// OR: union
orFilter<T>(f1: (s: Set<T>) => Set<T>): (f2: (s: Set<T>) => Set<T>) => (set: Set<T>) => Set<T>

// NOT: complement
notFilter<T>(f: (s: Set<T>) => Set<T>): (set: Set<T>) => Set<T>
```

### Utility Filters

```typescript
// Threshold by score
threshold<T>(minScore: Real): (scoreFn: (x: T) => Real) => (set: Set<T>) => Set<T>

// Range by score
range<T>(min: Real): (max: Real) => (scoreFn: (x: T) => Real) => (set: Set<T>) => Set<T>

// Sequential composition
seqFilter<T>(f1: (set: Set<T>) => Set<T>): (f2: (set: Set<T>) => Set<T>) => (set: Set<T>) => Set<T>
```

---

## Limits

### Basic Limits

```typescript
// Cap maximum
cap(maximum: Real): (dist: Distribution) => Distribution

// Floor minimum
floor(minimum: Real): (dist: Distribution) => Distribution

// Progressive (exponential)
progressive(alpha: Real): (dist: Distribution) => Distribution

// Type-based weighting
typeLimit(weights: (id: string) => Real): (dist: Distribution) => Distribution
```

### Specialized Limits

```typescript
// Range (min-max)
range(min: Real): (max: Real): (dist: Distribution) => Distribution

// Top K recipients
topK(k: number): (dist: Distribution) => Distribution

// Threshold minimum weight
thresholdLimit(minWeight: Real): (dist: Distribution) => Distribution

// Proportional cap
proportionalCap(proportion: Real): (dist: Distribution) => Distribution

// Gini-based fairness
gini(targetGini: Real): (dist: Distribution) => Distribution
```

### Composition

```typescript
// Compose two limits
composeLimits(l1: Limit): (l2: Limit) => Limit

// Sequential composition
seq(l1: Limit): (l2: Limit) => Limit

// Apply multiple limits
applyLimits(limits: Limit[]): (dist: Distribution) => Distribution
```

---

## Collective

### Formation

```typescript
// SCMRS: Selective Collective MRS
scmrs(matrix: RecognitionMatrix): (providers: Set<Entity>) => (recipients: Set<Entity>) => (filters: FilterFn[]) => (limits: LimitFn[]) => (entity: Entity) => Distribution

// SCRMRS: Selective Collective Relative MRS
scrmrs(matrix: RecognitionMatrix): (providers: Set<Entity>) => (recipients: Set<Entity>) => (filters: FilterFn[]) => (limits: LimitFn[]) => (entity: Entity) => Distribution

// Create collective
createCollective(id: string): (providers: Set<Entity>) => (recipients: Set<Entity>) => (filters: FilterFn[]) => (limits: LimitFn[]) => Collective
```

### Queries

```typescript
getProviders(collective: Collective): Set<Entity>
getRecipients(collective: Collective): Set<Entity>
isProvider(collective: Collective): (entity: Entity) => boolean
isRecipient(collective: Collective): (entity: Entity) => boolean
hasEntity(collective: Collective): (entity: Entity) => boolean
```

### Transformations

```typescript
addProvider(entity: Entity): (collective: Collective) => Collective
removeProvider(entity: Entity): (collective: Collective) => Collective
addRecipient(entity: Entity): (collective: Collective) => Collective
removeRecipient(entity: Entity): (collective: Collective) => Collective
```

### Aggregation

```typescript
totalCapacity(collective: Collective): (capacityFn: (e: Entity) => Real) => Real
avgCapacity(collective: Collective): (capacityFn: (e: Entity) => Real) => Real
totalDemand(collective: Collective): (demandFn: (e: Entity) => Real) => Real
avgDemand(collective: Collective): (demandFn: (e: Entity) => Real) => Real
```

---

## Commons

### Formation

```typescript
createCommons(id: string): (members: Set<Entity>) => (capacity: Real) => Commons
createCommonsWithMetadata(id: string): (members: Set<Entity>) => (capacity: Real) => (metadata: Record<string, unknown>) => Commons
```

### Queries

```typescript
getMembers(commons: Commons): Set<Entity>
getCapacity(commons: Commons): Real
getResources(commons: Commons): Record<string, Real>
getTotalResources(commons: Commons): Real
isMember(commons: Commons): (entity: Entity) => boolean
memberCount(commons: Commons): number
```

### Transformations

```typescript
addMember(entity: Entity): (commons: Commons) => Commons
removeMember(entity: Entity): (commons: Commons) => Commons
setCapacity(capacity: Real): (commons: Commons) => Commons
addResource(resourceId: string): (amount: Real) => (commons: Commons) => Commons
removeResource(resourceId: string): (amount: Real) => (commons: Commons) => Commons
setResource(resourceId: string): (amount: Real) => (commons: Commons) => Commons
```

### Evolution

```typescript
evolveCommons(commons: Commons): (allocation: Distribution) => Commons
evolveWithCapacity(commons: Commons): (allocation: Distribution) => (newCapacity: Real) => Commons
allocateFromCommons(commons: Commons): (distribution: Distribution) => Commons
distributeResources(commons: Commons): (entities: Set<Entity>) => Distribution
```

### HyperCollective

```typescript
createHyperCollective(id: string): (commonsSets: Set<Commons>) => HyperCollective
addCommonsToHyper(commons: Commons): (hyper: HyperCollective) => HyperCollective
getHyperMembers(hyper: HyperCollective): Set<Entity>
getHyperCapacity(hyper: HyperCollective): Real
getHyperResources(hyper: HyperCollective): Real
```

### Composition

```typescript
mergeCommons(c1: Commons): (c2: Commons) => Commons
splitCommons(commons: Commons): (ratio: Real) => [Commons, Commons]
```

---

## Allocation

### Main Algorithm

```typescript
allocateCapacity(matrix: RecognitionMatrix): (providers: Set<Entity>) => (recipients: Set<Entity>) => (capacityFn: CapacityFn) => (needFn: NeedFn) => (maxIterations?: number) => (convergenceThreshold?: Real) => AllocationResult
```

### Strategies

```typescript
equalAllocation(recipients: Set<Entity>): (totalCapacity: Real) => Distribution
proportionalAllocation(needFn: NeedFn): (recipients: Set<Entity>) => (totalCapacity: Real) => Distribution
priorityAllocation(priorityFn: (entity: Entity) => Real): (recipients: Set<Entity>) => (totalCapacity: Real) => Distribution
```

### Constraints

```typescript
minAllocation(minimum: Real): (dist: Distribution) => Distribution
maxAllocation(maximum: Real): (dist: Distribution) => Distribution
capTotal(cap: Real): (dist: Distribution) => Distribution
```

### Metrics

```typescript
satisfactionRate(needFn: NeedFn): (recipients: Set<Entity>) => (allocation: Distribution) => Real
utilizationRate(capacityFn: CapacityFn): (providers: Set<Entity>) => (allocation: Distribution) => Real
fairness(dist: Distribution): Real
```

### Transformations

```typescript
redistribute(dist: Distribution): (adjustmentFn: (id: string) => Real) => Distribution
transfer(fromId: string): (toId: string) => (amount: Real) => (dist: Distribution) => Distribution
```

---

## System

### Initialization

```typescript
initSystem(entities: Set<Entity>): (matrix: RecognitionMatrix) => SystemState
initSystemWithMetadata(entities: Set<Entity>): (matrix: RecognitionMatrix) => (metadata: Record<string, unknown>) => SystemState
```

### Queries

```typescript
getEntities(state: SystemState): Set<Entity>
getRecognitionMatrix(state: SystemState): RecognitionMatrix
getCollectives(state: SystemState): Collective[]
getCommons(state: SystemState): Commons[]
getTimestamp(state: SystemState): Real
findEntity(id: string): (state: SystemState) => Entity | undefined
findCollective(id: string): (state: SystemState) => Collective | undefined
findCommons(id: string): (state: SystemState) => Commons | undefined
```

### Transformations

```typescript
addEntity(entity: Entity): (state: SystemState) => SystemState
removeEntity(entityId: string): (state: SystemState) => SystemState
updateEntity(entity: Entity): (state: SystemState) => SystemState
addCollective(collective: Collective): (state: SystemState) => SystemState
removeCollective(collectiveId: string): (state: SystemState) => SystemState
addCommons(commons: Commons): (state: SystemState) => SystemState
removeCommons(commonsId: string): (state: SystemState) => SystemState
setRecognitionMatrix(matrix: RecognitionMatrix): (state: SystemState) => SystemState
setTimestamp(timestamp: Real): (state: SystemState) => SystemState
```

### Evolution

```typescript
evolveSystem(state: SystemState): (deltaTime: Real) => SystemState
evolveWithRecognition(recognizer: Entity): (recognized: Entity) => (amount: Real) => (state: SystemState) => SystemState
evolveWithAllocation(capacityFn: CapacityFn): (needFn: NeedFn) => (state: SystemState) => SystemState
evolveStep(state: SystemState): (deltaTime: Real) => (capacityFn: CapacityFn) => (needFn: NeedFn) => SystemState
```

### Metrics

```typescript
totalEntities(state: SystemState): number
totalCollectives(state: SystemState): number
totalCommons(state: SystemState): number
totalRecognition(state: SystemState): Real
avgRecognition(state: SystemState): Real
networkDensity(state: SystemState): Real
```

### Convergence

```typescript
hasConverged(oldState: SystemState): (newState: SystemState) => (threshold: Real) => boolean
iterateUntilConvergence(initialState: SystemState): (evolveFn: (state: SystemState) => SystemState) => (threshold: Real) => (maxIterations: number) => SystemState
```

### Utilities

```typescript
takeSnapshot(state: SystemState): SystemState
compareStates(state1: SystemState): (state2: SystemState) => Record<string, unknown>
cloneSystem(state: SystemState): SystemState
resetTimestamp(state: SystemState): SystemState
clearMetadata(state: SystemState): SystemState
```

---

## Usage Examples

### Composition Pipeline

```typescript
import { elegant } from '@free-association/lambda-calculus';

// Build a recognition-based allocation pipeline
const allocate = pipe(
  // Start with MRS
  elegant.mrs(matrix)(entityId)(universe),
  
  // Apply filters
  elegant.topK(10),
  elegant.threshold(0.1),
  
  // Apply limits
  elegant.cap(0.5),
  elegant.progressive(0.8)
);

const result = allocate;
```

### Partial Application

```typescript
// Create specialized functions through partial application
const aliceMutual = elegant.mutual(matrix)('alice');
const highRecognitionFilter = elegant.threshold(0.7);
const fairLimit = elegant.gini(0.3);

// Use them
const mutualWithBob = aliceMutual('bob');
const filtered = highRecognitionFilter(scoreFn)(entities);
const limited = fairLimit(distribution);
```

### Monadic Context

```typescript
// Use Reader monad for dependency injection
const calculateMetrics = (ctx: { matrix: RecognitionMatrix, universe: Set<string> }) => {
  const mutualFn = elegant.mutual(ctx.matrix);
  const tmrFn = elegant.tmr(ctx.matrix);
  
  return {
    totalMutual: Array.from(ctx.universe).reduce((sum, id) => sum + tmrFn(id)(ctx.universe), 0),
    avgMutual: /* ... */,
  };
};

const metrics = elegant.runReader(context)(calculateMetrics);
```

---

## Type Safety

All functions are fully typed with TypeScript generics. The compiler will catch type errors:

```typescript
// ✅ Correct
const mr: number = elegant.mutual(matrix)('alice')('bob');

// ❌ Type error: missing parameter
const mr = elegant.mutual(matrix)('alice');  // Returns (string) => number

// ❌ Type error: wrong type
const mr = elegant.mutual(matrix)(123)('bob');  // 123 is not a string
```

---

## Performance Notes

**Curried functions create intermediate closures**, which may have performance implications:

```typescript
// Many calls: may be slower
for (let i = 0; i < 10000; i++) {
  elegant.mutual(matrix)('alice')('bob');
}

// Reuse partial application: faster
const aliceMutual = elegant.mutual(matrix)('alice');
for (let i = 0; i < 10000; i++) {
  aliceMutual('bob');
}
```

For performance-critical code, consider using core API.

---

## See Also

- [ELEGANT-COMPLETE-STATUS.md](../ELEGANT-COMPLETE-STATUS.md) - Implementation status
- [CORE-VS-ELEGANT.md](./CORE-VS-ELEGANT.md) - Comparison with core API
- [ELEGANCE.md](./ELEGANCE.md) - Design principles

