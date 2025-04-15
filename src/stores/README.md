# Reactive Stores

This directory contains the reactive store factories for the FreeAssociation application. These stores create a bridge between Gun.js data and Svelte's reactivity system.

## Core Philosophy

The reactive stores approach represents a significant improvement over the legacy stream-based approach:

1. **Composition over Inheritance** - Uses factory functions instead of class inheritance
2. **TypeScript Friendly** - Properly typed with interfaces and generics
3. **Reactive by Default** - Designed to work seamlessly with Svelte's reactivity
4. **Separation of Concerns** - Domain models focus on data, reactive stores on UI binding
5. **Testable** - Factory functions are easier to test than complex class hierarchies

## Main Components

### Recognition Stores

The `createRecognitionStores` factory creates a bundle of reactive stores for a Recognition node:

```typescript
const {
  // Data stores
  nameStore,
  pointsStore,
  
  // Collection stores
  contributorsStore,
  childrenStore,
  
  // Derived stores
  isContributionStore,
  fulfilledStore,
  desireStore,
  
  // Methods
  addChild,
  removeChild,
} = createRecognitionStores(['recognitions', 'abc123']);
```

### Contributor Stores

The `createContributorStores` factory extends recognition stores with contributor-specific functionality:

```typescript
const {
  // Recognition stores
  nameStore,
  fulfilledStore,
  
  // Contributor-specific stores
  sharesOfGeneralFulfillmentStore,
  socialDistributionsStore,
  
  // Contributor methods
  updateSharesOfGeneralFulfillment,
  calculateSocialDistribution,
} = createContributorStores(['contributors', 'user1']);
```

### Store Access Helper

The `StoreAccess` singleton provides a convenient way to access stores within a specific app scope:

```typescript
const storeAccess = StoreAccess.getInstance('my-app');

const recognitionStores = storeAccess.getRecognitionStores('recognitions', 'abc123');
const contributorStores = storeAccess.getContributorStores('contributors', 'user1');
```

## Usage in Svelte Components

Stores can be used directly in Svelte components with the $ prefix:

```svelte
<script>
  import { createRecognitionStores } from '../stores/recognition';
  
  const {
    nameStore,
    pointsStore,
    fulfilledStore,
    addChild,
  } = createRecognitionStores(['recognitions', 'abc123']);
  
  let newChildName = '';
</script>

<h2>{$nameStore || 'Unnamed Recognition'}</h2>
<p>Points: {$pointsStore}</p>
<p>Fulfilled: {($fulfilledStore * 100).toFixed(1)}%</p>

<form on:submit|preventDefault={() => {
  if (newChildName) {
    addChild(newChildName);
    newChildName = '';
  }
}}>
  <input bind:value={newChildName} placeholder="Child name">
  <button type="submit">Add Child</button>
</form>
```

## Implementation Notes

1. **Gun Integration** - Uses `createGunStore` and `createCollectionStore` from the reactive stores library
2. **Derived Values** - Complex calculations use `deriveFromGun` for efficient, reactive computation
3. **Async Operations** - Methods return Promises for async operations
4. **Type Safety** - Interfaces ensure consistent API across implementations

## Migration from Legacy Streams

If you have code using the legacy stream approach, here's how to migrate:

Old approach:
```typescript
const recognition = new Recognition(['path', 'to', 'node']);
const stream = recognition.asStream();
const nameStore = stream.nameStore();
const pointsStore = stream.pointsStore();
```

New approach:
```typescript
const {
  nameStore,
  pointsStore
} = createRecognitionStores(['path', 'to', 'node']);
``` 