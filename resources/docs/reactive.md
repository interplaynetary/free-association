# Reactive Programming with GunDB: A Comprehensive Guide

## Overview

This guide introduces our reactive programming system built on top of GunDB. Our system implements a reactive data layer that automatically tracks dependencies between values and updates the UI efficiently when data changes. It's similar to MobX or Vue's reactivity system but is specifically designed to work with GunDB's offline-first, distributed database.

## Core Concepts

Reactive programming is a paradigm focused on data flows and the propagation of changes. Our implementation consists of several key components:

- **Reactive Values**: Observable values that notify listeners when they change
- **Computed Values**: Values derived from other reactive values, automatically recalculated when dependencies change
- **Reactive Entities**: Database objects with reactive properties that sync with GunDB
- **Automatic Dependency Tracking**: The system automatically detects and tracks dependencies

Here's a visualization of the dependency graph between reactive values:

```
    ┌─────────────┐      ┌─────────────┐
    │ Reactive<A> │      │ Reactive<B> │
    └──────┬──────┘      └──────┬──────┘
           │                    │
           │                    │
           ▼                    ▼
    ┌────────────────────────────────┐
    │       Computed<f(A,B)>         │─┐
    └──────────────┬─────────────────┘ │
                   │                   │
                   ▼                   │
    ┌─────────────────────────────┐    │
    │     Computed<g(f(A,B))>     │◄───┘
    └─────────────┬───────────────┘
                  │
                  ▼
             UI Rendering
```

## Internal Architecture

### Class Hierarchy

Our system consists of the following key classes:

```
Reactive<T>
├── ReactiveGun<T>
|
Computed<T>
|
ReactiveEntity<T>
├── TreeNode
├── User (example)
├── Other domain entities...
|
GunNode<T> (Used by ReactiveGun)
|
Batch (Utility)
|
ComputationCache<K,V> (Utility)
```

### Key Design Patterns

1. **Observer Pattern**: Reactive values maintain a list of observers
2. **Dependency Injection**: The `App` context is passed to entities
3. **Proxy/Decorator**: Properties are defined dynamically with getters/setters
4. **Factory Method**: Static creation methods for constructing entities
5. **Singleton**: Registry for tracking instances
6. **Command Pattern**: Batch processing for scheduling updates

## Basic Usage

### Creating Reactive Values

```typescript
import { reactive } from './models/Reactive';

// Create a simple reactive value
const count = reactive(0);

// Access the current value
console.log(count.value); // 0

// Update the value
count.value = 1;

// Subscribe to changes
const unsubscribe = count.subscribe(newValue => {
  console.log(`Count changed to ${newValue}`);
});

// Later, clean up the subscription
unsubscribe();

// You can also update with a function
count.update(current => current + 1); // count.value becomes 2
```

Internal implementation details:
- The `value` getter calls `Computed.trackAccess(this)` to register dependencies
- The `value` setter checks for actual changes before triggering notifications
- Subscriptions are stored in a `Set` for efficient add/remove operations

### Creating Computed Values

```typescript
import { reactive, computed } from './models/Reactive';

const width = reactive(5);
const height = reactive(10);

// Create a computed value that depends on other values
const area = computed(() => width.value * height.value);

console.log(area.value); // 50

// When dependencies change, computed values update automatically
width.value = 7;
console.log(area.value); // 70

// Computed values can depend on other computed values
const perimeter = computed(() => 2 * (width.value + height.value));
const areaToPerimeterRatio = computed(() => area.value / perimeter.value);

// Reading a computed value forces recalculation only if needed
console.log(areaToPerimeterRatio.value); // 70 / (2 * (7 + 10)) = 1.296...
```

How dependency tracking works:
1. When `area.value` is accessed, the system sets `Computed.currentComputation = area`
2. The computer function runs, accessing `width.value` and `height.value`
3. These getters call `Computed.trackAccess(this)`, registering as dependencies
4. When computation completes, `area` has dependencies on both `width` and `height`
5. When either changes, `area` is marked as invalid, triggering recomputation on next access

## Integrating with GunDB

### Using ReactiveGun

`ReactiveGun` connects reactive values to GunDB nodes:

```typescript
import { ReactiveGun } from './models/Reactive';
import { gun } from './models/Gun';

// Create a reactive value linked to a Gun node
const username = new ReactiveGun(['users', 'alice', 'name'], 'Alice');

// Changes to the value are automatically synced to Gun
username.value = 'Alice Smith';

// Changes from Gun are automatically reflected in the reactive value
// If another user updates the name, the reactive value will update

// You can also access the underlying GunNode
const gunNode = username.gunNode;
await gunNode.once(); // Get current value directly

// Access the Gun subscription stream
const stream = username.stream();
// Process stream data...
```

How synchronization works:
1. The `ReactiveGun` constructor creates a `GunNode` for the specified path
2. It sets up a subscription to the node that updates the reactive value
3. The `value` setter overrides the parent method to also update the Gun node
4. This creates a two-way binding between the reactive value and Gun

### Creating Reactive Entities

For more complex objects, use `ReactiveEntity`:

```typescript
import { ReactiveEntity } from './models/Reactive';
import { App } from '../App';

interface UserData {
  id: string;
  name: string;
  age: number;
  email?: string;
}

class User extends ReactiveEntity<UserData> {
  // Declare properties that will be defined reactively
  declare public name: string;
  declare public age: number;
  declare public email: string | undefined;
  
  // Declare computed properties
  declare public isAdult: boolean;
  declare public displayName: string;
  
  constructor(id: string, app: App) {
    super(id, app);
    
    // Define reactive properties that sync with Gun
    this.defineReactiveProperty('name', '', ['users', id, 'name']);
    this.defineReactiveProperty('age', 0, ['users', id, 'age']);
    this.defineReactiveProperty('email', undefined, ['users', id, 'email']);
    
    // Define computed properties
    this.defineComputedProperty('isAdult', () => this.age >= 18);
    this.defineComputedProperty('displayName', () => {
      return this.name || `User-${this.id.substring(0, 6)}`;
    });
  }
  
  // Add custom methods
  celebrateBirthday() {
    this.age += 1;
  }
  
  async loadProfile() {
    // Perform additional data loading if needed
    const data = await this.gunNode.once();
    console.log('Loaded profile data:', data);
    return data;
  }
}

// Usage
const user = new User('alice', app);
console.log(user.name); // Gets value from Gun
user.name = 'Alice'; // Updates value in Gun
console.log(user.isAdult); // Computed based on age

// Create multiple instances
const alice = new User('alice', app);
const bob = new User('bob', app);

// Define relationships between entities
alice.friends = [bob.id];
```

How property definition works:
1. `defineReactiveProperty` creates a `ReactiveGun` or `Reactive` instance
2. It defines a property on the entity with getter/setter accessors
3. The accessors delegate to the reactive instance
4. `defineComputedProperty` creates a `Computed` instance with the function bound to the entity

## Advanced Features

### Batch Processing

For performance optimization, you can batch related updates:

```typescript
import { Batch } from './models/Reactive';

// Multiple separate updates trigger UI updates for each change
user.name = 'Bob'; // UI update 1
user.age = 30;     // UI update 2
user.email = 'bob@example.com'; // UI update 3

// With batch processing, updates are processed together
Batch.schedule(() => {
  // Multiple updates in one batch
  user.name = 'Bob';
  user.age = 30;
  user.email = 'bob@example.com';
  // UI will update only once after all these changes
});

// You can nest batch operations
Batch.schedule(() => {
  user.name = 'Bob';
  
  Batch.schedule(() => {
    user.age = 30;
    user.email = 'bob@example.com';
  });
  
  // Still just one UI update after all changes
});
```

Implementation details:
1. `Batch.schedule` adds the callback to a queue
2. If not already scheduled, it sets a timeout to process the queue
3. When processing, it runs all queued callbacks in sequence
4. This allows multiple changes to be applied before any notifications

### Debounced Updates

Debounce functions to avoid excessive updates:

```typescript
import { debounced } from './models/Reactive';

// Automatically batched and debounced update function
const updateProfile = debounced((name, age) => {
  user.name = name;
  user.age = age;
}, 300); // 300ms debounce time

// Call multiple times, but will only execute once after 300ms of inactivity
updateProfile('Alice', 25);
updateProfile('Alice', 26);
updateProfile('Alice', 27);

// Great for handling UI input events
searchInput.addEventListener('input', debounced((e) => {
  searchResults.value = performSearch(e.target.value);
}, 250));
```

Implementation details:
1. `debounced` returns a function that delays execution of the original function
2. It clears any pending timeout when called again
3. When the timeout expires, it schedules the execution via `Batch.schedule`
4. This ensures both debouncing and batching of updates

### Computation Cache

Cache expensive computations with dependency tracking:

```typescript
import { ComputationCache } from './models/Reactive';

// Define a cache for expensive calculations
const expensiveCalculation = new ComputationCache<string, number>((key) => {
  console.log('Performing expensive calculation for:', key);
  // Simulate expensive operation
  let result = 0;
  for (let i = 0; i < 1000000; i++) {
    result += key.charCodeAt(i % key.length);
  }
  return result;
});

// First time calculates
console.time('first');
const result1 = expensiveCalculation.get('test');
console.timeEnd('first'); // Slow

// Second time uses cache
console.time('second');
const result2 = expensiveCalculation.get('test');
console.timeEnd('second'); // Fast

// Dependency tracking
function computeWithDependencies() {
  const computed = computed(() => {
    // This computed value now depends on the cached result
    return expensiveCalculation.get('test') * 2;
  });
  
  return computed.value;
}

// Clear when needed
expensiveCalculation.invalidate('test'); // Clear specific key
expensiveCalculation.clear(); // Clear all cache
```

Implementation details:
1. `ComputationCache` stores results in a Map for quick lookup
2. It also tracks dependencies between computed values and cache entries
3. When `get()` is called within a computation, it registers as a dependency
4. This enables automatic cache invalidation when dependencies change

### Working with Collections

Managing collections of reactive entities:

```typescript
import { reactive, Batch } from './models/Reactive';

// Create a collection of users
const users = reactive(new Map<string, User>());

// Add a user
function addUser(id: string, name: string, age: number) {
  const user = new User(id, app);
  user.name = name;
  user.age = age;
  
  // Update the collection - create a new Map to trigger change detection
  users.update(current => {
    const updated = new Map(current);
    updated.set(id, user);
    return updated;
  });
}

// Remove a user
function removeUser(id: string) {
  users.update(current => {
    const updated = new Map(current);
    
    // Get the user to dispose it properly
    const user = updated.get(id);
    if (user) {
      user.dispose(); // Clean up subscriptions
      updated.delete(id);
    }
    
    return updated;
  });
}

// Batch update multiple users
function updateAges(increment: number) {
  Batch.schedule(() => {
    users.value.forEach(user => {
      user.age += increment;
    });
  });
}
```

Best practices for collections:
1. Always create a new collection when updating (immutable update pattern)
2. Use `update()` method for derived updates
3. Remember to clean up entity resources when removing from collections
4. Batch updates that affect multiple entities

## Integration with Svelte

Our reactive system works seamlessly with Svelte's stores:

```svelte
<script>
  import { User } from './models/User';
  import { app } from './app';
  import { reactive, computed } from './models/Reactive';
  
  // Create a user entity
  const user = new User('alice', app);
  
  // Get reactive stores
  const nameStore = user.getStore('name');
  const ageStore = user.getStore('age');
  const isAdultStore = user.getComputedStore('isAdult');
  
  // Create additional derived stores
  const nameLength = computed(() => user.name.length);
  const nameLengthStore = nameLength.toStore();
  
  // Handle events
  function incrementAge() {
    user.age += 1;
  }
</script>

<h1>User Profile</h1>

<div>
  <label>
    Name:
    <input bind:value={$nameStore} />
  </label>
  <p>Name length: {$nameLengthStore} characters</p>
</div>

<div>
  <label>
    Age:
    <input type="number" bind:value={$ageStore} />
  </label>
  <button on:click={incrementAge}>Add Year</button>
  <p>Status: {$isAdultStore ? 'Adult' : 'Minor'}</p>
</div>
```

How Svelte integration works:
1. The `toStore()` method creates a Svelte-compatible store object
2. This object has a `subscribe` method that Svelte uses for reactivity
3. For two-way binding, the store's values are updated, which then update the reactive values
4. Changes in reactive values propagate to the store subscribers (Svelte components)

## TreeNode Example: Recognition Calculation

Our `TreeNode` class demonstrates a complex reactive entity that calculates recognition shares:

```typescript
// Create a root node
const root = await TreeNode.create('Project', { points: 0 }, app);

// Add child nodes with points
const alice = await root.addChild('Alice', 30);
const bob = await root.addChild('Bob', 20);
const charlie = await root.addChild('Charlie', 50);

// Add types to nodes
alice.addType('design');
bob.addType('code');
charlie.addType('code');

// Recognition is automatically calculated with reactive dependencies
console.log(root.shares); // Shows distribution across types

// Visualize dependency chain for shares calculation:
// 
//  points    totalChildPoints    weight    fulfilled
//    │             │               │           │
//    └─────────────┼───────────────┘           │
//                  │                           │
//                  └───────────────────────────┘
//                                │
//                          calculateShares
//                                │
//                              shares

// Setting up observers to monitor changes
const sharesSubscription = root.getComputedStore('shares').subscribe(shares => {
  console.log('Shares updated:', shares);
  // Update UI visualization here
});

// Simulate changes
alice.points = 40; // Triggers recalculation of shares

// Later, clean up
sharesSubscription();
root.dispose(); // Cleans up all nodes
```

Implementation details of TreeNode's reactive recognition:
1. Each node has reactive properties for points, name, etc.
2. Computed properties derive totalChildPoints, weight, and fulfillment
3. The shares computation only happens at the root node
4. Changes to any node's points propagate up through parent references
5. The shares computation is cached to avoid redundant calculations
6. Type indices track which nodes implement which types

## Advanced Patterns

### Lazy Loading Relationships

```typescript
class Post extends ReactiveEntity<PostData> {
  declare public title: string;
  declare public content: string;
  declare public authorId: string;
  
  private _author: User | null = null;
  
  // Lazy-loaded relationship
  async getAuthor(): Promise<User> {
    if (!this._author) {
      this._author = new User(this.authorId, this._app);
      await this._author.loadProfile();
    }
    return this._author;
  }
  
  // Alternative with a reactive handle
  private _authorReactive = reactive<User | null>(null);
  
  get author(): User | null {
    return this._authorReactive.value;
  }
  
  async loadAuthor(): Promise<User> {
    if (!this._authorReactive.value) {
      const author = new User(this.authorId, this._app);
      await author.loadProfile();
      this._authorReactive.value = author;
    }
    return this._authorReactive.value!;
  }
}
```

### Cross-Entity Computed Values

```typescript
class Team extends ReactiveEntity<TeamData> {
  declare public name: string;
  declare public memberIds: string[];
  
  private _members = reactive<Map<string, User>>(new Map());
  
  async loadMembers() {
    const newMembers = new Map();
    for (const id of this.memberIds) {
      const user = new User(id, this._app);
      await user.loadProfile();
      newMembers.set(id, user);
    }
    this._members.value = newMembers;
  }
  
  // Computed across multiple entities
  get averageAge(): number {
    const members = this._members.value;
    if (members.size === 0) return 0;
    
    const sum = Array.from(members.values())
      .reduce((total, user) => total + user.age, 0);
    return sum / members.size;
  }
}
```

### Finite State Machines

```typescript
type OrderState = 'new' | 'processing' | 'shipped' | 'delivered' | 'cancelled';

class Order extends ReactiveEntity<OrderData> {
  declare public state: OrderState;
  declare public items: string[];
  
  // State transition methods
  process() {
    if (this.state !== 'new') {
      throw new Error(`Cannot process order in state: ${this.state}`);
    }
    this.state = 'processing';
  }
  
  ship() {
    if (this.state !== 'processing') {
      throw new Error(`Cannot ship order in state: ${this.state}`);
    }
    this.state = 'shipped';
  }
  
  // Computed based on state
  get canBeCancelled(): boolean {
    return this.state === 'new' || this.state === 'processing';
  }
  
  get isComplete(): boolean {
    return this.state === 'delivered';
  }
}
```

## Best Practices

1. **Prefer Computed Properties Over Methods**: Use computed properties for values derived from state.

```typescript
// Bad
class User {
  get age() { return this._age; }
  
  isAdult() { // Method, called repeatedly
    return this.age >= 18;
  }
}

// Good
class User {
  get age() { return this._age; }
  
  get isAdult() { // Computed property, cached
    return this.age >= 18;
  }
}
```

2. **Clean Up Subscriptions**: Always dispose entities when no longer needed to prevent memory leaks.

```typescript
// Component lifecycle
onMount(() => {
  const user = new User('alice', app);
  // Use user...
  
  return () => {
    user.dispose(); // Clean up on unmount
  };
});
```

3. **Organize by Entity**: Structure your code around reactive entities that represent business domain objects.

```typescript
// Prefer this organization:
/src
  /models
    /User.ts
    /Post.ts
    /Comment.ts
  /services
    /AuthService.ts
  /components
    /UserProfile.svelte
```

4. **Batch Related Updates**: Use `Batch.schedule()` for related changes to avoid excessive UI updates.

```typescript
// Instead of:
user.name = 'Alice';
user.email = 'alice@example.com';
user.age = 30;

// Prefer:
Batch.schedule(() => {
  user.name = 'Alice';
  user.email = 'alice@example.com';
  user.age = 30;
});
```

5. **Don't Mutate Internal Collections**: Always create new copies when updating collections in reactive values.

```typescript
// Bad
function addTodo(todo) {
  const todos = user.todos.value;
  todos.push(todo); // Direct mutation!
}

// Good
function addTodo(todo) {
  user.todos.update(current => {
    return [...current, todo]; // New array
  });
}
```

6. **Use TypeScript Declarations**: Always declare reactive properties to help TypeScript understand your entities.

```typescript
class User extends ReactiveEntity<UserData> {
  // Explicitly declare properties to get type checking
  declare public name: string;
  declare public email: string;
  declare public isAdmin: boolean;
}
```

## Common Pitfalls

1. **Infinite Dependencies**: Be careful not to create circular dependencies between computed values.

```typescript
// BAD: Circular dependency
class CircularExample {
  constructor() {
    this.defineComputedProperty('a', () => this.b + 1);
    this.defineComputedProperty('b', () => this.a + 1);
  }
}
```

2. **Memory Leaks**: Always call `dispose()` on entities when no longer needed.

```typescript
// Common memory leak pattern:
function createTemporaryUser() {
  const user = new User('temp', app);
  return user; // If caller doesn't dispose, memory leak!
}

// Better:
function useTemporaryUser(callback) {
  const user = new User('temp', app);
  try {
    callback(user);
  } finally {
    user.dispose(); // Always clean up
  }
}
```

3. **Direct Gun Access**: Avoid bypassing the reactive system by directly accessing Gun.

```typescript
// BAD: Bypassing reactive system
gun.get('users').get('alice').get('name').put('Alice');

// GOOD: Using reactive system
user.name = 'Alice';
```

4. **Excessive Subscriptions**: Too many fine-grained subscriptions can hurt performance.

```typescript
// BAD: Too many subscriptions
items.forEach(item => {
  item.getStore('name').subscribe(updateUI);
  item.getStore('price').subscribe(updateUI);
  item.getStore('quantity').subscribe(updateUI);
});

// BETTER: One subscription to the parent collection
itemsCollection.subscribe(items => {
  // Update UI with all items at once
  renderItems(items);
});
```

## Troubleshooting

### Reactive Value Not Updating

**Problem**: You've changed a reactive value but the UI isn't updating.

**Solutions**:
1. Check if you're modifying a nested property or collection without creating a new reference
2. Verify that the entity hasn't been disposed
3. Ensure dependency tracking is working by checking the code path

### Memory Leaks

**Problem**: Your application is using more and more memory over time.

**Solutions**:
1. Check that all entities are being disposed
2. Look for subscriptions that aren't being cleaned up
3. Verify that you're not holding references to disposed entities

### Excessive Recalculations

**Problem**: Your application is slow due to too many recalculations.

**Solutions**:
1. Use batch processing for related updates
2. Cache expensive computations
3. Use more granular reactive values to avoid unnecessary dependencies

## Conclusion

Our reactive system provides a powerful way to build responsive, real-time applications with GunDB. By automatically tracking dependencies and synchronizing with the database, it dramatically simplifies state management in distributed applications.

The integration of GunDB with reactive programming enables the creation of complex, collaborative applications that work offline and automatically synchronize when online. By following the patterns and practices outlined in this guide, you can create efficient, maintainable reactive applications.

For more examples and API details, see the TypeScript definitions in `Reactive.ts` and the implementation in `ReactTreeNode.ts`.