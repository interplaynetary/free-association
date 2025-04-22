# Reactive Gun Stores

This library provides a powerful reactive programming pattern for Gun databases, combining the best of Gun's real-time capabilities with Svelte's reactive store pattern and modern reactive programming concepts.

## Core Features

- **Seamless Svelte Integration**: All stores are fully compatible with Svelte's `$store` syntax
- **Reactive Stream Composition**: Combine, filter, map, and transform data streams
- **Automatic Resource Management**: Subscriptions are properly initialized and cleaned up
- **Type Safety**: Full TypeScript support with proper typing for collections and nested data
- **Optimized Updates**: Prevents unnecessary re-renders and efficiently processes updates

## Basic Usage

### Single Node Store

Access a single Gun node reactively:

```typescript
import { createGunStore } from "./reactiveStores";

// Create a store for a user profile
const profile = createGunStore(["users", "alice", "profile"]);

// Use in Svelte components
$: userName = $profile?.name || "Guest";

// Update the data
profile.node().put({ name: "Alice Smith" });
```

### Collection Store

Work with Gun collections (sets of nodes):

```typescript
import { createCollectionStore } from './reactiveStores';

// Create a tasks collection
const tasks = createCollectionStore<Task>('tasks');

// Add an item
tasks.add({
  title: 'Learn Gun',
  completed: false,
  priority: 'high',
  created: Date.now()
});

// Update an item
tasks.update('task123', { completed: true });

// Remove an item
tasks.remove('task123');

// Use in Svelte components
{#each $tasks as [id, task]}
  <div class:completed={task.completed}>
    {task.title}
    <button onclick={() => tasks.update(id, { completed: !task.completed })}>
      Toggle
    </button>
  </div>
{/each}
```

## Stream Composition

### Combining Stores

Merge multiple data sources into a single reactive stream:

```typescript
import { createGunStore, combineStores } from "./reactiveStores";

const user = createGunStore(["users", "current"]);
const preferences = createGunStore(["preferences"]);

// Combine into a unified store
const userWithPrefs = combineStores(
  user,
  preferences,
  (userData, prefsData) => ({
    ...userData,
    preferences: prefsData,
  })
);

// Use the combined store
$: theme = $userWithPrefs?.preferences?.theme || "light";
```

### Derived Data

Transform data with mapping and filtering:

```typescript
import { createCollectionStore, filterCollectionStore } from "./reactiveStores";

const tasks = createCollectionStore<Task>("tasks");

// Create a filtered view of completed tasks
const completedTasks = filterCollectionStore(
  tasks,
  (task) => task.completed === true
);

// Create a store for the completion percentage
const completionPercentage = mapStore(tasks, (tasks) => {
  const total = tasks.length;
  const completed = tasks.filter(([_, task]) => task.completed).length;
  return total > 0 ? Math.round((completed / total) * 100) : 0;
});
```

### Advanced Patterns

#### Dynamic Data Sources

Switch data sources based on conditions:

```typescript
import { createGunStore, switchMapStore } from "./reactiveStores";

const user = createGunStore(["users", "current"]);

// Switch content based on user role
const dynamicContent = switchMapStore(user, (userData) => {
  if (userData?.role === "admin") {
    return createGunStore(["content", "admin"]);
  } else {
    return createGunStore(["content", "user"]);
  }
});
```

#### Debounced Updates

Limit update frequency for better performance:

```typescript
import { createGunStore, debounceStore } from "./reactiveStores";

// User input store
const searchQuery = createGunStore(["app", "search"]);

// Debounce to avoid excessive processing
const debouncedSearch = debounceStore(searchQuery, 300);

// Use the debounced version for search operations
$: performSearch($debouncedSearch);
```

#### Deep Data Resolution

Automatically resolve references in Gun data:

```typescript
import { createDeepStore } from "./reactiveStores";

// Create a store that resolves references one level deep
const userWithFriends = createDeepStore(["users", "current"], 1);

// References in userWithFriends will be automatically resolved
$: friends = $userWithFriends?.friends || [];
```

## Best Practices

1. **Use the Right Store Type**

   - `createGunStore` for single nodes
   - `createCollectionStore` for sets of nodes
   - `createDeepStore` for data with references

2. **Minimize Transformations**

   - Put expensive transformations in derived stores
   - Use debouncing for frequently changing data
   - Consider memoizing complex calculations

3. **Proper Cleanup**

   - All stores automatically clean up when there are no more subscribers
   - Manually call `unsubscribe()` for programmatically created subscriptions

4. **Type Safety**

   - Define proper interfaces for your data
   - Extend the `CollectionItem` interface for collection items
   - Use type parameters to ensure type safety in transformations

5. **Performance Considerations**
   - Use `filterCollectionStore` instead of filtering in components
   - Consider using `debounceStore` for user input and high-frequency updates
   - For complex UIs, split data into smaller, focused stores

## Real-World Examples

Check out these example files to see the reactive store pattern in action:

- `enhancedStores.ts` - A practical todo application
- `reactiveStoreExamples.ts` - Complex examples showcasing store composition

## Under the Hood

This pattern builds upon:

1. Gun's real-time database capabilities
2. Svelte's reactive store pattern
3. Reactive streams inspired by RxJS
4. GunSubscription's composition methods
5. TypeScript's strong typing system

By combining these powerful concepts, we create a seamless, type-safe way to work with decentralized real-time data that feels natural in a Svelte application.
