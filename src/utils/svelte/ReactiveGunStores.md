# Reactive Gun Stores

This guide explains how our reactive programming capabilities enhance the store pattern for Gun data, providing a cleaner and more maintainable approach to state management in decentralized applications.

## The Store Pattern and Reactive Programming

The store pattern is a way to manage and share state across components in an application. With our enhanced reactive capabilities, we can now create more powerful and flexible Gun stores that leverage the reactive programming paradigm.

## Traditional Gun Store Approach

In the traditional approach (based on Svelte stores or similar patterns), a Gun store might look something like this:

```typescript
// stores.js
import { gun } from "./gunSetup";
import { writable } from "svelte/store";

// Create a custom store that wraps Gun data
function createGunStore(path) {
  const { set, update, subscribe } = writable({ loading: true });
  
  // Set up Gun subscription
  gun.get(path).on(data => {
    set({ loading: false, data });
  });
  
  return {
    subscribe,
    put: (value) => gun.get(path).put(value)
  };
}

export const userStore = createGunStore("users/current");
```

While this works, it has limitations in composability and doesn't leverage the full potential of reactive data flows.

## Enhanced Reactive Store Approach

Our reactive programming capabilities enable a more powerful store pattern:

```typescript
// stores.js
import { GunNode } from "./GunNode";
import { GunSubscription } from "./GunSubscription";

function createReactiveStore(path) {
  // Create a node reference
  const node = new GunNode(Array.isArray(path) ? path : [path]);
  
  // Create a base stream
  const stream = node.stream().startWith({ loading: true });
  
  // Return an object with the subscription interface and additional methods
  return {
    // Standard subscription interface for Svelte compatibility
    subscribe: stream.on.bind(stream),
    
    // Methods for modifying data
    put: (value) => node.put(value),
    
    // Access to the underlying stream for composition
    stream: () => stream,
    
    // Access to the node for direct operations
    node: () => node
  };
}

export const userStore = createReactiveStore("users/current");
```

## Benefits of Reactive Stores

1. **Composability**: Reactive stores can be combined, transformed, and composed:

```typescript
import { userStore } from "./stores";
import { preferencesStore } from "./stores";

// Combine user data with preferences
const enhancedUserStore = {
  subscribe: userStore.stream()
    .combine(preferencesStore.stream(), (user, prefs) => {
      return { ...user, preferences: prefs };
    })
    .on.bind()
};
```

2. **Derived Stores**: Create derived stores with clean transformations:

```typescript
// Create a store that only contains the user's theme preference
export const themeStore = {
  subscribe: userStore.stream()
    .map(user => user.theme || "default")
    .on.bind()
};
```

3. **Conditional Logic**: Add conditional logic to your stores:

```typescript
// Create a store that has different behavior for admins vs regular users
export const permissionsStore = {
  subscribe: userStore.stream()
    .switchMap(user => {
      if (user.role === "admin") {
        return adminPermissionsNode.stream();
      } else {
        return regularPermissionsNode.stream();
      }
    })
    .on.bind()
};
```

4. **Efficient Updates**: Process and filter updates before they reach components:

```typescript
// Only emit when notification count changes
export const notificationCountStore = {
  subscribe: notificationsStore.stream()
    .map(notifications => Object.keys(notifications || {}).length)
    .on.bind()
};
```

## Working with Dynamic Collections and Nested Stores

Gun's graph database naturally leads to working with collections and nested data. Our reactive approach makes handling these complex data structures much easier.

### Dynamic Collections of Reactive Items

A common pattern in Gun applications is having collections of items where each item might change independently. The reactive operators make this pattern much more manageable:

```typescript
function createItemsStore(collectionPath) {
  const node = new GunNode(Array.isArray(collectionPath) ? collectionPath : [collectionPath]);
  
  // Create a stream that represents the collection
  const itemsStream = node.stream().switchMap(() => node.each());
  
  // Track the state of all items
  let items = {};
  
  // The store will emit the entire collection whenever any item changes
  return {
    subscribe: (callback) => {
      return itemsStream.on(item => {
        if (item._removed) {
          delete items[item._key];
        } else {
          items[item._key] = item;
        }
        
        // Make a copy to ensure reactivity
        callback({...items});
      });
    },
    
    add: (value) => {
      const id = Date.now().toString(36) + Math.random().toString(36).substr(2);
      node.get(id).put(value);
      return id;
    },
    
    remove: (id) => node.get(id).put(null),
    update: (id, value) => node.get(id).put(value)
  };
}

export const tasksStore = createItemsStore("tasks");
```

### Deriving Values from Dynamic Collections

Following the insights from Svelte's store patterns, we can create a "scheduler" approach for computing derived values from dynamic collections:

```typescript
function createCollectionAggregator(collectionStore, aggregateFn) {
  // Create a writable store for the aggregated value
  const { set, subscribe } = writable(aggregateFn({}));
  
  // Subscribe to the collection and update the aggregate when it changes
  const unsubscribe = collectionStore.subscribe(items => {
    const aggregatedValue = aggregateFn(items);
    set(aggregatedValue);
  });
  
  return {
    subscribe,
    unsubscribe
  };
}

// Usage example:
const tasksStore = createItemsStore("tasks");
const completedTasksCount = createCollectionAggregator(
  tasksStore,
  (tasks) => Object.values(tasks).filter(task => task.completed).length
);

// Use in a Svelte component
$: console.log(`You have ${$completedTasksCount} completed tasks!`);
```

### Nested Reactive Stores

With Gun's graph structure, you often have nested data that needs to be reactive. Our approach makes this more manageable:

```typescript
function createNestedReactiveStore(path, nestedFields) {
  const mainNode = new GunNode(Array.isArray(path) ? path : [path]);
  const mainStream = mainNode.stream();
  
  // Create streams for each nested field
  const nestedStreams = {};
  for (const field of nestedFields) {
    nestedStreams[field] = mainStream.switchMap(data => {
      if (!data || !data[field]) {
        return GunSubscription.of(null);
      }
      return new GunNode([...path, field]).stream();
    });
  }
  
  // Combine main data with nested fields
  let combinedStream = mainStream;
  for (const [field, stream] of Object.entries(nestedStreams)) {
    combinedStream = combinedStream.combine(stream, (main, nested) => {
      return { ...main, [field]: nested };
    });
  }
  
  return {
    subscribe: combinedStream.on.bind(combinedStream),
    put: (value) => mainNode.put(value),
    // Add methods to update nested fields
    updateNested: (field, value) => {
      if (nestedFields.includes(field)) {
        mainNode.get(field).put(value);
      }
    }
  };
}

// Usage:
const userWithProfileStore = createNestedReactiveStore(
  ['users', userId], 
  ['profile', 'settings', 'friends']
);
```

## Advanced Store Patterns

### Stateful Stores

Create stores with local state that syncs with Gun:

```typescript
function createStatefulStore(path, initialState = {}) {
  const node = new GunNode(Array.isArray(path) ? path : [path]);
  let localState = { ...initialState };
  
  // Create a merged stream of local and remote state
  const remoteStream = node.stream();
  const localStream = GunSubscription.of(localState);
  
  const stream = remoteStream.combine(localStream, (remote, local) => {
    return { ...local, ...remote };
  });
  
  return {
    subscribe: stream.on.bind(stream),
    
    // Update both local state and Gun
    put: (value) => {
      node.put(value);
      localState = { ...localState, ...value };
      // Force an update of the local stream
      remoteStream.on(() => {});
    },
    
    // Update only local state (doesn't sync to Gun)
    setLocal: (value) => {
      localState = { ...localState, ...value };
      // Force an update of the local stream
      remoteStream.on(() => {});
    },
    
    // Access to the underlying stream for composition
    stream: () => stream,
    
    // Access to the node for direct operations
    node: () => node
  };
}
```

### Collection Stores

Create stores specifically designed for Gun collections:

```typescript
function createCollectionStore(path) {
  const node = new GunNode(Array.isArray(path) ? path : [path]);
  
  // Use the each method to get all items
  const itemsStream = node.stream().map(() => {
    return node.each();
  }).switchMap(itemStream => itemStream);
  
  return {
    subscribe: (callback) => {
      const items = {};
      
      return itemsStream.on(item => {
        // Handle addition/removal of items
        if (item._removed) {
          delete items[item._key];
        } else {
          items[item._key] = item;
        }
        // Call the callback with the full collection
        callback({ ...items });
      });
    },
    
    // Add an item to the collection
    add: (value) => {
      const id = Date.now().toString(36) + Math.random().toString(36).substr(2);
      node.get(id).put(value);
      return id;
    },
    
    // Remove an item from the collection
    remove: (id) => {
      node.get(id).put(null);
    },
    
    // Update an item in the collection
    update: (id, value) => {
      node.get(id).put(value);
    },
    
    // Get a specific item as a store
    item: (id) => createReactiveStore([...path, id]),
    
    // Access to the node for direct operations
    node: () => node
  };
}

export const tasksStore = createCollectionStore("tasks");
```

## Fixing the Svelte Todo App

Looking at the Todo.svelte component from our example app, we see a TypeScript error because the Gun chain doesn't have a `subscribe` method that Svelte expects. We can fix this by creating a proper store:

```typescript
// stores.js
import { gun } from '../utils/gun/gunSetup';
import { GunNode } from '../utils/gun/GunNode';
import { GunSubscription } from '../utils/gun/GunSubscription';

// Create a proper Svelte-compatible todos store
export function createTodosStore() {
  const todosNode = new GunNode(['todos']);
  
  // Create a collection stream that transforms the map into an array of [id, todo] pairs
  const todosStream = todosNode.stream().switchMap(() => {
    return todosNode.each();
  }).map(item => {
    // Each item already has _key from the GunSubscription.each() implementation
    return [item._key, item];
  });
  
  // Create an array to hold all todos
  let todoArray = [];
  
  return {
    subscribe: (callback) => {
      return todosStream.on(([id, todo]) => {
        if (todo._removed) {
          // Remove the todo from the array
          todoArray = todoArray.filter(([todoId]) => todoId !== id);
        } else {
          // Replace or add the todo
          const index = todoArray.findIndex(([todoId]) => todoId === id);
          if (index >= 0) {
            todoArray[index] = [id, todo];
          } else {
            todoArray.push([id, todo]);
          }
        }
        
        // Call the callback with a fresh copy of the array
        callback([...todoArray]);
      });
    },
    
    // Methods to modify todos
    add: (title) => {
      if (title.trim()) {
        const id = Date.now().toString();
        todosNode.get(id).put({ title, done: false, created: Date.now() });
      }
    },
    
    toggle: (id, currentState) => {
      todosNode.get(id).get('done').put(!currentState);
    },
    
    remove: (id) => {
      todosNode.get(id).put(null);
    }
  };
}

export const todos = createTodosStore();
```

Then in Todo.svelte:

```typescript
<script lang="ts">
  import { todos } from '../stores';
  
  let newTodo = '';
  
  function addTodo() {
    if (newTodo.trim()) {
      todos.add(newTodo);
      newTodo = '';
    }
  }
</script>

<div class="todo-app">
  <h1>GUN Todo App</h1>
  
  <form on:submit|preventDefault={addTodo}>
    <input bind:value={newTodo} placeholder="What needs to be done?">
    <button type="submit">Add</button>
  </form>
  
  <ul class="todo-list">
    {#each $todos as [id, todo] (id)}
      <li class:completed={todo.done}>
        <input 
          type="checkbox" 
          checked={todo.done} 
          on:change={() => todos.toggle(id, todo.done)}
        >
        <span>{todo.title}</span>
        <button on:click={() => todos.remove(id)}>Delete</button>
      </li>
    {/each}
  </ul>
</div>
```

## Integration with Svelte

Our reactive stores integrate seamlessly with Svelte:

```svelte
<script>
  import { userStore, tasksStore } from "./stores";
  
  // Reactive declarations based on store values
  $: username = $userStore.name || "Guest";
  $: taskCount = Object.keys($tasksStore).length;
  
  // Add a new task
  function addTask() {
    tasksStore.add({ 
      title: "New task", 
      completed: false, 
      createdAt: Date.now() 
    });
  }
</script>

<h1>Welcome, {username}!</h1>
<p>You have {taskCount} tasks.</p>

<button on:click={addTask}>Add Task</button>

<ul>
  {#each Object.entries($tasksStore) as [id, task]}
    <li class:completed={task.completed}>
      {task.title}
      <button on:click={() => tasksStore.remove(id)}>Delete</button>
      <button on:click={() => tasksStore.update(id, { ...task, completed: !task.completed })}>
        {task.completed ? "Mark Incomplete" : "Mark Complete"}
      </button>
    </li>
  {/each}
</ul>
```

## Optimizing Performance

Our reactive approach enables several performance optimizations:

1. **Debouncing Updates**: Prevent excessive UI updates

```typescript
export const typingStore = {
  subscribe: inputStore.stream()
    .debounce(300) // Only update after 300ms of inactivity
    .on.bind()
};
```

2. **Filtering Irrelevant Updates**: Skip processing when data hasn't changed

```typescript
export const filteredStore = {
  subscribe: dataStore.stream()
    .map(data => {
      // Only include items that match the filter
      return Object.entries(data)
        .filter(([_, item]) => item.active)
        .reduce((acc, [key, value]) => {
          acc[key] = value;
          return acc;
        }, {});
    })
    .on.bind()
};
```

3. **Lazy Loading**: Only load data when needed

```typescript
function createLazyStore(pathFn) {
  let currentPath = null;
  let currentNode = null;
  
  const triggerStream = GunSubscription.of(null);
  
  return {
    subscribe: (callback) => {
      return triggerStream
        .switchMap(trigger => {
          if (!currentPath) return GunSubscription.of(null);
          if (!currentNode) {
            currentNode = new GunNode(currentPath);
          }
          return currentNode.stream();
        })
        .on(callback);
    },
    
    // Set the path and load data
    load: (params) => {
      const newPath = pathFn(params);
      if (JSON.stringify(newPath) !== JSON.stringify(currentPath)) {
        currentPath = newPath;
        currentNode = new GunNode(currentPath);
        triggerStream.on(() => {}); // Force an update
      }
    }
  };
}

// Usage: 
const userProfileStore = createLazyStore(userId => ['users', userId, 'profile']);

// Later, load a specific user:
userProfileStore.load('user123');
```

## Syncing with External State

Our reactive approach makes it easy to sync Gun data with external state sources:

```typescript
// Sync with localStorage
function createPersistedStore(path, storageKey) {
  const node = new GunNode(Array.isArray(path) ? path : [path]);
  
  // Load initial state from localStorage
  const savedData = localStorage.getItem(storageKey);
  let initialData = {};
  
  if (savedData) {
    try {
      initialData = JSON.parse(savedData);
      // Also put the saved data into Gun
      node.put(initialData);
    } catch (e) {
      console.error("Failed to parse saved data:", e);
    }
  }
  
  // Create the stream
  const stream = node.stream().startWith(initialData);
  
  // Save to localStorage when Gun data changes
  stream.on(data => {
    localStorage.setItem(storageKey, JSON.stringify(data));
  });
  
  return {
    subscribe: stream.on.bind(stream),
    put: (value) => node.put(value),
    reset: () => {
      localStorage.removeItem(storageKey);
      node.put(null);
    }
  };
}

export const settingsStore = createPersistedStore("settings", "app_settings");
```

## Conclusion

By combining Gun's real-time database capabilities with reactive programming patterns, we've created a powerful store pattern that offers:

1. **Better Composability**: Easily combine, transform, and filter data streams
2. **Cleaner Code**: Avoid nested callback hell with declarative transformations
3. **Performance Optimizations**: Control exactly when and how updates flow through your app
4. **Integrated Resource Management**: Automatically clean up subscriptions to prevent memory leaks
5. **Svelte Integration**: Seamlessly work with Svelte's reactive programming model

This approach transforms Gun from a simple real-time database into a comprehensive state management solution that can handle complex data flows in decentralized applications. 