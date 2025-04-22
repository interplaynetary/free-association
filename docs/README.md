# Decentralized Real-Time App Stack with Gun.js and Svelte

This comprehensive guide provides an overview of our decentralized, real-time application architecture based on Gun.js and Svelte. The stack is designed to be modular, maintainable, and scalable, following best practices for both libraries.

## What is Gun.js?

[Gun.js](https://gun.eco/) is a real-time, distributed, offline-first, graph database engine that runs in both browsers and servers. Key features include:

- **Decentralized Architecture**: Data is stored across peers with no central point of failure
- **Real-Time Synchronization**: Changes propagate to all connected peers automatically
- **Offline-First**: Applications work without internet connectivity, syncing when reconnected
- **Graph Database**: Flexible data model for complex relationships
- **Security**: Built-in encryption and user authentication through SEA (Security, Encryption, Authentication)

## Why Svelte?

[Svelte](https://svelte.dev/) is a radical new approach to building user interfaces that offers:

- **No Virtual DOM**: Compiles applications to highly efficient vanilla JavaScript
- **True Reactivity**: Fine-grained reactivity without complex state management libraries
- **Less Boilerplate**: Intuitive syntax with minimal code
- **Seamless Transitions**: Built-in animation and transition system
- **Small Bundle Size**: Smaller, faster applications compared to other frameworks

Our integration creates a powerful combination of decentralized data with a performant, reactive UI layer.

## Architecture Overview

Our architecture follows a layered approach:

1. **Infrastructure Layer** - Core Gun configuration and setup
2. **Data Access Layer** - Reactive store wrappers and data models
3. **Connection Management Layer** - Peer connection and network status
4. **Application Layer** - Business logic and UI components

### Key Components

```
free/
├── src/
│   ├── utils/
│   │   ├── gun/
│   │   │   ├── index.ts                # Core exports
│   │   │   ├── gunSetup.ts             # Gun initialization
│   │   │   ├── GunNode.ts              # OOP wrapper for Gun nodes
│   │   │   ├── GunSubscription.ts      # Subscription management
│   │   │   ├── reactiveStores.ts       # Svelte store adapters
│   │   │   └── connectionManager.ts     # Network management
│   ├── stores/
│   │   └── todo.ts                     # Application-specific stores
│   ├── components/
│   │   └── ConnectionStatus.svelte     # Connection UI component
│   └── lib/
│       └── Todo.svelte                 # Example application component
└── server.ts                           # Gun relay server
```

### Architecture Diagram

```
┌────────────────────────────────────────────────────────────────┐
│                        Application Layer                        │
│  ┌────────────────┐  ┌─────────────────┐  ┌────────────────┐   │
│  │  Components    │  │  Page Views     │  │  App Logic     │   │
│  └────────────────┘  └─────────────────┘  └────────────────┘   │
├────────────────────────────────────────────────────────────────┤
│                        Store Layer                              │
│  ┌────────────────┐  ┌─────────────────┐  ┌────────────────┐   │
│  │  Custom Stores │  │  Derived Stores │  │  Gun Adapters  │   │
│  └────────────────┘  └─────────────────┘  └────────────────┘   │
├────────────────────────────────────────────────────────────────┤
│                        Data Access Layer                        │
│  ┌────────────────┐  ┌─────────────────┐  ┌────────────────┐   │
│  │    GunNode     │  │ GunSubscription │  │ Reactive Stores│   │
│  └────────────────┘  └─────────────────┘  └────────────────┘   │
├────────────────────────────────────────────────────────────────┤
│                     Infrastructure Layer                        │
│  ┌────────────────┐  ┌─────────────────┐  ┌────────────────┐   │
│  │  Gun Instance  │  │ Connection Mgmt │  │  SEA Auth      │   │
│  └────────────────┘  └─────────────────┘  └────────────────┘   │
└────────────────────────────────────────────────────────────────┘
             ▲                   ▲                    ▲
             │                   │                    │
             ▼                   ▼                    ▼
    ┌─────────────────┐ ┌─────────────────┐ ┌──────────────────┐
    │  Browser Storage│ │  Gun Relay Peers│ │  Other Gun Apps  │
    └─────────────────┘ └─────────────────┘ └──────────────────┘
```

## Core Features

1. **Decentralized Data Storage** - Data persists across peers with Gun.js
2. **Real-Time Synchronization** - Changes propagate automatically between clients
3. **Reactive Programming** - Svelte stores integrate seamlessly with Gun data
4. **Connection Management** - Built-in network status monitoring and recovery
5. **Modular Architecture** - Clean separation of concerns for maintainability

## Getting Started

### 1. Setting Up a Local Relay

We provide a Gun relay server that works with both Node.js and Bun:

```javascript
// start-relay.js
import relay from "@gun-vue/relay";

relay.init({
  host: "localhost",
  port: 8765,
  store: true, // Enable persistent storage
  path: "public",
});
```

Run with:

```bash
node start-relay.js
```

### 2. Gun Setup & Configuration

Our `gunSetup.ts` provides a configured Gun instance with optimizations:

```typescript
// Core Gun initialization
export const gun = Gun({
  peers: [
    "http://localhost:8765/gun", // Local relay peer
  ],
  localStorage: true, // Enable browser persistence
});
```

### 3. Application State Management

#### Creating a Collection Store

```typescript
// stores/todo.ts
import { createCollectionStore } from "../utils/gun";

// Define item interface
export interface Todo {
  title: string;
  done: boolean;
  created: number;
}

// Create a collection store with sorting
export const todos = createCollectionStore<Todo>(
  "todos",
  (a, b) => (b[1].created || 0) - (a[1].created || 0)
);

// Add helper methods
todos.toggle = (id: string, currentState: boolean) => {
  todos.update(id, { done: !currentState });
};
```

#### Derived Stores

```typescript
// Create filtered and aggregated stores
export const completedTodos = filterCollectionStore(
  todos,
  (todo) => todo.done === true
);

export const completedCount = aggregateCollection(
  todos,
  (items) => items.filter(([_, todo]) => todo.done).length
);
```

### 4. Connection Management

Our dedicated ConnectionManager provides network status monitoring:

```typescript
// Components can subscribe to connection status
import { connectionStatus } from "../utils/gun";

// Reactive access to connection state
$: isConnected = $connectionStatus.connected;
$: statusText = $connectionStatus.statusText;
$: peerCount = $connectionStatus.peerCount;

// Connection can be monitored and manual reconnection triggered
import { reconnectToPeers } from "../utils/gun";

function attemptReconnect() {
  reconnectToPeers();
}
```

### 5. Component Usage

A Todo component example:

```svelte
<script lang="ts">
  import { todos, completedCount } from '../stores/todo';
  import ConnectionStatus from '../components/ConnectionStatus.svelte';

  let newTodo = '';

  function addTodo() {
    if (newTodo.trim()) {
      todos.add({
        title: newTodo,
        done: false,
        created: Date.now()
      });
      newTodo = '';
    }
  }
</script>

<div class="todo-app">
  <header>
    <h1>GUN Todo App</h1>
    <ConnectionStatus />
  </header>

  <form on:submit|preventDefault={addTodo}>
    <input bind:value={newTodo} placeholder="What needs to be done?">
    <button type="submit">Add</button>
  </form>

  {#if $todos?.length === 0}
    <p>No todos yet!</p>
  {:else}
    <ul>
      {#each $todos as [id, todo] (id)}
        <li>
          <input
            type="checkbox"
            checked={todo.done}
            on:change={() => todos.toggle(id, todo.done)}
          >
          <span class:completed={todo.done}>{todo.title}</span>
          <button onclick={() => todos.remove(id)}>Delete</button>
        </li>
      {/each}
    </ul>
  {/if}

  <div>Completed: {$completedCount}</div>
</div>
```

### Asynchronous Reactive Composition

Our architecture supports complex asynchronous transformations through the GunSubscription system:

```typescript
// Create a subscription that processes values asynchronously
const processedSubscription = userNode.stream().map(async (userData) => {
  // Perform async operations like API calls or heavy computation
  const enrichedData = await fetchAdditionalData(userData.id);
  return { ...userData, ...enrichedData };
});

// Subscribe to the processed data stream
const cleanup = processedSubscription.on((data) => {
  console.log("Processed data:", data);
});
```

Key capabilities include:

1. **Async Transformation**: The `.map()` operator fully supports async functions and Promise returns
2. **Race Condition Handling**: Asynchronous operations are properly sequenced to prevent out-of-order updates
3. **Error Boundary**: Failed async operations are gracefully handled without breaking the subscription chain
4. **Cancellation**: All async operations properly clean up when subscriptions are terminated

Combine these with our reactive store system for powerful async workflows:

```typescript
// Create a store with async transformation
function createEnrichedUserStore(userId: string) {
  const baseStore = createGunStore(["users", userId]);

  return mapStore(baseStore, async (userData) => {
    if (!userData) return null;

    // Async data enrichment
    const [friends, activity] = await Promise.all([
      fetchUserFriends(userId),
      fetchUserActivity(userId),
    ]);

    return {
      ...userData,
      friends,
      recentActivity: activity,
    };
  });
}
```

## Advanced Usage

### 1. Custom Gun Node Operations

For advanced operations, use the `GunNode` class:

```typescript
import { GunNode } from "../utils/gun";

// Create a reference to a node
const userNode = new GunNode(["users", userId]);

// Read data once
const userData = await userNode.once();

// Subscribe to changes
const unsubscribe = userNode.on((data) => {
  console.log("User data updated:", data);
});

// Update data
userNode.put({ name: "New Name" });

// Deep query with reference resolution
const deepData = await userNode.deepGet(2);
```

### 2. Working with User Authentication

```typescript
import { user, authenticate, logout, recallUser } from "../utils/gun";

// Auto-login from session
await recallUser();

// Register/login
try {
  await authenticate("username", "password");
  console.log("Logged in as:", user.is?.alias);
} catch (err) {
  console.error("Authentication failed:", err);
}

// Check authentication state
if (user.is) {
  console.log("User is authenticated:", user.is.pub);
}

// Logout
logout();
```

### 3. Certificate Management for Data Access Control

```typescript
import { createCertificate, CertificatePermission } from "../utils/gun";

// Grant read-only access to a specific path
const certificate = await createCertificate(
  targetUserPubKey,
  ["shared/todos"],
  CertificatePermission.READ
);

// Grant temporary write access (expires in 1 hour)
const tempCertificate = await createCertificate(
  targetUserPubKey,
  ["collab/docs"],
  CertificatePermission.WRITE,
  3600000 // 1 hour in milliseconds
);

// Store certificate for reuse
await storeCertificate("my-certificate", certificate, targetUserPubKey);

// Retrieve stored certificate
const storedCert = await getCertificate("my-certificate");
```

### 4. Creating Custom Reactive Stores

```typescript
import { mapStore, combineStores } from "../utils/gun";

// Transform store values
const userDisplayStore = mapStore(userStore, (user) => ({
  displayName: user.name || user.username,
  avatarUrl: user.avatar || "/default-avatar.png",
}));

// Combine multiple stores
const combinedDataStore = combineStores(
  profileStore,
  settingsStore,
  (profile, settings) => ({
    ...profile,
    preferences: settings.preferences,
  })
);
```

### 5. Deep Reference Resolution

Gun uses references to link data nodes. Our stack provides tools to traverse and resolve these references:

```typescript
// Get deep data with automatic reference resolution
const node = new GunNode(["users", userId]);

// Resolve references up to 2 levels deep
const userData = await node.deepGet(2);

// Create a reactive subscription with deep resolution
const deepSubscription = node.deepStream(3);
const unsubscribe = deepSubscription.on((data) => {
  console.log("Deep data updated:", data);
});
```

### 6. Working with GunSubscription API

The GunSubscription class provides a powerful stream-like API:

```typescript
import { GunSubscription } from "../utils/gun";

// Create a subscription
const subscription = new GunSubscription(["users", userId]);

// Transform values with map
const nameSubscription = subscription.map((user) => user?.name || "Anonymous");

// Combine with another subscription
const combinedSub = subscription.combine(
  otherSubscription,
  (user, settings) => ({
    ...user,
    theme: settings?.theme || "default",
  })
);

// Debounce rapid updates
const debouncedSub = subscription.debounce(300);

// Chain operations
const processedSub = subscription
  .map((data) => processData(data))
  .debounce(200)
  .startWith(defaultValue);
```

## Best Practices

1. **Separation of Concerns**

   - Keep Gun setup separate from application logic
   - Use data stores as intermediaries between UI and Gun
   - Create dedicated components for connection management

2. **Error Handling**

   - Always wrap Gun operations in try/catch blocks
   - Provide fallbacks for network disconnections
   - Use timeouts for operations that might hang

3. **Performance Optimization**

   - Use debounced stores for high-frequency updates
   - Implement cleanup for subscriptions when components unmount
   - Leverage the `GunSubscription` class for subscription management

4. **Security**

   - Use certificate-based access control for sensitive data
   - Never trust client-side validation for data integrity
   - Properly handle user authentication and session management

5. **Data Modeling Best Practices**

   - Keep data structures shallow and denormalized
   - Use references for relationships between data
   - Add timestamps for conflict resolution
   - Include unique identifiers for all nodes

6. **Offline-First Development**
   - Design UI to show sync status clearly
   - Make optimistic updates with eventual consistency in mind
   - Cache critical data for offline operation
   - Handle reconnection gracefully

## Common Patterns

### Handling Collections

```typescript
// Create a collection with filtering capability
function createFilterableCollection<T>(
  path: string,
  sortFn?: (a: [string, T], b: [string, T]) => number
) {
  const collection = createCollectionStore<T>(path, sortFn);

  // Create a function to get filtered views of the collection
  const getFiltered = (filterFn: (item: T) => boolean) =>
    filterCollectionStore(collection, filterFn);

  return {
    ...collection,
    getFiltered,
  };
}

// Usage
const users = createFilterableCollection<User>("users");
const activeUsers = users.getFiltered((user) => user.status === "active");
```

### User Profiles with Authentication

```typescript
// stores/userProfile.ts
import { user, getPath } from "../utils/gun";
import { derived } from "svelte/store";
import { createGunStore } from "../utils/gun";

// Create a store that automatically points to current user's profile
export function createUserProfileStore() {
  // Create a readable store from the authentication state
  const authStore = {
    subscribe: (callback) => {
      // Initial state
      callback({
        isAuthenticated: !!user.is?.pub,
        pub: user.is?.pub || null,
      });

      // Listen for auth events
      const authHandler = () => {
        callback({
          isAuthenticated: !!user.is?.pub,
          pub: user.is?.pub || null,
        });
      };

      // Listen for both auth and unauth events
      gun.on("auth", authHandler);

      // Cleanup
      return () => {
        gun.off("auth", authHandler);
      };
    },
  };

  // Create a derived store for the profile
  return derived(
    authStore,
    ($auth, set) => {
      if (!$auth.isAuthenticated) {
        set(null);
        return;
      }

      // Create a profile store for the authenticated user
      const profileStore = createGunStore(["users", $auth.pub, "profile"]);

      // Subscribe to the profile
      const unsubscribe = profileStore.subscribe((profile) => {
        set(profile);
      });

      return unsubscribe;
    },
    null // Initial value
  );
}
```

## Troubleshooting

### Connection Issues

If you experience connection problems:

1. Check that the relay server is running
2. Verify the peer URL in gunSetup.ts is correct
3. Use the ConnectionStatus component to monitor peer connections
4. Try manual reconnection with reconnectToPeers()
5. Check browser network tab for pending requests
6. Verify WebSocket connections in the network monitor
7. Ensure your firewall isn't blocking WebSocket connections

### Data Synchronization Issues

If data isn't syncing properly:

1. Ensure data is properly structured for Gun
2. Check for null or undefined values in your data
3. Verify that peer connections are established
4. Use Gun.log() to debug data flow
5. Validate data paths and references
6. Ensure certificates are valid for write operations
7. Check browser localStorage for corruption
8. Verify that data isn't being filtered by middleware

### Memory Management

To prevent memory leaks:

1. Always unsubscribe from Gun data when components unmount
2. Use the provided cleanup functions from subscription methods
3. Implement proper destruction in component lifecycle methods
4. Avoid creating new subscriptions in reactive statements
5. Use cleanup functions in onDestroy lifecycle hooks
6. Watch for cascading subscriptions in deeply nested components

### Gun-Specific Issues

1. **Circular Reference Detection**: Gun sometimes struggles with circular references

   - Solution: Avoid circular references in your data model
   - Use separate nodes with cross-references instead

2. **Node Deletion**: Gun uses null values to mark deleted nodes

   - Solution: Use the correct `.put(null)` pattern and check for nulls in subscriptions
   - Be aware that deletes sometimes require time to propagate

3. **Race Conditions**: Concurrent updates can lead to unexpected results
   - Solution: Use timestamps in your data and conflict resolution strategies
   - Consider using Gun's `.later()` method for ordered operations

## Implementation Example

A complete implementation of a user profile component:

```typescript
// stores/profile.ts
import { GunNode, createGunStore } from "../utils/gun";

export interface Profile {
  name: string;
  bio: string;
  avatar: string;
  lastUpdated: number;
}

// Create store for the current user's profile
export function createProfileStore(userId: string) {
  const profileNode = new GunNode(["users", userId, "profile"]);

  const profileStore = createGunStore<Profile>(["users", userId, "profile"], {
    name: "",
    bio: "",
    avatar: "",
    lastUpdated: 0,
  });

  return {
    ...profileStore,
    updateProfile: (updates: Partial<Profile>) => {
      profileNode.put({
        ...updates,
        lastUpdated: Date.now(),
      });
    },
  };
}
```

```svelte
<!-- components/UserProfile.svelte -->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { createProfileStore } from '../stores/profile';
  import { connectionStatus } from '../utils/gun';
  import ConnectionStatus from './ConnectionStatus.svelte';

  export let userId: string;

  // Create profile store when component mounts
  let profileStore = createProfileStore(userId);
  let editMode = false;
  let formData = {
    name: '',
    bio: ''
  };

  // Initialize form data when profile data loads
  $: if ($profileStore) {
    formData = {
      name: $profileStore.name || '',
      bio: $profileStore.bio || ''
    };
  }

  function saveProfile() {
    profileStore.updateProfile(formData);
    editMode = false;
  }
</script>

<div class="profile-container">
  <header>
    <h2>User Profile</h2>
    <ConnectionStatus />
  </header>

  {#if !$connectionStatus.connected}
    <div class="offline-warning">
      You're offline. Changes will sync when reconnected.
    </div>
  {/if}

  {#if !editMode}
    <div class="profile-view">
      <img src={$profileStore.avatar || '/default-avatar.png'} alt="Profile Avatar">
      <h3>{$profileStore.name || 'Unknown User'}</h3>
      <p>{$profileStore.bio || 'No bio provided'}</p>
      <button onclick={() => editMode = true}>Edit Profile</button>
    </div>
  {:else}
    <form on:submit|preventDefault={saveProfile}>
      <div class="form-group">
        <label for="name">Name</label>
        <input id="name" bind:value={formData.name} required>
      </div>

      <div class="form-group">
        <label for="bio">Bio</label>
        <textarea id="bio" bind:value={formData.bio}></textarea>
      </div>

      <div class="form-actions">
        <button type="button" onclick={() => editMode = false}>Cancel</button>
        <button type="submit">Save Profile</button>
      </div>
    </form>
  {/if}

  <footer>
    <small>Last updated: {$profileStore.lastUpdated ? new Date($profileStore.lastUpdated).toLocaleString() : 'Never'}</small>
  </footer>
</div>
```

## Advanced Topics

### Building Multi-User Applications

For collaborative applications, you'll need to combine authentication, certificates, and shared data spaces:

```typescript
// Create a shared document with access control
export function createSharedDocument(docId: string) {
  // Create the document node
  const docNode = new GunNode(["documents", docId]);

  // Create a store for the document
  const docStore = createGunStore(["documents", docId]);

  return {
    ...docStore,
    // Update document contents
    updateContent: async (content: string) => {
      docNode.put({ content, updatedAt: Date.now() });
    },
    // Share with another user
    shareWithUser: async (
      userPub: string,
      permission: CertificatePermission
    ) => {
      const cert = await createCertificate(
        userPub,
        [`documents/${docId}`],
        permission
      );

      if (cert) {
        // Store the certificate
        await storeCertificate(`share_${docId}_${userPub}`, cert, userPub);

        // Add to shared users list
        const sharesNode = docNode.get("shares");
        sharesNode.get(userPub).put({
          pub: userPub,
          permission: permission,
          sharedAt: Date.now(),
        });
      }

      return !!cert;
    },
  };
}
```

### Scaling with Gun

As your application grows, consider these scaling strategies:

1. **Distribute Load**: Run multiple relay peers for redundancy
2. **Optimize Data Access**: Use shallow paths and references
3. **Data Partitioning**: Split data into logical domains
4. **Selective Syncing**: Only sync data the user needs
5. **Background Processing**: Use web workers for computationally intensive tasks
6. **Service Workers**: Implement caching and offline strategies
7. **Lazy Loading**: Only load data when needed

## Conclusion

This architecture provides a solid foundation for building decentralized, real-time applications with Gun.js and Svelte. By following the patterns and practices outlined in this guide, developers can create maintainable, scalable applications that leverage the power of distributed data while providing a responsive user experience.

The modular design allows for easy extension and customization while maintaining a clean separation of concerns. Whether building a simple todo app or a complex collaborative platform, these patterns can scale to meet the needs of the application.

## Contributing

We welcome contributions to improve this stack:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgements

- The [Gun.js team](https://gun.eco/) for creating an amazing decentralized database
- The [Svelte team](https://svelte.dev/) for their revolutionary approach to UI
- All contributors who have helped shape and improve this architecture
