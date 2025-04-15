# Using Gun with Svelte

This guide explains the recommended patterns for using Gun with Svelte in our application.

## Two Integration Approaches

We provide two complementary approaches for integrating Gun with Svelte:

1. **Direct Gun Chain** - Simple, lightweight, ideal for basic data binding
2. **GunNode Wrapper** - Type-safe, structured, ideal for complex operations

## Approach 1: Direct Gun Chain

The Gun chain has been extended with a `.subscribe()` method that makes it compatible with Svelte's reactivity system. This allows you to use Gun nodes directly as Svelte stores.

### Basic Usage:

```svelte
<script>
  import { gun } from '../utils/gun/gunSetup';
  
  // Get a Gun node to use as a Svelte store
  const user = gun.get('users').get('alice');
  
  // Modify data
  function updateName(newName) {
    user.get('name').put(newName);
  }
</script>

<h1>Hello, {$user?.name || 'Anonymous'}</h1>
<button on:click={() => updateName('Alice Smith')}>
  Update Name
</button>
```

### Benefits:

- Minimal code
- No wrapper objects required
- Direct reactivity in templates with `$` notation
- Simplest approach for basic data binding

### When to use:

- For simple data binding in Svelte components
- When you don't need complex operations or type safety
- For read-only displays of Gun data

## Approach 2: GunNode Wrapper

For more complex scenarios, we provide the `GunNode` class that adds type safety, structured operations, and domain-specific methods.

### Basic Usage:

```svelte
<script>
  import { UserNode } from '../models/UserNode';
  
  // Create a typed node wrapper
  const userNode = new UserNode('alice');
  
  // Get the raw Gun chain for Svelte binding
  const user = userNode.toStore();
  
  // Use typed methods for complex operations
  function updateProfile() {
    userNode.updateProfile('Alice Smith', 'alice@example.com')
      .then(() => console.log('Profile updated'))
      .catch(err => console.error('Update failed:', err));
  }
</script>

<h1>Hello, {$user?.name || 'Anonymous'}</h1>
<p>Email: {$user?.email || 'No email provided'}</p>
<button on:click={updateProfile}>
  Update Profile
</button>
```

### Benefits:

- Type safety with TypeScript
- Domain-specific methods
- Better error handling
- Structured data operations
- Support for complex scenarios like certificate-based access control

### When to use:

- For complex data operations
- When type safety is important
- When you need domain-specific methods
- For handling authentication and certificates
- When working with complex data relationships

## Combining Both Approaches

Often, the best solution is to combine both approaches:

```svelte
<script>
  import { gun } from '../utils/gun/gunSetup';
  import { UserNode } from '../models/UserNode';
  
  // For simple direct access
  const allUsers = gun.get('users');
  
  // For complex operations
  const currentUser = new UserNode('alice');
  
  // Use the raw Gun chain for Svelte binding
  const userStore = currentUser.toStore();
  
  // Use typed methods for complex operations
  function updateProfile() {
    currentUser.updateProfile('Alice Smith', 'alice@example.com');
  }
</script>

<!-- Direct binding for simple list -->
<ul>
  {#each $allUsers as [id, user]}
    <li>{user.name}</li>
  {/each}
</ul>

<!-- Binding through toStore() -->
<h1>Current user: {$userStore?.name}</h1>
<button on:click={updateProfile}>Update Profile</button>
```

## Best Practices

1. **Prefer direct Gun chains for simple bindings**
   ```svelte
   const user = gun.get('users').get('alice');
   // Use in template as: {$user.name}
   ```

2. **Use GunNode for complex operations**
   ```typescript
   const user = new UserNode('alice');
   await user.updateProfile('Alice', 'alice@example.com');
   ```

3. **Combine both with toStore() when needed**
   ```typescript
   const user = new UserNode('alice');
   const userStore = user.toStore();
   // Use in template as: {$userStore.name}
   ```

4. **Use customStore for advanced reactivity**
   ```typescript
   import { customStore } from '../utils/gun/gunSetup';
   
   const userStore = customStore(gun.get('users').get('alice'), {
     updateName: (name) => gun.get('users').get('alice').get('name').put(name)
   });
   
   // Use methods: userStore.updateName('Alice')
   // Use in template: {$userStore.name}
   ```

## Performance Considerations

- Direct Gun chains have the least overhead
- GunNode adds a small layer of abstraction
- Both approaches are optimized for Svelte's reactivity system
- For large collections, be careful with `.map()` as it can load a lot of data
- Use optimized methods like GunNode's `each()` for iterating over collections 