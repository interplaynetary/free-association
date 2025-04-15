# Enhanced GUN Utilities

This directory contains enhanced utilities for working with GUN, a decentralized graph database. These utilities build on top of GUN's native API to provide more structured, type-safe, and developer-friendly interfaces.

## Core Components

### 1. gunSetup.ts

Central configuration and setup for GUN. Contains:

- Basic GUN initialization
- User authentication helpers
- Type definitions for GUN data structures
- Certificate management utilities
- Svelte compatibility integration

### 2. GunNode.ts

A wrapper around GUN nodes that provides a consistent API for working with data:

- Type-safe put/get operations
- Certificate-based access control
- Deep reference traversal (resolves soul references automatically)
- Reactive subscription patterns
- Promise-based interfaces with proper error handling

### 3. GunSubscription.ts

A stream-based subscription system for GUN data:

- Clean, reliable subscription management
- Proper clean-up of resources
- Transformation and mapping of data streams
- Collection iteration utilities

## New Features

### Deep Reference Traversal

GUN's graph database relies heavily on references (souls) between nodes. Our implementation provides automatic traversal of these references:

```typescript
// Regular GUN get only fetches the reference, not the data it points to:
const user = await userNode.once();
// { profile: { '#': 'someProfileSoul' } }

// Deep get automatically resolves references to any depth:
const userWithProfile = await userNode.deepGet(1);
// { profile: { name: 'Alice', bio: 'Developer' } }

// Reactive subscriptions with automatic reference resolution:
userNode.deepOn(data => {
  // Data includes all resolved references
  console.log(data.profile.name);
}, 2); // Specify depth of reference resolution
```

### Certificate Management

Manage write access to protected nodes through SEA certificates:

```typescript
// Create a certificate for a user
const certificate = await createCertificate(
  bobPubKey,         // Public key of the grantee
  ['shared/notes'],  // Paths they can access
  CertificatePermission.WRITE,
  3600000            // Expiration time in ms (1 hour)
);

// Store certificate for later use
await storeCertificate('bob-notes-access', certificate, bobPubKey);

// Retrieve a stored certificate
const cert = await getCertificate('bob-notes-access');

// Use a certificate to access protected nodes
const sharedNode = new GunNode(['~alice', 'shared'], certificate);
await sharedNode.get('notes').put({ text: 'Hello!' });
```

### Convenience Certificate Presets

Common certificate patterns are available as presets:

```typescript
// Create a read-only certificate
const readCert = await certificatePresets.readOnly(userPub, 'path');

// Create a write-only certificate
const writeCert = await certificatePresets.writeOnly(userPub, 'path');

// Create a temporary certificate (expires after 1 hour)
const tempCert = await certificatePresets.temporary(userPub, 'path', 3600000);

// Create a full access certificate
const fullCert = await certificatePresets.fullAccess(userPub, 'path');
```

## Usage Examples

### Basic Node Operations

```typescript
// Create a node
const userNode = new GunNode(['users', 'alice']);

// Write data
await userNode.put({ name: 'Alice', email: 'alice@example.com' });

// Read data
const userData = await userNode.once();

// Subscribe to updates
const unsubscribe = userNode.on(data => {
  console.log('User updated:', data);
});

// Clean up when done
unsubscribe();
```

### Collection Operations

```typescript
// Create a collection node
const usersNode = new GunNode(['users']);

// Get a specific item
const alice = usersNode.get('alice');

// Iterate over all items
usersNode.each(user => {
  console.log(`User: ${user._key}`, user);
});

// Get a stream of values
const stream = usersNode.stream().each();
const reader = stream.subscribe().getReader();

// Process values from the stream
while (true) {
  const { value, done } = await reader.read();
  if (done) break;
  console.log('User:', value);
}
```

### Deep Get with Nested References

```typescript
// Set up related nodes
const userNode = new GunNode(['users', 'alice']);
const profileNode = new GunNode(['profiles', 'alice-profile']);

// Store profile data
await profileNode.put({
  name: 'Alice',
  bio: 'Developer',
  skills: ['JavaScript', 'TypeScript']
});

// Get the soul of the profile node
const profileSoul = await profileNode.getSoul();

// Store a reference to the profile in the user node
await userNode.get('profile').put({ '#': profileSoul });

// Get user data with resolved profile reference
const user = await userNode.deepGet(1);
console.log(user.profile.name); // 'Alice'
```

### Custom Domain Modeling

Extend GunNode to create domain-specific node types:

```typescript
class UserNode extends GunNode<{name: string, email: string}> {
  constructor(userId: string) {
    super(['users', userId]);
  }
  
  // Add domain-specific methods
  async updateProfile(name: string, email: string): Promise<void> {
    await this.put({ name, email });
  }
  
  // Create relationships with other node types
  profile() {
    return new ProfileNode(this.getPath().concat(['profile']));
  }
}

// Usage
const alice = new UserNode('alice');
await alice.updateProfile('Alice Smith', 'alice@example.com');
```

## Best Practices

1. **Use the streaming mindset**: Always think in terms of data streams, not just discrete values.

2. **Structure your data efficiently**: Use references for relationships between nodes.

3. **Leverage certificate patterns**: Use certificates for multi-user access instead of switching authentication.

4. **Close subscriptions properly**: Always clean up subscriptions when they're no longer needed.

5. **Use domain-specific extensions**: Extend GunNode to model your specific domain entities and relationships.

## Further Resources

- GUN documentation: https://gun.eco/docs/
- SEA API documentation: https://gun.eco/docs/SEA
- Svelte integration: https://gun.eco/docs/Svelte 