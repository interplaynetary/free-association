##

 Capability Revocation Guide

Complete guide to the production-ready revocation system for RPC capabilities.

## 🎯 Overview

This revocation system implements all security best practices:

- ✅ **Revocable by default** - All capabilities can be revoked
- ✅ **Time-based expiration** - Automatic expiration (default 24h)
- ✅ **Permission-based** - Fine-grained access control
- ✅ **Audit trail** - Complete logging of all operations
- ✅ **Automatic cleanup** - Expired capabilities cleaned up automatically
- ✅ **Membrane pattern** - Domain-level revocation
- ✅ **Least privilege** - Issue minimum necessary permissions

## 📚 Quick Start

### Basic Usage

```typescript
import { secureRpcManager, initializeSecureAllocationStores } from '$lib/protocol/stores-rpc-secure.svelte';

// 1. Initialize (once on startup)
initializeSecureAllocationStores();

// 2. Issue a revocable capability
const capability = secureRpcManager.issueCommitmentCapability(
  alicePubKey,
  {
    recipientId: alicePubKey,
    expiresInMs: 24 * 60 * 60 * 1000, // 24 hours
    permissions: ['getCommitment', 'getNeedSlots'] // Optional: restrict methods
  }
);

// 3. Give to Alice
await alice.hereIsYourAccess(capability);

// 4. Later: Revoke access
secureRpcManager.revokeAllFor(alicePubKey, 'Access revoked by admin');
```

### With Presets

```typescript
import {
  PRESET_TEMPORARY,  // 1 hour
  PRESET_SHORT_LIVED, // 5 minutes
  PRESET_DAILY,      // 24 hours
  PRESET_READ_ONLY   // Read-only methods
} from '$lib/protocol/stores-rpc-secure.svelte';

// Temporary access (1 hour)
const tempAccess = secureRpcManager.issueCommitmentCapability(
  bobPubKey,
  PRESET_TEMPORARY
);

// Read-only access (24 hours)
const readOnlyAccess = secureRpcManager.issueCommitmentCapability(
  carolPubKey,
  PRESET_READ_ONLY(['getCommitment', 'getNeedSlots', 'getCapacitySlots'])
);

// Or use the convenience method
const readOnly = secureRpcManager.issueReadOnlyCommitmentAccess(carolPubKey);
```

## 🔐 Authentication Pattern

The recommended way to use revocation with authentication:

### Server Side

```typescript
import { AuthenticatedCommitmentApi } from '$lib/protocol/stores-rpc-secure.svelte';
import { newWorkersRpcResponse } from 'capnweb';

// Create authenticated API (capability 0)
const authApi = new AuthenticatedCommitmentApi(secureRpcManager);

export default {
  fetch(request, env, ctx) {
    if (url.pathname === '/rpc/auth') {
      // Expose authentication endpoint
      return newWorkersRpcResponse(request, authApi);
    }
  }
}
```

### Client Side

```typescript
import { newWebSocketRpcSession } from 'capnweb';

// Connect to auth endpoint (capability 0)
const authApi = newWebSocketRpcSession('wss://server.com/rpc/auth');

// Authenticate to get a revocable capability
const myCommitmentAccess = await authApi.authenticate(
  myPubKey,
  mySignature,
  60 * 60 * 1000 // 1 hour
);

// Use the authenticated capability
const myCommitment = await myCommitmentAccess.getCommitment();
const myNeeds = await myCommitmentAccess.getNeedSlots();

// After 1 hour, this will throw: "Capability expired"
```

## 🎭 Revocation Patterns

### Pattern 1: Individual Revocation

Revoke a specific capability by ID:

```typescript
// Issue capability and get ID
const capability = secureRpcManager.issueCommitmentCapability(bobPubKey);
const metadata = capability.getMetadata();
const capabilityId = metadata.id;

// Later: Revoke by ID
secureRpcManager.myCommitmentManager.revoke(capabilityId, 'User requested revocation');
```

### Pattern 2: Recipient Revocation

Revoke all capabilities for a specific user:

```typescript
// Bob has 3 different capabilities
const cap1 = secureRpcManager.issueCommitmentCapability(bobPubKey);
const cap2 = secureRpcManager.issueReadOnlyCommitmentAccess(bobPubKey);
const cap3 = secureRpcManager.issueTreeCapability(bobPubKey);

// Later: Revoke ALL of Bob's capabilities
const count = secureRpcManager.revokeAllFor(bobPubKey, 'User violated terms');
console.log(`Revoked ${count} capabilities`); // 3
```

### Pattern 3: Emergency Revocation

Revoke everything (emergency use):

```typescript
// Nuclear option: Revoke ALL issued capabilities
const count = secureRpcManager.revokeAll('Security incident detected');
console.log(`Revoked ${count} capabilities`);
```

### Pattern 4: Session Revocation (Membrane)

Revoke an entire session at once:

```typescript
// Create a session membrane
const sessionId = crypto.randomUUID();
const membrane = secureRpcManager.createSessionMembrane(sessionId, 'Alice Session');

// Issue capabilities within the session
const commitment = membrane.wrap(
  secureRpcManager.issueCommitmentCapability(alicePubKey)
);
const tree = membrane.wrap(
  secureRpcManager.issueTreeCapability(alicePubKey)
);

// Give Alice both capabilities
await alice.sessionCapabilities({ commitment, tree });

// Later: Revoke the ENTIRE session
secureRpcManager.revokeSession(sessionId, 'Session timeout');
// Both commitment AND tree capabilities are now revoked!
```

## 📊 Monitoring & Auditing

### Get Statistics

```typescript
const stats = secureRpcManager.getStats();

console.log('Commitment capabilities:', stats.commitment);
// {
//   total: 10,
//   active: 5,
//   revoked: 3,
//   expired: 2,
//   auditStats: { ... }
// }

console.log('Tree capabilities:', stats.tree);
console.log('Active sessions:', stats.sessions);
console.log('Remote connections:', stats.remoteConnections);
```

### View Audit Log

```typescript
// Get all audit entries
const allEntries = secureRpcManager.getAuditEntries();

// Filter by recipient
const aliceEntries = secureRpcManager.getAuditEntries({
  recipientId: alicePubKey
});

// Filter by event type
const revokedEntries = secureRpcManager.getAuditEntries({
  event: 'revoked'
});

// Filter by time
const recentEntries = secureRpcManager.getAuditEntries({
  since: Date.now() - (24 * 60 * 60 * 1000) // Last 24 hours
});

// Print audit log
for (const entry of allEntries) {
  console.log(
    `${new Date(entry.timestamp).toISOString()} - ` +
    `${entry.event.toUpperCase()}: ${entry.capabilityId.slice(0, 8)} ` +
    `(${entry.recipientId?.slice(0, 8) || 'unknown'}) ` +
    `${entry.method ? `- ${entry.method}()` : ''} ` +
    `${entry.reason ? `- ${entry.reason}` : ''}`
  );
}
```

### List Active Capabilities

```typescript
const activeCapabilities = secureRpcManager.listActiveCapabilities();

for (const cap of activeCapabilities) {
  console.log(`
    ID: ${cap.id}
    Recipient: ${cap.recipientId}
    Issued: ${new Date(cap.issuedAt).toISOString()}
    Expires: ${cap.expiresAt ? new Date(cap.expiresAt).toISOString() : 'Never'}
    Permissions: ${cap.permissions?.join(', ') || 'All'}
  `);
}
```

## 🎨 Advanced Patterns

### Read-Only Access (Least Privilege)

```typescript
// Define read-only methods
const readOnlyMethods = [
  'getCommitment',
  'getNeedSlots',
  'getCapacitySlots',
  'getRecognitionWeights',
  'getPubKey'
];

// Issue read-only capability
const readOnly = secureRpcManager.issueCommitmentCapability(
  publicUserPubKey,
  PRESET_READ_ONLY(readOnlyMethods)
);

// User can read
await readOnly.getCommitment(); // ✅ Works

// But cannot write
await readOnly.subscribeToUpdates(() => {}); // ❌ Throws: Permission denied
```

### Time-Limited Access

```typescript
// Short-lived access (5 minutes)
const shortLived = secureRpcManager.issueCommitmentCapability(
  tempUserPubKey,
  PRESET_SHORT_LIVED
);

// Temporary access (1 hour)
const temporary = secureRpcManager.issueCommitmentCapability(
  guestPubKey,
  PRESET_TEMPORARY
);

// Daily access (24 hours)
const daily = secureRpcManager.issueCommitmentCapability(
  regularUserPubKey,
  PRESET_DAILY
);

// Custom duration (30 minutes)
const custom = secureRpcManager.issueCommitmentCapability(
  customUserPubKey,
  { expiresInMs: 30 * 60 * 1000 }
);
```

### Extending Expiration

```typescript
// Issue 1-hour capability
const capability = secureRpcManager.issueCommitmentCapability(
  userPubKey,
  { expiresInMs: 60 * 60 * 1000 }
);

// User requests extension
// (In real implementation, get the capability from manager)
const manager = secureRpcManager.myCommitmentManager;
const revocable = manager.get(capability.getMetadata().id);

// Extend by 1 more hour
if (revocable) {
  revocable.extend(60 * 60 * 1000);
}
```

### Multi-Tenant Isolation (Membranes)

```typescript
// Create membranes for each organization
const orgAMembrane = secureRpcManager.createSessionMembrane(orgAId, 'Organization A');
const orgBMembrane = secureRpcManager.createSessionMembrane(orgBId, 'Organization B');

// Issue capabilities for Org A
const orgACap = orgAMembrane.wrap(
  secureRpcManager.issueCommitmentCapability(orgAUserId)
);

// Issue capabilities for Org B
const orgBCap = orgBMembrane.wrap(
  secureRpcManager.issueCommitmentCapability(orgBUserId)
);

// Later: Org A violates terms - revoke entire organization
secureRpcManager.revokeSession(orgAId, 'Terms violation');
// ALL Org A capabilities revoked, Org B unaffected
```

### Conditional Revocation

```typescript
// Issue capabilities with conditions
const capabilities = new Map<string, any>();

for (const userId of userIds) {
  const cap = secureRpcManager.issueCommitmentCapability(userId);
  capabilities.set(userId, cap);
}

// Monitor usage and revoke if conditions not met
setInterval(() => {
  const entries = secureRpcManager.getAuditEntries({
    event: 'used',
    since: Date.now() - (60 * 60 * 1000) // Last hour
  });
  
  // Revoke users who haven't used their capability in 1 hour
  for (const [userId, cap] of capabilities.entries()) {
    const userEntries = entries.filter(e => e.recipientId === userId);
    
    if (userEntries.length === 0) {
      console.log(`Revoking ${userId} due to inactivity`);
      secureRpcManager.revokeAllFor(userId, 'Inactivity timeout');
      capabilities.delete(userId);
    }
  }
}, 60 * 60 * 1000); // Check every hour
```

## 🛠️ Admin Functions

### Admin API Example

```typescript
// Create admin API with revocation controls
class AdminApi extends RpcTarget {
  constructor(private secureManager: SecureRpcConnectionManager) {
    super();
  }
  
  async revokeUser(adminKey: string, targetUserId: string, reason: string) {
    if (!this.isAdmin(adminKey)) {
      throw new Error('Unauthorized');
    }
    
    const count = this.secureManager.revokeAllFor(targetUserId, reason);
    return { revoked: count, userId: targetUserId };
  }
  
  async getAuditLog(adminKey: string, filter?: any) {
    if (!this.isAdmin(adminKey)) {
      throw new Error('Unauthorized');
    }
    
    return this.secureManager.getAuditEntries(filter);
  }
  
  async getActiveUsers(adminKey: string) {
    if (!this.isAdmin(adminKey)) {
      throw new Error('Unauthorized');
    }
    
    const capabilities = this.secureManager.listActiveCapabilities();
    const users = new Set(capabilities.map(c => c.recipientId).filter(Boolean));
    
    return Array.from(users);
  }
  
  private isAdmin(key: string): boolean {
    // Implement admin verification
    return true; // Placeholder
  }
}
```

## 🧪 Testing

### Unit Tests

```typescript
import { CapabilityManager, RevocableCapability } from '$lib/protocol/capability-revocation';
import { CommitmentRpcTarget } from '$lib/protocol/stores-rpc.svelte';

describe('Capability Revocation', () => {
  test('revoked capability throws error', async () => {
    const target = new CommitmentRpcTarget(testPubKey, testStore);
    const manager = new CapabilityManager(target);
    
    const capability = manager.issue({ recipientId: 'test-user' });
    
    // Works before revocation
    await expect(capability.getCommitment()).resolves.toBeDefined();
    
    // Revoke
    manager.revoke(capability.getMetadata().id, 'Test revocation');
    
    // Throws after revocation
    await expect(capability.getCommitment()).rejects.toThrow('Capability revoked');
  });
  
  test('expired capability throws error', async () => {
    const target = new CommitmentRpcTarget(testPubKey, testStore);
    const manager = new CapabilityManager(target);
    
    const capability = manager.issue({
      recipientId: 'test-user',
      expiresInMs: 100 // 100ms
    });
    
    // Works before expiration
    await expect(capability.getCommitment()).resolves.toBeDefined();
    
    // Wait for expiration
    await new Promise(resolve => setTimeout(resolve, 150));
    
    // Throws after expiration
    await expect(capability.getCommitment()).rejects.toThrow('Capability expired');
  });
  
  test('permission-restricted capability', async () => {
    const target = new CommitmentRpcTarget(testPubKey, testStore);
    const manager = new CapabilityManager(target);
    
    const capability = manager.issue({
      recipientId: 'test-user',
      permissions: ['getCommitment', 'getPubKey']
    });
    
    // Allowed methods work
    await expect(capability.getCommitment()).resolves.toBeDefined();
    await expect(capability.getPubKey()).resolves.toBeDefined();
    
    // Disallowed method throws
    await expect(capability.getNeedSlots()).rejects.toThrow('Permission denied');
  });
});
```

## 📈 Performance Considerations

### Automatic Cleanup

The system automatically cleans up expired capabilities every minute (configurable):

```typescript
// Default: cleanup every 60 seconds
const manager = new CapabilityManager(target, 60000);

// More frequent cleanup
const managerFast = new CapabilityManager(target, 10000); // Every 10 seconds

// Manual cleanup
manager.cleanup(); // Returns number of capabilities removed
```

### Memory Usage

- Active capabilities: ~1KB each
- Audit log: Capped at 10,000 entries (configurable)
- Automatic cleanup prevents memory leaks

## 🎯 Best Practices Summary

1. **Default to Revocable** ✅
   ```typescript
   // Good: Always revocable
   const cap = secureRpcManager.issueCommitmentCapability(userId);
   ```

2. **Add Expiration** ✅
   ```typescript
   // Good: Time-limited (default 24h)
   const cap = secureRpcManager.issueCommitmentCapability(userId, PRESET_DAILY);
   ```

3. **Use Least Privilege** ✅
   ```typescript
   // Good: Read-only access
   const cap = secureRpcManager.issueReadOnlyCommitmentAccess(userId);
   ```

4. **Monitor Usage** ✅
   ```typescript
   // Good: Regular auditing
   const stats = secureRpcManager.getStats();
   const audit = secureRpcManager.getAuditEntries();
   ```

5. **Session Management** ✅
   ```typescript
   // Good: Use membranes for sessions
   const membrane = secureRpcManager.createSessionMembrane(sessionId);
   ```

6. **Cleanup Regularly** ✅
   ```typescript
   // Good: Automatic cleanup (built-in)
   // Or manual: secureRpcManager.cleanup();
   ```

## 🔍 Debugging

Enable debug output in browser console:

```typescript
// Access secure manager
console.log(window.secureRpcManager);

// View statistics
console.log(window.secureRpcManager.getStats());

// View active capabilities
console.table(window.secureRpcManager.listActiveCapabilities());

// View audit log
console.table(window.secureRpcManager.getAuditEntries());
```

## 📚 Resources

- [Cap'n Web Documentation](https://github.com/cloudflare/capnweb)
- [Object Capability Security](https://en.wikipedia.org/wiki/Object-capability_model)
- [Principle of Least Privilege](https://en.wikipedia.org/wiki/Principle_of_least_privilege)

## License

Same as parent project (see LICENSE.md)

