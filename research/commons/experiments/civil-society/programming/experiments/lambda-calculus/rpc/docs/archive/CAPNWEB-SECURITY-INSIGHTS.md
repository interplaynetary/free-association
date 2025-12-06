# Cap'n Web Security Insights 🔑

## Key Insight: Capability-Based Security

From the article:

> "When we call the `authenticate()` method, after it has verified the provided API key, it returns an authenticated session object. **It is impossible for the client to 'forge' a session object.** The only way to get one is to call `authenticate()`, and have it return successfully."

**This is EXACTLY what our cryptographic signatures provide!**

## The Cap'n Web Authentication Pattern

### Their Example

```typescript
class MyApiServer extends RpcTarget {
  authenticate(apiKey) {
    let username = await checkApiKey(apiKey);
    return new AuthenticatedSession(username);
  }
}

class AuthenticatedSession extends RpcTarget {
  constructor(username) {
    super();
    this.username = username;
  }

  whoami() {
    return this.username;
  }

  // ...other methods requiring auth...
}
```

### Key Security Properties

1. **Can't forge the session** - Client can only get it via `authenticate()`
2. **No credentials on every call** - Session object IS the authorization
3. **Type-safe** - Can't call authenticated methods without the session
4. **Natural abstraction** - Authentication fits into the RPC model

## How This Applies to Our Implementation

### We Should Do This!

```typescript
class RelayServer extends RpcTarget {
  /**
   * Authenticate and get a capability token
   * 
   * This returns a session that CAN'T BE FORGED because:
   * 1. It's signed with our private key
   * 2. Client can only get it by proving they have the password
   * 3. Signature verifies it came from real authenticate() call
   */
  async authenticate(challenge: Challenge, signature: string): Promise<EntitySession> {
    // Verify the challenge was signed correctly
    const isValid = await verifyChallenge(challenge, signature, publicKey);
    
    if (!isValid) {
      throw new AuthenticationError('Invalid signature');
    }

    // Derive entity ID from public key
    const entityId = deriveEntityId(publicKey);
    
    // Create session (this is the "capability"!)
    const session = await SecureEntitySession.create(entityId, keypair);
    
    // Return it - client now has an UNFORGEABLE capability
    return session;
  }
}

// Client usage
const challenge = await relay.createChallenge();
const signature = await signChallenge(challenge, myPrivateKey);

// Get the authenticated session (unforgeable!)
const session = await relay.authenticate(challenge, signature);

// Now use it - no need to pass credentials again!
await session.allocateRecognition('bob', 0.8);
const mr = await session.getMutualRecognition('bob');
```

### Why This Is Secure

**Cap'n Web perspective:**
- Session object can only come from `authenticate()`
- RPC system ensures it can't be forged

**Our cryptographic perspective:**
- Session operations are signed with private key
- Signatures prove operations came from authenticated entity
- Even if client tries to forge, signatures will fail

**Combined:** Double security!
- Cap'n Web prevents forging the session object itself
- Cryptographic signatures prevent forging operations on the session

## Export Table Pattern (ID 0)

From the article:

> "At the start of the connection, Alice and Bob each populate their export tables with a single entry, numbered zero, representing their 'main' interfaces."

### We Should Adopt This!

```typescript
// Server exports RelayServer at ID 0
const relay = new RelayServer();
transport.exportAt(0, relay);

// Client exports empty interface at ID 0
transport.exportAt(0, new RpcTarget());

// Then authentication:
const mainApi = transport.getExport(0);  // RelayServer
const session = await mainApi.authenticate(challenge, signature);

// Now client exports the session
transport.exportAt(-1, session);  // Negative IDs for client exports
```

### Benefits

- ✅ Standard convention (ID 0 = main API)
- ✅ Symmetric protocol (both sides can export)
- ✅ Clean separation of concerns
- ✅ Follows Cap'n Web patterns

## Security Through Object References

From the article:

> "In most RPC systems, it is not possible for one RPC to return a stub pointing at a new RPC object in this way. Instead, all functions are top-level, and can be called by anyone. In such a traditional RPC system, it would be necessary to pass the API key again to every function call."

### Traditional (Insecure) Pattern

```typescript
// ❌ Pass credentials on EVERY call
await api.allocateRecognition(apiKey, 'bob', 0.8);
await api.getMutualRecognition(apiKey, 'alice', 'bob');
await api.getMRS(apiKey, 'alice');

// Problems:
// - API key exposed in every message
// - Server must verify on every call
// - Can't revoke without changing API key
// - Easy to forget to check
```

### Cap'n Web + Our Pattern (Secure)

```typescript
// ✅ Authenticate once, get capability
const session = await relay.authenticate(challenge, signature);

// ✅ Use capability (no credentials needed!)
await session.allocateRecognition('bob', 0.8);
await session.getMutualRecognition('alice', 'bob');
await session.getMRS('alice');

// Benefits:
// - Credentials never re-transmitted
// - Session IS the authorization
// - Can revoke by closing session
// - Type-safe (can't forget)
// - Operations are ALSO signed (double security!)
```

## WebSocket Authorization Problem (Solved!)

From the article:

> "This is a common pain point for WebSockets in particular. Due to the design of the web APIs for WebSocket, you generally cannot use headers nor cookies to authorize them. Instead, authorization must happen in-band, by sending a message over the WebSocket itself."

### The Problem

```typescript
// ❌ Traditional WebSocket auth
const ws = new WebSocket('wss://example.com');

ws.onopen = () => {
  // Send auth message (special case!)
  ws.send(JSON.stringify({ type: 'auth', token: apiKey }));
  
  // Now connection is "authenticated"
  // But this is fragile and error-prone
};
```

### Our Solution (Cap'n Web Pattern)

```typescript
// ✅ Authentication as RPC
const relay = newWebSocketRpcSession('wss://example.com');

// Authenticate using challenge-response
const challenge = await relay.createChallenge();
const signature = await signChallenge(challenge, privateKey);
const session = await relay.authenticate(challenge, signature);

// Session is unforgeable capability!
await session.allocateRecognition('bob', 0.8);
```

**Benefits:**
- ✅ Authentication is just another RPC
- ✅ No special state changes on the connection
- ✅ Type-safe (can't forget to authenticate)
- ✅ Session object IS the authorization
- ✅ Clean abstraction

## Recommended Architecture Changes

### Current (Good but Could Be Better)

```typescript
// Login returns EntitySession
const session = await login('alice@example.com', 'password');

// But how does the relay know it's authenticated?
// We're relying on implicit trust
```

### Recommended (Cap'n Web Pattern)

```typescript
// 1. Connect to relay (unauthenticated)
const relay = await connectToRelay('wss://relay.example.com');

// 2. Create challenge
const challenge = await relay.createChallenge();

// 3. Sign challenge with derived keypair
const keypair = await deriveKeypair(password, email);
const signature = await signChallenge(challenge, keypair.privateKey);

// 4. Authenticate - returns UNFORGEABLE session capability
const session = await relay.authenticate(challenge, signature, keypair.publicKey);

// 5. Use session - IT IS the authorization!
await session.allocateRecognition('bob', 0.8);
```

### Why This Is Better

1. **Explicit authentication flow**
   - Clear when authentication happens
   - No implicit trust

2. **Capability-based**
   - Session object can't be forged
   - Having the reference IS the authorization

3. **Cryptographically verified**
   - Challenge-response proves identity
   - All operations signed with private key

4. **Follows Cap'n Web patterns**
   - Standard convention
   - Clean abstraction

## Implementation Checklist

### Phase 1: Update RelayServer ✅ (Mostly Done)

```typescript
class RelayServer extends RpcTarget {
  // Create challenge for authentication
  createChallenge(): Challenge
  
  // Authenticate and return session capability
  authenticate(challenge, signature, publicKey): Promise<EntitySession>
  
  // ... other methods ...
}
```

### Phase 2: Update Login Flow

```typescript
// Update login() to use authenticate pattern
async function login(email: string, password: string): Promise<EntitySession> {
  // 1. Connect to relay
  const relay = await connectToRelay();
  
  // 2. Derive keypair
  const keypair = await deriveKeypair(password, email);
  
  // 3. Challenge-response
  const challenge = await relay.createChallenge();
  const signature = await signChallenge(challenge, keypair.privateKey);
  
  // 4. Get session capability
  const session = await relay.authenticate(challenge, signature, keypair.publicKey);
  
  // 5. Restore state (all signed!)
  await session.restoreState();
  
  return session;
}
```

### Phase 3: Export Pattern (ID 0)

```typescript
// Server exports RelayServer at ID 0
class RelayServerWorker {
  async fetch(request) {
    const relay = new RelayServer();
    return newWorkersRpcResponse(request, relay);
  }
}

// Client connects to ID 0
const relay = newWebSocketRpcSession('wss://relay.example.com');
// relay is the ID 0 export (RelayServer)
```

## Security Comparison

### Traditional RPC

```
Client --[call(apiKey, params)]--> Server
                                    ├─ Verify apiKey
                                    ├─ Execute
                                    └─ Return result
                                    
Every call: Check credentials ❌
```

### Cap'n Web Only

```
Client --[authenticate(apiKey)]--> Server
                                    ├─ Verify apiKey
                                    └─ Return Session object

Client --[session.call(params)]--> Server
                                    ├─ Trust session object
                                    └─ Execute
                                    
Forging session: Prevented by RPC system ✅
Forging operations: Not cryptographically verified ⚠️
```

### Cap'n Web + Our Cryptographic Signatures

```
Client --[authenticate(challenge, sig)]--> Server
                                            ├─ Verify signature
                                            └─ Return Session object

Client --[session.call(params)]--> Server
         (signed with private key)  ├─ Trust session object ✅
                                    ├─ Verify signature ✅
                                    └─ Execute
                                    
Forging session: Prevented by RPC system ✅
Forging operations: Prevented by cryptography ✅
Double security! 🔒🔒
```

## Summary

### Key Insights from Cap'n Web

1. **Authentication returns capabilities** - Session objects that can't be forged
2. **Having reference IS authorization** - No credentials on every call
3. **Type-safe security** - Can't forget to authenticate
4. **Clean abstraction** - Authentication fits naturally into RPC
5. **Export table pattern** - ID 0 convention for main API

### How We Should Apply This

1. ✅ **Already doing:** RpcTarget pattern, promise pipelining, HTTP batch
2. 🔜 **Should add:** Explicit authenticate() → session pattern
3. 🔜 **Should add:** Export table with ID 0 convention
4. ✅ **Already doing better:** Cryptographic signatures on operations

### Our Advantage

**Cap'n Web + Cryptographic Signatures = Double Security**

- Cap'n Web prevents forging session objects
- Cryptographic signatures prevent forging operations
- Challenge-response proves identity
- Every operation is signed and verified

**Result: Production-grade, capability-based, cryptographically secure RPC! 🚀🔒**

## Next Steps

1. Refactor `login()` to use `authenticate()` pattern
2. Implement export table (ID 0 for RelayServer)
3. Update documentation with capability-based security model
4. Add tests for authentication flow
5. Consider adding capability tokens (time-limited, revocable)

