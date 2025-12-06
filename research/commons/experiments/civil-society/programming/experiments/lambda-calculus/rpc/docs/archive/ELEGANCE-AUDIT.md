# Elegance Audit: Further Improvements

## Summary

✅ **Already Elegant:**
- One-line login API
- Shared RPC dispatching
- Unified message handling
- Automatic serialization in transports

🎯 **Can Be More Elegant:**

## 1. Replace Raw JSON with RpcJSON

**Found:** 21 files still using `JSON.parse/stringify`

**Why it matters:**
- RpcJSON handles Map, Set, BigInt, undefined
- Consistent serialization everywhere
- Better error messages

**Fix:**
```typescript
// ❌ Before
const data = JSON.parse(message);
response.send(JSON.stringify(result));

// ✅ After
import { RpcJSON } from '../json-rpc';
const data = RpcJSON.parse(message);
response.send(RpcJSON.stringify(result));
```

**Files to update:**
- `server/workers.ts` (stats endpoint)
- `server/node.ts` (stats endpoint)
- Transport files (if any)
- Test files (acceptable for test data)

## 2. Create Factory Functions

**Pattern:** Repeated object creation with similar initialization

### Current Duplication:

```typescript
// In multiple places:
const storage = new BrowserStorage(`fa-db-${entityId}`);
await storage.initialize();

const cache = new RecognitionCache();

const clock = ITClock.seed();
```

**Elegant Solution:**

```typescript
// Create: rpc/factories.ts
export async function createStorage(entityId: string): Promise<BrowserStorage> {
  const storage = new BrowserStorage(`fa-db-${entityId}`);
  await storage.initialize();
  return storage;
}

export function createCache(config?: CacheConfig): RecognitionCache {
  return new RecognitionCache(config);
}

export function createClock(): ITClock {
  return ITClock.seed();
}

// Usage - one line!
const storage = await createStorage(entityId);
const cache = createCache();
const clock = createClock();
```

## 3. Unify Error Types

**Current:** Error handling is inconsistent

```typescript
// Some places:
throw new Error('Method not found');

// Other places:
return { error: 'Method not found' };

// Others:
throw { type: 'RpcError', message: 'Method not found' };
```

**Elegant Solution:**

```typescript
// Create: rpc/errors.ts
export class RpcError extends Error {
  constructor(
    message: string,
    public code: string,
    public details?: any
  ) {
    super(message);
    this.name = 'RpcError';
  }

  toJSON() {
    return {
      error: this.message,
      code: this.code,
      details: this.details
    };
  }
}

export class MethodNotFoundError extends RpcError {
  constructor(method: string) {
    super(`Method not found: ${method}`, 'METHOD_NOT_FOUND', { method });
  }
}

export class SessionNotFoundError extends RpcError {
  constructor(entityId: string) {
    super(`Session not found: ${entityId}`, 'SESSION_NOT_FOUND', { entityId });
  }
}

// Usage:
throw new MethodNotFoundError('allocateRecognition');
```

## 4. Consolidate Type Definitions

**Found:** Duplicate interfaces across files

Example duplications:
- `EntityId` defined in multiple places?
- Similar `Config` interfaces
- Duplicate transport types

**Solution:** Ensure all types are in `types.ts` and imported, not redefined.

## 5. Create Composition Helpers

**Pattern:** Building complex objects from simpler ones

```typescript
// Current - manual composition
const session = new EntitySession(entityId);
session.storage = await createStorage(entityId);
session.cache = createCache();
await session.initialize();

// Elegant - builder pattern
const session = await EntitySession.builder(entityId)
  .withStorage()
  .withCache()
  .withClock()
  .build();
```

## 6. Standardize Async Patterns

**Pattern:** Inconsistent promise handling

```typescript
// Some places use callbacks:
session.subscribe((update) => { /* ... */ });

// Others use promises:
await session.waitForUpdate();

// Elegant: Offer both!
const unsubscribe = session.subscribe(callback);
// AND
for await (const update of session.updates()) {
  // async iteration
}
```

## 7. Create Middleware Pattern for Servers

**Current:** Each server has custom setup

**Elegant:**

```typescript
// Create: rpc/server/middleware.ts
export function createServerMiddleware(relay: RelayServer) {
  return {
    websocket: (ws: WebSocketAdapter) => handleRelayMessage(ws, relay),
    http: (req: HttpRequest) => handleHttpRpcBatch(req, relay),
    stats: () => handleStatsRequest(relay)
  };
}

// Usage in any server:
const middleware = createServerMiddleware(relay);

// Workers:
if (isWebSocket) return middleware.websocket(wsAdapter);
if (isHttp) return middleware.http(request);

// Node:
ws.on('message', middleware.websocket);
app.post('/rpc', middleware.http);
```

## Implementation Priority

### High Impact (Do Now)

1. ✅ **Shared RPC dispatching** (DONE)
2. ✅ **Shared message handlers** (DONE)
3. 🎯 **Factory functions** - Most impact on elegance
4. 🎯 **Error types** - Better DX

### Medium Impact (Do Soon)

5. **Replace raw JSON** - Consistency
6. **Server middleware** - Even more DRY
7. **Consolidate types** - Reduce confusion

### Nice to Have (Do Later)

8. **Builder pattern** - Advanced ergonomics
9. **Async iteration** - Modern API
10. **More examples** - Better onboarding

## Files to Create

```
rpc/
├── factories.ts         (NEW) - Object creation helpers
├── errors.ts           (NEW) - Unified error types
└── server/
    └── middleware.ts   (NEW) - Server middleware pattern
```

## Expected Impact

| Improvement | Lines Saved | Elegance Gain |
|-------------|-------------|---------------|
| Factories | 50-100 | ⭐⭐⭐⭐ |
| Error types | 30-50 | ⭐⭐⭐ |
| Middleware | 40-60 | ⭐⭐⭐⭐ |
| Replace JSON | 20-30 | ⭐⭐ |
| **Total** | **140-240** | **Very High** |

## Next Steps

Want me to implement:
1. Factory functions?
2. Error types?
3. Server middleware?
4. All of the above?

The most impactful would be **factories + error types** - they make the API significantly more elegant with minimal effort!

