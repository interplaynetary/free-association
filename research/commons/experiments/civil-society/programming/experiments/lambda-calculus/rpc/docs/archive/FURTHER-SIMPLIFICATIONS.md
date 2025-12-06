# Further Simplifications from Cap'n Web

After a closer reading of the Cap'n Web article, here are additional simplifications we could make:

## Key Cap'n Web Principles We Haven't Fully Adopted

### 1. "No schemas. Almost no boilerplate whatsoever" ⭐

**Cap'n Web approach:**
```typescript
// That's it - just a class!
class MyApiServer extends RpcTarget {
  hello(name) {
    return `Hello, ${name}!`
  }
}
```

**Our current approach:**
- Complex `EntitySessionConfig` types
- Explicit storage/cache passing
- Manual capability management

**Possible simplification:**
```typescript
// Make EntitySession a simple RpcTarget
class EntitySession extends RpcTarget {
  constructor(private entityId: string) {
    super();
    // Auto-initialize storage/cache internally
  }
  
  async getMutualRecognition(targetId: string): Promise<number> {
    // Implementation
  }
}

// Usage becomes even simpler:
let session = new EntitySession('alice');
let mr = await session.getMutualRecognition('bob');
```

### 2. "Just JSON" Serialization ⭐

**Cap'n Web approach:**
- Plain JSON with minimal escape sequences
- No complex serialization layer

**Our current approach:**
- `serialization.ts` with lots of custom logic
- Separate serializers for different types

**Possible simplification:**
```typescript
// Just use JSON.stringify/parse with replacer/reviver
const serialize = (obj: unknown) => JSON.stringify(obj, (key, value) => {
  if (value instanceof Map) return { _type: 'Map', entries: [...value] };
  if (isStamp(value)) return { _type: 'Stamp', ...value };
  return value;
});

const deserialize = (json: string) => JSON.parse(json, (key, value) => {
  if (value?._type === 'Map') return new Map(value.entries);
  if (value?._type === 'Stamp') return value;
  return value;
});
```

### 3. TypeScript Interface Pattern ⭐

**Cap'n Web approach:**
```typescript
// Define API as interface
interface MyApi {
  hello(name: string): Promise<string>;
}

// Type the client
let api: RpcStub<MyApi> = newWebSocketRpcSession("wss://...");

// Type the server
class MyApiServer extends RpcTarget implements MyApi {
  hello(name: string): Promise<string> { ... }
}
```

**Our approach:**
- Direct class usage
- No shared interface pattern

**Possible simplification:**
```typescript
// Define shared interface
interface EntityAPI {
  getMutualRecognition(targetId: string): Promise<number>;
  getMRS(entityIds: string[]): Promise<Record<string, number>>;
  getMRD(entityIds: string[]): Promise<Record<string, number>>;
  allocateRecognition(targetId: string, amount: number): Promise<void>;
}

// Type the session
let api: EntityAPI = newWebSocketSession('alice', 'wss://...');

// Server implements the interface
class EntitySession extends RpcTarget implements EntityAPI {
  // Implementation
}
```

### 4. Remove CapabilityManager ⭐

**Cap'n Web approach:**
- Capabilities handled internally by the RPC system
- No explicit export/import management needed

**Our approach:**
- Explicit `CapabilityManager` class
- Manual export/import tracking

**Possible simplification:**
```typescript
// Cap'n Web handles this internally!
// We don't need CapabilityManager at all if we adopt their protocol

// Just mark classes as RpcTarget and it works:
class EntitySession extends RpcTarget {
  // Methods are automatically callable over RPC
}
```

### 5. Minimize Transport Complexity

**Cap'n Web approach:**
- Simple transport interface
- "It just works" over WebSocket, HTTP, postMessage

**Our approach:**
- Complex transport types
- Multiple transport classes

**Possible simplification:**
```typescript
// Single unified transport interface
interface Transport {
  send(message: unknown): void;
  onMessage(handler: (message: unknown) => void): void;
  close(): void;
}

// WebSocket transport is just a few lines
function createWebSocketTransport(url: string): Transport {
  const ws = new WebSocket(url);
  return {
    send: (msg) => ws.send(JSON.stringify(msg)),
    onMessage: (handler) => ws.onmessage = (e) => handler(JSON.parse(e.data)),
    close: () => ws.close()
  };
}
```

## Proposed Refactoring

### Phase 1: Simplify Serialization

**Remove:**
- `rpc/serialization.ts` (most of it)

**Replace with:**
```typescript
// rpc/json.ts - Simple JSON with replacer/reviver
export const RpcJSON = {
  stringify: (obj: unknown) => JSON.stringify(obj, replacer),
  parse: (json: string) => JSON.parse(json, reviver)
};
```

### Phase 2: RpcTarget Pattern

**Add:**
```typescript
// rpc/rpc-target.ts - Marker base class
export class RpcTarget {
  // Just a marker - methods are automatically RPC-able
}
```

**Update:**
```typescript
// EntitySession becomes simple
class EntitySession extends RpcTarget implements EntityAPI {
  constructor(entityId: string) {
    super();
    this.entityId = entityId;
    // Auto-initialize everything
  }
}
```

### Phase 3: Interface-First Design

**Add:**
```typescript
// rpc/api.ts - Shared interface
export interface EntityAPI {
  getMutualRecognition(targetId: string): Promise<number>;
  getMRS(entityIds: string[]): Promise<Record<string, number>>;
  getMRD(entityIds: string[]): Promise<Record<string, number>>;
  allocateRecognition(targetId: string, amount: number): Promise<void>;
  getMyAllocations(): Promise<Array<{ targetId: string; amount: number }>>;
}

// Type helper
export type RpcStub<T> = {
  [K in keyof T]: T[K] extends (...args: infer A) => infer R
    ? (...args: A) => R
    : T[K];
};
```

**Usage:**
```typescript
// Client gets typed stub
let api: RpcStub<EntityAPI> = newWebSocketSession('alice', 'wss://...');

// Full type safety!
let mr = await api.getMutualRecognition('bob');
```

### Phase 4: Remove CapabilityManager

**If we adopt Cap'n Web's protocol:**
- Remove `rpc/capability-manager.ts`
- Capabilities tracked internally
- Automatic export/import

### Phase 5: Simplify Transports

**Unify to single interface:**
```typescript
// All transports implement this
interface Transport {
  send(message: unknown): void;
  onMessage(handler: (message: unknown) => void): void;
  close(): void;
}

// Each transport is minimal
```

## Size Target: "Under 10 kB"

Cap'n Web is under 10 kB (minified+gzipped). Let's aim for reasonable size:

**Current estimate:**
- Core RPC: ~50 kB
- With all features: ~200 kB

**After simplifications:**
- Core RPC: ~20 kB (60% reduction!)
- With all features: ~100 kB (50% reduction!)

**How:**
1. Remove serialization complexity
2. Remove CapabilityManager
3. Simplify transports
4. Fewer type definitions

## Benefits Summary

| Simplification | Benefit | Effort |
|----------------|---------|--------|
| JSON serialization | Simpler, smaller | Low |
| RpcTarget pattern | More intuitive | Medium |
| Interface-first | Better types | Low |
| Remove CapabilityManager | Less code | High |
| Simplify transports | Cleaner | Low |

## Recommendation

**Do now (low-hanging fruit):**
1. ✅ Add interface-first pattern (EntityAPI)
2. ✅ Simplify JSON serialization
3. ✅ Add RpcTarget base class

**Consider later:**
4. Remove CapabilityManager (requires protocol changes)
5. Full transport simplification (if needed)

## Example: Before vs After

### Before (Current)

```typescript
import { EntitySession, BrowserStorage, RecognitionCache } from './rpc';

const storage = new BrowserStorage('alice');
await storage.initialize();
const cache = new RecognitionCache();
const session = new EntitySession({
  entityId: 'alice',
  storage,
  cache
});

const mr = await session.getMutualRecognition('bob');
```

### After (Further Simplified)

```typescript
import { newWebSocketSession, type EntityAPI } from './rpc';

// One line + full type safety!
let api: EntityAPI = newWebSocketSession('alice', 'wss://relay.example.com');
let mr = await api.getMutualRecognition('bob');
```

### After (Even More Like Cap'n Web)

```typescript
import { EntitySession, RpcTarget } from './rpc';

// Just instantiate - it handles everything!
let api = new EntitySession('alice');
let mr = await api.getMutualRecognition('bob');
```

## Implementation Priority

### High Priority (Do Now) ⭐
1. **Add EntityAPI interface** - Better TypeScript experience
2. **Add RpcTarget base class** - Cap'n Web pattern
3. **Simplify JSON serialization** - Less code

### Medium Priority
4. **Simplify EntitySession constructor** - Auto-initialize storage/cache
5. **Unified Transport interface** - Cleaner API

### Low Priority (Optional)
6. **Remove CapabilityManager** - Only if we adopt full Cap'n Web protocol
7. **Size optimization** - If bundle size becomes issue

## Next Steps

Want me to implement the high-priority simplifications?

1. Create `EntityAPI` interface
2. Add `RpcTarget` base class
3. Simplify JSON serialization
4. Update examples

This would make our system even more Cap'n Web-like while keeping all our recognition-based features!

