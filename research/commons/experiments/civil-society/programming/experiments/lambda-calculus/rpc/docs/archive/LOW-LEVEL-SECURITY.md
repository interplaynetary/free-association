# Low-Level Security Integration Strategy

## Goal: Security by Default

Make signing/verification **automatic** at the lowest layers so developers can't forget it.

## Integration Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Application Layer                          │
│   // Developers just do this:                               │
│   await session.allocateRecognition('bob', 0.8);            │
│   // Signing happens automatically! ✨                       │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│              EntitySession Layer (AUTO-SIGNS)                │
│   - Intercepts all state-changing operations                │
│   - Automatically signs with private key                    │
│   - Delegates to SecureStorage                              │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│           SecureStorage Layer (STORES SIGNATURES)            │
│   - Extends BrowserStorage                                  │
│   - Stores all updates with signatures                      │
│   - Verifies on load                                        │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│          Transport Layer (SIGNS MESSAGES)                    │
│   - All RPC messages signed                                 │
│   - Automatic verification on receive                       │
│   - Built into Transport base class                         │
└──────────────────────┬──────────────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────────────┐
│         Restoration Layer (VERIFIES EVERYTHING)              │
│   - Automatically verifies all signatures                   │
│   - Rejects invalid updates                                 │
│   - Blacklists bad replicas                                 │
└──────────────────────────────────────────────────────────────┘
```

## Implementation Points

### 1. Secure Context (New Core Abstraction)

This holds the keypair and provides signing primitives.

### 2. SecureStorage (Wraps BrowserStorage)

Automatically signs/verifies all stored updates.

### 3. EntitySession Integration

Uses SecureContext for automatic signing.

### 4. Transport Integration

Signs all outgoing messages, verifies all incoming.

### 5. Restoration Integration

Automatic verification during state reconstruction.

## The Key Insight

**Make security IMPOSSIBLE to forget by building it into the primitives.**

Instead of:
```typescript
// ❌ Manual signing (error-prone)
const update = { ... };
const signature = await sign(update, privateKey);  // Easy to forget!
await storage.store({ ...update, signature });
```

We want:
```typescript
// ✅ Automatic signing (foolproof)
await secureStorage.storeUpdate(update);
// Signing happens automatically inside storeUpdate()!
```

