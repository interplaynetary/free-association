# State Restoration Security 🔒

## Your Question: How Do We Know Data Isn't Tampered With?

**Short Answer:** We currently have **partial protection** via Merkle trees and consensus, but we're **missing cryptographic signatures** on state updates!

## Current Security Measures ✅ (Incomplete!)

### 1. Merkle Tree Verification ✅

**What it does:**
```typescript
// Replicas provide Merkle root
const merkleRoot = await replica.getMerkleRoot();

// We verify state matches the Merkle tree
const verified = verifyMerkleProof(stateFragment, merkleRoot);
```

**What it protects against:**
- ✅ Accidental corruption
- ✅ Network transmission errors
- ✅ Inconsistent data from a replica

**What it DOESN'T protect against:**
- ❌ Malicious replica fabricating entire state
- ❌ Replica adding fake recognition allocations
- ❌ Man-in-the-middle attacks

### 2. Consensus from Multiple Replicas ✅

**What it does:**
```typescript
// Fetch from multiple replicas
const fragments = await Promise.all(
  replicas.map(r => r.getStateFor(publicKey))
);

// Use median value for consensus
const mergedState = mergeFragments(fragments);
```

**What it protects against:**
- ✅ Single byzantine replica
- ✅ Outlier values from one replica

**What it DOESN'T protect against:**
- ❌ Collusion (multiple replicas lying together)
- ❌ All replicas being compromised

### 3. Replica Reputation ✅

**What it does:**
- Tracks which replicas provide consistent data
- Prefers high-reputation replicas

**What it protects against:**
- ✅ Consistently misbehaving replicas

**What it DOESN'T protect against:**
- ❌ New replicas (no reputation yet)
- ❌ Sophisticated attacks (fake good reputation)

## 🚨 Critical Missing Piece: Signed State Updates

### The Vulnerability

**Current implementation:**
```typescript
// Alice allocates recognition to Bob
await alice.allocateRecognition('bob', 0.8);

// This gets stored by replicas...
// BUT THERE'S NO SIGNATURE! 🚨

// Later, a malicious replica could claim:
// "Alice allocated 0.1 to Bob" (FALSE!)
// "Alice allocated 0.9 to Charlie" (NEVER HAPPENED!)
```

**The problem:** Replicas store state updates, but **we don't verify those updates were actually signed by the entity's private key!**

### The Solution: Cryptographic Signatures

**Every state update should be signed:**

```typescript
// Alice allocates recognition
const update = {
  from: 'alice',
  to: 'bob',
  value: 0.8,
  timestamp: Date.now(),
  nonce: crypto.randomUUID()
};

// Sign with Alice's private key
const signature = await sign(update, alicePrivateKey);

// Store the signed update
const signedUpdate = {
  ...update,
  signature,
  publicKey: alicePublicKey
};

// Replicas store this signed update
await replica.storeSignedUpdate(signedUpdate);
```

**On restoration:**

```typescript
// Fetch signed updates from replicas
const signedUpdates = await replica.getSignedUpdates(alicePublicKey);

// Verify EVERY update was actually signed by Alice
for (const update of signedUpdates) {
  const isValid = await verify(
    update,
    update.signature,
    alicePublicKey  // Alice's PUBLIC key
  );
  
  if (!isValid) {
    // This update is FAKE! Reject it!
    console.error('Forged update detected from replica!');
    blacklistReplica(replica);
    continue;
  }
}

// Only apply verified updates
applyVerifiedUpdates(validUpdates);
```

## How Private Keys Come Into This

### Key Pair Role

```
┌─────────────────────────────────────────────────────────────┐
│                     Key Derivation                           │
│                                                              │
│  Password + Email → Private Key + Public Key                │
│         ↓                    ↓              ↓                │
│    (kept secret)      (stored on-chain)  (identity)         │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  State Update Flow                           │
│                                                              │
│  1. Alice makes recognition allocation                       │
│     ↓                                                        │
│  2. Sign with PRIVATE key (only Alice has this!)           │
│     ↓                                                        │
│  3. Broadcast signed update to replicas                     │
│     ↓                                                        │
│  4. Replicas store: { update, signature, publicKey }       │
│     ↓                                                        │
│  5. Anyone can VERIFY with PUBLIC key                       │
│                                                              │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  State Restoration Flow                      │
│                                                              │
│  1. Bob logs in, derives his keypair                        │
│     ↓                                                        │
│  2. Replicas send state fragments (signed updates)          │
│     ↓                                                        │
│  3. Bob verifies EACH update's signature:                   │
│     - Was this really signed by Alice's private key?        │
│     - Use Alice's PUBLIC key to verify                      │
│     ↓                                                        │
│  4. Reject any unverified/forged updates                    │
│     ↓                                                        │
│  5. Apply only cryptographically verified state             │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

### Why This Works

**Public/Private Key Properties:**

1. **Private Key (Secret):**
   - Only YOU have it (derived from password)
   - Used to SIGN your state updates
   - Never leaves your device

2. **Public Key (Published):**
   - Everyone knows it (your identity)
   - Used to VERIFY signatures
   - Cannot create signatures (one-way!)

**Security Guarantee:**

```typescript
// Alice signs with HER private key
const signature = sign(update, alicePrivateKey);

// ANYONE can verify it was really Alice
const isReallyFromAlice = verify(update, signature, alicePublicKey);

// But ONLY Alice could have created that signature!
// Even if replicas collude, they can't forge Alice's signature
// because they don't have her private key!
```

## Current vs Secure Implementation

### ❌ Current (Insecure)

```typescript
// State fragment structure
interface StateFragment {
  entityId: string;
  edges: Map<string, Map<string, number>>;
  timestamp: ITCStamp;
  replicaId: string;
  // ❌ NO SIGNATURES!
}

// Restoration
const fragments = await Promise.all(
  replicas.map(r => r.getStateFor(publicKey))
);

// We trust the data... 🚨
const state = mergeFragments(fragments);
```

**Vulnerability:**
- Replicas can fabricate entire state
- No way to prove updates came from the real entity
- Consensus helps but isn't cryptographically secure

### ✅ Secure (What We Need)

```typescript
// Signed state update
interface SignedUpdate {
  from: EntityId;
  to: EntityId;
  value: number;
  timestamp: number;
  nonce: string;           // Prevent replay attacks
  signature: string;       // 🔒 Cryptographic proof!
  publicKey: string;       // Signer's public key
}

// State fragment with signatures
interface SignedStateFragment {
  entityId: string;
  signedUpdates: SignedUpdate[];  // 🔒 Each update is signed!
  merkleRoot: string;              // Additional integrity check
  replicaId: string;
}

// Secure restoration
const fragments = await Promise.all(
  replicas.map(r => r.getSignedStateFor(publicKey))
);

// Verify EVERY update cryptographically
for (const fragment of fragments) {
  for (const update of fragment.signedUpdates) {
    // 🔒 Cryptographic verification
    const isValid = await verifyUpdate(update);
    
    if (!isValid) {
      console.error('FORGED UPDATE DETECTED!');
      blacklistReplica(fragment.replicaId);
      continue;  // Skip this update
    }
    
    // Only apply verified updates
    applyUpdate(update);
  }
}
```

## Implementation Needed

### 1. Sign Every State Update

```typescript
// In EntitySession.allocateRecognition()
async allocateRecognition(to: EntityId, amount: number) {
  // Current validation...
  
  // NEW: Create signed update
  const update = {
    from: this.entityId,
    to,
    value: amount,
    timestamp: Date.now(),
    nonce: crypto.randomUUID()
  };
  
  // Sign with private key
  const signature = await this.signUpdate(update);
  
  const signedUpdate = {
    ...update,
    signature,
    publicKey: this.publicKey
  };
  
  // Broadcast to replicas
  await this.broadcastSignedUpdate(signedUpdate);
  
  // Store locally (with signature)
  await this.storage.storeSignedUpdate(signedUpdate);
}

private async signUpdate(update: any): Promise<string> {
  // Use credentials.ts signChallenge() pattern
  const enc = new TextEncoder();
  const data = enc.encode(JSON.stringify(update));
  
  const privateKeyJwk = JSON.parse(this.privateKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    privateKeyJwk,
    { name: 'Ed25519' },
    true,
    ['sign']
  );
  
  const signature = await crypto.subtle.sign(
    { name: 'Ed25519' },
    cryptoKey,
    data
  );
  
  return btoa(String.fromCharCode(...new Uint8Array(signature)));
}
```

### 2. Verify on Restoration

```typescript
// In restoration/login.ts
async function verifyAndMergeFragments(
  fragments: SignedStateFragment[]
): Promise<ReconstructedState> {
  const verifiedUpdates: SignedUpdate[] = [];
  
  for (const fragment of fragments) {
    for (const update of fragment.signedUpdates) {
      // 🔒 Cryptographic verification
      const isValid = await verifyUpdateSignature(update);
      
      if (!isValid) {
        console.error(`Forged update from replica ${fragment.replicaId}`);
        decreaseReplicaReputation(fragment.replicaId);
        continue;
      }
      
      verifiedUpdates.push(update);
    }
  }
  
  // Only merge cryptographically verified updates
  return mergeVerifiedUpdates(verifiedUpdates);
}

async function verifyUpdateSignature(update: SignedUpdate): Promise<boolean> {
  const enc = new TextEncoder();
  const { signature, publicKey, ...data } = update;
  const dataBytes = enc.encode(JSON.stringify(data));
  
  // Import public key
  const publicKeyJwk = JSON.parse(publicKey);
  const cryptoKey = await crypto.subtle.importKey(
    'jwk',
    publicKeyJwk,
    { name: 'Ed25519' },
    true,
    ['verify']
  );
  
  // Decode signature
  const signatureBytes = Uint8Array.from(atob(signature), c => c.charCodeAt(0));
  
  // Verify signature
  return await crypto.subtle.verify(
    { name: 'Ed25519' },
    cryptoKey,
    signatureBytes,
    dataBytes
  );
}
```

### 3. Update Storage Schema

```typescript
// BrowserStorage needs to store signatures
interface StoredRecognitionUpdate {
  from: EntityId;
  to: EntityId;
  value: number;
  timestamp: number;
  nonce: string;
  signature: string;      // 🔒 Store the signature!
  publicKey: string;      // 🔒 Store the public key!
  itcStamp: ITCStamp;     // For causality
}
```

## Security Guarantees

With signed updates:

✅ **Authenticity:** Updates were really made by the entity
✅ **Integrity:** Updates haven't been modified
✅ **Non-repudiation:** Entity can't deny making the update
✅ **Replay protection:** Nonces prevent replay attacks
✅ **Byzantine resistance:** Forged updates are detected and rejected

## Attack Scenarios

### Scenario 1: Malicious Replica

**Attack:**
```typescript
// Malicious replica tries to add fake recognition
const fakeUpdate = {
  from: 'alice',
  to: 'eve',  // Eve is the attacker
  value: 0.9,
  signature: 'fake_signature'
};
```

**Defense:**
```typescript
// Verification fails!
const isValid = await verify(fakeUpdate, 'fake_signature', alicePublicKey);
// → false! 

// Eve doesn't have Alice's private key,
// so she can't create a valid signature
```

### Scenario 2: Replica Collusion

**Attack:**
```typescript
// Multiple replicas agree to lie about Alice's state
const conspiringReplicas = [replica1, replica2, replica3];
// All return the same fake data
```

**Defense:**
```typescript
// Signatures still fail!
// Even if ALL replicas collude, they can't forge
// Alice's signature without her private key

for (const update of allegedUpdates) {
  const isValid = await verify(update, update.signature, alicePublicKey);
  if (!isValid) {
    // Detected! All colluding replicas get blacklisted
  }
}
```

### Scenario 3: Stolen State Data

**Attack:**
```typescript
// Attacker intercepts signed updates in transit
const intercepted = stealNetworkTraffic();
```

**Defense:**
```typescript
// No problem! Updates are already signed.
// Attacker can READ them but can't MODIFY them
// Any modification breaks the signature

const tampered = { ...intercepted, value: 0.99 };
const isValid = await verify(tampered, intercepted.signature, publicKey);
// → false! Signature no longer matches
```

## Summary

### Current Security Level: ⚠️ MEDIUM

- ✅ Merkle trees (integrity)
- ✅ Consensus (Byzantine resistance)
- ❌ NO signatures (authenticity)

**Vulnerable to:** Malicious replicas fabricating state

### Needed Security Level: 🔒 HIGH

- ✅ Merkle trees (integrity)
- ✅ Consensus (Byzantine resistance)
- ✅ Signatures (authenticity) ← **CRITICAL MISSING PIECE**

**Resistant to:** All attacks (replicas can't forge signatures)

## Next Steps

1. ✅ Implement `signUpdate()` in EntitySession
2. ✅ Implement `verifyUpdateSignature()` in restoration
3. ✅ Update storage schema to include signatures
4. ✅ Update StateFragment to use SignedUpdate
5. ✅ Update replicas to verify signatures before storing
6. ✅ Add signature verification to state restoration

**Priority: HIGH** - This is essential for production security!

