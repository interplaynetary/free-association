# Trust-Circle Encryption for Allocation Privacy

**Date:** 2025-11-02  
**Model:** Selective disclosure via public-key encryption  
**Core Idea:** "Only people I trust can see my needs and allocate to me"

---

## Concept Overview

Instead of making data fully public or using expensive ZK proofs, use **asymmetric encryption**:

```
1. Alice encrypts her needs with her private key
2. Alice shares decryption capability only with trusted providers (Bob, Carol)
3. Bob can decrypt Alice's needs ? can allocate to her
4. Dave (not trusted) cannot decrypt ? cannot see needs or allocate
5. Alice can revoke trust ? re-encrypt with new key, exclude Dave
```

**Key Advantage:** Natural fit for the algorithm's design:
- Only mutual recognition matters for Tier 1 allocations
- If I don't trust you, I won't recognize you ? no MR ? no allocation anyway
- Trust-based encryption makes this relationship **cryptographically enforced**

---

## Architecture: Trust-Circle Model

### **Data Structure**

```typescript
interface EncryptedCommitment {
  pubkey: string;  // My public identity
  
  // PUBLIC (everyone can see)
  capacity_slots: AvailabilitySlot[];  // What I can provide
  global_recognition_weights: GlobalRecognitionWeights;  // Who I recognize
  itc_stamp: ITCStamp;
  
  // ENCRYPTED (only trusted circle can decrypt)
  encrypted_need_slots: {
    ciphertext: Uint8Array;  // Encrypted need slots
    encryption_scheme: 'hybrid' | 'abe' | 'proxy-re-encryption';
    trust_circle_hash: string;  // Hash of current trust circle (for versioning)
    nonce: Uint8Array;  // For authenticated encryption
  };
  
  // TRUST MANAGEMENT
  trust_circle: {
    trusted_pubkeys: string[];  // Who can decrypt my needs
    revoked_pubkeys: string[];  // Previously trusted, now revoked
    trust_updated_at: number;
  };
}
```

### **Encryption Flow**

```typescript
// 1. ENCRYPT MY NEEDS (only trusted circle can decrypt)
function encryptNeedsForTrustCircle(
  myNeedSlots: NeedSlot[],
  myPrivateKey: PrivateKey,
  trustedPubkeys: string[]
): EncryptedNeedData {
  // Hybrid encryption: symmetric + asymmetric
  const symmetricKey = generateRandomKey();
  const ciphertext = AES_GCM.encrypt(needSlots, symmetricKey);
  
  // Encrypt symmetric key for each trusted person
  const encryptedKeys: Record<string, Uint8Array> = {};
  for (const trustedPubkey of trustedPubkeys) {
    encryptedKeys[trustedPubkey] = RSA_OAEP.encrypt(
      symmetricKey, 
      trustedPubkey
    );
  }
  
  return { ciphertext, encryptedKeys, nonce };
}

// 2. DECRYPT SOMEONE ELSE'S NEEDS (if they trust me)
function decryptNeedsFromTrustedPerson(
  encrypted: EncryptedNeedData,
  myPrivateKey: PrivateKey,
  myPubkey: string
): NeedSlot[] | null {
  // Check if I'm in their trust circle
  const encryptedKeyForMe = encrypted.encryptedKeys[myPubkey];
  if (!encryptedKeyForMe) {
    return null;  // They don't trust me
  }
  
  // Decrypt the symmetric key with my private key
  const symmetricKey = RSA_OAEP.decrypt(encryptedKeyForMe, myPrivateKey);
  
  // Decrypt the actual needs
  return AES_GCM.decrypt(encrypted.ciphertext, symmetricKey, encrypted.nonce);
}
```

---

## Cryptographic Techniques for Dynamic Trust Circles

### **Technique 1: Hybrid Encryption (Simplest)**

**How it works:**
- Encrypt needs with symmetric key (AES-256-GCM)
- Encrypt symmetric key with each trusted person's public key (RSA-OAEP or X25519)
- Store one encrypted key copy per trusted person

**Adding trust:**
```typescript
function addToTrustCircle(newTrustedPubkey: string) {
  // Encrypt existing symmetric key for new person
  const encryptedKeyForNew = RSA_OAEP.encrypt(
    currentSymmetricKey, 
    newTrustedPubkey
  );
  
  // Append to encrypted keys
  commitment.encrypted_need_slots.encryptedKeys[newTrustedPubkey] = encryptedKeyForNew;
  
  // Publish updated commitment (no need to re-encrypt data!)
  publishCommitment(commitment);
}
```

**Revoking trust:**
```typescript
function revokeFromTrustCircle(revokedPubkey: string) {
  // CRITICAL: Must re-encrypt with NEW symmetric key
  const newSymmetricKey = generateRandomKey();
  const newCiphertext = AES_GCM.encrypt(myNeedSlots, newSymmetricKey);
  
  // Encrypt new key for all trusted people EXCEPT revoked
  const newEncryptedKeys: Record<string, Uint8Array> = {};
  for (const trustedPubkey of trustedPubkeys) {
    if (trustedPubkey === revokedPubkey) continue;  // Skip revoked
    newEncryptedKeys[trustedPubkey] = RSA_OAEP.encrypt(
      newSymmetricKey,
      trustedPubkey
    );
  }
  
  // Publish updated commitment with NEW encryption
  commitment.encrypted_need_slots = {
    ciphertext: newCiphertext,
    encryptedKeys: newEncryptedKeys,
    nonce: newNonce
  };
  
  publishCommitment(commitment);
}
```

**Pros:**
- ? Simple to implement (standard crypto libraries)
- ? Fast encryption/decryption (AES is hardware-accelerated)
- ? Easy to add new trusted people (no data re-encryption)
- ? Works with existing key infrastructure

**Cons:**
- ? Revocation requires full re-encryption + republishing
- ? Encrypted key storage grows linearly with trust circle size
- ? No forward secrecy (if private key leaked, all past data compromised)

**Best for:** Small-medium trust circles (5-50 people), infrequent revocations

---

### **Technique 2: Proxy Re-Encryption (PRE)**

**How it works:**
- Encrypt needs with your public key
- Generate "re-encryption keys" that let proxy (network) transform your ciphertext into format decryptable by trusted person
- Proxy never sees plaintext

**Adding trust:**
```typescript
function addToTrustCircleWithPRE(newTrustedPubkey: string) {
  // Generate re-encryption key: my_private_key ? their_public_key
  const reEncryptionKey = PRE.generateReKey(
    myPrivateKey,
    newTrustedPubkey
  );
  
  // Publish re-encryption key (can be public!)
  publishReEncryptionKey(myPubkey, newTrustedPubkey, reEncryptionKey);
  
  // Network/proxy automatically transforms ciphertext for new person
  // No need to re-encrypt original data!
}
```

**Revoking trust:**
```typescript
function revokeFromTrustCircleWithPRE(revokedPubkey: string) {
  // Delete the re-encryption key for revoked person
  deleteReEncryptionKey(myPubkey, revokedPubkey);
  
  // Optionally: rotate my key pair for forward secrecy
  const newKeyPair = generateKeyPair();
  const newCiphertext = PRE.encrypt(myNeedSlots, newKeyPair.publicKey);
  
  // Generate new re-encryption keys for remaining trusted people
  for (const trustedPubkey of remainingTrusted) {
    const newReKey = PRE.generateReKey(newKeyPair.privateKey, trustedPubkey);
    publishReEncryptionKey(newKeyPair.publicKey, trustedPubkey, newReKey);
  }
}
```

**Pros:**
- ? Adding trust is very cheap (just publish a small re-encryption key)
- ? Network can transform ciphertext without seeing plaintext
- ? More scalable for large trust circles
- ? Forward secrecy possible with key rotation

**Cons:**
- ? More complex cryptography (fewer libraries)
- ? Revocation still requires re-encryption
- ? Potential security issues if proxy is compromised

**Best for:** Large trust circles (50-500 people), frequent trust additions

**Libraries:**
- [AFGH Proxy Re-Encryption](https://github.com/nucypher/pyUmbral) (Python)
- [umbral-pre](https://github.com/nucypher/umbral-pre) (Rust)

---

### **Technique 3: Attribute-Based Encryption (ABE)**

**How it works:**
- Define access policy: "Can decrypt if: (recognized_by_me AND capacity_type='food') OR (mutual_recognition > 0.5)"
- Encrypt with policy, not specific public keys
- Anyone matching policy can decrypt

**Example:**
```typescript
function encryptNeedsWithPolicy() {
  // Define access policy (boolean formula over attributes)
  const policy = {
    type: 'AND',
    children: [
      { attribute: 'recognized_by', value: myPubkey },
      { attribute: 'has_capacity_type', value: 'food' }
    ]
  };
  
  // Encrypt with policy (not specific pubkeys!)
  const ciphertext = ABE.encrypt(myNeedSlots, policy, masterPublicKey);
  
  return ciphertext;
}

function decryptIfMatchPolicy(
  ciphertext: Uint8Array,
  myAttributes: Record<string, any>,
  myPrivateKey: ABEPrivateKey
): NeedSlot[] | null {
  // My attributes: { recognized_by: ['alice', 'bob'], has_capacity_type: 'food' }
  // ABE library checks if my attributes satisfy policy
  
  return ABE.decrypt(ciphertext, myAttributes, myPrivateKey);
}
```

**Adding/revoking trust:**
```typescript
// No explicit add/revoke needed!
// Just update recognition weights (public data)
function updateRecognition(newWeights: GlobalRecognitionWeights) {
  // When I recognize someone new, they automatically can decrypt
  // (if they have the required capacity type)
  publishRecognitionWeights(newWeights);
}

// For strong revocation, rotate the policy
function rotatePolicyWithRevocation(revokedPubkey: string) {
  const newPolicy = {
    type: 'AND',
    children: [
      { attribute: 'recognized_by', value: myPubkey },
      { attribute: 'NOT', child: { attribute: 'pubkey', value: revokedPubkey } }  // Explicit exclusion
    ]
  };
  
  // Re-encrypt with new policy
  const newCiphertext = ABE.encrypt(myNeedSlots, newPolicy, masterPublicKey);
  publishCommitment({ encrypted_need_slots: newCiphertext });
}
```

**Pros:**
- ? Very flexible access policies (boolean formulas, thresholds)
- ? No need to enumerate all trusted pubkeys
- ? Natural fit for recognition-based allocation
- ? Adding trust is implicit (just change recognition)

**Cons:**
- ? Complex cryptography (fewer mature libraries)
- ? Slower than hybrid encryption
- ? Requires trusted authority for key generation (or decentralized ABE)
- ? Policy updates may require re-encryption

**Best for:** Complex trust policies, dynamic communities, research projects

**Libraries:**
- [OpenABE](https://github.com/zeutro/openabe) (C++)
- [charm-crypto](https://github.com/JHUISI/charm) (Python)

---

### **Technique 4: Broadcast Encryption with Revocation**

**How it works:**
- Encrypt once for entire trust circle
- Use binary tree structure for efficient revocation
- Revoking someone only requires O(log n) key updates

**Structure:**
```
Trust Circle Tree (8 people):
             ROOT
          /        \
        A           B
       / \         / \
      C   D       E   F
     / \ / \     / \ / \
    P1 P2 P3 P4 P5 P6 P7 P8

Each person has keys for all ancestors from their leaf to root
```

**Encryption:**
```typescript
function encryptForBroadcast(
  needSlots: NeedSlot[],
  trustCircleTree: Tree
): BroadcastCiphertext {
  // Encrypt with root key
  const rootKey = trustCircleTree.getRootKey();
  const ciphertext = AES_GCM.encrypt(needSlots, rootKey);
  
  return { ciphertext, treeVersion: trustCircleTree.version };
}
```

**Revocation:**
```typescript
function revokePerson(revokedPubkey: string) {
  // Find their position in tree
  const leaf = trustCircleTree.findLeaf(revokedPubkey);
  
  // Re-key all siblings on path to root (O(log n) operations)
  let current = leaf;
  while (current.parent) {
    // Generate new key for sibling subtree
    const sibling = current.parent.getOtherChild(current);
    sibling.rotateKeys();  // All descendants get new keys
    
    current = current.parent;
  }
  
  // Distribute new keys to remaining trusted people
  trustCircleTree.distributeKeysToValidMembers();
}
```

**Pros:**
- ? Efficient revocation (O(log n) complexity)
- ? Constant-size ciphertext (doesn't grow with trust circle)
- ? Good for frequent revocations

**Cons:**
- ? Complex key management (tree structure)
- ? Adding people requires tree rebalancing
- ? Requires careful implementation to avoid timing attacks

**Best for:** Very large trust circles (100s-1000s), frequent revocations

**Libraries:**
- Custom implementation (complex)
- Research papers: [Naor-Naor-Lotspiech (NNL)](https://www.cs.tau.ac.il/~bchor/broadcast.pdf)

---

## Integration with Allocation Algorithm

### **Modified Allocation Flow**

```typescript
// PROVIDER'S PERSPECTIVE
function computeAllocationsWithTrustCircles() {
  const allocations: SlotAllocationRecord[] = [];
  
  // Get all commitments from network
  const allCommitments = getAllCommitmentsRecord();
  
  for (const capacitySlot of myCapacitySlots) {
    const typeId = capacitySlot.need_type_id;
    
    // Find compatible recipients
    for (const [recipientPub, commitment] of Object.entries(allCommitments)) {
      // TRY TO DECRYPT THEIR NEEDS
      const decryptedNeeds = decryptNeedsFromTrustedPerson(
        commitment.encrypted_need_slots,
        myPrivateKey,
        myPubkey
      );
      
      if (decryptedNeeds === null) {
        // They don't trust me ? cannot allocate to them
        console.log(`[TRUST-CIRCLE] ${recipientPub} doesn't trust me, skipping`);
        continue;
      }
      
      // Find compatible need slots
      const compatibleSlots = decryptedNeeds.filter(needSlot => 
        needSlot.need_type_id === typeId && 
        slotsCompatible(needSlot, capacitySlot)
      );
      
      if (compatibleSlots.length === 0) continue;
      
      // Check mutual recognition (as before)
      const mutualRec = myMutualRecognition[recipientPub] || 0;
      if (mutualRec <= 0) {
        console.log(`[TRUST-CIRCLE] No mutual recognition with ${recipientPub}, skipping`);
        continue;
      }
      
      // COMPUTE ALLOCATION (same algorithm as before)
      // ... rest of allocation logic
    }
  }
  
  return allocations;
}
```

### **Trust Circle Synchronization with Recognition**

**Key Insight:** Trust circles and recognition weights should be aligned!

```typescript
// AUTOMATIC TRUST CIRCLE SYNC
function syncTrustCircleWithRecognition() {
  const myRecognition = get(myRecognitionOfOthers);
  const currentTrustCircle = get(myTrustCircleStore);
  
  // Add anyone I recognize to trust circle
  const recognizedPubkeys = Object.keys(myRecognition);
  const newTrusted = recognizedPubkeys.filter(
    pubkey => !currentTrustCircle.includes(pubkey)
  );
  
  if (newTrusted.length > 0) {
    console.log(`[TRUST-SYNC] Adding ${newTrusted.length} people to trust circle`);
    for (const pubkey of newTrusted) {
      addToTrustCircle(pubkey);
    }
  }
  
  // Remove anyone I no longer recognize (optional - may keep for grace period)
  const noLongerRecognized = currentTrustCircle.filter(
    pubkey => !myRecognition[pubkey] || myRecognition[pubkey] === 0
  );
  
  if (noLongerRecognized.length > 0) {
    console.log(`[TRUST-SYNC] Revoking ${noLongerRecognized.length} people from trust circle`);
    for (const pubkey of noLongerRecognized) {
      revokeFromTrustCircle(pubkey);
    }
  }
}

// Auto-sync whenever recognition changes
myRecognitionOfOthers.subscribe(() => {
  syncTrustCircleWithRecognition();
});
```

---

## Revocation Strategies

### **Strategy 1: Immediate Hard Revocation**
```typescript
function immediateRevocation(revokedPubkey: string) {
  // Re-encrypt with new key, exclude revoked person
  const newKey = generateRandomKey();
  const newCiphertext = AES_GCM.encrypt(myNeedSlots, newKey);
  
  // Distribute new key to remaining trusted
  const newEncryptedKeys = {};
  for (const trustedPubkey of remainingTrusted) {
    newEncryptedKeys[trustedPubkey] = RSA_OAEP.encrypt(newKey, trustedPubkey);
  }
  
  // Publish immediately
  publishCommitment({
    encrypted_need_slots: { ciphertext: newCiphertext, encryptedKeys: newEncryptedKeys }
  });
}
```
- ? Strong security (revoked person cannot decrypt new data)
- ? High overhead (full re-encryption + republishing)

### **Strategy 2: Lazy Revocation with Grace Period**
```typescript
function lazyRevocation(revokedPubkey: string) {
  // Mark as revoked, but don't re-encrypt yet
  revokedList.add(revokedPubkey);
  
  // Re-encrypt on next natural update
  needSlotsStore.subscribe((newNeeds) => {
    if (needsChanged(newNeeds)) {
      // Re-encrypt with new key (excluding revoked)
      reEncryptNeedsExcluding(revokedList);
    }
  });
}
```
- ? Lower overhead (batches revocations)
- ?? Revoked person can still see data until next update

### **Strategy 3: Periodic Key Rotation**
```typescript
function scheduleKeyRotation() {
  setInterval(() => {
    // Rotate encryption key weekly
    const newKey = generateRandomKey();
    reEncryptAllData(newKey);
    
    // Distribute to current trust circle (excludes anyone revoked during week)
    distributeKeyToTrustCircle(newKey);
  }, 7 * 24 * 60 * 60 * 1000);  // Weekly
}
```
- ? Forward secrecy (past data can't be decrypted if key leaked)
- ? Batches revocations efficiently
- ?? Revoked people can see data for up to 1 week

---

## Growing Trust Circles: Discovery Mechanisms

### **Problem:** How do I find new people to trust?

### **Mechanism 1: Web of Trust**
```typescript
function discoverThroughWebOfTrust() {
  const myTrusted = get(myTrustCircleStore);
  const suggestions: Record<string, number> = {};
  
  // For each person I trust, see who THEY trust
  for (const trustedPubkey of myTrusted) {
    const theirCommitment = networkCommitments.get(trustedPubkey);
    const theirRecognition = theirCommitment?.global_recognition_weights || {};
    
    // Score potential new trusted people by how many of my trusted peers recognize them
    for (const [theirTrustedPubkey, weight] of Object.entries(theirRecognition)) {
      if (myTrusted.includes(theirTrustedPubkey)) continue;  // Already trust
      suggestions[theirTrustedPubkey] = (suggestions[theirTrustedPubkey] || 0) + weight;
    }
  }
  
  // Sort by score (most trusted by my trust circle)
  return Object.entries(suggestions)
    .sort(([, a], [, b]) => b - a)
    .slice(0, 10);  // Top 10 suggestions
}
```

### **Mechanism 2: Capability Matching**
```typescript
function discoverByCapability(neededType: string) {
  // Search network for providers with matching capacity
  // WHO ALSO have public "seeking trust" flag set
  
  const potentialProviders = Array.from(networkCommitments.entries())
    .filter(([pubkey, commitment]) => {
      // Has capacity I need
      const hasCapacity = commitment.capacity_slots?.some(
        slot => slot.need_type_id === neededType && slot.quantity > 0
      );
      
      // Is open to new trust relationships
      const isOpenToTrust = commitment.trust_preferences?.accepting_new_trust;
      
      return hasCapacity && isOpenToTrust;
    });
  
  return potentialProviders;
}
```

### **Mechanism 3: Gradual Trust Escalation**
```typescript
interface TrustLevel {
  level: 'stranger' | 'acquaintance' | 'trusted' | 'inner-circle';
  canDecrypt: boolean;
  dataVisibility: 'none' | 'aggregates' | 'ranges' | 'full';
}

function establishTrustGradually(newPubkey: string) {
  // Stage 1: Stranger (no data visible)
  setTrustLevel(newPubkey, {
    level: 'stranger',
    canDecrypt: false,
    dataVisibility: 'none'
  });
  
  // Stage 2: After first successful interaction ? Acquaintance (aggregates visible)
  // Share only aggregate needs: { food: 'high', healthcare: 'low' }
  onSuccessfulInteraction(() => {
    setTrustLevel(newPubkey, {
      level: 'acquaintance',
      canDecrypt: false,
      dataVisibility: 'aggregates'
    });
  });
  
  // Stage 3: After multiple successful interactions ? Trusted (ranges visible)
  // Share need ranges: { food: '20-40 meals', healthcare: '1-5 checkups' }
  onTrustThreshold(() => {
    setTrustLevel(newPubkey, {
      level: 'trusted',
      canDecrypt: true,  // Can decrypt, but sees ranges only
      dataVisibility: 'ranges'
    });
  });
  
  // Stage 4: Explicit promotion ? Inner Circle (full data)
  onUserPromotion(() => {
    addToTrustCircle(newPubkey);  // Full access
    setTrustLevel(newPubkey, {
      level: 'inner-circle',
      canDecrypt: true,
      dataVisibility: 'full'
    });
  });
}
```

---

## Shrinking Trust Circles: Triggers for Revocation

### **Automatic Revocation Triggers**

```typescript
interface RevocationPolicy {
  // Revoke if no mutual recognition for X days
  noMutualRecognitionDays: number;
  
  // Revoke if they haven't provided capacity in X iterations
  inactiveIterations: number;
  
  // Revoke if trust score drops below threshold
  trustScoreThreshold: number;
  
  // Revoke if explicitly reported by other trusted members
  minReportsForRevocation: number;
}

function checkAutoRevocation(policy: RevocationPolicy) {
  for (const [trustedPubkey, trustMeta] of trustCircleMetadata.entries()) {
    // Check 1: No mutual recognition
    const mutualRec = myMutualRecognition[trustedPubkey] || 0;
    const daysSinceMR = (Date.now() - trustMeta.lastMutualRecognition) / (1000 * 60 * 60 * 24);
    
    if (mutualRec === 0 && daysSinceMR > policy.noMutualRecognitionDays) {
      console.log(`[AUTO-REVOKE] ${trustedPubkey}: No MR for ${daysSinceMR} days`);
      revokeFromTrustCircle(trustedPubkey);
      continue;
    }
    
    // Check 2: Inactivity
    if (trustMeta.iterationsSinceLastProvision > policy.inactiveIterations) {
      console.log(`[AUTO-REVOKE] ${trustedPubkey}: Inactive for ${trustMeta.iterationsSinceLastProvision} iterations`);
      revokeFromTrustCircle(trustedPubkey);
      continue;
    }
    
    // Check 3: Trust score degradation
    const trustScore = computeTrustScore(trustedPubkey);
    if (trustScore < policy.trustScoreThreshold) {
      console.log(`[AUTO-REVOKE] ${trustedPubkey}: Trust score ${trustScore} below threshold`);
      revokeFromTrustCircle(trustedPubkey);
      continue;
    }
    
    // Check 4: Reports from other trusted members
    const reportCount = getReportCount(trustedPubkey);
    if (reportCount >= policy.minReportsForRevocation) {
      console.log(`[AUTO-REVOKE] ${trustedPubkey}: ${reportCount} reports from trusted members`);
      revokeFromTrustCircle(trustedPubkey);
      continue;
    }
  }
}

// Run checks periodically
setInterval(() => checkAutoRevocation(myRevocationPolicy), 24 * 60 * 60 * 1000);  // Daily
```

### **Manual Revocation UI**

```typescript
// User-initiated revocation with confirmation
function manualRevoke(pubkeyToRevoke: string, reason: string) {
  // Confirm with user
  const confirmed = confirm(
    `Revoke trust for ${getUserName(pubkeyToRevoke)}?\n\n` +
    `Reason: ${reason}\n\n` +
    `They will no longer see your needs or be able to allocate to you.\n` +
    `This action requires re-encrypting your data.`
  );
  
  if (!confirmed) return;
  
  // Log for audit trail
  revocationLog.add({
    revokedPubkey: pubkeyToRevoke,
    reason,
    timestamp: Date.now(),
    revokedBy: myPubkey
  });
  
  // Perform revocation
  revokeFromTrustCircle(pubkeyToRevoke);
  
  // Notify user
  notify(`? Trust revoked for ${getUserName(pubkeyToRevoke)}`);
}
```

---

## Performance Considerations

### **Hybrid Encryption Benchmarks (Estimated)**

| Operation | Trust Circle Size | Latency | Bandwidth |
|-----------|-------------------|---------|-----------|
| Encrypt needs | N/A | ~1ms | +32 bytes |
| Encrypt key (per person) | 1 | ~0.5ms | +256 bytes (RSA) |
| Total encryption | 10 people | ~6ms | ~2.8KB |
| Total encryption | 50 people | ~26ms | ~13KB |
| Total encryption | 200 people | ~101ms | ~52KB |
| Decrypt needs (recipient) | N/A | ~1ms | N/A |
| Add person (no re-encrypt) | +1 | ~0.5ms | +256 bytes |
| Revoke person (re-encrypt) | 50 people | ~26ms | ~13KB (republish) |

**Conclusion:** Scales reasonably up to ~100 people per trust circle

### **Optimization: Lazy Encryption**

```typescript
// Only encrypt when publishing, not on every local update
let needsChanged = false;
let pendingNeeds = [];

needSlotsStore.subscribe((newNeeds) => {
  pendingNeeds = newNeeds;
  needsChanged = true;
  // Don't encrypt yet!
});

// Encrypt just before publishing (batches multiple changes)
async function publishCommitmentLazy() {
  if (!needsChanged) {
    await publishCommitment(cachedCommitment);
    return;
  }
  
  // Now encrypt (once, with batched changes)
  const encrypted = encryptNeedsForTrustCircle(pendingNeeds, myPrivateKey, trustCircle);
  
  cachedCommitment.encrypted_need_slots = encrypted;
  await publishCommitment(cachedCommitment);
  
  needsChanged = false;
}
```

---

## Implementation Roadmap

### **Phase 1: Proof of Concept (2-3 weeks)**
1. Implement hybrid encryption (AES-256-GCM + X25519)
2. Add `encrypted_need_slots` to commitment schema
3. Modify allocation algorithm to decrypt before matching
4. Basic trust circle UI (add/remove trusted people)

**Libraries:**
- `@noble/ciphers` - AES-GCM encryption
- `@noble/curves` - X25519 key exchange
- `tweetnacl` - Lightweight crypto (NaCl/libsodium)

### **Phase 2: Trust Management (1-2 weeks)**
5. Implement revocation with re-encryption
6. Web-of-trust discovery mechanism
7. Auto-sync trust circle with recognition weights
8. Audit log for trust changes

### **Phase 3: Advanced Features (2-4 weeks)**
9. Gradual trust escalation (stranger ? acquaintance ? trusted)
10. Automatic revocation policies
11. Key rotation for forward secrecy
12. Performance optimization (lazy encryption, caching)

### **Phase 4: Research (Optional, 3-6 months)**
13. Proxy re-encryption for efficient key distribution
14. Attribute-based encryption for policy-based access
15. Broadcast encryption for large trust circles

---

## Security Considerations

### **Threat Model**

| Threat | Mitigation |
|--------|-----------|
| **Revoked person keeps old data** | Re-encrypt with new key on revocation (hard revocation) |
| **Network observer sees trust circle** | `trusted_pubkeys` list is public - consider hiding (encrypt list itself) |
| **Malicious provider claims to be trusted** | Recipient verifies provider has valid encrypted key |
| **Sybil attack (fake trusted identities)** | Web-of-trust + reputation system |
| **Private key compromise** | Key rotation + forward secrecy |
| **Timing attacks on encryption** | Use constant-time crypto libraries |

### **Privacy Guarantees**

**What remains private:**
- ? Exact need quantities (only trust circle can see)
- ? Need slot details (time, location constraints)
- ? Damping history and factors
- ? Past needs (if keys rotated)

**What becomes public:**
- ?? Trust circle membership (list of trusted pubkeys) - consider encrypting this too
- ?? Trust circle size (observable from encrypted key count)
- ?? When trust changes (timestamp of commitment updates)

**Enhanced privacy (optional):**
```typescript
// Encrypt the trust circle list itself
interface DoubleEncryptedCommitment {
  encrypted_need_slots: Uint8Array;  // Needs encrypted with key K
  encrypted_trust_circle: Uint8Array;  // Trust circle encrypted with key K
  
  // Only encrypted keys are public (can't tell WHO is trusted)
  encrypted_keys: {
    [pubkeyHash: string]: Uint8Array  // Use hash instead of raw pubkey
  };
}
```

---

## Example: Full Trust Circle Flow

```typescript
// ===== ALICE'S SIDE =====

// 1. Alice creates her commitment with needs
const aliceNeeds: NeedSlot[] = [
  { id: 'need-1', need_type_id: 'food', quantity: 40, /* ... */ }
];

// 2. Alice chooses her trust circle
const aliceTrustCircle = ['bob_pubkey', 'carol_pubkey'];

// 3. Alice encrypts her needs for trust circle
const encrypted = encryptNeedsForTrustCircle(
  aliceNeeds,
  alicePrivateKey,
  aliceTrustCircle
);

// 4. Alice publishes commitment
await publishCommitment({
  pubkey: alicePubkey,
  capacity_slots: [],
  encrypted_need_slots: encrypted,
  trust_circle: { trusted_pubkeys: aliceTrustCircle },
  global_recognition_weights: { 'bob_pubkey': 0.6, 'carol_pubkey': 0.4 }
});

// ===== BOB'S SIDE =====

// 5. Bob receives Alice's commitment
const aliceCommitment = networkCommitments.get(alicePubkey);

// 6. Bob tries to decrypt Alice's needs
const decryptedNeeds = decryptNeedsFromTrustedPerson(
  aliceCommitment.encrypted_need_slots,
  bobPrivateKey,
  bobPubkey
);

if (decryptedNeeds) {
  console.log('? Alice trusts me! I can see her needs:', decryptedNeeds);
  
  // 7. Bob computes allocations (same algorithm as before)
  const allocation = computeAllocationForRecipient(alicePubkey, decryptedNeeds);
  
  // 8. Bob publishes allocation
  await publishAllocation(allocation);
} else {
  console.log('? Alice doesn\'t trust me. Cannot allocate to her.');
}

// ===== ALICE REVOKES CAROL =====

// 9. Alice decides to revoke Carol's trust
revokeFromTrustCircle('carol_pubkey');

// This triggers:
// - Generate new symmetric key
// - Re-encrypt needs with new key
// - Encrypt new key for Bob only (not Carol)
// - Republish commitment

// 10. Carol tries to decrypt after revocation
const carolDecrypt = decryptNeedsFromTrustedPerson(
  updatedAliceCommitment.encrypted_need_slots,
  carolPrivateKey,
  carolPubkey
);

// carolDecrypt === null (Carol no longer has valid encrypted key)
console.log('? Carol: Alice revoked my trust. Cannot see her needs anymore.');
```

---

## Conclusion

**Trust-circle encryption is the sweet spot for this protocol:**

? **Practical:** Uses standard crypto (no exotic ZK proofs)  
? **Efficient:** Minimal overhead (<10ms per allocation)  
? **Privacy:** Hides needs from untrusted parties  
? **Aligned:** Matches algorithm's recognition-based design  
? **Flexible:** Easy to grow/shrink trust circle  
? **Revocable:** Can remove trust with re-encryption

**Recommended:** Start with **Hybrid Encryption (Technique 1)** - simple, battle-tested, sufficient for most use cases.

---

## Next Steps

1. ? Implement hybrid encryption POC
2. ? Add trust circle management UI
3. ? Auto-sync trust with recognition
4. ?? Performance testing with 50-person trust circles
5. ?? Research proxy re-encryption for scale (if needed)

