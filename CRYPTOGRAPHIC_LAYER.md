# Cryptographic Layer for Free Association

**Date:** November 12, 2025  
**Module:** `SEA.hs` (Security, Encryption, and Authorization)

## Overview

The cryptographic layer adds **authentication, encryption, and verifiable commitments** to the Free Association protocol, enabling secure, privacy-preserving, peer-to-peer coordination.

Inspired by GunDB's SEA.js, this layer provides:
- **ECDSA signing/verification** for authenticated data
- **ECDH shared secrets** for peer-to-peer encryption
- **AES-GCM authenticated encryption** for private data
- **User-owned encrypted spaces** in the distributed graph
- **Verifiable commitments** that can't be forged

---

## 🔐 Core Cryptographic Primitives

### 1. Key Pair Generation

Each user has **two key pairs**:

```haskell
data KeyPair = KeyPair
  { -- ECDSA keys (signing/verification)
    publicKey :: Text      -- "x.y" format (P-256 curve point)
  , privateKey :: Text     -- "d" scalar (private)
  
    -- ECDH keys (encryption/decryption)
  , encryptionPublicKey :: Text   -- "x.y" format
  , encryptionPrivateKey :: Text  -- "d" scalar
  }

generateKeyPair :: MonadRandom m => m KeyPair
```

**Why two key pairs?**
- **ECDSA (signing):** Proves identity, prevents forgery
- **ECDH (encryption):** Derives shared secrets, enables private communication

### 2. Signing & Verification

**Sign data:**
```haskell
signData :: (MonadRandom m, ToJSON a) => a -> KeyPair -> m (Signed a)

-- Example:
commitment <- signData myNeedData userKeyPair
-- Result: Signed { message = myNeedData, signature = "base64..." }
```

**Verify signature:**
```haskell
verifyData :: (FromJSON a, ToJSON a) => Signed a -> Text -> Maybe a

-- Example:
case verifyData signedCommitment senderPublicKey of
  Just data -> -- Signature valid!
  Nothing -> -- Invalid or tampered
```

**Properties:**
- Uses SHA-256 hash of data
- ECDSA signature over hash
- Base64-encoded for network transmission
- Immutable (tampering breaks signature)

### 3. Encryption & Decryption

**Encrypt to self:**
```haskell
encryptData :: (MonadRandom m, ToJSON a) => a -> KeyPair -> m Encrypted

-- Example:
encrypted <- encryptData myPrivateNeed userKeyPair
-- Result: Encrypted { ciphertext, iv, salt }
```

**Decrypt:**
```haskell
decryptData :: (FromJSON a) => Encrypted -> KeyPair -> Maybe a

-- Example:
case decryptData encrypted userKeyPair of
  Just data -> -- Decryption successful
  Nothing -> -- Wrong key or tampered data
```

**Encrypt to another user:**
```haskell
encryptTo :: (MonadRandom m, ToJSON a) => a -> KeyPair -> KeyPair -> m Encrypted

-- Example: Alice encrypts to Bob
encrypted <- encryptTo secretData bobKeyPair aliceKeyPair
-- Bob decrypts with:
data <- decryptFrom encrypted aliceKeyPair bobKeyPair
```

**Properties:**
- AES-GCM authenticated encryption
- Random IV per encryption (prevents pattern attacks)
- Derived keys from ECDH (PBKDF2 or HKDF)
- Authentication tag detects tampering

---

## 🗂️ Graph Nodes with Signatures

### Per-Property Signatures

Each property in a graph node can be **individually signed**:

```haskell
data GraphNode = GraphNode
  { nodeId :: Text  -- Soul (#)
  , nodeData :: M.Map Text JSON.Value
  , nodeStates :: M.Map Text Integer  -- State timestamps (>)
  , nodeSignatures :: M.Map Text Text  -- Per-property signatures (s)
  , userPubKey :: Maybe Text  -- User who owns this node
  }
```

**Example node:**
```json
{
  "_": {
    "#": "user123-commitment",
    ">": {
      "type": 1699900000,
      "quantity": 1699900000,
      "recognition": 1699900001
    },
    "s": {
      "type": "MEUCIQDx...",
      "quantity": "MEQCIAb...",
      "recognition": "MEYCIQC..."
    },
    "~": "userPublicKey123"
  },
  "type": "tutoring",
  "quantity": 5,
  "recognition": { "provider1": 0.3 }
}
```

**Why per-property signatures?**
- Fine-grained verification
- Partial node updates (only changed properties re-signed)
- Selective disclosure (share only verified properties)

### Creating Signed Nodes

```haskell
createSignedNode :: (MonadRandom m) => 
  Text                         -- Node ID
  -> M.Map Text JSON.Value     -- Properties
  -> KeyPair                   -- User's key pair
  -> m GraphNode

-- Example:
node <- createSignedNode "my-commitment" properties myKeyPair
-- All properties automatically signed
```

### Verifying Nodes

```haskell
verifyProperties :: (ToJSON a) => GraphNode -> Text -> [Text]

-- Returns list of properties with valid signatures
validProps <- verifyProperties node userPublicKey
-- Only use properties in validProps!
```

---

## 👤 User Space: Encrypted Personal Data

### Concept

Each user has a **private space** in the graph:

```
~{userPubKey}/
├── public/              # Public data (signed but not encrypted)
│   ├── profile
│   └── capabilities
└── private/             # Encrypted data (only user can read)
    ├── needs
    ├── recognitions
    └── preferences
```

### Implementation

```haskell
data UserSpace = UserSpace
  { userKey :: KeyPair
  , privateData :: M.Map Text JSON.Value
  , encryptedData :: M.Map Text Encrypted
  }

-- Store encrypted data
storePrivate :: (MonadRandom m, ToJSON a) => 
  Text -> a -> UserSpace -> m UserSpace

userSpace' <- storePrivate "my-need" needData userSpace

-- Retrieve and decrypt
case retrievePrivate "my-need" userSpace' of
  Just need -> -- Decrypted successfully
  Nothing -> -- Not found or decryption failed
```

### Use Cases

1. **Private Needs:** Store needs before deciding to publish
2. **Draft Recognitions:** Calculate recognitions privately
3. **Personal Preferences:** Store filters and criteria
4. **Allocation History:** Keep private record of past allocations

---

## ✍️ Verifiable Commitments

### Concept

**Problem:** In a decentralized system, anyone can claim anything!
- "I recognized you at 0.9!" (lie)
- "I need 1000 units!" (inflated)
- "I allocated you 10!" (never happened)

**Solution:** Cryptographically signed commitments

```haskell
data VerifiableCommitment = VerifiableCommitment
  { commitmentId :: Text
  , commitmentType :: Text  -- "need", "capacity", "recognition"
  , commitmentData :: JSON.Value
  , commitmentTimestamp :: Integer
  , commitmentSignature :: Text
  , commitmentOwner :: Text  -- Public key
  }
```

### Creating Commitments

```haskell
-- Create a need commitment
commitment <- createCommitment "need" needData userKeyPair

-- Properties:
-- - Signed by user (can't be forged)
-- - Timestamped (prevents replay attacks)
-- - Immutable (tampering breaks signature)
```

### Verifying Commitments

```haskell
-- Check if commitment is authentic
if verifyCommitment commitment
  then -- Valid! Trust this commitment
  else -- Invalid! Reject or warn user
```

### Protocol Integration

**Before crypto:**
```haskell
-- Anyone can publish fake data
publishCommitment fakeNeed  -- No way to verify!
```

**After crypto:**
```haskell
-- Only owner can create valid commitment
commitment <- createCommitment "need" realNeed userKeyPair
publishCommitment commitment

-- Recipients verify before trusting
if verifyCommitment commitment
  then useCommitment commitment
  else rejectFake commitment
```

---

## 🔗 Integration with NetworkedZipper

### Authenticated References

**Before crypto:**
```haskell
data RemoteRef = RemoteRef
  { nodeId :: Text
  , peerAddress :: Text
  }

-- No way to verify authenticity!
```

**After crypto:**
```haskell
data AuthenticatedRef = AuthenticatedRef
  { refNodeId :: Text
  , refOwner :: Text      -- Public key of node owner
  , refSignature :: Text  -- Signature over (nodeId, timestamp)
  , refTimestamp :: Integer
  }

-- Verify before following reference
if verifyRef ref
  then fetchNode ref
  else rejectFakeRef ref
```

### Secure Fetch

```haskell
secureFetch :: AuthenticatedRef -> IO (Maybe GraphNode)

-- 1. Verify reference signature
-- 2. Fetch node from network
-- 3. Verify all property signatures
-- 4. Return only verified data
```

**Properties:**
- Can't fake references
- Can't tamper with fetched data
- Timestamps prevent replay attacks
- Only verified data enters local graph

### Encrypted Navigation

**Navigate to user's private space:**

```haskell
-- Zipper can navigate to encrypted paths
zipper <- focusPath ["~userPubKey", "private", "needs"] rootZipper

-- Automatic decryption if user has key
case getCurrentData zipper of
  Just encryptedData -> 
    case decryptData encryptedData userKeyPair of
      Just needs -> -- Access granted!
      Nothing -> -- Wrong key
  Nothing -> -- Path not found
```

---

## 🔐 Integration with ProtocolCompliant.hs

### Step 0: Publish (Signed)

**Before:**
```haskell
publishCommitment :: Commitment -> NetworkM ()
```

**After:**
```haskell
publishCommitment :: Commitment -> KeyPair -> NetworkM VerifiableCommitment
publishCommitment commitment kp = do
  -- Sign commitment
  signed <- createCommitment "commitment" commitment kp
  
  -- Publish with signature
  networkPublish signed
  
  return signed
```

### Step 1: Fetch (Verified)

**Before:**
```haskell
fetchCommitment :: EntityId -> NetworkM Commitment
```

**After:**
```haskell
fetchCommitment :: EntityId -> NetworkM (Maybe Commitment)
fetchCommitment entityId = do
  -- Fetch signed commitment
  maybeSigned <- networkFetch entityId
  
  case maybeSigned of
    Just signed ->
      -- Verify signature
      if verifyCommitment signed
        then return $ Just (commitmentData signed)
        else do
          logWarning $ "Invalid signature from " ++ entityId
          return Nothing
    Nothing -> return Nothing
```

### Step 2: Filter Compatible (Bilateral Verification)

**Enhanced filter checking:**

```haskell
checkCompatibility :: 
  Timestamp 
  -> ResourceType 
  -> VerifiableCommitment  -- Provider (signed)
  -> VerifiableCommitment  -- Recipient (signed)
  -> Bool
checkCompatibility currentTime resType providerCommit recipientCommit =
  -- Verify both signatures
  verifyCommitment providerCommit
  && verifyCommitment recipientCommit
  -- Then check compatibility
  && checkTimeWindow currentTime (filters $ commitmentData recipientCommit)
  && checkLocation (filters $ commitmentData recipientCommit)
  && checkResourceType resType (filters $ commitmentData recipientCommit)
```

### Step 3: Calculate Allocations (Signed Results)

**Sign allocation decisions:**

```haskell
calculateAllocations :: 
  ProviderState 
  -> KeyPair 
  -> NetworkM (Signed [SlotAllocation])
calculateAllocations provider kp = do
  -- Run 5-step algorithm
  allocations <- providerPhase provider
  
  -- Sign the allocations
  signData allocations kp
```

**Recipients verify:**

```haskell
receiveAllocations :: 
  EntityId 
  -> Text  -- Provider's public key
  -> NetworkM [SlotAllocation]
receiveAllocations providerId providerPubKey = do
  -- Fetch signed allocations
  signedAllocs <- networkFetch providerId
  
  -- Verify signature
  case verifyData signedAllocs providerPubKey of
    Just allocs -> return allocs
    Nothing -> do
      logError $ "Invalid allocation signature from " ++ providerId
      return []
```

### Step 5: Update State (Timestamped)

**Signed state updates:**

```haskell
updateState :: RecipientState -> KeyPair -> NetworkM ()
updateState newState kp = do
  -- Get current timestamp
  now <- getCurrentTimestamp
  
  -- Sign the update
  sig <- signTimestamp now kp
  
  -- Publish with signature
  networkPublish (newState, now, sig)
```

---

## 🌐 Integration with DataReplication.hs

### Signed Data Blocks

**Before:**
```haskell
data DataBlock = DataBlock
  { blockId :: Text
  , blockData :: ByteString
  , blockSize :: Integer
  }
```

**After:**
```haskell
data SignedDataBlock = SignedDataBlock
  { block :: DataBlock
  , blockOwner :: Text
  , blockSignature :: Text
  , blockTimestamp :: Integer
  }

-- Verify before storing
storeBlock :: SignedDataBlock -> ReplicationNode -> IO Bool
storeBlock signedBlock node = do
  if verifyDataBlock signedBlock
    then do
      store (block signedBlock) node
      return True
    else do
      logWarning "Rejecting block with invalid signature"
      return False
```

### Encrypted Private Data

**Replicate with encryption:**

```haskell
-- Alice wants to replicate private data to Bob's storage node
replicatePrivate :: 
  DataBlock 
  -> KeyPair      -- Alice's key pair
  -> KeyPair      -- Bob's public key
  -> IO SignedDataBlock
replicatePrivate block aliceKP bobPubKP = do
  -- Encrypt to Bob
  encrypted <- encryptTo (blockData block) bobPubKP aliceKP
  
  -- Sign the encrypted block
  now <- getCurrentTimestamp
  sig <- signTimestamp now aliceKP
  
  return SignedDataBlock
    { block = block { blockData = encode encrypted }
    , blockOwner = publicKey aliceKP
    , blockSignature = sig
    , blockTimestamp = now
    }
```

### Verifiable Replication Allocations

**Storage nodes verify allocation legitimacy:**

```haskell
-- Recipient claims provider allocated X MB
receiveReplicationAllocation :: 
  ReplicationAllocation 
  -> Text  -- Provider's public key
  -> StorageNode 
  -> IO Bool
receiveReplicationAllocation alloc providerPubKey node = do
  -- Verify allocation signature
  if not (verifyAllocation alloc providerPubKey)
    then do
      logWarning "Rejecting allocation with invalid signature"
      return False
    else do
      -- Accept and store
      acceptAllocation alloc node
      return True
```

---

## 🎯 Security Properties

### 1. Authentication

**Property:** Only the key owner can create valid signatures  
**Implication:** Can't forge commitments, allocations, or recognitions  
**Attack prevented:** Sybil attacks with fake data

### 2. Non-Repudiation

**Property:** Signatures can't be denied later  
**Implication:** Commitments are binding  
**Attack prevented:** "I never said that!"

### 3. Integrity

**Property:** Tampering breaks signatures  
**Implication:** Data can't be altered in transit  
**Attack prevented:** Man-in-the-middle tampering

### 4. Confidentiality

**Property:** Encrypted data is unreadable without key  
**Implication:** Private needs stay private  
**Attack prevented:** Data snooping

### 5. Freshness

**Property:** Timestamps prevent replay attacks  
**Implication:** Old commitments can't be reused  
**Attack prevented:** Replay attacks with stale data

### 6. Bilateral Trust

**Property:** Both parties must sign (no unilateral action)  
**Implication:** Allocations require mutual agreement  
**Attack prevented:** Forced or unwanted allocations

---

## 🚀 Use Cases Enabled

### 1. Private Needs

**Before crypto:**
> "Everyone can see I need help with debt. Embarrassing!"

**After crypto:**
```haskell
-- Store need privately
privateNeed <- storePrivate "debt-help" needData userSpace

-- Decide later whether to publish
if wantToPublish
  then publishCommitment needData userKeyPair
  else keepPrivate
```

### 2. Verified Allocations

**Before crypto:**
> "Provider says they allocated me 10 units, but did they really?"

**After crypto:**
```haskell
-- Provider signs allocation
signedAlloc <- signData allocation providerKeyPair

-- Recipient verifies before trusting
case verifyData signedAlloc providerPubKey of
  Just alloc -> trustAndUse alloc
  Nothing -> rejectFake
```

### 3. Peer-to-Peer Encryption

**Before crypto:**
> "I want to send private recognition to Bob, but others might see!"

**After crypto:**
```haskell
-- Alice encrypts recognition to Bob only
encrypted <- encryptTo recognition bobKeyPair aliceKeyPair

-- Bob (and only Bob) can decrypt
case decryptFrom encrypted aliceKeyPair bobKeyPair of
  Just recognition -> -- Bob sees it
  Nothing -> -- Others can't decrypt
```

### 4. Tamper-Proof History

**Before crypto:**
> "Did I really recognize them at 0.5 or 0.8? Can't remember!"

**After crypto:**
```haskell
-- Every recognition is signed and timestamped
recognition <- createCommitment "recognition" recData userKeyPair

-- Later: Verify original value
if verifyCommitment recognition
  then -- This is the authentic value
  else -- Someone tampered with it
```

### 5. Distributed Trust

**Before crypto:**
> "Who can I trust in this decentralized network?"

**After crypto:**
```haskell
-- Verify every piece of data
verifiedCommitments <- filterM verifyCommitment allCommitments

-- Only use verified data
processCommitments verifiedCommitments
```

---

## 📊 Performance Considerations

### Signing Cost

- **ECDSA signature:** ~1ms per signature
- **Batch signing:** Sign multiple properties at once
- **Optimization:** Sign only changed properties

### Verification Cost

- **ECDSA verification:** ~2ms per verification
- **Caching:** Cache verified data (don't re-verify)
- **Parallel:** Verify multiple signatures concurrently

### Encryption Cost

- **AES-GCM:** ~0.1ms per KB
- **Symmetric:** Much faster than asymmetric
- **Bulk:** Encrypt large blocks efficiently

### Network Overhead

- **Signature size:** ~64 bytes (base64-encoded)
- **Encrypted overhead:** ~32 bytes (IV + auth tag)
- **Acceptable:** <5% overhead for typical commitments

---

## 🔧 Implementation Status

### ✅ Implemented (Stubs)

- Key pair generation structure
- Signing/verification API
- Encryption/decryption API
- User space concept
- Verifiable commitments
- Integration points defined

### 🚧 TODO (Real Crypto)

1. **cryptonite Integration:**
   ```bash
   cabal install cryptonite memory
   ```

2. **Real ECDSA Signing:**
   ```haskell
   import Crypto.PubKey.ECC.ECDSA
   import Crypto.PubKey.ECC.Generate
   import Crypto.PubKey.ECC.Types (SEC_p256r1)
   ```

3. **Real AES-GCM:**
   ```haskell
   import Crypto.Cipher.AES (AES256)
   import Crypto.Cipher.Types (cipherInit, cfbEncrypt)
   ```

4. **Real ECDH:**
   ```haskell
   import Crypto.PubKey.ECC.DH (getShared)
   ```

5. **Key Serialization:**
   - JWK format for interop with TypeScript
   - Base64 encoding for text representation

---

## 🎓 Next Steps

### Phase 1: Core Crypto (High Priority)

1. Integrate `cryptonite` for real crypto
2. Implement ECDSA signing/verification
3. Implement AES-GCM encryption
4. Test with real key pairs

### Phase 2: Protocol Integration

1. Add signatures to `ProtocolCompliant.hs`
2. Verify commitments in Step 1 (Fetch)
3. Sign allocations in Step 3 (Calculate)
4. Timestamp updates in Step 5 (Update)

### Phase 3: Network Integration

1. Extend `NetworkedZipper.hs` with AuthenticatedRef
2. Implement `secureFetch`
3. Add encrypted user spaces
4. Test distributed verification

### Phase 4: Data Replication

1. Sign data blocks in `DataReplication.hs`
2. Encrypt private data for specific replicas
3. Verify allocations before storing
4. Test secure replication

---

## 🌟 Impact

**This cryptographic layer transforms Free Association from a trust-based system to a trustless, verifiable, privacy-preserving coordination infrastructure.**

### Before Crypto

- Trust required
- No forgery prevention
- No privacy
- Public data only
- Centralized verification

### After Crypto

- **Trustless:** Verify don't trust
- **Forgery-proof:** Signatures required
- **Private:** User-encrypted spaces
- **Selective disclosure:** Share what you want
- **Decentralized verification:** Anyone can verify

---

## 📚 References

- **GunDB SEA.js:** Original inspiration
- **ECDSA (P-256):** NIST standard elliptic curve
- **AES-GCM:** Authenticated encryption standard
- **ECDH:** Diffie-Hellman key exchange on elliptic curves
- **cryptonite:** Haskell cryptographic library

---

**That's not just security. That's infrastructure for trustless civilization.** 🔐✨

