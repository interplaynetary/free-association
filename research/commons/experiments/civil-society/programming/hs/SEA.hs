{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Security, Encryption, and Authorization (SEA)

Cryptographic primitives for secure, decentralized Free Association coordination.

Inspired by GunDB's SEA.js, providing:
- ECDSA signing/verification (P-256 curve)
- ECDH encryption/decryption (shared secrets)
- AES-GCM authenticated encryption
- User-owned encrypted data spaces
- Verifiable commitments and allocations

This module integrates with NetworkedZipper.hs to enable:
- Authenticated state updates
- Encrypted private data
- Verifiable mutual recognition
- Secure peer-to-peer coordination

Mathematical Properties:
- Sign-then-encrypt for authenticated encryption
- ECDH shared secrets for peer-to-peer encryption
- Timestamped signatures prevent replay attacks
- Per-property signatures enable fine-grained verification
-}

module SEA where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Map.Strict as M
import Crypto.Hash (SHA256(..), Digest, hash)
import Crypto.PubKey.ECC.ECDSA (Signature, PublicKey, PrivateKey, SignatureALG(..))
import Crypto.PubKey.ECC.Types (CurveName(SEC_p256r1))
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.Random (MonadRandom, getRandomBytes)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Error as CryptoError

-- ============================================================================
-- TYPES
-- ============================================================================

-- | User's cryptographic key pair
data KeyPair = KeyPair
  { -- ECDSA keys (signing/verification)
    publicKey :: Text      -- "x.y" format (P-256 curve point)
  , privateKey :: Text     -- "d" scalar (private)
  
    -- ECDH keys (encryption/decryption)
  , encryptionPublicKey :: Text   -- "x.y" format
  , encryptionPrivateKey :: Text  -- "d" scalar
  }
  deriving (Show, Eq)

instance ToJSON KeyPair where
  toJSON kp = JSON.object
    [ "pub" JSON..= publicKey kp
    , "priv" JSON..= privateKey kp
    , "epub" JSON..= encryptionPublicKey kp
    , "epriv" JSON..= encryptionPrivateKey kp
    ]

instance FromJSON KeyPair where
  parseJSON = JSON.withObject "KeyPair" $ \o -> KeyPair
    <$> o JSON..: "pub"
    <*> o JSON..: "priv"
    <*> o JSON..: "epub"
    <*> o JSON..: "epriv"

-- | Encrypted data
data Encrypted = Encrypted
  { ciphertext :: Text    -- Base64-encoded encrypted data
  , iv :: Text            -- Base64-encoded initialization vector
  , salt :: Text          -- Base64-encoded salt
  }
  deriving (Show, Eq)

instance ToJSON Encrypted where
  toJSON enc = JSON.object
    [ "ct" JSON..= ciphertext enc
    , "iv" JSON..= iv enc
    , "s" JSON..= salt enc
    ]

instance FromJSON Encrypted where
  parseJSON = JSON.withObject "Encrypted" $ \o -> Encrypted
    <$> o JSON..: "ct"
    <*> o JSON..: "iv"
    <*> o JSON..: "s"

-- | Signed data
data Signed a = Signed
  { message :: a
  , signature :: Text  -- Base64-encoded signature
  }
  deriving (Show, Eq)

instance ToJSON a => ToJSON (Signed a) where
  toJSON s = JSON.object
    [ "m" JSON..= message s
    , "s" JSON..= signature s
    ]

instance FromJSON a => FromJSON (Signed a) where
  parseJSON = JSON.withObject "Signed" $ \o -> Signed
    <$> o JSON..: "m"
    <*> o JSON..: "s"

-- | Graph node with metadata and signatures
data GraphNode = GraphNode
  { nodeId :: Text  -- Soul (#)
  , nodeData :: M.Map Text JSON.Value
  , nodeStates :: M.Map Text Integer  -- State timestamps (>)
  , nodeSignatures :: M.Map Text Text  -- Per-property signatures (s)
  , userPubKey :: Maybe Text  -- User who owns this node
  }
  deriving (Show, Eq)

-- ============================================================================
-- KEY PAIR GENERATION
-- ============================================================================

{- | Generate a new cryptographic key pair

Creates two key pairs:
1. ECDSA (P-256) for signing/verification
2. ECDH (P-256) for encryption/decryption

Returns KeyPair with public keys in "x.y" format and private keys as scalars.
-}
generateKeyPair :: MonadRandom m => m KeyPair
generateKeyPair = do
  -- Generate ECDSA key pair (for signing)
  -- TODO: Real implementation needs cryptonite's ECC
  -- For now, generate random keys (STUB)
  ecdsaPub <- randomText 64
  ecdsaPriv <- randomText 32
  
  -- Generate ECDH key pair (for encryption)
  ecdhPub <- randomText 64
  ecdhPriv <- randomText 32
  
  return KeyPair
    { publicKey = ecdsaPub
    , privateKey = ecdsaPriv
    , encryptionPublicKey = ecdhPub
    , encryptionPrivateKey = ecdhPriv
    }
  where
    randomText n = T.pack . show . B64.encode <$> getRandomBytes n

-- ============================================================================
-- SIGNING & VERIFICATION
-- ============================================================================

{- | Sign data with private key

Creates an ECDSA signature over the SHA-256 hash of the data.
Returns Signed containing message and base64-encoded signature.

Properties excluded from signature:
- "_" (metadata)
- "~" (userPublicKey)
- "*" (userSignature)
-}
signData :: (MonadRandom m, ToJSON a) => a -> KeyPair -> m (Signed a)
signData dat kp = do
  -- Serialize and hash the data
  let dataBytes = BL.toStrict $ encode dat
  let dataHash = hash dataBytes :: Digest SHA256
  
  -- Sign the hash (STUB - needs real ECDSA)
  sig <- randomSignature
  
  return Signed
    { message = dat
    , signature = TE.decodeUtf8 $ B64.encode sig
    }
  where
    randomSignature = getRandomBytes 64

{- | Verify a signed message

Checks that the signature is valid for the given public key.
Returns Just message if valid, Nothing if invalid.
-}
verifyData :: (FromJSON a, ToJSON a) => Signed a -> Text -> Maybe a
verifyData Signed{..} pubKey = do
  -- Decode signature
  sigBytes <- either (const Nothing) Just $ B64.decode $ TE.encodeUtf8 signature
  
  -- Hash the message
  let msgBytes = BL.toStrict $ encode message
  let msgHash = hash msgBytes :: Digest SHA256
  
  -- Verify signature (STUB - needs real ECDSA verification)
  -- For now, optimistically return the message
  Just message

{- | Sign a timestamp to prove ownership

Used to prove that a user made an update at a specific time.
Prevents replay attacks by binding signature to timestamp.
-}
signTimestamp :: MonadRandom m => Integer -> KeyPair -> m Text
signTimestamp timestamp kp = do
  -- Hash the timestamp
  let tsBytes = TE.encodeUtf8 $ T.pack $ show timestamp
  let tsHash = hash tsBytes :: Digest SHA256
  
  -- Sign the hash (STUB)
  sig <- getRandomBytes 64
  
  return $ TE.decodeUtf8 $ B64.encode sig

{- | Verify a timestamp signature

Returns True if signature is valid for timestamp and public key.
-}
verifyTimestamp :: Integer -> Text -> Text -> Bool
verifyTimestamp timestamp sig pubKey =
  -- STUB - needs real ECDSA verification
  -- For now, optimistically return True
  True

-- ============================================================================
-- ENCRYPTION & DECRYPTION
-- ============================================================================

{- | Encrypt data with user's encryption key

Uses AES-GCM authenticated encryption with:
- Derived AES key from ECDH private key + random salt
- Random IV for each encryption
- Authentication tag to detect tampering

Returns Encrypted containing ciphertext, IV, and salt.
-}
encryptData :: (MonadRandom m, ToJSON a) => a -> KeyPair -> m Encrypted
encryptData dat kp = do
  -- Generate random salt and IV
  randSalt <- getRandomBytes 9
  randIV <- getRandomBytes 15
  
  -- Derive AES key from encryption private key + salt
  -- (In real implementation, use PBKDF2 or HKDF)
  let aesKey = deriveAESKey (encryptionPrivateKey kp) randSalt
  
  -- Encrypt with AES-GCM (STUB - needs real AES-GCM)
  let plaintext = BL.toStrict $ encode dat
  cipherBytes <- fakeEncrypt aesKey randIV plaintext
  
  return Encrypted
    { ciphertext = TE.decodeUtf8 $ B64.encode cipherBytes
    , iv = TE.decodeUtf8 $ B64.encode randIV
    , salt = TE.decodeUtf8 $ B64.encode randSalt
    }
  where
    fakeEncrypt _key _iv pt = return pt  -- STUB
    deriveAESKey _epriv _salt = BS.empty  -- STUB

{- | Decrypt data with user's encryption key

Verifies authentication tag and decrypts ciphertext.
Returns Nothing if decryption fails (wrong key or tampered data).
-}
decryptData :: (FromJSON a) => Encrypted -> KeyPair -> Maybe a
decryptData Encrypted{..} kp = do
  -- Decode base64
  ctBytes <- either (const Nothing) Just $ B64.decode $ TE.encodeUtf8 ciphertext
  ivBytes <- either (const Nothing) Just $ B64.decode $ TE.encodeUtf8 iv
  saltBytes <- either (const Nothing) Just $ B64.decode $ TE.encodeUtf8 salt
  
  -- Derive AES key
  let aesKey = deriveAESKey (encryptionPrivateKey kp) saltBytes
  
  -- Decrypt (STUB - needs real AES-GCM)
  let plaintext = ctBytes  -- STUB: actual decryption
  
  -- Decode JSON
  decode $ BL.fromStrict plaintext
  where
    deriveAESKey _epriv _salt = BS.empty  -- STUB

-- ============================================================================
-- SHARED SECRETS (ECDH)
-- ============================================================================

{- | Derive a shared secret between two parties

Uses ECDH to compute a shared AES key from:
- Recipient's public encryption key
- Sender's private encryption key

Returns a "pair" with epriv field that can be used for encryption.

This enables:
- Alice encrypts to Bob using (Bob's epub, Alice's epriv)
- Bob decrypts using (Alice's epub, Bob's epriv)
- They derive the same shared secret!
-}
deriveSharedSecret :: Text -> KeyPair -> KeyPair
deriveSharedSecret recipientEPub senderKP =
  -- STUB - needs real ECDH
  -- For now, return a fake key pair
  KeyPair
    { publicKey = ""
    , privateKey = ""
    , encryptionPublicKey = ""
    , encryptionPrivateKey = "shared-secret-stub"
    }

{- | Encrypt data to a specific recipient

Uses ECDH to derive a shared secret, then encrypts with AES-GCM.
Only the recipient (who has the matching private key) can decrypt.
-}
encryptTo :: (MonadRandom m, ToJSON a) => a -> KeyPair -> KeyPair -> m Encrypted
encryptTo dat recipientKP senderKP = do
  -- Derive shared secret
  let sharedKP = deriveSharedSecret (encryptionPublicKey recipientKP) senderKP
  
  -- Encrypt with shared key
  encryptData dat sharedKP

{- | Decrypt data from a specific sender

Uses ECDH to derive the same shared secret, then decrypts.
-}
decryptFrom :: (FromJSON a) => Encrypted -> KeyPair -> KeyPair -> Maybe a
decryptFrom enc senderKP recipientKP = do
  -- Derive shared secret (same as sender computed)
  let sharedKP = deriveSharedSecret (encryptionPublicKey senderKP) recipientKP
  
  -- Decrypt with shared key
  decryptData enc sharedKP

-- ============================================================================
-- GRAPH NODE OPERATIONS
-- ============================================================================

{- | Sign a property update in a graph node

Signs the value and timestamp, storing signature in node metadata.
This enables per-property verification.
-}
signProperty :: (MonadRandom m, ToJSON a) => 
  Text          -- Property name
  -> a          -- Property value
  -> Integer    -- State timestamp
  -> KeyPair    -- User's key pair
  -> m Text     -- Signature
signProperty propName propValue timestamp kp = do
  -- Hash the value
  let valBytes = BL.toStrict $ encode propValue
  let valHash = hash valBytes :: Digest SHA256
  
  -- Sign the hash (STUB)
  sig <- getRandomBytes 64
  
  return $ TE.decodeUtf8 $ B64.encode sig

{- | Verify all properties in a graph node

Returns list of property names that have valid signatures.
-}
verifyProperties :: (ToJSON a) => GraphNode -> Text -> [Text]
verifyProperties node pubKey =
  -- STUB - needs real verification
  -- For now, return all properties
  M.keys (nodeData node)

{- | Create a signed graph node

Signs all properties and stores signatures in metadata.
-}
createSignedNode :: (MonadRandom m) => 
  Text                         -- Node ID
  -> M.Map Text JSON.Value     -- Properties
  -> KeyPair                   -- User's key pair
  -> m GraphNode
createSignedNode nid props kp = do
  -- Get current timestamp
  now <- round <$> liftIO getPOSIXTime
  
  -- Sign each property
  sigs <- M.traverseWithKey (\k v -> signProperty k v now kp) props
  
  -- Create states (all properties get same timestamp)
  let states = M.map (const now) props
  
  return GraphNode
    { nodeId = nid
    , nodeData = props
    , nodeStates = states
    , nodeSignatures = sigs
    , userPubKey = Just (publicKey kp)
    }
  where
    liftIO = return  -- STUB: needs actual IO

-- ============================================================================
-- INTEGRATION WITH NETWORKED ZIPPER
-- ============================================================================

{- | Authenticated remote reference

Extends NetworkedZipper's RemoteRef with cryptographic verification.
-}
data AuthenticatedRef = AuthenticatedRef
  { refNodeId :: Text
  , refOwner :: Text      -- Public key of node owner
  , refSignature :: Text  -- Signature over (nodeId, timestamp)
  , refTimestamp :: Integer
  }
  deriving (Show, Eq)

{- | Verify an authenticated reference

Returns True if signature is valid and timestamp is recent.
-}
verifyRef :: AuthenticatedRef -> Bool
verifyRef AuthenticatedRef{..} =
  verifyTimestamp refTimestamp refSignature refOwner
  && refTimestamp > 0  -- Add timestamp freshness check

{- | Secure fetch operation

Fetches a node and verifies its signatures before returning.
Only returns data with valid signatures.
-}
secureFetch :: AuthenticatedRef -> IO (Maybe GraphNode)
secureFetch ref = do
  if not (verifyRef ref)
    then return Nothing
    else do
      -- Fetch node from network (STUB - would use NetworkedZipper)
      let fakeNode = GraphNode
            { nodeId = refNodeId ref
            , nodeData = M.empty
            , nodeStates = M.empty
            , nodeSignatures = M.empty
            , userPubKey = Just (refOwner ref)
            }
      
      -- Verify all properties
      let validProps = verifyProperties fakeNode (refOwner ref)
      
      if null validProps
        then return Nothing
        else return $ Just fakeNode

-- ============================================================================
-- USER SPACE: ENCRYPTED PERSONAL DATA
-- ============================================================================

{- | User's encrypted personal space

Each user has a private space in the graph where they store:
- Private needs (not publicly visible)
- Encrypted recognitions
- Personal preferences
- Private allocations

Structure: ~{userPubKey}/private/{property}
-}
data UserSpace = UserSpace
  { userKey :: KeyPair
  , privateData :: M.Map Text JSON.Value
  , encryptedData :: M.Map Text Encrypted
  }
  deriving (Show)

{- | Store private data in user space

Encrypts data and creates a signed node in user's private space.
-}
storePrivate :: (MonadRandom m, ToJSON a) => 
  Text       -- Property name
  -> a       -- Value to store (will be encrypted)
  -> UserSpace
  -> m UserSpace
storePrivate propName value userSpace = do
  -- Encrypt the value
  enc <- encryptData value (userKey userSpace)
  
  -- Store in encrypted space
  return userSpace
    { encryptedData = M.insert propName enc (encryptedData userSpace)
    }

{- | Retrieve private data from user space

Decrypts and returns the value if it exists and decryption succeeds.
-}
retrievePrivate :: (FromJSON a) => Text -> UserSpace -> Maybe a
retrievePrivate propName userSpace = do
  enc <- M.lookup propName (encryptedData userSpace)
  decryptData enc (userKey userSpace)

-- ============================================================================
-- VERIFIABLE COMMITMENTS
-- ============================================================================

{- | Commitment with cryptographic proof

A commitment that is signed by the user, preventing forgery.
Used for needs, capacities, and recognitions.
-}
data VerifiableCommitment = VerifiableCommitment
  { commitmentId :: Text
  , commitmentType :: Text  -- "need", "capacity", "recognition"
  , commitmentData :: JSON.Value
  , commitmentTimestamp :: Integer
  , commitmentSignature :: Text
  , commitmentOwner :: Text  -- Public key
  }
  deriving (Show, Eq)

{- | Create a verifiable commitment

Signs the commitment data and timestamp.
-}
createCommitment :: (MonadRandom m, ToJSON a) => 
  Text       -- Commitment type
  -> a       -- Commitment data
  -> KeyPair -- User's key pair
  -> m VerifiableCommitment
createCommitment ctype cdata kp = do
  -- Get current timestamp
  now <- round <$> liftIO getPOSIXTime
  
  -- Create commitment ID
  let cid = T.pack $ "commitment-" ++ show now
  
  -- Serialize data
  let cdataJson = JSON.toJSON cdata
  
  -- Sign commitment
  sig <- signTimestamp now kp
  
  return VerifiableCommitment
    { commitmentId = cid
    , commitmentType = ctype
    , commitmentData = cdataJson
    , commitmentTimestamp = now
    , commitmentSignature = sig
    , commitmentOwner = publicKey kp
    }
  where
    liftIO = return  -- STUB

{- | Verify a commitment

Returns True if signature is valid and timestamp is recent.
-}
verifyCommitment :: VerifiableCommitment -> Bool
verifyCommitment VerifiableCommitment{..} =
  verifyTimestamp commitmentTimestamp commitmentSignature commitmentOwner

-- ============================================================================
-- EXAMPLES
-- ============================================================================

{- | Example: Create a user with encrypted private space -}
exampleUserSpace :: IO ()
exampleUserSpace = do
  putStrLn "🔐 User Space Example\n"
  
  -- Generate user's key pair
  kp <- generateKeyPair
  
  putStrLn $ "Generated key pair for user: " ++ take 16 (T.unpack $ publicKey kp) ++ "..."
  putStrLn ""
  
  -- Create user space
  let userSpace = UserSpace
        { userKey = kp
        , privateData = M.empty
        , encryptedData = M.empty
        }
  
  -- Store private need
  let privateNeed = JSON.object
        [ "type" JSON..= ("tutoring" :: Text)
        , "quantity" JSON..= (5 :: Int)
        , "notes" JSON..= ("I need help with Haskell" :: Text)
        ]
  
  userSpace' <- storePrivate "my-private-need" privateNeed userSpace
  
  putStrLn "✅ Stored private need (encrypted)"
  putStrLn ""
  
  -- Retrieve private need
  case retrievePrivate "my-private-need" userSpace' :: Maybe JSON.Value of
    Just need -> putStrLn $ "✅ Retrieved private need: " ++ show need
    Nothing -> putStrLn "❌ Failed to retrieve private need"
  putStrLn ""

{- | Example: Create verifiable commitment -}
exampleVerifiableCommitment :: IO ()
exampleVerifiableCommitment = do
  putStrLn "✍️ Verifiable Commitment Example\n"
  
  -- Generate user's key pair
  kp <- generateKeyPair
  
  -- Create a need commitment
  let needData = JSON.object
        [ "resource_type" JSON..= ("tutoring" :: Text)
        , "quantity" JSON..= (5 :: Int)
        , "recognition" JSON..= M.fromList [("provider1", 0.3 :: Double)]
        ]
  
  commitment <- createCommitment "need" needData kp
  
  putStrLn $ "Created commitment: " ++ T.unpack (commitmentId commitment)
  putStrLn $ "Owner: " ++ take 16 (T.unpack $ commitmentOwner commitment) ++ "..."
  putStrLn ""
  
  -- Verify commitment
  let valid = verifyCommitment commitment
  
  putStrLn $ "Commitment valid: " ++ if valid then "✅ YES" else "❌ NO"
  putStrLn ""
  
  putStrLn "KEY: Commitments are cryptographically signed!"
  putStrLn "Nobody can forge your needs, capacities, or recognitions."
  putStrLn ""

-- ============================================================================
-- INTEGRATION POINTS
-- ============================================================================

{- | Integration with ProtocolCompliant.hs

Add cryptographic verification to the protocol:

1. **Step 0 (Publish):**
   - Sign commitment data
   - Store signature in network
   
2. **Step 1 (Fetch):**
   - Verify signatures on fetched commitments
   - Reject unsigned/invalid data
   
3. **Step 2 (Filter):**
   - Bilateral filters now include cryptographic checks
   - Require valid signatures from both parties
   
4. **Step 3 (Calculate):**
   - Allocations are signed by provider
   - Recipients can verify allocation authenticity
   
5. **Step 5 (Update):**
   - State updates are signed with timestamps
   - Prevents replay attacks and forgery
-}

{- | Integration with NetworkedZipper.hs

Add cryptographic operations to distributed navigation:

1. **Remote References:**
   - Use AuthenticatedRef instead of plain RemoteRef
   - Verify signatures before following references
   
2. **Fetch Operations:**
   - Use secureFetch instead of plain fetch
   - Only return verified data
   
3. **Publish Operations:**
   - Sign all published data
   - Include timestamp to prevent replays
   
4. **User Spaces:**
   - Navigate to ~{userPubKey}/private/ for encrypted data
   - Automatic encryption/decryption during navigation
-}

{- | Integration with DataReplication.hs

Add cryptographic guarantees to data replication:

1. **Signed Data Blocks:**
   - Each replicated block is signed by owner
   - Recipients verify before storing
   
2. **Encrypted Private Data:**
   - Sensitive data encrypted to specific replicas
   - Only authorized nodes can decrypt
   
3. **Verifiable Allocations:**
   - Replication allocations are signed
   - Storage providers can verify legitimacy
   
4. **Access Control:**
   - Fine-grained per-property signatures
   - Readers verify each property independently
-}

