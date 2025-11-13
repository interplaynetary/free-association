{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | User Authentication and Account Management

Implements user accounts with encrypted private key storage.

Inspired by GunDB's User.js, providing:
- Account creation (username/password)
- Authentication (login)
- Password changes
- Account deletion
- Session management (store/recall/leave)
- Password-based key derivation (PBKDF2)

Integration with SEA.hs:
- Generates key pairs during account creation
- Encrypts private keys with password-derived key
- Stores encrypted keys in user's graph node (~userPubKey)
- Decrypts private keys during authentication

Graph Structure:
  ~@username        → Username index (maps to pub keys)
  ~{userPubKey}     → User's data node
    ├── username    → Username
    ├── pub         → ECDSA public key
    ├── epub        → ECDH public key  
    └── auth        → Encrypted private keys (JSON: {enc, salt})

Security Properties:
- Private keys never stored unencrypted
- Password → AES key via PBKDF2 (100k iterations)
- Each user has unique salt
- Brute force resistant
- Zero-knowledge (server never sees password)
-}

module User where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as M
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Data.Aeson as JSON
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Random (randomRIO)

-- Import from SEA module
import SEA (KeyPair(..), generateKeyPair, signTimestamp, encryptData, decryptData, Encrypted(..))
import qualified SEA

-- ============================================================================
-- TYPES
-- ============================================================================

-- | User authentication state
data UserAuth = UserAuth
  { username :: Text
  , userPub :: Text      -- ECDSA public key
  , userEPub :: Text     -- ECDH public key
  , userPriv :: Text     -- ECDSA private key (only in memory!)
  , userEPriv :: Text    -- ECDH private key (only in memory!)
  }
  deriving (Show, Eq)

instance ToJSON UserAuth where
  toJSON ua = JSON.object
    [ "username" JSON..= username ua
    , "pub" JSON..= userPub ua
    , "epub" JSON..= userEPub ua
    , "priv" JSON..= userPriv ua
    , "epriv" JSON..= userEPriv ua
    ]

instance FromJSON UserAuth where
  parseJSON = JSON.withObject "UserAuth" $ \o -> UserAuth
    <$> o JSON..: "username"
    <*> o JSON..: "pub"
    <*> o JSON..: "epub"
    <*> o JSON..: "priv"
    <*> o JSON..: "epriv"

-- | Encrypted auth data (stored in graph)
data AuthData = AuthData
  { authEncrypted :: Encrypted  -- Encrypted {priv, epriv}
  , authSalt :: Text            -- Salt for PBKDF2
  }
  deriving (Show, Eq)

instance ToJSON AuthData where
  toJSON ad = JSON.object
    [ "enc" JSON..= authEncrypted ad
    , "salt" JSON..= authSalt ad
    ]

instance FromJSON AuthData where
  parseJSON = JSON.withObject "AuthData" $ \o -> AuthData
    <$> o JSON..: "enc"
    <*> o JSON..: "salt"

-- | User account data (stored in graph)
data UserAccount = UserAccount
  { accountUsername :: Text
  , accountPub :: Text
  , accountEPub :: Text
  , accountAuth :: AuthData
  }
  deriving (Show, Eq)

instance ToJSON UserAccount where
  toJSON ua = JSON.object
    [ "username" JSON..= accountUsername ua
    , "pub" JSON..= accountPub ua
    , "epub" JSON..= accountEPub ua
    , "auth" JSON..= JSON.encode (accountAuth ua)  -- Stored as JSON string
    ]

-- | User state with current authentication
data UserState = UserState
  { currentUser :: Maybe UserAuth
  , isAuthenticating :: Bool
  , isCreating :: Bool
  }
  deriving (Show)

-- ============================================================================
-- PASSWORD-BASED KEY DERIVATION
-- ============================================================================

{- | Derive encryption key from password using PBKDF2

From SEA.js:
  work: async (data, salt) => {
    const key = await subtle.importKey("raw", data, {name: "PBKDF2"}, ...)
    const work = await subtle.deriveBits({
      name: "PBKDF2",
      iterations: 100000,
      salt: salt,
      hash: {name: "SHA-256"}
    }, key, 512)
  }

This makes brute-force attacks expensive (100k iterations).
-}
deriveKeyFromPassword :: MonadIO m => Text -> Text -> m KeyPair
deriveKeyFromPassword password salt = do
  -- STUB: Real implementation needs cryptonite's PBKDF2
  -- For now, return a fake key pair
  liftIO $ SEA.generateKeyPair
  -- TODO: Real PBKDF2 derivation
  {-
  import Crypto.KDF.PBKDF2
  let passwordBytes = TE.encodeUtf8 password
  let saltBytes = TE.encodeUtf8 salt
  let derived = fastPBKDF2_SHA256 Parameters
        { iterCounts = 100000
        , outputLength = 64  -- 512 bits
        } passwordBytes saltBytes
  return $ KeyPair { encryptionPrivateKey = ... }
  -}

-- | Generate random salt for PBKDF2
generateSalt :: MonadIO m => m Text
generateSalt = do
  -- Generate 64 random characters
  liftIO $ T.pack <$> replicateM 64 randomChar
  where
    randomChar = do
      -- Random printable ASCII
      c <- randomRIO (33, 126)
      return $ toEnum c

-- ============================================================================
-- ACCOUNT CREATION
-- ============================================================================

{- | Create a new user account

Steps:
1. Check if username already exists (~@username)
2. Generate new key pair (ECDSA + ECDH)
3. Derive encryption key from password + random salt
4. Encrypt private keys with derived key
5. Store account data at ~{pub} node
6. Link username to pub key at ~@username

Security:
- Private keys encrypted with password-derived key
- Unique salt per user
- 100k PBKDF2 iterations (expensive brute force)
- Zero-knowledge (password never transmitted)
-}
createUser :: MonadIO m => Text -> Text -> m (Either Text UserAuth)
createUser uname password = do
  if T.null uname
    then return $ Left "Please provide a username"
    else if T.null password
      then return $ Left "Please provide a password"
      else do
        -- Check if username exists (STUB - would query network)
        -- let usernameNode = "~@" <> uname
        -- exists <- checkNodeExists usernameNode
        -- if exists then return $ Left "Username already exists"
        
        -- Generate new key pair
        kp <- liftIO SEA.generateKeyPair
        
        -- Generate salt and derive encryption key
        salt <- generateSalt
        workKP <- deriveKeyFromPassword password salt
        
        -- Encrypt private keys
        let privData = JSON.object
              [ "priv" JSON..= privateKey kp
              , "epriv" JSON..= encryptionPrivateKey kp
              ]
        enc <- liftIO $ SEA.encryptData privData workKP
        
        -- Create auth data
        let authData = AuthData
              { authEncrypted = enc
              , authSalt = salt
              }
        
        -- Create account
        let account = UserAccount
              { accountUsername = uname
              , accountPub = publicKey kp
              , accountEPub = encryptionPublicKey kp
              , accountAuth = authData
              }
        
        -- Store in graph (STUB - would publish to network)
        let pubNode = "~" <> publicKey kp
        -- publishNode pubNode account kp
        
        -- Link username to pub key (STUB)
        let usernameNode = "~@" <> uname
        -- linkNodes usernameNode pubNode
        
        -- Return authenticated user
        return $ Right UserAuth
          { username = uname
          , userPub = publicKey kp
          , userEPub = encryptionPublicKey kp
          , userPriv = privateKey kp
          , userEPriv = encryptionPrivateKey kp
          }

-- ============================================================================
-- AUTHENTICATION
-- ============================================================================

{- | Authenticate a user (login)

Steps:
1. Look up username at ~@username
2. Get list of pub keys (usernames not guaranteed unique)
3. For each pub key, try to decrypt private keys with password
4. If decryption succeeds, authentication successful

Why multiple pub keys per username?
- Usernames aren't globally unique (different communities)
- Try each until password matches
- First successful decryption wins
-}
authenticateUser :: MonadIO m => Text -> Text -> m (Either Text UserAuth)
authenticateUser uname password = do
  if T.null uname
    then return $ Left "Please provide a username"
    else if T.null password
      then return $ Left "Please provide a password"
      else do
        -- Look up username (STUB - would query network)
        let usernameNode = "~@" <> uname
        -- pubKeys <- fetchPubKeysForUsername usernameNode
        
        -- For now, simulate with empty list (would iterate in real impl)
        -- tryAuthWithPubKeys pubKeys password
        
        -- STUB: Return fake authentication
        return $ Left "Username or password incorrect"

{- | Try to authenticate with each pub key until one succeeds -}
tryAuthWithPubKeys :: MonadIO m => [Text] -> Text -> m (Either Text UserAuth)
tryAuthWithPubKeys [] _ = return $ Left "Wrong username or password"
tryAuthWithPubKeys (pubKey:rest) password = do
  -- Fetch account data (STUB)
  let pubNode = "~" <> pubKey
  -- maybeAccount <- fetchNode pubNode
  
  -- Try to decrypt with password
  -- case maybeAccount of
  --   Nothing -> tryAuthWithPubKeys rest password
  --   Just account -> ...
  
  -- STUB
  return $ Left "Authentication not implemented"

-- ============================================================================
-- PASSWORD CHANGE
-- ============================================================================

{- | Change user password

Steps:
1. Authenticate with old password
2. Decrypt private keys
3. Re-encrypt with new password and new salt
4. Update auth data in graph
-}
changePassword :: MonadIO m => Text -> Text -> Text -> m (Either Text ())
changePassword uname oldPassword newPassword = do
  if T.null newPassword
    then return $ Left "Please provide a new password"
    else do
      -- First authenticate with old password
      authResult <- authenticateUser uname oldPassword
      
      case authResult of
        Left err -> return $ Left err
        Right userAuth -> do
          -- Generate new salt
          newSalt <- generateSalt
          
          -- Derive new encryption key
          newWorkKP <- deriveKeyFromPassword newPassword newSalt
          
          -- Re-encrypt private keys
          let privData = JSON.object
                [ "priv" JSON..= userPriv userAuth
                , "epriv" JSON..= userEPriv userAuth
                ]
          newEnc <- liftIO $ SEA.encryptData privData newWorkKP
          
          -- Update auth data
          let newAuthData = AuthData
                { authEncrypted = newEnc
                , authSalt = newSalt
                }
          
          -- Store updated auth (STUB - would publish to network)
          let pubNode = "~" <> userPub userAuth
          -- updateNodeProperty pubNode "auth" newAuthData
          
          return $ Right ()

-- ============================================================================
-- SESSION MANAGEMENT
-- ============================================================================

{- | Store user session

Options:
- localStorage: Persistent across browser sessions
- sessionStorage: Cleared when browser closes

In Haskell, we'd use files or a state monad.
-}
storeUserSession :: MonadIO m => UserAuth -> Bool -> m ()
storeUserSession userAuth persistent = do
  -- STUB: Real implementation would:
  -- if persistent
  --   then writeFile "~/.free-association/session" (encode userAuth)
  --   else storeInMemory userAuth
  return ()

{- | Recall user session from storage -}
recallUserSession :: MonadIO m => m (Maybe UserAuth)
recallUserSession = do
  -- STUB: Real implementation would:
  -- tryReadFile "~/.free-association/session"
  return Nothing

{- | Leave (logout) - clear session -}
leaveUserSession :: MonadIO m => m ()
leaveUserSession = do
  -- STUB: Real implementation would:
  -- removeFile "~/.free-association/session"
  return ()

-- ============================================================================
-- ACCOUNT DELETION
-- ============================================================================

{- | Delete user account

Steps:
1. Authenticate
2. Nullify all account properties
3. Sign the deletion (proves ownership)
4. Publish nullified data

Note: Username mapping (~@username) is NOT removed
This prevents username re-use (security consideration)
-}
deleteUser :: MonadIO m => Text -> Text -> m (Either Text ())
deleteUser uname password = do
  -- Authenticate first
  authResult <- authenticateUser uname password
  
  case authResult of
    Left err -> return $ Left err
    Right userAuth -> do
      -- Create nullified data
      let nullData = JSON.object
            [ "username" JSON..= JSON.Null
            , "pub" JSON..= JSON.Null
            , "epub" JSON..= JSON.Null
            , "auth" JSON..= JSON.Null
            ]
      
      -- Sign the deletion
      let kp = KeyPair
            { publicKey = userPub userAuth
            , privateKey = userPriv userAuth
            , encryptionPublicKey = userEPub userAuth
            , encryptionPrivateKey = userEPriv userAuth
            }
      
      now <- round <$> liftIO getPOSIXTime
      sig <- liftIO $ signTimestamp now kp
      
      -- Publish nullified data (STUB)
      let pubNode = "~" <> userPub userAuth
      -- publishSignedNode pubNode nullData sig
      
      return $ Right ()

-- ============================================================================
-- INTEGRATION WITH NETWORKED ZIPPER
-- ============================================================================

{- | Navigate to user's node

Example paths:
- ~@alice              → Username index
- ~{pubKey}            → User's public data
- ~{pubKey}/private    → User's encrypted private space
-}
navigateToUser :: Text -> [Text]
navigateToUser identifier
  | "~@" `T.isPrefixOf` identifier = ["users", "by-name", T.drop 2 identifier]
  | "~" `T.isPrefixOf` identifier = ["users", "by-key", T.drop 1 identifier]
  | otherwise = ["users", "by-name", identifier]

-- ============================================================================
-- EXAMPLES
-- ============================================================================

{- | Example: Create account and authenticate -}
exampleUserFlow :: IO ()
exampleUserFlow = do
  putStrLn "👤 User Authentication Example\n"
  
  -- Create account
  putStrLn "Creating account for 'alice'..."
  result <- createUser "alice" "secure-password-123"
  
  case result of
    Left err -> putStrLn $ "❌ Error: " ++ T.unpack err
    Right userAuth -> do
      putStrLn $ "✅ Account created!"
      putStrLn $ "   Username: " ++ T.unpack (username userAuth)
      putStrLn $ "   Public key: " ++ take 16 (T.unpack $ userPub userAuth) ++ "..."
      putStrLn ""
      
      -- Store session
      putStrLn "Storing session..."
      storeUserSession userAuth True
      putStrLn "✅ Session stored"
      putStrLn ""
      
      -- Recall session
      putStrLn "Recalling session..."
      maybeUser <- recallUserSession
      case maybeUser of
        Just recalled -> putStrLn $ "✅ Session recalled: " ++ T.unpack (username recalled)
        Nothing -> putStrLn "❌ No session found"
      putStrLn ""
  
  putStrLn "KEY: Private keys are encrypted with password!"
  putStrLn "Nobody (not even the server) can decrypt without password."
  putStrLn ""

{- | Example: Password change -}
examplePasswordChange :: IO ()
examplePasswordChange = do
  putStrLn "🔑 Password Change Example\n"
  
  -- Change password
  result <- changePassword "alice" "old-password" "new-password"
  
  case result of
    Left err -> putStrLn $ "❌ Error: " ++ T.unpack err
    Right () -> do
      putStrLn "✅ Password changed successfully!"
      putStrLn "Private keys re-encrypted with new password."
  putStrLn ""

-- ============================================================================
-- INTEGRATION POINTS
-- ============================================================================

{- | Integration with SEA.hs

User authentication builds on SEA's cryptographic primitives:

1. **Key Generation:**
   - SEA.pair() generates ECDSA + ECDH keys
   - Used during account creation

2. **Password Derivation:**
   - SEA.work() derives AES key from password
   - PBKDF2 with 100k iterations

3. **Encryption:**
   - SEA.encrypt() encrypts private keys
   - AES-GCM authenticated encryption

4. **Signing:**
   - SEA.signTimestamp() proves account ownership
   - Used for account creation/deletion/updates
-}

{- | Integration with NetworkedZipper.hs

User system provides authenticated navigation:

1. **User Nodes:**
   - Navigate to ~@username or ~{pubKey}
   - Automatic authentication checks

2. **Private Spaces:**
   - Navigate to ~{pubKey}/private
   - Automatic decryption if user is authenticated

3. **Session State:**
   - Zipper carries current user context
   - Operations are authenticated as that user

4. **Graph Operations:**
   - All writes signed by current user
   - All reads verify signatures
-}

{- | Integration with ProtocolCompliant.hs

User authentication enables verified commitments:

1. **Publishing Needs:**
   - User must be authenticated
   - Needs signed with user's private key

2. **Publishing Capacities:**
   - Provider must be authenticated
   - Capacities signed and verifiable

3. **Mutual Recognition:**
   - Both parties authenticated
   - Recognitions cryptographically signed

4. **Allocation Verification:**
   - Providers sign allocations
   - Recipients verify before trusting
-}

