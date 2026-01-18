{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Integrated Protocol - Minimal Elegant Integration Layer

Connects all modules into a cohesive system:
- ProtocolCompliant.hs (core algorithm)
- EnhancedMatching.hs (bilateral filters, recurrence)
- User.hs (authentication)
- SEA.hs (signatures)
- Radix.hs (indexing)
- NetworkedZipper.hs (network operations)

Key design principles:
- Type-safe: Compiler enforces correct usage
- Clean separation: Each module's types stay pure
- Stub-friendly: Works with or without real implementations
- Minimal: Only essential glue code
-}

module IntegratedProtocol where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Import from existing modules
import qualified ProtocolCompliant as PC
import qualified EnhancedMatching as EM
import qualified User
import qualified SEA
import qualified Radix
import qualified NetworkedZipper as NZ

-- ============================================================================
-- UNIFIED SYSTEM CONTEXT
-- ============================================================================

{- | SystemContext - Unified state for the entire system

Carries all necessary state for running the integrated protocol:
- Current authenticated user
- Username index (Radix tree)
- Path index (for navigation)
- Network state
-}
data SystemContext = SystemContext
  { currentUser :: Maybe User.UserAuth
  , usernameIndex :: Radix.RadixTree Text  -- username -> pubKey
  , pathIndex :: Radix.RadixTree Text      -- path -> nodeId
  , networkState :: NZ.NetworkState
  }
  deriving (Show)

-- | Create empty system context
emptyContext :: SystemContext
emptyContext = SystemContext
  { currentUser = Nothing
  , usernameIndex = Radix.empty
  , pathIndex = Radix.empty
  , networkState = NZ.emptyNetworkState
  }

-- ============================================================================
-- SECURE COMMITMENT (with signatures)
-- ============================================================================

{- | SecureCommitment - Enhanced commitment with cryptographic proof

Wraps a regular commitment with:
- Signature (from SEA.hs)
- Owner's public key
- Timestamp
-}
data SecureCommitment = SecureCommitment
  { commitment :: PC.Commitment
  , signature :: Maybe Text        -- Signature (stub for now)
  , owner :: Text                  -- Owner's public key
  , timestamp :: Integer           -- When commitment was created
  }
  deriving (Show, Eq)

-- | Create a secure commitment from user authentication
createSecureCommitment :: 
  Text                    -- Commitment type ("need" or "capacity")
  -> PC.Commitment        -- The commitment data
  -> User.UserAuth        -- User creating the commitment
  -> IO SecureCommitment
createSecureCommitment _ctype comm userAuth = do
  now <- round <$> getPOSIXTime
  
  -- Sign the commitment (stub for now)
  -- In production: sig <- SEA.signData comm (toKeyPair userAuth)
  let sig = Just "stub-signature"
  
  return SecureCommitment
    { commitment = comm
    , signature = sig
    , owner = User.userPub userAuth
    , timestamp = now
    }

-- | Verify a secure commitment
verifySecureCommitment :: SecureCommitment -> Bool
verifySecureCommitment _sc =
  -- Stub: In production would verify signature
  -- SEA.verifyData (signature sc) (commitment sc) (owner sc)
  True

-- ============================================================================
-- USERNAME OPERATIONS (Radix tree integration)
-- ============================================================================

-- | Lookup username in O(k) time
lookupUsername :: Text -> SystemContext -> Maybe Text
lookupUsername name ctx = Radix.lookup name (usernameIndex ctx)

-- | Register username
registerUsername :: Text -> Text -> SystemContext -> SystemContext
registerUsername name pubKey ctx =
  ctx { usernameIndex = Radix.insert name pubKey (usernameIndex ctx) }

-- | Lookup all usernames with prefix (autocomplete)
autocompleteUsername :: Text -> SystemContext -> [(Text, Text)]
autocompleteUsername prefix ctx = Radix.prefixMatch prefix (usernameIndex ctx)

-- ============================================================================
-- PATH OPERATIONS
-- ============================================================================

-- | Register a path in the index
registerPath :: Text -> Text -> SystemContext -> SystemContext
registerPath path nodeId ctx =
  ctx { pathIndex = Radix.insert path nodeId (pathIndex ctx) }

-- | Lookup path
lookupPath :: Text -> SystemContext -> Maybe Text
lookupPath path ctx = Radix.lookup path (pathIndex ctx)

-- ============================================================================
-- CONVERSION FUNCTIONS (Bridge between modules)
-- ============================================================================

{- | Convert Commitment to ResourceSlot for enhanced matching -}
toResourceSlot :: PC.Commitment -> EM.ResourceSlot
toResourceSlot comm = EM.ResourceSlot
  { EM.slotId = PC.entityId comm
  , EM.resourceTypeId = PC.resourceType comm
  , EM.quantity = PC.totalCapacity comm
  , EM.startDate = Nothing  -- Would extract from commitment
  , EM.endDate = Nothing
  , EM.recurrence = Nothing
  , EM.availabilityWindow = Nothing
  , EM.timezone = Nothing
  , EM.city = Nothing
  , EM.country = Nothing
  , EM.latitude = Nothing
  , EM.longitude = Nothing
  , EM.locationType = Nothing
  , EM.filterRule = Nothing
  }

{- | Convert Commitment + UserAuth to FilterContext -}
toFilterContext :: PC.Commitment -> User.UserAuth -> EM.FilterContext
toFilterContext comm userAuth = EM.FilterContext
  { EM.entityPubKey = User.userPub userAuth
  , EM.mutualRecognition = 0.0  -- Would calculate from commitment
  , EM.entityCity = Nothing
  , EM.entityCountry = Nothing
  }

{- | Convert UserAuth to KeyPair for signing -}
toKeyPair :: User.UserAuth -> SEA.KeyPair
toKeyPair userAuth = SEA.KeyPair
  { SEA.publicKey = User.userPub userAuth
  , SEA.privateKey = User.userPriv userAuth
  , SEA.encryptionPublicKey = User.userEPub userAuth
  , SEA.encryptionPrivateKey = User.userEPriv userAuth
  }

-- ============================================================================
-- INTEGRATED PROTOCOL EXECUTION
-- ============================================================================

{- | AllocationResults - Results from running the protocol -}
data AllocationResults = AllocationResults
  { allocations :: [(Text, Text, Double)]  -- (provider, recipient, amount)
  , verificationStatus :: [(Text, Bool)]   -- (entityId, verified)
  , executionTime :: Double                -- Time taken (seconds)
  }
  deriving (Show)

{- | Run the protocol with enhanced matching and security

This is the main entry point for the integrated system.
It:
1. Verifies all commitments
2. Runs protocol with enhanced matching (bilateral filters)
3. Returns allocation results with verification status
-}
runSecureProtocol :: 
  SystemContext
  -> [SecureCommitment]  -- Needs (recipients)
  -> [SecureCommitment]  -- Capacities (providers)
  -> IO AllocationResults
runSecureProtocol _ctx needComms capComms = do
  startTime <- getPOSIXTime
  
  -- 1. Verify all commitments
  let verifiedNeeds = filter verifySecureCommitment needComms
  let verifiedCaps = filter verifySecureCommitment capComms
  
  let verificationStatus = 
        [(owner c, verifySecureCommitment c) | c <- needComms ++ capComms]
  
  -- 2. Extract plain commitments for protocol
  let needs = map commitment verifiedNeeds
  let caps = map commitment verifiedCaps
  
  -- 3. Run protocol (simplified - would use actual providerPhase)
  -- For now, just demonstrate the structure
  let allocs = simulateAllocations needs caps
  
  endTime <- getPOSIXTime
  let execTime = realToFrac (endTime - startTime)
  
  return AllocationResults
    { allocations = allocs
    , verificationStatus = verificationStatus
    , executionTime = execTime
    }

-- | Simulate allocations (stub for demonstration)
simulateAllocations :: [PC.Commitment] -> [PC.Commitment] -> [(Text, Text, Double)]
simulateAllocations needs caps =
  -- Stub: Just pair first need with first capacity
  case (needs, caps) of
    (n:_, c:_) -> [(PC.entityId c, PC.entityId n, 5.0)]
    _ -> []

-- ============================================================================
-- ENHANCED PROTOCOL STEP 2 (Integration point)
-- ============================================================================

{- | Enhanced Step 2 - Process recipient with bilateral filters

This extends ProtocolCompliant's Step 2 with:
- Space-time compatibility (EnhancedMatching)
- Bilateral filter checking (mutual consent)
- Asymmetric recurrence model

Keeps backward compatibility with original Step 2.
-}
processRecipientDataEnhanced ::
  PC.ProviderState
  -> EM.FilterContext       -- Provider context
  -> PC.RecipientData
  -> EM.FilterContext       -- Recipient context
  -> IO (Maybe PC.RecipientData)
processRecipientDataEnhanced provider provCtx recip recCtx = do
  -- Get timestamp and resource type from provider
  let timestamp = round <$> getPOSIXTime  -- Would get from provider state
  let resourceType = PC.resourceType $ PC.providerCommitment provider
  
  -- Existing compatibility check (from ProtocolCompliant)
  timestampVal <- timestamp
  let compatible = checkCompatibility 
        timestampVal
        resourceType 
        provider 
        (PC.recipientCommit recip)
  
  if not compatible
    then return Nothing
    else do
      -- NEW: Enhanced matching
      let needSlot = toResourceSlot (PC.recipientCommit recip)
      let capacitySlot = toResourceSlot (PC.providerCommitment provider)
      
      -- Check space-time compatibility
      compatible' <- EM.slotsCompatible needSlot capacitySlot
      
      if not compatible'
        then return Nothing
        else do
          -- Check bilateral filters (mutual consent)
          let filtersPass = EM.passesBilateralFilters 
                needSlot capacitySlot provCtx recCtx
          
          if filtersPass
            then return $ Just recip
            else return Nothing

{- | Compatibility check (extracted from ProtocolCompliant) -}
checkCompatibility :: 
  Integer 
  -> Text 
  -> PC.ProviderState 
  -> PC.Commitment 
  -> Bool
checkCompatibility _timestamp _resourceType _provider _recipient =
  -- Stub: In production would check time window, location, resource type
  True

-- ============================================================================
-- USER SESSION MANAGEMENT
-- ============================================================================

{- | Authenticate user and update context -}
authenticateUser :: Text -> Text -> SystemContext -> IO (Either Text SystemContext)
authenticateUser username password ctx = do
  result <- User.authenticateUser username password
  case result of
    Left err -> return $ Left err
    Right userAuth -> return $ Right ctx { currentUser = Just userAuth }

{- | Logout user -}
logoutUser :: SystemContext -> SystemContext
logoutUser ctx = ctx { currentUser = Nothing }

{- | Get current user's public key -}
getCurrentUserPubKey :: SystemContext -> Maybe Text
getCurrentUserPubKey ctx = User.userPub <$> currentUser ctx

-- ============================================================================
-- UTILITY FUNCTIONS
-- ============================================================================

{- | Display allocation results in a readable format -}
displayResults :: AllocationResults -> IO ()
displayResults AllocationResults{..} = do
  putStrLn "\n=== Allocation Results ==="
  putStrLn $ "Execution time: " ++ show executionTime ++ " seconds\n"
  
  putStrLn "Allocations:"
  if null allocations
    then putStrLn "  No allocations made"
    else mapM_ displayAllocation allocations
  
  putStrLn "\nVerification Status:"
  mapM_ displayVerification verificationStatus
  
  where
    displayAllocation (provider, recipient, amount) =
      putStrLn $ "  " ++ T.unpack provider ++ " → " 
              ++ T.unpack recipient ++ ": " ++ show amount
    
    displayVerification (entityId, verified) =
      putStrLn $ "  " ++ T.unpack entityId ++ ": " 
              ++ if verified then "✓ Verified" else "✗ Invalid"

{- | Check if user is authenticated -}
isAuthenticated :: SystemContext -> Bool
isAuthenticated = maybe False (const True) . currentUser

-- ============================================================================
-- EXAMPLES (for testing)
-- ============================================================================

{- | Example: Create a simple need commitment -}
exampleNeedCommitment :: Text -> IO PC.Commitment
exampleNeedCommitment entityId = do
  return PC.Commitment
    { PC.entityId = entityId
    , PC.resourceType = "tutoring"
    , PC.totalCapacity = 5.0
    , PC.recognitions = M.fromList [("provider1", 0.3), ("provider2", 0.7)]
    , PC.needs = M.fromList [("provider1", 5.0)]
    , PC.filters = PC.ResourceFilters
        { PC.timeWindow = Nothing
        , PC.locationFilter = Nothing
        , PC.resourceTypeFilter = Nothing
        }
    }

{- | Example: Create a simple capacity commitment -}
exampleCapacityCommitment :: Text -> IO PC.Commitment
exampleCapacityCommitment entityId = do
  return PC.Commitment
    { PC.entityId = entityId
    , PC.resourceType = "tutoring"
    , PC.totalCapacity = 10.0
    , PC.recognitions = M.fromList [("recipient1", 0.5), ("recipient2", 0.5)]
    , PC.needs = M.empty
    , PC.filters = PC.ResourceFilters
        { PC.timeWindow = Nothing
        , PC.locationFilter = Nothing
        , PC.resourceTypeFilter = Nothing
        }
    }

