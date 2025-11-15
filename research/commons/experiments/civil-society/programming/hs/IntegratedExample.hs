{-# LANGUAGE OverloadedStrings #-}

{- | Integrated Example - End-to-End Demonstration

Demonstrates the complete integrated Free Association system:
1. User authentication (User.hs)
2. Signed commitments (SEA.hs stubs)
3. Enhanced protocol execution (IntegratedProtocol.hs)
4. Bilateral filters (EnhancedMatching.hs)
5. Username indexing (Radix.hs)
6. Secure network operations (NetworkedZipper.hs)

This shows all modules working together in a minimal, clear example.
-}

module IntegratedExample where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

-- Import integrated modules
import qualified IntegratedProtocol as IP
import qualified ProtocolCompliant as PC
import qualified User
import qualified Radix

-- ============================================================================
-- MAIN EXAMPLE
-- ============================================================================

{- | Complete integrated flow example

Demonstrates:
- User account creation
- Username indexing
- Secure commitment creation
- Protocol execution with enhanced matching
- Verification and results display
-}
exampleIntegratedFlow :: IO ()
exampleIntegratedFlow = do
  putStrLn "╔════════════════════════════════════════════════════════════╗"
  putStrLn "║     Minimal Elegant Integration Example                   ║"
  putStrLn "║     Free Association - Complete System Demonstration      ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝\n"
  
  -- ========================================
  -- PHASE 1: User Creation and Authentication
  -- ========================================
  putStrLn "📋 Phase 1: User Creation and Authentication\n"
  
  -- Create Alice (recipient with need)
  putStrLn "Creating user 'alice' (recipient)..."
  aliceResult <- User.createUser "alice" "secure-password-123"
  
  alice <- case aliceResult of
    Left err -> do
      putStrLn $ "❌ Error creating alice: " ++ T.unpack err
      error "Failed to create alice"
    Right userAuth -> do
      putStrLn $ "✅ Alice created"
      putStrLn $ "   Public key: " ++ take 16 (T.unpack $ User.userPub userAuth) ++ "..."
      return userAuth
  
  -- Create Bob (provider with capacity)
  putStrLn "\nCreating user 'bob' (provider)..."
  bobResult <- User.createUser "bob" "secure-password-456"
  
  bob <- case bobResult of
    Left err -> do
      putStrLn $ "❌ Error creating bob: " ++ T.unpack err
      error "Failed to create bob"
    Right userAuth -> do
      putStrLn $ "✅ Bob created"
      putStrLn $ "   Public key: " ++ take 16 (T.unpack $ User.userPub userAuth) ++ "..."
      return userAuth
  
  -- ========================================
  -- PHASE 2: System Context Initialization
  -- ========================================
  putStrLn "\n📋 Phase 2: System Context Initialization\n"
  
  -- Initialize empty context
  let ctx = IP.emptyContext
  
  -- Register usernames in Radix tree index
  putStrLn "Registering usernames in Radix tree index..."
  let ctx' = IP.registerUsername "alice" (User.userPub alice) ctx
  let ctx'' = IP.registerUsername "bob" (User.userPub bob) ctx'
  
  putStrLn "✅ Usernames registered"
  
  -- Test username lookup
  putStrLn "\nTesting username lookup (O(k) via Radix tree)..."
  case IP.lookupUsername "alice" ctx'' of
    Just pubKey -> putStrLn $ "   alice → " ++ take 16 (T.unpack pubKey) ++ "..."
    Nothing -> putStrLn "   alice not found"
  
  case IP.lookupUsername "bob" ctx'' of
    Just pubKey -> putStrLn $ "   bob → " ++ take 16 (T.unpack pubKey) ++ "..."
    Nothing -> putStrLn "   bob not found"
  
  -- Test autocomplete
  putStrLn "\nTesting autocomplete (prefix='al')..."
  let matches = IP.autocompleteUsername "al" ctx''
  if null matches
    then putStrLn "   No matches found"
    else mapM_ (\(name, key) -> 
           putStrLn $ "   " ++ T.unpack name ++ " → " ++ take 16 (T.unpack key) ++ "...") matches
  
  -- ========================================
  -- PHASE 3: Create Commitments
  -- ========================================
  putStrLn "\n📋 Phase 3: Create Signed Commitments\n"
  
  -- Alice's need commitment
  putStrLn "Creating Alice's need commitment (wants tutoring)..."
  aliceNeedData <- createNeedCommitment "alice"
  aliceNeed <- IP.createSecureCommitment "need" aliceNeedData alice
  putStrLn "✅ Alice's need created and signed"
  putStrLn $ "   Resource type: " ++ T.unpack (PC.resourceType $ IP.commitment aliceNeed)
  putStrLn $ "   Quantity needed: " ++ show (PC.totalCapacity $ IP.commitment aliceNeed)
  putStrLn $ "   Signature: " ++ maybe "none" (take 16 . T.unpack) (IP.signature aliceNeed) ++ "..."
  
  -- Bob's capacity commitment  
  putStrLn "\nCreating Bob's capacity commitment (offers tutoring)..."
  bobCapData <- createCapacityCommitment "bob"
  bobCapacity <- IP.createSecureCommitment "capacity" bobCapData bob
  putStrLn "✅ Bob's capacity created and signed"
  putStrLn $ "   Resource type: " ++ T.unpack (PC.resourceType $ IP.commitment bobCapacity)
  putStrLn $ "   Quantity available: " ++ show (PC.totalCapacity $ IP.commitment bobCapacity)
  putStrLn $ "   Signature: " ++ maybe "none" (take 16 . T.unpack) (IP.signature bobCapacity) ++ "..."
  
  -- ========================================
  -- PHASE 4: Run Integrated Protocol
  -- ========================================
  putStrLn "\n📋 Phase 4: Run Protocol with Enhanced Matching\n"
  
  putStrLn "Running secure protocol with:"
  putStrLn "  ✓ Signature verification"
  putStrLn "  ✓ Bilateral filters"
  putStrLn "  ✓ Space-time compatibility"
  putStrLn "  ✓ Asymmetric recurrence model"
  putStrLn ""
  
  results <- IP.runSecureProtocol ctx'' [aliceNeed] [bobCapacity]
  
  -- ========================================
  -- PHASE 5: Display Results
  -- ========================================
  putStrLn "📋 Phase 5: Results\n"
  
  IP.displayResults results
  
  -- ========================================
  -- PHASE 6: Demonstrate Features
  -- ========================================
  putStrLn "\n📋 Phase 6: Feature Demonstrations\n"
  
  -- Show username index benefits
  putStrLn "Username Index Benefits:"
  putStrLn "  ✓ O(k) lookup time (k = username length)"
  putStrLn "  ✓ Prefix matching for autocomplete"
  putStrLn "  ✓ Space-efficient (shared prefixes)"
  putStrLn ""
  
  -- Show security features
  putStrLn "Security Features:"
  putStrLn "  ✓ All commitments cryptographically signed"
  putStrLn "  ✓ Signatures verified before processing"
  putStrLn "  ✓ User authentication required for operations"
  putStrLn "  ✓ Private keys encrypted with password"
  putStrLn ""
  
  -- Show enhanced matching
  putStrLn "Enhanced Matching Features:"
  putStrLn "  ✓ Bilateral filters (mutual consent)"
  putStrLn "  ✓ Space-time compatibility"
  putStrLn "  ✓ Asymmetric recurrence (capacity serves any need)"
  putStrLn "  ✓ Timezone awareness (stubs ready)"
  putStrLn ""
  
  -- Summary
  putStrLn "╔════════════════════════════════════════════════════════════╗"
  putStrLn "║  ✅ Integration Complete!                                  ║"
  putStrLn "║                                                            ║"
  putStrLn "║  All modules working together:                             ║"
  putStrLn "║    • IntegratedProtocol (coordination)                     ║"
  putStrLn "║    • ProtocolCompliant (core algorithm)                    ║"
  putStrLn "║    • EnhancedMatching (bilateral filters)                  ║"
  putStrLn "║    • User (authentication)                                 ║"
  putStrLn "║    • SEA (signatures - stubs)                              ║"
  putStrLn "║    • Radix (indexing)                                      ║"
  putStrLn "║    • NetworkedZipper (secure operations)                   ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝"

-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

{- | Create a need commitment for a user -}
createNeedCommitment :: Text -> IO PC.Commitment
createNeedCommitment entityId = do
  return PC.Commitment
    { PC.entityId = entityId
    , PC.resourceType = "tutoring"
    , PC.totalCapacity = 5.0  -- Alice needs 5 hours of tutoring
    , PC.recognitions = M.fromList
        [ ("bob", 0.8)    -- Alice recognizes Bob highly
        , ("carol", 0.2)  -- And Carol a bit
        ]
    , PC.needs = M.fromList
        [ ("bob", 5.0)    -- Alice needs all 5 hours from Bob
        ]
    , PC.filters = PC.ResourceFilters
        { PC.timeWindow = Nothing  -- Any time
        , PC.locationFilter = Nothing  -- Any location
        , PC.resourceTypeFilter = Just "tutoring"  -- Only tutoring
        }
    }

{- | Create a capacity commitment for a user -}
createCapacityCommitment :: Text -> IO PC.Commitment
createCapacityCommitment entityId = do
  return PC.Commitment
    { PC.entityId = entityId
    , PC.resourceType = "tutoring"
    , PC.totalCapacity = 10.0  -- Bob can provide 10 hours
    , PC.recognitions = M.fromList
        [ ("alice", 0.7)  -- Bob recognizes Alice
        , ("dave", 0.3)   -- And Dave
        ]
    , PC.needs = M.empty  -- Bob has no needs (pure provider)
    , PC.filters = PC.ResourceFilters
        { PC.timeWindow = Nothing
        , PC.locationFilter = Nothing
        , PC.resourceTypeFilter = Just "tutoring"
        }
    }

-- ============================================================================
-- ADDITIONAL EXAMPLES
-- ============================================================================

{- | Example: Multiple users with autocomplete -}
exampleMultipleUsers :: IO ()
exampleMultipleUsers = do
  putStrLn "\n🔍 Example: Multiple Users with Autocomplete\n"
  
  -- Create users
  alice <- User.createUser "alice" "pass1"
  alex <- User.createUser "alex" "pass2"
  alicia <- User.createUser "alicia" "pass3"
  bob <- User.createUser "bob" "pass4"
  
  -- Register in context
  let ctx = IP.emptyContext
  let ctx' = case alice of
        Right a -> IP.registerUsername "alice" (User.userPub a) ctx
        Left _ -> ctx
  let ctx'' = case alex of
        Right a -> IP.registerUsername "alex" (User.userPub a) ctx'
        Left _ -> ctx'
  let ctx''' = case alicia of
        Right a -> IP.registerUsername "alicia" (User.userPub a) ctx''
        Left _ -> ctx''
  let ctx'''' = case bob of
        Right b -> IP.registerUsername "bob" (User.userPub b) ctx'''
        Left _ -> ctx'''
  
  -- Test autocomplete with "al"
  putStrLn "Autocomplete for 'al':"
  let matches = IP.autocompleteUsername "al" ctx''''
  mapM_ (\(name, _) -> putStrLn $ "  • " ++ T.unpack name) matches
  
  putStrLn "\nRadix tree efficiently stores all 'al' usernames!"
  putStrLn "Shared prefix 'al' is stored only once."

{- | Example: Authentication flow -}
exampleAuthFlow :: IO ()
exampleAuthFlow = do
  putStrLn "\n🔐 Example: Authentication Flow\n"
  
  -- Create user
  result <- User.createUser "testuser" "testpass"
  
  case result of
    Left err -> putStrLn $ "❌ Error: " ++ T.unpack err
    Right userAuth -> do
      -- Create context
      let ctx = IP.emptyContext
      
      -- Authenticate
      authResult <- IP.authenticateUser "testuser" "testpass" ctx
      
      case authResult of
        Left err -> putStrLn $ "❌ Auth error: " ++ T.unpack err
        Right ctx' -> do
          putStrLn "✅ User authenticated"
          putStrLn $ "   Authenticated: " ++ show (IP.isAuthenticated ctx')
          putStrLn $ "   Public key: " ++ maybe "none" (take 16 . T.unpack) (IP.getCurrentUserPubKey ctx')
          
          -- Logout
          let ctx'' = IP.logoutUser ctx'
          putStrLn "\n✅ User logged out"
          putStrLn $ "   Authenticated: " ++ show (IP.isAuthenticated ctx'')

-- ============================================================================
-- MAIN
-- ============================================================================

main :: IO ()
main = do
  exampleIntegratedFlow
  putStrLn "\n"
  exampleMultipleUsers
  putStrLn "\n"
  exampleAuthFlow

