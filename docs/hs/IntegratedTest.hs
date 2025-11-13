{-# LANGUAGE OverloadedStrings #-}

{- | Integrated Tests - Verify Complete System Integration

Tests the integrated Free Association system:
1. Bilateral filter checking
2. Asymmetric recurrence model
3. Username indexing (Radix tree)
4. Authentication flow
5. Secure operations

These tests verify that all modules work together correctly.
-}

module IntegratedTest where

import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

-- Import modules
import qualified IntegratedProtocol as IP
import qualified ProtocolCompliant as PC
import qualified EnhancedMatching as EM
import qualified User
import qualified Radix

-- ============================================================================
-- TEST 1: Bilateral Filters
-- ============================================================================

{- | Test bilateral filter checking

Scenario:
- Provider filters: Only recipients in San Francisco
- Recipient filters: Only certified providers
- Both filters must pass for allocation

Expected: Allocation happens only if BOTH filters pass
-}
testBilateralFilters :: IO Bool
testBilateralFilters = do
  putStrLn "TEST 1: Bilateral Filters"
  putStrLn "─────────────────────────"
  
  -- Create filter contexts
  let providerCtx = EM.FilterContext
        { EM.entityPubKey = "provider-pub-key"
        , EM.mutualRecognition = 0.5
        , EM.entityCity = Just "San Francisco"
        , EM.entityCountry = Just "USA"
        }
  
  let recipientCtxSF = EM.FilterContext
        { EM.entityPubKey = "recipient-sf-pub-key"
        , EM.mutualRecognition = 0.3
        , EM.entityCity = Just "San Francisco"
        , EM.entityCountry = Just "USA"
        }
  
  let recipientCtxNYC = EM.FilterContext
        { EM.entityPubKey = "recipient-nyc-pub-key"
        , EM.mutualRecognition = 0.3
        , EM.entityCity = Just "New York"
        , EM.entityCountry = Just "USA"
        }
  
  -- Create slots with filters
  let providerSlot = EM.ResourceSlot
        { EM.slotId = "provider-slot"
        , EM.needTypeId = "tutoring"
        , EM.quantity = 10.0
        , EM.startDate = Nothing
        , EM.endDate = Nothing
        , EM.recurrence = Nothing
        , EM.availabilityWindow = Nothing
        , EM.timezone = Nothing
        , EM.city = Just "San Francisco"
        , EM.country = Just "USA"
        , EM.latitude = Nothing
        , EM.longitude = Nothing
        , EM.locationType = Nothing
        , EM.filterRule = Just $ EM.LocationFilter ["San Francisco"] ["USA"]
        }
  
  let recipientSlot = EM.ResourceSlot
        { EM.slotId = "recipient-slot"
        , EM.needTypeId = "tutoring"
        , EM.quantity = 5.0
        , EM.startDate = Nothing
        , EM.endDate = Nothing
        , EM.recurrence = Nothing
        , EM.availabilityWindow = Nothing
        , EM.timezone = Nothing
        , EM.city = Just "San Francisco"
        , EM.country = Just "USA"
        , EM.latitude = Nothing
        , EM.longitude = Nothing
        , EM.locationType = Nothing
        , EM.filterRule = Nothing  -- No filter from recipient side
        }
  
  -- Test 1a: SF recipient passes provider's filter
  let pass1 = EM.passesBilateralFilters recipientSlot providerSlot providerCtx recipientCtxSF
  putStrLn $ "  SF recipient + SF provider: " ++ if pass1 then "✓ PASS" else "✗ FAIL"
  
  -- Test 1b: NYC recipient fails provider's filter
  let pass2 = EM.passesBilateralFilters recipientSlot providerSlot providerCtx recipientCtxNYC
  putStrLn $ "  NYC recipient + SF provider: " ++ if not pass2 then "✓ PASS (correctly rejected)" else "✗ FAIL (should reject)"
  
  let result = pass1 && not pass2
  putStrLn $ "\nTest result: " ++ if result then "✓ PASSED" else "✗ FAILED"
  putStrLn ""
  return result

-- ============================================================================
-- TEST 2: Asymmetric Recurrence Model
-- ============================================================================

{- | Test asymmetric recurrence model

Scenario:
- Recurring capacity (weekly tutoring)
- One-time need (help this Monday)
- Recurring need (weekly tutoring)

Expected: Recurring capacity can serve BOTH types of needs
-}
testAsymmetricRecurrence :: IO Bool
testAsymmetricRecurrence = do
  putStrLn "TEST 2: Asymmetric Recurrence Model"
  putStrLn "───────────────────────────────────"
  
  -- Recurring capacity slot
  let recurringCapacity = EM.ResourceSlot
        { EM.slotId = "recurring-capacity"
        , EM.needTypeId = "tutoring"
        , EM.quantity = 10.0
        , EM.startDate = Nothing
        , EM.endDate = Nothing
        , EM.recurrence = Just "weekly"  -- RECURRING
        , EM.availabilityWindow = Nothing
        , EM.timezone = Nothing
        , EM.city = Just "San Francisco"
        , EM.country = Just "USA"
        , EM.latitude = Nothing
        , EM.longitude = Nothing
        , EM.locationType = Nothing
        , EM.filterRule = Nothing
        }
  
  -- One-time need
  let onetimeNeed = EM.ResourceSlot
        { EM.slotId = "onetime-need"
        , EM.needTypeId = "tutoring"
        , EM.quantity = 3.0
        , EM.startDate = Nothing
        , EM.endDate = Nothing
        , EM.recurrence = Nothing  -- ONE-TIME
        , EM.availabilityWindow = Nothing
        , EM.timezone = Nothing
        , EM.city = Just "San Francisco"
        , EM.country = Just "USA"
        , EM.latitude = Nothing
        , EM.longitude = Nothing
        , EM.locationType = Nothing
        , EM.filterRule = Nothing
        }
  
  -- Recurring need
  let recurringNeed = EM.ResourceSlot
        { EM.slotId = "recurring-need"
        , EM.needTypeId = "tutoring"
        , EM.quantity = 5.0
        , EM.startDate = Nothing
        , EM.endDate = Nothing
        , EM.recurrence = Just "weekly"  -- RECURRING
        , EM.availabilityWindow = Nothing
        , EM.timezone = Nothing
        , EM.city = Just "San Francisco"
        , EM.country = Just "USA"
        , EM.latitude = Nothing
        , EM.longitude = Nothing
        , EM.locationType = Nothing
        , EM.filterRule = Nothing
        }
  
  -- Test: Recurring capacity should match both types
  compat1 <- EM.slotsCompatible onetimeNeed recurringCapacity
  compat2 <- EM.slotsCompatible recurringNeed recurringCapacity
  
  putStrLn $ "  Recurring capacity + one-time need: " ++ if compat1 then "✓ Compatible" else "✗ Not compatible"
  putStrLn $ "  Recurring capacity + recurring need: " ++ if compat2 then "✓ Compatible" else "✗ Not compatible"
  
  -- Get recurrence tracks
  let track1 = EM.getRecurrenceTrack onetimeNeed
  let track2 = EM.getRecurrenceTrack recurringNeed
  let track3 = EM.getRecurrenceTrack recurringCapacity
  
  putStrLn $ "\n  One-time need track: " ++ show track1
  putStrLn $ "  Recurring need track: " ++ show track2
  putStrLn $ "  Capacity track: " ++ show track3
  
  let result = compat1 && compat2
  putStrLn $ "\nTest result: " ++ if result then "✓ PASSED (asymmetric model works)" else "✗ FAILED"
  putStrLn ""
  return result

-- ============================================================================
-- TEST 3: Username Index (Radix Tree)
-- ============================================================================

{- | Test username indexing with Radix tree

Tests:
- O(k) lookup time
- Prefix matching (autocomplete)
- Insertion and retrieval
-}
testUsernameIndex :: IO Bool
testUsernameIndex = do
  putStrLn "TEST 3: Username Index (Radix Tree)"
  putStrLn "───────────────────────────────────"
  
  -- Create empty index
  let index = Radix.empty :: Radix.RadixTree Text
  
  -- Insert usernames
  let index1 = Radix.insert "alice" "alice-pub-key" index
  let index2 = Radix.insert "alex" "alex-pub-key" index1
  let index3 = Radix.insert "alicia" "alicia-pub-key" index2
  let index4 = Radix.insert "bob" "bob-pub-key" index3
  let index5 = Radix.insert "carol" "carol-pub-key" index4
  
  putStrLn "  Inserted: alice, alex, alicia, bob, carol"
  
  -- Test exact lookup
  let lookupAlice = Radix.lookup "alice" index5
  let test1 = maybe False (== "alice-pub-key") lookupAlice
  putStrLn $ "  Exact lookup 'alice': " ++ if test1 then "✓ Found" else "✗ Not found"
  
  -- Test prefix matching
  let matches = Radix.prefixMatch "al" index5
  let test2 = length matches == 3  -- alice, alex, alicia
  putStrLn $ "  Prefix match 'al': " ++ if test2 then "✓ Found 3 matches" else "✗ Wrong count"
  
  -- Test non-existent
  let lookupDave = Radix.lookup "dave" index5
  let test3 = maybe True (const False) lookupDave
  putStrLn $ "  Lookup 'dave' (not exists): " ++ if test3 then "✓ Correctly not found" else "✗ Incorrectly found"
  
  let result = test1 && test2 && test3
  putStrLn $ "\nTest result: " ++ if result then "✓ PASSED (radix tree works)" else "✗ FAILED"
  putStrLn ""
  return result

-- ============================================================================
-- TEST 4: Authentication Flow
-- ============================================================================

{- | Test authentication and context management -}
testAuthenticationFlow :: IO Bool
testAuthenticationFlow = do
  putStrLn "TEST 4: Authentication Flow"
  putStrLn "───────────────────────────"
  
  -- Create user
  result <- User.createUser "testuser" "testpass"
  
  case result of
    Left err -> do
      putStrLn $ "  ✗ Failed to create user: " ++ T.unpack err
      return False
    Right userAuth -> do
      putStrLn "  ✓ User created"
      
      -- Create context and authenticate
      let ctx = IP.emptyContext
      authResult <- IP.authenticateUser "testuser" "testpass" ctx
      
      case authResult of
        Left err -> do
          putStrLn $ "  ✗ Authentication failed: " ++ T.unpack err
          return False
        Right ctx' -> do
          putStrLn "  ✓ User authenticated"
          
          -- Test authenticated state
          let isAuth = IP.isAuthenticated ctx'
          let hasPubKey = maybe False (const True) (IP.getCurrentUserPubKey ctx')
          
          putStrLn $ "    Is authenticated: " ++ show isAuth
          putStrLn $ "    Has public key: " ++ show hasPubKey
          
          -- Test logout
          let ctx'' = IP.logoutUser ctx'
          let isAuthAfterLogout = IP.isAuthenticated ctx''
          
          putStrLn $ "  ✓ User logged out"
          putStrLn $ "    Is authenticated after logout: " ++ show isAuthAfterLogout
          
          let result = isAuth && hasPubKey && not isAuthAfterLogout
          putStrLn $ "\nTest result: " ++ if result then "✓ PASSED" else "✗ FAILED"
          putStrLn ""
          return result

-- ============================================================================
-- TEST 5: Secure Commitments
-- ============================================================================

{- | Test secure commitment creation and verification -}
testSecureCommitments :: IO Bool
testSecureCommitments = do
  putStrLn "TEST 5: Secure Commitments"
  putStrLn "──────────────────────────"
  
  -- Create user
  result <- User.createUser "provider" "providerpass"
  
  case result of
    Left err -> do
      putStrLn $ "  ✗ Failed to create user: " ++ T.unpack err
      return False
    Right userAuth -> do
      putStrLn "  ✓ User created"
      
      -- Create commitment
      let commitment = PC.Commitment
            { PC.entityId = "provider"
            , PC.resourceType = "tutoring"
            , PC.totalCapacity = 10.0
            , PC.recognitions = M.empty
            , PC.needs = M.empty
            , PC.filters = PC.ResourceFilters Nothing Nothing Nothing
            }
      
      -- Create secure commitment
      secureComm <- IP.createSecureCommitment "capacity" commitment userAuth
      
      let hasSignature = maybe False (const True) (IP.signature secureComm)
      let hasOwner = not $ T.null (IP.owner secureComm)
      let hasTimestamp = IP.timestamp secureComm > 0
      
      putStrLn $ "  ✓ Secure commitment created"
      putStrLn $ "    Has signature: " ++ show hasSignature
      putStrLn $ "    Has owner: " ++ show hasOwner
      putStrLn $ "    Has timestamp: " ++ show hasTimestamp
      
      -- Verify commitment
      let isValid = IP.verifySecureCommitment secureComm
      putStrLn $ "    Verification: " ++ if isValid then "✓ Valid" else "✗ Invalid"
      
      let result = hasSignature && hasOwner && hasTimestamp && isValid
      putStrLn $ "\nTest result: " ++ if result then "✓ PASSED" else "✗ FAILED"
      putStrLn ""
      return result

-- ============================================================================
-- TEST RUNNER
-- ============================================================================

{- | Run all integration tests -}
main :: IO ()
main = do
  putStrLn "╔════════════════════════════════════════════════════════════╗"
  putStrLn "║          Integrated System Tests                          ║"
  putStrLn "╚════════════════════════════════════════════════════════════╝\n"
  
  -- Run tests
  test1 <- testBilateralFilters
  test2 <- testAsymmetricRecurrence
  test3 <- testUsernameIndex
  test4 <- testAuthenticationFlow
  test5 <- testSecureCommitments
  
  -- Summary
  let allPassed = test1 && test2 && test3 && test4 && test5
  let passedCount = length $ filter id [test1, test2, test3, test4, test5]
  
  putStrLn "╔════════════════════════════════════════════════════════════╗"
  putStrLn "║                      Test Summary                          ║"
  putStrLn "╠════════════════════════════════════════════════════════════╣"
  putStrLn $ "║  Tests passed: " ++ show passedCount ++ "/5" ++ replicate (45 - length (show passedCount)) ' ' ++ "║"
  putStrLn "║                                                            ║"
  
  if allPassed
    then do
      putStrLn "║  ✓ ALL TESTS PASSED!                                       ║"
      putStrLn "║                                                            ║"
      putStrLn "║  The integrated system is working correctly:               ║"
      putStrLn "║    • Bilateral filters enforce mutual consent              ║"
      putStrLn "║    • Asymmetric recurrence model is flexible               ║"
      putStrLn "║    • Radix tree provides O(k) lookups                      ║"
      putStrLn "║    • Authentication flow works correctly                   ║"
      putStrLn "║    • Secure commitments are signed and verified            ║"
    else do
      putStrLn "║  ✗ SOME TESTS FAILED                                       ║"
      putStrLn "║                                                            ║"
      putStrLn "║  Please review the test output above.                      ║"
  
  putStrLn "╚════════════════════════════════════════════════════════════╝"

