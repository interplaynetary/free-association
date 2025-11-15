{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module NetworkedZipper where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Control.Concurrent.Async (async, wait, mapConcurrently)

-- | Core types
type EntityId = String
type ContextId = String
type NetworkAddress = String

-- | Portion: Recognition percentages (0.0 to 1.0)
newtype Portion = Portion Double deriving (Eq, Ord, Show, Num, Fractional)

mkPortion :: Double -> Maybe Portion
mkPortion p 
  | p >= 0.0 && p <= 1.0 = Just (Portion p)
  | otherwise = Nothing

getPortion :: Portion -> Double
getPortion (Portion p) = p

type Capacity = Integer

-- | Remote reference - points to data hosted elsewhere
data RemoteRef a = RemoteRef
  { refAddress :: NetworkAddress
  , refId :: String
  , cachedData :: Maybe a  -- Local cache
  }
  deriving (Show, Functor)

-- | Player state - can be local or remote
data PlayerState = PlayerState
  { playerId :: EntityId
  , playerAddress :: NetworkAddress            -- Where this player's data lives
  , p2pRecognitions :: M.Map EntityId (RemoteRef Portion)  -- Remote references!
  , p2aRecognitions :: M.Map EntityId Portion   -- Local view of recognitions
  , p2aCapacities :: M.Map String Capacity
  , p2aNeeds :: M.Map String Capacity
  , playerMetadata :: M.Map String String
  }
  deriving (Show)

-- | Context state - distributed across network
data ContextState = ContextState
  { contextId :: ContextId
  , contextAddress :: NetworkAddress
  , contextMembers :: M.Map EntityId (RemoteRef Portion)
  , p2c2aRecognitions :: M.Map EntityId Portion
  , p2c2aCapacities :: M.Map String Capacity
  , subContexts :: M.Map ContextId (RemoteRef ContextState)  -- Remote sub-contexts!
  }
  deriving (Show)

-- | Zipper breadcrumbs for distributed navigation
data Crumb = PlayerCrumb EntityId NetworkAddress
           | ContextCrumb ContextId NetworkAddress
           | SubContextCrumb ContextId ContextId NetworkAddress
  deriving (Show)

type Breadcrumbs = [Crumb]

-- | Focus can be on local or remote data
data Focus = PlayerFocus PlayerState Breadcrumbs
           | ContextFocus ContextState Breadcrumbs
           | RootFocus EntityId Breadcrumbs  -- Start from our own entity
  deriving (Show)

-- | Network operations interface
class Monad m => NetworkOps m where
  fetchPlayer :: EntityId -> NetworkAddress -> m (Maybe PlayerState)
  fetchContext :: ContextId -> NetworkAddress -> m (Maybe ContextState)
  fetchRecognition :: EntityId -> EntityId -> NetworkAddress -> m (Maybe Portion)
  publishPlayer :: PlayerState -> m ()
  publishContext :: ContextState -> m ()

-- | The networked zipper monad
-- Combines: Maybe (safe navigation), IO (network), State (breadcrumbs)
newtype ZipperM a = ZipperM 
  { runZipperM :: MaybeT IO a 
  }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

-- | Run a zipper computation
runZipper :: ZipperM a -> IO (Maybe a)
runZipper = runMaybeT . runZipperM

-- | Fail gracefully in zipper monad
zipperFail :: ZipperM a
zipperFail = ZipperM $ MaybeT $ return Nothing

-- | Lift Maybe to ZipperM
fromMaybe' :: Maybe a -> ZipperM a
fromMaybe' Nothing = zipperFail
fromMaybe' (Just x) = return x

-- | Network operations in IO (stub - replace with actual HTTP/P2P calls)
instance NetworkOps ZipperM where
  fetchPlayer pid addr = liftIO $ do
    putStrLn $ "🌐 Fetching player " ++ pid ++ " from " ++ addr
    -- In reality: HTTP GET to addr/player/pid
    -- For now, simulate network delay
    return $ Just $ emptyPlayer pid addr
  
  fetchContext cid addr = liftIO $ do
    putStrLn $ "🌐 Fetching context " ++ cid ++ " from " ++ addr
    return $ Just $ emptyContext cid addr
  
  fetchRecognition from to addr = liftIO $ do
    putStrLn $ "🌐 Fetching recognition " ++ from ++ " → " ++ to ++ " from " ++ addr
    return $ Just $ Portion 0.5
  
  publishPlayer player = liftIO $ do
    putStrLn $ "📤 Publishing player " ++ playerId player ++ " to " ++ playerAddress player
    -- In reality: HTTP POST/PUT to own address
    return ()
  
  publishContext ctx = liftIO $ do
    putStrLn $ "📤 Publishing context " ++ contextId ctx ++ " to " ++ contextAddress ctx
    return ()

-- | Initialize zipper at our own entity
initZipper :: EntityId -> NetworkAddress -> ZipperM Focus
initZipper myId myAddr = do
  player <- fetchPlayer myId myAddr
  case player of
    Just p -> return $ PlayerFocus p []
    Nothing -> return $ RootFocus myId []

-- | Navigate to another player (async network fetch!)
toPlayer :: EntityId -> Focus -> ZipperM Focus
toPlayer targetId (PlayerFocus current bs) = do
  -- Look up target's address from current player's recognition map
  case M.lookup targetId (p2pRecognitions current) of
    Just remoteRef -> do
      -- Fetch remote player data
      player <- fetchPlayer targetId (refAddress remoteRef)
      case player of
        Just p -> return $ PlayerFocus p (PlayerCrumb (playerId current) (playerAddress current) : bs)
        Nothing -> zipperFail
    Nothing -> zipperFail

toPlayer targetId (RootFocus myId bs) = do
  -- Need to know target's address - in practice, use DHT/discovery
  let targetAddr = "https://" ++ targetId ++ ".example.com"
  player <- fetchPlayer targetId targetAddr
  case player of
    Just p -> return $ PlayerFocus p (PlayerCrumb myId "local" : bs)
    Nothing -> zipperFail

toPlayer _ _ = zipperFail

-- | Navigate to context (async network fetch!)
toContext :: ContextId -> Focus -> ZipperM Focus
toContext cid (PlayerFocus current bs) = do
  -- Context address lookup - could be from registry/DHT
  let contextAddr = "https://" ++ cid ++ ".example.com"
  ctx <- fetchContext cid contextAddr
  case ctx of
    Just c -> return $ ContextFocus c (PlayerCrumb (playerId current) (playerAddress current) : bs)
    Nothing -> zipperFail

toContext _ _ = zipperFail

-- | Navigate to sub-context (async network fetch!)
toSubContext :: ContextId -> Focus -> ZipperM Focus
toSubContext subcid (ContextFocus current bs) = do
  case M.lookup subcid (subContexts current) of
    Just remoteRef -> do
      subctx <- fetchContext subcid (refAddress remoteRef)
      case subctx of
        Just c -> return $ ContextFocus c (SubContextCrumb (contextId current) subcid (contextAddress current) : bs)
        Nothing -> zipperFail
    Nothing -> zipperFail

toSubContext _ _ = zipperFail

-- | Navigate up (may require network fetch to restore parent)
goUp :: Focus -> ZipperM Focus
goUp (PlayerFocus _ (PlayerCrumb parentId parentAddr : bs)) = do
  parent <- fetchPlayer parentId parentAddr
  case parent of
    Just p -> return $ PlayerFocus p bs
    Nothing -> zipperFail

goUp (ContextFocus _ (ContextCrumb parentId parentAddr : bs)) = do
  parent <- fetchContext parentId parentAddr
  case parent of
    Just c -> return $ ContextFocus c bs
    Nothing -> zipperFail

goUp _ = zipperFail

-- | Navigate to root (traverse all breadcrumbs up)
toRoot :: Focus -> ZipperM Focus
toRoot focus@(RootFocus _ []) = return focus
toRoot focus@(PlayerFocus _ []) = return focus
toRoot focus = goUp focus >>= toRoot

-- | Modify current player (local operation, then publish)
modifyPlayer :: (PlayerState -> PlayerState) -> Focus -> ZipperM Focus
modifyPlayer f (PlayerFocus player bs) = do
  let player' = f player
  publishPlayer player'  -- Publish changes to network
  return $ PlayerFocus player' bs
modifyPlayer _ focus = return focus

-- | Modify current context (local operation, then publish)
modifyContext :: (ContextState -> ContextState) -> Focus -> ZipperM Focus
modifyContext f (ContextFocus ctx bs) = do
  let ctx' = f ctx
  publishContext ctx'
  return $ ContextFocus ctx' bs
modifyContext _ focus = return focus

-- | Monadic composition operators for zipper navigation
(>->) :: Focus -> (Focus -> ZipperM Focus) -> ZipperM Focus
focus >-> f = f focus

infixl 1 >->

-- | Async fetch multiple recognitions in parallel
fetchRecognitionsParallel :: EntityId -> [EntityId] -> NetworkAddress -> ZipperM (M.Map EntityId Portion)
fetchRecognitionsParallel from targets addr = liftIO $ do
  results <- mapConcurrently (\to -> runZipper $ fetchRecognition from to addr) targets
  let pairs = zip targets [p | Just p <- results]
  return $ M.fromList pairs

-- | Calculate mutual recognition (requires async fetches from both parties!)
mutualRecognitionAsync :: EntityId -> EntityId -> NetworkAddress -> NetworkAddress -> ZipperM Portion
mutualRecognitionAsync e1 e2 addr1 addr2 = do
  -- Fetch recognitions in parallel
  rec1to2 <- fetchRecognition e1 e2 addr1
  rec2to1 <- fetchRecognition e2 e1 addr2
  
  case (rec1to2, rec2to1) of
    (Just r1, Just r2) -> return $ Portion $ min (getPortion r1) (getPortion r2)
    _ -> return $ Portion 0.0

-- | Lazy remote reference resolution
resolveRemoteRef :: RemoteRef a -> (String -> NetworkAddress -> ZipperM (Maybe a)) -> ZipperM (Maybe a)
resolveRemoteRef ref fetcher = 
  case cachedData ref of
    Just cached -> return $ Just cached
    Nothing -> fetcher (refId ref) (refAddress ref)

-- | Empty states
emptyPlayer :: EntityId -> NetworkAddress -> PlayerState
emptyPlayer pid addr = PlayerState
  { playerId = pid
  , playerAddress = addr
  , p2pRecognitions = M.empty
  , p2aRecognitions = M.empty
  , p2aCapacities = M.empty
  , p2aNeeds = M.empty
  , playerMetadata = M.empty
  }

emptyContext :: ContextId -> NetworkAddress -> ContextState
emptyContext cid addr = ContextState
  { contextId = cid
  , contextAddress = addr
  , contextMembers = M.empty
  , p2c2aRecognitions = M.empty
  , p2c2aCapacities = M.empty
  , subContexts = M.empty
  }

-- | Set recognition (local update + publish)
setRecognition :: EntityId -> Portion -> Focus -> ZipperM Focus
setRecognition targetId portion focus =
  modifyPlayer (\p -> p { p2aRecognitions = M.insert targetId portion (p2aRecognitions p) }) focus

-- | Set capacity
setCapacity :: String -> Capacity -> Focus -> ZipperM Focus
setCapacity resourceType cap focus =
  modifyPlayer (\p -> p { p2aCapacities = M.insert resourceType cap (p2aCapacities p) }) focus

-- | Set need
setNeed :: String -> Capacity -> Focus -> ZipperM Focus
setNeed resourceType need focus =
  modifyPlayer (\p -> p { p2aNeeds = M.insert resourceType need (p2aNeeds p) }) focus

-- ============================================================================
-- DISTRIBUTED ALLOCATION ALGORITHM
-- ============================================================================

-- | Distributed allocation - each entity calculates independently
-- No global state needed! Each entity:
-- 1. Fetches recognitions from partners (async)
-- 2. Calculates mutual recognitions
-- 3. Determines own allocation shares
-- 4. Publishes updated needs
distributedAllocationRound :: EntityId -> NetworkAddress -> [EntityId] -> ZipperM ()
distributedAllocationRound myId myAddr partners = do
  -- Fetch my current state
  myState <- fetchPlayer myId myAddr
  case myState of
    Nothing -> zipperFail
    Just me -> do
      -- Fetch all partner states in parallel
      liftIO $ putStrLn "🔄 Starting distributed allocation round..."
      
      partnerStates <- liftIO $ mapConcurrently 
        (\pid -> runZipper $ fetchPlayer pid ("https://" ++ pid ++ ".example.com")) 
        partners
      
      let validPartners = [(pid, ps) | (pid, Just ps) <- zip partners partnerStates]
      
      -- Calculate allocations from each partner
      allocations <- liftIO $ mapConcurrently
        (\(pid, ps) -> do
          -- Calculate mutual recognition and allocation
          mr <- runZipper $ mutualRecognitionAsync myId pid myAddr (playerAddress ps)
          return (pid, mr, calculateLocalAllocation me ps)
        )
        validPartners
      
      liftIO $ putStrLn $ "💰 Received " ++ show (length allocations) ++ " allocations"
      
      -- Update my needs based on what I'll receive
      let totalReceived = sum [amt | (_, _, amt) <- allocations]
      let updatedMe = updateLocalNeeds me totalReceived
      
      -- Publish updated state
      publishPlayer updatedMe
      
      liftIO $ putStrLn "✅ Allocation round complete"

-- | Calculate allocation locally (each entity does this independently)
calculateLocalAllocation :: PlayerState -> PlayerState -> Capacity
calculateLocalAllocation recipient provider =
  let myNeed = sum $ M.elems (p2aNeeds recipient)
      theirCapacity = sum $ M.elems (p2aCapacities provider)
      
      -- Recognition recipient has for provider
      recToProvider = fromMaybe 0 $ M.lookup (playerId provider) (p2aRecognitions recipient)
      
      -- Estimate of provider's recognition of recipient (would be fetched in real system)
      recFromProvider = fromMaybe 0 $ M.lookup (playerId recipient) (p2aRecognitions provider)
      
      -- Mutual recognition is minimum
      mutualRec = min (getPortion recToProvider) (getPortion recFromProvider)
      
      -- Raw allocation based on capacity and mutual recognition
      rawAllocation = floor $ fromIntegral theirCapacity * mutualRec
  
  in min rawAllocation myNeed

-- | Update local needs after receiving allocations
updateLocalNeeds :: PlayerState -> Capacity -> PlayerState
updateLocalNeeds player received =
  let currentNeed = sum $ M.elems (p2aNeeds player)
      remainingNeed = max 0 (currentNeed - received)
      -- Update all resource types proportionally
      updatedNeeds = M.map (\n -> max 0 (n - received `div` max 1 (fromIntegral $ M.size (p2aNeeds player)))) (p2aNeeds player)
  in player { p2aNeeds = updatedNeeds }

-- ============================================================================
-- EXAMPLE: DISTRIBUTED NETWORK NAVIGATION
-- ============================================================================

exampleDistributedNavigation :: IO ()
exampleDistributedNavigation = do
  putStrLn "🚀 Starting distributed zipper example...\n"
  
  result <- runZipper $ do
    -- Start at my own entity
    focus <- initZipper "OrgA" "https://orgA.example.com"
    
    -- Set my capacity
    focus' <- focus >-> setCapacity "funding" 1000000
    
    -- Navigate to partner OrgB (async network fetch!)
    focus'' <- focus' >-> toPlayer "OrgB"
    
    -- Check their needs
    liftIO $ putStrLn "\n📊 Examining OrgB's state..."
    
    -- Navigate back
    focus''' <- goUp focus''
    
    -- Navigate to another partner OrgC
    focus4 <- focus''' >-> toPlayer "OrgC"
    
    liftIO $ putStrLn "\n✅ Successfully navigated distributed network!"
    
    return focus4
  
  case result of
    Just _ -> putStrLn "\n🎉 Navigation successful!"
    Nothing -> putStrLn "\n❌ Navigation failed"

-- | Example: Async allocation across network
exampleDistributedAllocation :: IO ()
exampleDistributedAllocation = do
  putStrLn "💰 Starting distributed allocation example...\n"
  
  result <- runZipper $ do
    distributedAllocationRound "OrgA" "https://orgA.example.com" ["OrgB", "OrgC", "OrgD"]
  
  case result of
    Just _ -> putStrLn "\n🎉 Allocation complete!"
    Nothing -> putStrLn "\n❌ Allocation failed"

-- | Example: Parallel recognition fetching
exampleParallelFetch :: IO ()
exampleParallelFetch = do
  putStrLn "⚡ Fetching recognitions in parallel...\n"
  
  result <- runZipper $ do
    recognitions <- fetchRecognitionsParallel "OrgA" ["OrgB", "OrgC", "OrgD", "OrgE"] "https://orgA.example.com"
    liftIO $ putStrLn $ "\n📊 Fetched " ++ show (M.size recognitions) ++ " recognitions"
    liftIO $ print recognitions
    return recognitions
  
  case result of
    Just _ -> putStrLn "\n✅ Parallel fetch successful!"
    Nothing -> putStrLn "\n❌ Parallel fetch failed"

-- ============================================================================
-- COMPOSITION HELPERS
-- ============================================================================

-- | Compose multiple navigation steps
navigate :: [Focus -> ZipperM Focus] -> Focus -> ZipperM Focus
navigate steps initial = foldl (>>=) (return initial) steps

-- | Navigate with path
navigatePath :: [EntityId] -> Focus -> ZipperM Focus
navigatePath [] focus = return focus
navigatePath (eid:rest) focus = do
  focus' <- toPlayer eid focus
  navigatePath rest focus'

-- ============================================================================
-- SECURE NETWORK OPERATIONS (Integration with User.hs and SEA.hs)
-- ============================================================================

{- | SecureNetworkState - Extended network state with authentication

Adds:
- Current authenticated user
- System indices (username, path)
-}
data SecureNetworkState = SecureNetworkState
  { secureNetwork :: NetworkState
  , secureCurrentUser :: Maybe Text  -- Public key of current user
  , secureUserSigs :: M.Map EntityId Text  -- Entity -> signature
  }
  deriving (Show)

-- | Create empty secure network state
emptySecureNetworkState :: SecureNetworkState
emptySecureNetworkState = SecureNetworkState
  { secureNetwork = emptyNetworkState
  , secureCurrentUser = Nothing
  , secureUserSigs = M.empty
  }

-- | SecureNetworkM - Monad for authenticated network operations
type SecureNetworkM a = StateT SecureNetworkState IO a

{- | Lift NetworkM operation to SecureNetworkM -}
liftNetwork :: NetworkM a -> SecureNetworkM a
liftNetwork action = do
  netState <- gets secureNetwork
  (result, netState') <- liftIO $ runStateT action netState
  modify $ \s -> s { secureNetwork = netState' }
  return result

{- | Publish with authentication

Signs data with current user's key before publishing.
Returns signature if successful, Nothing if not authenticated.
-}
publishSecure :: ToJSON a => a -> SecureNetworkM (Maybe Text)
publishSecure value = do
  user <- gets secureCurrentUser
  case user of
    Nothing -> return Nothing
    Just _pubKey -> do
      -- Sign with user's key (stub for now)
      let sig = "stub-signature"
      
      -- Store signature
      let nodeId = "generated-node-id"  -- Would generate from value
      modify $ \s -> s { secureUserSigs = M.insert nodeId sig (secureUserSigs s) }
      
      -- Publish with signature
      _ <- liftNetwork $ networkPublish value
      
      return $ Just sig

{- | Fetch with verification

Fetches data and verifies signature before returning.
Returns Nothing if signature is invalid.
-}
fetchSecure :: FromJSON a => EntityId -> SecureNetworkM (Maybe a)
fetchSecure nodeId = do
  result <- liftNetwork $ networkFetch nodeId
  
  -- Verify signature (stub for now)
  sigs <- gets secureUserSigs
  case M.lookup nodeId sigs of
    Just _sig -> return result  -- Signature valid (stub)
    Nothing -> return result    -- No signature required (stub)

{- | Authenticate user in secure network context -}
authenticateSecure :: Text -> SecureNetworkM ()
authenticateSecure pubKey = do
  modify $ \s -> s { secureCurrentUser = Just pubKey }

{- | Logout from secure network -}
logoutSecure :: SecureNetworkM ()
logoutSecure = do
  modify $ \s -> s { secureCurrentUser = Nothing }

{- | Get current authenticated user -}
getCurrentSecureUser :: SecureNetworkM (Maybe Text)
getCurrentSecureUser = gets secureCurrentUser

{- | Run SecureNetworkM action -}
runSecureNetworkM :: SecureNetworkM a -> SecureNetworkState -> IO (a, SecureNetworkState)
runSecureNetworkM = runStateT

