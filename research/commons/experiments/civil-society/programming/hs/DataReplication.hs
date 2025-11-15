{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

{- | Data Replication & Distribution using Free Association Protocol

This module demonstrates that the 5-step allocation algorithm works for
ANY resource - including storage, bandwidth, and data replication.

The exact same protocol that allocates food can allocate:
- Storage capacity (who replicates what data)
- Bandwidth (who propagates updates)
- Compute (who runs what code)
- Network routes (who forwards what packets)
-}

module DataReplication where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative (Alternative)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.ByteString as BS
import Crypto.Hash.SHA256 (hash)

-- ============================================================================
-- CORE TYPES (extended for data)
-- ============================================================================

type EntityId = String
type ContentHash = BS.ByteString  -- SHA-256 hash
type Portion = Double
type Bytes = Integer  -- Storage/bandwidth in bytes
type Timestamp = UTCTime

-- | A path in the distributed filesystem
-- Same as StateTree paths, but now they point to DATA
data DataPath = DataPath
  { pathSegments :: [String]
  , pathHash :: Maybe ContentHash  -- For content-addressed storage (IPFS-style)
  }
  deriving (Show, Eq, Ord)

-- | Construct path from string
mkPath :: String -> DataPath
mkPath s = DataPath (splitOn '/' s) Nothing
  where
    splitOn :: Char -> String -> [String]
    splitOn _ "" = []
    splitOn c str = case break (== c) str of
      (a, "") -> [a]
      (a, _:b) -> a : splitOn c b

-- | Content-addressed path (IPFS/Git style)
contentPath :: ContentHash -> DataPath
contentPath hash = DataPath ["content", show hash] (Just hash)

-- ============================================================================
-- DATA BLOB (What we're replicating)
-- ============================================================================

data DataBlob = DataBlob
  { blobContent :: BS.ByteString
  , blobSize :: Bytes
  , blobHash :: ContentHash
  , blobMimeType :: String
  , blobOwner :: EntityId
  , blobCreated :: Timestamp
  }
  deriving (Show)

mkBlob :: EntityId -> String -> BS.ByteString -> Timestamp -> DataBlob
mkBlob owner mimeType content timestamp = DataBlob
  { blobContent = content
  , blobSize = fromIntegral $ BS.length content
  , blobHash = hash content
  , blobMimeType = mimeType
  , blobOwner = owner
  , blobCreated = timestamp
  }

-- ============================================================================
-- OSCILLATION DETECTION (for data access patterns)
-- ============================================================================

data AccessPattern = AccessPattern
  { accessHistory :: [(Timestamp, AccessType)]
  , accessCount :: Int
  , thrashingDetected :: Bool
  , dampingFactor :: Double
  }
  deriving (Show)

data AccessType = Read | Write | Delete
  deriving (Show, Eq)

emptyAccessPattern :: AccessPattern
emptyAccessPattern = AccessPattern [] 0 False 1.0

-- | Detect thrashing (repeated request/delete cycles)
detectThrashing :: AccessPattern -> Bytes -> (AccessPattern, Double)
detectThrashing pattern requestSize =
  let recentAccesses = take 5 (accessHistory pattern)
      deleteThenRequest = any (\(_, t) -> t == Delete) recentAccesses && requestSize > 0
      
      dampingFactor' = if deleteThenRequest
                       then 0.7  -- Dampen thrashing
                       else 1.0
      
      pattern' = pattern 
        { thrashingDetected = deleteThenRequest
        , dampingFactor = dampingFactor'
        }
  in (pattern', dampingFactor')

updateAccessPattern :: Timestamp -> AccessType -> AccessPattern -> AccessPattern
updateAccessPattern timestamp accessType pattern = pattern
  { accessHistory = take 10 $ (timestamp, accessType) : accessHistory pattern
  , accessCount = accessCount pattern + 1
  }

-- ============================================================================
-- STORAGE NODE STATE
-- ============================================================================

{- | A storage node in the network
  
  Like a provider in the economic protocol, but for data
-}
data StorageNode = StorageNode
  { nodeId :: EntityId
  , nodeAddress :: String
  , storageCapacity :: Bytes         -- Total storage available
  , storageUsed :: Bytes             -- Currently used
  , bandwidthCapacity :: Bytes       -- Bytes/second
  , recognitions :: M.Map EntityId Portion
  , replicatedData :: M.Map DataPath DataBlob
  , accessPatterns :: M.Map EntityId AccessPattern  -- Per-requester patterns
  , replicationSlots :: [ReplicationSlot]
  }
  deriving (Show)

emptyNode :: EntityId -> String -> Bytes -> Bytes -> StorageNode
emptyNode nid addr storage bandwidth = StorageNode
  { nodeId = nid
  , nodeAddress = addr
  , storageCapacity = storage
  , storageUsed = 0
  , bandwidthCapacity = bandwidth
  , recognitions = M.empty
  , replicatedData = M.empty
  , accessPatterns = M.empty
  , replicationSlots = []
  }

-- | Available storage
availableStorage :: StorageNode -> Bytes
availableStorage node = storageCapacity node - storageUsed node

-- ============================================================================
-- DATA REQUEST (like resource need)
-- ============================================================================

data DataRequest = DataRequest
  { requesterId :: EntityId
  , requesterAddress :: String
  , requestedPath :: DataPath
  , requestSize :: Bytes
  , requestType :: RequestType
  , requestRecognitions :: M.Map EntityId Portion  -- Requester's recognitions
  }
  deriving (Show)

data RequestType = 
    ReadRequest      -- Just need to read
  | ReplicateRequest -- Want a full replica
  | PinRequest       -- Keep it indefinitely
  deriving (Show, Eq)

-- ============================================================================
-- REPLICATION SLOT (like slot allocation)
-- ============================================================================

data ReplicationSlot = ReplicationSlot
  { slotPath :: DataPath
  , slotRecipient :: EntityId
  , slotSize :: Bytes
  , slotProvider :: EntityId
  , slotType :: ReplicationType
  }
  deriving (Show, Eq)

data ReplicationType =
    FullReplica      -- Complete copy
  | CachedCopy       -- Temporary cache
  | StreamingAccess  -- Just streaming, no storage
  deriving (Show, Eq)

-- ============================================================================
-- NETWORK MONAD
-- ============================================================================

newtype DataNetworkM a = DataNetworkM { runDataNetworkM :: MaybeT IO a }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

runDataNetwork :: DataNetworkM a -> IO (Maybe a)
runDataNetwork = runMaybeT . runDataNetworkM

dataNetworkFail :: DataNetworkM a
dataNetworkFail = DataNetworkM $ MaybeT $ return Nothing

-- ============================================================================
-- DATA REPLICATION PROTOCOL (THE SAME 5 STEPS!)
-- ============================================================================

{- | Provider Phase for Data Replication
  
  IDENTICAL structure to economic protocol, but for data!
  
  Step 0: Check access patterns (oscillation detection)
  Step 1: Apply dampening (for thrashing requesters)
  Step 2: Filter compatible (do we have storage/bandwidth?)
  Step 3: Calculate mutual recognition shares
  Step 4: Proportional allocation of storage/bandwidth
  Step 5: Cap at active need (damped request size)
-}
dataProviderPhase :: StorageNode -> [DataRequest] -> DataNetworkM [ReplicationSlot]
dataProviderPhase node requests = do
  timestamp <- liftIO getCurrentTime
  
  let available = availableStorage node
  
  if available <= 0
    then do
      liftIO $ putStrLn $ "   ℹ️  " ++ nodeId node ++ ": No storage capacity"
      return []
    else do
      liftIO $ putStrLn $ "\n🔷 " ++ nodeId node ++ " AS STORAGE PROVIDER:"
      liftIO $ putStrLn $ "   Available: " ++ showBytes available
      liftIO $ putStrLn $ "   Bandwidth: " ++ showBytes (bandwidthCapacity node) ++ "/s"
      
      -- Process each request through 5 steps
      requestData <- mapM (processDataRequest timestamp node) requests
      
      -- Steps 3-5: Proportional distribution
      let slots = distributeStorage available (mapMaybe id requestData) node
      
      liftIO $ putStrLn $ "   📤 Publishing " ++ show (length slots) ++ " replication slots"
      
      return slots

-- | Process single data request (Steps 0-2)
processDataRequest :: Timestamp -> StorageNode -> DataRequest -> DataNetworkM (Maybe (EntityId, DataPath, Bytes, Portion))
processDataRequest timestamp node request = do
  let rid = requesterId request
  let path = requestedPath request
  let requestedSize = requestSize request
  
  -- STEP 0: Check access patterns (oscillation detection!)
  let existingPattern = fromMaybe emptyAccessPattern $ M.lookup rid (accessPatterns node)
  let (updatedPattern, dampingFactor) = detectThrashing existingPattern requestedSize
  
  when (dampingFactor < 1.0) $ liftIO $
    putStrLn $ "   ⚠️  Thrashing detected for " ++ rid ++ " → damping = " ++ show dampingFactor
  
  -- STEP 1: Apply dampening
  let activeSize = floor (fromIntegral requestedSize * dampingFactor)
  
  liftIO $ putStrLn $ "   " ++ rid ++ " requests " ++ show path
  liftIO $ putStrLn $ "      declared=" ++ showBytes requestedSize ++ 
                     ", active=" ++ showBytes activeSize ++ 
                     " (damping=" ++ show dampingFactor ++ ")"
  
  -- STEP 2: Filter compatible (do we have the data? do we have space?)
  let hasData = M.member path (replicatedData node)
  let hasSpace = availableStorage node >= activeSize
  
  if not hasData
    then do
      liftIO $ putStrLn $ "      ❌ Don't have this data"
      return Nothing
    else if not hasSpace
      then do
        liftIO $ putStrLn $ "      ❌ Insufficient storage"
        return Nothing
      else do
        -- STEP 3: Calculate mutual recognition
        let myRecOfThem = fromMaybe 0 $ M.lookup rid (recognitions node)
        let theirRecOfMe = fromMaybe 0 $ M.lookup (nodeId node) (requestRecognitions request)
        let mutualRec = min myRecOfThem theirRecOfMe
        
        liftIO $ putStrLn $ "      MR=" ++ show (mutualRec * 100) ++ "%"
        
        return $ Just (rid, path, activeSize, mutualRec)

-- | Steps 3-5: Distribute storage proportionally
distributeStorage :: Bytes -> [(EntityId, DataPath, Bytes, Portion)] -> StorageNode -> [ReplicationSlot]
distributeStorage nodeCapacity requestData node =
  let totalMR = sum [mr | (_, _, _, mr) <- requestData]
  in if totalMR <= 0
     then []
     else
       [ ReplicationSlot
         { slotPath = path
         , slotRecipient = rid
         , slotSize = min activeSize rawAllocation
         , slotProvider = nodeId node
         , slotType = determineReplicationType activeSize
         }
       | (rid, path, activeSize, mutualRec) <- requestData
       , let share = mutualRec / totalMR
       , let rawAllocation = floor $ fromIntegral nodeCapacity * share
       , rawAllocation > 0
       ]

determineReplicationType :: Bytes -> ReplicationType
determineReplicationType size
  | size > 1_000_000_000 = FullReplica      -- > 1GB
  | size > 1_000_000     = CachedCopy       -- > 1MB
  | otherwise            = StreamingAccess   -- Small files

-- ============================================================================
-- RECIPIENT PHASE (Data Requester)
-- ============================================================================

{- | Recipient aggregates replication offers
  
  Same as economic protocol!
-}
data DataRequester = DataRequester
  { requestorId :: EntityId
  , requestorAddress :: String
  , pendingRequests :: M.Map DataPath Bytes  -- What we need
  , receivedSlots :: [ReplicationSlot]       -- What we've been offered
  , localCache :: M.Map DataPath DataBlob    -- What we have locally
  , recognitionsOut :: M.Map EntityId Portion
  }
  deriving (Show)

dataRecipientPhase :: DataRequester -> [ReplicationSlot] -> DataNetworkM DataRequester
dataRecipientPhase requester incomingSlots = do
  liftIO $ putStrLn $ "\n🔶 " ++ requestorId requester ++ " AS DATA RECIPIENT:"
  
  -- Aggregate slots by path
  let slotsByPath = M.fromListWith (+)
        [(slotPath slot, slotSize slot) | slot <- incomingSlots]
  
  liftIO $ forM_ (M.toList slotsByPath) $ \(path, totalSize) -> do
    let requested = fromMaybe 0 $ M.lookup path (pendingRequests requester)
    putStrLn $ "   Path: " ++ show path
    putStrLn $ "   Requested: " ++ showBytes requested
    putStrLn $ "   Total offered: " ++ showBytes totalSize
    when (totalSize > requested) $
      putStrLn $ "   ⚠️  OVER-ALLOCATION! (" ++ showBytes totalSize ++ " > " ++ showBytes requested ++ ")"
  
  -- Apply UPDATE LAW: Remaining_Need = max(0, Requested - Total_Received)
  let updatedRequests = M.mapWithKey (\path requested ->
        let received = fromMaybe 0 $ M.lookup path slotsByPath
        in max 0 (requested - received)
        ) (pendingRequests requester)
  
  liftIO $ putStrLn $ "   ✅ Updated requests: " ++ show (M.size $ M.filter (> 0) updatedRequests) ++ " remaining"
  
  return requester
    { pendingRequests = updatedRequests
    , receivedSlots = receivedSlots requester ++ incomingSlots
    }

-- ============================================================================
-- FULL ITERATION
-- ============================================================================

dataReplicationIteration :: [StorageNode] -> [DataRequester] -> DataNetworkM ([StorageNode], [DataRequester])
dataReplicationIteration providers requesters = do
  liftIO $ putStrLn "\n═══════════════════════════════════════════════════════"
  liftIO $ putStrLn "      DATA REPLICATION ITERATION"
  liftIO $ putStrLn "═══════════════════════════════════════════════════════"
  
  -- PHASE 1: Providers calculate replication slots
  liftIO $ putStrLn "\n━━━ PHASE 1: STORAGE PROVIDER CALCULATIONS ━━━"
  
  let requesterRequests = map requesterToRequest requesters
  
  providerResults <- mapM (\p -> do
    slots <- dataProviderPhase p (concat requesterRequests)
    return (p { replicationSlots = slots }, slots)
    ) providers
  
  let updatedProviders = map fst providerResults
  let allSlots = concatMap snd providerResults
  
  liftIO $ putStrLn $ "\n📊 Total replication slots offered: " ++ show (length allSlots)
  
  -- PHASE 2: Requesters aggregate and update
  liftIO $ putStrLn "\n━━━ PHASE 2: DATA REQUESTER UPDATES ━━━"
  
  updatedRequesters <- mapM (\r -> do
    let mySlots = filter (\s -> slotRecipient s == requestorId r) allSlots
    dataRecipientPhase r mySlots
    ) requesters
  
  return (updatedProviders, updatedRequesters)

-- | Helper: Convert requester to requests
requesterToRequest :: DataRequester -> [DataRequest]
requesterToRequest r =
  [ DataRequest
    { requesterId = requestorId r
    , requesterAddress = requestorAddress r
    , requestedPath = path
    , requestSize = size
    , requestType = ReplicateRequest
    , requestRecognitions = recognitionsOut r
    }
  | (path, size) <- M.toList (pendingRequests r)
  , size > 0
  ]

-- ============================================================================
-- CONVERGENCE
-- ============================================================================

convergeDataReplication :: Int -> [StorageNode] -> [DataRequester] -> DataNetworkM [DataRequester]
convergeDataReplication maxIters providers requesters = go 1 providers requesters
  where
    go iter provs reqs
      | iter > maxIters = do
          liftIO $ putStrLn $ "\n⏱️  Max iterations (" ++ show maxIters ++ ") reached"
          return reqs
      | allSatisfied reqs = do
          liftIO $ putStrLn $ "\n✅ DATA REPLICATION CONVERGED in " ++ show (iter - 1) ++ " iterations!"
          return reqs
      | otherwise = do
          liftIO $ putStrLn $ "\n📍 Iteration " ++ show iter ++ " / " ++ show maxIters
          (provs', reqs') <- dataReplicationIteration provs reqs
          go (iter + 1) provs' reqs'
    
    allSatisfied reqs = all (\r -> all (<= 0) $ M.elems $ pendingRequests r) reqs

-- ============================================================================
-- UTILITIES
-- ============================================================================

showBytes :: Bytes -> String
showBytes b
  | b >= 1_000_000_000_000 = show (b `div` 1_000_000_000_000) ++ " TB"
  | b >= 1_000_000_000     = show (b `div` 1_000_000_000) ++ " GB"
  | b >= 1_000_000         = show (b `div` 1_000_000) ++ " MB"
  | b >= 1_000             = show (b `div` 1_000) ++ " KB"
  | otherwise              = show b ++ " B"

-- ============================================================================
-- EXAMPLE: RECOGNITION-BASED CDN
-- ============================================================================

{- | Example Scenario: Research Data Distribution
  
  Climate scientists share large datasets.
  Storage nodes replicate based on mutual recognition.
  Important data (high recognition) gets more replication.
-}
exampleDataCDN :: IO ()
exampleDataCDN = do
  putStrLn "🚀 Recognition-Based CDN Example\n"
  putStrLn "Scenario: Climate research data distribution"
  putStrLn "─────────────────────────────────────────────\n"
  
  timestamp <- getCurrentTime
  
  -- Create storage nodes (the "CDN")
  let nodeCarol = (emptyNode "Carol" "https://carol-storage.org" (500 * gb) (100 * mb))
        { recognitions = M.fromList
            [ ("AliceResearch", 0.4)  -- Recognizes Alice's research
            , ("BobInstitute", 0.3)   -- Recognizes Bob's work
            , ("DaveRandom", 0.05)    -- Doesn't really recognize Dave
            ]
        , replicatedData = M.fromList
            [ (mkPath "/research/climate/dataset-2024.csv", 
               mkBlob "AliceResearch" "text/csv" "climate data..." timestamp)
            , (mkPath "/research/models/prediction.py",
               mkBlob "BobInstitute" "text/python" "model code..." timestamp)
            ]
        }
  
  let nodeKitchen = (emptyNode "Kitchen" "https://kitchen-storage.net" (1000 * gb) (200 * mb))
        { recognitions = M.fromList
            [ ("AliceResearch", 0.5)  -- Highly recognizes Alice
            , ("BobInstitute", 0.4)
            , ("DaveRandom", 0.02)
            ]
        , replicatedData = M.fromList
            [ (mkPath "/research/climate/dataset-2024.csv",
               mkBlob "AliceResearch" "text/csv" "climate data..." timestamp)
            ]
        }
  
  -- Create data requesters
  let alice = DataRequester
        { requestorId = "AliceResearch"
        , requestorAddress = "https://alice-university.edu"
        , pendingRequests = M.fromList
            [ (mkPath "/research/climate/dataset-2024.csv", 10 * gb)
            ]
        , receivedSlots = []
        , localCache = M.empty
        , recognitionsOut = M.fromList [("Carol", 0.4), ("Kitchen", 0.5)]
        }
  
  let bob = DataRequester
        { requestorId = "BobInstitute"
        , requestorAddress = "https://bob-institute.org"
        , pendingRequests = M.fromList
            [ (mkPath "/research/climate/dataset-2024.csv", 10 * gb)
            , (mkPath "/research/models/prediction.py", 1 * mb)
            ]
        , receivedSlots = []
        , localCache = M.empty
        , recognitionsOut = M.fromList [("Carol", 0.3), ("Kitchen", 0.4)]
        }
  
  let dave = DataRequester
        { requestorId = "DaveRandom"
        , requestorAddress = "https://dave-random.com"
        , pendingRequests = M.fromList
            [ (mkPath "/research/climate/dataset-2024.csv", 10 * gb)
            ]
        , receivedSlots = []
        , localCache = M.empty
        , recognitionsOut = M.fromList [("Carol", 0.0), ("Kitchen", 0.0)]
        }
  
  -- Run replication protocol
  result <- runDataNetwork $ do
    finalRequesters <- convergeDataReplication 5 [nodeCarol, nodeKitchen] [alice, bob, dave]
    
    liftIO $ putStrLn "\n═══════════════════════════════════════════════════════"
    liftIO $ putStrLn "           FINAL REPLICATION STATE"
    liftIO $ putStrLn "═══════════════════════════════════════════════════════"
    
    liftIO $ forM_ finalRequesters $ \r -> do
      putStrLn $ "\n" ++ requestorId r ++ ":"
      putStrLn $ "  Remaining requests: " ++ show (M.size $ M.filter (> 0) $ pendingRequests r)
      putStrLn $ "  Received slots: " ++ show (length $ receivedSlots r)
      
      when (null $ receivedSlots r) $
        putStrLn $ "  ⚠️  NO REPLICATION (low mutual recognition)"
    
    liftIO $ putStrLn "\n💡 KEY INSIGHTS:"
    liftIO $ putStrLn "  • Alice & Bob: High mutual recognition → data replicated"
    liftIO $ putStrLn "  • Dave: Low/no recognition → denied replication"
    liftIO $ putStrLn "  • Recognition-based CDN prevents freeloading!"
    liftIO $ putStrLn "  • Important data (high recognition) gets priority"
    
    return finalRequesters
  
  case result of
    Just _ -> putStrLn "\n🎉 CDN simulation complete!"
    Nothing -> putStrLn "\n❌ Simulation failed"

-- Constants
gb, mb :: Bytes
gb = 1_000_000_000
mb = 1_000_000

