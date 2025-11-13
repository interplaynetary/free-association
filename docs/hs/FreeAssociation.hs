{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module FreeAssociation where

import qualified Data.Map.Strict as M
import Data.List (break, foldl')
import Data.Maybe (fromMaybe)

-- | Core types from equations.md
type EntityId = String
type ContextId = String

-- | Portion: Recognition percentages (0.0 to 1.0)
-- Invariant: Sum of all recognitions from one entity = 1.0
newtype Portion = Portion Double
  deriving (Eq, Ord)

instance Show Portion where
  show (Portion p) = show (p * 100) ++ "%"

mkPortion :: Double -> Maybe Portion
mkPortion p 
  | p >= 0.0 && p <= 1.0 = Just (Portion p)
  | otherwise = Nothing

unsafePortion :: Double -> Portion
unsafePortion = Portion

getPortion :: Portion -> Double
getPortion (Portion p) = p

-- | Capacity: Natural numbers representing resources
type Capacity = Integer

-- | Path through the state tree
data Path = Root 
          | PlayerPath EntityId Path
          | ContextPath ContextId Path
          | BranchPath String Path
  deriving (Eq, Show)

-- | State tree structure - heterogeneous tree with different data at different paths
data StateTree = StateTree
  { players :: M.Map EntityId PlayerState
  , contexts :: M.Map ContextId ContextState
  }
  deriving (Show)

-- | Player-level state (p2a)
data PlayerState = PlayerState
  { playerId :: EntityId
  , p2pRecognitions :: M.Map EntityId Portion        -- Direct recognitions
  , p2cRecognitions :: M.Map ContextId Portion       -- Context memberships
  , p2aRecognitions :: M.Map EntityId Portion        -- General recognitions
  , p2aCapacities :: M.Map String Capacity           -- Available resources
  , p2aNeeds :: M.Map String Capacity                -- Declared needs
  , playerMetadata :: M.Map String String            -- Extensible metadata
  }
  deriving (Show)

-- | Context-level state (p2c2a)
data ContextState = ContextState
  { contextId :: ContextId
  , contextMembers :: M.Map EntityId Portion         -- Member recognitions
  , p2c2aRecognitions :: M.Map EntityId Portion      -- Collective recognitions
  , p2c2aCapacities :: M.Map String Capacity         -- Collective resources
  , p2c2aNeeds :: M.Map String Capacity              -- Collective needs
  , contextMetadata :: M.Map String String           -- Extensible metadata
  , subContexts :: M.Map ContextId ContextState      -- Tree of contexts
  }
  deriving (Show)

-- | Zipper breadcrumbs for navigating the state tree
data Crumb = PlayerCrumb EntityId (M.Map EntityId PlayerState)
           | ContextCrumb ContextId (M.Map ContextId ContextState)
           | SubContextCrumb ContextId ContextId (M.Map ContextId ContextState)
  deriving (Show)

type Breadcrumbs = [Crumb]

-- | Zipper: Focus on part of state tree with breadcrumbs
data Focus = PlayerFocus PlayerState Breadcrumbs
           | ContextFocus ContextState Breadcrumbs
           | TreeFocus StateTree Breadcrumbs
  deriving (Show)

type Zipper = Maybe Focus

-- | Empty initial state
emptyState :: StateTree
emptyState = StateTree M.empty M.empty

-- | Empty player
emptyPlayer :: EntityId -> PlayerState
emptyPlayer pid = PlayerState
  { playerId = pid
  , p2pRecognitions = M.empty
  , p2cRecognitions = M.empty
  , p2aRecognitions = M.empty
  , p2aCapacities = M.empty
  , p2aNeeds = M.empty
  , playerMetadata = M.empty
  }

-- | Empty context
emptyContext :: ContextId -> ContextState
emptyContext cid = ContextState
  { contextId = cid
  , contextMembers = M.empty
  , p2c2aRecognitions = M.empty
  , p2c2aCapacities = M.empty
  , p2c2aNeeds = M.empty
  , contextMetadata = M.empty
  , subContexts = M.empty
  }

-- | Initialize zipper at tree root
initZipper :: StateTree -> Zipper
initZipper tree = Just $ TreeFocus tree []

-- | Navigate to a player
toPlayer :: EntityId -> Zipper -> Zipper
toPlayer pid (Just (TreeFocus tree bs)) = 
  case M.lookup pid (players tree) of
    Just player -> Just $ PlayerFocus player (PlayerCrumb pid (players tree) : bs)
    Nothing -> Nothing
toPlayer _ _ = Nothing

-- | Navigate to a context
toContext :: ContextId -> Zipper -> Zipper
toContext cid (Just (TreeFocus tree bs)) =
  case M.lookup cid (contexts tree) of
    Just ctx -> Just $ ContextFocus ctx (ContextCrumb cid (contexts tree) : bs)
    Nothing -> Nothing
toContext _ _ = Nothing

-- | Navigate to a sub-context within a context
toSubContext :: ContextId -> Zipper -> Zipper
toSubContext subcid (Just (ContextFocus ctx bs)) =
  case M.lookup subcid (subContexts ctx) of
    Just subctx -> Just $ ContextFocus subctx (SubContextCrumb (contextId ctx) subcid (subContexts ctx) : bs)
    Nothing -> Nothing
toSubContext _ _ = Nothing

-- | Navigate up in the tree
goUp :: Zipper -> Zipper
goUp (Just (PlayerFocus player (PlayerCrumb pid allPlayers : bs))) =
  Just $ TreeFocus (StateTree (M.insert pid player allPlayers) M.empty) bs
goUp (Just (ContextFocus ctx (ContextCrumb cid allContexts : bs))) =
  Just $ TreeFocus (StateTree M.empty (M.insert cid ctx allContexts)) bs
goUp (Just (ContextFocus subctx (SubContextCrumb parentId subcid allSubCtx : bs))) =
  let updatedParent = emptyContext parentId 
                      { subContexts = M.insert subcid subctx allSubCtx }
  in Just $ ContextFocus updatedParent bs
goUp _ = Nothing

-- | Navigate to tree root
toRoot :: Zipper -> Zipper
toRoot z@(Just (TreeFocus _ [])) = z
toRoot (Just focus) = toRoot (goUp (Just focus))
toRoot Nothing = Nothing

-- | Modify player state
modifyPlayer :: (PlayerState -> PlayerState) -> Zipper -> Zipper
modifyPlayer f (Just (PlayerFocus player bs)) = Just $ PlayerFocus (f player) bs
modifyPlayer _ z = z

-- | Modify context state
modifyContext :: (ContextState -> ContextState) -> Zipper -> Zipper
modifyContext f (Just (ContextFocus ctx bs)) = Just $ ContextFocus (f ctx) bs
modifyContext _ z = z

-- | Add or update player
addPlayer :: PlayerState -> StateTree -> StateTree
addPlayer player tree = tree { players = M.insert (playerId player) player (players tree) }

-- | Add or update context
addContext :: ContextState -> StateTree -> StateTree
addContext ctx tree = tree { contexts = M.insert (contextId ctx) ctx (contexts tree) }

-- | Set recognition from one player to another
setRecognition :: EntityId -> EntityId -> Portion -> Zipper -> Zipper
setRecognition _from to portion z =
  modifyPlayer (\p -> p { p2aRecognitions = M.insert to portion (p2aRecognitions p) }) z

-- | Set capacity for a resource type
setCapacity :: String -> Capacity -> Zipper -> Zipper
setCapacity resourceType cap z =
  modifyPlayer (\p -> p { p2aCapacities = M.insert resourceType cap (p2aCapacities p) }) z

-- | Set need for a resource type
setNeed :: String -> Capacity -> Zipper -> Zipper
setNeed resourceType need z =
  modifyPlayer (\p -> p { p2aNeeds = M.insert resourceType need (p2aNeeds p) }) z

-- | Calculate mutual recognition (minimum of bidirectional recognition)
mutualRecognition :: StateTree -> EntityId -> EntityId -> Portion
mutualRecognition tree e1 e2 =
  let rec1to2 = getRecognitionBetween tree e1 e2
      rec2to1 = getRecognitionBetween tree e2 e1
  in Portion $ min (getPortion rec1to2) (getPortion rec2to1)

-- | Get recognition from entity1 to entity2
getRecognitionBetween :: StateTree -> EntityId -> EntityId -> Portion
getRecognitionBetween tree from to =
  case M.lookup from (players tree) of
    Just player -> fromMaybe (Portion 0.0) $ M.lookup to (p2aRecognitions player)
    Nothing -> Portion 0.0

-- | Calculate proportional share for resource allocation
-- Share(Recipient, Provider) = MR(Recipient, Provider) / Σ MR(Provider, All_Recipients)
proportionalShare :: StateTree -> EntityId -> EntityId -> Portion
proportionalShare tree recipient provider =
  let mr = mutualRecognition tree recipient provider
      allRecipients = M.keys (players tree)
      totalMR = sum $ map (\r -> getPortion $ mutualRecognition tree r provider) allRecipients
  in if totalMR > 0
     then Portion (getPortion mr / totalMR)
     else Portion 0.0

-- | Calculate allocation from provider to recipient
-- Raw_Allocation = Provider_Capacity × Share(Recipient, Provider)
-- Final_Allocation = min(Raw_Allocation, Declared_Need)
calculateAllocation :: StateTree -> EntityId -> EntityId -> String -> Capacity
calculateAllocation tree provider recipient resourceType =
  let share = proportionalShare tree recipient provider
      providerCap = getCapacity tree provider resourceType
      recipientNeed = getNeed tree recipient resourceType
      rawAllocation = floor $ fromIntegral providerCap * getPortion share
  in min rawAllocation recipientNeed

-- | Get capacity for a resource type
getCapacity :: StateTree -> EntityId -> String -> Capacity
getCapacity tree eid resourceType =
  case M.lookup eid (players tree) of
    Just player -> fromMaybe 0 $ M.lookup resourceType (p2aCapacities player)
    Nothing -> 0

-- | Get need for a resource type
getNeed :: StateTree -> EntityId -> String -> Capacity
getNeed tree eid resourceType =
  case M.lookup eid (players tree) of
    Just player -> fromMaybe 0 $ M.lookup resourceType (p2aNeeds player)
    Nothing -> 0

-- | Update remaining need after receiving allocation
updateRemainingNeed :: EntityId -> String -> Capacity -> StateTree -> StateTree
updateRemainingNeed eid resourceType received tree =
  case M.lookup eid (players tree) of
    Just player ->
      let currentNeed = fromMaybe 0 $ M.lookup resourceType (p2aNeeds player)
          remainingNeed = max 0 (currentNeed - received)
          updatedPlayer = player { p2aNeeds = M.insert resourceType remainingNeed (p2aNeeds player) }
      in addPlayer updatedPlayer tree
    Nothing -> tree

-- | Validate that total recognition sums to 1.0 (100%)
validateRecognitions :: PlayerState -> Either String PlayerState
validateRecognitions player =
  let total = sum $ map getPortion $ M.elems (p2aRecognitions player)
      epsilon = 0.001  -- Floating point tolerance
  in if abs (total - 1.0) < epsilon
     then Right player
     else Left $ "Recognition sum is " ++ show total ++ ", expected 1.0"

-- | Run allocation round across all players
-- This is the convergence algorithm mentioned in the README
allocationRound :: StateTree -> StateTree
allocationRound tree =
  let allPlayers = M.keys (players tree)
      allocations = [(provider, recipient, calculateAllocation tree provider recipient "default")
                    | provider <- allPlayers
                    , recipient <- allPlayers
                    , provider /= recipient]
      
      -- Update needs based on total received
      updateNeeds eid =
        let totalReceived = sum [amt | (_, r, amt) <- allocations, r == eid]
        in updateRemainingNeed eid "default" totalReceived
  
  in foldl' (flip updateNeeds) tree allPlayers

-- | Converge allocations (run rounds until stable)
-- README mentions: "System converges to stable equilibrium in 5-10 rounds"
convergeAllocations :: Int -> StateTree -> StateTree
convergeAllocations maxRounds tree = go maxRounds tree
  where
    go 0 t = t
    go n t = 
      let t' = allocationRound t
      in if isStable t t'
         then t'
         else go (n - 1) t'
    
    -- Check if allocation has stabilized
    isStable t1 t2 =
      let needs1 = [getNeed t1 eid "default" | eid <- M.keys (players t1)]
          needs2 = [getNeed t2 eid "default" | eid <- M.keys (players t2)]
      in needs1 == needs2

-- | Extract state tree from zipper
extractTree :: Zipper -> Maybe StateTree
extractTree z = 
  case toRoot z of
    Just (TreeFocus tree _) -> Just tree
    _ -> Nothing

-- ============================================================================
-- CONTEXT-MEDIATED COORDINATION (p2c2a)
-- ============================================================================

-- | Add member to context with recognition portion
addMemberToContext :: EntityId -> Portion -> Zipper -> Zipper
addMemberToContext memberId portion z =
  modifyContext (\ctx -> ctx { contextMembers = M.insert memberId portion (contextMembers ctx) }) z

-- | Calculate collective recognition for context
-- Aggregates recognition across all members weighted by their membership
collectiveRecognition :: ContextState -> EntityId -> Portion
collectiveRecognition ctx targetEntity =
  let memberWeights = M.toList (contextMembers ctx)
      contextRecognitions = p2c2aRecognitions ctx
      targetRec = fromMaybe (Portion 0.0) $ M.lookup targetEntity contextRecognitions
  in targetRec

-- | Set collective capacity for context
setContextCapacity :: String -> Capacity -> Zipper -> Zipper
setContextCapacity resourceType cap z =
  modifyContext (\ctx -> ctx { p2c2aCapacities = M.insert resourceType cap (p2c2aCapacities ctx) }) z

-- | Allocate context resources to members based on mutual recognition
allocateContextResources :: ContextState -> StateTree -> M.Map EntityId Capacity
allocateContextResources ctx tree =
  let members = M.keys (contextMembers ctx)
      totalCapacity = sum $ M.elems (p2c2aCapacities ctx)
      
      memberShares = M.fromList
        [ (member, calculateMemberShare ctx tree member)
        | member <- members
        ]
  in memberShares

-- | Calculate member's share of context resources
calculateMemberShare :: ContextState -> StateTree -> EntityId -> Capacity
calculateMemberShare ctx tree member =
  let memberWeight = fromMaybe (Portion 0.0) $ M.lookup member (contextMembers ctx)
      totalWeight = sum $ map getPortion $ M.elems (contextMembers ctx)
      memberNeed = getNeed tree member "default"
      totalCapacity = sum $ M.elems (p2c2aCapacities ctx)
      
      rawShare = floor $ fromIntegral totalCapacity * (getPortion memberWeight / totalWeight)
  in min rawShare memberNeed

-- ============================================================================
-- EXAMPLE USAGE
-- ============================================================================

-- | Build example network from README scenario
exampleNetwork :: StateTree
exampleNetwork =
  let -- Create three organizations
      orgA = (emptyPlayer "OrgA") 
        { p2aRecognitions = M.fromList [("OrgB", unsafePortion 0.5), ("OrgC", unsafePortion 0.5)]
        , p2aCapacities = M.fromList [("funding", 1000000)]
        , p2aNeeds = M.fromList [("funding", 0)]
        }
      
      orgB = (emptyPlayer "OrgB")
        { p2aRecognitions = M.fromList [("OrgA", unsafePortion 0.1), ("OrgC", unsafePortion 0.9)]
        , p2aCapacities = M.fromList [("funding", 500000)]
        , p2aNeeds = M.fromList [("funding", 500000)]
        }
      
      orgC = (emptyPlayer "OrgC")
        { p2aRecognitions = M.fromList [("OrgA", unsafePortion 0.3), ("OrgB", unsafePortion 0.7)]
        , p2aCapacities = M.fromList [("funding", 200000)]
        , p2aNeeds = M.fromList [("funding", 200000)]
        }
  
  in addPlayer orgC $ addPlayer orgB $ addPlayer orgA emptyState

-- | Example: Navigate and modify using zipper
exampleZipperUsage :: Maybe StateTree
exampleZipperUsage = do
  -- Start at root
  z <- initZipper exampleNetwork
  
  -- Navigate to OrgA and increase capacity
  z' <- toPlayer "OrgA" z
  let z'' = setCapacity "funding" 1500000 z'
  
  -- Navigate back to root
  z''' <- toRoot z''
  
  -- Extract final state
  extractTree z'''

-- | Example: Calculate mutual recognition
exampleMutualRecognition :: IO ()
exampleMutualRecognition = do
  let tree = exampleNetwork
  putStrLn "Mutual Recognition between OrgA and OrgB:"
  print $ mutualRecognition tree "OrgA" "OrgB"
  putStrLn "\nMutual Recognition between OrgA and OrgC:"
  print $ mutualRecognition tree "OrgA" "OrgC"
  putStrLn "\nMutual Recognition between OrgB and OrgC:"
  print $ mutualRecognition tree "OrgB" "OrgC"

-- | Example: Run allocation rounds
exampleAllocation :: IO ()
exampleAllocation = do
  let tree = exampleNetwork
  putStrLn "Initial state:"
  putStrLn $ "OrgB needs: " ++ show (getNeed tree "OrgB" "default")
  putStrLn $ "OrgC needs: " ++ show (getNeed tree "OrgC" "default")
  
  let convergedTree = convergeAllocations 10 tree
  
  putStrLn "\nAfter convergence:"
  putStrLn $ "OrgB remaining needs: " ++ show (getNeed convergedTree "OrgB" "default")
  putStrLn $ "OrgC remaining needs: " ++ show (getNeed convergedTree "OrgC" "default")

