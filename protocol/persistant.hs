{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocol.Persistant where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT)
import Data.Function.Memoize (memoize)
import Data.List (foldl', maximumBy, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust, isJust, isNothing, listToMaybe, maybeToList)
import Data.Maybe qualified as Maybe
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

-- Define the schema for the database

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Node
  nodeId Text
  name Text
  points Int
  manualFulfillment Double Maybe
  UniqueNodeId nodeId
  deriving Show Eq

NodeContributor
  nodeId Text
  contributorId Text
  Primary nodeId contributorId
  deriving Show Eq

NodeChild
  parentId Text
  childId Text
  Primary parentId childId
  deriving Show Eq

Capacity
  capacityId Text
  name Text
  quantity Int
  unit Text
  shareDepth Int
  expanded Bool
  naturalDiv Int
  percentageDiv Double
  hidden Bool
  nodeId Text
  UniqueCapacityId capacityId
  deriving Show Eq

CapacityShare
  shareId Text
  capacityId Text
  nodeId Text
  percentage Double
  computedQuantity Int
  UniqueShareId shareId
  deriving Show Eq

SpaceTimeCoordinate
  capacityId Text
  locationType Int
  allDay Bool
  recurrence Text Maybe
  startDate UTCTime
  startTime UTCTime
  endDate UTCTime
  endTime UTCTime
  timeZone Text
  Foreign Capacity capacityId
  UniqueCapacityCoordinate capacityId
  deriving Show Eq

CustomRecurrenceData
  coordinateId SpaceTimeCoordinateId
  repeatEvery Int
  repeatUnit Int
  recurrenceEndType Int
  recurrenceEndDate UTCTime Maybe
  recurrenceEndCount Int Maybe
  deriving Show Eq

ShareOfGeneralFulfillment
  nodeId Text
  contributorId Text
  shareValue Double
  Primary nodeId contributorId
  deriving Show Eq

ProviderShare
  providerId Text
  recipientId Text
  depth Int
  shareValue Double
  Primary providerId recipientId depth
  deriving Show Eq
|]

-- Data types for in-memory representation

-- Enum types for the database
data RecurrenceUnit = Days | Weeks | Months | Years
  deriving (Show, Eq)

toRecurrenceUnitInt :: RecurrenceUnit -> Int
toRecurrenceUnitInt Days = 0
toRecurrenceUnitInt Weeks = 1
toRecurrenceUnitInt Months = 2
toRecurrenceUnitInt Years = 3

fromRecurrenceUnitInt :: Int -> RecurrenceUnit
fromRecurrenceUnitInt 0 = Days
fromRecurrenceUnitInt 1 = Weeks
fromRecurrenceUnitInt 2 = Months
fromRecurrenceUnitInt 3 = Years
fromRecurrenceUnitInt _ = Days

data RecurrenceEnd
  = Never
  | EndsOn UTCTime
  | EndsAfter Int
  deriving (Show, Eq)

toRecurrenceEndType :: RecurrenceEnd -> Int
toRecurrenceEndType Never = 0
toRecurrenceEndType (EndsOn _) = 1
toRecurrenceEndType (EndsAfter _) = 2

data CustomRecurrence = CustomRecurrence
  { repeatEvery :: Int,
    repeatUnit :: RecurrenceUnit,
    recurrenceEnd :: RecurrenceEnd
  }
  deriving (Show, Eq)

data LocationType
  = Undefined
  | LiveLocation
  | Specific
  deriving (Show, Eq)

toLocationTypeInt :: LocationType -> Int
toLocationTypeInt Undefined = 0
toLocationTypeInt LiveLocation = 1
toLocationTypeInt Specific = 2

fromLocationTypeInt :: Int -> LocationType
fromLocationTypeInt 0 = Undefined
fromLocationTypeInt 1 = LiveLocation
fromLocationTypeInt 2 = Specific
fromLocationTypeInt _ = Undefined

data SpaceTimeCoordinates = SpaceTimeCoordinates
  { locationType :: LocationType,
    allDay :: Bool,
    recurrence :: Maybe Text,
    customRecurrence :: Maybe CustomRecurrence,
    startDate :: UTCTime,
    startTime :: UTCTime,
    endDate :: UTCTime,
    endTime :: UTCTime,
    timeZone :: Text
  }
  deriving (Show, Eq)

data MaxDivisibility = MaxDivisibility
  { naturalDiv :: Int,
    percentageDiv :: Double
  }
  deriving (Show, Eq)

data MemCapacity = MemCapacity
  { capacityId :: Text,
    capacityName :: Text,
    quantity :: Int,
    unit :: Text,
    shareDepth :: Int,
    expanded :: Bool,
    coordinates :: SpaceTimeCoordinates,
    maxDivisibility :: MaxDivisibility,
    hiddenUntilRequestAccepted :: Bool
  }
  deriving (Show, Eq)

-- A share in someone else's capacity
data MemCapacityShare = MemCapacityShare
  { targetCapacity :: MemCapacity,
    sharePercentage :: Double,
    computedQuantity :: Int -- Derived from percentage * capacity quantity, respecting maxDivisibility
  }
  deriving (Show, Eq)

type CapacityInventory = Map.Map Text MemCapacity

type CapacityShares = Map.Map Text MemCapacityShare

type ShareMap = Map.Map Text Double

-- Memory representation of a Node
data MemNode = MemNode
  { memNodeId :: Text,
    memNodeName :: Text,
    memNodePoints :: Int,
    memNodeChildren :: Map.Map Text MemNode,
    memNodeContributors :: Set.Set Text,
    memNodeManualFulfillment :: Maybe Double,
    memNodeCapacities :: CapacityInventory,
    memNodeCapacityShares :: CapacityShares,
    -- Store the maps directly rather than in a cache
    memNodeSOGFMap :: Maybe ShareMap,
    memNodeProviderSharesMap :: Map.Map Int ShareMap
  }
  deriving (Show, Eq)

data Ctx = Ctx
  { ctxParent :: MemNode,
    ctxSiblings :: Map.Map Text MemNode,
    ctxAncestors :: [Ctx]
  }
  deriving (Show)

data TreeZipper = TreeZipper
  { zipperCurrent :: MemNode,
    zipperContext :: Maybe Ctx
  }
  deriving (Show)

type Forest = Map.Map Text TreeZipper

-- Database connection type
type DbPool = Pool SqlBackend

type DbAction a = ReaderT SqlBackend (LoggingT IO) a

-- Create connection pool to the database
createDbPool :: Text -> Text -> Text -> Int -> IO DbPool
createDbPool host dbName user size =
  runStdoutLoggingT $ createPostgresqlPool connectionString size
  where
    connectionString = "host=" <> host <> " dbname=" <> dbName <> " user=" <> user

-- Run migrations
runMigrations :: DbPool -> IO ()
runMigrations pool = runStdoutLoggingT $ runSqlPool (runMigration migrateAll) pool

-- Conversion functions between database and memory models

-- Convert from database Node to MemNode (with basic data only)
dbNodeToMemNode :: Entity Node -> DbAction MemNode
dbNodeToMemNode (Entity _ node) = do
  -- Get contributors
  contributorRecords <- selectList [NodeContributorNodeId ==. nodeNodeId node] []
  let contributors = Set.fromList $ map (nodeContributorContributorId . entityVal) contributorRecords

  -- Get capacities
  capacityRecords <- selectList [CapacityNodeId ==. nodeNodeId node] []
  capacities <- Map.fromList <$> mapM dbCapacityToMemCapacity capacityRecords

  -- Get capacity shares
  shareRecords <- selectList [CapacityShareNodeId ==. nodeNodeId node] []
  shares <- Map.fromList <$> mapM dbShareToMemShare shareRecords

  -- Get SOGF map
  sogfRecords <- selectList [ShareOfGeneralFulfillmentNodeId ==. nodeNodeId node] []
  let sogfMap =
        if null sogfRecords
          then Nothing
          else
            Just $
              Map.fromList $
                map
                  ( \(Entity _ r) ->
                      (shareOfGeneralFulfillmentContributorId r, shareOfGeneralFulfillmentShareValue r)
                  )
                  sogfRecords

  -- Get provider shares map
  providerShareRecords <- selectList [ProviderShareProviderId ==. nodeNodeId node] []
  let providerSharesByDepth = groupProviderSharesByDepth providerShareRecords

  return
    MemNode
      { memNodeId = nodeNodeId node,
        memNodeName = nodeName node,
        memNodePoints = nodePoints node,
        memNodeChildren = Map.empty, -- Children are loaded separately
        memNodeContributors = contributors,
        memNodeManualFulfillment = Just <$> nodeManualFulfillment node,
        memNodeCapacities = capacities,
        memNodeCapacityShares = shares,
        memNodeSOGFMap = sogfMap,
        memNodeProviderSharesMap = providerSharesByDepth
      }
  where
    groupProviderSharesByDepth records =
      let grouped =
            Map.fromListWith (++) $
              map
                ( \(Entity _ r) ->
                    (providerShareDepth r, [(providerShareRecipientId r, providerShareShareValue r)])
                )
                records
       in Map.map Map.fromList grouped

dbCapacityToMemCapacity :: Entity Capacity -> DbAction (Text, MemCapacity)
dbCapacityToMemCapacity (Entity _ cap) = do
  -- Get space-time coordinates
  coordRecord <- getBy $ UniqueCapacityCoordinate (capacityCapacityId cap)
  coordinates <- case coordRecord of
    Nothing -> return defaultCoordinates
    Just (Entity coordId coord) -> do
      -- Get custom recurrence if it exists
      customRec <- selectFirst [CustomRecurrenceDataCoordinateId ==. coordId] []
      return $ dbCoordToMemCoord coord customRec

  return
    ( capacityCapacityId cap,
      MemCapacity
        { capacityId = capacityCapacityId cap,
          capacityName = capacityName cap,
          quantity = capacityQuantity cap,
          unit = capacityUnit cap,
          shareDepth = capacityShareDepth cap,
          expanded = capacityExpanded cap,
          coordinates = coordinates,
          maxDivisibility =
            MaxDivisibility
              { naturalDiv = capacityNaturalDiv cap,
                percentageDiv = capacityPercentageDiv cap
              },
          hiddenUntilRequestAccepted = capacityHidden cap
        }
    )
  where
    defaultCoordinates =
            SpaceTimeCoordinates
        { locationType = Undefined,
                allDay = True,
                recurrence = Nothing,
                customRecurrence = Nothing,
          startDate = undefined,
                startTime = undefined,
                endDate = undefined,
                endTime = undefined,
                timeZone = "UTC"
        }

dbCoordToMemCoord :: SpaceTimeCoordinate -> Maybe (Entity CustomRecurrenceData) -> SpaceTimeCoordinates
dbCoordToMemCoord coord customRec =
  SpaceTimeCoordinates
    { locationType = fromLocationTypeInt (spaceTimeCoordinateLocationType coord),
      allDay = spaceTimeCoordinateAllDay coord,
      recurrence = spaceTimeCoordinateRecurrence coord,
      customRecurrence = case customRec of
        Nothing -> Nothing
        Just (Entity _ rec) ->
          Just $
            CustomRecurrence
              { repeatEvery = customRecurrenceDataRepeatEvery rec,
                repeatUnit = fromRecurrenceUnitInt (customRecurrenceDataRepeatUnit rec),
                recurrenceEnd = case customRecurrenceDataRecurrenceEndType rec of
                  0 -> Never
                  1 -> maybe Never EndsOn (customRecurrenceDataRecurrenceEndDate rec)
                  2 -> maybe Never EndsAfter (customRecurrenceDataRecurrenceEndCount rec)
                  _ -> Never
              },
      startDate = spaceTimeCoordinateStartDate coord,
      startTime = spaceTimeCoordinateStartTime coord,
      endDate = spaceTimeCoordinateEndDate coord,
      endTime = spaceTimeCoordinateEndTime coord,
      timeZone = spaceTimeCoordinateTimeZone coord
    }

dbShareToMemShare :: Entity CapacityShare -> DbAction (Text, MemCapacityShare)
dbShareToMemShare (Entity _ share) = do
  -- Get the target capacity
  capacityRecord <- getBy $ UniqueCapacityId (capacityShareCapacityId share)
  capacity <- case capacityRecord of
    Nothing -> return defaultCapacity
    Just capEntity -> snd <$> dbCapacityToMemCapacity capEntity

  return
    ( capacityShareShareId share,
      MemCapacityShare
        { targetCapacity = capacity,
          sharePercentage = capacitySharePercentage share,
          computedQuantity = capacityShareComputedQuantity share
        }
    )
  where
    defaultCapacity =
      MemCapacity
        { capacityId = "",
          capacityName = "",
          quantity = 0,
          unit = "",
          shareDepth = 1,
          expanded = False,
          coordinates = defaultCoordinates,
          maxDivisibility = MaxDivisibility 1 1.0,
          hiddenUntilRequestAccepted = False
        }
    defaultCoordinates =
            SpaceTimeCoordinates
        { locationType = Undefined,
                allDay = True,
                recurrence = Nothing,
                customRecurrence = Nothing,
                startDate = undefined,
                startTime = undefined,
                endDate = undefined,
                endTime = undefined,
                timeZone = "UTC"
        }

-- Load a full tree from the database
loadNodeTree :: Text -> DbAction TreeZipper
loadNodeTree rootId = do
  rootNodeRecord <- getBy $ UniqueNodeId rootId
  case rootNodeRecord of
    Nothing -> error $ "Root node not found: " ++ T.unpack rootId
    Just rootEntity -> do
      rootMemNode <- dbNodeToMemNode rootEntity
      fullRootMemNode <- loadNodeChildren rootMemNode
      return $ TreeZipper fullRootMemNode Nothing

-- Load children recursively for a node
loadNodeChildren :: MemNode -> DbAction MemNode
loadNodeChildren node = do
  -- Get direct child relationships
  childRecords <- selectList [NodeChildParentId ==. memNodeId node] []

  -- For each child, load its node data
  childrenMap <-
    Map.fromList
      <$> forM
        childRecords
        ( \(Entity _ rel) -> do
            let childId = nodeChildChildId rel
            childNodeRecord <- getBy $ UniqueNodeId childId
            case childNodeRecord of
              Nothing -> return (childId, emptyNode childId)
              Just childEntity -> do
                childMemNode <- dbNodeToMemNode childEntity
                -- Recursively load children
                fullChildMemNode <- loadNodeChildren childMemNode
                return (childId, fullChildMemNode)
        )

  -- Return node with children loaded
  return node {memNodeChildren = childrenMap}
  where
    emptyNode id =
      MemNode
        { memNodeId = id,
          memNodeName = id,
          memNodePoints = 0,
          memNodeChildren = Map.empty,
          memNodeContributors = Set.empty,
          memNodeManualFulfillment = Nothing,
          memNodeCapacities = Map.empty,
          memNodeCapacityShares = Map.empty,
          memNodeSOGFMap = Nothing,
          memNodeProviderSharesMap = Map.empty
        }

-- Load the entire forest
loadForest :: DbAction Forest
loadForest = do
  -- Get all root nodes (nodes that are not children of any other node)
  allNodeIds <- map (nodeNodeId . entityVal) <$> selectList [] []
  allChildIds <- map (nodeChildChildId . entityVal) <$> selectList [] []
  let childIdSet = Set.fromList allChildIds
      rootIds = filter (\nid -> not $ Set.member nid childIdSet) allNodeIds

  -- Load each root node as a tree
      Map.fromList
    <$> forM
      rootIds
      ( \rootId -> do
          tree <- loadNodeTree rootId
          return (rootId, tree)
      )

-- Store a node to the database
storeNode :: MemNode -> DbAction ()
storeNode node = do
  -- Store the basic node data
  let dbNode =
        Node
          { nodeNodeId = memNodeId node,
            nodeName = memNodeName node,
            nodePoints = memNodePoints node,
            nodeManualFulfillment = case memNodeManualFulfillment node of
              Nothing -> Nothing
              Just val -> Just (realToFrac val)
          }
  insertBy dbNode >>= \case
    Left (Entity nid _) -> replace nid dbNode -- Update existing
    Right _ -> return () -- New node inserted

  -- Store contributors
  deleteWhere [NodeContributorNodeId ==. memNodeId node]
  forM_ (Set.toList $ memNodeContributors node) $ \contributorId ->
    insert_ $ NodeContributor (memNodeId node) contributorId

  -- Store parent-child relationships
  forM_ (Map.toList $ memNodeChildren node) $ \(childId, childNode) -> do
    insert_ $ NodeChild (memNodeId node) childId
    storeNode childNode -- Recursively store child node

  -- Store capacities
  deleteWhere [CapacityNodeId ==. memNodeId node]
  forM_ (Map.toList $ memNodeCapacities node) $ \(_, capacity) -> do
    storeCapacity (memNodeId node) capacity

  -- Store capacity shares
  deleteWhere [CapacityShareNodeId ==. memNodeId node]
  forM_ (Map.toList $ memNodeCapacityShares node) $ \(shareId, share) -> do
    storeCapacityShare (memNodeId node) shareId share

  -- Store SOGF map if present
  deleteWhere [ShareOfGeneralFulfillmentNodeId ==. memNodeId node]
  case memNodeSOGFMap node of
    Nothing -> return ()
    Just sogfMap -> forM_ (Map.toList sogfMap) $ \(contributorId, shareValue) ->
      insert_ $ ShareOfGeneralFulfillment (memNodeId node) contributorId (realToFrac shareValue)

  -- Store provider shares maps
  deleteWhere [ProviderShareProviderId ==. memNodeId node]
  forM_ (Map.toList $ memNodeProviderSharesMap node) $ \(depth, shareMap) ->
    forM_ (Map.toList shareMap) $ \(recipientId, shareValue) ->
      insert_ $ ProviderShare (memNodeId node) recipientId depth (realToFrac shareValue)

storeCapacity :: Text -> MemCapacity -> DbAction ()
storeCapacity nodeId capacity = do
  -- Store the capacity
  let dbCapacity =
        Capacity
          { capacityCapacityId = capacityId capacity,
            capacityName = capacityName capacity,
            capacityQuantity = quantity capacity,
            capacityUnit = unit capacity,
            capacityShareDepth = shareDepth capacity,
            capacityExpanded = expanded capacity,
            capacityNaturalDiv = naturalDiv $ maxDivisibility capacity,
            capacityPercentageDiv = percentageDiv $ maxDivisibility capacity,
            capacityHidden = hiddenUntilRequestAccepted capacity,
            capacityNodeId = nodeId
          }

  insertBy dbCapacity >>= \case
    Left (Entity cid _) -> replace cid dbCapacity -- Update existing
    Right _ -> return () -- New capacity inserted

  -- Store coordinates
  let dbCoord =
        SpaceTimeCoordinate
          { spaceTimeCoordinateCapacityId = capacityId capacity,
            spaceTimeCoordinateLocationType = toLocationTypeInt $ locationType $ coordinates capacity,
            spaceTimeCoordinateAllDay = allDay $ coordinates capacity,
            spaceTimeCoordinateRecurrence = recurrence $ coordinates capacity,
            spaceTimeCoordinateStartDate = startDate $ coordinates capacity,
            spaceTimeCoordinateStartTime = startTime $ coordinates capacity,
            spaceTimeCoordinateEndDate = endDate $ coordinates capacity,
            spaceTimeCoordinateEndTime = endTime $ coordinates capacity,
            spaceTimeCoordinateTimeZone = timeZone $ coordinates capacity
          }

  -- Insert or update the coordinates
  coordIdResult <- insertBy dbCoord
  coordId <- case coordIdResult of
    Left (Entity cid _) -> do
      replace cid dbCoord
      return cid
    Right cid -> return cid

  -- Store custom recurrence if present
  case customRecurrence $ coordinates capacity of
    Nothing -> return ()
    Just customRec -> do
      deleteWhere [CustomRecurrenceDataCoordinateId ==. coordId]

      let (endType, endDate, endCount) = case recurrenceEnd customRec of
            Never -> (0, Nothing, Nothing)
            EndsOn date -> (1, Just date, Nothing)
            EndsAfter count -> (2, Nothing, Just count)

      insert_ $
        CustomRecurrenceData
          { customRecurrenceDataCoordinateId = coordId,
            customRecurrenceDataRepeatEvery = repeatEvery customRec,
            customRecurrenceDataRepeatUnit = toRecurrenceUnitInt $ repeatUnit customRec,
            customRecurrenceDataRecurrenceEndType = endType,
            customRecurrenceDataRecurrenceEndDate = endDate,
            customRecurrenceDataRecurrenceEndCount = endCount
          }

storeCapacityShare :: Text -> Text -> MemCapacityShare -> DbAction ()
storeCapacityShare nodeId shareId share = do
  let dbShare =
        CapacityShare
          { capacityShareShareId = shareId,
            capacityShareCapacityId = capacityId $ targetCapacity share,
            capacityShareNodeId = nodeId,
            capacitySharePercentage = realToFrac $ sharePercentage share,
            capacityShareComputedQuantity = computedQuantity share
          }

  insertBy dbShare >>= \case
    Left (Entity sid _) -> replace sid dbShare -- Update existing
    Right _ -> return () -- New share inserted

-- Store the entire forest
storeForest :: Forest -> DbAction ()
storeForest forest = do
  -- Delete all data that's not referenced from a root
  let rootNodeIds = Map.keys forest
      rootNodes = map (zipperCurrent . snd) $ Map.toList forest

  -- Store each root node
  forM_ rootNodes storeNode

-- Helper function to run database actions
runDbAction :: DbPool -> DbAction a -> IO a
runDbAction pool action = runStdoutLoggingT $ runSqlPool action pool

-- Convenience functions for external use

-- Initialize database with schema
initDatabase :: Text -> Text -> Text -> IO DbPool
initDatabase host dbName user = do
  pool <- createDbPool host dbName user 10
  runMigrations pool
  return pool

-- Save a forest to the database
saveForest :: DbPool -> Forest -> IO ()
saveForest pool forest = runDbAction pool $ storeForest forest

-- Load a forest from the database
loadEntireForest :: DbPool -> IO Forest
loadEntireForest pool = runDbAction pool loadForest

-- Get a specific node tree
loadNodeTreeById :: DbPool -> Text -> IO TreeZipper
loadNodeTreeById pool nodeId = runDbAction pool $ loadNodeTree nodeId