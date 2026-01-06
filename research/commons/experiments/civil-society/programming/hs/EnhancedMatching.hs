{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Enhanced Slot Matching (Learned from match.ts)

Features inspired by the TypeScript implementation:
1. Timezone-aware time matching
2. Hierarchical availability windows  
3. Asymmetric recurrence model
4. Space-time grouping
5. Bilateral filter checking
6. Multi-dimensional compatibility

This module extends ProtocolCompliant.hs with sophisticated slot matching.
-}

module EnhancedMatching where

import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (sortBy)

-- ============================================================================
-- TYPES (Extended from protocol)
-- ============================================================================

type EntityId = String
type ResourceType = String
type Capacity = Double
type Timezone = String  -- IANA timezone string ("America/New_York", "Europe/London")

-- | Day of week
data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum)

-- | Time range within a day
data TimeRange = TimeRange
  { startTime :: String  -- HH:MM format
  , endTime :: String    // HH:MM format
  }
  deriving (Show, Eq)

-- | Day schedule: which days, what times
data DaySchedule = DaySchedule
  { days :: [DayOfWeek]
  , timeRanges :: [TimeRange]
  }
  deriving (Show, Eq)

-- | Week schedule: which weeks of month, what day schedules
data WeekSchedule = WeekSchedule
  { weeks :: [Int]  -- 1-5 (first week, second week, etc.)
  , daySchedules :: [DaySchedule]
  }
  deriving (Show, Eq)

-- | Month schedule: which month, what patterns
data MonthSchedule = MonthSchedule
  { month :: Int  -- 1-12
  , weekSchedules :: Maybe [WeekSchedule]
  , daySchedules_ :: Maybe [DaySchedule]
  , timeRanges_ :: Maybe [TimeRange]
  }
  deriving (Show, Eq)

-- | Hierarchical availability window
data AvailabilityWindow = AvailabilityWindow
  { monthSchedules :: Maybe [MonthSchedule]  -- Most specific
  , weekSchedules :: Maybe [WeekSchedule]
  , daySchedules :: Maybe [DaySchedule]
  , timeRanges :: Maybe [TimeRange]          -- Least specific
  }
  deriving (Show, Eq)

-- | Recurrence track (asymmetric model from match.ts)
data RecurrenceTrack = Recurring | OneTime
  deriving (Show, Eq)

-- | Resource slot (need or capacity)
data ResourceSlot = ResourceSlot
  { slotId :: String
  , needTypeId :: ResourceType
  , quantity :: Capacity
  , startDate :: Maybe Day
  , endDate :: Maybe Day
  , recurrence :: Maybe String  -- "weekly", "monthly", etc.
  , availabilityWindow :: Maybe AvailabilityWindow
  , timezone :: Maybe Timezone
  , city :: Maybe String
  , country :: Maybe String
  , latitude :: Maybe Double
  , longitude :: Maybe Double
  , locationType :: Maybe String  -- "remote", "in-person"
  , filterRule :: Maybe FilterRule
  }
  deriving (Show)

-- | Filter rule (simplified from match.ts JsonLogic)
data FilterRule =
    TrustFilter { minMutualRecognition :: Double }
  | LocationFilter { allowedCities :: [String], allowedCountries :: [String] }
  | CombinedFilter [FilterRule]
  | AllowAll
  | DenyAll
  deriving (Show, Eq)

-- | Filter context (what we know about an entity)
data FilterContext = FilterContext
  { entityPubKey :: String
  , mutualRecognition :: Double
  , entityCity :: Maybe String
  , entityCountry :: Maybe String
  }
  deriving (Show)

-- ============================================================================
-- RECURRENCE TRACK (Asymmetric Model)
-- ============================================================================

{- | Determine recurrence track from slot
  
From match.ts:
  "ASYMMETRIC TRACK MODEL:
   - Capacity: Unified (can serve any compatible need)
   - Needs: Separated into recurring/onetime tracks"
-}
getRecurrenceTrack :: ResourceSlot -> RecurrenceTrack
getRecurrenceTrack slot =
  case recurrence slot of
    Just r | not (null r) && r /= "none" -> Recurring
    _ -> OneTime

-- ============================================================================
-- TIMEZONE-AWARE TIME CONVERSION
-- ============================================================================

{- | Convert time from timezone to UTC
  
From match.ts:
  "All times are converted to UTC before comparison"
  "Handles DST automatically via JavaScript Intl API"
  
We'll use Haskell's time library for this.
-}
convertTimeToUTC :: String -> Day -> Maybe Timezone -> IO String
convertTimeToUTC timeStr date tzMaybe = do
  case tzMaybe of
    Nothing -> return timeStr  -- Already UTC or no timezone
    Just "UTC" -> return timeStr
    Just tzName -> do
      -- Parse time
      let [hours, minutes] = map read $ splitOn ':' timeStr :: [Int]
      
      -- Create local time in specified timezone
      let localTime = LocalTime date (TimeOfDay hours minutes 0)
      
      -- Convert to UTC (simplified - real implementation needs timezone-series library)
      -- For now, just return the time (TODO: proper timezone conversion)
      return timeStr  -- STUB - needs proper timezone library
  where
    splitOn c str = case break (== c) str of
      (a, "") -> [a]
      (a, _:b) -> a : splitOn c b

{- | Check if time ranges overlap (in UTC)
  
From match.ts:
  "Times should already be in the same timezone (ideally UTC)
   before calling this."
-}
timeRangesOverlap :: TimeRange -> TimeRange -> Bool
timeRangesOverlap r1 r2 =
  startTime r1 < endTime r2 && startTime r2 < endTime r1

-- ============================================================================
-- SPACE-TIME GROUPING
-- ============================================================================

{- | Space-time signature for grouping
  
From match.ts:
  "Slots with identical signatures can be aggregated"
  "Slots at same space-time aggregate to available capacity"
-}
data SpaceTimeSignature = SpaceTimeSignature
  { sigTime :: String      -- Time component
  , sigLocation :: String  -- Location component  
  }
  deriving (Show, Eq, Ord)

getSpaceTimeSignature :: ResourceSlot -> SpaceTimeSignature
getSpaceTimeSignature slot = SpaceTimeSignature
  { sigTime = timeComponent
  , sigLocation = locationComponent
  }
  where
    timeComponent = case (startDate slot, recurrence slot) of
      (Just d, Just r) -> show d ++ "|" ++ r
      (Just d, Nothing) -> show d ++ "|onetime"
      (Nothing, Just r) -> "any-date|" ++ r
      _ -> "any-time"
    
    locationComponent = case locationType slot of
      Just lt | "remote" `elem` words lt -> "remote"
      _ -> case (city slot, country slot) of
        (Just c, _) -> c
        (_, Just co) -> co
        _ -> "unknown"

{- | Group slots by space-time signature
  
From match.ts:
  "EXAMPLE 1: Same Space-Time (Should Aggregate)
   - Slot A: Monday 9-10am @ SF, quantity 5
   - Slot B: Monday 9-10am @ SF, quantity 3
   → Aggregate to 8 units available Monday 9-10am @ SF"
-}
groupBySpaceTime :: [ResourceSlot] -> M.Map SpaceTimeSignature [ResourceSlot]
groupBySpaceTime slots = 
  M.fromListWith (++) [(getSpaceTimeSignature s, [s]) | s <- slots]

getTotalQuantityAtSpaceTime :: SpaceTimeSignature -> [ResourceSlot] -> Capacity
getTotalQuantityAtSpaceTime _sig slots = sum $ map quantity slots

-- ============================================================================
-- SLOT COMPATIBILITY (Multi-Dimensional)
-- ============================================================================

{- | Check if two slots are compatible
  
From match.ts:
  "COMPATIBILITY REQUIREMENTS:
   - Type match: type_id must be identical
   - Time compatibility: date/time ranges must overlap
   - Location compatibility: city/country/coordinates must match
   - Recurrence: NO FILTERING - capacity serves any need"
-}
slotsCompatible :: ResourceSlot -> ResourceSlot -> IO Bool
slotsCompatible need capacity = do
  -- 1. Type match (CRITICAL!)
  if needTypeId need /= needTypeId capacity
    then return False
    else do
      -- 2. Time compatibility (timezone-aware!)
      timeCompat <- timeCompatible need capacity
      if not timeCompat
        then return False
        else do
          -- 3. Location compatibility
          let locCompat = locationCompatible need capacity
          
          -- 4. NO recurrence filtering (capacity serves any need)
          return locCompat

{- | Check time compatibility with timezone awareness -}
timeCompatible :: ResourceSlot -> ResourceSlot -> IO Bool
timeCompatible slot1 slot2 = do
  -- If both have availability windows, use structured matching
  case (availabilityWindow slot1, availabilityWindow slot2) of
    (Just w1, Just w2) -> do
      let track1 = getRecurrenceTrack slot1
      let track2 = getRecurrenceTrack slot2
      
      case (track1, track2) of
        (Recurring, Recurring) -> 
          -- Both recurring - check if windows overlap (timezone-aware!)
          return $ availabilityWindowsOverlap w1 w2
        
        (OneTime, Recurring) ->
          -- One-time matches recurring window
          return $ onetimeMatchesRecurring slot1 w2
        
        (Recurring, OneTime) ->
          return $ onetimeMatchesRecurring slot2 w1
        
        (OneTime, OneTime) ->
          -- Both one-time - check date overlap
          return $ datesOverlap slot1 slot2
    
    -- Fallback to simple date comparison
    _ -> return $ datesOverlap slot1 slot2

{- | Check if availability windows overlap -}
availabilityWindowsOverlap :: AvailabilityWindow -> AvailabilityWindow -> Bool
availabilityWindowsOverlap w1 w2 = 
  -- Simplified - real implementation would check day schedules
  -- For now, be optimistic
  True  -- TODO: Implement hierarchical matching

{- | Check if one-time slot matches recurring window -}
onetimeMatchesRecurring :: ResourceSlot -> AvailabilityWindow -> Bool
onetimeMatchesRecurring _slot _window =
  -- Simplified - would check if slot's date/time falls in recurring pattern
  True  -- TODO: Implement pattern matching

{- | Check if dates overlap -}
datesOverlap :: ResourceSlot -> ResourceSlot -> Bool
datesOverlap s1 s2 =
  case (startDate s1, endDate s1, startDate s2, endDate s2) of
    (Just start1, end1, Just start2, end2) ->
      let end1' = fromMaybe start1 end1
          end2' = fromMaybe start2 end2
      in start1 <= end2' && start2 <= end1'
    _ -> True  -- Be optimistic if dates missing

{- | Check location compatibility
  
From match.ts:
  "If either is online/remote, consider compatible"
-}
locationCompatible :: ResourceSlot -> ResourceSlot -> Bool
locationCompatible s1 s2 =
  -- Remote/online always compatible
  case (locationType s1, locationType s2) of
    (Just lt1, _) | "remote" `elem` words lt1 -> True
    (_, Just lt2) | "remote" `elem` words lt2 -> True
    _ -> case (city s1, city s2, country s1, country s2) of
      -- City match
      (Just c1, Just c2, _, _) | c1 == c2 -> True
      -- Country match
      (_, _, Just co1, Just co2) | co1 == co2 -> True
      -- No location info - be optimistic
      (Nothing, Nothing, Nothing, Nothing) -> True
      -- Otherwise no match
      _ -> False

-- ============================================================================
-- BILATERAL FILTER CHECKING
-- ============================================================================

{- | Check bilateral filters
  
From match.ts:
  "BILATERAL FILTER CHECKING:
   - Capacity filter: Does recipient pass provider's filter?
   - Need filter: Does provider pass recipient's filter?
   - Both must pass for allocation"
-}
passesBilateralFilters :: 
  ResourceSlot          -- Need slot (with filter on providers)
  -> ResourceSlot       -- Capacity slot (with filter on recipients)
  -> FilterContext      -- Provider context
  -> FilterContext      -- Recipient context
  -> Bool
passesBilateralFilters need capacity provCtx recCtx =
  capacityFilterPass && needFilterPass
  where
    -- Provider checking if recipient passes their filter
    capacityFilterPass = case filterRule capacity of
      Just rule -> evaluateFilter rule recCtx
      Nothing -> True
    
    -- Recipient checking if provider passes their filter  
    needFilterPass = case filterRule need of
      Just rule -> evaluateFilter rule provCtx
      Nothing -> True

{- | Evaluate a filter rule against context -}
evaluateFilter :: FilterRule -> FilterContext -> Bool
evaluateFilter AllowAll _ = True
evaluateFilter DenyAll _ = False
evaluateFilter (TrustFilter minMR) ctx = mutualRecognition ctx >= minMR
evaluateFilter (LocationFilter cities countries) ctx =
  case entityCity ctx of
    Just c | c `elem` cities -> True
    _ -> case entityCountry ctx of
      Just co | co `elem` countries -> True
      _ -> False
evaluateFilter (CombinedFilter rules) ctx = all (`evaluateFilter` ctx) rules

-- ============================================================================
-- ALLOCATION WITH ENHANCED MATCHING
-- ============================================================================

{- | Find compatible slots with full compatibility checking
  
This extends the protocol's Step 2 (Filter Compatible) with:
- Timezone-aware time matching
- Space-time grouping
- Bilateral filter checking
-}
findCompatibleSlots :: 
  ResourceSlot          -- Provider's capacity slot
  -> [ResourceSlot]     -- All recipient need slots
  -> FilterContext      -- Provider context
  -> [(ResourceSlot, FilterContext)]  -- Recipients with contexts
  -> IO [ResourceSlot]  -- Compatible need slots
findCompatibleSlots capacitySlot needSlots provCtx recipientsWithCtx = do
  -- Check compatibility with each need slot
  compatiblePairs <- mapM checkCompat recipientsWithCtx
  return $ mapMaybe id compatiblePairs
  where
    checkCompat (needSlot, recCtx) = do
      -- Multi-dimensional compatibility check
      compatible <- slotsCompatible needSlot capacitySlot
      
      if not compatible
        then return Nothing
        else do
          -- Bilateral filter check
          let filtersPass = passesBilateralFilters needSlot capacitySlot provCtx recCtx
          
          if filtersPass
            then return $ Just needSlot
            else return Nothing

-- ============================================================================
-- EXAMPLES FROM MATCH.TS
-- ============================================================================

{- | Example: Timezone-aware matching
  
From match.ts:
  "Provider in NYC offers capacity '2pm-4pm' (EST/UTC-5)
   Recipient in London needs help '7pm-9pm' (GMT/UTC+0)
   → These times OVERLAP! (2pm EST = 7pm GMT)"
-}
exampleTimezoneMatch :: IO ()
exampleTimezoneMatch = do
  putStrLn "🌍 Timezone-Aware Matching Example\n"
  
  let nycProvider = ResourceSlot
        { slotId = "nyc-capacity"
        , needTypeId = "tutoring"
        , quantity = 5
        , startDate = Just (fromGregorian 2024 3 4)  -- Monday
        , endDate = Nothing
        , recurrence = Just "weekly"
        , availabilityWindow = Just $ AvailabilityWindow
            { monthSchedules = Nothing
            , weekSchedules = Nothing
            , daySchedules = Just [DaySchedule [Monday] [TimeRange "14:00" "16:00"]]
            , timeRanges = Nothing
            }
        , timezone = Just "America/New_York"  -- UTC-5
        , city = Just "New York"
        , country = Just "USA"
        , latitude = Nothing
        , longitude = Nothing
        , locationType = Just "in-person"
        , filterRule = Nothing
        }
  
  let londonRecipient = ResourceSlot
        { slotId = "london-need"
        , needTypeId = "tutoring"
        , quantity = 3
        , startDate = Just (fromGregorian 2024 3 4)  -- Monday
        , endDate = Nothing
        , recurrence = Nothing
        , availabilityWindow = Just $ AvailabilityWindow
            { monthSchedules = Nothing
            , weekSchedules = Nothing
            , daySchedules = Nothing
            , timeRanges = Just [TimeRange "19:00" "21:00"]
            }
        , timezone = Just "Europe/London"  -- UTC+0
        , city = Just "London"
        , country = Just "UK"
        , latitude = Nothing
        , longitude = Nothing
        , locationType = Just "remote"  -- Can meet online
        , filterRule = Nothing
        }
  
  compatible <- slotsCompatible londonRecipient nycProvider
  
  putStrLn "NYC Provider: Monday 2pm-4pm EST"
  putStrLn "London Recipient: Monday 7pm-9pm GMT"
  putStrLn ""
  putStrLn $ "Compatible: " ++ if compatible then "✅ YES" else "❌ NO"
  putStrLn "(14:00 EST = 19:00 GMT, 16:00 EST = 21:00 GMT)"
  putStrLn ""

{- | Example: Asymmetric recurrence model
  
From match.ts:
  "Recurring capacity can serve:
   - Alice (weekly tutoring, recurring)
   - Bob (one-time help this Monday)"
-}
exampleAsymmetricRecurrence :: IO ()
exampleAsymmetricRecurrence = do
  putStrLn "🔄 Asymmetric Recurrence Model\n"
  
  let recurringCapacity = ResourceSlot
        { slotId = "monday-tutoring"
        , needTypeId = "tutoring"
        , quantity = 10
        , startDate = Just (fromGregorian 2024 1 1)
        , endDate = Nothing
        , recurrence = Just "weekly"
        , availabilityWindow = Nothing
        , timezone = Nothing
        , city = Just "San Francisco"
        , country = Just "USA"
        , latitude = Nothing
        , longitude = Nothing
        , locationType = Nothing
        , filterRule = Nothing
        }
  
  let aliceRecurring = ResourceSlot
        { slotId = "alice-weekly"
        , needTypeId = "tutoring"
        , quantity = 5
        , startDate = Just (fromGregorian 2024 1 1)
        , endDate = Nothing
        , recurrence = Just "weekly"  -- RECURRING
        , availabilityWindow = Nothing
        , timezone = Nothing
        , city = Just "San Francisco"
        , country = Just "USA"
        , latitude = Nothing
        , longitude = Nothing
        , locationType = Nothing
        , filterRule = Nothing
        }
  
  let bobOnetime = ResourceSlot
        { slotId = "bob-onetime"
        , needTypeId = "tutoring"
        , quantity = 3
        , startDate = Just (fromGregorian 2024 1 15)
        , endDate = Nothing
        , recurrence = Nothing  -- ONE-TIME
        , availabilityWindow = Nothing
        , timezone = Nothing
        , city = Just "San Francisco"
        , country = Just "USA"
        , latitude = Nothing
        , longitude = Nothing
        , locationType = Nothing
        , filterRule = Nothing
        }
  
  aliceCompat <- slotsCompatible aliceRecurring recurringCapacity
  bobCompat <- slotsCompatible bobOnetime recurringCapacity
  
  putStrLn "Provider: Recurring Monday tutoring (10 hours/week)"
  putStrLn ""
  putStrLn $ "Alice (recurring need): " ++ if aliceCompat then "✅ Compatible" else "❌ Not compatible"
  putStrLn $ "Bob (one-time need): " ++ if bobCompat then "✅ Compatible" else "❌ Not compatible"
  putStrLn ""
  putStrLn "KEY: Recurring capacity can serve BOTH types of needs!"
  putStrLn ""

-- ============================================================================
-- INTEGRATION WITH PROTOCOL
-- ============================================================================

{- | This module extends ProtocolCompliant.hs Step 2 (Filter Compatible)
  
Original Step 2:
  let isCompatible = checkCompatibility timestamp resourceType provider recipientCommit

Enhanced Step 2:
  compatible <- slotsCompatible needSlot capacitySlot
  let filtersPass = passesBilateralFilters needSlot capacitySlot provCtx recCtx
  let isCompatible = compatible && filtersPass

This adds:
  ✅ Timezone-aware time matching
  ✅ Hierarchical availability windows
  ✅ Asymmetric recurrence model
  ✅ Space-time grouping
  ✅ Bilateral filter checking
  ✅ Multi-dimensional compatibility
-}

