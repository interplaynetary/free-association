# Learnings from TypeScript Implementation (match.ts)

**Date:** November 12, 2025  
**Source:** `src/lib/protocol/utils/match.ts` from the TypeScript implementation

## Overview

The TypeScript implementation of Free Association provides **sophisticated slot matching** that goes beyond our initial Haskell implementation. This document captures key insights and enhancements we should incorporate.

---

## 🌍 1. Timezone-Aware Matching

### Problem

**Global coordination requires timezone awareness:**
- Provider in NYC offers capacity "2pm-4pm" (EST/UTC-5)
- Recipient in London needs help "7pm-9pm" (GMT/UTC+0)
- **These times OVERLAP!** (2pm EST = 7pm GMT)

### Solution from match.ts

```typescript
// Convert ALL times to UTC before comparison
function convertTimeToUTC(timeStr: string, dateStr: string, timezone?: string): string

// Handles day shifts automatically:
// Monday 11pm PST → Tuesday 8am CET (day shifts from monday to tuesday!)
```

**Features:**
- Uses IANA timezone strings: `"America/New_York"`, `"Europe/London"`, `"Asia/Tokyo"`
- Handles DST automatically via JavaScript Intl API
- Detects day-boundary crossings
- All comparisons done in UTC

### Status in Our Implementation

❌ **Missing** - `ProtocolCompliant.hs` assumes same timezone  
✅ **Added** - `EnhancedMatching.hs` has stubs for timezone conversion (needs `timezone-series` library)

### Implementation Path

```haskell
-- TODO: Add timezone-series dependency
import Data.Time.Zones

convertTimeToUTC :: String -> Day -> Maybe Timezone -> IO String
convertTimeToUTC timeStr date tzMaybe = do
  case tzMaybe of
    Just tzName -> do
      -- Load timezone data
      tz <- loadTZFromDB tzName
      -- Convert local time to UTC
      ...
```

---

## 📅 2. Hierarchical Availability Windows

### Problem

**Complex recurring patterns can't be expressed with simple date ranges:**
- "First Monday of every month, 2-4pm"
- "Weeks 2 and 4 of March, Tuesdays and Thursdays"
- "Every weekday except holidays"

### Solution from match.ts

**4-level hierarchy:**

```typescript
AvailabilityWindow = {
  month_schedules?: [      // LEVEL 1: Month → Week → Day → Time
    { month: 3, 
      week_schedules: [{ weeks: [1, 3], day_schedules: [...] }] 
    }
  ],
  week_schedules?: [       // LEVEL 2: Week → Day → Time
    { weeks: [2, 4], day_schedules: [...] }
  ],
  day_schedules?: [        // LEVEL 3: Day → Time
    { days: ['monday', 'wednesday'], time_ranges: [...] }
  ],
  time_ranges?: [          // LEVEL 4: Time only (all days)
    { start_time: '14:00', end_time: '16:00' }
  ]
}
```

**Matching logic:**
- Uses most specific level available in each window
- Hierarchically filters: month → week → day → time
- Optimistic when levels missing

### Status in Our Implementation

❌ **Missing** - We only handle basic time ranges  
✅ **Designed** - `EnhancedMatching.hs` has types but not full logic

### Example Use Case

```haskell
-- "First and third Monday of every month, 9am-5pm"
monthSchedule = MonthSchedule
  { month = 0  -- All months
  , weekSchedules = Just [WeekSchedule
      { weeks = [1, 3]  -- First and third week
      , daySchedules = [DaySchedule
          { days = [Monday]
          , timeRanges = [TimeRange "09:00" "17:00"]
          }]
      }]
  , daySchedules_ = Nothing
  , timeRanges_ = Nothing
  }
```

---

## 🔄 3. Asymmetric Recurrence Model

### Insight: Brilliant Design!

From match.ts comments:

```
ASYMMETRIC TRACK MODEL:
- Capacity: Unified (can serve any compatible need, regardless of recurrence)
- Needs: Separated into two tracks:
  - "recurring": Ongoing commitments (weekly tutoring, monthly groceries)
  - "onetime": Discrete requests (help moving, one workshop)

This separation happens naturally at the slot level.
```

### Why This Matters

**Provider's Mental Model:**
> "I don't care if someone needs recurring or one-time help. My Monday tutoring is available - whoever needs it can request it!"

**Recipient's Mental Model:**
> "I have two types of needs:
> - RECURRING: Weekly groceries (need every week indefinitely)
> - ONE-TIME: Help moving next month (discrete event)"

### Implementation

```typescript
function getRecurrenceTrack(slot): 'recurring' | 'onetime' {
  if (slot.recurrence && slot.recurrence !== '' && slot.recurrence !== 'none') {
    return 'recurring';
  }
  return 'onetime';
}

// CRITICAL: No recurrence filtering in slotsCompatible()!
// Capacity can serve ANY compatible need
```

### Example Scenario

**Provider:**
- Capacity: "Monday tutoring, recurring weekly, 10 hours"

**Week 1 Recipients:**
- Alice: "Weekly tutoring, recurring, 5 hours" → Gets 5 hours
- Bob: "One-time tutoring this Monday, 3 hours" → Gets 3 hours  
- Carol: "Weekly tutoring, recurring, 4 hours" → Gets 2 hours (partially)

**Week 2 Recipients:**
- Alice: Still needs 5 hours (recurring) → Gets 5 hours
- Bob: **GONE** (one-time need satisfied, doesn't recur)
- Carol: Still needs 4 hours → Gets 4 hours (fully satisfied)
- 1 hour spare capacity

### Status in Our Implementation

❌ **Missing** - We haven't modeled recurring vs one-time slots  
✅ **Added** - `EnhancedMatching.hs` has `RecurrenceTrack` type and logic

---

## 📍 4. Space-Time Grouping

### Problem

**Not all capacity can be aggregated:**
- Monday @ SF + Monday @ NYC ≠ combined capacity (different space)
- Monday @ SF + Tuesday @ SF ≠ combined capacity (different time)
- Monday @ SF (Slot A) + Monday @ SF (Slot B) = **CAN AGGREGATE** (same space-time!)

### Solution from match.ts

**Space-time signatures for grouping:**

```typescript
function getSpaceTimeSignature(slot): string {
  const timeKey = slot.recurrence + '|' + slot.start_date + '|' + ...
  const locKey = slot.city + '|' + slot.country + '|' + ...
  return `${timeKey}::${locKey}`;
}

function groupSlotsBySpaceTime(slots): Map<signature, {quantity, slots}> {
  // Slots with identical signatures aggregate
}
```

### Examples

**✅ EXAMPLE 1: Same Space-Time (Should Aggregate)**
```
Provider has:
  - Slot A: Monday 9-10am @ SF, quantity 5
  - Slot B: Monday 9-10am @ SF, quantity 3

Signature: "onetime|2024-01-15::san-francisco"
→ Aggregate to 8 units available Monday 9-10am @ SF
```

**❌ EXAMPLE 2: Same Time, Different Space (Cannot Aggregate)**
```
Provider has:
  - Slot A: Monday 9-10am @ SF, quantity 5
  - Slot B: Monday 9-10am @ NYC, quantity 3

→ 5 units in SF + 3 units in NYC (separate pools)
→ SF recipient can only get from Slot A
```

**❌ EXAMPLE 3: Same Space, Different Time (Cannot Aggregate)**
```
Provider has:
  - Slot A: Monday 9-10am @ SF, quantity 5
  - Slot B: Tuesday 9-10am @ SF, quantity 3

→ 5 units Monday + 3 units Tuesday (separate pools)
→ Monday recipient can only get from Slot A
```

**🔀 EXAMPLE 4: Overlapping Time Ranges (Both Apply!)**
```
Provider has:
  - Slot A: Monday 9-11am @ SF, quantity 5
  - Slot B: Monday 10am-12pm @ SF, quantity 3

Recipient needs: Monday 10-11am @ SF
→ Compatible with BOTH slots! (up to 8 units total)
→ The overlap period has both capacities available ✓
```

### Status in Our Implementation

❌ **Missing** - We treat all slots independently  
✅ **Added** - `EnhancedMatching.hs` has `SpaceTimeSignature` and grouping

---

## 🔒 5. Bilateral Filter Checking

### Problem

**Filters must work both ways:**
- Provider: "I only want to help people in SF"
- Recipient: "I only want help from certified tutors"
- **Both filters must pass!**

### Solution from match.ts

```typescript
function passesSlotFilters(
  needSlot,         // Recipient's need (with filter on providers)
  availabilitySlot, // Provider's capacity (with filter on recipients)
  providerContext,
  recipientContext
): boolean {
  // Check provider's filter: Does recipient pass?
  if (availabilitySlot.filter_rule) {
    if (!evaluateFilter(availabilitySlot.filter_rule, recipientContext)) {
      return false; // Provider rejects recipient
    }
  }
  
  // Check recipient's filter: Does provider pass?
  if (needSlot.filter_rule) {
    if (!evaluateFilter(needSlot.filter_rule, providerContext)) {
      return false; // Recipient rejects provider
    }
  }
  
  return true; // Both filters passed
}
```

### Security Implication

**Single-sided filtering is insufficient:**
- If only provider filters, recipients have no control
- If only recipient filters, providers have no control
- **Bilateral filtering = mutual consent**

### Status in Our Implementation

⚠️ **Partial** - `ProtocolCompliant.hs` has Step 2 filters but not bilateral  
✅ **Added** - `EnhancedMatching.hs` has `passesBilateralFilters`

---

## 🧠 6. JsonLogic-Based Filters

### Problem

**Hard-coded filter types are not extensible:**
```typescript
// OLD (discriminated union):
type FilterRule =
  | { type: 'trust', min_mutual_recognition: 0.1 }
  | { type: 'location', allowed_cities: ['SF'] }
  | { type: 'certification', required: ['CPR'] }
```

**Issues:**
- Can't serialize complex logic
- Can't compose filters flexibly
- Hard to extend without code changes

### Solution from match.ts

**Use JsonLogic (serializable, composable):**

```typescript
// NEW (JsonLogic):
type EligibilityFilter = JsonLogic | boolean

// Simple filter:
{">=": [{"var": "mutualRecognition"}, 0.1]}

// Complex composed filter:
{"and": [
  {">=": [{"var": "mutualRecognition"}, 0.1]},
  {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]},
  {"!": {"in": [{"var": "commitment.resource_type"}, ["dangerous"]]}}
]}
```

**Benefits:**
- **Serializable:** Can store in database, send over network
- **Composable:** Combine with `and`, `or`, `not`
- **Extensible:** Add new operators without code changes
- **Secure:** No eval() or arbitrary code execution
- **Expressive:** Can express any boolean logic

### Examples

```typescript
// Trust filter:
{">=": [{"var": "mutualRecognition"}, 0.1]}

// Location filter:
{"or": [
  {"in": [{"var": "commitment.city"}, ["San Francisco", "Oakland"]]},
  {"==": [{"var": "commitment.location_type"}, "remote"]}
]}

// Certification filter:
{"and": [
  {"in": ["CPR", {"var": "attributes.certifications"}]},
  {">=": [{"var": "attributes.certification_level"}, 2]}
]}

// Combined filter (trust + location + certification):
{"and": [
  {">=": [{"var": "mutualRecognition"}, 0.1]},
  {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]},
  {"in": ["CPR", {"var": "attributes.certifications"}]}
]}
```

### Status in Our Implementation

❌ **Missing** - We use simple `ResourceFilters` checks  
⚠️ **Partial** - `EnhancedMatching.hs` has `FilterRule` but not JsonLogic

### Implementation Path

```haskell
-- Would need a JsonLogic evaluator for Haskell
-- Could use aeson for JSON representation

type JsonLogic = Value  -- Aeson Value

evaluateJsonLogic :: JsonLogic -> FilterContext -> Bool
evaluateJsonLogic (Object logic) ctx =
  case lookup "var" logic of
    Just (String varName) -> getVar varName ctx
    _ -> case lookup "and" logic of
      Just (Array rules) -> all (\r -> evaluateJsonLogic r ctx) rules
      _ -> ...
```

---

## 📊 7. Multi-Dimensional Compatibility Matrix

From match.ts, the **complete compatibility checking:**

```typescript
export function slotsCompatible(needSlot, availabilitySlot): boolean {
  // DIMENSION 1: Type compatibility (CRITICAL!)
  if (needSlot.need_type_id !== availabilitySlot.need_type_id) {
    return false;
  }

  // DIMENSION 2: NO recurrence filtering (asymmetric model)
  // Capacity (recurring or one-time) can match any compatible need

  // DIMENSION 3: Time compatibility (timezone-aware!)
  if (!timeRangesOverlap(needSlot, availabilitySlot)) {
    return false;
  }

  // DIMENSION 4: Location compatibility
  if (!locationsCompatible(needSlot, availabilitySlot)) {
    return false;
  }

  // All dimensions passed!
  return true;
}
```

Then **separately** check bilateral filters (not in `slotsCompatible`):

```typescript
// In allocation algorithm:
if (slotsCompatible(need, capacity)) {
  if (passesSlotFilters(need, capacity, providerCtx, recipientCtx)) {
    // Proceed with allocation
  }
}
```

**Design Insight:** Separation of concerns
- `slotsCompatible()` checks **inherent compatibility** (type, time, location)
- `passesSlotFilters()` checks **eligibility** (permissions, trust, rules)

---

## 🎯 Integration with Our Protocol

### Current State (ProtocolCompliant.hs)

```haskell
-- Step 2: Filter Compatible
let isCompatible = checkCompatibility timestamp resourceType provider recipientCommit
  where
    checkCompatibility currentTime resType _prov recip =
      checkTimeWindow currentTime (filters recip)
      && checkLocation (filters recip)
      && checkResourceType resType (filters recip)
```

**What's missing:**
- ❌ No timezone awareness
- ❌ No hierarchical scheduling
- ❌ No recurrence model
- ❌ No space-time grouping
- ❌ No bilateral filtering
- ❌ No JsonLogic filters

### Enhanced State (EnhancedMatching.hs)

```haskell
-- Step 2: Enhanced compatibility check
findCompatibleSlots capacitySlot needSlots provCtx recipientsWithCtx = do
  compatiblePairs <- mapM checkCompat recipientsWithCtx
  return $ mapMaybe id compatiblePairs
  where
    checkCompat (needSlot, recCtx) = do
      -- Multi-dimensional compatibility (timezone-aware!)
      compatible <- slotsCompatible needSlot capacitySlot
      
      if not compatible
        then return Nothing
        else do
          -- Bilateral filter check
          let filtersPass = passesBilateralFilters needSlot capacitySlot provCtx recCtx
          
          if filtersPass
            then return $ Just needSlot
            else return Nothing
```

**What's added:**
- ✅ Timezone-aware time matching (stubs, needs library)
- ✅ Hierarchical scheduling (types defined, logic TODO)
- ✅ Asymmetric recurrence model (full implementation)
- ✅ Space-time grouping (full implementation)
- ✅ Bilateral filtering (full implementation)
- ⚠️ JsonLogic filters (types only, evaluator TODO)

---

## 🚀 Implementation Roadmap

### Phase 1: Critical Features (Do First)

1. **Bilateral Filter Checking** ✅ DONE
   - Already implemented in `EnhancedMatching.hs`
   - Integrate into `ProtocolCompliant.hs` Step 2

2. **Asymmetric Recurrence Model** ✅ DONE
   - Types and logic in `EnhancedMatching.hs`
   - Test with examples

3. **Space-Time Grouping** ✅ DONE
   - Signatures and grouping functions ready
   - Add to allocation algorithm

### Phase 2: Timezone Support

4. **Add timezone-series Dependency**
   ```bash
   cabal install timezone-series timezone-olson
   ```

5. **Implement convertTimeToUTC**
   ```haskell
   import Data.Time.Zones
   import Data.Time.Zones.All
   
   convertTimeToUTC :: String -> Day -> Maybe Timezone -> IO String
   ```

6. **Update Time Matching**
   - Convert both slots to UTC before comparison
   - Handle day-boundary crossings

### Phase 3: Hierarchical Scheduling

7. **Implement Month/Week/Day Schedule Matching**
   ```haskell
   availabilityWindowsOverlap :: AvailabilityWindow -> AvailabilityWindow -> Bool
   onetimeMatchesRecurring :: ResourceSlot -> AvailabilityWindow -> Bool
   ```

8. **Add Pattern Matching Logic**
   - "First Monday of every month"
   - "Weeks 2 and 4, Tuesdays and Thursdays"

### Phase 4: Advanced Filters

9. **Implement JsonLogic Evaluator**
   ```haskell
   evaluateJsonLogic :: Value -> FilterContext -> Bool
   ```

10. **Convert Legacy Filters**
    ```haskell
    convertLegacyFilter :: FilterRule -> JsonLogic
    ```

---

## 📈 Impact Analysis

### Performance

**Space-Time Grouping:**
- Before: O(n²) comparisons for every slot pair
- After: O(n) grouping by signature, then match within groups
- **Speedup:** ~10-100x for large slot arrays

**Timezone Caching:**
- Convert time zones once per slot
- Cache UTC times
- **Speedup:** Avoid repeated conversions

### Expressiveness

**Before:**
- "I need help on Mondays" (vague)
- "I'm available in SF" (no remote option)

**After:**
- "First and third Monday of every month, 2-4pm PST" (precise!)
- "Remote OR within 50km of my location" (flexible!)
- "Certified tutors with 0.1+ mutual recognition" (secure!)

### Global Coordination

**Before:**
- Timezones cause false mismatches
- NYC provider at 2pm can't match London recipient at 7pm (even though times overlap!)

**After:**
- Full timezone awareness
- Global matching works correctly
- Day-boundary crossing handled automatically

---

## 🔐 7. Cryptographic Security (SEA.js)

From the TypeScript implementation's security layer:

```typescript
const SEA = {
  pair: async () => {
    // ECDSA keys for signing/verifying
    // ECDH keys for encryption/decryption
  },
  sign: async (data, pair) => { /* Sign with ECDSA */ },
  verify: async (data, pair) => { /* Verify signature */ },
  encrypt: async (data, pair) => { /* AES-GCM encryption */ },
  decrypt: async (enc, pair) => { /* Decrypt */ },
  secret: async (to, from) => { /* ECDH shared secret */ }
}
```

### Key Features

**1. Dual Key Pairs:**
- **ECDSA (P-256):** For signing/verification (authentication)
- **ECDH (P-256):** For encryption/decryption (confidentiality)

**2. Per-Property Signatures:**
```typescript
node._["s"] = {
  "property1": "signature1...",
  "property2": "signature2..."
}
```

Each property individually signed, enabling:
- Fine-grained verification
- Partial updates (only changed properties)
- Selective disclosure

**3. User Spaces:**
```
~{userPubKey}/
├── public/   # Signed but not encrypted
└── private/  # Encrypted (only user can read)
```

**4. Timestamp Signatures:**
```typescript
signTimestamp: async (timestamp, pair) => { /* Prove ownership */ }
verifyTimestamp: async (timestamp, signature, publicKey) => { /* Verify */ }
```

Prevents replay attacks by binding signatures to timestamps.

**5. Shared Secrets (ECDH):**
```typescript
// Alice encrypts to Bob
const secret = await SEA.secret(bobPair.epub, alicePair)
const encrypted = await SEA.encrypt(data, secret)

// Bob decrypts from Alice
const secret = await SEA.secret(alicePair.epub, bobPair)
const decrypted = await SEA.decrypt(encrypted, secret)
```

### Status in Our Implementation

❌ **Missing** - No cryptographic layer in initial implementation  
✅ **Added** - `SEA.hs` provides full crypto API (stubs, needs cryptonite)

### Security Properties

1. **Authentication:** Can't forge signatures
2. **Non-repudiation:** Can't deny signed commitments
3. **Integrity:** Tampering breaks signatures
4. **Confidentiality:** Encryption protects private data
5. **Freshness:** Timestamps prevent replays
6. **Bilateral trust:** Both parties must sign

### Use Cases

**Verifiable Commitments:**
```haskell
-- Create signed commitment
commitment <- createCommitment "need" needData userKeyPair

-- Verify before trusting
if verifyCommitment commitment
  then trustAndUse commitment
  else rejectFake
```

**Private User Spaces:**
```haskell
-- Store encrypted private need
userSpace' <- storePrivate "my-need" needData userSpace

-- Only user can decrypt
case retrievePrivate "my-need" userSpace' of
  Just need -> -- Decrypted!
  Nothing -> -- Wrong key
```

**Peer-to-Peer Encryption:**
```haskell
-- Alice encrypts to Bob
encrypted <- encryptTo secretData bobKeyPair aliceKeyPair

-- Bob decrypts from Alice
data <- decryptFrom encrypted aliceKeyPair bobKeyPair
```

### Integration Points

**Protocol (ProtocolCompliant.hs):**
- Step 0 (Publish): Sign commitments
- Step 1 (Fetch): Verify signatures
- Step 3 (Calculate): Sign allocations
- Step 5 (Update): Timestamp state changes

**Network (NetworkedZipper.hs):**
- Authenticated references
- Secure fetch operations
- Encrypted user spaces
- Verifiable data only

**Replication (DataReplication.hs):**
- Signed data blocks
- Encrypted private data
- Verifiable allocation proofs

---

## 🎓 Key Takeaways

1. **Timezone awareness is critical** for global coordination
2. **Hierarchical scheduling** enables complex recurring patterns
3. **Asymmetric recurrence model** provides flexibility (capacity serves any need)
4. **Space-time grouping** prevents false aggregation across incompatible slots
5. **Bilateral filtering** ensures mutual consent
6. **JsonLogic** makes filters serializable and composable
7. **Multi-dimensional compatibility** (type + time + location) is the foundation
8. **Cryptographic security** (SEA.js) makes the system trustless and verifiable

---

## 📝 Next Steps

1. **Integrate `EnhancedMatching.hs` into `ProtocolCompliant.hs`**
   - Replace Step 2 compatibility check
   - Add bilateral filter checking

2. **Add Timezone Support**
   - Dependency: `timezone-series`, `timezone-olson`
   - Implement `convertTimeToUTC`

3. **Test with Real-World Scenarios**
   - NYC ↔ London matching
   - Recurring vs one-time needs
   - Space-time aggregation

4. **Documentation**
   - Update `HASKELL_README.md`
   - Add examples to `CompleteProtocolTest.hs`

---

## 🙏 Credits

**TypeScript Implementation:** `src/lib/protocol/utils/match.ts`  
**Insights:** Timezone awareness, hierarchical scheduling, asymmetric recurrence, bilateral filters  
**Date Analyzed:** November 12, 2025

This analysis reveals that the TypeScript implementation has **production-grade matching logic** that we should adopt for the Haskell reference implementation!

