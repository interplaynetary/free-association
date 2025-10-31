# TimeScheduleBuilder Architecture Redesign

## Problem
Current implementation uses shared `startTime`/`endTime` state for all selected days, making it impossible to set different times for different days.

## Requirements
1. Support different time ranges for different days (e.g., Monday 9-5, Wednesday 10-3)
2. No useEffect - pure props-driven components
3. Clear state management without shared mutable state

## Proposed Architecture

### Option 1: Simple (MVP) - One Time Range Per Slot
**Keep current behavior but make it explicit:**
- User selects days: Mon, Wed, Fri
- User sets ONE time range: 9:00-17:00
- Result: Mon/Wed/Fri all have 9:00-17:00

If user wants different times for different days, they create **separate availability slots**.

**Pros:**
- Matches most common use case
- Simple UI/UX
- Aligns with schema's day_schedules structure
- No major refactoring needed

**Cons:**
- Can't have different times per day in one slot

### Option 2: Per-Day Time Ranges (Complex)
**Support different times per day:**

```typescript
type TimeRangesByDay = Record<DayOfWeek, TimeRange[]>;

// State structure:
{
  selectedDays: ['monday', 'wednesday', 'friday'],
  timeRangesByDay: {
    'monday': [{ start_time: '09:00', end_time: '17:00' }],
    'wednesday': [{ start_time: '10:00', end_time: '15:00' }],
    'friday': [{ start_time: '09:00', end_time: '17:00' }]
  }
}
```

**UI Flow:**
```
1. Select days: Mon, Wed, Fri
2. For each day, set time:
   Mon → [ Start: 09:00 ] → [ End: 17:00 ]
   Wed → [ Start: 10:00 ] → [ End: 15:00 ]
   Fri → [ Start: 09:00 ] → [ End: 17:00 ]
3. Confirm
```

**Navigation:**
```
Days → [ Mon ] → [ Wed ] → [ Fri ] → Confirm
        ↓         ↓         ↓
    Set times Set times Set times
```

**Pros:**
- Maximum flexibility
- Can set different times per day

**Cons:**
- More complex UI
- Longer creation flow
- Most users don't need this

### Option 3: Hybrid - Default + Per-Day Override
**Set default time, optionally override per day:**

```
1. Select days: Mon, Wed, Fri
2. Set default time: 09:00-17:00
3. Override specific days:
   Wed → Override → 10:00-15:00
4. Confirm
```

**Pros:**
- Fast for common case (same time)
- Flexible for exceptions

**Cons:**
- More complex UI
- Need override mechanism

## Recommended: Option 1 (MVP)

**Rationale:**
1. Matches 90% of use cases
2. Simple, intuitive UI
3. Users can create multiple slots for different times
4. No architectural complexity

**For complex schedules:**
- User creates multiple availability slots:
  - Slot 1: Monday 9-5 (tutoring)
  - Slot 2: Wednesday 10-3 (tutoring)
  - Slot 3: Friday 9-5 (tutoring)

This is actually more flexible because each slot can have:
- Different quantities
- Different locations
- Different recurrence patterns

## Implementation for Option 1

### Key Changes:
1. **Make UI explicit**: Show which days will get the same time
2. **Clarify in confirmation**: "Mon/Wed/Fri will all be 9:00-17:00"
3. **Document pattern**: Multiple slots for complex schedules

### No Code Changes Needed!
The current implementation already does this - we just need to make it clearer in the UI that the time applies to ALL selected days.

### UI Improvements:
```
Step: Select Days
[ ] Monday
[✓] Wednesday  
[✓] Friday

Step: Set Time Range
This time will apply to: Wednesday, Friday

Start: [ 09:00 ]
End:   [ 17:00 ]

Tip: To set different times for different days, create separate availability slots.
```

## If We Need Option 2 Later

We can implement it as an advanced mode:

```typescript
// Simple mode (current):
<TimeScheduleBuilder mode="simple" />

// Advanced mode (per-day):
<TimeScheduleBuilder mode="per-day" />
```

This keeps the simple path clean while supporting power users.

