# Allocation Test Coverage Gaps

## Priority 1: High-Value Missing Coverage

### 1. **Scarcity + Recognition Prioritization** ⭐⭐⭐
**What's missing**: Explicit tests showing MR determines WHO gets satisfied when capacity < total need

```typescript
describe('Recognition-Based Prioritization Under Scarcity', () => {
  it('should prioritize high-MR recipients when capacity is insufficient', () => {
    // Capacity: 50 meals
    // Alice (MR=60%, needs 40) vs Bob (MR=40%, needs 40)
    // Total need: 80 meals > 50 capacity
    // Expected: Alice gets 30 (60%), Bob gets 20 (40%)
    // Both get partial satisfaction proportional to MR
  });
  
  it('should satisfy high-MR recipients fully before low-MR when possible', () => {
    // Capacity: 50 meals
    // Alice (MR=80%, needs 30) vs Bob (MR=20%, needs 60)
    // Expected: Alice gets 30 (full), Bob gets 20 (partial from remaining)
  });
  
  it('should handle zero mutual recognition (Tier 2 only)', () => {
    // All recipients have 0% MR (no mutual recognition)
    // Should fall back to Tier 2 (generous giving based on my recognition)
  });
});
```

### 2. **Organization + Filter Application in Allocation** ⭐⭐⭐
**What's missing**: Testing that `members` field and `SlotFilter` actually affect allocations

```typescript
describe('Organization-Based Allocation Filtering', () => {
  it('should only allocate collective capacity to members', () => {
    // Capacity has members: ['org_coop', 'alice']
    // Recipient 'bob' (not a member) should NOT receive allocation
    // Recipient 'alice' (member) SHOULD receive allocation
  });
  
  it('should apply must_include_ids filter during allocation', () => {
    // Filter: must_include_ids: ['org_community_garden']
    // Slot1: members includes org_community_garden → should allocate
    // Slot2: members doesn't include it → should NOT allocate
  });
  
  it('should resolve organization members recursively during allocation', () => {
    // org_parent contains org_child
    // org_child contains alice and bob
    // Capacity for org_parent should reach alice and bob
  });
  
  it('should handle circular organization references gracefully', () => {
    // org_a contains org_b
    // org_b contains org_a (circular!)
    // Should detect and handle without infinite loop
  });
});
```

### 3. **Location Matching Edge Cases** ⭐⭐
**What's missing**: Tests for location compatibility affecting allocations

```typescript
describe('Location-Based Filtering', () => {
  it('should match online capacity with online needs', () => {
    // Provider offers online tutoring
    // Recipient needs online tutoring
    // Should match regardless of physical location
  });
  
  it('should NOT match physical capacity with incompatible cities', () => {
    // Provider offers in Berlin
    // Recipient needs in Paris
    // Should NOT allocate (unless distance-based matching is enabled)
  });
  
  it('should respect distance-based location matching', () => {
    // Provider: lat/lon in Berlin, max_distance: 50km
    // Recipient1: 20km away → should match
    // Recipient2: 100km away → should NOT match
  });
});
```

### 4. **Time Window Edge Cases** ⭐⭐
**What's missing**: Cross-timezone and recurring pattern tests

```typescript
describe('Advanced Time Window Matching', () => {
  it('should match across timezones when UTC times overlap', () => {
    // Provider: 2pm EST (NYC) = 7pm GMT (London)
    // Recipient: 7pm GMT (London)
    // Should match! (same UTC time)
  });
  
  it('should handle recurring-to-onetime matching correctly', () => {
    // Provider: recurring weekly on Mondays
    // Recipient: one-time need on Monday 2024-03-04
    // Should match!
  });
  
  it('should reject mismatched recurring patterns', () => {
    // Provider: weekly on Mondays
    // Recipient: weekly on Tuesdays
    // Should NOT match
  });
});
```

## Priority 2: Important Edge Cases

### 5. **Empty/Zero/Invalid Values**
```typescript
describe('Edge Cases: Invalid Values', () => {
  it('should handle empty commitments gracefully', () => {
    // Commitment with no slots
  });
  
  it('should handle zero capacity', () => {
    // capacity_slot with quantity: 0
  });
  
  it('should handle all recipients incompatible', () => {
    // Provider has capacity but no recipient matches (time/location/type)
    // Should allocate nothing, no errors
  });
  
  it('should reject negative quantities in slots', () => {
    // Schema validation should prevent this
  });
});
```

### 6. **Multi-Slot Scenarios**
```typescript
describe('Multiple Slots of Same Type', () => {
  it('should aggregate multiple capacity slots of same type', () => {
    // Provider has 2 food slots (50 meals each)
    // Total capacity should be 100
  });
  
  it('should distribute from multiple capacity slots proportionally', () => {
    // Provider has 2 food slots (different times/locations)
    // Recipient matches both
    // Should receive from both (not double allocation!)
  });
  
  it('should handle recipient with multiple need slots', () => {
    // Recipient has 2 food needs (different times)
    // Both should be considered for allocation
  });
});
```

### 7. **Damping Edge Cases**
```typescript
describe('Adaptive Damping Edge Cases', () => {
  it('should detect oscillation pattern (up-down-up)', () => {
    // Round 1: receive 60 (need 50) → +10 over
    // Round 2: receive 30 (need 50) → -20 under
    // Round 3: receive 60 (need 50) → +10 over
    // Should detect oscillation and apply damping
  });
  
  it('should handle persistent over-allocation', () => {
    // Consistently receiving more than needed
    // Should increase damping factor
  });
});
```

## Priority 3: Performance & Stress Tests

### 8. **Scalability**
```typescript
describe('Performance Under Load', () => {
  it('should handle 100 recipients efficiently', () => {
    // Measure allocation time with 100 recipients
  });
  
  it('should use spatial/temporal indexes for O(k) lookup', () => {
    // Verify that filtering uses indexes, not full scan
  });
});
```

## Recommendation

**Start with Priority 1** - these test core allocation logic that directly affects user experience:
1. Recognition prioritization under scarcity (tests the CORE of mutual aid)
2. Organization filtering (tests the new features we just implemented)
3. Location matching (tests a key compatibility requirement)
4. Time window edge cases (we just fixed bugs here!)

Would you like me to implement any of these test suites?

