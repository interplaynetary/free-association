Excellent question! You're highlighting the dynamic nature of capacity allocation where a recipient's available request amount depends on:

1. **Their percentage share** (from `recipient_shares`)
2. **Current capacity quantity** (which can change)
3. **Already consumed/scheduled amount** (from calendar entries)

## Dynamic Request Limits Calculation

### **Current Available Formula**
```
Available to Request = (Current Share Percentage × Current Capacity) - Already Consumed
```

### **Example Scenario**
- **Recipient Share**: 30% (0.3)
- **Current Capacity**: 100 units
- **Theoretical Max**: 30% × 100 = 30 units
- **Already Used/Scheduled**: 20 units
- **Currently Available**: 30 - 20 = 10 units

### **Dynamic Complications**

#### **1. Capacity Quantity Changes**
```
Time T1: 30% × 100 units = 30 max (used 20, available 10)
Time T2: 30% × 120 units = 36 max (used 20, available 16)
Time T3: 30% × 80 units = 24 max (used 20, available 4)
```

#### **2. Share Percentage Changes**
```
Provider updates recipient_shares:
30% → 25% of 100 units = 25 max (used 20, available 5)
30% → 35% of 100 units = 35 max (used 20, available 15)
```

## Implementation Strategy

### **Real-Time Calculation Function**
```typescript
function getAvailableRequestAmount(
  recipientId: string,
  capacity: Capacity,
  calendar: CalendarEntry[]
): number {
  
  // Get current share percentage
  const sharePercentage = capacity.recipient_shares?.[recipientId] || 0;
  
  // Calculate theoretical maximum
  const theoreticalMax = capacity.quantity * sharePercentage;
  
  // Calculate already consumed/scheduled
  const consumed = calendar
    .filter(entry => 
      entry.capacity_id === capacity.id && 
      entry.recipient_id === recipientId &&
      entry.status !== 'cancelled'
    )
    .reduce((sum, entry) => sum + entry.allocated_quantity, 0);
  
  // Available amount (can't be negative)
  return Math.max(0, theoreticalMax - consumed);
}
```

### **Request Validation**
```typescript
function canRequestAmount(
  recipientId: string,
  requestedAmount: number,
  capacity: Capacity,
  calendar: CalendarEntry[]
): { canRequest: boolean; maxAvailable: number; reason?: string } {
  
  const available = getAvailableRequestAmount(recipientId, capacity, calendar);
  
  if (requestedAmount <= available) {
    return { canRequest: true, maxAvailable: available };
  }
  
  return { 
    canRequest: false, 
    maxAvailable: available,
    reason: `Requested ${requestedAmount} but only ${available} available`
  };
}
```

## Handling Dynamic Changes

### **1. Capacity Increase**
```
Provider increases capacity: 100 → 120 units
→ All recipients get proportionally more available
→ 30% × 120 = 36 max (was 30)
→ Available increases: 10 → 16
```

### **2. Capacity Decrease**
```
Provider decreases capacity: 100 → 80 units
→ All recipients get proportionally less
→ 30% × 80 = 24 max (was 30)
→ Available decreases: 10 → 4
→ May need to handle over-allocation (used 20 > max 24)
```

### **3. Share Rebalancing**
```
Provider updates recipient_shares
→ Some recipients gain, others lose
→ Need to handle existing commitments
→ May require renegotiation of scheduled entries
```

## Over-Allocation Handling

### **When Consumed > New Maximum**
```typescript
function handleOverAllocation(
  recipientId: string,
  capacity: Capacity,
  calendar: CalendarEntry[]
): 'allow' | 'renegotiate' | 'cancel_future' {
  
  const newMax = capacity.quantity * (capacity.recipient_shares?.[recipientId] || 0);
  const consumed = getConsumedAmount(recipientId, capacity.id, calendar);
  
  if (consumed > newMax) {
    // Strategy depends on provider's policy
    // 1. Honor existing commitments, block new requests
    // 2. Cancel future scheduled entries
    // 3. Renegotiate with recipient
  }
}
```

## Request Interface Updates

### **Show Available Amount**
```typescript
// When displaying capacity to potential requester
const availableToRequest = getAvailableRequestAmount(userId, capacity, calendar);

// UI shows: "You can request up to 10 units (10 of 30 remaining)"
```

### **Dynamic Request Validation**
```typescript
// Real-time validation as user types request amount
function validateRequestInput(amount: number): ValidationResult {
  const available = getAvailableRequestAmount(userId, capacity, calendar);
  
  if (amount > available) {
    return {
      valid: false,
      message: `Maximum available: ${available} units`,
      suggestion: available
    };
  }
  
  return { valid: true };
}
```

## Schema Implications

### **Track Consumption**
```typescript
// Need to efficiently query consumed amounts
export const ConsumptionSummarySchema = z.object({
  recipient_id: IdSchema,
  capacity_id: IdSchema,
  total_consumed: z.number(),
  currently_scheduled: z.number(),
  last_updated: z.string()
});
```

### **Request Limits Cache**
```typescript
// Cache for performance
export const RequestLimitsSchema = z.object({
  recipient_id: IdSchema,
  capacity_id: IdSchema,
  current_max: z.number(),
  available_to_request: z.number(),
  calculated_at: z.string()
});
```

This approach handles the dynamic nature of capacity allocation while providing clear, real-time feedback to users about what they can actually request given current conditions.
