# Commitment System Migration to Attributes

## 🎯 Vision: Pure Attribute-Based Commitments

Instead of `VersionedStore<Commitment>`, use the attribute system with decomposed commitment fields.

## Architecture Comparison

### Current (VersionedStore):
```typescript
networkCommitments: VersionedStore<Commitment, string>
// Entity-level ITC + field versions
// Fields: recognition, needs, capacity, damping, allocations

subscribeToCommitment(pubkey) // Subscribe to entire commitment
```

### Proposed (Attribute System):
```typescript
myAttributeRecognitions: AttributeRecognitionsCollection
// Attribute-level ITC per field
// Same granularity, more flexible!

// Decomposed commitment fields:
"commitment:recognition"   → global_recognition_weights
"commitment:needs"         → need_slots[]
"commitment:capacity"      → capacity_slots[]
"commitment:damping"       → multi_dimensional_damping
"commitment:allocations"   → slot_allocations[]
"commitment:others_cache"  → others_recognition_of_me
```

## Migration Strategy

### 1. Attribute Type Definitions

Add commitment field types to `attribute-types.ts`:

```typescript
/**
 * Detect commitment field from attribute name
 */
export function isCommitmentField(attribute_name: string): boolean {
  return attribute_name.startsWith('commitment:');
}

/**
 * Extract commitment field name
 * "commitment:recognition" → "recognition"
 */
export function extractCommitmentField(attribute_name: string): string | undefined {
  if (attribute_name.startsWith('commitment:')) {
    return attribute_name.substring('commitment:'.length);
  }
  return undefined;
}

/**
 * Commitment field equality checkers
 */
export function getCommitmentFieldEqualityChecker(fieldName: string): 
  ((a: any, b: any) => boolean) | undefined {
  
  switch (fieldName) {
    case 'recognition':
    case 'others_cache':
      // Use default deepEquals for objects
      return undefined;
    
    case 'needs':
    case 'capacity':
    case 'allocations':
      // Use jsonEquals for arrays of complex objects
      return jsonEquals;
    
    case 'damping':
      // Use jsonEquals for damping state
      return jsonEquals;
    
    default:
      return undefined;
  }
}
```

Update `getEqualityChecker()`:

```typescript
export function getEqualityChecker(attribute_name: string): 
  ((a: any, b: any) => boolean) | undefined {
  
  const type = detectAttributeType(attribute_name);
  
  switch (type) {
    case 'membership':
      return membershipEquals;
    
    case 'capacity':
    case 'need':
      return slotArrayEquals;
    
    default:
      // Check if it's a commitment field
      const commitmentField = extractCommitmentField(attribute_name);
      if (commitmentField) {
        return getCommitmentFieldEqualityChecker(commitmentField);
      }
      
      return undefined; // Use default deepEquals
  }
}
```

### 2. Commitment Composition Helpers

Add to `attribute-recognition.svelte.ts`:

```typescript
/**
 * Set commitment field as attribute
 * 
 * Helper to update a specific commitment field for an entity.
 * Preserves atomicity at the field level while allowing independent updates.
 */
export function setCommitmentField(
  entity_id: string,
  fieldName: string,
  value: any,
  source_pubkey?: string
): void {
  const collection = get(myAttributeRecognitions) || { _timestamp: Date.now() };
  const attribute_name = `commitment:${fieldName}`;
  
  // Get existing attribute for ITC increment
  const existing = getAttributeFromCollection(collection, entity_id, attribute_name);
  const existingITC = existing?.itcStamp;
  
  // Get custom equality checker
  const equalityChecker = getEqualityChecker(attribute_name);
  
  // Update with change detection
  const updated = updateAttributeInCollection(
    collection,
    entity_id,
    attribute_name,
    value,
    source_pubkey,
    1.0, // confidence
    existingITC,
    equalityChecker
  );
  
  myAttributeRecognitions.set(updated);
}

/**
 * Get commitment field from attributes
 */
export function getCommitmentField<T = any>(
  entity_id: string,
  fieldName: string
): T | undefined {
  const attribute_name = `commitment:${fieldName}`;
  return getAttribute(entity_id, attribute_name);
}

/**
 * Get full commitment from attributes
 * 
 * Reconstructs a Commitment object from individual attribute fields.
 */
export function getCommitmentFromAttributes(entity_id: string): Commitment | undefined {
  const recognition = getCommitmentField<GlobalRecognitionWeights>(entity_id, 'recognition');
  const needs = getCommitmentField<NeedSlot[]>(entity_id, 'needs');
  const capacity = getCommitmentField<AvailabilitySlot[]>(entity_id, 'capacity');
  const damping = getCommitmentField<any>(entity_id, 'damping');
  const allocations = getCommitmentField<SlotAllocationRecord[]>(entity_id, 'allocations');
  const othersCache = getCommitmentField<Record<string, GlobalRecognitionWeights>>(entity_id, 'others_cache');
  
  // Need at least recognition or needs/capacity to have a valid commitment
  if (!recognition && !needs && !capacity) {
    return undefined;
  }
  
  // Get most recent ITC/timestamp from all fields
  const collection = get(myAttributeRecognitions) || { _timestamp: Date.now() };
  const entityAttrs = collection[entity_id];
  
  let latestITC: ITCStamp | undefined;
  let latestTimestamp = 0;
  
  if (entityAttrs && typeof entityAttrs === 'object') {
    for (const [attr_name, attr_value] of Object.entries(entityAttrs)) {
      if (attr_name.startsWith('commitment:') && typeof attr_value === 'object' && attr_value !== null) {
        const av = attr_value as AttributeValue;
        if (av.timestamp > latestTimestamp) {
          latestTimestamp = av.timestamp;
          latestITC = av.itcStamp;
        }
      }
    }
  }
  
  return {
    global_recognition_weights: recognition || {},
    need_slots: needs || [],
    capacity_slots: capacity || [],
    multi_dimensional_damping: damping,
    slot_allocations: allocations,
    others_recognition_of_me: othersCache,
    itcStamp: latestITC || itcSeed(),
    timestamp: latestTimestamp || Date.now()
  };
}

/**
 * Set full commitment as attributes
 * 
 * Decomposes a Commitment object into individual attribute fields.
 */
export function setCommitmentAsAttributes(
  entity_id: string,
  commitment: Commitment,
  source_pubkey?: string
): void {
  // Set each field as a separate attribute
  setCommitmentField(entity_id, 'recognition', commitment.global_recognition_weights, source_pubkey);
  setCommitmentField(entity_id, 'needs', commitment.need_slots, source_pubkey);
  setCommitmentField(entity_id, 'capacity', commitment.capacity_slots, source_pubkey);
  
  if (commitment.multi_dimensional_damping) {
    setCommitmentField(entity_id, 'damping', commitment.multi_dimensional_damping, source_pubkey);
  }
  
  if (commitment.slot_allocations) {
    setCommitmentField(entity_id, 'allocations', commitment.slot_allocations, source_pubkey);
  }
  
  if (commitment.others_recognition_of_me) {
    setCommitmentField(entity_id, 'others_cache', commitment.others_recognition_of_me, source_pubkey);
  }
}
```

### 3. Fine-Grained Derived Stores (Commitment Fields)

Replace `VersionedStore` field stores with attribute-based ones:

```typescript
/**
 * Network Recognition Weights - ATTRIBUTE FIELD STORE
 * 
 * ✅ Only updates when "commitment:recognition" changes
 * ✅ Same reactivity as VersionedStore's deriveField('recognition')
 */
export const networkRecognitionWeights: Readable<Map<string, GlobalRecognitionWeights>> = 
  deriveAttribute('commitment:recognition');

/**
 * Network Need Slots - ATTRIBUTE FIELD STORE
 * 
 * ✅ Only updates when "commitment:needs" changes
 */
export const networkNeedSlots: Readable<Map<string, NeedSlot[]>> = 
  deriveAttribute('commitment:needs');

/**
 * Network Capacity Slots - ATTRIBUTE FIELD STORE
 * 
 * ✅ Only updates when "commitment:capacity" changes
 */
export const networkCapacitySlots: Readable<Map<string, AvailabilitySlot[]>> = 
  deriveAttribute('commitment:capacity');

/**
 * Network Allocations - ATTRIBUTE FIELD STORE
 * 
 * ✅ Only updates when "commitment:allocations" changes
 */
export const networkAllocations: Readable<Map<string, SlotAllocationRecord[]>> = 
  deriveAttribute('commitment:allocations');
```

### 4. Subscription Management

Replace `subscribeToCommitment()` with attribute-based subscription:

```typescript
/**
 * Subscribe to a participant's commitment (via attributes)
 * 
 * Subscribes to all commitment fields from this pubkey.
 */
export function subscribeToCommitment(pubkey: string) {
  if (activeSubscriptions.has(`${pubkey}:commitment`)) return;
  
  // Subscribe to their attribute recognitions
  // The subscription handler will process all "commitment:*" attributes
  subscribeToAttributeRecognitions(pubkey);
  
  activeSubscriptions.add(`${pubkey}:commitment`);
  console.log(`[📡 COMMITMENT-SUB] ✅ Subscribed to ${pubkey.slice(0, 20)}... commitment`);
}
```

The existing `subscribeToAttributeRecognitions()` already handles:
- ITC causality checking ✅
- Change detection via custom equality checkers ✅
- Writing to unified storage ✅

No changes needed! It already works for commitment fields!

### 5. Migrate Stores

**Before (VersionedStore):**
```typescript
export const networkCommitments: VersionedStore<Commitment, string> = createVersionedStore({
  fields: {
    recognition: (c) => c.global_recognition_weights,
    needs: (c) => c.need_slots,
    capacity: (c) => c.capacity_slots,
    damping: (c) => c.multi_dimensional_damping,
    allocations: (c) => c.slot_allocations
  },
  // ... field equality checkers, schema, ITC extractor
});
```

**After (Attribute System):**
```typescript
// Commitments are now just attributes in myAttributeRecognitions!
// No separate store needed!

// Derived stores for fine-grained reactivity:
export const networkRecognitionWeights = deriveAttribute('commitment:recognition');
export const networkNeedSlots = deriveAttribute('commitment:needs');
export const networkCapacitySlots = deriveAttribute('commitment:capacity');
export const networkAllocations = deriveAttribute('commitment:allocations');
```

### 6. Backward Compatibility Helpers

```typescript
/**
 * Get all commitments as Record (for allocation.ts compatibility)
 */
export function getAllCommitmentsRecord(): Record<string, Commitment> {
  const record: Record<string, Commitment> = {};
  const collection = get(myAttributeRecognitions) || { _timestamp: Date.now() };
  
  // Find all entities with commitment fields
  const entitiesWithCommitments = new Set<string>();
  
  for (const [entity_id, entityAttrs] of Object.entries(collection)) {
    if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;
    
    if (typeof entityAttrs === 'object' && entityAttrs !== null) {
      for (const attr_name of Object.keys(entityAttrs)) {
        if (attr_name.startsWith('commitment:')) {
          entitiesWithCommitments.add(entity_id);
          break;
        }
      }
    }
  }
  
  // Reconstruct commitments
  for (const entity_id of entitiesWithCommitments) {
    const commitment = getCommitmentFromAttributes(entity_id);
    if (commitment) {
      record[entity_id] = commitment;
    }
  }
  
  return record;
}
```

## 📊 Comparison: VersionedStore vs. Attributes

| Feature | VersionedStore | Attribute System (Decomposed) |
|---------|---------------|-------------------------------|
| **ITC Granularity** | Entity-level ITC + field versions | Attribute-level ITC (per field) |
| **Fine-Grained Reactivity** | ✅ `deriveField('recognition')` | ✅ `deriveAttribute('commitment:recognition')` |
| **Change Detection** | Field versions + deep equality | ITC + deep equality |
| **Schema Flexibility** | Fixed `Commitment` schema | Dynamic - any fields |
| **Storage Structure** | `Map<EntityKey, VersionedEntity<Commitment>>` | `Record<entity_id, Record<attr_name, AttributeValue>>` |
| **Source Tracking** | Manual | ✅ Built-in (`source_pubkey`) |
| **Confidence** | Manual | ✅ Built-in (`confidence`) |
| **Atomicity** | Commitment-level | Field-level (can batch) |

## 🎯 Key Advantages

1. **Unified Architecture**: Everything is an attribute!
   - Contacts: attributes
   - Organizations: attributes
   - Commitments: attributes
   - Recognition: attributes

2. **Same Reactivity**: Fine-grained derived stores work identically
   - `networkCapacityIndex` subscribes to `deriveAttribute('commitment:capacity')`
   - Only updates when capacity changes, not recognition!

3. **More Flexible**: Can add new commitment fields without schema changes
   - Want to track `commitment:quality_score`? Just add it!
   - No need to modify `Commitment` type or versioned store config

4. **Simpler**: One storage system instead of two
   - No `VersionedStore` + `AttributeRecognitionsCollection`
   - Just `AttributeRecognitionsCollection` for everything!

## 🔄 Subscription Management

### Current Approach (stores.svelte.ts):

```typescript
/**
 * Auto-subscribe based on recognition tree
 */
export function syncSubscriptionsWithTree() {
  const contributors = getMyContributors(); // From tree
  
  for (const contributor of contributors) {
    subscribeToCommitment(contributor); // Subscribe to their commitment
  }
}
```

**Logic:** If someone is in my recognition tree → subscribe to their commitment

### Proposed Unified Subscription System:

```typescript
/**
 * Subscription Policy:
 * 
 * 1. Recognition tree contributors → subscribe to "commitment:*"
 * 2. Organization members → subscribe to "membership", "commitment:*"  
 * 3. Manual subscriptions → subscribe to specific attributes
 */
export interface SubscriptionPolicy {
  /** Who to subscribe to */
  entity_id: string;
  
  /** Which attributes to subscribe from them */
  attributes: string[] | '*'; // '*' = all attributes
  
  /** Reason for subscription (for debugging) */
  reason: 'tree_contributor' | 'org_member' | 'manual' | 'capacity_provider' | 'need_recipient';
}

/**
 * Compute subscription policies from current state
 */
export function computeSubscriptionPolicies(): SubscriptionPolicy[] {
  const policies: SubscriptionPolicy[] = [];
  
  // 1. Subscribe to tree contributors (for mutual recognition)
  const contributors = getMyContributors();
  for (const contributor of contributors) {
    policies.push({
      entity_id: contributor,
      attributes: ['commitment:*'], // All commitment fields
      reason: 'tree_contributor'
    });
  }
  
  // 2. Subscribe to org members (for coordination)
  const orgMemberships = getEntitiesWithAttribute(
    get(myAttributeRecognitions) || {},
    'membership'
  );
  for (const org_id of orgMemberships) {
    const members = getAttribute(org_id, 'membership') as string[] | undefined;
    if (members) {
      for (const member_id of members) {
        policies.push({
          entity_id: member_id,
          attributes: ['commitment:*', 'capacity:*', 'need:*'],
          reason: 'org_member'
        });
      }
    }
  }
  
  // 3. Manual subscription configs
  const manualSubs = get(myAttributeSubscriptions) || {};
  for (const [entity_id, attrSubs] of Object.entries(manualSubs)) {
    for (const attr_name of Object.keys(attrSubs)) {
      policies.push({
        entity_id,
        attributes: [attr_name],
        reason: 'manual'
      });
    }
  }
  
  // Deduplicate and merge
  return mergePolicies(policies);
}

/**
 * Apply subscription policies
 */
export function applySubscriptionPolicies(policies: SubscriptionPolicy[]) {
  for (const policy of policies) {
    // Resolve entity_id to pubkey if needed
    const pubkey = resolveToPublicKey(policy.entity_id) || policy.entity_id;
    
    // Subscribe to their attribute recognitions
    // This gives us ALL their attributes - we filter what we use
    subscribeToAttributeRecognitions(pubkey);
    
    console.log(`[SUB-POLICY] Subscribed to ${pubkey.slice(0, 20)}... for ${policy.reason}`);
  }
}

/**
 * Enable automatic subscription management
 * 
 * Watches for changes in:
 * - Recognition tree
 * - Organization memberships
 * - Manual subscription configs
 * 
 * Automatically subscribes/unsubscribes as policies change.
 */
export function enableAutoSubscriptionManagement(): () => void {
  console.log('[AUTO-SUB] 🔄 Enabling intelligent subscription management');
  
  let currentPolicies: SubscriptionPolicy[] = [];
  
  const recomputeAndApply = () => {
    const newPolicies = computeSubscriptionPolicies();
    
    // TODO: Compare and only change what's different
    // For now, just apply all
    applySubscriptionPolicies(newPolicies);
    
    currentPolicies = newPolicies;
  };
  
  // Watch relevant stores
  const unsubTree = myRecognitionTreeStore.subscribe(() => {
    console.log('[AUTO-SUB] Tree changed, recomputing subscriptions...');
    recomputeAndApply();
  });
  
  const unsubAttrs = myAttributeRecognitions.subscribe(() => {
    // Check if org memberships changed
    // TODO: More efficient - only recompute when membership attrs change
    recomputeAndApply();
  });
  
  const unsubManual = myAttributeSubscriptions.subscribe(() => {
    console.log('[AUTO-SUB] Manual subscriptions changed, recomputing...');
    recomputeAndApply();
  });
  
  // Initial
  recomputeAndApply();
  
  return () => {
    unsubTree();
    unsubAttrs();
    unsubManual();
    console.log('[AUTO-SUB] ⏸️  Disabled subscription management');
  };
}
```

## 🚀 Migration Path

### Phase 1: Add Commitment Support to Attribute System
1. ✅ Add commitment field type detection (`attribute-types.ts`)
2. ✅ Add custom equality checkers for commitment fields
3. ✅ Add helpers: `setCommitmentField()`, `getCommitmentFromAttributes()`

### Phase 2: Create Attribute-Based Derived Stores
1. ✅ Create `networkRecognitionWeights = deriveAttribute('commitment:recognition')`
2. ✅ Create `networkNeedSlots = deriveAttribute('commitment:needs')`
3. ✅ Create `networkCapacitySlots = deriveAttribute('commitment:capacity')`
4. ✅ Test that spatial/temporal indexes update correctly

### Phase 3: Migrate Subscription Logic
1. ✅ Update `subscribeToCommitment()` to use `subscribeToAttributeRecognitions()`
2. ✅ Update `syncSubscriptionsWithTree()` to work with attributes
3. ✅ Add unified subscription policy system

### Phase 4: Migrate My Commitment Store
1. Replace `myCommitmentStore` with attribute-based storage
2. Update `setMyNeedSlots()` / `setMyCapacitySlots()` to use `setCommitmentField()`
3. Update `composeCommitmentFromSources()` to use `setCommitmentAsAttributes()`

### Phase 5: Remove VersionedStore
1. Delete `networkCommitments` VersionedStore
2. Update `getAllCommitmentsRecord()` to use `getCommitmentFromAttributes()`
3. Delete VersionedStore-specific code

### Phase 6: Test & Verify
1. Test allocation algorithm with attribute-based commitments
2. Test fine-grained reactivity (capacity change doesn't rebuild recognition)
3. Test ITC causality and conflict resolution
4. Performance benchmarks

## ✨ End State: Pure Attribute Architecture

**Everything is an attribute:**
- Contact info: `name`, `email`, `public_key`
- Organization: `membership`, `founding_date`
- Commitments: `commitment:recognition`, `commitment:needs`, `commitment:capacity`
- Capacities: `capacity:food`, `capacity:tutoring`
- Needs: `need:food`, `need:housing`

**One storage system:**
- `myAttributeRecognitions` - my recognitions + subscribed data
- ITC causality for all updates
- Fine-grained derived stores for reactivity
- Flexible schema for evolution

**Intelligent subscriptions:**
- Auto-subscribe to tree contributors
- Auto-subscribe to org members
- Manual subscriptions for special cases
- Policy-based management

**Elegant, pure, unified!** 🎉

