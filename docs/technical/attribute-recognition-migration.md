# Entity Attribute Recognition - Migration Guide

## Migration Strategy

This document outlines the step-by-step migration from the current specialized systems (membership, capacity subscriptions) to the unified Entity-Attribute-Recognition framework.

## Phase 1: Foundation (Week 1)

### 1.1 Add Schemas

**File**: `src/lib/protocol/schemas.ts`

**Changes**: Add to the end of the file, before exports:

```typescript
// ═══════════════════════════════════════════════════════════════════
// ENTITY ATTRIBUTE RECOGNITION SCHEMAS (New Unified System)
// ═══════════════════════════════════════════════════════════════════

// ... (Copy from attribute-recognition-implementation.md)
```

### 1.2 Create Core Module

**New File**: `src/lib/network/attributes.ts`

```typescript
// Pure functions (no Svelte dependencies)
// ... (Copy from attribute-recognition-implementation.md)
```

**New File**: `src/lib/network/attributes.svelte.ts`

```typescript
// Svelte store integration
// ... (Copy from attribute-recognition-implementation.md)
```

### 1.3 Initialize in App

**File**: `src/routes/+layout.ts` or main initialization

```typescript
import { initializeAttributes } from '$lib/network/attributes.svelte';

// In your initialization code
export async function load() {
  // ... existing code ...
  
  // Initialize unified attributes
  initializeAttributes();
  
  // ... rest of initialization ...
}
```

**Test**: Verify that stores initialize without errors.

## Phase 2: Backward-Compatible Adapters (Week 2)

### 2.1 Adapt Membership System

**File**: `src/lib/network/membership.svelte.ts`

**Strategy**: Make existing functions delegate to the new unified system.

```typescript
/**
 * Membership Module - ADAPTER to Unified Attributes
 * 
 * This module now delegates to the unified attribute system
 * while maintaining backward compatibility with existing APIs.
 */

import {
  getEntityAttribute,
  setEntityAttribute,
  removeEntityAttribute,
  subscribeToAttribute,
  unsubscribeFromAttribute,
  getAttributeSource,
  attributeCache
} from './attributes.svelte';

// Re-export for external use
export {
  initializeMembership,  // delegates to initializeAttributes
  cleanupMembership      // delegates to cleanupAttributes
} from './attributes.svelte';

// ═══════════════════════════════════════════════════════════════════
// BACKWARD-COMPATIBLE API
// ═══════════════════════════════════════════════════════════════════

/**
 * Set membership list for an organization
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function setMembershipList(org_id: string, members: string[]): void {
  setEntityAttribute(org_id, 'membership', members);
  console.log(`[MEMBERSHIP-ADAPTER] Set membership list for ${org_id}: ${members.length} members`);
}

/**
 * Remove declared membership list for an organization
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function removeMembershipList(org_id: string): void {
  removeEntityAttribute(org_id, 'membership');
  console.log(`[MEMBERSHIP-ADAPTER] Removed membership list for ${org_id}`);
}

/**
 * Add member to an organization's membership list
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function addMemberToList(org_id: string, member_id: string): void {
  const currentMembers = getMembershipList(org_id) || [];
  
  // Avoid duplicates
  if (currentMembers.includes(member_id)) {
    console.log(`[MEMBERSHIP-ADAPTER] Member ${member_id} already in ${org_id}`);
    return;
  }
  
  const updatedMembers = [...currentMembers, member_id];
  setEntityAttribute(org_id, 'membership', updatedMembers);
  
  console.log(`[MEMBERSHIP-ADAPTER] Added member ${member_id} to ${org_id}`);
}

/**
 * Remove member from an organization's membership list
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function removeMemberFromList(org_id: string, member_id: string): void {
  const currentMembers = getMembershipList(org_id);
  if (!currentMembers) return;
  
  const wasPresent = currentMembers.includes(member_id);
  if (!wasPresent) return;
  
  const updatedMembers = currentMembers.filter(id => id !== member_id);
  setEntityAttribute(org_id, 'membership', updatedMembers);
  
  console.log(`[MEMBERSHIP-ADAPTER] Removed member ${member_id} from ${org_id}`);
}

/**
 * Subscribe to someone else's membership list for an organization
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function subscribeMembershipList(org_id: string, source_pubkey: string): void {
  subscribeToAttribute(org_id, 'membership', source_pubkey);
  console.log(`[MEMBERSHIP-ADAPTER] Subscribed to ${source_pubkey.slice(0, 20)}...'s list for ${org_id}`);
}

/**
 * Unsubscribe from a membership list
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function unsubscribeMembershipList(org_id: string): void {
  unsubscribeFromAttribute(org_id, 'membership');
  console.log(`[MEMBERSHIP-ADAPTER] Unsubscribed from membership list for ${org_id}`);
}

/**
 * Get membership list for an organization (cached or declared)
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function getMembershipList(org_id: string): string[] | undefined {
  return getEntityAttribute(org_id, 'membership') as string[] | undefined;
}

/**
 * Check if we have membership data for an organization
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function hasMembershipList(org_id: string): boolean {
  return getMembershipList(org_id) !== undefined;
}

/**
 * Get source of membership data for an organization
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function getMembershipSource(org_id: string): 'declared' | string | undefined {
  const source = getAttributeSource(org_id, 'membership');
  // Map 'self' to undefined for backward compatibility
  return source === 'self' ? undefined : source;
}

// ═══════════════════════════════════════════════════════════════════
// DEPRECATED STORES (For gradual migration)
// ═══════════════════════════════════════════════════════════════════

import { derived } from 'svelte/store';
import { myAttributeDeclarations, attributeSubscriptions } from './attributes.svelte';

/**
 * @deprecated Use myAttributeDeclarations from attributes.svelte.ts instead
 * 
 * Provided for backward compatibility during migration.
 * This is a derived store that extracts only membership data.
 */
export const myMembershipLists = derived(
  myAttributeDeclarations,
  ($declarations) => {
    const membershipOnly: Record<string, string[]> = {};
    
    if (!$declarations) return membershipOnly;
    
    for (const [entity_id, attrs] of Object.entries($declarations)) {
      if (attrs.membership && Array.isArray(attrs.membership)) {
        membershipOnly[entity_id] = attrs.membership;
      }
    }
    
    return membershipOnly;
  }
);

/**
 * @deprecated Use attributeSubscriptions from attributes.svelte.ts instead
 * 
 * Provided for backward compatibility during migration.
 * This is a derived store that extracts only membership subscriptions.
 */
export const myMembershipSubscriptions = derived(
  attributeSubscriptions,
  ($subscriptions) => {
    const membershipOnly: Record<string, string> = {};
    
    if (!$subscriptions) return membershipOnly;
    
    for (const [entity_id, subs] of Object.entries($subscriptions)) {
      if (subs.membership && typeof subs.membership === 'string') {
        membershipOnly[entity_id] = subs.membership;
      }
    }
    
    return membershipOnly;
  }
);

/**
 * @deprecated Use attributeCache from attributes.svelte.ts instead
 * 
 * Provided for backward compatibility during migration.
 * This is a derived store that extracts only membership cache.
 */
export const membershipCache = derived(
  attributeCache,
  ($cache) => {
    const membershipOnly: Record<string, Record<string, string[]>> = {};
    
    if (!$cache) return membershipOnly;
    
    for (const [source, sourceData] of Object.entries($cache)) {
      membershipOnly[source] = {};
      
      for (const [entity_id, attrs] of Object.entries(sourceData)) {
        if (attrs.membership && Array.isArray(attrs.membership)) {
          membershipOnly[source][entity_id] = attrs.membership;
        }
      }
    }
    
    return membershipOnly;
  }
);
```

### 2.2 Adapt Capacity Subscription System

**File**: `src/lib/network/capacity-subscriptions.svelte.ts`

**Strategy**: Similar adapter pattern for capacity and need slots.

```typescript
/**
 * Capacity & Need Subscriptions - ADAPTER to Unified Attributes
 * 
 * This module now delegates to the unified attribute system
 * while maintaining backward compatibility with existing APIs.
 */

import {
  getEntityAttribute,
  setEntityAttribute,
  subscribeToAttribute,
  unsubscribeFromAttribute,
  attributeSubscriptions,
  attributeCache
} from './attributes.svelte';

import { derived } from 'svelte/store';
import type { Writable } from 'svelte/store';
import type { AvailabilitySlot, NeedSlot } from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// BACKWARD-COMPATIBLE API
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to a user's capacity slots
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function subscribeToCapacity(pubkey: string): void {
  subscribeToAttribute(pubkey, 'capacity', null);  // null = subscribe to entity's own
  console.log(`[CAPACITY-ADAPTER] Subscribed to capacity of ${pubkey.slice(0, 20)}...`);
}

/**
 * Subscribe to a user's need slots
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function subscribeToNeeds(pubkey: string): void {
  subscribeToAttribute(pubkey, 'needs', null);  // null = subscribe to entity's own
  console.log(`[CAPACITY-ADAPTER] Subscribed to needs of ${pubkey.slice(0, 20)}...`);
}

/**
 * Subscribe to a user's slots (capacity, needs, or both) - UNIFIED!
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function subscribeToSlots(
  pubkey: string,
  types: { capacity?: boolean; needs?: boolean } = { capacity: true, needs: true }
): void {
  if (types.capacity) {
    subscribeToCapacity(pubkey);
  }
  if (types.needs) {
    subscribeToNeeds(pubkey);
  }
}

/**
 * Unsubscribe from a user's slots completely - UNIFIED!
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function unsubscribeFromSlots(pubkey: string): void {
  unsubscribeFromAttribute(pubkey, 'capacity');
  unsubscribeFromAttribute(pubkey, 'needs');
  console.log(`[CAPACITY-ADAPTER] Unsubscribed from ${pubkey.slice(0, 20)}...`);
}

/**
 * Get cached capacities for a user
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function getUserCapacities(pubkey: string): AvailabilitySlot[] | undefined {
  return getEntityAttribute(pubkey, 'capacity') as AvailabilitySlot[] | undefined;
}

/**
 * Get cached needs for a user
 * 
 * MIGRATION: Delegates to unified attribute system
 */
export function getUserNeeds(pubkey: string): NeedSlot[] | undefined {
  return getEntityAttribute(pubkey, 'needs') as NeedSlot[] | undefined;
}

// ═══════════════════════════════════════════════════════════════════
// DEPRECATED STORES (For gradual migration)
// ═══════════════════════════════════════════════════════════════════

/**
 * @deprecated Use attributeSubscriptions from attributes.svelte.ts instead
 * 
 * Backward-compatible derived store for slot subscriptions.
 */
export const slotSubscriptions = derived(
  attributeSubscriptions,
  ($subscriptions) => {
    const slotSubs: Record<string, { capacity: boolean; needs: boolean }> = {};
    
    if (!$subscriptions) return slotSubs;
    
    // Aggregate all pubkeys that have capacity or needs subscriptions
    const pubkeys = new Set<string>();
    for (const [entity_id, subs] of Object.entries($subscriptions)) {
      if (subs.capacity !== undefined || subs.needs !== undefined) {
        // Assuming entity_id is a pubkey for slot subscriptions
        pubkeys.add(entity_id);
      }
    }
    
    for (const pubkey of pubkeys) {
      slotSubs[pubkey] = {
        capacity: $subscriptions[pubkey]?.capacity !== undefined,
        needs: $subscriptions[pubkey]?.needs !== undefined
      };
    }
    
    return slotSubs;
  }
);

/**
 * @deprecated Use attributeCache from attributes.svelte.ts instead
 * 
 * Backward-compatible derived store for capacity cache.
 */
export const capacityCache = derived(
  attributeCache,
  ($cache) => {
    const capacities: Record<string, AvailabilitySlot[]> = {};
    
    if (!$cache) return capacities;
    
    for (const [source, sourceData] of Object.entries($cache)) {
      // For self-published capacity: cache[pubkey][pubkey][capacity]
      if (sourceData[source]?.capacity && Array.isArray(sourceData[source].capacity)) {
        capacities[source] = sourceData[source].capacity as AvailabilitySlot[];
      }
    }
    
    return capacities;
  }
);

/**
 * @deprecated Use attributeCache from attributes.svelte.ts instead
 * 
 * Backward-compatible derived store for need cache.
 */
export const needCache = derived(
  attributeCache,
  ($cache) => {
    const needs: Record<string, NeedSlot[]> = {};
    
    if (!$cache) return needs;
    
    for (const [source, sourceData] of Object.entries($cache)) {
      // For self-published needs: cache[pubkey][pubkey][needs]
      if (sourceData[source]?.needs && Array.isArray(sourceData[source].needs)) {
        needs[source] = sourceData[source].needs as NeedSlot[];
      }
    }
    
    return needs;
  }
);
```

**Test**: Verify that all existing components still work with adapted APIs.

## Phase 3: Migrate Key Consumers (Week 3-4)

### 3.1 Update Components One by One

**Strategy**: Update components to use unified API directly (not through adapters).

**Example**: Organization membership component

**Before**:
```typescript
import { 
  getMembershipList,
  setMembershipList,
  subscribeMembershipList
} from '$lib/network/membership.svelte';

const members = getMembershipList('org_redcross');
```

**After**:
```typescript
import {
  getEntityAttribute,
  setEntityAttribute,
  subscribeToAttribute
} from '$lib/network/attributes.svelte';

const members = getEntityAttribute('org_redcross', 'membership') as string[] | undefined;
```

**Recommended Approach**: Create type-safe helper functions:

```typescript
// src/lib/network/attributes-helpers.ts

import type { AvailabilitySlot, NeedSlot } from '$lib/protocol/schemas';
import { getEntityAttribute, setEntityAttribute, subscribeToAttribute } from './attributes.svelte';

// Membership helpers
export function getMembership(org_id: string): string[] | undefined {
  return getEntityAttribute(org_id, 'membership') as string[] | undefined;
}

export function setMembership(org_id: string, members: string[]): void {
  setEntityAttribute(org_id, 'membership', members);
}

export function subscribeToMembership(org_id: string, source_pubkey: string): void {
  subscribeToAttribute(org_id, 'membership', source_pubkey);
}

// Capacity helpers
export function getCapacity(entity_id: string): AvailabilitySlot[] | undefined {
  return getEntityAttribute(entity_id, 'capacity') as AvailabilitySlot[] | undefined;
}

export function setCapacity(entity_id: string, slots: AvailabilitySlot[]): void {
  setEntityAttribute(entity_id, 'capacity', slots);
}

// Need helpers
export function getNeeds(entity_id: string): NeedSlot[] | undefined {
  return getEntityAttribute(entity_id, 'needs') as NeedSlot[] | undefined;
}

export function setNeeds(entity_id: string, slots: NeedSlot[]): void {
  setEntityAttribute(entity_id, 'needs', slots);
}

// Skills helpers (NEW!)
export function getSkills(entity_id: string): string[] | undefined {
  return getEntityAttribute(entity_id, 'skills') as string[] | undefined;
}

export function setSkills(entity_id: string, skills: string[]): void {
  setEntityAttribute(entity_id, 'skills', skills);
}

// Location helpers (NEW!)
export interface Location {
  lat: number;
  lng: number;
  address?: string;
}

export function getLocation(entity_id: string): Location | undefined {
  return getEntityAttribute(entity_id, 'location') as Location | undefined;
}

export function setLocation(entity_id: string, location: Location): void {
  setEntityAttribute(entity_id, 'location', location);
}
```

### 3.2 Component Migration Checklist

For each component that uses membership or capacity subscriptions:

1. ✅ Identify all imports from `membership.svelte` or `capacity-subscriptions.svelte`
2. ✅ Replace with imports from `attributes.svelte` or `attributes-helpers`
3. ✅ Update function calls to use new API
4. ✅ Test component functionality
5. ✅ Verify Holster persistence
6. ✅ Check network sync

**Example Components to Migrate**:
- Organization detail pages
- Capacity/need management forms
- Allocation algorithms (read-only access)
- Recognition trees (contributor resolution)

## Phase 4: Add New Attribute Types (Week 5)

Once migration is complete, demonstrate extensibility by adding new attributes:

### 4.1 Skills Recognition

```typescript
// In a component or service
import { setEntityAttribute, getEntityAttribute, subscribeToAttribute } from '$lib/network/attributes.svelte';

// Declare my skills
setEntityAttribute(myPubkey, 'skills', [
  'emergency_medicine',
  'trauma_care',
  'disaster_response',
  'team_coordination'
]);

// Recognize Alice's skills (my perspective)
setEntityAttribute('alice_pub', 'skills', [
  'software_engineering',
  'system_design',
  'mentoring'
]);

// Subscribe to Bob's recognition of Carol's skills
subscribeToAttribute('carol_pub', 'skills', 'bob_pub');

// Query skills
const mySkills = getEntityAttribute(myPubkey, 'skills');
const aliceSkills = getEntityAttribute('alice_pub', 'skills');
```

### 4.2 Location Recognition

```typescript
// Organization locations
setEntityAttribute('org_redcross', 'location', {
  lat: 40.7128,
  lng: -74.0060,
  address: '431 18th St NW, Washington, DC 20006'
});

// User locations (for matching)
setEntityAttribute(myPubkey, 'location', {
  lat: 37.7749,
  lng: -122.4194,
  address: 'San Francisco, CA'
});

// Query
const orgLocation = getEntityAttribute('org_redcross', 'location');
```

### 4.3 Reputation/Ratings

```typescript
// Multi-dimensional reputation
setEntityAttribute('alice_pub', 'reputation', {
  reliability: 0.95,
  communication: 0.88,
  technical_skill: 0.92,
  teamwork: 0.90
});

// Query
const aliceReputation = getEntityAttribute('alice_pub', 'reputation');
```

## Phase 5: Deprecate Old System (Week 6)

### 5.1 Mark as Deprecated

**File**: `src/lib/network/membership.svelte.ts`

Add deprecation notices:

```typescript
/**
 * @deprecated This module is deprecated in favor of the unified attributes system.
 * Use `src/lib/network/attributes.svelte.ts` instead.
 * 
 * This adapter will be removed in v6.0.
 * 
 * Migration guide: /docs/technical/attribute-recognition-migration.md
 */
```

### 5.2 Remove Pure Function Duplicates

Delete `src/lib/network/membership.ts` (pure functions) as they're now superseded by `attributes.ts`.

### 5.3 Final Cleanup

Once all consumers migrated:

1. Remove adapter functions from `membership.svelte.ts`
2. Remove adapter functions from `capacity-subscriptions.svelte.ts`
3. Delete old test files
4. Update documentation

## Testing Strategy

### Unit Tests

**File**: `src/lib/network/attributes.test.ts`

```typescript
import { describe, it, expect } from 'vitest';
import {
  setEntityAttributePure,
  removeEntityAttributePure,
  subscribeToAttributePure,
  getEntityAttributePure,
  resolveEntityToPubkeyPure
} from './attributes';

describe('Entity Attribute Recognition', () => {
  it('should set and get entity attributes', () => {
    let declarations = {};
    
    declarations = setEntityAttributePure(declarations, 'alice_pub', 'skills', ['medicine']);
    
    const { value } = getEntityAttributePure(
      declarations,
      {},
      {},
      'alice_pub',
      'skills',
      {},
      {}
    );
    
    expect(value).toEqual(['medicine']);
  });
  
  it('should handle subscription override', () => {
    const declarations = {
      'alice_pub': { skills: ['my_view'] }
    };
    
    const subscriptions = {
      'alice_pub': { skills: 'bob_pub' }
    };
    
    const cache = {
      'bob_pub': {
        'alice_pub': { skills: ['bobs_view'] }
      }
    };
    
    // Subscription should NOT override declaration (declarations take precedence)
    const { value, source } = getEntityAttributePure(
      declarations,
      subscriptions,
      cache,
      'alice_pub',
      'skills',
      {},
      {}
    );
    
    expect(value).toEqual(['my_view']);
    expect(source).toBe('declared');
  });
  
  it('should auto-subscribe to entity\'s own data', () => {
    const cache = {
      'alice_pub': {
        'alice_pub': { capacity: [{ slot: '...' }] }
      }
    };
    
    const { value, source } = getEntityAttributePure(
      {},
      {},
      cache,
      'alice_pub',
      'capacity',
      {},
      {}
    );
    
    expect(value).toBeDefined();
    expect(source).toBe('self');
  });
  
  it('should resolve contact_id to pubkey', () => {
    const contacts = {
      'contact_123': { public_key: 'alice_pub' }
    };
    
    const pubkey = resolveEntityToPubkeyPure('contact_123', contacts, {});
    expect(pubkey).toBe('alice_pub');
  });
});
```

### Integration Tests

Test full roundtrip:
1. Declare attribute → persist to Holster
2. Subscribe from another user → receive via network
3. Cache and resolve → verify correct value
4. Update attribute → sync update
5. Unsubscribe → clear cache

### Backward Compatibility Tests

Verify adapters work:

```typescript
import { describe, it, expect } from 'vitest';
import {
  setMembershipList,
  getMembershipList,
  subscribeMembershipList
} from './membership.svelte';

describe('Membership Adapter (Backward Compatibility)', () => {
  it('should work with old API', () => {
    setMembershipList('org_test', ['alice', 'bob']);
    const members = getMembershipList('org_test');
    expect(members).toEqual(['alice', 'bob']);
  });
});
```

## Rollback Plan

If issues arise during migration:

1. **Adapters remain**: Old API continues to work
2. **Feature flag**: Add `USE_UNIFIED_ATTRIBUTES` flag
3. **Gradual rollout**: Enable for specific components first
4. **Monitoring**: Track errors and performance
5. **Quick disable**: Revert to old system if critical issues

## Performance Considerations

### Memory

- **Before**: 3 separate caches (membership, capacity, needs)
- **After**: 1 unified cache (slightly larger keys)
- **Impact**: ~10% increase in memory, acceptable for flexibility gain

### Lookup Speed

- **Before**: Direct store access `membershipCache[source][org]`
- **After**: Attribute lookup `cache[source][entity]['membership']`
- **Impact**: Negligible (one extra level of indirection, O(1))

### Network Traffic

- **Before**: Separate subscriptions per data type
- **After**: Can consolidate or keep separate
- **Impact**: Neutral (can optimize later)

## Documentation Updates

Files to update after migration:

1. `README.md` - Update API examples
2. `PROTOCOL.md` - Document unified attribute model
3. `docs/technical/protocol.md` - Update technical details
4. `docs/concepts/how-it-works.md` - Update recognition explanation
5. Component README files - Update usage examples

## Success Criteria

Migration is complete when:

- ✅ All components use unified API (directly or via helpers)
- ✅ All tests pass
- ✅ Backward compatibility maintained (adapters work)
- ✅ Documentation updated
- ✅ Performance benchmarks meet targets
- ✅ At least one new attribute type added (e.g., skills)
- ✅ Zero regressions in production

## Conclusion

This migration plan provides a safe, incremental path from the current specialized systems to the unified Entity-Attribute-Recognition framework. The adapter layer ensures backward compatibility while allowing gradual adoption of the new API.

Key benefits after migration:
- Symmetric recognition of any attribute for any entity
- Trivial to add new attribute types
- Clear, consistent API across the codebase
- Foundation for rich, multi-faceted recognition networks
