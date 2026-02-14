# Current System Mapping to Unified Attribute Recognition

## Executive Summary

This document traces the current fragmented recognition systems and shows how they map to the proposed unified Entity Attribute Recognition framework.

## Current File Inventory

### 1. Organization Management
**File**: `src/lib/network/organizations.svelte.ts`

**Purpose**: Manage organization metadata (names, descriptions, emoji)

**Current Structure**:
```typescript
// User's organizations (what orgs I'm tracking/managing)
holsterOrganizations: Record<org_id, Organization>

// Global organizations list (all registered orgs)
globalOrganizations: Record<org_id, Organization>

type Organization = {
  org_id: string;
  names: Record<language, string>;  // Multi-language names
  emoji?: string;
  description?: string;
  created_at: number;
  updated_at: number;
}
```

**Unified Model Mapping**:
```typescript
// Organization metadata becomes attributes on org entities
setEntityAttribute('org_redcross', 'metadata', {
  names: { en: 'Red Cross', es: 'Cruz Roja' },
  emoji: '❤️',
  description: 'Humanitarian organization'
});

// Registration in global list → attribute of the org
setEntityAttribute('org_redcross', 'registered', true);
```

### 2. Membership Management
**File**: `src/lib/network/membership.svelte.ts` + `membership.ts`

**Purpose**: Track who belongs to which organizations

**Current Structure**:
```typescript
// What I declare about org membership
myMembershipLists: Record<org_id, string[]>

// Who I subscribe to for org membership
myMembershipSubscriptions: Record<org_id, source_pubkey>

// Cached membership from network
membershipCache: Record<source_pubkey, Record<org_id, string[]>>

// Example
myMembershipLists = {
  'org_redcross': ['alice_pub', 'bob_pub', 'org_local']
}

myMembershipSubscriptions = {
  'org_unicef': 'carol_pub'  // Subscribe to Carol's declaration
}
```

**Unified Model Mapping**:
```typescript
// EXACT SAME SEMANTICS, but unified structure
setEntityAttribute('org_redcross', 'membership', ['alice_pub', 'bob_pub', 'org_local']);

subscribeToAttribute('org_unicef', 'membership', 'carol_pub');

// Resolution is identical
getEntityAttribute('org_redcross', 'membership')  // → ['alice_pub', 'bob_pub', 'org_local']
getEntityAttribute('org_unicef', 'membership')   // → cached from Carol
```

### 3. Capacity/Need Slot Subscriptions
**File**: `src/lib/network/capacity-subscriptions.svelte.ts`

**Purpose**: Subscribe to others' capacity/need declarations with filtering

**Current Structure**:
```typescript
// Who I subscribe to + what types
slotSubscriptions: Record<pubkey, { capacity: boolean, needs: boolean }>

// Filters for auto-population
slotFilters: Record<filter_id, SlotFilter>

// Cached slots from network
capacityCache: Record<source_pubkey, AvailabilitySlot[]>
needCache: Record<source_pubkey, NeedSlot[]>

// Example
slotSubscriptions = {
  'alice_pub': { capacity: true, needs: false },
  'bob_pub': { capacity: false, needs: true }
}
```

**Unified Model Mapping**:
```typescript
// Subscribe to Alice's capacity
subscribeToAttribute('alice_pub', 'capacity', null);  // null = self

// Subscribe to Bob's needs
subscribeToAttribute('bob_pub', 'needs', null);

// Get capacities
getEntityAttribute('alice_pub', 'capacity')  // → AvailabilitySlot[]

// Get needs
getEntityAttribute('bob_pub', 'needs')  // → NeedSlot[]
```

### 4. User/Contact Management
**File**: `src/lib/network/users.svelte.ts` + `contacts.svelte.ts`

**Purpose**: Map contact_ids to pubkeys, cache user names/aliases

**Current Structure**:
```typescript
// User's contacts (identity mapping)
userContacts: Record<contact_id, Contact>

type Contact = {
  contact_id: string;
  name: string;
  public_key?: string;
  created_at: number;
  updated_at: number;
}

// Cached user names/aliases from network
userNamesCache: Record<pubkey, string>
userAliasesCache: Record<pubkey, string>
```

**Unified Model Mapping**:
```typescript
// Contact remains as identity mapping (not an attribute)
// But contact metadata can be attributes

// User's chosen name for a contact (local)
setEntityAttribute('contact_abc', 'display_name', 'Alice Smith');

// User's own alias (self-declared)
setEntityAttribute(myPubkey, 'alias', 'bob_the_builder');

// Subscribe to Alice's self-declared alias
subscribeToAttribute('alice_pub', 'alias', null);
```

## Unified Model: Complete Picture

### Entity Types

```typescript
type EntityId = 
  | string  // Pubkey (base64)
  | `contact_${string}`  // Contact ID (local identifier)
  | `org_${string}`;     // Organization ID

// Identity Resolution
contact_id → Contact.public_key → pubkey
org_id → Organization.designated_pubkey → pubkey (optional)
pubkey → pubkey (already resolved)
```

### Attribute Types

```typescript
type AttributeType =
  // Organization attributes
  | 'metadata'       // Organization metadata (names, description, emoji)
  | 'membership'     // Members of organization
  | 'registered'     // Whether org is globally registered
  
  // User attributes
  | 'capacity'       // User's capacity slots
  | 'needs'          // User's need slots
  | 'alias'          // User's self-chosen alias
  | 'display_name'   // Name others use for this user
  | 'skills'         // User's skills/expertise
  | 'availability'   // User's time availability
  | 'location'       // User's location
  | 'contact_info'   // User's contact information
  
  // Shared attributes
  | 'reputation'     // Reputation scores (for users or orgs)
  | string;          // Extensible
```

### Storage Layout

```
<my_pubkey>/
  attributes/
    entities/
      <my_pubkey>/
        capacity: AvailabilitySlot[]
        needs: NeedSlot[]
        alias: "bob_the_builder"
        skills: ["programming", "design"]
      
      org_redcross/
        membership: ["alice_pub", "bob_pub"]
        metadata: { names: {...}, emoji: "❤️" }
      
      alice_pub/
        skills: ["medicine"]  # My recognition of Alice's skills
      
      contact_abc/
        display_name: "Alice Smith"  # My local name for this contact
  
  attribute-subscriptions/
    org_unicef/
      membership: "carol_pub"  # Subscribe to Carol's declaration
    
    alice_pub/
      capacity: null           # Subscribe to Alice's own
      alias: null              # Subscribe to Alice's own
      skills: "bob_pub"        # Subscribe to Bob's recognition of Alice
  
  contacts/
    contact_abc: { name: "Alice Smith", public_key: "alice_pub" }
    contact_xyz: { name: "Bob", public_key: null }  # No pubkey yet
  
  organizations/
    org_redcross: { names: {...}, designated_pubkey: null }
```

## Migration Strategy

### Phase 1: Add Unified Layer (Parallel)

Create `attributes.svelte.ts` with unified system, but don't change existing code:

```typescript
// NEW: attributes.svelte.ts
export function setEntityAttribute(...) { ... }
export function getEntityAttribute(...) { ... }

// UNCHANGED: membership.svelte.ts (still works)
export function setMembershipList(...) { ... }
export function getMembershipList(...) { ... }
```

### Phase 2: Bridge Existing to New (Dual-Write)

Update existing modules to write to BOTH systems:

```typescript
// membership.svelte.ts (BRIDGED)
export function setMembershipList(org_id: string, members: string[]): void {
  // WRITE TO BOTH SYSTEMS
  setEntityAttribute(org_id, 'membership', members);  // NEW
  
  // OLD (keep for backward compatibility)
  const currentLists = get(myMembershipLists);
  const updatedLists = setMembershipListPure(currentLists, org_id, members);
  myMembershipLists.set(updatedLists);
}

export function getMembershipList(org_id: string): string[] | undefined {
  // READ FROM NEW SYSTEM (authoritative)
  return getEntityAttribute(org_id, 'membership') as string[] | undefined;
}
```

### Phase 3: Migrate Consumers

Update components to use unified API:

```typescript
// BEFORE
import { getMembershipList } from '$lib/network/membership.svelte';
const members = getMembershipList('org_redcross');

// AFTER
import { getEntityAttribute } from '$lib/network/attributes.svelte';
const members = getEntityAttribute('org_redcross', 'membership') as string[];
```

OR use type-specific helpers:

```typescript
// Helper for backward compatibility
import { getMembershipList } from '$lib/network/attributes.svelte';
const members = getMembershipList('org_redcross');

// Where getMembershipList is defined as:
export function getMembershipList(org_id: string): string[] | undefined {
  return getEntityAttribute(org_id, 'membership') as string[] | undefined;
}
```

### Phase 4: Remove Old Systems

Once all consumers migrated, remove old stores and logic:

```typescript
// DELETE: myMembershipLists store
// DELETE: myMembershipSubscriptions store
// DELETE: membershipCache store
// DELETE: Pure functions in membership.ts (replaced by attributes.ts)

// KEEP: Type-specific helpers for convenience
export function getMembershipList(...) { ... }  // Delegates to getEntityAttribute
```

## Detailed Mapping Table

| Current System | Current Location | Unified Attribute Type | Unified Entity | Notes |
|----------------|------------------|------------------------|----------------|-------|
| `myMembershipLists[org_id]` | `membership.svelte.ts` | `'membership'` | `org_id` | Exact same semantics |
| `myMembershipSubscriptions[org_id]` | `membership.svelte.ts` | Subscription to `'membership'` | `org_id` | Maps to `attributeSubscriptions` |
| `slotSubscriptions[pubkey].capacity` | `capacity-subscriptions.svelte.ts` | Subscription to `'capacity'` | `pubkey` | Subscribe to self |
| `slotSubscriptions[pubkey].needs` | `capacity-subscriptions.svelte.ts` | Subscription to `'needs'` | `pubkey` | Subscribe to self |
| `capacityCache[source][...]` | `capacity-subscriptions.svelte.ts` | Cache for `'capacity'` | `source` → entity | Unified cache structure |
| `needCache[source][...]` | `capacity-subscriptions.svelte.ts` | Cache for `'needs'` | `source` → entity | Unified cache structure |
| `holsterOrganizations[org_id]` | `organizations.svelte.ts` | `'metadata'` + `'registered'` | `org_id` | Org metadata as attributes |
| `userContacts[contact_id]` | `users.svelte.ts` | Identity mapping (not attribute) | - | Contacts remain separate |
| `userNamesCache[pubkey]` | `users.svelte.ts` | `'alias'` or `'display_name'` | `pubkey` | User-chosen name |
| `userAliasesCache[pubkey]` | `users.svelte.ts` | `'alias'` | `pubkey` | Network alias |

## Code Examples: Before vs. After

### Example 1: Set Organization Membership

**BEFORE** (Current):
```typescript
import { setMembershipList } from '$lib/network/membership.svelte';

setMembershipList('org_redcross', ['alice_pub', 'bob_pub']);
```

**AFTER** (Unified):
```typescript
import { setEntityAttribute } from '$lib/network/attributes.svelte';

setEntityAttribute('org_redcross', 'membership', ['alice_pub', 'bob_pub']);
```

**COMPATIBILITY** (Type-specific helper):
```typescript
import { setMembershipList } from '$lib/network/attributes.svelte';

setMembershipList('org_redcross', ['alice_pub', 'bob_pub']);
```

### Example 2: Subscribe to Someone's Capacity

**BEFORE** (Current):
```typescript
import { subscribeToSlots } from '$lib/network/capacity-subscriptions.svelte';

subscribeToSlots('alice_pub', { capacity: true, needs: false });
```

**AFTER** (Unified):
```typescript
import { subscribeToAttribute } from '$lib/network/attributes.svelte';

subscribeToAttribute('alice_pub', 'capacity', null);  // null = self
```

**COMPATIBILITY** (Type-specific helper):
```typescript
import { subscribeToCapacity } from '$lib/network/attributes.svelte';

subscribeToCapacity('alice_pub');
```

### Example 3: Get Membership List

**BEFORE** (Current):
```typescript
import { getMembershipList } from '$lib/network/membership.svelte';

const members = getMembershipList('org_redcross');
// → string[] | undefined
```

**AFTER** (Unified):
```typescript
import { getEntityAttribute } from '$lib/network/attributes.svelte';

const members = getEntityAttribute('org_redcross', 'membership') as string[];
// → string[] | undefined
```

**COMPATIBILITY** (Type-specific helper):
```typescript
import { getMembershipList } from '$lib/network/attributes.svelte';

const members = getMembershipList('org_redcross');
// → string[] | undefined (same API!)
```

### Example 4: Mixed Recognition

**BEFORE** (Not possible - fragmented across multiple systems):
```typescript
// Had to use different APIs for different attributes
import { setMembershipList } from '$lib/network/membership.svelte';
import { subscribeToSlots } from '$lib/network/capacity-subscriptions.svelte';

setMembershipList('org_redcross', ['alice_pub']);
subscribeToSlots('alice_pub', { capacity: true });
// Can't recognize Alice's skills - no system for it!
```

**AFTER** (Unified - consistent API):
```typescript
import { 
  setEntityAttribute, 
  subscribeToAttribute 
} from '$lib/network/attributes.svelte';

// Declare org membership
setEntityAttribute('org_redcross', 'membership', ['alice_pub']);

// Subscribe to Alice's capacity
subscribeToAttribute('alice_pub', 'capacity', null);

// Recognize Alice's skills (NEW capability!)
setEntityAttribute('alice_pub', 'skills', ['medicine', 'emergency_response']);
```

## Benefits Summary

### 1. **Consistency**
- Same API for all attribute types
- Same resolution logic everywhere
- Same subscription pattern

### 2. **Extensibility**
- Add new attributes without changing architecture
- Custom attributes per use case
- No need to create new stores/modules

### 3. **Clarity**
- Clear separation: identity mapping vs. attribute recognition
- Explicit source tracking (declared vs. subscribed)
- Transparent resolution order

### 4. **Power**
- Self-recognition and other-recognition use same structure
- Mix declarations and subscriptions freely
- Subscribe to others' recognition of third parties

### 5. **Backward Compatibility**
- Type-specific helpers maintain existing API
- Gradual migration path
- No breaking changes required

## Implementation Checklist

- [ ] Create schemas in `src/lib/protocol/schemas.ts`
- [ ] Implement pure functions in `src/lib/network/attributes.ts`
- [ ] Implement Svelte stores in `src/lib/network/attributes.svelte.ts`
- [ ] Add type-specific helpers (getMembershipList, setCapacities, etc.)
- [ ] Bridge existing membership.svelte.ts (dual-write)
- [ ] Bridge existing capacity-subscriptions.svelte.ts (dual-write)
- [ ] Migrate components one by one
- [ ] Remove old stores once migration complete
- [ ] Add new attribute types (skills, availability, etc.)
- [ ] Update documentation and examples

## Conclusion

The unified Entity Attribute Recognition framework provides a symmetric, powerful foundation that subsumes all current recognition patterns while enabling new capabilities. The migration path is clear, incremental, and maintains backward compatibility throughout.
