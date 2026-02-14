# Attribute Recognition Generalization

## Overview

This document explores generalizing the current org membership recognition system to a unified **Entity Attribute Recognition** framework that handles:

- **Entities**: Both users (pubkeys/contact_ids) and organizations (org_ids)
- **Attributes**: Membership, capacities, needs, and extensible to other attributes
- **Recognition Modes**: Self-declaration, recognition of others, and subscription-based

## Current Architecture Analysis

### 1. Organization Membership System

**Location**: `src/lib/network/membership.svelte.ts`, `src/lib/network/membership.ts`

**Structure**:
```typescript
// What YOU declare about org membership
UserMembershipLists: Record<org_id, member_ids[]>

// Who you subscribe to for org membership data
MembershipSubscriptions: Record<org_id, source_pubkey>

// Cached membership from network
MembershipCache: Record<source_pubkey, Record<org_id, member_ids[]>>
```

**Resolution Logic**:
1. Check your declared membership list first
2. If subscribed, check cached list from source
3. Otherwise undefined

**Example**:
```typescript
// I declare that "org_redcross" has these members
myMembershipLists = {
  "org_redcross": ["alice_pub", "bob_pub", "org_local_chapter"]
}

// I subscribe to Carol's membership list for "org_unicef"
myMembershipSubscriptions = {
  "org_unicef": "carol_pub"
}
```

### 2. Slot Subscription System

**Location**: `src/lib/network/capacity-subscriptions.svelte.ts`

**Structure**:
```typescript
// Who you subscribe to + what types
SlotSubscriptions: Record<pubkey, { capacity: boolean, needs: boolean }>

// Cached slots from network
capacityCache: Record<source_pubkey, AvailabilitySlot[]>
needCache: Record<source_pubkey, NeedSlot[]>
```

**Key Difference**: Slots are always self-declared by their owner, and we subscribe to others' declarations.

### 3. User/Organization Data

**Location**: `src/lib/network/users.svelte.ts`, `src/lib/network/organizations.svelte.ts`

**Structure**:
```typescript
// User's organizations (self-managed)
holsterOrganizations: Record<org_id, Organization>

// User's contacts (self-managed)
userContacts: Record<contact_id, Contact>

// Optional mapping: contact_id/org_id -> pubkey
Contact.public_key?: string
```

## Proposed Unified Model

### Core Concept: Entity-Attribute-Source Triple

Every piece of recognized data is a triple:
```
(entity_id, attribute_type, source_pubkey) → attribute_value
```

Where:
- **entity_id**: Who/what the attribute describes (pubkey, contact_id, org_id)
- **attribute_type**: What aspect we're recognizing (membership, capacity, needs, skills, etc.)
- **source_pubkey**: Who is making this declaration (defaults to entity's pubkey if available)

### Unified Schema

```typescript
// ═══════════════════════════════════════════════════════════════════
// ENTITY ATTRIBUTE RECOGNITION - UNIFIED
// ═══════════════════════════════════════════════════════════════════

/**
 * Attribute Types - Extensible enum
 */
type AttributeType = 
  | 'membership'      // Entity is member of orgs
  | 'capacity'        // Entity's capacity slots
  | 'needs'           // Entity's need slots
  | 'skills'          // Entity's skills/expertise
  | 'availability'    // Entity's time availability
  | 'location'        // Entity's location
  | 'contact_info'    // Entity's contact information
  | 'reputation'      // Entity's reputation scores
  | string;           // Extensible to custom attributes

/**
 * Entity Types - Union of all entity identifiers
 */
type EntityId = 
  | string;           // Can be: pubkey, contact_id, org_id

/**
 * Attribute Value - Type depends on attribute_type
 * Generic to allow any attribute structure
 */
type AttributeValue = unknown;

/**
 * My Attribute Declarations
 * 
 * What I declare about entities (including myself).
 * Structure: entity_id -> attribute_type -> value
 * 
 * Example:
 * {
 *   "my_pubkey": {
 *     "capacity": [{ slot: "...", quantity: 10 }],
 *     "needs": [{ slot: "...", quantity: 5 }],
 *     "skills": ["programming", "design"]
 *   },
 *   "org_redcross": {
 *     "membership": ["alice_pub", "bob_pub"],
 *     "location": { lat: 40.7, lng: -74.0 }
 *   },
 *   "alice_pub": {
 *     "skills": ["medicine", "emergency_response"]
 *   }
 * }
 */
type MyAttributeDeclarations = Record<EntityId, Partial<Record<AttributeType, AttributeValue>>>;

/**
 * Attribute Subscriptions
 * 
 * Who I subscribe to for which entity's which attributes.
 * Structure: entity_id -> attribute_type -> source_pubkey
 * 
 * Semantics:
 * - If source_pubkey is undefined → use entity's own pubkey (after id resolution)
 * - If entity doesn't have self-data → must specify source_pubkey
 * 
 * Example:
 * {
 *   "org_unicef": {
 *     "membership": "carol_pub"  // Subscribe to Carol's declaration of UNICEF membership
 *   },
 *   "alice_pub": {
 *     "capacity": undefined,     // Subscribe to Alice's own capacity declaration
 *     "skills": "bob_pub"        // Subscribe to Bob's recognition of Alice's skills
 *   }
 * }
 */
type AttributeSubscriptions = Record<EntityId, Partial<Record<AttributeType, string | undefined>>>;

/**
 * Attribute Cache
 * 
 * Cached attribute data from network.
 * Structure: source_pubkey -> entity_id -> attribute_type -> value
 * 
 * Local-first: "Trust until proven otherwise"
 */
type AttributeCache = Record<string, Record<EntityId, Partial<Record<AttributeType, AttributeValue>>>>;
```

### Resolution Logic

```typescript
/**
 * Get attribute value for an entity
 * 
 * Resolution order:
 * 1. Check my declarations (what I declare about this entity's attribute)
 * 2. Check subscriptions → resolve to source → check cache
 * 3. Auto-subscribe to entity's own data if:
 *    - Entity is a pubkey OR
 *    - Entity can be resolved to pubkey (contact_id → pubkey, org_id has designated pubkey)
 * 4. Otherwise undefined
 */
function getEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): AttributeValue | undefined {
  // 1. My declaration takes precedence
  if (myAttributeDeclarations[entity_id]?.[attribute_type] !== undefined) {
    return myAttributeDeclarations[entity_id][attribute_type];
  }
  
  // 2. Check explicit subscription
  const subscribedSource = attributeSubscriptions[entity_id]?.[attribute_type];
  if (subscribedSource !== undefined) {
    if (subscribedSource === null) {
      // Explicit: subscribe to entity's own data
      const pubkey = resolveEntityToPubkey(entity_id);
      if (pubkey) {
        return attributeCache[pubkey]?.[entity_id]?.[attribute_type];
      }
    } else {
      // Subscribe to specific source's declaration
      return attributeCache[subscribedSource]?.[entity_id]?.[attribute_type];
    }
  }
  
  // 3. Default: Auto-subscribe to entity's own data
  const pubkey = resolveEntityToPubkey(entity_id);
  if (pubkey) {
    return attributeCache[pubkey]?.[pubkey]?.[attribute_type];
  }
  
  return undefined;
}

/**
 * Resolve entity identifier to pubkey
 * 
 * Handles:
 * - pubkey → return as-is
 * - contact_id → lookup in contacts, return public_key
 * - org_id → lookup in organizations, return designated pubkey (if any)
 */
function resolveEntityToPubkey(entity_id: EntityId): string | undefined {
  // Already a pubkey
  if (!entity_id.startsWith('contact_') && !entity_id.startsWith('org_')) {
    return entity_id;
  }
  
  // Contact ID
  if (entity_id.startsWith('contact_')) {
    const contact = userContacts[entity_id];
    return contact?.public_key;
  }
  
  // Organization ID
  if (entity_id.startsWith('org_')) {
    const org = holsterOrganizations[entity_id];
    // Organizations could have a designated pubkey field
    return org?.designated_pubkey;
  }
  
  return undefined;
}
```

### Holster Storage Structure

```typescript
/**
 * User's Holster space structure:
 * 
 * <my_pubkey>/
 *   attributes/                           # My attribute declarations
 *     entities/                           # Organized by entity
 *       <entity_id>/
 *         membership: [...]               # My recognition of entity's membership
 *         capacity: [...]                 # My recognition of entity's capacities
 *         needs: [...]                    # My recognition of entity's needs
 *         skills: [...]                   # My recognition of entity's skills
 *         ...
 *   
 *   attribute-subscriptions/              # Who I subscribe to
 *     <entity_id>/
 *       membership: "source_pubkey"
 *       capacity: null                    # null = subscribe to entity's own
 *       ...
 *   
 *   contacts/                             # Contact mappings
 *     <contact_id>: { name, public_key? }
 *   
 *   organizations/                        # Organization mappings
 *     <org_id>: { names, designated_pubkey? }
 */

// Holster paths
const HOLSTER_PATHS = {
  // My declarations about entities
  myAttributes: 'attributes/entities',
  
  // My subscription preferences
  attributeSubscriptions: 'attribute-subscriptions',
  
  // Identity mappings
  contacts: 'contacts',
  organizations: 'organizations'
};
```

## Migration Path

### Phase 1: Extend Current System (Backward Compatible)

Add unified attribute layer alongside existing specialized systems:

```typescript
// NEW: Unified attribute store
export const myAttributeDeclarations = createStore({
  holsterPath: 'attributes/entities',
  schema: MyAttributeDeclarationsSchema,
  persistDebounce: 200
});

export const attributeSubscriptions = createStore({
  holsterPath: 'attribute-subscriptions',
  schema: AttributeSubscriptionsSchema,
  persistDebounce: 200
});

export const attributeCache = writable<AttributeCache>({});

// BRIDGE: Existing stores delegate to unified system
// membership.svelte.ts can read/write through attribute layer
export function setMembershipList(org_id: string, members: string[]): void {
  // Write to BOTH old and new systems during migration
  setEntityAttribute(org_id, 'membership', members);
  // ... old code remains for compatibility
}
```

### Phase 2: Migrate Consumers

Update components to use unified API:

```typescript
// BEFORE
import { getMembershipList } from './membership.svelte';
const members = getMembershipList('org_redcross');

// AFTER
import { getEntityAttribute } from './attributes.svelte';
const members = getEntityAttribute('org_redcross', 'membership');
```

### Phase 3: Deprecate Specialized Systems

Remove old membership/slot-specific stores once all consumers migrated.

## Benefits of Unified Model

### 1. **Symmetry**
- Same pattern for all attributes (membership, capacity, needs, skills, etc.)
- Self-recognition and other-recognition use same structure
- Clear, consistent resolution logic

### 2. **Flexibility**
- Subscribe to entity's own data (default) OR
- Subscribe to someone else's recognition of that entity
- Mix declarations and subscriptions freely

### 3. **Extensibility**
- Easy to add new attribute types without changing architecture
- Custom attributes per use case
- Future: reputation, credentials, availability, location, etc.

### 4. **Composability**
- Organizations can be members of organizations (recursive)
- Contacts can be resolved to pubkeys for subscription
- Clear separation: identity mapping vs. attribute recognition

### 5. **Transparency**
- Always know where data comes from (source_pubkey)
- Can inspect subscription graph
- Clear distinction between my view and others' views

## Example Use Cases

### Use Case 1: Self-Declaration

```typescript
// I declare my own capacities
setEntityAttribute(myPubkey, 'capacity', [
  { need_type_id: 'food', quantity: 100 },
  { need_type_id: 'housing', quantity: 5 }
]);

// Others subscribe to my capacity declaration (default)
// getEntityAttribute(myPubkey, 'capacity') → auto-subscribes to me
```

### Use Case 2: Organization Membership (Declared)

```typescript
// I manage an organization, so I declare its membership
setEntityAttribute('org_redcross', 'membership', [
  'alice_pub',
  'bob_pub',
  'org_local_chapter'  // Recursive!
]);

// Others can subscribe to MY declaration of org_redcross membership
attributeSubscriptions.set({
  'org_redcross': {
    'membership': myPubkey
  }
});
```

### Use Case 3: Organization Membership (Subscribed)

```typescript
// I don't know UNICEF membership, but Carol does
subscribeToAttribute('org_unicef', 'membership', 'carol_pub');

// Now when I call getEntityAttribute('org_unicef', 'membership')
// → Fetches from Carol's declaration
```

### Use Case 4: Skill Recognition

```typescript
// I recognize Alice's skills (my perspective)
setEntityAttribute('alice_pub', 'skills', [
  'emergency_medicine',
  'trauma_care',
  'disaster_response'
]);

// But Bob might have different recognition of Alice's skills
// If I want Bob's perspective instead:
subscribeToAttribute('alice_pub', 'skills', 'bob_pub');
```

### Use Case 5: Mixed Recognition

```typescript
// My attribute declarations
{
  // Self-declaration
  [myPubkey]: {
    capacity: [...my capacities...],
    needs: [...my needs...],
    skills: [...my skills...]
  },
  
  // I declare org membership
  "org_redcross": {
    membership: ["alice_pub", "bob_pub"]
  },
  
  // I recognize Alice's skills
  "alice_pub": {
    skills: ["medicine"]
  }
}

// My subscriptions
{
  // Subscribe to Carol's view of UNICEF
  "org_unicef": {
    membership: "carol_pub"
  },
  
  // Subscribe to Alice's own capacity (default behavior)
  "alice_pub": {
    capacity: undefined  // or null, means self
  },
  
  // Subscribe to Bob's recognition of Alice's skills
  // (overrides my own declaration above!)
  "alice_pub": {
    skills: "bob_pub"
  }
}
```

## API Design

### Core Functions

```typescript
/**
 * Declare an attribute for an entity
 */
export function setEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType,
  value: AttributeValue
): void;

/**
 * Remove an attribute declaration
 */
export function removeEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): void;

/**
 * Get an attribute value (with resolution)
 */
export function getEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): AttributeValue | undefined;

/**
 * Subscribe to someone's declaration of an attribute
 */
export function subscribeToAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType,
  source_pubkey: string | null  // null = entity's own
): void;

/**
 * Unsubscribe from an attribute
 */
export function unsubscribeFromAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): void;

/**
 * Get subscription source for an attribute
 */
export function getAttributeSource(
  entity_id: EntityId,
  attribute_type: AttributeType
): 'declared' | 'self' | string | undefined;

/**
 * List all entities with a specific attribute
 */
export function getEntitiesWithAttribute(
  attribute_type: AttributeType
): EntityId[];

/**
 * Get all attributes for an entity
 */
export function getEntityAttributes(
  entity_id: EntityId
): Partial<Record<AttributeType, AttributeValue>>;
```

### Type-Specific Helpers

```typescript
/**
 * Membership-specific helpers (backward compatibility)
 */
export function setMembershipList(org_id: string, members: string[]): void {
  setEntityAttribute(org_id, 'membership', members);
}

export function getMembershipList(org_id: string): string[] | undefined {
  return getEntityAttribute(org_id, 'membership') as string[] | undefined;
}

/**
 * Capacity-specific helpers
 */
export function setCapacities(entity_id: EntityId, slots: AvailabilitySlot[]): void {
  setEntityAttribute(entity_id, 'capacity', slots);
}

export function getCapacities(entity_id: EntityId): AvailabilitySlot[] | undefined {
  return getEntityAttribute(entity_id, 'capacity') as AvailabilitySlot[] | undefined;
}

/**
 * Need-specific helpers
 */
export function setNeeds(entity_id: EntityId, slots: NeedSlot[]): void {
  setEntityAttribute(entity_id, 'needs', slots);
}

export function getNeeds(entity_id: EntityId): NeedSlot[] | undefined {
  return getEntityAttribute(entity_id, 'needs') as NeedSlot[] | undefined;
}
```

## Open Questions

1. **Attribute Type Registry**: Should we have a central registry of attribute types with schemas?

2. **Conflict Resolution**: What happens when I have both a declaration AND a subscription for the same entity-attribute pair?
   - Current proposal: Declaration takes precedence
   - Alternative: Subscription takes precedence
   - Alternative: Explicit priority flag

3. **Organization Pubkeys**: Should organizations have designated pubkeys?
   - Allows organization to self-publish attributes
   - Enables organization-signed declarations
   - Requires key management for orgs

4. **Multi-Source Subscriptions**: Should we allow subscribing to multiple sources and merging?
   - Example: Subscribe to both Alice and Bob's recognition of Carol's skills
   - Requires merge strategy (union, intersection, weighted average)

5. **Attribute Versioning**: How to handle attribute schema evolution?
   - Attributes might have different versions over time
   - Need migration paths for breaking changes

6. **Privacy & Permissions**: Should there be access control on attributes?
   - Some attributes might be private (contact_info)
   - Some might be public (skills, capacities)
   - Some might be permissioned (membership lists)

## Next Steps

1. **Create schemas** in `schemas.ts`:
   - `AttributeType` enum
   - `MyAttributeDeclarationsSchema`
   - `AttributeSubscriptionsSchema`
   - `AttributeCacheSchema`

2. **Implement core module** `src/lib/network/attributes.svelte.ts`:
   - Holster-backed stores
   - Resolution logic
   - Subscription management

3. **Bridge existing systems**:
   - Update `membership.svelte.ts` to delegate to attributes
   - Update `capacity-subscriptions.svelte.ts` to delegate to attributes
   - Ensure backward compatibility

4. **Migrate consumers**:
   - Update components one by one
   - Use type-specific helpers for smooth migration

5. **Add new attribute types**:
   - Skills
   - Availability
   - Reputation
   - Contact info

## Conclusion

This unified Entity-Attribute-Recognition framework provides a symmetric, extensible foundation for recognizing any attribute of any entity. It maintains backward compatibility while opening the door to rich, multi-faceted recognition beyond just organizational membership and resource slots.

The key insight is treating all recognition as triples `(entity, attribute, source)` with consistent resolution logic that defaults to self-declaration but allows flexible subscription to others' perspectives.
