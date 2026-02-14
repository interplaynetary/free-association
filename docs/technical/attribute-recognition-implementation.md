# Entity Attribute Recognition - Implementation Guide

## File Structure

```
src/lib/network/
├── attributes.svelte.ts          # Main unified attribute system
├── attributes.ts                 # Pure functions (no Svelte deps)
├── membership.svelte.ts          # ADAPTER: delegates to attributes
├── capacity-subscriptions.svelte.ts  # ADAPTER: delegates to attributes
├── organizations.svelte.ts       # Entity registry (identity mapping)
├── users.svelte.ts              # Entity registry (identity mapping)
└── contacts.svelte.ts           # Entity registry (identity mapping)

src/lib/protocol/
└── schemas.ts                    # Add attribute schemas
```

## Schema Definitions

Add to `src/lib/protocol/schemas.ts`:

```typescript
// ═══════════════════════════════════════════════════════════════════
// ENTITY ATTRIBUTE RECOGNITION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Attribute Type - Extensible string union
 * 
 * Core types defined here, but system allows arbitrary strings
 * for custom attribute types.
 */
export const AttributeTypeSchema = z.enum([
  'membership',    // Entity membership in orgs
  'capacity',      // Entity's capacity slots
  'needs',         // Entity's need slots
  'skills',        // Entity's skills/expertise
  'availability',  // Entity's time availability
  'location',      // Entity's location
  'contact_info',  // Entity's contact information
  'reputation'     // Entity's reputation/ratings
]);

export type AttributeType = z.infer<typeof AttributeTypeSchema> | string;

/**
 * Entity Identifier
 * 
 * Can be:
 * - Public key (base64 string)
 * - contact_id (starts with "contact_")
 * - org_id (starts with "org_")
 */
export type EntityId = string;

/**
 * Attribute Value Schemas by Type
 * 
 * Each attribute type has its own value schema.
 * This provides type safety while maintaining flexibility.
 */
export const AttributeValueSchemas = {
  membership: z.array(z.string()),  // Array of entity IDs
  capacity: z.array(AvailabilitySlotSchema),
  needs: z.array(NeedSlotSchema),
  skills: z.array(z.string()),  // Array of skill identifiers
  availability: z.any(),  // TBD: availability schema
  location: z.object({
    lat: z.number(),
    lng: z.number(),
    address: z.string().optional()
  }),
  contact_info: z.object({
    email: z.string().optional(),
    phone: z.string().optional(),
    website: z.string().optional()
  }),
  reputation: z.record(z.string(), z.number())  // dimension -> score
};

/**
 * Attribute Value - Generic container
 * 
 * The actual type depends on attribute_type.
 * Runtime validation uses AttributeValueSchemas.
 */
export const AttributeValueSchema = z.any();
export type AttributeValue = unknown;

/**
 * Entity Attributes - All attributes for a single entity
 * 
 * Maps attribute_type -> value
 * Partial because not all entities have all attributes
 */
export const EntityAttributesSchema = z.record(
  z.string(),  // attribute_type
  AttributeValueSchema
);

export type EntityAttributes = Record<string, AttributeValue>;

/**
 * My Attribute Declarations
 * 
 * What I declare about entities (including myself).
 * Structure: entity_id -> attribute_type -> value
 * 
 * Stored at: <my_pubkey>/attributes/entities
 */
export const MyAttributeDeclarationsSchema = z.record(
  z.string(),  // entity_id
  EntityAttributesSchema
);

export type MyAttributeDeclarations = z.infer<typeof MyAttributeDeclarationsSchema>;

/**
 * Attribute Subscription Entry
 * 
 * Who to subscribe to for a specific attribute.
 * - string: pubkey of source
 * - null: subscribe to entity's own data
 * - undefined: no subscription (use my declaration or default to self)
 */
export const AttributeSubscriptionEntrySchema = z.union([
  z.string(),  // source pubkey
  z.null()     // self
]);

export type AttributeSubscriptionEntry = string | null | undefined;

/**
 * Entity Subscriptions - Subscriptions for one entity
 * 
 * Maps attribute_type -> source
 */
export const EntitySubscriptionsSchema = z.record(
  z.string(),  // attribute_type
  AttributeSubscriptionEntrySchema.optional()
);

export type EntitySubscriptions = Record<string, AttributeSubscriptionEntry>;

/**
 * Attribute Subscriptions
 * 
 * Who I subscribe to for which entity's which attributes.
 * Structure: entity_id -> attribute_type -> source_pubkey | null
 * 
 * Stored at: <my_pubkey>/attribute-subscriptions
 */
export const AttributeSubscriptionsSchema = z.record(
  z.string(),  // entity_id
  EntitySubscriptionsSchema
);

export type AttributeSubscriptions = z.infer<typeof AttributeSubscriptionsSchema>;

/**
 * Attribute Cache Entry - Cached data from one source
 * 
 * Structure: entity_id -> attribute_type -> value
 */
export const AttributeCacheEntrySchema = z.record(
  z.string(),  // entity_id
  EntityAttributesSchema
);

export type AttributeCacheEntry = Record<string, EntityAttributes>;

/**
 * Attribute Cache
 * 
 * Cached attribute data from network sources.
 * Structure: source_pubkey -> entity_id -> attribute_type -> value
 * 
 * In-memory only (not persisted, rebuilt from network on load)
 */
export const AttributeCacheSchema = z.record(
  z.string(),  // source_pubkey
  AttributeCacheEntrySchema
);

export type AttributeCache = z.infer<typeof AttributeCacheSchema>;
```

## Core Implementation

### File: `src/lib/network/attributes.ts`

Pure functions (no Svelte dependencies):

```typescript
/**
 * Attribute Recognition - Pure Functions
 * 
 * Pure computational functions for entity attribute management.
 * These functions operate on plain data structures without Svelte dependencies.
 * 
 * For Svelte store integration, see attributes.svelte.ts
 */

import type {
  EntityId,
  AttributeType,
  AttributeValue,
  MyAttributeDeclarations,
  AttributeSubscriptions,
  AttributeCache,
  EntityAttributes,
  AttributeSubscriptionEntry
} from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// DECLARATION OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Set an attribute for an entity
 */
export function setEntityAttributePure(
  currentDeclarations: MyAttributeDeclarations | null | undefined,
  entity_id: EntityId,
  attribute_type: AttributeType,
  value: AttributeValue
): MyAttributeDeclarations {
  const declarations = currentDeclarations || {};
  const entityAttrs = declarations[entity_id] || {};
  
  return {
    ...declarations,
    [entity_id]: {
      ...entityAttrs,
      [attribute_type]: value
    }
  };
}

/**
 * Remove an attribute for an entity
 */
export function removeEntityAttributePure(
  currentDeclarations: MyAttributeDeclarations | null | undefined,
  entity_id: EntityId,
  attribute_type: AttributeType
): MyAttributeDeclarations {
  if (!currentDeclarations || !currentDeclarations[entity_id]) {
    return currentDeclarations || {};
  }
  
  const { [attribute_type]: removed, ...remainingAttrs } = currentDeclarations[entity_id];
  
  // If no attributes remain, remove the entity entry
  if (Object.keys(remainingAttrs).length === 0) {
    const { [entity_id]: removedEntity, ...remainingDeclarations } = currentDeclarations;
    return remainingDeclarations;
  }
  
  return {
    ...currentDeclarations,
    [entity_id]: remainingAttrs
  };
}

/**
 * Remove all attributes for an entity
 */
export function removeEntityPure(
  currentDeclarations: MyAttributeDeclarations | null | undefined,
  entity_id: EntityId
): MyAttributeDeclarations {
  if (!currentDeclarations) return {};
  
  const { [entity_id]: removed, ...remaining } = currentDeclarations;
  return remaining;
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to an attribute from a source
 */
export function subscribeToAttributePure(
  currentSubscriptions: AttributeSubscriptions | null | undefined,
  entity_id: EntityId,
  attribute_type: AttributeType,
  source_pubkey: string | null  // null = entity's own
): AttributeSubscriptions {
  const subscriptions = currentSubscriptions || {};
  const entitySubs = subscriptions[entity_id] || {};
  
  return {
    ...subscriptions,
    [entity_id]: {
      ...entitySubs,
      [attribute_type]: source_pubkey
    }
  };
}

/**
 * Unsubscribe from an attribute
 */
export function unsubscribeFromAttributePure(
  currentSubscriptions: AttributeSubscriptions | null | undefined,
  currentCache: AttributeCache,
  entity_id: EntityId,
  attribute_type: AttributeType
): {
  subscriptions: AttributeSubscriptions;
  cache: AttributeCache;
  removedSource: string | null | undefined;
} {
  if (!currentSubscriptions || !currentSubscriptions[entity_id]) {
    return {
      subscriptions: currentSubscriptions || {},
      cache: currentCache,
      removedSource: undefined
    };
  }
  
  const { [attribute_type]: removedSource, ...remainingAttrs } = currentSubscriptions[entity_id];
  
  // Remove entity subscription entry if empty
  let updatedSubscriptions: AttributeSubscriptions;
  if (Object.keys(remainingAttrs).length === 0) {
    const { [entity_id]: removedEntity, ...remaining } = currentSubscriptions;
    updatedSubscriptions = remaining;
  } else {
    updatedSubscriptions = {
      ...currentSubscriptions,
      [entity_id]: remainingAttrs
    };
  }
  
  // Clear from cache if it was from a specific source
  let updatedCache = currentCache;
  if (removedSource && typeof removedSource === 'string') {
    const sourceCache = currentCache[removedSource];
    if (sourceCache && sourceCache[entity_id]) {
      const { [attribute_type]: removedAttr, ...remainingEntityAttrs } = sourceCache[entity_id];
      updatedCache = {
        ...currentCache,
        [removedSource]: {
          ...sourceCache,
          [entity_id]: remainingEntityAttrs
        }
      };
    }
  }
  
  return {
    subscriptions: updatedSubscriptions,
    cache: updatedCache,
    removedSource
  };
}

// ═══════════════════════════════════════════════════════════════════
// CACHE OPERATIONS (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Update cache with data from a source
 */
export function updateAttributeCachePure(
  currentCache: AttributeCache,
  source_pubkey: string,
  entity_id: EntityId,
  attribute_type: AttributeType,
  value: AttributeValue
): AttributeCache {
  const sourceCache = currentCache[source_pubkey] || {};
  const entityAttrs = sourceCache[entity_id] || {};
  
  return {
    ...currentCache,
    [source_pubkey]: {
      ...sourceCache,
      [entity_id]: {
        ...entityAttrs,
        [attribute_type]: value
      }
    }
  };
}

/**
 * Update cache with multiple attributes from a source
 */
export function updateEntityCachePure(
  currentCache: AttributeCache,
  source_pubkey: string,
  entity_id: EntityId,
  attributes: EntityAttributes
): AttributeCache {
  const sourceCache = currentCache[source_pubkey] || {};
  
  return {
    ...currentCache,
    [source_pubkey]: {
      ...sourceCache,
      [entity_id]: {
        ...(sourceCache[entity_id] || {}),
        ...attributes
      }
    }
  };
}

/**
 * Remove cached attribute
 */
export function removeAttributeCachePure(
  currentCache: AttributeCache,
  source_pubkey: string,
  entity_id: EntityId,
  attribute_type: AttributeType
): AttributeCache {
  const sourceCache = currentCache[source_pubkey];
  if (!sourceCache || !sourceCache[entity_id]) {
    return currentCache;
  }
  
  const { [attribute_type]: removed, ...remainingAttrs } = sourceCache[entity_id];
  
  return {
    ...currentCache,
    [source_pubkey]: {
      ...sourceCache,
      [entity_id]: remainingAttrs
    }
  };
}

// ═══════════════════════════════════════════════════════════════════
// RESOLUTION LOGIC (Pure Functions)
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolve entity identifier to pubkey
 * 
 * @param entity_id - Entity identifier (pubkey, contact_id, or org_id)
 * @param contactsMap - Map of contact_id -> Contact
 * @param orgsMap - Map of org_id -> Organization
 * @returns Resolved pubkey or undefined
 */
export function resolveEntityToPubkeyPure(
  entity_id: EntityId,
  contactsMap: Record<string, { public_key?: string }>,
  orgsMap: Record<string, { designated_pubkey?: string }>
): string | undefined {
  // Already a pubkey (doesn't start with contact_ or org_)
  if (!entity_id.startsWith('contact_') && !entity_id.startsWith('org_')) {
    return entity_id;
  }
  
  // Contact ID
  if (entity_id.startsWith('contact_')) {
    return contactsMap[entity_id]?.public_key;
  }
  
  // Organization ID
  if (entity_id.startsWith('org_')) {
    return orgsMap[entity_id]?.designated_pubkey;
  }
  
  return undefined;
}

/**
 * Get attribute value with full resolution logic
 * 
 * Resolution order:
 * 1. My declaration (what I declare about this entity's attribute)
 * 2. Explicit subscription → cached value from source
 * 3. Auto-subscribe to entity's own data (if resolvable to pubkey)
 * 4. Otherwise undefined
 */
export function getEntityAttributePure(
  declarations: MyAttributeDeclarations | null | undefined,
  subscriptions: AttributeSubscriptions | null | undefined,
  cache: AttributeCache,
  entity_id: EntityId,
  attribute_type: AttributeType,
  contactsMap: Record<string, { public_key?: string }>,
  orgsMap: Record<string, { designated_pubkey?: string }>
): {
  value: AttributeValue | undefined;
  source: 'declared' | 'self' | string | undefined;
} {
  // 1. My declaration takes precedence
  if (declarations && declarations[entity_id]?.[attribute_type] !== undefined) {
    return {
      value: declarations[entity_id][attribute_type],
      source: 'declared'
    };
  }
  
  // 2. Check explicit subscription
  const subscribedSource = subscriptions?.[entity_id]?.[attribute_type];
  if (subscribedSource !== undefined) {
    if (subscribedSource === null) {
      // Explicit: subscribe to entity's own data
      const pubkey = resolveEntityToPubkeyPure(entity_id, contactsMap, orgsMap);
      if (pubkey && cache[pubkey]?.[entity_id]?.[attribute_type] !== undefined) {
        return {
          value: cache[pubkey][entity_id][attribute_type],
          source: 'self'
        };
      }
    } else {
      // Subscribe to specific source's declaration
      if (cache[subscribedSource]?.[entity_id]?.[attribute_type] !== undefined) {
        return {
          value: cache[subscribedSource][entity_id][attribute_type],
          source: subscribedSource
        };
      }
    }
  }
  
  // 3. Default: Auto-subscribe to entity's own data
  const pubkey = resolveEntityToPubkeyPure(entity_id, contactsMap, orgsMap);
  if (pubkey && cache[pubkey]?.[pubkey]?.[attribute_type] !== undefined) {
    return {
      value: cache[pubkey][pubkey][attribute_type],
      source: 'self'
    };
  }
  
  return {
    value: undefined,
    source: undefined
  };
}

/**
 * Get all attributes for an entity
 */
export function getEntityAttributesPure(
  declarations: MyAttributeDeclarations | null | undefined,
  subscriptions: AttributeSubscriptions | null | undefined,
  cache: AttributeCache,
  entity_id: EntityId,
  contactsMap: Record<string, { public_key?: string }>,
  orgsMap: Record<string, { designated_pubkey?: string }>
): EntityAttributes {
  const result: EntityAttributes = {};
  
  // Collect attribute types from all sources
  const attributeTypes = new Set<string>();
  
  // From declarations
  if (declarations && declarations[entity_id]) {
    Object.keys(declarations[entity_id]).forEach(type => attributeTypes.add(type));
  }
  
  // From subscriptions
  if (subscriptions && subscriptions[entity_id]) {
    Object.keys(subscriptions[entity_id]).forEach(type => attributeTypes.add(type));
  }
  
  // From cache (entity's own data)
  const pubkey = resolveEntityToPubkeyPure(entity_id, contactsMap, orgsMap);
  if (pubkey && cache[pubkey]?.[pubkey]) {
    Object.keys(cache[pubkey][pubkey]).forEach(type => attributeTypes.add(type));
  }
  
  // Resolve each attribute type
  for (const attributeType of attributeTypes) {
    const { value } = getEntityAttributePure(
      declarations,
      subscriptions,
      cache,
      entity_id,
      attributeType,
      contactsMap,
      orgsMap
    );
    
    if (value !== undefined) {
      result[attributeType] = value;
    }
  }
  
  return result;
}

/**
 * Get all entities that have a specific attribute
 */
export function getEntitiesWithAttributePure(
  declarations: MyAttributeDeclarations | null | undefined,
  subscriptions: AttributeSubscriptions | null | undefined,
  cache: AttributeCache,
  attribute_type: AttributeType,
  contactsMap: Record<string, { public_key?: string }>,
  orgsMap: Record<string, { designated_pubkey?: string }>
): EntityId[] {
  const entities = new Set<EntityId>();
  
  // From declarations
  if (declarations) {
    for (const [entity_id, attrs] of Object.entries(declarations)) {
      if (attrs[attribute_type] !== undefined) {
        entities.add(entity_id);
      }
    }
  }
  
  // From subscriptions
  if (subscriptions) {
    for (const [entity_id, subs] of Object.entries(subscriptions)) {
      if (subs[attribute_type] !== undefined) {
        entities.add(entity_id);
      }
    }
  }
  
  // From cache (self-published data)
  for (const [source_pubkey, sourceCache] of Object.entries(cache)) {
    for (const [entity_id, attrs] of Object.entries(sourceCache)) {
      if (attrs[attribute_type] !== undefined) {
        entities.add(entity_id);
      }
    }
  }
  
  return Array.from(entities);
}
```

### File: `src/lib/network/attributes.svelte.ts`

Svelte store integration:

```typescript
/**
 * Entity Attribute Recognition - Svelte Store Integration
 * 
 * Manages entity attribute recognition with local-first caching pattern.
 * Uses createStore() from store.svelte.ts for Holster persistence and sync.
 */

import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import type {
  EntityId,
  AttributeType,
  AttributeValue,
  MyAttributeDeclarations,
  AttributeSubscriptions,
  AttributeCache,
  EntityAttributes
} from '$lib/protocol/schemas';
import {
  MyAttributeDeclarationsSchema,
  AttributeSubscriptionsSchema
} from '$lib/protocol/schemas';

// Import pure functions
import {
  setEntityAttributePure,
  removeEntityAttributePure,
  removeEntityPure,
  subscribeToAttributePure,
  unsubscribeFromAttributePure,
  updateAttributeCachePure,
  updateEntityCachePure,
  removeAttributeCachePure,
  getEntityAttributePure,
  getEntityAttributesPure,
  getEntitiesWithAttributePure,
  resolveEntityToPubkeyPure
} from './attributes';

// Import entity registries
import { userContacts } from './users.svelte';
import { holsterOrganizations } from './organizations.svelte';

// Re-export pure functions for external use
export * from './attributes';

// ═══════════════════════════════════════════════════════════════════
// ATTRIBUTE STORES (Holster-backed via createStore)
// ═══════════════════════════════════════════════════════════════════

/**
 * My Attribute Declarations
 * 
 * What I declare about entities (including myself).
 * Maps entity_id -> attribute_type -> value
 */
export const myAttributeDeclarations = createStore({
  holsterPath: 'attributes/entities',
  schema: MyAttributeDeclarationsSchema,
  persistDebounce: 200
});

/**
 * Attribute Subscriptions
 * 
 * Who I subscribe to for which entity's which attributes.
 * Maps entity_id -> attribute_type -> source_pubkey | null
 */
export const attributeSubscriptions = createStore({
  holsterPath: 'attribute-subscriptions',
  schema: AttributeSubscriptionsSchema,
  persistDebounce: 200
});

/**
 * Attribute Cache (Local-First Pattern)
 * 
 * Cached attribute data from network sources.
 * Structure: source_pubkey -> entity_id -> attribute_type -> value
 * 
 * In-memory only, rebuilt from network on load.
 */
export const attributeCache: Writable<AttributeCache> = writable({});

// ═══════════════════════════════════════════════════════════════════
// LIFECYCLE FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize attribute stores
 */
export function initializeAttributes() {
  myAttributeDeclarations.initialize();
  attributeSubscriptions.initialize();
  console.log('[ATTRIBUTES] Initialized stores');
}

/**
 * Cleanup attribute stores
 */
export async function cleanupAttributes() {
  await myAttributeDeclarations.cleanup();
  await attributeSubscriptions.cleanup();
  attributeCache.set({});
  console.log('[ATTRIBUTES] Cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// DECLARATION OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Set an attribute for an entity
 */
export function setEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType,
  value: AttributeValue
): void {
  const currentDeclarations = get(myAttributeDeclarations);
  const updatedDeclarations = setEntityAttributePure(
    currentDeclarations,
    entity_id,
    attribute_type,
    value
  );
  
  myAttributeDeclarations.set(updatedDeclarations);
  
  console.log(`[ATTRIBUTES] Set ${attribute_type} for ${entity_id}`);
}

/**
 * Remove an attribute for an entity
 */
export function removeEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): void {
  const currentDeclarations = get(myAttributeDeclarations);
  const updatedDeclarations = removeEntityAttributePure(
    currentDeclarations,
    entity_id,
    attribute_type
  );
  
  myAttributeDeclarations.set(updatedDeclarations);
  
  console.log(`[ATTRIBUTES] Removed ${attribute_type} for ${entity_id}`);
}

/**
 * Remove all attributes for an entity
 */
export function removeEntity(entity_id: EntityId): void {
  const currentDeclarations = get(myAttributeDeclarations);
  const updatedDeclarations = removeEntityPure(currentDeclarations, entity_id);
  
  myAttributeDeclarations.set(updatedDeclarations);
  
  console.log(`[ATTRIBUTES] Removed all attributes for ${entity_id}`);
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to an attribute from a source
 */
export function subscribeToAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType,
  source_pubkey: string | null  // null = entity's own
): void {
  const currentSubscriptions = get(attributeSubscriptions);
  const updatedSubscriptions = subscribeToAttributePure(
    currentSubscriptions,
    entity_id,
    attribute_type,
    source_pubkey
  );
  
  attributeSubscriptions.set(updatedSubscriptions);
  
  const sourceName = source_pubkey === null ? 'self' : source_pubkey.slice(0, 20) + '...';
  console.log(`[ATTRIBUTES] Subscribed to ${entity_id}.${attribute_type} from ${sourceName}`);
}

/**
 * Unsubscribe from an attribute
 */
export function unsubscribeFromAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): void {
  const currentSubscriptions = get(attributeSubscriptions);
  const currentCache = get(attributeCache);
  
  const { subscriptions, cache } = unsubscribeFromAttributePure(
    currentSubscriptions,
    currentCache,
    entity_id,
    attribute_type
  );
  
  attributeSubscriptions.set(subscriptions);
  attributeCache.set(cache);
  
  console.log(`[ATTRIBUTES] Unsubscribed from ${entity_id}.${attribute_type}`);
}

// ═══════════════════════════════════════════════════════════════════
// RESOLUTION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolve entity identifier to pubkey
 */
export function resolveEntityToPubkey(entity_id: EntityId): string | undefined {
  const contacts = get(userContacts) || {};
  const orgs = get(holsterOrganizations) || {};
  return resolveEntityToPubkeyPure(entity_id, contacts, orgs);
}

/**
 * Get an attribute value for an entity (with resolution)
 */
export function getEntityAttribute(
  entity_id: EntityId,
  attribute_type: AttributeType
): AttributeValue | undefined {
  const declarations = get(myAttributeDeclarations);
  const subscriptions = get(attributeSubscriptions);
  const cache = get(attributeCache);
  const contacts = get(userContacts) || {};
  const orgs = get(holsterOrganizations) || {};
  
  const { value } = getEntityAttributePure(
    declarations,
    subscriptions,
    cache,
    entity_id,
    attribute_type,
    contacts,
    orgs
  );
  
  return value;
}

/**
 * Get attribute source for an entity
 */
export function getAttributeSource(
  entity_id: EntityId,
  attribute_type: AttributeType
): 'declared' | 'self' | string | undefined {
  const declarations = get(myAttributeDeclarations);
  const subscriptions = get(attributeSubscriptions);
  const cache = get(attributeCache);
  const contacts = get(userContacts) || {};
  const orgs = get(holsterOrganizations) || {};
  
  const { source } = getEntityAttributePure(
    declarations,
    subscriptions,
    cache,
    entity_id,
    attribute_type,
    contacts,
    orgs
  );
  
  return source;
}

/**
 * Get all attributes for an entity
 */
export function getEntityAttributes(entity_id: EntityId): EntityAttributes {
  const declarations = get(myAttributeDeclarations);
  const subscriptions = get(attributeSubscriptions);
  const cache = get(attributeCache);
  const contacts = get(userContacts) || {};
  const orgs = get(holsterOrganizations) || {};
  
  return getEntityAttributesPure(
    declarations,
    subscriptions,
    cache,
    entity_id,
    contacts,
    orgs
  );
}

/**
 * Get all entities that have a specific attribute
 */
export function getEntitiesWithAttribute(attribute_type: AttributeType): EntityId[] {
  const declarations = get(myAttributeDeclarations);
  const subscriptions = get(attributeSubscriptions);
  const cache = get(attributeCache);
  const contacts = get(userContacts) || {};
  const orgs = get(holsterOrganizations) || {};
  
  return getEntitiesWithAttributePure(
    declarations,
    subscriptions,
    cache,
    attribute_type,
    contacts,
    orgs
  );
}

// ═══════════════════════════════════════════════════════════════════
// CACHE MANAGEMENT (Called by auto-sync system)
// ═══════════════════════════════════════════════════════════════════

/**
 * Update cache with attribute data from a source
 */
export function updateAttributeCache(
  source_pubkey: string,
  entity_id: EntityId,
  attribute_type: AttributeType,
  value: AttributeValue
): void {
  const currentCache = get(attributeCache);
  const updatedCache = updateAttributeCachePure(
    currentCache,
    source_pubkey,
    entity_id,
    attribute_type,
    value
  );
  
  attributeCache.set(updatedCache);
  
  console.log(
    `[ATTRIBUTES] Cached ${attribute_type} for ${entity_id} from ${source_pubkey.slice(0, 20)}...`
  );
}

/**
 * Update cache with multiple attributes from a source
 */
export function updateEntityCache(
  source_pubkey: string,
  entity_id: EntityId,
  attributes: EntityAttributes
): void {
  const currentCache = get(attributeCache);
  const updatedCache = updateEntityCachePure(
    currentCache,
    source_pubkey,
    entity_id,
    attributes
  );
  
  attributeCache.set(updatedCache);
  
  console.log(
    `[ATTRIBUTES] Cached ${Object.keys(attributes).length} attributes for ${entity_id} from ${source_pubkey.slice(0, 20)}...`
  );
}
```

This implementation provides a complete, working system for unified entity attribute recognition. The key design principles are:

1. **Separation of concerns**: Pure functions in `attributes.ts`, Svelte integration in `attributes.svelte.ts`
2. **Local-first**: Declarations and subscriptions persisted via Holster, cache rebuilt from network
3. **Flexible resolution**: Declarations > Subscriptions > Auto-subscribe to self
4. **Type-safe**: Schema validation at runtime, TypeScript types at compile time
5. **Extensible**: Easy to add new attribute types without changing core system

Next steps would be to create adapter layers in the existing `membership.svelte.ts` and `capacity-subscriptions.svelte.ts` to delegate to this new unified system while maintaining backward compatibility.
