# Attribute Recognition System

A generalized attribute recognition framework that extends beyond organization membership to support recognizing any attribute of any entity (users, organizations, contacts).

## Overview

The attribute recognition system enables:
- **Generalized Recognition**: Recognize any attribute (membership, capacities, needs, skills, location, etc.)
- **Flexible Entities**: Apply to users, organizations, or contacts via pubkey/uuid/contact_id
- **Subscription Model**: Subscribe to specific sources for entity attributes
- **Resolution Priority**: `specified_source → entity's_pubkey → our_local_recognition`
- **ITC Causality**: Built-in conflict resolution with Interval Tree Clocks
- **Backward Compatible**: Seamlessly migrates existing org membership system

## Architecture

### Core Modules

1. **`attribute-recognition.ts`** - Pure functions for recognizing and managing attributes
2. **`attribute-types.ts`** - Type-specific helpers and validation
3. **`attribute-recognition.svelte.ts`** - Svelte store integration with unified storage
4. **`index.ts`** - Public API exports

### Data Flow (Unified Storage)

```
┌─────────────────────────────────────────────────────────────┐
│     My Attribute Recognitions (UNIFIED STORAGE)            │
│     - Local recognitions (source_pubkey = undefined)       │
│     - Subscription data (source_pubkey = source)           │
│     - ITC conflict resolution (like stores.svelte.ts)      │
│     - Stored: attributes/recognitions                      │
│     - Per-attribute ITC + collection-level ITC             │
└─────────────────┬───────────────────────────────────────────┘
                  │
                  ├─────────────────────────────┐
                  │                             │
                  ▼                             ▼
┌─────────────────────────────┐  ┌─────────────────────────────┐
│  My Subscriptions (Config)  │  │  My ID Mappings (Local)     │
│  - Who to subscribe to      │  │  - uuid/contact → pubkey    │
│  - Triggers data fetch      │  │  - Stored: attributes/...   │
│  - Stored: attributes/...   │  └─────────────────────────────┘
└─────────────────────────────┘

SUBSCRIPTION FLOW:
1. Configure subscription → myAttributeSubscriptions
2. Auto-subscribe to source's Holster path
3. Their data arrives → ITC check
4. If causally newer → write to myAttributeRecognitions
5. Resolution reads from unified store!

KEY INSIGHT: Like others_recognition_of_me in myCommitmentStore!
```

### Resolution (Simplified with Unified Storage)

**ONE SOURCE**: Just read from `myAttributeRecognitions`!

Resolution type determined by:
- **`source_pubkey` present + matches subscription** → `'subscription'`
- **`source_pubkey` present + matches entity_id** → `'self'`  
- **`source_pubkey` absent (undefined)** → `'local'`
- **Attribute not found** → `'not_found'`

**ITC Handles Priority Automatically:**
- Subscription data written with ITC checking
- Manual edits can "win" if causally newer
- No need for complex priority logic!

## Usage Examples

### Recognize Organization Membership

```typescript
import { myAttributeRecognitions } from '$lib/protocol/attributes/attribute-recognition.svelte';
import { updateAttributeInCollection } from '$lib/protocol/attributes';

// Recognize org membership
myAttributeRecognitions.update($recognitions => {
  return updateAttributeInCollection(
    $recognitions,
    'org_abc123',
    'membership',
    ['pubkey_alice', 'pubkey_bob', 'org_nested']
  );
});
```

### Subscribe to Another User's View

```typescript
import { myAttributeSubscriptions } from '$lib/protocol/attributes/attribute-recognition.svelte';
import { subscribeToAttribute } from '$lib/protocol/attributes';

// Subscribe to Alice's view of org membership
myAttributeSubscriptions.update($subs => {
  return subscribeToAttribute($subs, 'org_abc123', 'membership', 'pubkey_alice');
});
```

### Recognize Capacity/Need

```typescript
// Recognize someone's capacity for food provision
myAttributeRecognitions.update($recognitions => {
  return updateAttributeInCollection(
    $recognitions,
    'pubkey_bob',
    'capacity:food',
    [
      { id: 'slot1', quantity: 100, need_type_id: 'food', unit: 'meals/week' }
    ]
  );
});

// Recognize our own need for housing
myAttributeRecognitions.update($recognitions => {
  return updateAttributeInCollection(
    $recognitions,
    myPubkey, // Our own pubkey
    'need:housing',
    [
      { id: 'need1', quantity: 1, need_type_id: 'housing', unit: 'apartment' }
    ]
  );
});
```

### Resolve Attribute with Provenance

```typescript
import { resolveAttribute } from '$lib/protocol/attributes/attribute-recognition.svelte';

// Get membership with full provenance
const result = resolveAttribute('org_abc123', 'membership');

console.log(result.value); // ["pubkey_alice", "pubkey_bob"]
console.log(result.resolution_type); // "subscription" | "self" | "local"
console.log(result.source_pubkey); // Who provided this data
console.log(result.confidence); // 0-1
```

### Reactive Attribute Store

```typescript
import { createAttributeStore } from '$lib/protocol/attributes/attribute-recognition.svelte';

// Create reactive store that auto-updates
const orgMembers = createAttributeStore('org_abc123', 'membership');

// Use in Svelte component
{#if $orgMembers}
  <ul>
    {#each $orgMembers as member}
      <li>{member}</li>
    {/each}
  </ul>
{/if}
```

## Attribute Types

### Built-in Types

#### 1. Membership
- **Name**: `"membership"`
- **Value**: `string[]` - Array of member IDs (pubkeys, org_ids, contact_ids)
- **Example**:
  ```typescript
  { value: ["pubkey_alice", "pubkey_bob", "org_nested"] }
  ```

#### 2. Capacity
- **Name**: `"capacity:{type}"` (e.g., `"capacity:food"`)
- **Value**: `AvailabilitySlot[]` - Array of capacity slots
- **Example**:
  ```typescript
  {
    value: [
      { id: "slot1", quantity: 100, need_type_id: "food", unit: "meals/week" }
    ]
  }
  ```

#### 3. Need
- **Name**: `"need:{type}"` (e.g., `"need:housing"`)
- **Value**: `NeedSlot[]` - Array of need slots
- **Example**:
  ```typescript
  {
    value: [
      { id: "need1", quantity: 1, need_type_id: "housing", unit: "apartment" }
    ]
  }
  ```

#### 4. Skill
- **Name**: `"skill:{name}"` (e.g., `"skill:javascript"`)
- **Value**: `SkillValue` - Skill level with metadata
- **Example**:
  ```typescript
  {
    value: {
      level: 8, // 1-10
      years: 5,
      description: "Expert TypeScript developer",
      verified: true,
      endorsements: ["pubkey_alice", "pubkey_bob"]
    }
  }
  ```

#### 5. Location
- **Name**: `"location"`
- **Value**: `LocationValue` - Location with optional coordinates
- **Example**:
  ```typescript
  {
    value: {
      city: "Berlin",
      country: "Germany",
      coords: [52.5200, 13.4050],
      online: false
    }
  }
  ```

### Custom Attributes

The system is fully extensible - any attribute name and value type is supported:

```typescript
// Custom attribute: project participation
myAttributeRecognitions.update($recognitions => {
  return updateAttributeInCollection(
    $recognitions,
    'pubkey_alice',
    'project:quantum-computing',
    { role: 'lead', hours_per_week: 20, since: '2024-01-01' }
  );
});
```

## Storage Structure

### AttributeRecognitionsCollection

```typescript
{
  "org_abc123": {
    "membership": {
      value: ["pubkey_alice", "pubkey_bob"],
      confidence: 1.0,
      timestamp: 1234567890,
      itcStamp: { ... }  // ITC causality
    },
    "capacity:food": {
      value: [...slots...],
      source_pubkey: "pubkey_alice",
      confidence: 0.9,
      timestamp: 1234567891,
      itcStamp: { ... }
    }
  },
  "pubkey_bob": {
    "need:housing": {
      value: {...},
      confidence: 1.0,
      timestamp: 1234567892,
      itcStamp: { ... }
    }
  },
  _itcStamp: { ... },  // Collection-level ITC
  _timestamp: 1234567893
}
```

### AttributeSubscriptions

```typescript
{
  "org_abc123": {
    "membership": "pubkey_alice"  // Subscribe to Alice's view
  },
  "pubkey_bob": {
    "capacity:food": "pubkey_bob"  // Subscribe to Bob's self-declaration
  }
}
```

### EntityIdMappings

```typescript
{
  "contact_alice_123": "pubkey_abc...",  // Local contact → pubkey
  "uuid_def_456": "pubkey_xyz..."        // UUID → pubkey
}
```

## Pure Implementation

The system has been fully migrated to a pure attribute-based implementation. No legacy compatibility layer remains.

**Direct usage:**

```typescript
import { setEntityAttribute, getEntityAttribute } from '$lib/network/entities.svelte';

// Set organization membership (entity with 'membership' attribute)
setEntityAttribute('org_abc123', 'membership', ['alice_pub', 'bob_pub']);

// Get organization membership
const members = getEntityAttribute('org_abc123', 'membership');
```

## Initialization

Enable the attribute system after authentication:

```typescript
import {
  initializeAttributeStores,
  enableAutoAttributeSync
} from '$lib/protocol/attributes/attribute-recognition.svelte';

// Initialize stores (after holster auth)
initializeAttributeStores();

// Enable auto-sync (subscribes based on myAttributeSubscriptions)
const unsubscribe = enableAutoAttributeSync();

// Later: cleanup on logout
// unsubscribe();
// cleanupAttributeStores();
```

## Testing

Comprehensive test suite covers:
- ✅ Core recognition functions (34 tests)
- ✅ Unified storage with ITC conflict resolution
- ✅ Type-specific parsing and validation
- ✅ Subscription management
- ✅ Entity ID resolution
- ✅ Collection merging
- ✅ Collection merging
- ✅ ITC causality tracking

Run tests:

```bash
bun test src/lib/protocol/attributes/attribute-recognition.test.ts
```

## Design Principles

### 1. Fully General (1a)
No predefined attribute types - extensible for any use case.

### 2. Priority-Based Resolution (2b)
Clear resolution order: `subscription → self → manual → not_found`

### 3. Flat Storage (3a)
Nested structure: `{entity_id: {attribute_name: {value, ...}}}`

### 4. Migration-Friendly (4a)
Org membership becomes one attribute type among many.

### 5. ITC Causality
Per-attribute and collection-level ITC for conflict resolution.

### 6. No $effect
Uses `store.subscribe()` pattern for auto-syncing (best practice).

### 7. Provenance Tracking
Every attribute tracks source_pubkey and resolution_type.

## Benefits

1. **Generalization**: Recognize any attribute, not just membership
2. **Flexibility**: Subscribe to different sources per attribute
3. **Transparency**: Track data provenance (who said what)
4. **Extensibility**: Easy to add new attribute types
5. **Pure Implementation**: No legacy compatibility layers
6. **Self-Sovereign**: Recognize own attributes without needing a pubkey
7. **Federation**: Subscribe to others' perspectives on entities

## API Reference

See individual module files for detailed API documentation:
- [`attribute-recognition.ts`](./attribute-recognition.ts) - Pure functions for attribute management
- [`attribute-types.ts`](./attribute-types.ts) - Type-specific helpers and validation
- [`attribute-recognition.svelte.ts`](./attribute-recognition.svelte.ts) - Svelte stores with unified storage
- [`index.ts`](./index.ts) - Public API exports

See also:
- [`entities.svelte.ts`](../../network/entities.svelte.ts) - Unified entity/attribute API

## Future Enhancements

- [ ] Network-wide attribute discovery protocol
- [ ] Attribute endorsement system (verifiable claims)
- [ ] Time-based attribute validity (expiration)
- [ ] Attribute change notifications
- [ ] Query language for complex attribute filters
- [ ] Attribute schemas with validation rules
- [ ] Multi-source aggregation (beyond priority)

