# Entity Attribute Recognition - Visual Architecture

## System Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    ENTITY ATTRIBUTE RECOGNITION                  │
│                         Unified Framework                        │
└─────────────────────────────────────────────────────────────────┘

┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│   Entities   │    │  Attributes  │    │   Sources    │
├──────────────┤    ├──────────────┤    ├──────────────┤
│ • Users      │    │ • Membership │    │ • Self       │
│   (pubkeys)  │    │ • Capacity   │    │ • Declared   │
│ • Contacts   │    │ • Needs      │    │ • Subscribed │
│   (IDs)      │    │ • Skills     │    │              │
│ • Orgs       │    │ • Location   │    │              │
│   (org_ids)  │    │ • (Custom)   │    │              │
└──────────────┘    └──────────────┘    └──────────────┘
       │                    │                    │
       └────────────────────┴────────────────────┘
                            │
                   ┌────────▼────────┐
                   │  (entity_id,    │
                   │   attribute,    │
                   │   source)       │
                   │  → value        │
                   └─────────────────┘
```

## Data Flow Architecture

### Declaration Flow

```
User Action: "I declare Alice has skills: ['medicine', 'trauma']"
     │
     ▼
┌─────────────────────────────────────────────────┐
│ setEntityAttribute(                              │
│   'alice_pub',                                   │
│   'skills',                                      │
│   ['medicine', 'trauma']                         │
│ )                                                │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│  myAttributeDeclarations                         │
│  {                                               │
│    "alice_pub": {                                │
│      "skills": ["medicine", "trauma"]            │
│    }                                             │
│  }                                               │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│  Holster: <my_pubkey>/attributes/entities        │
│  ✓ Persisted                                     │
│  ✓ Synced                                        │
└─────────────────────────────────────────────────┘
```

### Subscription Flow

```
User Action: "Subscribe to Bob's view of Alice's skills"
     │
     ▼
┌─────────────────────────────────────────────────┐
│ subscribeToAttribute(                            │
│   'alice_pub',                                   │
│   'skills',                                      │
│   'bob_pub'                                      │
│ )                                                │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│  attributeSubscriptions                          │
│  {                                               │
│    "alice_pub": {                                │
│      "skills": "bob_pub"                         │
│    }                                             │
│  }                                               │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│  Subscribe to Bob's Holster:                     │
│  <bob_pub>/attributes/entities                   │
│                                                  │
│  When data arrives:                              │
│  attributeCache["bob_pub"]["alice_pub"]         │
│                ["skills"] = [...]               │
└─────────────────────────────────────────────────┘
```

### Resolution Flow

```
Query: getEntityAttribute('alice_pub', 'skills')
     │
     ▼
┌─────────────────────────────────────────────────┐
│ Step 1: Check My Declarations                    │
│ myAttributeDeclarations["alice_pub"]["skills"]? │
├─────────────────────────────────────────────────┤
│ ✓ Found → Return (source: 'declared')           │
│ ✗ Not found → Continue                           │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│ Step 2: Check Subscriptions                      │
│ source = attributeSubscriptions                  │
│          ["alice_pub"]["skills"]                │
├─────────────────────────────────────────────────┤
│ • source = "bob_pub"                             │
│   → Return cache["bob_pub"]["alice_pub"]        │
│              ["skills"]                          │
│   (source: 'bob_pub')                            │
│                                                  │
│ • source = null                                  │
│   → Return cache["alice_pub"]["alice_pub"]      │
│              ["skills"]                          │
│   (source: 'self')                               │
│                                                  │
│ • source undefined → Continue                    │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│ Step 3: Auto-Subscribe to Self                   │
│ pubkey = resolveEntityToPubkey('alice_pub')     │
│        = 'alice_pub'                             │
│                                                  │
│ Return cache["alice_pub"]["alice_pub"]          │
│             ["skills"]                           │
│ (source: 'self')                                 │
└──────────────┬──────────────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────────────┐
│ Step 4: Not Found                                │
│ Return undefined                                 │
└─────────────────────────────────────────────────┘
```

## Entity Resolution

```
Entity ID Resolution: How IDs map to Pubkeys

┌──────────────────────┐
│  Entity Identifier   │
│  (entity_id)         │
└──────────┬───────────┘
           │
           ▼
    ┌──────────────┐
    │ What type?   │
    └──┬─────┬─────┘
       │     │
   ┌───┴─┐ ┌─┴────┐ ┌──────────┐
   │pubkey│ │contact│ │ org_id   │
   │      │ │  _id  │ │          │
   └───┬──┘ └───┬───┘ └────┬─────┘
       │        │           │
       │        ▼           ▼
       │   ┌──────────┐ ┌──────────┐
       │   │ contacts │ │   orgs   │
       │   │   [id]   │ │   [id]   │
       │   │ .pubkey? │ │ .designated│
       │   └────┬─────┘ │  _pubkey? │
       │        │       └────┬──────┘
       │        │            │
       └────────┴────────────┘
                │
                ▼
         ┌──────────────┐
         │ Resolved     │
         │ Pubkey       │
         │ (or undef)   │
         └──────────────┘
```

## Data Storage Structure

### Holster Layout

```
<my_pubkey>/
├── attributes/
│   └── entities/                          # MyAttributeDeclarations
│       ├── <my_pubkey>/
│       │   ├── capacity: [...]
│       │   ├── needs: [...]
│       │   └── skills: [...]
│       ├── org_redcross/
│       │   ├── membership: [...]
│       │   └── location: {...}
│       └── alice_pub/
│           └── skills: [...]
│
├── attribute-subscriptions/               # AttributeSubscriptions
│   ├── org_unicef/
│   │   └── membership: "carol_pub"
│   └── alice_pub/
│       ├── capacity: null                 # null = subscribe to Alice's own
│       └── skills: "bob_pub"              # subscribe to Bob's view
│
├── contacts/                              # Contact ID mappings
│   ├── contact_123/
│   │   ├── name: "Alice"
│   │   └── public_key: "alice_pub"
│   └── contact_456/
│       └── name: "Bob" (no pubkey)
│
└── organizations/                         # Organization mappings
    ├── org_redcross/
    │   ├── names: {...}
    │   └── designated_pubkey: "..."
    └── org_unicef/
        └── names: {...}
```

### In-Memory Cache

```
attributeCache: {
  // From Alice's Holster: <alice_pub>/attributes/entities
  "alice_pub": {
    "alice_pub": {
      "capacity": [...],      // Alice's self-declared capacity
      "needs": [...],         // Alice's self-declared needs
      "skills": [...]         // Alice's self-declared skills
    },
    "bob_pub": {
      "skills": [...]         // Alice's view of Bob's skills
    }
  },
  
  // From Bob's Holster: <bob_pub>/attributes/entities
  "bob_pub": {
    "bob_pub": {
      "capacity": [...],      // Bob's self-declared capacity
    },
    "alice_pub": {
      "skills": [...]         // Bob's view of Alice's skills (different!)
    },
    "org_redcross": {
      "membership": [...]     // Bob's view of RedCross membership
    }
  },
  
  // From Carol's Holster: <carol_pub>/attributes/entities
  "carol_pub": {
    "carol_pub": {
      "needs": [...]          // Carol's self-declared needs
    },
    "org_unicef": {
      "membership": [...]     // Carol's view of UNICEF membership
    }
  }
}
```

## Use Case Examples

### Example 1: Self-Declaration with Auto-Subscribe

```
┌─────────────┐
│   Alice     │
│  (alice_pub)│
└──────┬──────┘
       │
       │ setEntityAttribute(alice_pub, 'capacity', [...])
       │
       ▼
┌─────────────────────────────────────┐
│ Holster: alice_pub/                  │
│   attributes/entities/               │
│     alice_pub/                       │
│       capacity: [...]                │
└─────────────────────────────────────┘
       │
       │ Network sync
       │
       ▼
┌─────────────┐
│   Bob       │
│  (bob_pub)  │
└──────┬──────┘
       │
       │ getEntityAttribute(alice_pub, 'capacity')
       │ → No declaration
       │ → No subscription
       │ → Auto-subscribe to Alice's own
       │
       ▼
┌─────────────────────────────────────┐
│ Bob's Cache:                         │
│   cache[alice_pub][alice_pub]       │
│        [capacity] = [...]            │
│                                      │
│ Returns: Alice's capacity            │
│ Source: 'self'                       │
└─────────────────────────────────────┘
```

### Example 2: Declared Membership with Subscription

```
┌─────────────┐
│   Alice     │
│  (alice_pub)│
└──────┬──────┘
       │
       │ setEntityAttribute(org_redcross, 'membership', [bob, carol, ...])
       │
       ▼
┌─────────────────────────────────────┐
│ Holster: alice_pub/                  │
│   attributes/entities/               │
│     org_redcross/                    │
│       membership: [bob, carol, ...]  │
└─────────────────────────────────────┘
       │
       │ Network sync
       │
       ▼
┌─────────────┐
│   Bob       │
│  (bob_pub)  │
└──────┬──────┘
       │
       │ subscribeToAttribute(org_redcross, 'membership', alice_pub)
       │
       ▼
┌─────────────────────────────────────┐
│ Bob's subscription:                  │
│   attributeSubscriptions            │
│     [org_redcross][membership]      │
│       = alice_pub                    │
└─────────────────────────────────────┘
       │
       │ Subscribe to Alice's Holster
       │
       ▼
┌─────────────────────────────────────┐
│ Bob's Cache:                         │
│   cache[alice_pub][org_redcross]    │
│        [membership] = [bob, carol...]│
│                                      │
│ getEntityAttribute(org_redcross, membership)│
│ Returns: [bob, carol, ...]           │
│ Source: 'alice_pub'                  │
└─────────────────────────────────────┘
```

### Example 3: Mixed Self + Others Recognition

```
┌─────────────┐
│   Alice     │
│  (alice_pub)│
└──────┬──────┘
       │
       │ My declarations:
       │   setEntityAttribute(alice_pub, 'capacity', [food: 100])
       │   setEntityAttribute(bob_pub, 'skills', ['medicine'])
       │
       │ My subscriptions:
       │   subscribeToAttribute(carol_pub, 'capacity', null)  # Carol's own
       │   subscribeToAttribute(bob_pub, 'needs', null)       # Bob's own
       │
       ▼
┌──────────────────────────────────────────────────────┐
│ Alice's perspective:                                  │
│                                                       │
│ getEntityAttribute(alice_pub, 'capacity')            │
│ → Returns: [food: 100] (source: 'declared')          │
│                                                       │
│ getEntityAttribute(bob_pub, 'skills')                │
│ → Returns: ['medicine'] (source: 'declared')         │
│                                                       │
│ getEntityAttribute(carol_pub, 'capacity')            │
│ → Returns: [...from Carol...] (source: 'self')       │
│                                                       │
│ getEntityAttribute(bob_pub, 'needs')                 │
│ → Returns: [...from Bob...] (source: 'self')         │
│                                                       │
│ getEntityAttribute(bob_pub, 'capacity')              │
│ → Returns: [...from Bob...] (source: 'self')         │
│   (no declaration, no subscription → auto-subscribe) │
└──────────────────────────────────────────────────────┘
```

## Comparison: Old vs New

### Old System (Specialized)

```
┌──────────────────────────────────────────────┐
│           Organization Membership             │
│                                               │
│  myMembershipLists[org_id] = [members]       │
│  membershipSubscriptions[org_id] = source    │
│  membershipCache[source][org_id] = [members] │
└──────────────────────────────────────────────┘

┌──────────────────────────────────────────────┐
│               Slot Subscriptions              │
│                                               │
│  slotSubscriptions[pubkey] = {cap, need}     │
│  capacityCache[pubkey] = [slots]             │
│  needCache[pubkey] = [slots]                 │
└──────────────────────────────────────────────┘

❌ Separate systems for each attribute type
❌ Different APIs for membership vs slots
❌ No unified pattern for adding new attributes
❌ Asymmetric: Can't recognize others' capacities
```

### New System (Unified)

```
┌──────────────────────────────────────────────────────┐
│           Entity Attribute Recognition                │
│                                                       │
│  myAttributeDeclarations[entity][attribute] = value  │
│  attributeSubscriptions[entity][attribute] = source  │
│  attributeCache[source][entity][attribute] = value   │
└──────────────────────────────────────────────────────┘

✅ Single system for all attribute types
✅ Consistent API for everything
✅ Trivial to add new attributes
✅ Symmetric: Recognize any attribute of any entity
✅ Flexible: Self-declaration + other-recognition + subscription
✅ Clear: Always know the source of data
```

## Extension Example: Adding "Skills" Attribute

### Old System
```
❌ Would need:
   - New skills.svelte.ts file
   - New skillsCache store
   - New skillSubscriptions store
   - New resolution logic
   - Update every consumer manually
   - 300+ lines of code
```

### New System
```
✅ Just use it:

// Declare my skills
setEntityAttribute(myPubkey, 'skills', [
  'programming', 'design', 'facilitation'
]);

// Recognize Alice's skills
setEntityAttribute('alice_pub', 'skills', [
  'medicine', 'trauma_care'
]);

// Subscribe to Bob's view of Carol's skills
subscribeToAttribute('carol_pub', 'skills', 'bob_pub');

// Query anyone's skills
const skills = getEntityAttribute(entity_id, 'skills');

// Zero new code needed!
```

## Summary

The unified Entity-Attribute-Recognition framework provides:

1. **One Pattern**: `(entity_id, attribute_type, source) → value`
2. **Three Modes**: Self-declaration, Other-recognition, Subscription
3. **Clear Resolution**: Declared > Subscribed > Self > Undefined
4. **Full Symmetry**: Same API for all entities and attributes
5. **Easy Extension**: Add new attributes with zero new code
6. **Transparent Source**: Always know where data comes from

This creates a powerful foundation for rich, multi-faceted recognition networks beyond simple org membership.
