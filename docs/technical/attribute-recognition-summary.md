# Entity Attribute Recognition - Executive Summary

## The Problem

The current system has separate, specialized subsystems for different types of recognition:

1. **Organization Membership** (`membership.svelte.ts`)
   - Tracks who belongs to which organizations
   - Supports declaring membership OR subscribing to others' declarations
   - Org-specific only

2. **Capacity/Need Subscriptions** (`capacity-subscriptions.svelte.ts`)
   - Tracks capacity and need slots
   - Self-declaration only (can't recognize others' capacities)
   - User-specific only

**Limitations**:
- Each new attribute type requires ~300+ lines of duplicate code
- No unified pattern for recognition
- Asymmetric: Can recognize org membership but not user capacities
- Can't express "my view of Alice's skills" or "Bob's view of Carol's reputation"
- Adding new attributes (skills, location, reputation) requires creating new subsystems

## The Solution: Unified Entity-Attribute Recognition

A single, symmetric framework for recognizing **any attribute** of **any entity** (users or orgs).

### Core Concept

Every piece of recognized data is a triple:
```
(entity_id, attribute_type, source_pubkey) → value
```

### Three Recognition Modes

1. **Self-Declaration**: "I declare my own capacity"
   ```typescript
   setEntityAttribute(myPubkey, 'capacity', [...]);
   ```

2. **Other-Recognition**: "I recognize Alice's skills"
   ```typescript
   setEntityAttribute('alice_pub', 'skills', ['medicine', 'trauma']);
   ```

3. **Subscription**: "I subscribe to Bob's view of Carol's skills"
   ```typescript
   subscribeToAttribute('carol_pub', 'skills', 'bob_pub');
   ```

### Resolution Order

When querying an attribute:
1. **My declaration** (what I say about this entity) takes precedence
2. **Subscription** (if I'm subscribed, use cached data from source)
3. **Auto-subscribe to self** (if entity has own data, use that)
4. Otherwise `undefined`

## Architecture

### Data Structures

```typescript
// What I declare about entities (including myself)
MyAttributeDeclarations: Record<entity_id, Record<attribute_type, value>>

// Who I subscribe to for which entity's which attributes
AttributeSubscriptions: Record<entity_id, Record<attribute_type, source_pubkey | null>>

// Cached data from network
AttributeCache: Record<source_pubkey, Record<entity_id, Record<attribute_type, value>>>
```

### Storage

```
<my_pubkey>/
├── attributes/entities/          # My declarations
│   ├── <my_pubkey>/
│   │   ├── capacity: [...]
│   │   ├── needs: [...]
│   │   └── skills: [...]
│   ├── org_redcross/
│   │   ├── membership: [...]
│   │   └── location: {...}
│   └── alice_pub/
│       └── skills: [...]
│
└── attribute-subscriptions/      # My subscriptions
    ├── org_unicef/
    │   └── membership: "carol_pub"
    └── alice_pub/
        └── capacity: null        # null = subscribe to Alice's own
```

## Key Benefits

### 1. Symmetry
Same pattern for all entities and attributes:
- Users can have membership in orgs
- Orgs can have membership (recursive)
- Both can have capacities, needs, skills, etc.

### 2. Flexibility
Three modes (self-declaration, other-recognition, subscription) available for everything.

### 3. Extensibility
Adding new attributes requires **zero new code**:

```typescript
// Skills (NEW!)
setEntityAttribute(myPubkey, 'skills', ['programming', 'design']);

// Location (NEW!)
setEntityAttribute('org_redcross', 'location', { lat: 40.7, lng: -74.0 });

// Reputation (NEW!)
setEntityAttribute('alice_pub', 'reputation', { reliability: 0.95 });
```

### 4. Transparency
Always know where data comes from:
```typescript
const { value, source } = getEntityAttribute('alice_pub', 'skills');
// source can be: 'declared', 'self', '<pubkey>', or undefined
```

### 5. Composability
- Organizations can be members of organizations
- Contact IDs resolve to pubkeys for subscriptions
- Clear separation between identity (who) and attributes (what)

## Use Cases Enabled

### Current (Maintained)
✅ Organization membership tracking  
✅ Capacity/need slot subscriptions

### New (Enabled)
✅ Skill recognition ("I recognize Alice's medical skills")  
✅ Multi-source reputation ("Subscribe to Bob's view of Carol")  
✅ Location tracking (users and orgs)  
✅ Contact information  
✅ Time availability  
✅ Custom attributes (extensible)

### Future (Easy to Add)
✅ Credentials/certifications  
✅ Language proficiencies  
✅ Equipment/resources  
✅ Project involvement  
✅ Conflict of interest declarations  
✅ Any domain-specific attribute

## Migration Path

### Phase 1: Foundation (Week 1)
- Add schemas to `schemas.ts`
- Create `attributes.ts` (pure functions)
- Create `attributes.svelte.ts` (Svelte stores)
- Initialize in app

### Phase 2: Backward-Compatible Adapters (Week 2)
- Update `membership.svelte.ts` to delegate to unified system
- Update `capacity-subscriptions.svelte.ts` to delegate
- Maintain old APIs via derived stores
- **Zero breaking changes** for existing code

### Phase 3: Migrate Consumers (Week 3-4)
- Update components one by one
- Use type-safe helpers for common patterns
- Test each component thoroughly

### Phase 4: Demonstrate Extensibility (Week 5)
- Add new attribute types (skills, location, reputation)
- Show how easy it is to extend

### Phase 5: Cleanup (Week 6)
- Mark old modules as deprecated
- Remove pure function duplicates
- Final documentation updates

### Rollback Safety
- Adapters ensure backward compatibility
- Feature flag for gradual rollout
- Can revert at any point

## Code Impact

### Before (Organization Membership Only)
```typescript
// membership.svelte.ts - 296 lines
// membership.ts - 420 lines
// Total: 716 lines for ONE attribute type

// To add skills: Need another ~700 lines
// To add location: Need another ~700 lines
// To add reputation: Need another ~700 lines
```

### After (Unified System)
```typescript
// attributes.ts - 350 lines (pure functions)
// attributes.svelte.ts - 200 lines (Svelte integration)
// Total: 550 lines for ALL attribute types

// To add skills: 0 new lines
// To add location: 0 new lines
// To add reputation: 0 new lines

// Just use it:
setEntityAttribute(entity_id, 'skills', [...]);
setEntityAttribute(entity_id, 'location', {...});
setEntityAttribute(entity_id, 'reputation', {...});
```

### ROI
- **Initial investment**: ~550 lines of unified code
- **Per-attribute savings**: ~700 lines
- **Break-even**: After 1 attribute type
- **Current system**: 2 attribute types (membership, slots)
- **Immediate savings**: ~850 lines
- **Future attributes**: Free

## Performance

### Memory
- Slight increase (~10%) due to unified cache
- Acceptable for flexibility gain

### Speed
- Negligible impact (one extra indirection level)
- Still O(1) lookup

### Network
- No change (can optimize later)

## Testing Strategy

- ✅ Unit tests for pure functions
- ✅ Integration tests for full roundtrip
- ✅ Backward compatibility tests for adapters
- ✅ Performance benchmarks
- ✅ Migration smoke tests

## Success Metrics

- ✅ All components migrated
- ✅ All tests passing
- ✅ Backward compatibility maintained
- ✅ At least one new attribute type added
- ✅ Zero production regressions
- ✅ Documentation complete

## Comparison: Current vs Unified

| Aspect | Current | Unified |
|--------|---------|---------|
| **Systems** | 2 separate (membership, slots) | 1 unified |
| **Code** | ~1400 lines (2 × 700) | ~550 lines |
| **Add new type** | ~700 lines | 0 lines |
| **API consistency** | Different per type | Same for all |
| **Self-declaration** | Mixed support | Full support |
| **Other-recognition** | Membership only | All attributes |
| **Subscription** | Both support | All attributes |
| **Source tracking** | Implicit | Explicit |
| **Extensibility** | Requires new subsystem | Just use it |

## Example Scenarios

### Scenario 1: Organization Management
```typescript
// I manage RedCross, declare membership
setEntityAttribute('org_redcross', 'membership', [
  'alice_pub', 'bob_pub', 'org_local_chapter'
]);

// Others can subscribe to my declaration
subscribeToAttribute('org_redcross', 'membership', myPubkey);
```

### Scenario 2: Skill Recognition Network
```typescript
// I recognize my own skills
setEntityAttribute(myPubkey, 'skills', ['programming', 'design']);

// I recognize Alice's skills (my perspective)
setEntityAttribute('alice_pub', 'skills', ['medicine', 'trauma']);

// But I trust Bob's assessment more, so subscribe to his view
subscribeToAttribute('alice_pub', 'skills', 'bob_pub');

// Query returns Bob's view (subscription overrides declaration)
// No wait - declarations take precedence!
// So I'd need to remove my declaration to use Bob's view
removeEntityAttribute('alice_pub', 'skills');
// Now subscribeToAttribute('alice_pub', 'skills', 'bob_pub');
```

### Scenario 3: Multi-Dimensional Reputation
```typescript
// I rate Carol across multiple dimensions
setEntityAttribute('carol_pub', 'reputation', {
  reliability: 0.90,
  communication: 0.85,
  technical: 0.95,
  teamwork: 0.88
});

// Alice rates Carol differently
// (Alice runs: setEntityAttribute('carol_pub', 'reputation', {...}))

// I can subscribe to Alice's reputation assessment of Carol
subscribeToAttribute('carol_pub', 'reputation', 'alice_pub');

// Or keep my own and aggregate later
```

### Scenario 4: Location-Based Matching
```typescript
// Organizations have locations
setEntityAttribute('org_redcross', 'location', {
  lat: 40.7128, lng: -74.0060,
  address: 'Washington, DC'
});

// Users have locations
setEntityAttribute(myPubkey, 'location', {
  lat: 37.7749, lng: -122.4194,
  address: 'San Francisco, CA'
});

// Matching algorithm can query locations uniformly
const orgLocation = getEntityAttribute('org_redcross', 'location');
const userLocation = getEntityAttribute(myPubkey, 'location');
const distance = calculateDistance(orgLocation, userLocation);
```

## Related Documentation

1. **[Conceptual Overview](./attribute-recognition-generalization.md)**
   - Detailed explanation of the unified model
   - Resolution logic
   - Use cases

2. **[Implementation Guide](./attribute-recognition-implementation.md)**
   - Complete code for `attributes.ts` and `attributes.svelte.ts`
   - Schema definitions
   - API reference

3. **[Visual Architecture](./attribute-recognition-diagrams.md)**
   - Flow diagrams
   - Data structure visualizations
   - Comparison charts

4. **[Migration Guide](./attribute-recognition-migration.md)**
   - Step-by-step migration plan
   - Adapter implementation
   - Testing strategy
   - Rollback plan

## Next Steps

1. **Review** these documents with the team
2. **Discuss** any concerns or alternative approaches
3. **Decide** on timeline for implementation
4. **Implement** Phase 1 (foundation)
5. **Validate** with backward-compatible adapters
6. **Migrate** consumers gradually
7. **Add** new attribute types to demonstrate value
8. **Celebrate** the flexibility! 🎉

## Questions & Answers

**Q: Won't this break existing code?**  
A: No! Adapters maintain backward compatibility. Existing APIs continue to work.

**Q: Is this over-engineering?**  
A: No. We already have 2 specialized systems (~1400 lines). The unified system is smaller (~550 lines) and enables unlimited attribute types.

**Q: What about performance?**  
A: Negligible impact. One extra indirection level in O(1) lookups.

**Q: What if we need attribute-specific logic?**  
A: Use type-safe helpers or runtime validation. The core system remains generic.

**Q: Can we add attributes without schema changes?**  
A: Yes! Attribute types are extensible strings. Add schemas for validation, but not required.

**Q: How do we handle attribute versioning?**  
A: Same as current: timestamp + schema versioning. The unified system doesn't change this.

**Q: What about privacy/permissions?**  
A: Future enhancement. Can add permission checks in getEntityAttribute().

**Q: Can we merge multiple sources?**  
A: Not in v1, but architecture supports it. Add merge strategies later.

## Conclusion

The unified Entity-Attribute-Recognition framework provides a powerful, flexible foundation for rich, multi-faceted recognition networks. It maintains backward compatibility while dramatically simplifying the addition of new attribute types and enabling symmetric recognition patterns across users and organizations.

By unifying membership, capacities, needs, and future attributes (skills, location, reputation, etc.) into a single consistent model, we reduce code complexity, improve maintainability, and unlock new use cases.

**The path forward is clear, safe, and valuable.**
