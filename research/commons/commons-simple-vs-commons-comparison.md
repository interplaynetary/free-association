# Commons-Simple vs Commons: A First-Principles Comparison

## The Core Question

How do we model the relationship between:
1. **Templates** (abstract, hashable patterns of cooperation)
2. **Instances** (concrete enactments in space-time)
3. **Fills** (the social facts of contribution)

And specifically: when the same slot template appears multiple times in a commons (e.g., two childcare slots), how do we address fills?

---

## The Two Approaches

### Commons.ts: Slot Instances Keyed by Template CID

```typescript
slotInstances: z.record(CID, SlotInstance)  // slot template id → instance
```

**Addressing a fill:**
```typescript
fill(commonsInstanceId: NanoId, slotId: CID, filledBy: {...})
```

**Structure:**
- The template's `slots` array contains `SlotWithId[]` (each has a CID)
- Each slot CID gets exactly one `SlotInstance` in the record
- Multiple identical slots in the template → collision (same CID → one instance)
- The `slotId` parameter is the template CID

### Commons-Simple.ts: Slot Instances Keyed by Instance NanoId

```typescript
slotInstances: z.record(NanoId, SlotInstance)  // instance id → instance
```

**Addressing a fill:**
```typescript
fill(commonsInstanceId: NanoId, slotInstanceId: NanoId, filledBy: {...})
```

**Structure:**
- The template's `slots` array can have duplicate CIDs
- Each slot gets a unique `SlotInstance` with its own NanoId, regardless of template duplication
- The `slotInstanceId` parameter is the instance's unique ID
- Context is passed as an array parallel to `slots`, not keyed by CID

---

## First-Principles Analysis

### 1. Identity and Addressability

**What is the identity of a slot?**

#### Template Identity (CID)
- The CID is the hash of (name, input, optional, acceptance_logic)
- Two slots with the same CID are *the same kind of slot*
- They represent the same abstract role ("childcare with these requirements")

#### Instance Identity (NanoId)
- The NanoId is a unique identifier for *this particular enactment*
- Even if two slots share a template CID, their instances are distinct
- They exist at different space-times, have different fills

**Commons.ts assumes:** One template CID → one instance per commons.
**Commons-simple.ts assumes:** One template CID → potentially many instances per commons.

Which is correct? **It depends on the semantics of templates vs. instances.**

If templates are *roles* and instances are *enactments of roles*, then:
- "We need two childcare providers" is **one role with quantity=2**, not two roles
- "We need morning childcare and evening childcare" is **two roles**, even if both have identical requirements structurally

The template-level distinction is: are these the same role filled multiple times, or distinct roles?

### 2. Quantity vs. Multiplicity

**Two ways to express "we need two of something":**

#### Quantity (within one slot)
```typescript
{
    name: "Childcare",
    input: { kind: 'resource', type_id: 'childcare', quantity: 2 }
}
```
- One slot template, one slot instance
- The `quantity` field says "2 units of this resource"
- Fills reference multiple providers, but all fill the *same slot instance*

#### Multiplicity (multiple slots)
```typescript
[
    { name: "Morning Childcare", input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: "Evening Childcare", input: { kind: 'resource', type_id: 'childcare', quantity: 1 } }
]
```
- Two slot templates (different names → different CIDs)
- Two slot instances, each with its own context and fills

But what if the names are also identical?
```typescript
[
    { name: "Childcare", input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: "Childcare", input: { kind: 'resource', type_id: 'childcare', quantity: 1 } }
]
```
- Same CID (identical content)
- Commons.ts: **collision** — second slot overwrites first in the record
- Commons-simple.ts: **two distinct instances** — same role, different enactments

**Which is semantically correct?**

The question is: **what does the template's `slots` array represent?**

If it represents *distinct roles*, then duplicate CIDs should be allowed (same role, multiple enactments).
If it represents *unique roles*, then duplicate CIDs are a modeling error — use `quantity` instead.

### 3. Many-to-One Fills (Multiple Resources Filling One Slot)

The observation: "many resources can fill one slot."

**Example:** A childcare slot might be filled by:
- Alice (Monday-Wednesday)
- Bob (Thursday-Friday)
- Carol (backup)

#### In Commons.ts
```typescript
slotInstance.actually_filled_by_refs = {
    'alice-nanoid': true,
    'bob-nanoid': true,
    'carol-nanoid': true
}
```
- One slot instance (keyed by template CID)
- The `filled_by_refs` record can hold multiple fillers
- Works for many-to-one

#### In Commons-simple.ts
```typescript
slotInstance.filled_by = {
    'alice-nanoid': true,
    'bob-nanoid': true,
    'carol-nanoid': true
}
```
- One slot instance (keyed by instance NanoId)
- The `filled_by` record can hold multiple fillers
- Also works for many-to-one

**Conclusion:** Both handle many-to-one fills identically. The `filled_by` record is the same structure. The difference is only in how slot instances are keyed, not in how they accumulate fills.

### 4. Context Binding

**Commons.ts:**
```typescript
instantiate(commons, author, slotContexts?: Partial<Record<CID, ResourceContext>>)
```
- Context keyed by template CID
- If two slots share a CID, they share a context (or one is ignored)

**Commons-simple.ts:**
```typescript
instantiate(commons, author, slotContexts?: (ResourceContext | undefined)[])
```
- Context is an array parallel to `commons.slots`
- Each slot (regardless of CID) gets its own context

**Example:**
```typescript
// Template has two identical childcare slots
const commons = {
    slots: [childcareSlot, childcareSlot]  // same CID twice
}

// Commons.ts:
instantiate(commons, 'alice', {
    [childcareSlot.id]: { city: 'Portland', start_date: '2025-09-01' }
})
// Result: Only one slot instance created (collision). The second slot is lost.

// Commons-simple.ts:
instantiate(commons, 'alice', [
    { city: 'Portland', start_date: '2025-09-01' },  // Monday
    { city: 'Portland', start_date: '2025-09-08' }   // Next Monday
])
// Result: Two slot instances created, each with its own context.
```

If the use case genuinely requires "same template, different contexts," then **commons-simple.ts is correct**.
If the use case should instead use `quantity` to express multiples, then **commons.ts is correct** and the duplicate templates are a modeling error.

### 5. Addressing Fills

**Commons.ts:**
```typescript
fill(commonsInstanceId, slotId: CID, filledBy)
```
- You address the slot by its template CID
- If the template has duplicate CIDs, you can't distinguish which one you're filling
- This forces you to use `quantity` instead of duplicate templates

**Commons-simple.ts:**
```typescript
fill(commonsInstanceId, slotInstanceId: NanoId, filledBy)
```
- You address the slot by its instance NanoId
- Each instance is distinct, even if they share a template CID
- The caller needs to know the instance NanoId (returned from `instantiate`)

**UX implications:**

With commons.ts:
```typescript
const instance = manager.instantiate(commons, 'alice', contexts);
// To fill the childcare slot:
manager.fill(instance.instance_id, childcareSlot.id, { 'bob': true });
```
The `slotId` is known from the template (it's the CID from `commons.slots[i].id`).

With commons-simple.ts:
```typescript
const instance = manager.instantiate(commons, 'alice', contexts);
// To fill the first childcare slot:
const firstChildcareInstanceId = Object.values(instance.slotInstances).find(
    inst => inst.slot_id === childcareSlot.id
)?.instance_id;
manager.fill(instance.instance_id, firstChildcareInstanceId, { 'bob': true });
```
The caller needs to look up the instance NanoId from the returned instance. This is more indirect.

**Ergonomics:** Commons.ts is simpler for the common case (no duplicate templates). Commons-simple.ts requires an extra lookup but handles duplicates correctly.

---

## Cycle Handling

### Commons.ts: DAG Enforcement

```typescript
// Validates no cycles in the reference graph
this.validateInstanceDAG(commonsInstanceId);
if (!validation.isValid) throw new Error(`Cycle: ${cyclePath}`);
```

- Structural constraint: reference graph must be acyclic
- Rejects reciprocity (Alice fills Bob's slot, Bob fills Alice's slot)
- Status is derived transitively (if your dependency is potential, you are potential)

### Commons-simple.ts: Cycle Tolerance

```typescript
// No cycle validation
// Status is local: a slot is actual when it has fills
const filled = inst.filled_by && Object.keys(inst.filled_by).length > 0;
```

- No structural constraint on cycles
- Allows reciprocity
- Status is not transitive — a slot is actual when someone filled it, period
- Causal constraint: referenced instances must exist (temporal ordering)

**Which is correct?**

See [cycles-time-and-reciprocity.md](./cycles-time-and-reciprocity.md). The argument:
- Reciprocity (mutual aid) is cyclic: A fills B's slot, B fills A's slot
- The DAG is in the temporal ordering of fill events, not in the reference graph
- Forbidding cycles forbids reciprocity, which forbids commons

**Verdict:** Commons-simple.ts is correct on this dimension. Markets require acyclicity (unidirectional flow). Commons require cycles (reciprocity).

---

## Derived State

### Commons.ts: Transitive Status

```typescript
private deriveSlotStatus(slotId, slots, seen): 'potential' | 'actual' {
    if (seen.has(slotId)) return 'potential';
    const slot = slots[slotId];
    if (!slot?.actually_filled_by_refs) return 'potential';
    for (const ref of Object.keys(slot.actually_filled_by_refs)) {
        if (slots[ref]) {
            if (this.deriveSlotStatus(ref, slots, seen) === 'potential') {
                return 'potential'; // Transitive: if my dependency is potential, so am I
            }
        }
    }
    return 'actual';
}
```

- Recursively walks the dependency graph
- A slot is actual only if all its transitive dependencies are actual
- Encodes supply-chain logic: "my output isn't real until my inputs are real"

### Commons-simple.ts: Local Status

```typescript
const filled = inst.filled_by && Object.keys(inst.filled_by).length > 0;
```

- No recursion, no graph walk
- A slot is actual when it has fills
- The fill is the social fact — the commitment has been made

**Which is correct?**

The transitive model assumes:
- The only way dependencies get resolved is through the same registry (closed-world assumption)
- A commitment isn't valid until its sub-dependencies are resolved

The local model assumes:
- A fill is a social commitment, which stands on its own
- Whether the filler's own dependencies are met is *their* problem, not this slot's status
- The UI can surface that a fill references a still-potential commons, but the slot itself is still filled

**Example:**
- Event organizer fills "catering" slot with a caterer's commons
- The caterer's commons has an unfilled "kitchen" slot (still potential)
- Transitive: the event's catering slot is potential (caterer isn't ready)
- Local: the event's catering slot is actual (commitment received), but the referenced caterer is potential

Which matches real-world semantics? If the caterer says "I'll cater your event," that's a commitment even if they haven't secured a kitchen yet. The event organizer has a filled slot (actual), but there's still risk (the caterer might fail to deliver). The risk is visible (the catering commons is potential), but the commitment is real.

**Verdict:** Local status is more semantically accurate. Transitive status conflates "slot is filled" with "slot's transitive dependencies are resolved," which are different facts.

---

## The ResourceTemplate/ResourceContext Split

Both approaches have this split:
- **ResourceTemplate** goes into `input` (hashable, part of the slot template CID)
- **ResourceContext** goes into `resource_context` on `SlotInstance` (not hashed)

This split is identical in both. The difference is:

**Commons.ts:**
- Context is keyed by template CID, so duplicate templates share context (or collide)

**Commons-simple.ts:**
- Context is an array parallel to slots, so duplicate templates get separate contexts

The split itself is sound in both. The question is the keying strategy.

---

## Synthesis: Which Approach is Correct?

### The Fundamental Question

**Can a commons template have multiple slots with the same CID?**

If **yes**, then:
- The slots are the same role, instantiated multiple times with different contexts
- Commons-simple.ts is correct (instance-keyed, array context)
- Example: "We need childcare on Monday and childcare on Wednesday" — same role, two enactments

If **no**, then:
- Duplicate CIDs in a template are a modeling error
- Use `quantity` to express multiples of the same resource
- Commons.ts is correct (template-keyed, CID-keyed context)
- Example: "We need 2 childcare providers" — one role, quantity=2

### The Case for Commons-Simple.ts

**When you need multiple distinct enactments of the same abstract need:**

Example: A recurring event needs the same role at different times
```typescript
// Template
const weeklyChildcare = await createCommonsFromSlots('Weekly Childcare', [
    { name: 'Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
]);

// Instance
const instance = manager.instantiate(weeklyChildcare, 'alice', [
    { start_date: '2025-09-01', availability_window: { day: 'Monday', ... } },
    { start_date: '2025-09-02', availability_window: { day: 'Tuesday', ... } },
    { start_date: '2025-09-03', availability_window: { day: 'Wednesday', ... } },
    { start_date: '2025-09-04', availability_window: { day: 'Thursday', ... } },
]);

// Each slot instance can be filled independently
manager.fill(instance.instance_id, mondaySlotInstanceId, { 'bob': true });
manager.fill(instance.instance_id, tuesdaySlotInstanceId, { 'carol': true });
```

This is clean and explicit. The template says "we need this role 4 times." Each instantiation binds it to a specific day. Each can be filled by different people.

**Trying to do this in commons.ts:**
```typescript
// You'd have to give each slot a distinct name to avoid CID collision
const weeklyChildcare = await createCommonsFromSlots('Weekly Childcare', [
    { name: 'Monday Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Tuesday Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Wednesday Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
    { name: 'Thursday Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 1 } },
]);
```

This works, but now the *day* is baked into the template (the name), which changes the CID. You can't reuse the same abstract "childcare need" template. You have to create "Monday Childcare" as a distinct role.

### The Case for Commons.ts

**When duplicate templates are a modeling error:**

If you genuinely need "2 childcare providers" and you don't care about distinct contexts for each (they're interchangeable), then:
```typescript
const event = await createCommonsFromSlots('Block Party', [
    { name: 'Childcare', input: { kind: 'resource', type_id: 'childcare', quantity: 2 } },
]);
```

One slot, quantity=2. Multiple people can fill it (many-to-one). No need for duplicate templates.

**Commons.ts enforces this model** by making duplicate CIDs collide. It says: "If you want multiples, use quantity. The slots array is for distinct roles."

**Commons-simple.ts allows both models** but adds complexity: you have to address slots by instance NanoId, and the caller needs to track which instance is which.

---

## Recommendation

### Hybrid Approach

The cleanest design might be:

1. **Disallow duplicate template CIDs in the template's `slots` array** at the schema level
   - A commons template's slots must have unique CIDs
   - Use `quantity` for multiples of the same resource
   - This is a validation rule, not a keying strategy

2. **Key slot instances by NanoId** (not CID)
   - Even with unique CIDs in the template, instances should be keyed by NanoId
   - This keeps the instance layer clean (every instance has a unique ID)
   - It also future-proofs: if we later allow duplicate CIDs, the instance layer doesn't break

3. **Context keyed by template CID** (since CIDs are unique in the template)
   - Simpler than array-parallel
   - Works because the template enforces unique CIDs

4. **Local status, not transitive** (from commons-simple.ts)
   - A slot is actual when it has fills
   - No recursive graph walk

5. **No cycle enforcement** (from commons-simple.ts)
   - Reciprocity is allowed
   - Causal ordering is the real constraint

### Implementation

```typescript
// Template: enforce unique slot CIDs
export const Commons = z.object({
    id: CID.optional(),
    name: z.string(),
    description: CommonsDescription.optional(),
    slots: z.array(SlotWithId),
}).refine(
    (data) => {
        const cids = data.slots.map(s => s.id);
        return new Set(cids).size === cids.length;
    },
    { message: "Duplicate slot CIDs in template. Use quantity for multiples." }
);

// Instance: keyed by NanoId
slotInstances: z.record(NanoId, SlotInstance)

// Context: keyed by CID (since CIDs are unique in template)
instantiate(
    commons: CommonsWithId,
    author: string,
    slotContexts?: Partial<Record<CID, ResourceContext>>,
    offerer?: string,
)

// Fill: addressed by instance NanoId
fill(
    commonsInstanceId: NanoId,
    slotInstanceId: NanoId,
    filledBy: Record<string, ...>,
)

// Helper: look up instance by template CID (since it's now 1:1)
function getSlotInstanceByTemplateId(
    instance: CommonsInstance,
    templateCID: CID
): SlotInstance | undefined {
    return Object.values(instance.slotInstances).find(
        inst => inst.slot_id === templateCID
    );
}
```

This gives you:
- **Simplicity:** No duplicate templates, `quantity` for multiples
- **Clean instance layer:** NanoId keying, future-proof
- **Easy addressing:** Template CID → instance lookup is 1:1
- **Correct semantics:** Local status, cycle tolerance

---

## Open Question

**What if you genuinely need the same role at different space-times?**

Example: "I need childcare every Monday for 4 weeks."

Option 1: Four distinct slots with distinct names
```typescript
[
    { name: 'Week 1 Childcare', ... },
    { name: 'Week 2 Childcare', ... },
    { name: 'Week 3 Childcare', ... },
    { name: 'Week 4 Childcare', ... },
]
```
Pro: Works with unique-CID constraint
Con: The week number is baked into the template, not the context

Option 2: One slot with recurrence context
```typescript
{
    name: 'Childcare',
    input: { kind: 'resource', type_id: 'childcare', quantity: 1 }
}

// Context
{
    recurrence: 'weekly',
    start_date: '2025-09-01',
    end_date: '2025-09-28'
}
```
Pro: Cleaner template
Con: Now you need to model "this slot repeats 4 times," and fills need to bind to specific occurrences

Option 3: Allow duplicate CIDs, use commons-simple.ts approach
Pro: Each week gets its own slot instance with its own context
Con: Addressing becomes more complex (instance NanoId lookup)

**This is the design tension.** The answer depends on whether you think "recurring childcare" is:
- One role with temporal multiplicity (option 2)
- Four distinct roles that happen to have the same name (option 1)
- One role template, four enactments (option 3)

Option 3 is what commons-simple.ts enables. Whether that's the right model depends on the use cases.
