# Cycles, Time, and Reciprocity in Commons Composition

## The Problem

The current `CommonsManager` enforces a DAG (directed acyclic graph) on instance references: if commons A fills a slot in commons B, then B cannot (transitively) fill a slot in A. The `fillSlot` method validates this and rejects cycles.

But reciprocity — the foundational mechanism of commons — is structurally cyclic:

```
Alice's Need: "Childcare"  → filled by Bob
Bob's Need:   "Tutoring"   → filled by Alice
```

If we forbid this, we forbid mutual aid. We'd be enforcing unidirectional resource flow — which is the structure of markets (buyer→seller) and hierarchies (command→labor), not commons.

So: should cycles be allowed? And if so, what constraint replaces the DAG requirement?

---

## Three Levels of Structure

The confusion arises from conflating three distinct levels:

### 1. Templates (timeless, hashable)

Templates are pure mathematical objects. "We need 10 hours of childcare with conflict-resolution skills" is a content-addressed pattern. It has no temporal dimension. Templates don't reference each other cyclically because they don't reference instances at all — they describe *shapes* of cooperation.

Templates are already clean. No cycle problem here.

### 2. Instances (created in time, contextually bound)

An instance is a template + context (WHERE/WHEN/WHO) + state (which slots are filled). Instances are created at specific moments. They accumulate fills over time.

The reference graph between instances (via `filled_by_refs`) is where "cycles" appear. But these aren't logical cycles — they're reciprocal relationships that emerged from sequential acts.

### 3. Fill Events (strictly ordered in time)

A fill event is: "At time T, agent X filled slot S of instance I with reference R."

Fill events are **always** temporally ordered. Even in a distributed system with no global clock, each fill is a discrete act that happens-after certain other events (the creation of both the filling and filled instances, at minimum).

**The DAG is in the fill events, not in the reference graph.**

---

## Why the Reference Graph Can Have Cycles

Consider the mutual aid example in detail:

```
T1: Alice creates instance A ("Childcare Need"), status: potential
T2: Bob creates instance B ("Tutoring Need"), status: potential
T3: Bob fills A's childcare slot with himself. A becomes actual.
T4: Alice fills B's tutoring slot with herself. B becomes actual.
```

After T4, the reference graph is:

```
A.slot[childcare].filled_by → Bob
B.slot[tutoring].filled_by → Alice
```

No cycle here — these reference people, not other commons instances. But now consider a more structured version:

```
T1: Alice creates instance A ("Childcare Coop"), with slot needing tutoring
T2: Bob creates instance B ("Tutoring Exchange"), with slot needing childcare
T3: Bob fills A's childcare slot, referencing instance B as the provider
T4: Alice fills B's tutoring slot, referencing instance A as the provider
```

Now the reference graph is:

```
A.slot[childcare].filled_by_refs → {B: true}
B.slot[tutoring].filled_by_refs → {A: true}
```

This is a cycle. The current code would reject whichever fill happens second. But both fills are perfectly valid — each references an instance that already exists at the time of the fill. There's no deadlock, no logical impossibility. The cycle in the reference graph is an artifact of reciprocity, not a bug.

---

## What the Real Constraint Is

The constraint isn't "no cycles in the reference graph." It's:

**A fill event can only reference things that exist at the time of the fill.**

More precisely:

1. The instance being filled must exist (obviously)
2. The thing doing the filling must exist (an instance, a resource, a person)
3. The fill event itself is recorded with a timestamp (or logical clock position)

This is a **causal ordering** constraint, not a structural one. It says: you can't fill a slot with a reference to something that hasn't been created yet. But it says nothing about whether the resulting reference graph has cycles.

In a distributed system, "exists at the time of the fill" means "the creation event happens-before the fill event" in the Lamport sense. This is exactly what the ITC (Interval Tree Clocks) in the codebase are for — establishing causal ordering without global time.

---

## The Temporal vs. Structural Distinction

| | Structural (Reference Graph) | Temporal (Event Ordering) |
|---|---|---|
| **What it tracks** | Which instances reference which | When events happened relative to each other |
| **Can have cycles?** | Yes — reciprocity | No — time is a partial order (DAG) |
| **What prevents deadlocks?** | Nothing structural needed | Causal ordering: can't reference the future |
| **Current implementation** | DAG enforced (too restrictive) | Not tracked (the actual constraint is missing) |

The current code enforces the wrong constraint (structural DAG) while missing the right one (temporal causality).

---

## Implications for `deriveSlotStatus`

The current `deriveSlotStatus` walks the reference graph transitively:

```typescript
private deriveSlotStatus(slotId, slots, seen) {
    // ...
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

This says: "A slot is actual only if everything it references is also transitively actual." With cycles, this would infinite-loop (the `seen` set prevents that, defaulting to `potential` — which means cycles are always `potential`, never `actual`).

But this transitive definition is wrong for commons. It encodes a supply-chain logic: "my output isn't real until all my inputs are real." In a commons, the relevant question is simpler:

**Has this slot been filled by a concrete contribution?**

A slot is actual when someone has filled it. Full stop. The fill itself is the social fact — it's a person or community saying "I'm contributing this." Whether their own needs are met elsewhere is a separate question about *their* commons, not *this* slot's actuality.

The revised logic:

```typescript
private deriveSlotStatus(slotId, slots) {
    const slot = slots[slotId];
    if (!slot?.actually_filled_by_refs) return 'potential';
    return Object.keys(slot.actually_filled_by_refs).length > 0 ? 'actual' : 'potential';
}
```

No recursion. No graph traversal. A slot is actual when it has fills. The commons is actual when all required slots are actual. Clean.

---

## What About Conditional Capacities?

The catering example from earlier: "I can cater, but I need a kitchen and ingredients." This seems to require transitive status — the catering isn't *really* available until the kitchen is secured.

But this conflates two things:

1. **The caterer's commons** — has slots for kitchen and ingredients. Its status reflects whether *those* slots are filled. If they're not, the catering commons is `potential`.

2. **The event's catering slot** — filled by a reference to the caterer's commons instance. The slot is `actual` because someone committed to filling it.

The event organizer can see that their catering slot is filled (actual) but the *referenced* catering commons is still potential (kitchen unfilled). That's useful information — it means "we have a caterer committed, but they still need a kitchen." The UI can surface this. But the event's slot status shouldn't collapse to `potential` because of the caterer's sub-dependencies.

Why? Because the caterer might find a kitchen through an entirely different channel. Or the caterer might already have a kitchen and just hasn't recorded it yet. Or the kitchen might be a standing resource that doesn't need a commons instance. Transitive status assumes that the only way sub-dependencies get resolved is through the same registry, which is a closed-world assumption that doesn't hold.

If the event organizer wants to enforce "catering is only actual when fully ready," that's what `acceptance_logic` is for — a governance decision, not a structural constraint.

---

## Revised Architecture

### What changes:

1. **Remove DAG validation from `fillSlot`** — cycles in the reference graph are valid (reciprocity)
2. **Simplify `deriveSlotStatus`** — a slot is actual when it has fills, no transitive walk
3. **Remove `validateInstanceDAG` / `validateAllDAGs`** — structural cycles are not errors
4. **Keep `extractInstanceDependencies` and `dependentsIndex`** — still useful for propagating *notifications* (when something changes, who might care?), just not for enforcing acyclicity
5. **Add temporal ordering** — fill events should carry timestamps or logical clock stamps, and the system should verify causal ordering (the referenced thing must exist before the fill)

### What stays:

- Templates are still content-addressed and acyclic (they don't reference instances)
- `ResourceTemplate` / `ResourceContext` split still holds
- `instantiate()` still works — it creates an instance with empty slots
- `fillSlot()` still works — it records a fill. Just without cycle rejection
- `propagateFillToDependents` / `propagateUnfillToDependents` still work — they propagate notifications through the dependency index. They just can't assume acyclicity (they already handle this with the `affected` set that prevents re-visiting)

### What this means conceptually:

The reference graph between commons instances is not a DAG — it's a **directed graph** that may contain cycles. The cycles represent reciprocity. The DAG exists in the **temporal ordering of events** (creation, fills, unfills), which is guaranteed by causality itself.

---

## The Deeper Question

This connects to something fundamental about commons vs. markets:

**Markets require acyclicity.** A supply chain is a DAG: raw materials → components → assembly → distribution → consumer. Value flows one direction. Money flows the other. If you introduce a cycle (A buys from B buys from A), you get either cancellation (net settlement) or instability (speculation).

**Commons require cyclicity.** Mutual aid, reciprocity, gift economies — all involve cycles. I help you, you help me. Not as a balanced exchange (that's barter, which is a degenerate market), but as ongoing mutual contribution to shared wellbeing. The cycle isn't a bug to be netted out — it's the mechanism of social reproduction.

By enforcing a DAG, the current implementation accidentally imports market logic into the commons structure. The `ResourceTemplate` / `ResourceContext` split correctly separates what from where/when/who. The next step is separating **structural** relationships (which can cycle) from **temporal** ordering (which cannot).

---

## Open Questions

1. **Should `computeDerived` report transitive status as supplementary information?** E.g., "this slot is filled, but 3 of its sub-dependencies are still potential." Not as a status override, but as a depth indicator.

2. **How does this interact with `acceptance_logic`?** If acceptance logic can reference the status of other commons, you could still create logical deadlocks ("accept this fill only if commons X is actual, but X accepts fills only if this commons is actual"). Is that a concern to guard against, or is it the community's problem to govern wisely?

3. **What does the temporal ordering look like in practice?** Each fill event needs a causal stamp. Is this where ITC integration happens? Or is wall-clock time sufficient for a single-node system?

4. **Does the `propagateFillToDependents` logic need to change?** Currently it promotes `potential_filled_by_refs` to `actually_filled_by_refs` when a dependency becomes actual. With non-transitive status, this promotion logic may need rethinking — what triggers a potential→actual promotion if not transitive status?
