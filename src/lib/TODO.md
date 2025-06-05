# Ideas

## AI

- conversation to value-graph. (Maybe using squiggly) (we now use JSON, this should be way easier)

## Map

- Make markers display the location of capacities, and color the tags according to the depth at which one received access.

## Narrative: Charecters

- People can create charecters that they can play. (accounts) This makes it feel more approachable.

Add filters to inventory UI

Within protocol:
✅ How do we do the calculation chain?
✅ When to recalculate and persist

When change in tree:
✅ persist Tree
✅ recalculate and persist SOGF
✅ then recalculate and persist provider-Shares
✅ then recalculate and persist recipient-Shares

When change in capacities:
✅ Recalculate and persist

✅ // Our capacities should have recipientShares: (with filters applied), these should be stored on the capacity object itself.

// What kind of reactivity is needed for filter conditions?
// When output of filter-conditions changes, we should update a capacities recipientShares

// then people would subscribe to their changes in the recipientShares of a capacity?

but how does the other know when a capacity has changed efficiently, without being notified for all changes to capcities that dont involve us? if we mantain:
user.get(recipients).get(recipientPubKey).put(JSON.stringify(structuredClone(getReceiverShares(recipientPubKey))))

// well as an effieciency question:
// what is more likely? if we are a mutual-contributor we probably have more capacities in which we are included than not, unless someone gets extremely specific with their filters.

---

We have 10 fast-prompts left

1. include/exclude filters: both dropdowns should allow toggling between listing people or listing categories.
2. changing the filter should lead to recalculation for that specific capacity, it should not need to trigger recalculation for all of them? (Or should it?) I wonder if once things get more complicated there will be dependant filters (this filter depends on the outputs of this other one) : it will be important to manage loops that would form between dependant filters, for the meantime none of them should be considered dependant at all. All capacity recipientShares should be recalculated independantly?
3. recipient_shares: we should only subscribe to the capacities of our **mutual-contributors**, and we should use this to populate our inventory with the capacities for which we have recipient_shares
4. request flow: lets do a live-sequence chart for requesting and accepting
5. we need to extend our protocol.ts, schema.ts, with the logic for max-request: (specified in filter-math.md and the chat titled: Capacity Request Specifications and Functionality)
6. for those with location data we want to reactively stream the locationData to the map with svelte-mapgl-libre
7. upload

IDEA: When we have a recipientShare in one's capacities, and that capacity has a filter: show us the filters and identify which included us, so we know the roles are are essentially playing in that other person.

Aesthetic:

- Add hash based coloring, so coloring is always consistent on the same strings for everything: nodes, tags, names (bar-colors) etc.

client.js:2949 [svelte] ownership_invalid_mutationsrc/lib/components/DropDown.svelte mutated a value owned by src/lib/components/Capacity.svelte. This is strongly discouraged. Consider passing values to child components with `bind:`, or use a callback insteadhttps://svelte.dev/e/ownership_invalid_mutation

Fix capacity filters: includes/excludes
name in Header loginPanel is not always resolving properly
make mobile friendly

Map:
improve accuracy of location
make capacities/shares feed into the map

Allow people to put up their means of contact (phone number etc.)

Make the dropdown into the userNameCache

✅ Lets make it so that if the breadcrumb path is longer than 10 charecters it gets truncated.

So i notice that our tree sometimes doesnt load during manifestation and it gets overwritten by a new tree! I dont think this is an issue with gun, rather it has to do with the way in which our tree loading differs from how we load capacities. With our capacities we never have this issue.

[MANIFEST] Raw tree data from Gun: Object: undefined
validation.ts:60 Tree data validation failed: ZodError: [
{
"expected": "object",
"code": "invalid_type",
"path": [],
"message": "Invalid input: expected object, received undefined"
}
]
at zod_v4.js?v=28d20a7b:1187:12
at inst.safeParse (zod_v4.js?v=28d20a7b:9282:38)
at parseTree (validation.ts:55:33)
at User.<anonymous> (gun.svelte.ts:431:22)
at once (chunk-QJ6Z5JNU.js?v=28d20a7b:1574:18)
parseTree @ validation.ts:60Understand this error
gun.svelte.ts:451 [MANIFEST] No valid tree data found, creating initial tree
subscriptions.svelte.ts:20 [TREE-SUB] Tree updated in store, rebuilding nodes map

Error Handling & Resilience
Graceful Degradation:
If filtering fails → capacity gets empty share map {}
If no filter rule → shares pass through unchanged
Missing context → filters work with available data
Invalid rules → system continues with unfiltered shares

Ok now i notice that maybe we have invalid rules or filtering is failing since applying

improve bottomsheet

make chatMessage display the telegram style name and use gunAvatar for the profile.




