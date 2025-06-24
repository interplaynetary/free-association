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

// So while we no longer face issues with respect to accidently selecting the text and creating the pop up, we are now longer able to open the keyboard to edit the text in some browsers.

// is there a more elegant scaling solution?

// for our uiDropDownProvider could replace our .once with our await gun.user(data)

// now on some phones we have troubles with text editing keyboard

Why is my alias, appearing as my pubKey?

Lets make the path better especially for really nested:
ruzgar/.../current-path
with ... being a back button?
Or have it scale

Todo:
browser scaling

name-resolution saving to UserList: remove UserList from getUserName

select is working strange on some inputs (i.e. bottomsheet)


Ideas:
Joint request shares?
Sharing time
Sharing space
Sharing Space-Time together?

Dividing a Capacity Quantity
Dividing Space-Holding-Time?

How to divide an apartment?
Over time?

# providing capacitiy with specific-space and specific-time:

Example: "Room 302 from 2-4pm on Tuesday"

Division means:
Temporal subdivision within the bounded time slot
Concurrent sharing if physically possible
Maximum constraints - both space and time are fixed boundaries

Division mechanics:
Person A gets 25% = 30 minutes (2:00-2:30pm)
Person B gets 50% = 60 minutes (2:30-3:30pm)
Person C gets 25% = 30 minutes (3:30-4:00pm)

# providing capacitiy with specific-space and no specific-time:

Example: "Room 302 whenever available"

Division means:
Temporal flexibility with spatial constraint
Scheduling coordination required among recipients
Time becomes the variable dimension for division

Division mechanics:
Person A: 30% = ~50 hours/week of room access
Person B: 40% = ~67 hours/week of room access
Person C: 30% = ~50 hours/week of room access
Recipients coordinate scheduling among themselves

# providing capacitiy with no specific-space and specific-time:

Example: "A workspace (any of my 3 rooms) from 2-4pm Tuesday"

Division means:
Spatial flexibility with temporal constraint
Parallel utilization across different spaces during same time
Space becomes the variable dimension for division

Division mechanics:
Person A: 33% = 1 room for the 2-hour slot
Person B: 33% = 1 room for the 2-hour slot
Person C: 33% = 1 room for the 2-hour slot
All happen simultaneously in different spaces


# providing capacitiy with specific-space and no specific-time:

Example: "A workspace whenever, wherever I have available"
Division means:
Maximum flexibility - both space and time are variable
Optimal resource allocation across all dimensions
Complex coordination but highest efficiency potential

Division mechanics:
Person A: 40% = ~202 room-hours/week (flexible when/where)
Person B: 35% = ~176 room-hours/week (flexible when/where)
Person C: 25% = ~126 room-hours/week (flexible when/where)
Maximum coordination flexibility but requires communication systems




// given that capacities also have units, what are we dividing the units or the space-time coordinates?

rename from coordinate capacities to : availability for distribution

when the unit is time, then the coordinates are the available time-slots


messages dont load for long
messaging needs toast notifications


not possible to scroll without using the sidebar. 


we need to be able to express desire/request
so that we can properly also use % in our userNetworkCapacityShares To userCapacities Map (and we can express min/max in absolute quanitites of desire) and percent can be optimized accordingly.

writeable store: userNetworkCapacitySharesToUserCapacitiesMap 
this is what is persisted.

writeable store: userRequests 
this is what is persisted.

writeable store: userCommitments 
this is what is persisted.

Share = {
    capacityID: string 
    recieverId: pubkey string
    providerOfferedPct: %
    providerOfferedQty: derived number (prior to reallocation from player desire-expression: for more efficient allocation)
    desiredQty: fetched number (from reciever) (can exceed offeredQty)
    undesiredQty: derived number (offeredQty - desiredQty) (can exceed offeredQty, but the offeredQty is the max undesired used for other computations)
    committed: fetched number (from provider)
}



compositionShare extends Share = {
    capacityID: string 
    recieverId: pubkey string
    providerOfferedPct: %
    providerOfferedQty: derived number (prior to reallocation from player desire-expression: for more efficient allocation)
    desiredQty: fetched number (from reciever) (can exceed offeredQty)

    // this is at the level of recipientShares
    undesiredQty: derived number (offeredQty - desiredQty) (can exceed offeredQty, but the offeredQty is the max undesired used for other computations)
    committed: fetched number (from provider)
}

offered (share), desire/request, commmited

there seems to be some bug with the providerShares
I am not sure they are correctly being displayed or calculated.
It seems  they show up differently on with different window sizes as well as the the bar that shows up on the Capacities is entirely different than what shows up in my Mutual-Reocgnition.

Simple notifications system

Ok we want to reformulate capacity composition as a specialized Multi-Commodity-Flow Problem using Glpk.md

We want to represent consumption of inputs in composition

But how much of inputs get consumed?


// compose into self (pubkey)
// compose into other (contributorID)
// compose into Capacity (capacityID)


// lets add parsers for composeFrom / composeInto

// we should be loading via Play!
// creating a new tree is not working right now for new users