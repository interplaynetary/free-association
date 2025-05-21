# Ideas
## AI
- conversation to value-graph. (Maybe using squiggly) (we now use JSON, this should be way easier)

## Map
- Make markers display the location of capacities, and color the tags according to the depth at which one received access.

## Narrative: Charecters
- People can create charecters that they can play. (accounts) This makes it feel more approachable.

// we should mantain a contact list of all the contributors we have ever added.
// Contact management
// The dropdown should be based on this (not the public)


client.js:2949 [svelte] binding_property_non_reactive`bind:value={entry.unit}` (src/lib/components/Inventory.svelte:256:5) is binding to a non-reactive propertyhttps://svelte.dev/e/binding_property_non_reactive


Fix dropdown

Modify NestedPie to show SOGF for the first layer, and then each ring be mutual from the providerShares

Add persistance

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

✅ Local first recipientshare calculation based on filters, then distributed look up from centralized source

✅ Lets make the color scheme, pastels


Lets use zod.dev/v4 for defining the zod structure for the JSON, which gives the parser and the types