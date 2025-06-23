# Generic Distributed Reactive Networked Protocol Architecture

This repository uses Gun.js with Svelte-5 (sveltekit), Zod v4, and JSOG (JSON Object Graph). It uses bun (npm alternative) and typescript.

We rely on svelte-5 writeables and derived stores.

## Schema Validation with Zod

All data structures are defined using Zod schemas for runtime validation and TypeScript type inference. This ensures data integrity when receiving data from the network or user input. Before updating writeables, validate incoming data against the appropriate schema. Zod schemas also provide the TypeScript types used throughout the application, keeping validation and types in sync.

Create writeables for all data that is loaded from the network or modified by the user, your interface will update these writeables.

Set up subscriptions to the gun-path, to reactively stream the value at that path, JSOG.decode() it and update the writeables. Only update the writeable if the incoming data is different from what is already in the writeable.

Determine which writeables are for user data vs network data (at different scopes), scopes can themselves be writeables/derived-stores making it easy to subscribe to only those parts of the gun.js graph that concern us for the specific data we wish to update our writeables with.

# Current Writeables/Derived Stores

## User Data (Writeables):

- userTree _(Gun: user.get('tree'))_
- userSogf _(derived from tree)_
- userCapacities _(Gun: user.get('capacities'))_
- userDesiredComposeFrom _(Gun: user.get('desiredComposeFrom'))_
- userDesiredComposeInto _(Gun: user.get('desiredComposeInto'))_
- nodesMap _(derived from tree)_
- contributors _(derived from tree)_
- allKnownContributors _(derived from tree)_
- recognitionCache _(updated by SOGF subscriptions)_

## Network Data (Writeables):

- networkCapacities _(Gun: ~contributorId/capacities for each **mutual contributor**)_
- networkCapacityShares _(Gun: ~contributorId/capacityShares/ourId for each **mutual contributor**)_
- networkDesiredComposeFrom _(Gun: ~contributorId/desiredComposeFrom for each **mutual contributor**)_
- networkDesiredComposeInto _(Gun: ~contributorId/desiredComposeInto for each **mutual contributor**)_
- userIds _(Gun: usersList.map() for **all users**)_
- userNamesCache _(Gun: usersList.map() + ~userId/alias for **all users**)_
- usersList _(Gun: usersList.map() for **all users**)_

### Network Subscription Patterns:

**Contributors Subscribe Pattern** _(triggers on contributors writeable changes)_:

- SOGF subscriptions: _(Gun: ~contributorId/sogf for each **all contributors**)_ → updates recognitionCache

**Mutual Contributors Subscribe Pattern** _(triggers on mutualContributors derived store changes)_:

- Capacity subscriptions: _(Gun: ~contributorId/capacities for each **mutual contributor**)_ → updates networkCapacities
- Share subscriptions: _(Gun: ~contributorId/capacityShares/ourId for each **mutual contributor**)_ → updates networkCapacityShares
- Composition subscriptions: _(Gun: ~contributorId/desiredComposeFrom & desiredComposeInto for each **mutual contributor**)_ → updates networkDesiredComposeFrom & networkDesiredComposeInto

## Core Derived Stores:

- userNetworkCapacitiesWithShares
- mutualRecognition
- mutualContributors
- providerShares
- subtreeContributorMap
- capacityShares
- contributorCapacityShares
- userCapacitiesWithShares
- subtreeOptions

## Composition Analysis (Derived):

- feasibleComposeFrom
- feasibleComposeInto
- mutualDesireOurCapacities
- mutualDesireTheirCapacities
- mutualFeasibleOurCapacities
- mutualFeasibleTheirCapacities

## Loading States (Writeables):

Critical for preventing infinite loops and race conditions in reactive network architecture:

- isLoadingCapacities _(prevents persistence loops during network data loading)_
- isLoadingTree _(prevents persistence loops during network data loading)_
- isLoadingSogf _(prevents persistence loops during network data loading)_
- isRecalculatingCapacities _(unused)_
- isRecalculatingTree _(prevents expensive recalculations during tree updates)_

**Key Functions:**

1. **Prevent Persistence Loops**: Skip `persist()` calls when data is being loaded from network
2. **Avoid Premature Calculations**: Skip expensive recalculations on incomplete data
3. **Maintain Data Integrity**: Prevent local changes from overwriting incoming network data

## Other:

- userObjectAttributes (writeable) _(Gun: ~ourId/objectName/objectAttributes) // can be used to recognize schemas. e.g. "pie"
- networkObjectAttributes (writeable)  _(Gun: ~contributorId/objectName/objectAttributes for each **mutual contributor**)_ // how others percieve "pie"
- commonObjectAttributes (derived) // mapping of object-attributes to those that recognize them.

- publicTemplates


Create derived stores from these writeables. (This is where the main protocol architecturing will come in)

Determine which of these writeables / derived stores must be accessed by others in the network, and ensure you put them at the corresponding gun-path. JSOG.encode what you wish to put at this gun-path.

Perhaps create derived stores that make data more easily indexible by the network. For example index by contributor so i can look up myself in your own data and subscribe to only what i care about.
