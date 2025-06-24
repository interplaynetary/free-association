# Generic Distributed Reactive Networked Protocol Architecture - Design Guide

This guide outlines the architecture pattern used to build peer-to-peer applications where multiple participants can reactively share, process, and compose data structures in real-time.

This repository uses Gun.js with Svelte-5 (sveltekit), Zod v4, and JSOG (JSON Object Graph). It uses bun (npm alternative) and typescript.

Follow these steps to implement a distributed reactive networked protocol:

## Step 1: Define Data Structures with Zod Schemas

All data structures are defined using Zod schemas for runtime validation and TypeScript type inference. This ensures data integrity when receiving data from the network or user input. Zod schemas also provide the TypeScript types used throughout the application, keeping validation and types in sync.

**Action**: Create Zod schemas for all your data types before implementing any network or state logic.

## Step 2: Create Writeables for Data Sources

We rely on svelte-5 writeables and derived stores for reactive state management.

Create writeables for all data that is loaded from the network or modified by the user. Your interface will update these writeables.

**Categorize into two types:**

### User Data (Writeables):

Data that the current user owns and modifies:

- userTree _(Gun: user.get('tree'))_
- userSogf _(derived from tree)_
- userCapacities _(Gun: user.get('capacities'))_
- userDesiredComposeFrom _(Gun: user.get('desiredComposeFrom'))_
- userDesiredComposeInto _(Gun: user.get('desiredComposeInto'))_
- userObjectAttributes _(Gun: ~ourId/objectName/objectAttributes)_

### Network Data (Writeables):

Data received from other network participants:

- networkCapacities _(Gun: ~contributorId/capacities for each **mutual contributor**)_
- networkCapacityShares _(Gun: ~contributorId/capacityShares/ourId for each **mutual contributor**)_
- networkDesiredComposeFrom _(Gun: ~contributorId/desiredComposeFrom for each **mutual contributor**)_
- networkDesiredComposeInto _(Gun: ~contributorId/desiredComposeInto for each **mutual contributor**)_
- userIds _(Gun: usersList.map() for **all users**)_
- userNamesCache _(Gun: usersList.map() + ~userId/alias for **all users**)_
- usersList _(Gun: usersList.map() for **all users**)_
- networkObjectAttributes _(Gun: ~contributorId/objectName/objectAttributes for each **mutual contributor**)_

## Step 3: Set Up Gun-Path Subscriptions

Set up subscriptions to the gun-path, to reactively stream the value at that path. For each subscription:

1. **JSOG.decode()** the incoming data
2. **Validate** against appropriate Zod schema before updating writeables
3. **Only update** the writeable if the incoming data is different from what is already stored
4. **Determine scopes**: Scopes can themselves be writeables/derived-stores making it easy to subscribe to only those parts of the gun.js graph that concern us for the specific data we wish to update

### Network Subscription Patterns:

**Contributors Subscribe Pattern** _(triggers on contributors writeable changes)_:

- SOGF subscriptions: _(Gun: ~contributorId/sogf for each **all contributors**)_ → updates recognitionCache

**Mutual Contributors Subscribe Pattern** _(triggers on mutualContributors derived store changes)_:

- Capacity subscriptions: _(Gun: ~contributorId/capacities for each **mutual contributor**)_ → updates networkCapacities
- Share subscriptions: _(Gun: ~contributorId/capacityShares/ourId for each **mutual contributor**)_ → updates networkCapacityShares
- Composition subscriptions: _(Gun: ~contributorId/desiredComposeFrom & desiredComposeInto for each **mutual contributor**)_ → updates networkDesiredComposeFrom & networkDesiredComposeInto

## Step 4: Create Derived Stores from Writeables

This is where the main protocol architecturing occurs. Create derived stores that compute relationships and transformations from your base writeables:

### Core Derived Stores:

- nodesMap _(derived from tree)_
- contributors _(derived from tree)_
- allKnownContributors _(derived from tree)_
- userNetworkCapacitiesWithShares
- mutualRecognition
- mutualContributors
- providerShares
- subtreeContributorMap
- capacityShares
- contributorCapacityShares
- userCapacitiesWithShares
- subtreeOptions
- commonObjectAttributes _(mapping of object-attributes to those that recognize them)_

### Composition Analysis (Derived):

- feasibleComposeFrom
- feasibleComposeInto
- mutualDesireOurCapacities
- mutualDesireTheirCapacities
- mutualFeasibleOurCapacities
- mutualFeasibleTheirCapacities

## Step 5: Determine Network Persistence Points

Determine which of these writeables/derived stores must be accessed by others in the network, and ensure you put them at the corresponding gun-path.

**Action**: For each piece of data that needs network visibility:

1. **JSOG.encode** what you wish to put at the gun-path
2. **Create indexable derived stores** that make data more easily discoverable by the network
3. **Example**: Index by contributor so others can look up themselves in your data and subscribe to only what they care about

## Step 6: Implement Loading States for Race Condition Prevention

Critical for preventing infinite loops and race conditions in reactive network architecture:

### Loading States (Writeables):

- isLoadingCapacities _(prevents persistence loops during network data loading)_
- isLoadingTree _(prevents persistence loops during network data loading)_
- isLoadingSogf _(prevents persistence loops during network data loading)_
- isRecalculatingCapacities _(unused)_
- isRecalculatingTree _(prevents expensive recalculations during tree updates)_
- recognitionCache _(updated by SOGF subscriptions)_

### Key Functions:

1. **Prevent Persistence Loops**: Skip `persist()` calls when data is being loaded from network
2. **Avoid Premature Calculations**: Skip expensive recalculations on incomplete data
3. **Maintain Data Integrity**: Prevent local changes from overwriting incoming network data

## Step 7: Implement Object Recognition System

Set up object recognition for schema discovery and mutual understanding:

1. **publicTemplates** (writeable) - Templates available for others to use
2. **Object attribute mapping** - How different participants perceive the same objects
3. **Recognition caching** - Store recognition results to avoid repeated computations

---

**Design Principle**: Always validate incoming data against Zod schemas before updating writeables to ensure type safety and data integrity across the distributed network.
