# Reactive P2P Decider with Svelte Stores

## Overview

The Reactive P2P Decider extends the P2P architecture with **Svelte stores** for automatic, real-time UI updates. As data arrives from the Holster network, stores update automatically and your UI reactively re-renders—no manual polling or refresh needed!

## What Makes It Reactive?

### Traditional Approach (Non-Reactive)
```typescript
// Manual polling every 5 seconds
setInterval(async () => {
  const proposals = await decider.readAllProposals();
  updateUI(proposals);  // Manual UI update
}, 5000);
```

**Problems:**
- ❌ Delayed updates (up to 5 seconds)
- ❌ Unnecessary network requests
- ❌ Manual UI management
- ❌ State synchronization complexity

### Reactive Approach
```svelte
<!-- Automatic updates as data arrives -->
{#each $allProposals as proposal}
  <div>{proposal.content}</div>
{/each}
```

**Benefits:**
- ✅ Instant updates (milliseconds)
- ✅ Efficient (event-driven, not polling)
- ✅ Automatic UI updates
- ✅ Simple, declarative code

## Files Created

### Core Implementation
1. **`src/lib/p2p-decider-reactive.svelte.ts`** - Reactive P2P Decider
   - Uses Svelte stores for all state
   - Sets up Holster listeners automatically
   - Provides derived stores for aggregated data

2. **`examples/ReactiveDeciderUI.svelte`** - Full UI Example
   - Complete game interface
   - Shows all reactive features
   - Beautiful, modern design

### Documentation
3. **`docs/p2p-reactive-guide.md`** - Complete reactive guide
   - Store architecture
   - Usage patterns
   - Advanced techniques

4. **`REACTIVE-P2P-README.md`** - This file
   - Quick start
   - Key concepts
   - Examples

## Store Architecture

### Raw Data Stores (Writable)
These hold raw data as it arrives from each participant:

```typescript
class ReactiveP2PDecider {
  config: Writable<GameConfig | null>
  participants: Writable<string[]>
  
  // Internal stores tracking each participant's data
  private participantStores: {
    proposals: Writable<Map<pubKey, Proposal>>
    challenges: Writable<Map<pubKey, Map<targetPub, Challenge>>>
    comments: Writable<Map<pubKey, Map<targetPub, Comment>>>
    modifications: Writable<Map<pubKey, Map<targetPub, Modification>>>
    support: Writable<Map<pubKey, Map<targetPub, Support>>>
  }
}
```

### Derived Stores (Readable)
These automatically aggregate data from all participants:

```typescript
  allProposals: Readable<Proposal[]>
  allChallenges: Readable<Map<targetPub, Challenge[]>>
  allComments: Readable<Map<targetPub, Comment[]>>
  allModifications: Readable<Map<targetPub, Modification[]>>
  allSupport: Readable<Map<targetPub, Support[]>>
  
  // High-level derived stores
  currentPhase: Readable<GamePhase>
  consensusResults: Readable<Map<targetPub, winningContent>>
  isReady: Readable<boolean>
```

## Quick Start

### 1. Create Reactive Decider

```typescript
import { ReactiveP2PDecider } from './lib/p2p-decider-reactive.svelte';
import { onMount, onDestroy } from 'svelte';

let decider: ReactiveP2PDecider;

onMount(async () => {
  decider = new ReactiveP2PDecider(user, gameId);
  await decider.createGame(['What should we do?']);
});

onDestroy(() => {
  decider?.destroy();  // Clean up listeners
});
```

### 2. Extract Stores for Reactive Use

```typescript
// Extract stores from decider
$: config = decider?.config;
$: currentPhase = decider?.currentPhase;
$: allProposals = decider?.allProposals;
$: allChallenges = decider?.allChallenges;
$: consensusResults = decider?.consensusResults;
```

### 3. Use Stores in Template

```svelte
<!-- Auto-updates as data arrives -->
<h2>Phase: {$currentPhase}</h2>

<div class="proposals">
  {#each $allProposals as proposal}
    <div class="proposal">
      <h3>{proposal.content}</h3>
      
      <!-- Challenges appear automatically -->
      {#if $allChallenges?.get(proposal.authorPub)}
        {#each $allChallenges.get(proposal.authorPub) as challenge}
          <p>⚠️ {challenge.content}</p>
        {/each}
      {/if}
      
      <!-- Winner updates automatically -->
      {#if $consensusResults?.get(proposal.authorPub)}
        <div class="winner">
          🏆 {$consensusResults.get(proposal.authorPub)}
        </div>
      {/if}
    </div>
  {/each}
</div>
```

### 4. Write Data (Same as Non-Reactive)

```typescript
// All write operations remain the same
await decider.writeMyProposal('My great idea');
await decider.writeMyChallengeToProposal(theirPub, 'Needs work');
await decider.writeMyCommentOnProposal(theirPub, 'Love it!');
await decider.writeMySupportForProposal(theirPub, { 'Original': 5, 'Modified': 5 });
```

## How It Works

### Data Flow Diagram

```
1. Player writes data
   ↓
2. Holster stores data in their user space
   ↓
3. Holster listeners detect change
   ↓
4. Writable store updates
   ↓
5. Derived stores recalculate (automatic)
   ↓
6. Svelte components re-render (automatic)
   ↓
7. UI updates (instant)
```

### Listener Setup

When you call `createGame()` or `joinGame()`, the reactive decider automatically:

1. Creates listeners for all data paths from all participants
2. Updates stores when data arrives
3. Handles cleanup when destroyed

```typescript
// Automatically sets up listeners like:
listenAtPath(user, [participantPub, 'games', gameId, 'proposals', '0'], (data) => {
  // Update store
  this.participantStores.proposals.update(map => {
    map.set(participantPub, data);
    return map;
  });
});
```

## Key Features

### 1. Automatic Phase Detection

The `currentPhase` store automatically determines the current game phase:

```typescript
$: currentPhase = decider?.currentPhase;

// In template:
{#if $currentPhase === 'proposing'}
  <input placeholder="Your proposal..." />
{:else if $currentPhase === 'challenging'}
  <button>Challenge</button>
{:else if $currentPhase === 'supporting'}
  <button>Vote</button>
{:else if $currentPhase === 'complete'}
  <h2>Results!</h2>
{/if}
```

### 2. Real-Time Aggregation

Derived stores automatically aggregate data from all participants:

```typescript
// Automatically recalculates when new data arrives
allProposals: derived(participantStores.proposals, ($proposals) => {
  return Array.from($proposals.values()).sort((a, b) => a.timestamp - b.timestamp);
});
```

### 3. Live Consensus Calculation

Consensus updates automatically as votes arrive:

```typescript
consensusResults: derived(
  [allProposals, allModifications, allSupport],
  ([$proposals, $modifications, $support]) => {
    // Calculate winner for each proposal
    // Updates instantly when new votes arrive
  }
);
```

## Example: Complete Reactive Game

```svelte
<script lang="ts">
  import { ReactiveP2PDecider } from './lib/p2p-decider-reactive.svelte';
  import { onMount, onDestroy } from 'svelte';
  
  export let user: any;
  export let gameId: string;
  
  let decider: ReactiveP2PDecider;
  let myInput = '';
  
  onMount(async () => {
    decider = new ReactiveP2PDecider(user, gameId);
    await decider.createGame(['What for dinner?']);
  });
  
  onDestroy(() => decider?.destroy());
  
  // Extract stores
  $: config = decider?.config;
  $: currentPhase = decider?.currentPhase;
  $: allProposals = decider?.allProposals;
  $: allChallenges = decider?.allChallenges;
  $: consensusResults = decider?.consensusResults;
  
  async function submit() {
    if ($currentPhase === 'proposing') {
      await decider.writeMyProposal(myInput);
    } else if ($currentPhase === 'challenging') {
      await decider.writeMyChallengeToProposal(selectedPub, myInput);
    }
    myInput = '';
  }
</script>

<div>
  <h1>P2P Dinner Decision</h1>
  
  <!-- Phase badge (auto-updates) -->
  <span class="phase">{$currentPhase}</span>
  
  <!-- Input section (changes based on phase) -->
  {#if $currentPhase !== 'complete'}
    <input bind:value={myInput} placeholder="Your input..." />
    <button on:click={submit}>Submit</button>
  {/if}
  
  <!-- Proposals (auto-populate as they arrive) -->
  <div class="proposals">
    {#each $allProposals || [] as proposal}
      <div class="proposal">
        <h3>{proposal.content}</h3>
        
        <!-- Challenges (auto-appear) -->
        {#if $allChallenges?.get(proposal.authorPub)}
          <div class="challenges">
            {#each $allChallenges.get(proposal.authorPub) as challenge}
              <p>⚠️ {challenge.content}</p>
            {/each}
          </div>
        {/if}
        
        <!-- Winner (auto-calculates) -->
        {#if $consensusResults?.get(proposal.authorPub)}
          <div class="winner">
            🏆 {$consensusResults.get(proposal.authorPub)}
          </div>
        {/if}
      </div>
    {/each}
  </div>
</div>
```

## Comparison: Non-Reactive vs Reactive

### Non-Reactive
```typescript
// Manual polling
setInterval(async () => {
  proposals = await decider.readAllProposals();
}, 5000);
```

**Code:** 👎 More complex  
**Performance:** 👎 Constant polling  
**Updates:** 👎 Delayed (5 seconds)  
**UX:** 👎 Laggy

### Reactive
```svelte
{#each $allProposals as proposal}
  <div>{proposal.content}</div>
{/each}
```

**Code:** ✨ Simple & declarative  
**Performance:** ✨ Event-driven  
**Updates:** ✨ Instant  
**UX:** ✨ Smooth

## Advanced Techniques

### Custom Derived Stores

```typescript
import { derived } from 'svelte/store';

// Proposals with challenge counts
$: proposalsWithChallenges = derived(
  [decider.allProposals, decider.allChallenges],
  ([$proposals, $challenges]) => {
    return $proposals.map(p => ({
      ...p,
      challengeCount: $challenges.get(p.authorPub)?.length || 0,
      isChallenged: ($challenges.get(p.authorPub)?.length || 0) > 0
    }));
  }
);
```

### Reactive Aggregations

```svelte
<script>
  // Auto-recalculates when stores update
  $: totalProposals = $allProposals?.length || 0;
  $: totalChallenges = Array.from($allChallenges?.values() || [])
    .reduce((sum, c) => sum + c.length, 0);
  $: totalVotes = Array.from($allSupport?.values() || [])
    .reduce((sum, s) => sum + s.length, 0);
  $: participantCount = $config?.participants.length || 0;
  $: progress = (totalVotes / (participantCount * totalProposals)) * 100;
</script>

<div class="stats">
  <p>Proposals: {totalProposals}</p>
  <p>Challenges: {totalChallenges}</p>
  <p>Votes: {totalVotes}</p>
  <progress value={progress} max="100"></progress>
</div>
```

### Conditional Rendering

```svelte
<script>
  $: canPropose = $currentPhase === 'proposing';
  $: canChallenge = $currentPhase === 'challenging';
  $: canVote = $currentPhase === 'supporting';
  $: hasResults = $currentPhase === 'complete';
</script>

<div class="actions">
  <button disabled={!canPropose}>Propose</button>
  <button disabled={!canChallenge}>Challenge</button>
  <button disabled={!canVote}>Vote</button>
</div>

{#if hasResults}
  <div class="celebration">🎉 Consensus Reached!</div>
{/if}
```

## Performance

### Listener Count
For N participants with M proposals:
- Proposal listeners: N
- Challenge listeners: N × M
- Comment listeners: N × M
- Modification listeners: N × M
- Support listeners: N × M

**Example (3 participants, 3 proposals):**
- 3 + (3 × 3 × 4) = **39 listeners**

### Optimizations
1. **Efficient Updates**: Svelte only re-renders changed parts
2. **Batched Network**: Holster batches network updates
3. **Memoized Derivations**: Derived stores only recalculate when dependencies change
4. **Smart Listeners**: Only listen to active game paths

## Debugging

### Log Store Changes

```typescript
decider.allProposals.subscribe($proposals => {
  console.log('Proposals updated:', $proposals);
});

decider.currentPhase.subscribe($phase => {
  console.log('Phase changed:', $phase);
});
```

### Inspect Current Values

```typescript
import { get } from 'svelte/store';

console.log('Current phase:', get(decider.currentPhase));
console.log('All proposals:', get(decider.allProposals));
```

## Best Practices

1. **Always clean up**: Call `destroy()` in `onDestroy`
2. **Use `$` syntax**: Simplest way to access stores
3. **Extract stores**: Use `$: store = decider?.store` pattern
4. **Null checks**: Use optional chaining (`?.`) for safety
5. **Derive, don't compute**: Use derived stores for complex calculations

## Files to Reference

- `/src/lib/p2p-decider-reactive.svelte.ts` - Implementation
- `/examples/ReactiveDeciderUI.svelte` - Complete UI example
- `/docs/p2p-reactive-guide.md` - Detailed guide
- `/docs/p2p-architecture.md` - Base P2P architecture
- `/docs/p2p-quick-reference.md` - API reference

## Next Steps

1. Try the example: `examples/ReactiveDeciderUI.svelte`
2. Read the guide: `docs/p2p-reactive-guide.md`
3. Build your own UI using the reactive decider
4. Extend with custom derived stores for your use case

---

**The reactive approach makes P2P consensus feel magical!** ✨  
Data flows from the network → through stores → into your UI automatically.

