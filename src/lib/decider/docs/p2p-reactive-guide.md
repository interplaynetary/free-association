# Reactive P2P Decider Guide

## Overview

The Reactive P2P Decider uses **Svelte stores** to provide real-time, reactive game state management. As data arrives from the network through Holster, stores automatically update and the UI reactively re-renders.

## Architecture

### Store Types

#### 1. Writable Stores (Raw Data)
These stores hold raw data as it arrives from the network:

```typescript
{
  config: Writable<GameConfig | null>
  participants: Writable<string[]>
  participantStores: {
    proposals: Writable<Map<participantPub, ProposalData>>
    challenges: Writable<Map<participantPub, Map<proposalAuthorPub, Challenge>>>
    comments: Writable<Map<participantPub, Map<proposalAuthorPub, Comment>>>
    modifications: Writable<Map<participantPub, Map<proposalAuthorPub, Modification>>>
    support: Writable<Map<participantPub, Map<proposalAuthorPub, SupportExpression>>>
  }
}
```

#### 2. Derived Stores (Aggregated Data)
These stores automatically compute aggregated views from the raw data:

```typescript
{
  allProposals: Readable<ProposalData[]>
  allChallenges: Readable<Map<proposalAuthorPub, Challenge[]>>
  allComments: Readable<Map<proposalAuthorPub, Comment[]>>
  allModifications: Readable<Map<proposalAuthorPub, ModificationProposal[]>>
  allSupport: Readable<Map<proposalAuthorPub, SupportExpression[]>>
  currentPhase: Readable<GamePhase>
  consensusResults: Readable<Map<proposalAuthorPub, winningContent>>
  isReady: Readable<boolean>
}
```

## Data Flow

```
Holster Network
    ↓
Holster Listener (listenAtPath)
    ↓
Writable Store Update
    ↓
Derived Store Recalculation (automatic)
    ↓
Svelte Component Re-render (automatic)
    ↓
UI Update
```

## Usage Examples

### Basic Setup

```typescript
import { ReactiveP2PDecider } from './lib/p2p-decider-reactive.svelte';
import { onMount, onDestroy } from 'svelte';

let decider: ReactiveP2PDecider;

onMount(async () => {
  // Create reactive decider
  decider = new ReactiveP2PDecider(user, gameId);
  
  // Initialize game
  await decider.createGame(['What should we do?'], [otherPlayerPub]);
  
  // Stores are now active and updating automatically!
});

onDestroy(() => {
  // Clean up listeners
  decider?.destroy();
});
```

### Accessing Store Values

#### In Svelte Components (Reactive)

Use the `$` prefix to automatically subscribe and re-render:

```svelte
<script>
  let decider: ReactiveP2PDecider;
  // ... initialize decider
</script>

<!-- Automatically reactive -->
<p>Current Phase: {$decider.currentPhase}</p>
<p>Proposal Count: {$decider.allProposals.length}</p>

{#if $decider.currentPhase === 'supporting'}
  <button>Vote Now!</button>
{/if}

{#each $decider.allProposals as proposal}
  <div>{proposal.content}</div>
{/each}
```

#### In JavaScript (Non-Reactive)

Use `get()` to read current value without subscribing:

```typescript
import { get } from 'svelte/store';

const currentPhase = get(decider.currentPhase);
const proposals = get(decider.allProposals);
```

#### Manual Subscription

```typescript
const unsubscribe = decider.allProposals.subscribe(proposals => {
  console.log('Proposals changed:', proposals);
  // Do something with proposals
});

// Later: clean up
unsubscribe();
```

### Writing Data

All write operations remain the same as the non-reactive version:

```typescript
// Write my proposal
await decider.writeMyProposal('My great idea');

// Write a challenge
await decider.writeMyChallengeToProposal(theirPub, 'Needs improvement');

// Write a comment
await decider.writeMyCommentOnProposal(theirPub, 'I like this!');

// Write a modification
await decider.writeMyModificationToProposal(theirPub, 'Modified version');

// Write support
await decider.writeMySupportForProposal(theirPub, {
  'Original': 3,
  'Modified': 7
});
```

## Reactive Features

### 1. Auto-Updating Phase Detection

The `currentPhase` store automatically determines the game phase based on available data:

```svelte
<script>
  // No manual phase management needed!
</script>

{#if $decider.currentPhase === 'proposing'}
  <input placeholder="Enter your proposal" />
{:else if $decider.currentPhase === 'challenging'}
  <button>Challenge Proposals</button>
{:else if $decider.currentPhase === 'commenting'}
  <button>Add Comments</button>
{:else if $decider.currentPhase === 'supporting'}
  <button>Vote</button>
{:else if $decider.currentPhase === 'complete'}
  <h2>Results</h2>
{/if}
```

### 2. Real-Time Proposal Updates

As proposals arrive from other players, the UI automatically updates:

```svelte
<!-- This list grows automatically as players submit proposals -->
<div class="proposals">
  <h3>Proposals ({$decider.allProposals.length})</h3>
  {#each $decider.allProposals as proposal}
    <div class="proposal-card">
      <p>{proposal.content}</p>
      <small>by {proposal.authorPub}</small>
      
      <!-- Challenges appear automatically -->
      {#if $decider.allChallenges.get(proposal.authorPub)?.length}
        <div class="challenges">
          {#each $decider.allChallenges.get(proposal.authorPub) as challenge}
            <p>⚠️ {challenge.content}</p>
          {/each}
        </div>
      {/if}
    </div>
  {/each}
</div>
```

### 3. Live Consensus Calculation

As support votes come in, the winning proposal is automatically recalculated:

```svelte
{#if $decider.currentPhase === 'complete' || $decider.allSupport.size > 0}
  <div class="results">
    {#each $decider.allProposals as proposal}
      <div class="result">
        <h4>Proposal by {proposal.authorPub}</h4>
        <p class="winner">
          Winner: {$decider.consensusResults.get(proposal.authorPub)}
        </p>
      </div>
    {/each}
  </div>
{/if}
```

### 4. Participant Count Updates

As new players join, the UI reflects the updated count:

```svelte
<div class="header">
  <p>Participants: {$decider.participants.length}</p>
  <ul>
    {#each $decider.participants as participantPub}
      <li>{participantPub.slice(0, 8)}...</li>
    {/each}
  </ul>
</div>
```

## Advanced Patterns

### Combining Multiple Stores

Create custom derived stores that combine multiple sources:

```typescript
import { derived } from 'svelte/store';

// Custom store: proposals with their challenge counts
const proposalsWithChallenges = derived(
  [decider.allProposals, decider.allChallenges],
  ([$proposals, $challenges]) => {
    return $proposals.map(proposal => ({
      ...proposal,
      challengeCount: $challenges.get(proposal.authorPub)?.length || 0
    }));
  }
);
```

```svelte
{#each $proposalsWithChallenges as item}
  <div>
    {item.content} ({item.challengeCount} challenges)
  </div>
{/each}
```

### Conditional Rendering Based on Phase

```svelte
<script>
  $: canPropose = $decider.currentPhase === 'proposing';
  $: canChallenge = $decider.currentPhase === 'challenging';
  $: canVote = $decider.currentPhase === 'supporting';
  $: isComplete = $decider.currentPhase === 'complete';
</script>

<div class="actions">
  <button disabled={!canPropose}>Propose</button>
  <button disabled={!canChallenge}>Challenge</button>
  <button disabled={!canVote}>Vote</button>
</div>

{#if isComplete}
  <div class="confetti">🎉 Consensus Reached!</div>
{/if}
```

### Reactive Aggregations

```svelte
<script>
  // These automatically recalculate when stores update
  $: totalProposals = $decider.allProposals.length;
  $: totalChallenges = Array.from($decider.allChallenges.values())
    .reduce((sum, challenges) => sum + challenges.length, 0);
  $: totalComments = Array.from($decider.allComments.values())
    .reduce((sum, comments) => sum + comments.length, 0);
  $: totalVotes = Array.from($decider.allSupport.values())
    .reduce((sum, support) => sum + support.length, 0);
</script>

<div class="stats">
  <div>Proposals: {totalProposals}</div>
  <div>Challenges: {totalChallenges}</div>
  <div>Comments: {totalComments}</div>
  <div>Votes: {totalVotes}</div>
</div>
```

### Progress Tracking

```svelte
<script>
  $: participantCount = $decider.participants.length;
  $: proposalCount = $decider.allProposals.length;
  $: proposalProgress = (proposalCount / participantCount) * 100;
  
  $: voteCount = Array.from($decider.allSupport.values())
    .reduce((sum, support) => sum + support.length, 0);
  $: expectedVotes = participantCount * proposalCount;
  $: voteProgress = (voteCount / expectedVotes) * 100;
</script>

<div class="progress">
  <h3>Proposal Progress</h3>
  <progress value={proposalProgress} max="100"></progress>
  <span>{proposalCount} / {participantCount} submitted</span>
</div>

<div class="progress">
  <h3>Voting Progress</h3>
  <progress value={voteProgress} max="100"></progress>
  <span>{voteCount} / {expectedVotes} votes cast</span>
</div>
```

## Listener Management

### Automatic Listener Setup

When you call `createGame()` or `joinGame()`, the reactive decider automatically sets up listeners for all data types from all participants:

```typescript
// This automatically creates listeners for:
// - All participants' proposals
// - All participants' challenges to all proposals
// - All participants' comments, modifications, support
await decider.createGame(['Topic 1'], [player2Pub, player3Pub]);

// Listeners are now active and updating stores!
```

### Cleanup

Always clean up listeners when component unmounts:

```svelte
<script>
  import { onDestroy } from 'svelte';
  
  onDestroy(() => {
    decider?.destroy();  // Removes all listeners
  });
</script>
```

## Performance Considerations

### Listener Count

The reactive decider creates many listeners:
- 1 proposal listener per participant
- N×N challenge listeners (N participants × N proposals)
- Same for comments, modifications, support

For 3 participants:
- 3 proposal listeners
- 9 challenge listeners  
- 9 comment listeners
- 9 modification listeners
- 9 support listeners
- **Total: 39 listeners**

### Optimization Strategies

1. **Lazy Loading**: Only set up listeners when needed:
```typescript
// Instead of listening to all proposals from the start,
// set up proposal listeners only after join
```

2. **Batch Updates**: Holster batches network updates automatically

3. **Derived Store Memoization**: Svelte only recalculates derived stores when dependencies change

4. **Virtual Scrolling**: For large proposal lists, use virtual scrolling

## Comparison: Non-Reactive vs Reactive

### Non-Reactive Version
```typescript
// Manual read
const proposals = await decider.readAllProposals();

// Manual polling
setInterval(async () => {
  const proposals = await decider.readAllProposals();
  updateUI(proposals);
}, 5000);
```

**Downsides:**
- Manual polling required
- Delayed updates
- More network requests
- Manual UI updates

### Reactive Version
```svelte
<!-- Automatic updates -->
{#each $decider.allProposals as proposal}
  <div>{proposal.content}</div>
{/each}
```

**Benefits:**
- Instant updates
- No polling needed
- Efficient network usage (listeners)
- Automatic UI updates

## Example: Complete Game Flow

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
  
  async function handleAction() {
    if (!myInput.trim()) return;
    
    const phase = $decider.currentPhase;
    
    if (phase === 'proposing') {
      await decider.writeMyProposal(myInput);
    } else if (phase === 'challenging') {
      await decider.writeMyChallengeToProposal(selectedProposal, myInput);
    } else if (phase === 'commenting') {
      await decider.writeMyCommentOnProposal(selectedProposal, myInput);
    }
    
    myInput = '';
  }
</script>

<div class="game">
  <h1>P2P Dinner Decision</h1>
  
  <!-- Phase indicator (auto-updates) -->
  <div class="phase">
    Current Phase: {$decider.currentPhase}
  </div>
  
  <!-- Input (changes based on phase) -->
  {#if $decider.currentPhase !== 'complete'}
    <input 
      bind:value={myInput}
      placeholder={
        $decider.currentPhase === 'proposing' ? 'Your proposal...' :
        $decider.currentPhase === 'challenging' ? 'Your challenge...' :
        'Your comment...'
      }
    />
    <button on:click={handleAction}>Submit</button>
  {/if}
  
  <!-- Proposals (auto-updates as they arrive) -->
  <div class="proposals">
    {#each $decider.allProposals as proposal}
      <div class="proposal">
        <h3>{proposal.content}</h3>
        
        <!-- Challenges (auto-appears) -->
        {#if $decider.allChallenges.get(proposal.authorPub)}
          <div class="challenges">
            {#each $decider.allChallenges.get(proposal.authorPub) as challenge}
              <p>⚠️ {challenge.content}</p>
            {/each}
          </div>
        {/if}
        
        <!-- Winner (auto-calculates) -->
        {#if $decider.currentPhase === 'complete'}
          <div class="winner">
            🏆 {$decider.consensusResults.get(proposal.authorPub)}
          </div>
        {/if}
      </div>
    {/each}
  </div>
</div>
```

## Debugging

### Log Store Changes

```typescript
// Subscribe to any store to log changes
decider.allProposals.subscribe(proposals => {
  console.log('Proposals updated:', proposals);
});

decider.currentPhase.subscribe(phase => {
  console.log('Phase changed:', phase);
});
```

### Inspect Store Values in Console

```typescript
import { get } from 'svelte/store';

// In browser console or debug code
console.log('Current phase:', get(decider.currentPhase));
console.log('All proposals:', get(decider.allProposals));
console.log('Consensus:', get(decider.consensusResults));
```

## Best Practices

1. **Always clean up**: Call `destroy()` in `onDestroy`
2. **Use `$` syntax**: Simplest way to access stores in Svelte
3. **Derive, don't compute**: Use `derived` stores instead of reactive statements for complex calculations
4. **Minimize subscriptions**: Let Svelte handle subscriptions with `$` syntax
5. **Test phases**: Ensure UI handles all phase transitions smoothly

## See Also

- `/src/lib/p2p-decider-reactive.svelte.ts` - Implementation
- `/examples/ReactiveDeciderUI.svelte` - Complete UI example
- [Svelte Store Documentation](https://svelte.dev/docs#run-time-svelte-store)

