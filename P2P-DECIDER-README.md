# P2P Decider with Holster

A distributed consensus system built on Holster that enables multiple players to collaboratively make decisions through proposals, challenges, comments, modifications, and support voting.

## What is This?

This is a **peer-to-peer (P2P) architecture** for the Decider consensus system. Unlike the original centralized version where a single coordinator manages all players, this P2P version has each player operate independently while synchronizing through Holster's user space.

### Key Innovation: Path-Based Data Structure

Each player writes to **their own Holster user space** and reads from **all other players' spaces**. Data is organized by path using this structure:

```
user[playerPublicKey]/games/[gameId]/[dataType]/[key]
```

## Files Created

### Core Implementation
- **`src/lib/utils/holsterData.ts`** - Generic utilities for writing/reading at Holster paths
  - `writeAtPath()` - Write data to a specific path in user space
  - `readAtPath()` - Read data from a specific path
  - `listenAtPath()` - Set up real-time listeners for path changes

- **`src/lib/p2p-decider.svelte.ts`** - P2P Decider implementation
  - `P2PDecider` class with full decision flow
  - Write operations (to my own space)
  - Read operations (from all players' spaces)
  - Game initialization and participant discovery

### Documentation
- **`docs/p2p-architecture.md`** - Comprehensive architecture documentation
  - Core principles and design decisions
  - Complete path structure documentation
  - Decision flow phases
  - Synchronization strategies
  - Future enhancements

- **`docs/p2p-path-patterns.md`** - Read/write patterns reference
  - Detailed pattern summary table
  - Flow diagrams for each data type
  - Code examples
  - Advantages and limitations

- **`docs/p2p-quick-reference.md`** - Quick reference guide
  - Path templates
  - API quick reference
  - Common patterns
  - Example usage
  - Troubleshooting

### Examples
- **`examples/p2p-decider-example.ts`** - Working examples
  - Simple 2-player game
  - Custom player actions
  - Path visualization
  - Real-time listeners
  - Multi-agenda games

## Quick Start

### 1. Install Dependencies

```bash
npm install zod
# Also need Holster - see resources/holster.wiki/
```

### 2. Create and Authenticate Users

```typescript
import Holster from './path/to/holster';
const holster = Holster();

const user = holster.user();
await user.create('alice', 'password123');
await user.auth('alice', 'password123');
```

### 3. Create a P2P Decider Instance

```typescript
import { P2PDecider } from './src/lib/p2p-decider.svelte';

const gameId = 'game-' + Date.now();
const decider = new P2PDecider(user, gameId);
```

### 4. Create or Join a Game

```typescript
// Creator
await decider.createGame(
  ['What should we do?'],  // Agenda
  [bobPubKey, charliePubKey]  // Other participants
);

// Joiner
await decider.joinGame(alicePubKey);  // Join Alice's game
```

### 5. Run the Decision Flow

```typescript
const results = await decider.runDecisionFlow();
console.log('Consensus reached:', results);
```

## Architecture at a Glance

### The Problem This Solves

In a centralized architecture:
- Single coordinator becomes bottleneck
- Coordinator failure breaks the system
- Players must trust the coordinator
- Not truly peer-to-peer

### The P2P Solution

Each player:
1. **Writes** only to their own user space
2. **Reads** from all players' spaces to aggregate data
3. **Independently** executes the decision flow
4. **Reaches the same consensus** by following the same rules

### Path Structure Example

For a game with Alice, Bob, and Charlie discussing "What for dinner?":

```
user[alice_pub]/
  games/
    game-123/
      config: {participants: [alice, bob, charlie], ...}
      proposals/
        0: {content: "Pizza", timestamp: ..., authorPub: alice_pub}
      challenges/
        bob_pub: {content: "Too expensive", ...}
      modifications/
        bob_pub: {content: "Sushi with discount", ...}
      support/
        bob_pub: {Sushi: 3, "Sushi with discount": 7}

user[bob_pub]/
  games/
    game-123/
      config: {participants: [alice, bob, charlie], ...}
      proposals/
        0: {content: "Sushi", timestamp: ..., authorPub: bob_pub}
      challenges/
        alice_pub: {content: "I prefer cooked food", ...}
      ...

user[charlie_pub]/
  games/
    game-123/
      ...
```

### Decision Flow

```
┌─────────────────────┐
│  CREATE/JOIN GAME   │
│  Establish session  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ EXPRESS PROPOSALS   │
│ Each: write my idea │
│ All: read everyone  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ EXPRESS CHALLENGES  │
│ For each proposal:  │
│   Write if object   │
│   Read all opinions │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ COMMENTS & MODS     │
│ Write my thoughts   │
│ Write alternatives  │
│ Read everyone's     │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ EXPRESS SUPPORT     │
│ Allocate points to  │
│ all candidates      │
│ Read all votes      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ CALCULATE CONSENSUS │
│ Sum support points  │
│ Highest wins        │
└─────────────────────┘
```

## Key Concepts

### Who Reads From Where?

| Data Type | I write to | I read from |
|-----------|------------|-------------|
| Config | `user[me]/games/[gameId]/config` | `user[*]/games/[gameId]/config` |
| Proposal | `user[me]/games/[gameId]/proposals/[idx]` | `user[*]/games/[gameId]/proposals/[idx]` |
| Challenge | `user[me]/games/[gameId]/challenges/[theirPub]` | `user[*]/games/[gameId]/challenges/[theirPub]` |
| Support | `user[me]/games/[gameId]/support/[theirPub]` | `user[*]/games/[gameId]/support/[theirPub]` |

**Legend:** `[me]` = my public key, `[*]` = all participants

### Why This Design?

1. **No Write Conflicts** - Each player only writes to their own space
2. **Transparent** - All data is publicly readable (within game)
3. **Fault Tolerant** - One player's absence doesn't block others
4. **Eventually Consistent** - All players reach same conclusion
5. **Auditable** - Complete history of who wrote what when
6. **Holster Native** - Uses Holster's strengths (signed data, P2P sync)

## Customizing Player Behavior

The default implementation uses mock data for player actions. Override these methods for real UI/AI:

```typescript
class MyCustomDecider extends P2PDecider {
  async expressProposals(prompt: string): Promise<void> {
    const userInput = await getInputFromUser(prompt);
    await this.writeMyProposal(userInput);
  }

  async expressChallenges(proposal: Proposal): Promise<void> {
    if (await userWantsToChallenge(proposal)) {
      const challenge = await getChallengeFromUser(proposal);
      await this.writeMyChallengeToProposal(proposal.authorPub, challenge);
    }
  }

  // Override other methods...
}
```

## Real-time Updates

Set up listeners for live updates:

```typescript
const unsubscribe = decider.setupProposalListeners(
  proposalAuthorPub,
  (updatedProposal) => {
    console.log('New data:', updatedProposal);
    updateUI(updatedProposal);
  }
);

// Later: stop listening
unsubscribe();
```

## Documentation Guide

1. **Start here:** `P2P-DECIDER-README.md` (this file) - Overview
2. **Quick usage:** `docs/p2p-quick-reference.md` - API and examples
3. **Path patterns:** `docs/p2p-path-patterns.md` - Read/write details
4. **Deep dive:** `docs/p2p-architecture.md` - Complete architecture
5. **Code examples:** `examples/p2p-decider-example.ts` - Working code

## Comparison: Centralized vs P2P

### Centralized (Original)
```typescript
const decider = new Decider(['alice', 'bob']);
const results = await decider.expressProposals('What to do?');
// Decider coordinates everything centrally
```

**Pros:**
- Simple to understand
- Predictable execution order
- Easy debugging

**Cons:**
- Single point of failure
- Not truly distributed
- Coordinator bottleneck

### P2P (This Implementation)
```typescript
// Each player runs independently
const aliceDecider = new P2PDecider(aliceUser, gameId);
const bobDecider = new P2PDecider(bobUser, gameId);

await aliceDecider.createGame(['What to do?'], [bobPub]);
await bobDecider.joinGame(alicePub);

// Both run independently, reach same consensus
const aliceResults = await aliceDecider.runDecisionFlow();
const bobResults = await bobDecider.runDecisionFlow();
```

**Pros:**
- Truly distributed
- No coordinator needed
- Fault tolerant
- Scalable

**Cons:**
- More complex synchronization
- Eventual consistency (not immediate)
- Higher read overhead

## Future Enhancements

- **Encryption**: Private games visible only to participants
- **Signatures**: Verify data authenticity
- **Smart Sync**: Gossip protocol to reduce reads
- **Phase Signals**: Automatic phase transitions
- **Late Joiners**: Catch-up mechanisms
- **Ranked Choice**: Advanced voting algorithms
- **Multi-Agenda**: Parallel decision streams

## Testing

To test with mock Holster users:

```typescript
// See examples/p2p-decider-example.ts for full examples
import { example3_PathVisualization } from './examples/p2p-decider-example';
example3_PathVisualization();
```

To test with real Holster:

1. Set up Holster server (see `resources/holster.wiki/`)
2. Create authenticated users
3. Run the examples with real user instances

## Contributing

When adding features:
1. Maintain the "write to own space, read from all" pattern
2. Update path documentation in `p2p-path-patterns.md`
3. Add examples to `p2p-decider-example.ts`
4. Update this README

## License

[Your license here]

## Questions?

Refer to:
- `docs/p2p-quick-reference.md` for API questions
- `docs/p2p-path-patterns.md` for path structure questions
- `docs/p2p-architecture.md` for design questions
- `examples/p2p-decider-example.ts` for usage questions

