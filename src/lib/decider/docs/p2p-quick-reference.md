# P2P Decider Quick Reference

## Core Concepts

- **Each player writes ONLY to their own user space**
- **Each player reads FROM ALL players' spaces**
- **Game ID identifies the session**
- **Public key identifies players and proposals**

## Path Template

```
user[playerPubKey]/games/[gameId]/[dataType]/[key]
                   └─────┬─────┘ └───┬───┘ └─┬─┘
                         │           │       │
                  Game session  Data type   Key
```

## Quick Path Reference

| What am I doing? | Path I write to | Paths I read from |
|-----------------|-----------------|-------------------|
| Create/join game | `user[me]/games/[gameId]/config` | `user[all]/games/[gameId]/config` |
| Submit proposal | `user[me]/games/[gameId]/proposals/[agendaIdx]` | `user[all]/games/[gameId]/proposals/[agendaIdx]` |
| Challenge proposal | `user[me]/games/[gameId]/challenges/[theirPub]` | `user[all]/games/[gameId]/challenges/[theirPub]` |
| Comment on proposal | `user[me]/games/[gameId]/comments/[theirPub]` | `user[all]/games/[gameId]/comments/[theirPub]` |
| Modify proposal | `user[me]/games/[gameId]/modifications/[theirPub]` | `user[all]/games/[gameId]/modifications/[theirPub]` |
| Vote on proposal | `user[me]/games/[gameId]/support/[theirPub]` | `user[all]/games/[gameId]/support/[theirPub]` |

## API Quick Reference

### Setup
```typescript
import { P2PDecider } from './lib/p2p-decider.svelte';

// User must be authenticated first
const user = holster.user();
await user.auth('username', 'password');

// Create decider instance
const decider = new P2PDecider(user, gameId);
```

### Creating a Game
```typescript
await decider.createGame(
  ['Prompt 1', 'Prompt 2'],  // Agenda
  [otherPlayer1Pub, otherPlayer2Pub]  // Other participants
);
```

### Joining a Game
```typescript
await decider.joinGame(creatorPubKey);
await decider.discoverParticipants();
```

### Write Operations (to my space)
```typescript
await decider.writeMyProposal(content);
await decider.writeMyChallengeToProposal(proposalAuthorPub, challenge);
await decider.writeMyCommentOnProposal(proposalAuthorPub, comment);
await decider.writeMyModificationToProposal(proposalAuthorPub, modification);
await decider.writeMySupportForProposal(proposalAuthorPub, supportDistribution);
```

### Read Operations (from all spaces)
```typescript
const proposals = await decider.readAllProposals();
const challenges = await decider.readAllChallengesForProposal(proposalAuthorPub);
const comments = await decider.readAllCommentsForProposal(proposalAuthorPub);
const modifications = await decider.readAllModificationsForProposal(proposalAuthorPub);
const support = await decider.readAllSupportForProposal(proposalAuthorPub);
```

### Running the Full Flow
```typescript
const passedProposals = await decider.runDecisionFlow();
```

## Data Types

### GameConfig
```typescript
{
  gameId: string,
  participants: string[],  // Public keys
  agenda: string[],
  currentAgendaIndex: number,
  timeWindow: number,
  createdAt: number,
  createdBy: string
}
```

### Proposal
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

### Challenge / Comment / Modification
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

### SupportExpression
```typescript
{
  [candidateContent: string]: number  // Points allocated
}
```

## Decision Flow Phases

```
1. CREATE/JOIN GAME
   ↓
2. EXPRESS PROPOSALS
   ↓ (wait for all)
3. EXPRESS CHALLENGES
   ↓ (wait for all)
4. EXPRESS COMMENTS & MODIFICATIONS
   ↓ (wait for all)
5. EXPRESS SUPPORT
   ↓ (wait for all)
6. CALCULATE CONSENSUS
```

## Example: 3-Player Game

### Players
- Alice: `pub_alice_123`
- Bob: `pub_bob_456`
- Charlie: `pub_charlie_789`

### Game ID
- `game_dinner_xyz`

### After Proposals (Agenda Index 0)

| Player | Path | Data |
|--------|------|------|
| Alice | `user[pub_alice_123]/games/game_dinner_xyz/proposals/0` | `{content: "Pizza", ...}` |
| Bob | `user[pub_bob_456]/games/game_dinner_xyz/proposals/0` | `{content: "Sushi", ...}` |
| Charlie | `user[pub_charlie_789]/games/game_dinner_xyz/proposals/0` | `{content: "Tacos", ...}` |

### After Challenges (to Alice's Pizza proposal)

| Player | Path | Data |
|--------|------|------|
| Bob | `user[pub_bob_456]/games/game_dinner_xyz/challenges/pub_alice_123` | `{content: "Too greasy", ...}` |
| Charlie | `user[pub_charlie_789]/games/game_dinner_xyz/challenges/pub_alice_123` | `{content: "Unhealthy", ...}` |

### After Modifications (to Alice's Pizza proposal)

| Player | Path | Data |
|--------|------|------|
| Alice | `user[pub_alice_123]/games/game_dinner_xyz/modifications/pub_alice_123` | `{content: "Veggie Pizza", ...}` |
| Bob | `user[pub_bob_456]/games/game_dinner_xyz/modifications/pub_alice_123` | `{content: "Thin Crust Pizza", ...}` |

### After Support (for Alice's Pizza proposal)

| Player | Path | Data |
|--------|------|------|
| Alice | `user[pub_alice_123]/games/game_dinner_xyz/support/pub_alice_123` | `{Pizza: 2, "Veggie Pizza": 8, "Thin Crust": 0}` |
| Bob | `user[pub_bob_456]/games/game_dinner_xyz/support/pub_alice_123` | `{Pizza: 0, "Veggie Pizza": 5, "Thin Crust": 5}` |
| Charlie | `user[pub_charlie_789]/games/game_dinner_xyz/support/pub_alice_123` | `{Pizza: 1, "Veggie Pizza": 6, "Thin Crust": 3}` |

### Consensus Calculation

```
Pizza:          2 + 0 + 1 = 3 points
Veggie Pizza:   8 + 5 + 6 = 19 points ← WINNER
Thin Crust:     0 + 5 + 3 = 8 points
```

## Common Patterns

### Pattern: I want to participate in the decision flow
```typescript
// 1. Setup
const decider = new P2PDecider(user, gameId);
await decider.joinGame(creatorPub);

// 2. Run flow (handles all phases automatically)
const results = await decider.runDecisionFlow();
```

### Pattern: I want custom UI for player actions
```typescript
class MyCustomDecider extends P2PDecider {
  async expressProposals(prompt: string): Promise<void> {
    const userInput = await showUIPrompt(prompt);
    await this.writeMyProposal(userInput);
  }
  
  async expressChallenges(proposal: any): Promise<void> {
    const shouldChallenge = await showUIYesNo('Challenge?');
    if (shouldChallenge) {
      const challenge = await showUIInput('Your challenge:');
      await this.writeMyChallengeToProposal(proposal.authorPub, challenge);
    }
  }
  
  // ... override other methods
}
```

### Pattern: I want real-time updates
```typescript
// Listen for changes to a specific proposal
const unsubscribe = decider.setupProposalListeners(
  proposalAuthorPub,
  (updatedProposal) => {
    updateUI(updatedProposal);
  }
);

// Later: stop listening
unsubscribe();
```

### Pattern: I want to manually control each phase
```typescript
// Phase 1: Proposals
await decider.expressProposals(prompt);
await waitForEveryone();
const proposals = await decider.readAllProposals();

// Phase 2: Challenges
for (const proposal of proposals) {
  await decider.expressChallenges(proposal);
}
await waitForEveryone();

// ... continue with other phases
```

## Troubleshooting

### Q: I'm not seeing other players' data
- Verify they've written to the correct path
- Check you have their public key in participants list
- Ensure game ID matches exactly
- Try `await decider.discoverParticipants()` again

### Q: How do I know when everyone has submitted?
- Currently uses time windows (default 24 hours)
- Can poll periodically: `setInterval(() => checkForNewData(), 5000)`
- Can set up listeners for real-time updates
- Future: implement ready/done signals

### Q: What if someone submits late?
- Time windows allow late submissions
- Each player independently decides when to read
- Late data will be included if read after submission
- Consider: wait until near end of time window

### Q: Can players join mid-game?
- Yes, they can join at any time
- They should catch up by reading all existing data
- They may miss earlier voting phases
- Future: implement catch-up mechanisms

## Files to Reference

- `/src/lib/utils/holsterData.ts` - Path utilities
- `/src/lib/p2p-decider.svelte.ts` - Main P2P implementation
- `/docs/p2p-architecture.md` - Detailed architecture docs
- `/docs/p2p-path-patterns.md` - Read/write patterns
- `/examples/p2p-decider-example.ts` - Usage examples

