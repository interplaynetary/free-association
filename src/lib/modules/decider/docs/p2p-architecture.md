# P2P Decider Architecture with Holster

## Overview

The P2P Decider implements a distributed consensus system where each player operates independently while synchronizing data through Holster's user space. Unlike the centralized architecture where a single Decider orchestrates all players, the P2P architecture has each player writing to their own user space and reading from all other players' spaces.

## Core Principles

1. **Peer Autonomy**: Each player writes only to their own user space
2. **Public Readable**: All data is readable by other participants
3. **No Central Coordinator**: Each player independently executes the decision flow
4. **Eventual Consistency**: Players synchronize by reading from all participants
5. **Identity-Based**: Players are identified by their Holster public keys

## Data Path Structure

Each player's data is stored in their authenticated user space following this hierarchy:

```
user[playerPublicKey]/
  games/
    [gameId]/
      config: GameConfig
      proposals/
        [agendaItemIndex]: Proposal
      challenges/
        [proposalAuthorPub]: Challenge
      comments/
        [proposalAuthorPub]: Comment
      modifications/
        [proposalAuthorPub]: ModificationProposal
      support/
        [proposalAuthorPub]: SupportExpression
```

### Path Components

#### `/games/[gameId]/config`
Stores the game configuration including:
- `gameId`: Unique identifier for the game session
- `participants`: Array of participant public keys
- `agenda`: Array of prompts/topics to decide on
- `currentAgendaIndex`: Current agenda item being processed
- `timeWindow`: Time limit for each phase (in milliseconds)
- `createdAt`: Timestamp of game creation
- `createdBy`: Public key of game creator

**Who reads**: All participants read from all other participants to discover the full participant list
**Who writes**: Each participant writes their own copy

#### `/games/[gameId]/proposals/[agendaItemIndex]`
Each player's proposal for a specific agenda item.
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Who reads**: All participants read from all other participants
**Who writes**: Each participant writes their own proposal once per agenda item

#### `/games/[gameId]/challenges/[proposalAuthorPub]`
Each player's challenge to a specific proposal (identified by the proposal author's public key).
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Who reads**: All participants read from all other participants for each proposal
**Who writes**: Each participant writes one challenge per proposal they wish to challenge

#### `/games/[gameId]/comments/[proposalAuthorPub]`
Each player's comment on a specific proposal.
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Who reads**: All participants read from all other participants for each proposal
**Who writes**: Each participant writes one comment per proposal

#### `/games/[gameId]/modifications/[proposalAuthorPub]`
Each player's modification proposal for a specific proposal.
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Who reads**: All participants read from all other participants for each proposal
**Who writes**: Each participant writes one modification per proposal (optional)

#### `/games/[gameId]/support/[proposalAuthorPub]`
Each player's support distribution for a specific proposal's candidates.
```typescript
{
  [candidateContent: string]: number  // points allocated to each candidate
}
```

**Who reads**: All participants read from all other participants for each proposal
**Who writes**: Each participant writes their support distribution once per proposal

## Decision Flow

### Phase 1: Game Initialization

1. **Creator creates game**:
   ```typescript
   const decider = new P2PDecider(user, gameId);
   await decider.createGame(
     ['What should we do?', 'How should we do it?'],
     [otherPlayer1PubKey, otherPlayer2PubKey]
   );
   ```

2. **Others join game**:
   ```typescript
   const decider = new P2PDecider(user, gameId);
   await decider.joinGame(creatorPubKey);
   ```

3. **Discover all participants**:
   ```typescript
   const allParticipants = await decider.discoverParticipants();
   ```

### Phase 2: Express Proposals

Each player writes their proposal to their own space:
```typescript
await decider.writeMyProposal('Implement new training program');
```

**Path**: `user[myPubKey]/games/[gameId]/proposals/[agendaIndex]`

### Phase 3: Express Challenges

Each player reads all proposals, then writes challenges:
```typescript
const proposals = await decider.readAllProposals();
for (const proposal of proposals) {
  // Decide whether to challenge
  if (shouldChallenge(proposal)) {
    await decider.writeMyChallengeToProposal(
      proposal.authorPub,
      'Needs more detail'
    );
  }
}
```

**Read from**: `user[eachParticipant]/games/[gameId]/proposals/[agendaIndex]`  
**Write to**: `user[myPubKey]/games/[gameId]/challenges/[proposalAuthorPub]`

### Phase 4: Express Comments & Modifications

For proposals with challenges, each player writes comments and modifications:
```typescript
await decider.writeMyCommentOnProposal(
  proposalAuthorPub,
  'This could work with adjustments'
);

await decider.writeMyModificationToProposal(
  proposalAuthorPub,
  'Modified version of the proposal'
);
```

**Read from**: `user[eachParticipant]/games/[gameId]/challenges/[proposalAuthorPub]`  
**Write to**: 
- `user[myPubKey]/games/[gameId]/comments/[proposalAuthorPub]`
- `user[myPubKey]/games/[gameId]/modifications/[proposalAuthorPub]`

### Phase 5: Express Support

Each player allocates points across all proposal candidates:
```typescript
const support = {
  'Original proposal': 5,
  'Modified version 1': 8,
  'Modified version 2': 2
};
await decider.writeMySupportForProposal(proposalAuthorPub, support);
```

**Read from**: `user[eachParticipant]/games/[gameId]/modifications/[proposalAuthorPub]`  
**Write to**: `user[myPubKey]/games/[gameId]/support/[proposalAuthorPub]`

### Phase 6: Determine Consensus

Each player independently reads all support and calculates the winner:
```typescript
const allSupport = await decider.readAllSupportForProposal(proposalAuthorPub);
proposal.supportExpressions = allSupport;
const winner = proposal.mostSupportedVersion();
```

**Read from**: `user[eachParticipant]/games/[gameId]/support/[proposalAuthorPub]`

## Synchronization Strategy

### Time-Based Windows

Each phase has a time window (default 24 hours). Players can:
1. Write immediately when ready
2. Read at any time during the window
3. Wait until near the end to read all contributions

### Event-Based Listeners

For real-time updates, players can set up listeners:
```typescript
const unsubscribe = decider.setupProposalListeners(
  proposalAuthorPub,
  (updatedProposal) => {
    console.log('Proposal updated:', updatedProposal);
  }
);
```

### Polling

Players can poll for updates at intervals:
```typescript
setInterval(async () => {
  const proposals = await decider.readAllProposals();
  // Update UI
}, 5000);
```

## Key Design Decisions

### Why use authorPub as key for challenges/comments/modifications/support?

This creates a one-to-one mapping between proposals and responses. Each player can only submit one challenge, one comment, one modification, and one support distribution per proposal. This simplifies aggregation and prevents spam.

### Why store agenda index instead of proposal content as key?

Proposals are written independently and simultaneously. Using the agenda index as the key allows multiple players to write different proposals for the same prompt without collision. The author's public key in the proposal data distinguishes between different players' proposals.

### Why does each player maintain their own config copy?

This enables peer discovery without a central registry. New participants can join by reading any participant's config, and they announce their presence by writing their own config. Over time, all participants discover each other through config synchronization.

### Why read from everyone every time?

This ensures eventual consistency without complex synchronization protocols. Each player independently aggregates the current state by reading from all known participants. This is robust against network partitions and late joiners.

## Example Usage

```typescript
// Player 1 (Creator)
const user1 = holster.user();
await user1.auth('player1', 'password');

const decider1 = new P2PDecider(user1, 'game-123');
await decider1.createGame(
  ['What should we build?'],
  [player2PubKey, player3PubKey]
);

// Player 2 (Joiner)
const user2 = holster.user();
await user2.auth('player2', 'password');

const decider2 = new P2PDecider(user2, 'game-123');
await decider2.joinGame(player1PubKey);

// Both players run the decision flow independently
const results1 = await decider1.runDecisionFlow();
const results2 = await decider2.runDecisionFlow();

// Both should reach the same consensus
console.log('Player 1 results:', results1);
console.log('Player 2 results:', results2);
```

## Future Enhancements

1. **Encryption**: Encrypt game data so only participants can read it
2. **Signatures**: Verify all data is signed by the claimed author
3. **Conflict Resolution**: Handle edge cases where players disagree on state
4. **Late Joiners**: Allow players to join mid-game and catch up
5. **Phase Transitions**: Automatic phase transitions based on participation threshold
6. **Proposal Deduplication**: Detect and merge similar proposals
7. **Ranked Choice Support**: More sophisticated voting mechanisms
8. **Game History**: Store and query past decisions
9. **Multi-Agenda Support**: Process multiple agenda items in parallel

