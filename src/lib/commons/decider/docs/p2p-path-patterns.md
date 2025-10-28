# P2P Path Patterns: Who Reads & Writes Where

This document provides a quick reference for understanding the read/write patterns in the P2P Decider architecture.

## Pattern Summary

| Phase | Write Path | Read Paths | Purpose |
|-------|-----------|------------|---------|
| **Config** | `user[myPub]/games/[gameId]/config` | `user[*]/games/[gameId]/config` | Discover participants |
| **Proposals** | `user[myPub]/games/[gameId]/proposals/[agendaIdx]` | `user[*]/games/[gameId]/proposals/[agendaIdx]` | Collect all proposals |
| **Challenges** | `user[myPub]/games/[gameId]/challenges/[proposalAuthorPub]` | `user[*]/games/[gameId]/challenges/[proposalAuthorPub]` | Collect challenges to a proposal |
| **Comments** | `user[myPub]/games/[gameId]/comments/[proposalAuthorPub]` | `user[*]/games/[gameId]/comments/[proposalAuthorPub]` | Collect comments on a proposal |
| **Modifications** | `user[myPub]/games/[gameId]/modifications/[proposalAuthorPub]` | `user[*]/games/[gameId]/modifications/[proposalAuthorPub]` | Collect alternative versions |
| **Support** | `user[myPub]/games/[gameId]/support/[proposalAuthorPub]` | `user[*]/games/[gameId]/support/[proposalAuthorPub]` | Collect votes |

**Legend:**
- `[myPub]` = My public key (I write here)
- `[*]` = All participants' public keys (I read from everyone)
- `[gameId]` = Unique game identifier
- `[agendaIdx]` = Index of current agenda item (0, 1, 2, ...)
- `[proposalAuthorPub]` = Public key of the proposal's author

## Detailed Read/Write Patterns

### 1. Game Configuration

```
WRITE: user[myPub]/games/[gameId]/config
READ:  user[alice]/games/[gameId]/config
       user[bob]/games/[gameId]/config
       user[charlie]/games/[gameId]/config
       ... (all participants)
```

**Flow:**
1. Creator writes config with initial participant list
2. Joiners read creator's config to get participant list
3. Joiners write their own config copy with themselves added
4. All participants periodically read all configs to discover new joiners

**Data:**
```typescript
{
  gameId: string,
  participants: string[],  // Array of public keys
  agenda: string[],
  currentAgendaIndex: number,
  timeWindow: number,
  createdAt: number,
  createdBy: string
}
```

### 2. Proposals (One per player per agenda item)

```
WRITE: user[myPub]/games/[gameId]/proposals/[agendaIdx]
READ:  user[alice]/games/[gameId]/proposals/[agendaIdx]
       user[bob]/games/[gameId]/proposals/[agendaIdx]
       user[charlie]/games/[gameId]/proposals/[agendaIdx]
       ... (all participants)
```

**Flow:**
1. Each player writes their proposal for the current agenda item
2. Each player reads all participants' proposals
3. Result: N proposals (where N = number of participants)

**Data:**
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Example for agenda item 0:**
- `user[alice]/games/game-123/proposals/0` → "Pizza"
- `user[bob]/games/game-123/proposals/0` → "Sushi"
- `user[charlie]/games/game-123/proposals/0` → "Tacos"

### 3. Challenges (One per challenger per proposal)

```
For Alice's proposal:
  WRITE: user[myPub]/games/[gameId]/challenges/[alice]
  READ:  user[alice]/games/[gameId]/challenges/[alice]
         user[bob]/games/[gameId]/challenges/[alice]
         user[charlie]/games/[gameId]/challenges/[alice]
         ... (all participants)

For Bob's proposal:
  WRITE: user[myPub]/games/[gameId]/challenges/[bob]
  READ:  user[alice]/games/[gameId]/challenges/[bob]
         user[bob]/games/[gameId]/challenges/[bob]
         user[charlie]/games/[gameId]/challenges/[bob]
         ... (all participants)
```

**Flow:**
1. For each proposal, each player decides whether to challenge
2. If challenging, player writes to `challenges/[proposalAuthorPub]`
3. To aggregate challenges for a proposal, read from all participants' `challenges/[proposalAuthorPub]`

**Data:**
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Example for Alice's proposal:**
- `user[bob]/games/game-123/challenges/alice` → "Too expensive"
- `user[charlie]/games/game-123/challenges/alice` → "Health concerns"
- (Alice doesn't challenge her own proposal)

### 4. Comments (One per commenter per proposal)

```
For Alice's proposal:
  WRITE: user[myPub]/games/[gameId]/comments/[alice]
  READ:  user[alice]/games/[gameId]/comments/[alice]
         user[bob]/games/[gameId]/comments/[alice]
         user[charlie]/games/[gameId]/comments/[alice]
         ... (all participants)
```

**Flow:**
1. For each challenged proposal, each player writes a comment
2. To aggregate comments, read from all participants' `comments/[proposalAuthorPub]`

**Data:**
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Example for Alice's proposal:**
- `user[alice]/games/game-123/comments/alice` → "I can make it healthier"
- `user[bob]/games/game-123/comments/alice` → "Let's discuss toppings"
- `user[charlie]/games/game-123/comments/alice` → "Agree with concerns"

### 5. Modifications (One per modifier per proposal)

```
For Alice's proposal:
  WRITE: user[myPub]/games/[gameId]/modifications/[alice]
  READ:  user[alice]/games/[gameId]/modifications/[alice]
         user[bob]/games/[gameId]/modifications/[alice]
         user[charlie]/games/[gameId]/modifications/[alice]
         ... (all participants)
```

**Flow:**
1. For each challenged proposal, players may propose modifications
2. To aggregate modifications, read from all participants' `modifications/[proposalAuthorPub]`
3. These become alternative candidates for voting

**Data:**
```typescript
{
  content: string,
  timestamp: number,
  authorPub: string
}
```

**Example for Alice's proposal:**
- `user[alice]/games/game-123/modifications/alice` → "Vegetarian pizza"
- `user[bob]/games/game-123/modifications/alice` → "Pizza with salad"
- (Charlie proposes no modification)

### 6. Support (One distribution per supporter per proposal)

```
For Alice's proposal:
  WRITE: user[myPub]/games/[gameId]/support/[alice]
  READ:  user[alice]/games/[gameId]/support/[alice]
         user[bob]/games/[gameId]/support/[alice]
         user[charlie]/games/[gameId]/support/[alice]
         ... (all participants)
```

**Flow:**
1. Each player allocates points across all candidates (original + modifications)
2. To aggregate support, read from all participants' `support/[proposalAuthorPub]`
3. Sum points for each candidate to determine winner

**Data:**
```typescript
{
  [candidateContent: string]: number  // points per candidate
}
```

**Example for Alice's proposal with 2 modifications:**
- `user[alice]/games/game-123/support/alice` → `{ "Pizza": 2, "Vegetarian pizza": 8, "Pizza with salad": 0 }`
- `user[bob]/games/game-123/support/alice` → `{ "Pizza": 0, "Vegetarian pizza": 5, "Pizza with salad": 5 }`
- `user[charlie]/games/game-123/support/alice` → `{ "Pizza": 1, "Vegetarian pizza": 7, "Pizza with salad": 2 }`

**Aggregation:**
- "Pizza": 2 + 0 + 1 = 3 points
- "Vegetarian pizza": 8 + 5 + 7 = **20 points (WINNER)**
- "Pizza with salad": 0 + 5 + 2 = 7 points

## Key Patterns

### Pattern 1: Broadcast (Config, Proposals)

Each player writes to their own space once. All players read from all spaces.

```
Alice writes: user[alice]/path
Bob writes:   user[bob]/path
Charlie writes: user[charlie]/path

Everyone reads: user[alice]/path, user[bob]/path, user[charlie]/path
```

Result: N items (one per participant)

### Pattern 2: Targeted Response (Challenges, Comments, Modifications, Support)

Each player writes to their own space, keyed by the target proposal author. All players read from all spaces for each target.

```
About Alice's proposal:
  Bob writes:     user[bob]/path/alice
  Charlie writes: user[charlie]/path/alice
  
  Everyone reads: user[bob]/path/alice, user[charlie]/path/alice
```

Result: Up to N-1 items per proposal (everyone except author)

## Code Examples

### Writing My Proposal
```typescript
// I write to my space
writeAtPath(
  user,
  ['games', gameId, 'proposals', agendaIndex.toString()],
  { content: 'My proposal', timestamp: Date.now(), authorPub: myPubKey },
  callback
);
```

### Reading All Proposals
```typescript
// I read from everyone's space
for (const participantPub of participants) {
  readAtPath(
    user,
    [participantPub, 'games', gameId, 'proposals', agendaIndex.toString()],
    (data) => {
      if (data) proposals.push(data);
    }
  );
}
```

### Writing My Challenge to Alice's Proposal
```typescript
// I write to my space, keyed by Alice
writeAtPath(
  user,
  ['games', gameId, 'challenges', alicePubKey],
  { content: 'My challenge', timestamp: Date.now(), authorPub: myPubKey },
  callback
);
```

### Reading All Challenges to Alice's Proposal
```typescript
// I read from everyone's space, looking for challenges keyed by Alice
for (const participantPub of participants) {
  readAtPath(
    user,
    [participantPub, 'games', gameId, 'challenges', alicePubKey],
    (data) => {
      if (data) challenges.push(data);
    }
  );
}
```

## Advantages of This Pattern

1. **No Write Conflicts**: Each player only writes to their own space
2. **Public Readable**: Anyone can read from anyone (within the game)
3. **Simple Aggregation**: Just read from all participants and collect
4. **Fault Tolerant**: Missing data from one participant doesn't block others
5. **Eventually Consistent**: New data appears as it's written
6. **Auditable**: Complete history of who wrote what when
7. **Scalable**: O(N) reads per aggregation, O(1) writes per action

## Limitations & Trade-offs

1. **Read Overhead**: Must read from all participants to get full state (O(N))
2. **No Atomic Transactions**: Can't guarantee all players have written before reading
3. **Time-based Sync**: Relies on time windows or polling for coordination
4. **Duplicate Participant Discovery**: Each player must independently discover participants
5. **No Built-in Conflict Resolution**: Application must handle inconsistencies

## Future Optimizations

1. **Gossip Protocol**: Participants share what they've seen from others
2. **Bloom Filters**: Quick check if participant has data before reading
3. **Last-Write-Wins**: Use timestamps for conflict resolution
4. **Merkle Trees**: Verify completeness of data across participants
5. **Subscription Registry**: Central list of active listeners

