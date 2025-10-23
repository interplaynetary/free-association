# Peerbit Implementation for Free Association

This document explains our peer-to-peer implementation of the Free Association model using Peerbit.

## Overview

We've simplified our approach to focus on how Peerbit can enable a decentralized version of the mutual contribution model from `centralized.hs`. Each peer in the network maintains their own tree structure, and mutual fulfillment is calculated across peers' trees.

## Setup and Running

To run and test this implementation:

1. **Install dependencies**:

   ```bash
   cd protocol/peerbit
   npm install
   ```

2. **Run the test directly with ts-node**:

   ```bash
   npm test
   ```

3. **Run in development mode with auto-reload**:

   ```bash
   npm run dev
   ```

4. **Build for production**:
   ```bash
   npm run build
   npm start
   ```

## Key Design Decisions

1. **One Forest Per Peer**: Each peer maintains their own node hierarchy with a single root node.

2. **Simplified Data Model**: Instead of separate `RootNode` and `NonRootNode` types, we use a unified `Node` class with an `isRoot` field to determine node type.

3. **Peer-to-Peer Mutual Fulfillment**: Mutual fulfillment is calculated between nodes in different peers' trees, respecting contribution relationships.

4. **Capacity Sharing Across Peers**: Peers can access portions of others' capacities based on their mutual fulfillment scores.

## Implementation Details

### Data Structures

Our implementation includes these key classes:

- **Node**: Represents any node in a tree, with properties for ID, name, points, children, contributors, etc.
- **Capacity**: Represents a capacity that can be shared (e.g., computing resources, storage, etc.)
- **CapacityShare**: Represents one peer's share in another peer's capacity
- **MutualFulfillment**: Records the fulfillment relationship between nodes across peers

### Key Components

#### 1. ForestProgram

This is the core program that each peer runs to maintain their node tree:

```typescript
class ForestProgram extends Program {
  // Document collections for different entity types
  nodes: Documents<Node>;
  capacities: Documents<Capacity>;
  capacityShares: Documents<CapacityShare>;
  mutualFulfillments: Documents<MutualFulfillment>;

  // The peer's root node ID
  rootNodeId: string;

  // Methods for tree manipulation
  initializeRoot(name: string, points: number): Promise<Node>;
  addChild(parentId: string, name: string, points: number, contributors: string[]): Promise<Node>;
  addCapacity(name: string, quantity: number, ...): Promise<Capacity>;
  // ...and more
}
```

#### 2. FreeAssociationNetwork

This class handles peer interactions and fulfillment calculations across different forests:

```typescript
class FreeAssociationNetwork {
	private peerbit: Peerbit;
	private localForest: ForestProgram;
	private remotePeers: Map<string, ForestProgram>;

	// Join another peer's forest
	joinForest(address: string): Promise<void>;

	// Calculate mutual fulfillment between nodes across peers
	calculateMutualFulfillment(
		localNodeId: string,
		remoteAddress: string,
		remoteNodeId: string
	): Promise<number>;

	// Create capacity shares based on mutual fulfillment
	createCapacityShare(
		remoteAddress: string,
		remoteNodeId: string,
		capacityId: string
	): Promise<CapacityShare | null>;
}
```

## Test Output Example

When running the test, you should see output similar to:

```
Starting Peerbit Free Association test...
Creating Peerbit instances...
Alice Peer ID: 12D3KooWA1b9VqMD9zrsw9UNo5CwkNKC37DNW5JaVPKCRmhR3rTM
Bob Peer ID: 12D3KooWDfSVTpQWH4aWnufrYgmGLXMzHBGdP1fQy9YgNvEZVBpE

Initializing forests...
Alice Forest Address: zb2rhfMbrkV71WTxWyWPf32dCJJkpx5Y1D6...
Bob Forest Address: zb2rhmmXF1HeTKtUiUaCNR4tWyEaKab8EjT...

Creating root nodes...
Alice Root ID: 123e4567-e89b-12d3-a456-426614174000
Bob Root ID: 123e4567-e89b-12d3-a456-426614174001

Adding capacity to Alice...
Capacity ID: 123e4567-e89b-12d3-a456-426614174002
Capacity Details: { name: 'Spare Room', quantity: 10, unit: 'room' }

Adding children with mutual contributions...
Alice Child: { id: '123e4567-e89b-12d3-a456-426614174003', name: 'alice_child', contributors: [ '123e4567-e89b-12d3-a456-426614174001' ] }
Bob Child: { id: '123e4567-e89b-12d3-a456-426614174004', name: 'bob_child', contributors: [ '123e4567-e89b-12d3-a456-426614174000' ] }

Creating network interfaces...

Connecting peers...
Connection established between peers

Calculating mutual fulfillment...
Mutual fulfillment between Alice and Bob: 75%

Creating capacity shares...
Bob's share of Alice's room: 7.5%
Computed quantity: 7 room

Verifying capacity shares...
Bob's capacity share IDs: 123e4567-e89b-12d3-a456-426614174005
Retrieved capacity share: {
  id: '123e4567-e89b-12d3-a456-426614174005',
  targetCapacityId: '123e4567-e89b-12d3-a456-426614174002',
  sharePercentage: 0.075,
  computedQuantity: 7
}

Test completed successfully!
```

## Troubleshooting

If you encounter TypeScript errors related to decorators, ensure:

1. Your `tsconfig.json` has `experimentalDecorators` and `emitDecoratorMetadata` set to `true`
2. The correct versions of Peerbit packages are installed
3. You're using TypeScript version 5.3+ which has better support for decorators

## Mutual Fulfillment Calculation

Our simplified mutual fulfillment algorithm considers:

1. **Direct contributions**: If nodes directly contribute to each other (score: 0.75)
2. **One-way contributions**: If only one node contributes to the other (score: 0.5)
3. **Transitive contributions**: If there are contribution relationships between descendants (score: 0.25)
4. **No relationship**: If no contribution relationship exists (score: 0)

## Capacity Sharing

Capacity shares are allocated based on mutual fulfillment scores between peers, with respect to:

1. **Percentage divisibility**: The maximum percentage of capacity that can be shared
2. **Natural divisibility**: The minimum unit of capacity that can be allocated (e.g., 1 room, 1 CPU core)

## Differences from the Haskell Model

The key differences from `centralized.hs` are:

1. **Distributed state**: Each peer maintains their own state instead of a central model
2. **Simplified structure**: We've unified the node types and simplified some calculations
3. **Peer-oriented**: Mutual fulfillment and capacity sharing work across peer boundaries
4. **Progressive calculations**: Instead of pre-computing all values, we calculate them as needed

## Future Enhancements

1. Implement more sophisticated mutual fulfillment calculations based on the Haskell model
2. Add caching of calculated values to improve performance
3. Implement automatic synchronization of contribution relationships
4. Add blockchain-like consensus for validating contribution claims
