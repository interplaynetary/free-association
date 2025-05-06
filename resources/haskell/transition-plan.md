# Plan Part 1

I'll analyze all the global state and caching to help determine what should be scoped to a player (network node) vs. individual trees. Let me break this down:

I'll refine the transition plan based on the principle of only sharing computed results rather than entire trees. Let me analyze and propose a revised structure:

1. First, let's clarify what needs to be shared vs. kept private:

**Private (Local Only)**:
- Tree structures and their modifications
- Local calculation caches (weight, fulfillment, descendants)
- Node details (points, contributors, children)

**Shared (Network Exposed)**:
- Mutual fulfillment results
- Provider share calculations
- Capacity information and shares

Here's the refined structure:

1. **Player Level (Network Node)**:
```typescript
interface Player {
  playerId: string;
  trees: Map<string, TreeZipper>;  // Local trees only
  computedShares: ShareCache;      // Cache of computed shares to expose to network
  mutualFulfillments: Cache<string, number>; // Cache of mutual fulfillment calculations
}

interface ShareCache {
  // Key format: `${providerId}:${depth}`
  providerShares: Map<string, ShareMap>;
  // Key format: `${providerId}:${capacityId}`
  capacityShares: Map<string, CapacityShareMap>;
}
```

2. **Tree Level (Private)**:
```typescript
interface Tree {
  nodeData: Node;
  localCache: Cache<string, CacheValue>;  // Local calculations only
}

interface LocalCache {
  weights: Map<string, number>;
  fulfillments: Map<string, number>;
  descendants: Map<string, string[]>;
  totalPoints: Map<string, number>;
}
```

3. **Network Interface (Public API)**:
```typescript
interface NetworkAPI {
  // Only expose computed results
  getProviderShares(providerId: string, depth: number): ShareMap;
  getMutualFulfillment(nodeId: string): number;
  getCapacityShares(capacityId: string): CapacityShareMap;
  
  // Subscribe to updates
  onSharesUpdated(providerId: string): Observable<ShareMap>;
  onFulfillmentUpdated(nodeId: string): Observable<number>;
}
```

Key Changes from Previous Plan:
1. Removed remote tree caching - we don't need to store other's trees
2. Focused on caching computed results only
3. Clear separation between private tree data and public computed results
4. Network API only exposes final computed values

Benefits:
1. Better privacy - trees remain private by default
2. Reduced network traffic - only share computed results
3. Simpler caching - focus on caching what others need
4. Clear boundaries between private and public data

Implementation Notes:
1. Calculations still happen locally using full tree access
2. Results are cached and exposed via the network API
3. Other nodes only see the final computed values
4. Trees can be modified locally without exposing internal structure

Would you like me to:
1. Show how specific calculations would work in this model?
2. Detail the TypeScript adaptations of the Haskell tree navigation?
3. Elaborate on any part of this structure?

### Part 2

- from haskell to typescript, remaining as close as possible to haskell implementation but using typescript concepts
- we will want to use our adapt our TreeMap to use our TreeZippers for navigation, and modification.
