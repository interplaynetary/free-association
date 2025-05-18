import Gun from 'gun';
import 'gun/sea'; // For user authentication
import { fulfilled, mutualFulfillment, providerShares } from './protocol';
import type { Capacity } from './protocol';

// load the wholeTree into a svelte writeable Store

// we will only do calculations on the in memory tree.

// Modify the tree-in memory and then persist it to Gun

// what we want to persist to gun:

// 1. the whole tree
// 2. the user's SOGF
// 3. the user's providerShares

// lets talk advantage of gun's ability to automatically merge deeply nested objects

// however let's also make sure that for delete operations specifically, we can
// To integrate the Free Association Protocol with GunDB while maintaining efficient in-memory operations and proper synchronization, we'll implement the following modifications:

//**1. GunDB Initialization & Data Structure Optimization**
// Initialize Gun with SEA for security
import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import 'gun/lib/webrtc';
import 'gun/lib/then';
import 'gun/lib/yson.js';

// Initialize Gun once
export const gun = Gun({
peers: [
'http://localhost:8765/gun'
//"https://gun-manhattan.herokuapp.com/gun", // Public relay peer for cross-device syncing
],
localStorage: false
});

export const user = gun.user()
user.recall({ sessionStorage: true });

// Modified type definitions for Gun compatibility
export interface BaseNode {
id: string;
name: string;
type: string;
manual_fulfillment?: number;
children: string[]; // Now stores child IDs instead of full objects
}

export interface RootNode extends BaseNode {
type: 'RootNode';
user_id: string;
capacities: string[]; // Store capacity IDs
created_at: string;
updated_at: string;
}

// Create separate Gun node types
type GunNode = RootNode | NonRootNode;
type GunCapacity = Capacity & { shares: string[] }; // Store share IDs

//**2. Tree Loading Mechanism**

// Global in-memory cache
const nodesCache = new Map<string, GunNode>();
const capacitiesCache = new Map<string, GunCapacity>();

async function loadEntireTree(rootId: string): Promise<RootNode> {
const loadNode = async (id: string): Promise<GunNode> => {
if (nodesCache.has(id)) return nodesCache.get(id)!;

    const node = await gun.get('nodes').get(id).once();
    if (!node) throw new Error(`Node ${id} not found`);

    // Load children recursively
    const children = await Promise.all(
      (node.children || []).map(loadNode)
    );

    nodesCache.set(id, { ...node, children });
    return nodesCache.get(id)!;

};

return loadNode(rootId) as Promise<RootNode>;
}

// **3. Tree Persistence Mechanism**

async function persistTree(root: RootNode) {
const seenNodes = new Set<string>();
const seenCapacities = new Set<string>();

async function persistNode(node: GunNode) {
if (seenNodes.has(node.id)) return;
seenNodes.add(node.id);

    // Clone and prepare for Gun
    const gunNode = {
      ...node,
      children: node.children.map(c => typeof c === 'string' ? c : c.id)
    };

    gun.get('nodes').get(node.id).put(gunNode);

    // Process children
    await Promise.all(node.children.map(c =>
      typeof c === 'string' ? null : persistNode(c)
    ));

}

await persistNode(root);

// Process capacities
for (const capId of root.capacities) {
if (seenCapacities.has(capId)) continue;
const cap = capacitiesCache.get(capId);
if (!cap) continue;

    gun.get('capacities').get(capId).put({
      ...cap,
      shares: cap.shares.map(s => s.id)
    });

}
}

// **4. Advanced Deletion Handling**

async function deleteSubtree(nodeId: string) {
const markForDeletion = async (id: string) => {
const node = nodesCache.get(id);
if (!node) return;

    // Mark deleted in Gun
    gun.get('nodes').get(id).put(null);
    nodesCache.delete(id);

    // Recursive deletion
    await Promise.all(node.children.map(markForDeletion));

    // Handle capacity cleanup
    if (node.type === 'RootNode') {
      node.capacities.forEach(capId => {
        gun.get('capacities').get(capId).put(null);
        capacitiesCache.delete(capId);
      });
    }

};

await markForDeletion(nodeId);
}

// **5. SOGF & Provider Shares Integration**

async function updateUserState(userId: string) {
const root = await loadEntireTree(`root_${userId}`);
const nodesMap = Object.fromEntries(nodesCache.entries());

// Calculate and store SOGF
const sogf = sharesOfGeneralFulfillmentMap(root, nodesMap);
gun.get(`user/${userId}/sogf`).put(sogf);

// Calculate and store provider shares
const shares = providerShares(root, 3, nodesMap);
gun.get(`user/${userId}/providerShares`).put(shares);

// Store current tree version
gun.get(`user/${userId}/treeVersion`).put(Date.now());
}

// **6. Real-time Synchronization**

function setupRealTimeUpdates(userId: string) {
gun.get(`user/${userId}/sogf`).on(data => {
console.log('SOGF Updated:', data);
// Update UI or trigger downstream actions
});

gun.get('nodes').map().on((node, id) => {
if (node && !nodesCache.has(id)) {
nodesCache.set(id, node);
console.log('New node detected:', node);
}
});
}
/\*
**Key Implementation Notes:**

1. **Data Structure Optimization:**

   - Use ID references instead of nested objects
   - Maintain separate caches for nodes and capacities
   - Store relationships through ID lists

2. **Efficient Synchronization:**

   - Batch updates using Set tracking
   - Separate node and capacity persistence
   - Version tracking for tree updates

3. **Deletion Handling:**

   - Recursive null propagation in Gun
   - Cache cleanup
   - Capacity cascade deletion

4. **Real-time Features:**
   - Live updates for SOGF and provider shares
   - Node-level change detection
   - Version tracking for optimistic concurrency

This implementation maintains a clear separation between in-memory operations and Gun persistence while leveraging Gun's strengths in real-time synchronization and conflict resolution. The delete handling ensures proper cleanup while maintaining data integrity across the distributed system.
\*/
