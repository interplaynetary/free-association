/*
Analyze this code in depth, you are a mathematician seeking to understand how we can use mutual-recognition proportional mathematics to divide the infinite of natural numbers of our own capacities

Okay, so far what this system has achieved is setting up a system where the root of a tree represents an individual who can declare their capacities as well as declare a natural number for the amount of that capacity they have. It recognizes the contributions of others towards the satisfaction of their needs to derive mutual recognition of mutual contribution which is mutual fulfillment. This can be used to divide the capacities across contributors, so we multiply the natural number of capacity by the percentage from the mutual fulfillment. 

Now we want to explore the possibility of an equation. Keep it simple. Look at the simplicity of our existing mutual recognition calculations. The minimum function, normalization, percentages.

We want to understand if there is a way that we can recursively extend proportions into synthetic collectives of individuals if we can somehow extend from mutual fulfillment calculations of individuals derive synthetic collective.
*/

/*
A note on privacy:
- it is important that trees are private, with certain subtrees being public
- only subtrees in my personal tree marked as public will be mergeable
*/

import type { Node, NonRootNode, CapacitiesCollection } from '$lib/schema';
import { mutualFulfillment as originalMutualFulfillment } from '$lib/protocol';
import { writable, derived, get } from 'svelte/store';
import type { Writable } from 'svelte/store';

/**
 * Enhanced with Memoization & Caching
 */
type EntityID = string;

// Define collective-specific types
interface Collective {
	type: 'Collective';
	id: string;
	members: Array<Entity>;
	weights: Map<string, number>; // Maps member IDs to their weight in the collective
}

type Entity = Node | Collective;
type Forest = Map<string, Node>; // Maps node IDs to nodes

// we will subscribe to collectiveMembers trees to populate collectiveForest
export const collectiveMembers: Writable<Array<Entity>> = writable([]);
export const collectiveForest: Writable<Forest> = writable(new Map());

// ideally using the same format as userTree (so not strings etc.)

// Helper function to check if entity is a collective
function isCollective(entity: Entity): entity is Collective {
	return (entity as Collective).type === 'Collective';
}

// Global cache stores
const mfCache = new Map<EntityID, Map<EntityID, number>>();
const weightCache = new Map<EntityID, Map<EntityID, number>>();

// Cache key generator
function getCacheKey(a: EntityID, b: EntityID): string {
	return `${a}<=>${b}`;
}

// Get node capacities from the node's capacities collection
function getNodeCapacities(node: Node): CapacitiesCollection {
	// Return the capacities directly from the node if available
	if ('capacities' in node && node.capacities) {
		return node.capacities as CapacitiesCollection;
	}
	return {};
}

// Collective mutual fulfillment calculation
function collectiveMF(ci: Forest, a: Entity, b: Entity): number {
	// If both are individuals, use original calculation
	if (!isCollective(a) && !isCollective(b)) {
		const nodesMap = Object.fromEntries(ci);
		return originalMutualFulfillment(a as Node, b as Node, nodesMap);
	}

	// Handle collective-to-individual or collective-to-collective cases
	if (isCollective(a) && !isCollective(b)) {
		// Collective A to Individual B
		const [weights] = getCollectiveWeights(a, ci);
		let totalMF = 0;

		for (const member of a.members) {
			const memberWeight = weights.get(member.id) || 0;
			const memberMF = mutualFulfillment(ci, member, b);
			totalMF += memberWeight * memberMF;
		}

		return totalMF;
	}

	if (!isCollective(a) && isCollective(b)) {
		// Individual A to Collective B
		const [weights] = getCollectiveWeights(b, ci);
		let totalMF = 0;

		for (const member of b.members) {
			const memberWeight = weights.get(member.id) || 0;
			const memberMF = mutualFulfillment(ci, a, member);
			totalMF += memberWeight * memberMF;
		}

		return totalMF;
	}

	// Both are collectives
	if (isCollective(a) && isCollective(b)) {
		const [weightsA] = getCollectiveWeights(a, ci);
		const [weightsB] = getCollectiveWeights(b, ci);
		let totalMF = 0;

		for (const memberA of a.members) {
			for (const memberB of b.members) {
				const weightA = weightsA.get(memberA.id) || 0;
				const weightB = weightsB.get(memberB.id) || 0;
				const memberMF = mutualFulfillment(ci, memberA, memberB);
				totalMF += weightA * weightB * memberMF;
			}
		}

		return totalMF;
	}

	return 0;
}

// Enhanced mutual fulfillment with caching
function mutualFulfillment(ci: Forest, a: Entity, b: Entity): number {
	const key = getCacheKey(a.id, b.id);

	// Check cache
	const cached = mfCache.get(a.id)?.get(b.id);
	if (cached !== undefined) return cached;

	// Calculate fresh value
	let result = 0;

	if (!isCollective(a) && !isCollective(b)) {
		const nodesMap = Object.fromEntries(ci);
		result = originalMutualFulfillment(a as Node, b as Node, nodesMap);
	} else {
		result = collectiveMF(ci, a, b);
	}

	// Update cache
	if (!mfCache.has(a.id)) mfCache.set(a.id, new Map());
	mfCache.get(a.id)!.set(b.id, result);

	return result;
}

// Optimized weight calculation
function getCollectiveWeights(entity: Entity, ci: Forest): [Map<string, number>, number] {
	if (!isCollective(entity)) {
		return [new Map([[entity.id, 1.0]]), 1.0];
	}

	// Check weight cache
	if (weightCache.has(entity.id)) {
		const weights = weightCache.get(entity.id)!;
		const total = Array.from(weights.values()).reduce((sum, w) => sum + w, 0);
		return [weights, total];
	}

	// Calculate fresh weights
	const weights = new Map<string, number>();
	let total = 0;

	for (const member of entity.members) {
		const subCollective = createSubCollective(entity, member);
		const mf = mutualFulfillment(ci, member, subCollective);
		weights.set(member.id, mf);
		total += mf;
	}

	// Normalize and cache
	const normalized = new Map(Array.from(weights.entries()).map(([id, w]) => [id, w / total]));

	weightCache.set(entity.id, normalized);
	return [normalized, total];
}

// Optimized sub-collective creation
const subCollectiveCache = new Map<string, Collective>();

function createSubCollective(collective: Collective, exclude: Entity): Collective {
	const cacheKey = `${collective.id}-${exclude.id}`;

	if (subCollectiveCache.has(cacheKey)) {
		return subCollectiveCache.get(cacheKey)!;
	}

	const sub: Collective = {
		...collective,
		id: cacheKey,
		members: collective.members.filter((m: Entity) => m.id !== exclude.id),
		weights: new Map()
	};

	subCollectiveCache.set(cacheKey, sub);
	return sub;
}

// Enhanced collective capacity calculation
const capacityCache = new Map<EntityID, number>();

function collectiveCapacity(ci: Forest, collective: Collective): number {
	if (capacityCache.has(collective.id)) {
		return capacityCache.get(collective.id)!;
	}

	let total = 0;
	const memberMFs = new Map<string, number>();
	let mfSum = 0;

	// Pre-calculate all MF values
	for (const member of collective.members) {
		const mf = mutualFulfillment(ci, member, collective);
		memberMFs.set(member.id, mf);
		mfSum += mf;
	}

	for (const member of collective.members) {
		const mf = memberMFs.get(member.id)!;
		const phi = mf / mfSum;

		if (isCollective(member)) {
			total += collectiveCapacity(ci, member) * phi;
		} else {
			const capacities = getNodeCapacities(member as Node);
			const memberCap = Object.values(capacities).reduce((acc, cap) => {
				// Calculate total quantity from availability_slots
				if (cap.availability_slots && Array.isArray(cap.availability_slots)) {
					const slotTotal = cap.availability_slots.reduce(
						(slotSum, slot) => slotSum + (slot.quantity || 0),
						0
					);
					return acc + slotTotal;
				}
				return acc;
			}, 0);
			total += memberCap * phi;
		}
	}

	capacityCache.set(collective.id, total);
	return total;
}

// Export the main functions
export {
	type Entity,
	type Collective,
	type Forest,
	isCollective,
	mutualFulfillment,
	getCollectiveWeights,
	createSubCollective,
	collectiveCapacity,
	collectiveMF
};

// === NEW TREE MERGING FUNCTIONALITY ===

import type {
	CollectiveTree,
	CollectiveNode,
	CollectiveRootNode,
	CollectiveNonRootNode,
	TreeMergeConfig,
	TreeMergeResult
} from '$lib/schema';
import { shareOfParent } from '$lib/protocol';
import crypto from 'crypto';

// Hash function for creating collective IDs
function hashContributorIds(contributorIds: string[]): string {
	const sorted = [...contributorIds].sort();
	return crypto.createHash('sha256').update(sorted.join('|')).digest('hex').substring(0, 16);
}

// Calculate percentage weights for each contributor based on mutual recognition
function calculateContributorWeights(
	contributorIds: string[],
	contributorTrees: Map<string, Node>,
	recognitionShares?: Record<string, number>
): Record<string, number> {
	if (recognitionShares) {
		// Use provided recognition shares
		const totalWeight = Object.values(recognitionShares).reduce((sum, weight) => sum + weight, 0);
		const normalized: Record<string, number> = {};

		for (const contributorId of contributorIds) {
			const weight = recognitionShares[contributorId] || 0;
			normalized[contributorId] =
				totalWeight > 0 ? weight / totalWeight : 1 / contributorIds.length;
		}

		return normalized;
	}

	// Calculate mutual recognition between all contributors
	const weights: Record<string, number> = {};
	const forest: Forest = contributorTrees;

	for (const contributorId of contributorIds) {
		const contributorTree = contributorTrees.get(contributorId);
		if (!contributorTree) continue;

		let totalRecognition = 0;

		// Sum recognition this contributor gets from all others
		for (const otherId of contributorIds) {
			if (otherId === contributorId) continue;

			const otherTree = contributorTrees.get(otherId);
			if (!otherTree) continue;

			const recognition = mutualFulfillment(forest, contributorTree, otherTree);
			totalRecognition += recognition;
		}

		weights[contributorId] = totalRecognition;
	}

	// Normalize weights
	const totalWeight = Object.values(weights).reduce((sum, w) => sum + w, 0);
	if (totalWeight === 0) {
		// Equal weights if no mutual recognition
		for (const contributorId of contributorIds) {
			weights[contributorId] = 1 / contributorIds.length;
		}
	} else {
		for (const contributorId of contributorIds) {
			weights[contributorId] = weights[contributorId] / totalWeight;
		}
	}

	return weights;
}

// Node merging data structure
interface NodeMergeData {
	id: string;
	name: string;
	contributors: Map<
		string,
		{
			originalNode: Node;
			weightInParent: number;
			contributorWeight: number;
		}
	>;
	children: Map<string, NodeMergeData>;
	path: string[];
}

// Create a collective tree by merging multiple contributor trees
function mergeContributorTrees(config: TreeMergeConfig): TreeMergeResult {
	const startTime = Date.now();
	const contributorIds = Object.keys(config.contributor_trees);
	const contributorTrees = new Map(Object.entries(config.contributor_trees) as [string, Node][]);

	// Calculate contributor weights based on mutual recognition
	const contributorWeights = calculateContributorWeights(
		contributorIds,
		contributorTrees,
		config.recognition_shares
	);

	// Create root merge data
	const rootMergeData: NodeMergeData = {
		id: hashContributorIds(contributorIds),
		name: `Collective of ${contributorIds.length} contributors`,
		contributors: new Map(),
		children: new Map(),
		path: []
	};

	// Add root nodes from each contributor
	for (const contributorId of contributorIds) {
		const tree = contributorTrees.get(contributorId);
		if (!tree) continue;

		rootMergeData.contributors.set(contributorId, {
			originalNode: tree as Node,
			weightInParent: 1.0, // Root nodes always have full weight
			contributorWeight: contributorWeights[contributorId]
		});
	}

	// Recursively merge trees
	const mergeStats = {
		total_contributors: contributorIds.length,
		nodes_merged: 0,
		conflicts_resolved: 0,
		execution_time_ms: 0
	};

	const warnings: string[] = [];
	const errors: string[] = [];

	try {
		mergeNodeRecursively(
			rootMergeData,
			contributorTrees,
			contributorWeights,
			config,
			mergeStats,
			warnings
		);

		// Convert merge data to collective tree
		const collectiveRoot = convertMergeDataToCollectiveNode(
			rootMergeData,
			null
		) as CollectiveRootNode;

		// Create recognition matrix
		const recognitionMatrix: Record<string, Record<string, number>> = {};
		const forest: Forest = contributorTrees;

		for (const contributorA of contributorIds) {
			recognitionMatrix[contributorA] = {};
			const treeA = contributorTrees.get(contributorA);

			for (const contributorB of contributorIds) {
				if (contributorA === contributorB) {
					recognitionMatrix[contributorA][contributorB] = 1.0;
					continue;
				}

				const treeB = contributorTrees.get(contributorB);
				if (treeA && treeB) {
					recognitionMatrix[contributorA][contributorB] = mutualFulfillment(forest, treeA, treeB);
				} else {
					recognitionMatrix[contributorA][contributorB] = 0;
				}
			}
		}

		const collectiveTree: CollectiveTree = {
			id: rootMergeData.id,
			root: collectiveRoot,
			contributors: contributorIds,
			recognition_matrix: recognitionMatrix,
			creation_timestamp: new Date().toISOString(),
			last_updated: new Date().toISOString(),
			merge_algorithm_version: '1.0.0',
			total_nodes_merged: mergeStats.nodes_merged,
			merge_conflicts: [] // TODO: Implement conflict tracking
		};

		mergeStats.execution_time_ms = Date.now() - startTime;

		return {
			collective_tree: collectiveTree,
			merge_stats: mergeStats,
			warnings,
			errors
		};
	} catch (error) {
		errors.push(`Tree merge failed: ${error instanceof Error ? error.message : String(error)}`);
		throw error;
	}
}

// Recursively merge nodes from different contributor trees
function mergeNodeRecursively(
	mergeData: NodeMergeData,
	contributorTrees: Map<string, Node>,
	contributorWeights: Record<string, number>,
	config: TreeMergeConfig,
	mergeStats: { nodes_merged: number; conflicts_resolved: number },
	warnings: string[]
): void {
	// Process each contributor's version of this node
	for (const [contributorId, nodeData] of mergeData.contributors) {
		const node = nodeData.originalNode;

		// Process children of this node
		for (const child of node.children) {
			const childId = child.id;
			const childName = child.name;

			// Calculate child's weight within its parent's siblings
			const childWeightInParent = shareOfParent(child, node);

			// Get or create merge data for this child
			if (!mergeData.children.has(childId)) {
				mergeData.children.set(childId, {
					id: childId,
					name: childName,
					contributors: new Map(),
					children: new Map(),
					path: [...mergeData.path, childId]
				});
			}

			const childMergeData = mergeData.children.get(childId)!;

			// Add this contributor's version of the child
			childMergeData.contributors.set(contributorId, {
				originalNode: child,
				weightInParent: childWeightInParent,
				contributorWeight: contributorWeights[contributorId]
			});

			// Handle name conflicts
			if (childMergeData.name !== childName) {
				switch (config.name_collision_strategy) {
					case 'append_contributor':
						childMergeData.name = `${childName} (${contributorId})`;
						break;
					case 'weighted_priority':
						// Keep the name from the contributor with highest weight
						const currentContributorWeight = contributorWeights[contributorId];
						const existingMaxWeight = Math.max(
							...Array.from(childMergeData.contributors.values()).map((c) => c.contributorWeight)
						);
						if (currentContributorWeight > existingMaxWeight) {
							childMergeData.name = childName;
						}
						break;
					case 'manual_resolve':
						warnings.push(
							`Name conflict for node ${childId}: "${childMergeData.name}" vs "${childName}"`
						);
						break;
				}
			}
		}
	}

	// Recursively merge children
	for (const childMergeData of mergeData.children.values()) {
		mergeNodeRecursively(
			childMergeData,
			contributorTrees,
			contributorWeights,
			config,
			mergeStats,
			warnings
		);
		mergeStats.nodes_merged++;
	}
}

// Convert merge data to collective node
function convertMergeDataToCollectiveNode(
	mergeData: NodeMergeData,
	parentId: string | null
): CollectiveNode {
	// Calculate weighted percentage for this node
	let totalWeightedPercentage = 0;
	const sourceContributors: Record<string, number> = {};
	const mergedFromNodes: string[] = [];
	const allContributorIds: string[] = [];

	for (const [contributorId, nodeData] of mergeData.contributors) {
		const weightedPercentage = nodeData.weightInParent * nodeData.contributorWeight;
		totalWeightedPercentage += weightedPercentage;
		sourceContributors[contributorId] = weightedPercentage;
		mergedFromNodes.push(nodeData.originalNode.id);
		allContributorIds.push(contributorId);
	}

	// Normalize source contributors
	if (totalWeightedPercentage > 0) {
		for (const contributorId of Object.keys(sourceContributors)) {
			sourceContributors[contributorId] =
				sourceContributors[contributorId] / totalWeightedPercentage;
		}
	}

	// Convert children
	const children: CollectiveNode[] = [];
	for (const childMergeData of mergeData.children.values()) {
		const childNode = convertMergeDataToCollectiveNode(childMergeData, mergeData.id);
		children.push(childNode);
	}

	// Create collective node
	if (parentId === null) {
		// Root node
		return {
			id: mergeData.id,
			name: mergeData.name,
			type: 'CollectiveRootNode',
			manual_fulfillment: null,
			children,
			created_at: new Date().toISOString(),
			updated_at: new Date().toISOString(),
			contributors: allContributorIds,
			contributor_weights: sourceContributors,
			source_trees: {} // TODO: Store original trees if needed
		} as CollectiveRootNode;
	} else {
		// Non-root node
		return {
			id: mergeData.id,
			name: mergeData.name,
			type: 'CollectiveNonRootNode',
			manual_fulfillment: null,
			children,
			weight_percentage: totalWeightedPercentage,
			parent_id: parentId,
			contributor_ids: allContributorIds,
			source_contributors: sourceContributors,
			merged_from_nodes: mergedFromNodes
		} as CollectiveNonRootNode;
	}
}

// Utility function to create a simple merge configuration
function createSimpleMergeConfig(
	contributorTrees: Record<string, Node>,
	recognitionShares?: Record<string, number>
): TreeMergeConfig {
	return {
		contributor_trees: contributorTrees,
		recognition_shares: recognitionShares || {},
		merge_strategy: 'weighted_average',
		conflict_resolution: 'merge',
		name_collision_strategy: 'weighted_priority'
	};
}

// Export new tree merging functions
export {
	mergeContributorTrees,
	createSimpleMergeConfig,
	hashContributorIds,
	calculateContributorWeights,
	type NodeMergeData
};

// === MATHEMATICAL FRAMEWORK FOR COLLECTIVE RECOGNITION ===

/**
 * Core Mathematical Principles:
 *
 * 1. Individual Tree → Percentage Tree Conversion
 *    - Each node's weight = points / sum(sibling_points)
 *    - This converts absolute points to relative percentages
 *
 * 2. Collective Recognition Weighting
 *    - Each contributor has a recognition weight: w_i = Σ(recognition_from_others) / total_recognition
 *    - Node weights in collective = Σ(individual_node_percentage_i × contributor_weight_i)
 *
 * 3. Recursive Proportional Merging
 *    - At each level, percentages are combined proportionally
 *    - Path-dependent weighting: parent_weight × child_percentage × contributor_weight
 */

// Convert an individual tree to percentage-based representation
function convertTreeToPercentages(tree: Node): Map<string, number> {
	const percentageMap = new Map<string, number>();

	function calculatePercentagesRecursive(node: Node, pathWeight: number = 1.0): void {
		// Store this node's cumulative weight from root
		percentageMap.set(node.id, pathWeight);

		// Calculate total points of all siblings at this level
		const totalSiblingPoints = node.children.reduce((sum, child) => {
			return sum + (child.type === 'NonRootNode' ? (child as NonRootNode).points : 0);
		}, 0);

		// Process each child with its percentage of siblings
		for (const child of node.children) {
			if (child.type === 'NonRootNode') {
				const childPoints = (child as NonRootNode).points;
				const childPercentage = totalSiblingPoints > 0 ? childPoints / totalSiblingPoints : 0;
				const childPathWeight = pathWeight * childPercentage;

				calculatePercentagesRecursive(child, childPathWeight);
			} else {
				// Root nodes maintain full weight
				calculatePercentagesRecursive(child, pathWeight);
			}
		}
	}

	calculatePercentagesRecursive(tree);
	return percentageMap;
}

// Calculate each contributor's share of a specific node in the collective tree
function calculateNodeContributorShares(
	collectiveTree: CollectiveTree,
	targetNodeId: string
): Record<string, number> {
	const contributorShares: Record<string, number> = {};

	// Find the target node in the collective tree
	const targetNode = findCollectiveNodeById(collectiveTree.root, targetNodeId);
	if (!targetNode) {
		return contributorShares;
	}

	// Calculate each contributor's share of this node
	for (const contributorId of collectiveTree.contributors) {
		let contributorShare = 0;

		// Get the contributor's weight in the collective
		const contributorWeight = collectiveTree.root.contributor_weights[contributorId] || 0;

		// For collective nodes, shares come from source contributors
		if (targetNode.type === 'CollectiveNonRootNode') {
			const sourceContribution = targetNode.source_contributors[contributorId] || 0;
			contributorShare = sourceContribution * contributorWeight;
		} else if (targetNode.type === 'CollectiveRootNode') {
			contributorShare = contributorWeight;
		}

		contributorShares[contributorId] = contributorShare;
	}

	return contributorShares;
}

// Helper function to find a node by ID in a collective tree
function findCollectiveNodeById(node: CollectiveNode, nodeId: string): CollectiveNode | null {
	if (node.id === nodeId) {
		return node;
	}

	for (const child of node.children) {
		const found = findCollectiveNodeById(child as CollectiveNode, nodeId);
		if (found) {
			return found;
		}
	}

	return null;
}

// Calculate the "path weight" - how much influence a contributor has on a specific node
function calculatePathWeight(
	collectiveTree: CollectiveTree,
	contributorId: string,
	targetNodeId: string
): number {
	const targetNode = findCollectiveNodeById(collectiveTree.root, targetNodeId);
	if (!targetNode) {
		return 0;
	}

	// Get the path from root to target node
	const path = getPathToCollectiveNode(collectiveTree.root, targetNodeId);
	if (!path || path.length === 0) {
		return 0;
	}

	let pathWeight = 1.0;

	// Walk down the path, multiplying weights at each level
	for (let i = 0; i < path.length; i++) {
		const nodeId = path[i];
		const node = findCollectiveNodeById(collectiveTree.root, nodeId);

		if (!node) continue;

		if (node.type === 'CollectiveRootNode') {
			// Root level: use contributor weight
			pathWeight *= node.contributor_weights[contributorId] || 0;
		} else if (node.type === 'CollectiveNonRootNode') {
			// Non-root level: use source contributor weight × node weight
			const sourceContribution = node.source_contributors[contributorId] || 0;
			pathWeight *= sourceContribution * node.weight_percentage;
		}
	}

	return pathWeight;
}

// Get path to a node in collective tree
function getPathToCollectiveNode(
	tree: CollectiveNode,
	nodeId: string,
	path: string[] = []
): string[] | null {
	if (tree.id === nodeId) {
		return [...path, tree.id];
	}

	for (const child of tree.children) {
		const childPath = getPathToCollectiveNode(child as CollectiveNode, nodeId, [...path, tree.id]);
		if (childPath) {
			return childPath;
		}
	}

	return null;
}

// Advanced: Calculate collective recognition - how much collective recognition flows to a node
function calculateCollectiveRecognition(
	collectiveTree: CollectiveTree,
	targetNodeId: string
): {
	total_collective_recognition: number;
	contributor_collective_recognition: Record<string, number>;
	path_analysis: Array<{
		node_id: string;
		depth: number;
		cumulative_weight: number;
		contributor_weights: Record<string, number>;
	}>;
} {
	const path = getPathToCollectiveNode(collectiveTree.root, targetNodeId);
	if (!path) {
		return {
			total_collective_recognition: 0,
			contributor_collective_recognition: {},
			path_analysis: []
		};
	}

	const contributorCollectiveRecognition: Record<string, number> = {};
	const pathAnalysis: Array<{
		node_id: string;
		depth: number;
		cumulative_weight: number;
		contributor_weights: Record<string, number>;
	}> = [];

	// Initialize contributor collective recognition
	for (const contributorId of collectiveTree.contributors) {
		contributorCollectiveRecognition[contributorId] = 1.0; // Start with full recognition
	}

	// Walk down the path, calculating collective recognition at each level
	for (let i = 0; i < path.length; i++) {
		const nodeId = path[i];
		const node = findCollectiveNodeById(collectiveTree.root, nodeId);

		if (!node) continue;

		const levelWeights: Record<string, number> = {};

		for (const contributorId of collectiveTree.contributors) {
			if (node.type === 'CollectiveRootNode') {
				const contributorWeight = node.contributor_weights[contributorId] || 0;
				contributorCollectiveRecognition[contributorId] *= contributorWeight;
			} else if (node.type === 'CollectiveNonRootNode') {
				const sourceContribution = node.source_contributors[contributorId] || 0;
				contributorCollectiveRecognition[contributorId] *=
					sourceContribution * node.weight_percentage;
			}

			levelWeights[contributorId] = contributorCollectiveRecognition[contributorId];
		}

		const cumulativeWeight = Object.values(levelWeights).reduce((sum, w) => sum + w, 0);

		pathAnalysis.push({
			node_id: nodeId,
			depth: i,
			cumulative_weight: cumulativeWeight,
			contributor_weights: { ...levelWeights }
		});
	}

	const totalCollectiveRecognition = Object.values(contributorCollectiveRecognition).reduce(
		(sum, r) => sum + r,
		0
	);

	return {
		total_collective_recognition: totalCollectiveRecognition,
		contributor_collective_recognition: contributorCollectiveRecognition,
		path_analysis: pathAnalysis
	};
}

// Export mathematical analysis functions
export {
	convertTreeToPercentages,
	calculateNodeContributorShares,
	calculatePathWeight,
	calculateCollectiveRecognition,
	findCollectiveNodeById,
	getPathToCollectiveNode
};

// === CONTRIBUTOR PERCENTAGE CALCULATION ===

/**
 * Calculate what percentage a contributor represents of a given node in the collective tree
 *
 * Mathematical Derivation:
 *
 * Step 1: Individual Node Percentage
 * - In contributor's individual tree: node_percentage = points / sum(sibling_points)
 * - This gives the node's weight relative to its siblings in the individual tree
 *
 * Step 2: Contributor's Collective Weight
 * - contributor_weight = Σ(mutual_recognition_from_others) / total_collective_recognition
 * - This is the contributor's overall influence in the collective
 *
 * Step 3: Path-Weighted Contribution
 * - For each level from root to target node:
 *   - level_contribution = individual_node_percentage × contributor_weight
 *   - These multiply along the path: path_contribution = ∏(level_contributions)
 *
 * Step 4: Normalization
 * - All contributors' path_contributions are summed for the target node
 * - contributor_percentage = contributor_path_contribution / sum(all_path_contributions)
 *
 * Final Formula:
 * contributor_percentage_of_node = (individual_node_path_weight × contributor_collective_weight) / sum(all_weighted_contributions)
 */

interface ProportionalNode {
	contributor_id: string;
	percentage_of_node: number;
	individual_node_percentage: number;
	contributor_collective_weight: number;
	path_weight_contribution: number;
	derivation_steps: Array<{
		level: number;
		node_id: string;
		individual_percentage: number;
		collective_weight: number;
		cumulative_path_weight: number;
	}>;
}

function calculateContributorPercentageOfNode(
	collectiveTree: CollectiveTree,
	nodeId: string,
	contributorId: string
): ProportionalNode {
	// Step 1: Find the target node in collective tree
	const targetNode = findCollectiveNodeById(collectiveTree.root, nodeId);
	if (!targetNode) {
		return {
			contributor_id: contributorId,
			percentage_of_node: 0,
			individual_node_percentage: 0,
			contributor_collective_weight: 0,
			path_weight_contribution: 0,
			derivation_steps: []
		};
	}

	// Step 2: Get contributor's collective weight
	const contributorCollectiveWeight = collectiveTree.root.contributor_weights[contributorId] || 0;

	// Step 3: Calculate path from root to target node
	const path = getPathToCollectiveNode(collectiveTree.root, nodeId);
	if (!path) {
		return {
			contributor_id: contributorId,
			percentage_of_node: 0,
			individual_node_percentage: 0,
			contributor_collective_weight: contributorCollectiveWeight,
			path_weight_contribution: 0,
			derivation_steps: []
		};
	}

	// Step 4: Calculate path-weighted contribution
	const derivationSteps: Array<{
		level: number;
		node_id: string;
		individual_percentage: number;
		collective_weight: number;
		cumulative_path_weight: number;
	}> = [];

	let cumulativePathWeight = 1.0;

	for (let i = 0; i < path.length; i++) {
		const currentNodeId = path[i];
		const currentNode = findCollectiveNodeById(collectiveTree.root, currentNodeId);

		if (!currentNode) continue;

		let individualPercentage = 1.0;

		if (currentNode.type === 'CollectiveRootNode') {
			// Root level: contributor's collective weight is their percentage
			individualPercentage = contributorCollectiveWeight;
			cumulativePathWeight *= contributorCollectiveWeight;
		} else if (currentNode.type === 'CollectiveNonRootNode') {
			// Non-root level: use source contributor data
			const sourceContribution = currentNode.source_contributors[contributorId] || 0;
			individualPercentage = sourceContribution;
			cumulativePathWeight *= sourceContribution;
		}

		derivationSteps.push({
			level: i,
			node_id: currentNodeId,
			individual_percentage: individualPercentage,
			collective_weight: contributorCollectiveWeight,
			cumulative_path_weight: cumulativePathWeight
		});
	}

	// Step 5: Get final percentage from target node's source_contributors
	let finalPercentage = 0;
	if (targetNode.type === 'CollectiveRootNode') {
		finalPercentage = contributorCollectiveWeight;
	} else if (targetNode.type === 'CollectiveNonRootNode') {
		finalPercentage = targetNode.source_contributors[contributorId] || 0;
	}

	// Step 6: Calculate individual node percentage (from original tree)
	const individualNodePercentage = calculateIndividualNodePercentage(
		collectiveTree,
		nodeId,
		contributorId
	);

	return {
		contributor_id: contributorId,
		percentage_of_node: finalPercentage,
		individual_node_percentage: individualNodePercentage,
		contributor_collective_weight: contributorCollectiveWeight,
		path_weight_contribution: cumulativePathWeight,
		derivation_steps: derivationSteps
	};
}

// Calculate all contributors' percentages for a given node
function calculateAllContributorPercentagesOfNode(
	collectiveTree: CollectiveTree,
	nodeId: string
): Array<ProportionalNode> {
	const analyses: Array<ProportionalNode> = [];

	for (const contributorId of collectiveTree.contributors) {
		const analysis = calculateContributorPercentageOfNode(collectiveTree, nodeId, contributorId);
		analyses.push(analysis);
	}

	// Verify percentages sum to 1.0 (within floating point precision)
	const totalPercentage = analyses.reduce((sum, analysis) => sum + analysis.percentage_of_node, 0);

	if (Math.abs(totalPercentage - 1.0) > 0.001) {
		console.warn(`Node ${nodeId} contributor percentages sum to ${totalPercentage}, not 1.0`);
	}

	return analyses;
}

// Helper function to calculate individual node percentage from original tree
function calculateIndividualNodePercentage(
	collectiveTree: CollectiveTree,
	nodeId: string,
	contributorId: string
): number {
	// This would require access to original individual trees
	// For now, we approximate from the collective tree data
	const targetNode = findCollectiveNodeById(collectiveTree.root, nodeId);
	if (!targetNode || targetNode.type !== 'CollectiveNonRootNode') {
		return 0;
	}

	// The source_contributors field already contains the normalized percentages
	// To get the original individual percentage, we need to "denormalize" it
	const sourceContribution = targetNode.source_contributors[contributorId] || 0;
	const contributorWeight = collectiveTree.root.contributor_weights[contributorId] || 0;

	// Approximate individual percentage (this is an approximation)
	return contributorWeight > 0 ? sourceContribution / contributorWeight : 0;
}

// Detailed mathematical explanation function
function explainContributorPercentageCalculation(
	collectiveTree: CollectiveTree,
	nodeId: string,
	contributorId: string
): string {
	const analysis = calculateContributorPercentageOfNode(collectiveTree, nodeId, contributorId);

	let explanation = `\n=== CONTRIBUTOR PERCENTAGE CALCULATION ===\n`;
	explanation += `Contributor: ${contributorId}\n`;
	explanation += `Target Node: ${nodeId}\n`;
	explanation += `Final Percentage: ${(analysis.percentage_of_node * 100).toFixed(2)}%\n\n`;

	explanation += `MATHEMATICAL DERIVATION:\n\n`;

	explanation += `1. Contributor's Collective Weight: ${(analysis.contributor_collective_weight * 100).toFixed(2)}%\n`;
	explanation += `   - This is the contributor's overall influence in the collective\n`;
	explanation += `   - Calculated from mutual recognition: Σ(recognition_from_others) / total_recognition\n\n`;

	explanation += `2. Path-Weighted Contribution:\n`;
	for (let i = 0; i < analysis.derivation_steps.length; i++) {
		const step = analysis.derivation_steps[i];
		explanation += `   Level ${step.level} (${step.node_id}):\n`;
		explanation += `     - Individual percentage at this level: ${(step.individual_percentage * 100).toFixed(2)}%\n`;
		explanation += `     - Cumulative path weight: ${(step.cumulative_path_weight * 100).toFixed(2)}%\n`;
	}

	explanation += `\n3. Final Calculation:\n`;
	explanation += `   - The contributor represents ${(analysis.percentage_of_node * 100).toFixed(2)}% of node ${nodeId}\n`;
	explanation += `   - This is stored in the collective node's source_contributors field\n`;
	explanation += `   - It represents the normalized contribution after accounting for all contributors\n\n`;

	explanation += `VERIFICATION:\n`;
	const allAnalyses = calculateAllContributorPercentagesOfNode(collectiveTree, nodeId);
	const totalPercentage = allAnalyses.reduce((sum, a) => sum + a.percentage_of_node, 0);
	explanation += `All contributors' percentages sum to: ${(totalPercentage * 100).toFixed(2)}%\n`;

	return explanation;
}

// Export the new functions
export {
	calculateContributorPercentageOfNode,
	calculateAllContributorPercentagesOfNode,
	explainContributorPercentageCalculation,
	type ProportionalNode as ContributorNodeAnalysis
};

function generateMathematicalProof(
	proposal: Record<string, number>,
	influences: Record<string, Record<string, number>>
): string {
	let proof = 'MATHEMATICAL DERIVATION OF COLLECTIVE PROPOSAL:\n\n';

	proof += 'For each node N in the collective tree:\n';
	proof += 'proposal_value(N) = Σ(contributor_influence(i, N)) where i ∈ contributors\n';
	proof += 'contributor_influence(i, N) = path_weight(i, N) × recognition_weight(i)\n\n';

	proof += 'This ensures the proposal mathematically represents the collective will\n';
	proof += "weighted by each contributor's recognition and structural influence.\n";

	return proof;
}

// === 3. DISTRIBUTED CAPACITY ALLOCATION ===

/**
 * The same mathematical principles that govern decision-making also govern
 * resource allocation. This creates PERFECT ALIGNMENT between collective
 * decisions and collective resources.
 */

interface CollectiveCapacityAllocation {
	collective_tree_id: string;
	total_collective_capacity: Record<string, number>; // Capacity type → total amount
	node_capacity_allocations: Record<string, Record<string, number>>; // Node ID → Capacity type → allocated amount
	contributor_capacity_shares: Record<string, Record<string, number>>; // Contributor → Capacity type → share
	allocation_efficiency: number; // How efficiently capacity is allocated
	allocation_fairness: number; // How fairly capacity is distributed
}

function calculateCollectiveCapacityAllocation(
	collectiveTree: CollectiveTree,
	individualCapacities: Record<string, Record<string, number>> // Contributor → Capacity type → amount
): CollectiveCapacityAllocation {
	const totalCollectiveCapacity: Record<string, number> = {};
	const nodeCapacityAllocations: Record<string, Record<string, number>> = {};
	const contributorCapacityShares: Record<string, Record<string, number>> = {};

	// Calculate total collective capacity
	for (const contributorId of collectiveTree.contributors) {
		const contributorWeight = collectiveTree.root.contributor_weights[contributorId] || 0;
		const contributorCapacities = individualCapacities[contributorId] || {};

		contributorCapacityShares[contributorId] = {};

		for (const [capacityType, amount] of Object.entries(contributorCapacities)) {
			const weightedAmount = amount * contributorWeight;

			totalCollectiveCapacity[capacityType] =
				(totalCollectiveCapacity[capacityType] || 0) + weightedAmount;
			contributorCapacityShares[contributorId][capacityType] = weightedAmount;
		}
	}

	// Allocate capacity to nodes based on their collective weights
	function allocateCapacityToNode(node: CollectiveNode, pathWeight: number = 1.0): void {
		nodeCapacityAllocations[node.id] = {};

		// Calculate this node's share of total collective capacity
		const nodeWeight = node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage;
		const effectiveWeight = pathWeight * nodeWeight;

		for (const [capacityType, totalAmount] of Object.entries(totalCollectiveCapacity)) {
			const nodeAllocation = totalAmount * effectiveWeight;
			nodeCapacityAllocations[node.id][capacityType] = nodeAllocation;
		}

		// Recursively allocate to children
		for (const child of node.children) {
			const childWeight = child.type === 'CollectiveNonRootNode' ? child.weight_percentage : 1.0;
			allocateCapacityToNode(child as CollectiveNode, pathWeight * childWeight);
		}
	}

	allocateCapacityToNode(collectiveTree.root);

	// Calculate allocation efficiency and fairness
	const totalAllocated = Object.values(nodeCapacityAllocations).reduce(
		(sum, nodeAlloc) => sum + Object.values(nodeAlloc).reduce((s, a) => s + a, 0),
		0
	);
	const totalAvailable = Object.values(totalCollectiveCapacity).reduce((sum, cap) => sum + cap, 0);

	const allocationEfficiency = totalAvailable > 0 ? totalAllocated / totalAvailable : 0;

	// Calculate fairness as inverse of allocation variance
	const nodeAllocations = Object.values(nodeCapacityAllocations).map((alloc) =>
		Object.values(alloc).reduce((sum, a) => sum + a, 0)
	);
	const meanAllocation = nodeAllocations.reduce((sum, a) => sum + a, 0) / nodeAllocations.length;
	const allocationVariance =
		nodeAllocations.reduce((sum, a) => sum + Math.pow(a - meanAllocation, 2), 0) /
		nodeAllocations.length;
	const allocationFairness = 1 / (1 + allocationVariance);

	return {
		collective_tree_id: collectiveTree.id,
		total_collective_capacity: totalCollectiveCapacity,
		node_capacity_allocations: nodeCapacityAllocations,
		contributor_capacity_shares: contributorCapacityShares,
		allocation_efficiency: allocationEfficiency,
		allocation_fairness: allocationFairness
	};
}

// Export the architectural analysis functions
export { calculateCollectiveCapacityAllocation, type CollectiveCapacityAllocation };

// === COLLECTIVE TREE FILTERING MATHEMATICS ===

/**
 * ## Collective Tree Filtering: Mathematical Foundations 🔍
 *
 * Building on the collective tree mathematics, filtering functions enable selective
 * extraction of sub-collectives based on mathematical thresholds and criteria.
 *
 * ## Mathematical Principles
 *
 * ### 1. Minimum Percentage Filtering
 *
 * Filter nodes based on their collective weight percentage:
 *
 * ```
 * Filter Condition:
 * node_weight ≥ minimum_percentage_threshold
 *
 * Where:
 * node_weight = Σ(individual_node_path_weight_i × contributor_weight_i)
 *
 * Collective Tree (Original):
 * CollectiveRoot [100%]
 * ├── Community Building [60%] ← KEEP (≥ 50%)
 * │   ├── Housing Project [45%] ← REMOVE (< 50%)
 * │   └── Food Garden [15%] ← REMOVE (< 50%)
 * └── Technical Skills [40%] ← REMOVE (< 50%)
 *
 * Filtered Tree (min_percentage = 0.50):
 * CollectiveRoot [100%]
 * └── Community Building [60%] ← Only node meeting threshold
 * ```
 *
 * ### 2. Minimum Quorum Filtering
 *
 * Filter nodes based on contributor count:
 *
 * ```
 * Filter Condition:
 * |node_contributors| ≥ minimum_quorum_threshold
 *
 * Where:
 * node_contributors = {contributor_i : source_contribution_i > 0}
 *
 * Collective Tree (Original):
 * CollectiveRoot [100%] (Contributors: Alice, Bob, Carol)
 * ├── Node A [40%] (Contributors: Alice, Bob) ← 2 contributors
 * ├── Node B [35%] (Contributors: Alice) ← 1 contributor
 * └── Node C [25%] (Contributors: Bob, Carol, David) ← 3 contributors
 *
 * Filtered Tree (min_quorum = 2):
 * CollectiveRoot [100%]
 * ├── Node A [40%] ← KEEP (2 ≥ 2)
 * └── Node C [25%] ← KEEP (3 ≥ 2)
 * ```
 *
 * ### 3. Collective Recognition Filtering
 *
 * Filter nodes based on collective recognition intensity:
 *
 * ```
 * Filter Condition:
 * collective_recognition(node) ≥ minimum_collective_recognition_threshold
 *
 * Where:
 * collective_recognition(node) = Σ(contributor_path_weight_i × contributor_recognition_i)
 * contributor_path_weight_i = ∏(percentage_at_each_level_from_root)
 *
 * Collective Recognition Calculation:
 * Node: Software Development
 * Path: Root → Technical Skills → Software Development
 *
 * Alice: 1.0 × 0.4 × 0.8 × 0.417 = 0.133
 * Bob: 1.0 × 0.5 × 0.7 × 0.417 = 0.146
 * Carol: 1.0 × 0.2 × 0.9 × 0.167 = 0.030
 *
 * Total Collective Recognition = 0.133 + 0.146 + 0.030 = 0.309
 * ```
 *
 * ### 4. Capacity Allocation Filtering
 *
 * Filter nodes based on collective capacity allocation:
 *
 * ```
 * Filter Condition:
 * capacity_allocation(node) ≥ minimum_capacity_threshold
 *
 * Where:
 * capacity_allocation(node) = total_collective_capacity × node_weight
 * total_collective_capacity = Σ(individual_capacity_i × contributor_weight_i)
 *
 * Example - Housing Capacity Filter:
 * Total Collective Housing = 4.2 rooms
 * Node Weight = 0.35 (35%)
 * Node Capacity Allocation = 4.2 × 0.35 = 1.47 rooms
 *
 * Filter threshold = 1.0 room → KEEP (1.47 ≥ 1.0)
 * Filter threshold = 2.0 rooms → REMOVE (1.47 < 2.0)
 * ```
 *
 * ### 5. Multi-Criteria Filtering
 *
 * Combine multiple filter conditions with logical AND:
 *
 * ```
 * Combined Filter Condition:
 * keep_node = (node_weight ≥ min_percentage) AND
 *            (|contributors| ≥ min_quorum) AND
 *            (collective_recognition ≥ min_collective_recognition) AND
 *            (capacity_allocation ≥ min_capacity) AND
 *            (whitelist_check = true) AND
 *            (blacklist_check = false)
 *
 * Example Multi-Criteria Filter:
 * - min_percentage: 0.1 (10%)
 * - min_quorum: 2 contributors
 * - min_collective_recognition: 0.05
 * - contributor_whitelist: ["Alice", "Bob"]
 *
 * Node Analysis:
 * Node X: weight=0.15, contributors=["Alice","Bob"], collective_recognition=0.08, whitelist=✓
 * Result: KEEP (meets all criteria)
 *
 * Node Y: weight=0.12, contributors=["Carol"], collective_recognition=0.06, whitelist=✗
 * Result: REMOVE (fails quorum and whitelist)
 * ```
 *
 * ## Path Preservation Mathematics
 *
 * When `preserve_paths = true`, structural parent nodes are kept even if they
 * fail filter criteria, maintaining tree connectivity:
 *
 * ```
 * Preservation Logic:
 * keep_node = (node_passes_filter) OR
 *            (has_valid_children AND preserve_paths)
 *
 * Original Tree:
 * Root [100%]
 * ├── Parent A [5%] ← Fails percentage filter (< 10%)
 * │   ├── Child A1 [60%] ← Passes filter
 * │   └── Child A2 [80%] ← Passes filter
 * └── Parent B [8%] ← Fails percentage filter, no valid children
 *
 * Filtered Tree (preserve_paths = true):
 * Root [100%]
 * └── Parent A [5%] (filtered) ← PRESERVED as structural parent
 *     ├── Child A1 [60%]
 *     └── Child A2 [80%]
 * ```
 *
 * ## Filter Statistics Mathematics
 *
 * Each filter operation provides comprehensive statistics:
 *
 * ```
 * Filter Efficiency Metrics:
 *
 * node_retention_rate = filtered_node_count / original_node_count
 * weight_retention_rate = (1 - total_weight_removed)
 * contributor_retention_rate = |remaining_contributors| / |original_contributors|
 *
 * Filter Selectivity:
 * selectivity = nodes_removed / original_node_count
 *
 * High selectivity (> 0.5) = Very restrictive filter
 * Low selectivity (< 0.2) = Permissive filter
 * ```
 *
 * ## Contributor Impact Analysis
 *
 * Mathematical analysis of how filtering affects each contributor:
 *
 * ```
 * Contributor Impact Metrics:
 *
 * node_impact(contributor) = nodes_removed_with_contributor / total_nodes_with_contributor
 * weight_impact(contributor) = weight_removed_with_contributor / total_weight_with_contributor
 *
 * Contributor Resilience:
 * resilience(contributor) = 1 - node_impact(contributor)
 *
 * Example Analysis:
 * Alice: 8 original nodes → 6 filtered nodes = 75% resilience
 * Bob: 5 original nodes → 2 filtered nodes = 40% resilience
 * Carol: 3 original nodes → 0 filtered nodes = 0% resilience
 *
 * Interpretation:
 * - Alice: High resilience, well-distributed influence
 * - Bob: Moderate resilience, some concentration risk
 * - Carol: Low resilience, influence concentrated in filtered areas
 * ```
 *
 * ## Practical Filter Applications
 *
 * ### 1. Consensus Threshold Implementation
 * ```
 * // Filter to nodes with significant collective support
 * const consensusTree = filterTreeByMinimumPercentage(collectiveTree, 0.67); // 67% threshold
 * ```
 *
 * ### 2. Expertise-Based Filtering
 * ```
 * // Filter to nodes with sufficient expert participation
 * const expertTree = filterTreeByMinimumQuorum(collectiveTree, 3); // At least 3 experts
 * ```
 *
 * ### 3. Resource-Constrained Allocation
 * ```
 * // Filter to nodes meeting minimum resource requirements
 * const feasibleTree = filterTreeByCapacityAllocation(
 *   collectiveTree,
 *   capacityAllocation,
 *   100 // Minimum 100 units of capacity
 * );
 * ```
 *
 * ### 4. Governance Structure Creation
 * ```
 * // Create governance structure with multiple requirements
 * const governanceTree = filterTreeByMultipleCriteria(collectiveTree, {
 *   minimum_percentage: 0.1,      // 10% collective weight
 *   minimum_quorum: 2,            // At least 2 contributors
 *   minimum_collective_recognition: 0.05, // Meaningful collective recognition
 *   preserve_paths: true          // Maintain structure
 * });
 * ```
 *
 * ## Mathematical Properties of Filtered Trees
 *
 * ### 1. Weight Conservation
 * ```
 * For any filtered tree:
 * Σ(filtered_node_weights) ≤ Σ(original_node_weights)
 *
 * Weight is conserved within remaining subtrees but may be lost through filtering
 * ```
 *
 * ### 2. Proportional Consistency
 * ```
 * For preserved nodes:
 * relative_weight(node_A, node_B) = constant across filtering
 *
 * Filtering preserves propbortional relationships between remaining nodes
 * ```
 *
 * ### 3. Path Dependency Preservation
 * ```
 * For any remaining node:
 * filtered_path_weight ∝ original_path_weight × filter_selectivity
 *
 * Path-dependent recognition effects are preserved proportionally
 * ```
 *
 * ### 4. Collective Intelligence Conservation
 * ```
 * Filtered collective intelligence ⊆ Original collective intelligence
 *
 * Filtering creates focused sub-collectives that inherit mathematical properties
 * of the original collective while operating on reduced decision spaces
 * ```
 */

// === COLLECTIVE TREE FILTERING FUNCTIONS ===

/**
 * Filter Functions for Collective Trees
 *
 * These functions create filtered versions of collective trees based on various criteria:
 * - Minimum percentage thresholds
 * - Minimum contributor quorum
 * - Collective recognition thresholds
 * - Capacity allocation minimums
 */

interface TreeFilterConfig {
	minimum_percentage?: number; // Filter nodes below this percentage (0.0-1.0)
	minimum_quorum?: number; // Filter nodes with fewer contributors than this
	minimum_collective_recognition?: number; // Filter nodes below this collective recognition
	minimum_capacity_allocation?: number; // Filter nodes below this capacity threshold
	preserve_paths?: boolean; // Keep parent nodes even if they don't meet criteria (to preserve structure)
	contributor_whitelist?: string[]; // Only include nodes with these contributors
	contributor_blacklist?: string[]; // Exclude nodes with these contributors
}

interface FilteredTreeResult {
	filtered_tree: CollectiveTree;
	removed_nodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}>;
	filter_stats: {
		original_node_count: number;
		filtered_node_count: number;
		nodes_removed: number;
		total_weight_removed: number;
		contributors_affected: string[];
	};
}

// Helper function to get contributors from either node type
function getNodeContributors(node: CollectiveNode): string[] {
	if (node.type === 'CollectiveRootNode') {
		return node.contributors || [];
	} else if (node.type === 'CollectiveNonRootNode') {
		return node.contributor_ids || [];
	}
	return [];
}

// Filter collective tree based on minimum percentage threshold
function filterTreeByMinimumPercentage(
	collectiveTree: CollectiveTree,
	minimumPercentage: number,
	preservePaths: boolean = true
): FilteredTreeResult {
	const removedNodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}> = [];

	let totalWeightRemoved = 0;
	const contributorsAffected = new Set<string>();

	function shouldKeepNode(node: CollectiveNode): boolean {
		const nodeWeight = node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage;
		return nodeWeight >= minimumPercentage;
	}

	function filterNodeRecursive(
		node: CollectiveNode,
		parentId: string | null
	): CollectiveNode | null {
		// Filter children first
		const filteredChildren: CollectiveNode[] = [];

		for (const child of node.children) {
			const filteredChild = filterNodeRecursive(child as CollectiveNode, node.id);
			if (filteredChild) {
				filteredChildren.push(filteredChild);
			}
		}

		// Check if current node should be kept
		const keepNode = shouldKeepNode(node);
		const hasValidChildren = filteredChildren.length > 0;

		if (!keepNode && !hasValidChildren) {
			// Node and all children fail criteria - remove entirely
			removedNodes.push({
				node_id: node.id,
				node_name: node.name,
				reason: `Below minimum percentage threshold (${minimumPercentage})`,
				original_weight: node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage,
				contributor_count: getNodeContributors(node).length
			});

			if (node.type === 'CollectiveNonRootNode') {
				totalWeightRemoved += node.weight_percentage;
			}

			// Track affected contributors
			const nodeContributors = getNodeContributors(node);
			nodeContributors.forEach((id) => contributorsAffected.add(id));

			return null;
		}

		if (!keepNode && hasValidChildren && preservePaths) {
			// Keep node as structural parent even though it doesn't meet criteria
			return {
				...node,
				children: filteredChildren,
				name: `${node.name} (filtered)`
			};
		}

		if (keepNode) {
			// Keep node and update children
			return {
				...node,
				children: filteredChildren
			};
		}

		// This shouldn't happen, but return null as fallback
		return null;
	}

	const originalNodeCount = countNodesInTree(collectiveTree.root);
	const filteredRoot = filterNodeRecursive(collectiveTree.root, null);

	if (!filteredRoot) {
		throw new Error('Cannot filter root node - tree would be empty');
	}

	// Create filtered tree and renormalize to ensure children weights sum to 100%
	const preNormalizedTree: CollectiveTree = {
		...collectiveTree,
		root: filteredRoot as CollectiveRootNode,
		last_updated: new Date().toISOString()
	};

	const filteredTree = renormalizeCollectiveTree(preNormalizedTree);

	const filteredNodeCount = countNodesInTree(filteredTree.root);

	return {
		filtered_tree: filteredTree,
		removed_nodes: removedNodes,
		filter_stats: {
			original_node_count: originalNodeCount,
			filtered_node_count: filteredNodeCount,
			nodes_removed: removedNodes.length,
			total_weight_removed: totalWeightRemoved,
			contributors_affected: Array.from(contributorsAffected)
		}
	};
}

// Filter collective tree based on minimum contributor quorum
function filterTreeByMinimumQuorum(
	collectiveTree: CollectiveTree,
	minimumQuorum: number,
	preservePaths: boolean = true
): FilteredTreeResult {
	const removedNodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}> = [];

	let totalWeightRemoved = 0;
	const contributorsAffected = new Set<string>();

	function shouldKeepNode(node: CollectiveNode): boolean {
		const contributorCount = getNodeContributors(node).length;
		return contributorCount >= minimumQuorum;
	}

	function filterNodeRecursive(
		node: CollectiveNode,
		parentId: string | null
	): CollectiveNode | null {
		// Filter children first
		const filteredChildren: CollectiveNode[] = [];

		for (const child of node.children) {
			const filteredChild = filterNodeRecursive(child as CollectiveNode, node.id);
			if (filteredChild) {
				filteredChildren.push(filteredChild);
			}
		}

		// Check if current node should be kept
		const keepNode = shouldKeepNode(node);
		const hasValidChildren = filteredChildren.length > 0;

		if (!keepNode && !hasValidChildren) {
			// Node and all children fail criteria - remove entirely
			const contributorCount = getNodeContributors(node).length;
			removedNodes.push({
				node_id: node.id,
				node_name: node.name,
				reason: `Below minimum quorum (${minimumQuorum} contributors, has ${contributorCount})`,
				original_weight: node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage,
				contributor_count: contributorCount
			});

			if (node.type === 'CollectiveNonRootNode') {
				totalWeightRemoved += node.weight_percentage;
			}

			// Track affected contributors
			const nodeContributors = getNodeContributors(node);
			nodeContributors.forEach((id) => contributorsAffected.add(id));

			return null;
		}

		if (!keepNode && hasValidChildren && preservePaths) {
			// Keep node as structural parent
			return {
				...node,
				children: filteredChildren,
				name: `${node.name} (low quorum)`
			};
		}

		if (keepNode) {
			// Keep node and update children
			return {
				...node,
				children: filteredChildren
			};
		}

		return null;
	}

	const originalNodeCount = countNodesInTree(collectiveTree.root);
	const filteredRoot = filterNodeRecursive(collectiveTree.root, null);

	if (!filteredRoot) {
		throw new Error('Cannot filter root node - tree would be empty');
	}

	// Create filtered tree and renormalize to ensure children weights sum to 100%
	const preNormalizedTree: CollectiveTree = {
		...collectiveTree,
		root: filteredRoot as CollectiveRootNode,
		last_updated: new Date().toISOString()
	};

	const filteredTree = renormalizeCollectiveTree(preNormalizedTree);

	const filteredNodeCount = countNodesInTree(filteredTree.root);

	return {
		filtered_tree: filteredTree,
		removed_nodes: removedNodes,
		filter_stats: {
			original_node_count: originalNodeCount,
			filtered_node_count: filteredNodeCount,
			nodes_removed: removedNodes.length,
			total_weight_removed: totalWeightRemoved,
			contributors_affected: Array.from(contributorsAffected)
		}
	};
}

// Advanced filter with multiple criteria
function filterTreeByMultipleCriteria(
	collectiveTree: CollectiveTree,
	config: TreeFilterConfig
): FilteredTreeResult {
	const removedNodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}> = [];

	let totalWeightRemoved = 0;
	const contributorsAffected = new Set<string>();

	function shouldKeepNode(node: CollectiveNode): { keep: boolean; reasons: string[] } {
		const reasons: string[] = [];
		let keep = true;

		// Check minimum percentage
		if (config.minimum_percentage !== undefined) {
			const nodeWeight = node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage;
			if (nodeWeight < config.minimum_percentage) {
				keep = false;
				reasons.push(`below min percentage (${config.minimum_percentage})`);
			}
		}

		// Check minimum quorum
		if (config.minimum_quorum !== undefined) {
			const contributorCount = getNodeContributors(node).length;
			if (contributorCount < config.minimum_quorum) {
				keep = false;
				reasons.push(`below min quorum (${config.minimum_quorum})`);
			}
		}

		// Check contributor whitelist
		if (config.contributor_whitelist && config.contributor_whitelist.length > 0) {
			const nodeContributors = getNodeContributors(node);
			const hasWhitelistedContributor = nodeContributors.some((id) =>
				config.contributor_whitelist!.includes(id)
			);
			if (!hasWhitelistedContributor) {
				keep = false;
				reasons.push('no whitelisted contributors');
			}
		}

		// Check contributor blacklist
		if (config.contributor_blacklist && config.contributor_blacklist.length > 0) {
			const nodeContributors = getNodeContributors(node);
			const hasBlacklistedContributor = nodeContributors.some((id) =>
				config.contributor_blacklist!.includes(id)
			);
			if (hasBlacklistedContributor) {
				keep = false;
				reasons.push('has blacklisted contributors');
			}
		}

		// Check collective recognition (if provided)
		if (config.minimum_collective_recognition !== undefined) {
			const recognition = calculateCollectiveRecognition(collectiveTree, node.id);
			if (recognition.total_collective_recognition < config.minimum_collective_recognition) {
				keep = false;
				reasons.push(`below min collective recognition (${config.minimum_collective_recognition})`);
			}
		}

		return { keep, reasons };
	}

	function filterNodeRecursive(
		node: CollectiveNode,
		parentId: string | null
	): CollectiveNode | null {
		// Filter children first
		const filteredChildren: CollectiveNode[] = [];

		for (const child of node.children) {
			const filteredChild = filterNodeRecursive(child as CollectiveNode, node.id);
			if (filteredChild) {
				filteredChildren.push(filteredChild);
			}
		}

		// Check if current node should be kept
		const { keep: keepNode, reasons } = shouldKeepNode(node);
		const hasValidChildren = filteredChildren.length > 0;

		if (!keepNode && !hasValidChildren) {
			// Node and all children fail criteria - remove entirely
			removedNodes.push({
				node_id: node.id,
				node_name: node.name,
				reason: reasons.join(', '),
				original_weight: node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage,
				contributor_count: getNodeContributors(node).length
			});

			if (node.type === 'CollectiveNonRootNode') {
				totalWeightRemoved += node.weight_percentage;
			}

			// Track affected contributors
			const nodeContributors = getNodeContributors(node);
			nodeContributors.forEach((id) => contributorsAffected.add(id));

			return null;
		}

		if (!keepNode && hasValidChildren && config.preserve_paths) {
			// Keep node as structural parent
			return {
				...node,
				children: filteredChildren,
				name: `${node.name} (filtered: ${reasons.join(', ')})`
			};
		}

		if (keepNode) {
			// Keep node and update children
			return {
				...node,
				children: filteredChildren
			};
		}

		return null;
	}

	const originalNodeCount = countNodesInTree(collectiveTree.root);
	const filteredRoot = filterNodeRecursive(collectiveTree.root, null);

	if (!filteredRoot) {
		throw new Error('Cannot filter root node - tree would be empty');
	}

	// Create filtered tree and renormalize to ensure children weights sum to 100%
	const preNormalizedTree: CollectiveTree = {
		...collectiveTree,
		root: filteredRoot as CollectiveRootNode,
		last_updated: new Date().toISOString()
	};

	const filteredTree = renormalizeCollectiveTree(preNormalizedTree);

	const filteredNodeCount = countNodesInTree(filteredTree.root);

	return {
		filtered_tree: filteredTree,
		removed_nodes: removedNodes,
		filter_stats: {
			original_node_count: originalNodeCount,
			filtered_node_count: filteredNodeCount,
			nodes_removed: removedNodes.length,
			total_weight_removed: totalWeightRemoved,
			contributors_affected: Array.from(contributorsAffected)
		}
	};
}

// Helper function to count nodes in a tree
function countNodesInTree(node: CollectiveNode): number {
	let count = 1; // Count this node
	for (const child of node.children) {
		count += countNodesInTree(child as CollectiveNode);
	}
	return count;
}

// Filter tree to only show nodes above a certain capacity allocation threshold
function filterTreeByCapacityAllocation(
	collectiveTree: CollectiveTree,
	capacityAllocation: CollectiveCapacityAllocation,
	minimumCapacityValue: number,
	capacityType?: string
): FilteredTreeResult {
	const removedNodes: Array<{
		node_id: string;
		node_name: string;
		reason: string;
		original_weight: number;
		contributor_count: number;
	}> = [];

	let totalWeightRemoved = 0;
	const contributorsAffected = new Set<string>();

	function shouldKeepNode(node: CollectiveNode): boolean {
		const nodeAllocations = capacityAllocation.node_capacity_allocations[node.id] || {};

		if (capacityType) {
			// Check specific capacity type
			const allocation = nodeAllocations[capacityType] || 0;
			return allocation >= minimumCapacityValue;
		} else {
			// Check total capacity across all types
			const totalAllocation = Object.values(nodeAllocations).reduce((sum, val) => sum + val, 0);
			return totalAllocation >= minimumCapacityValue;
		}
	}

	function filterNodeRecursive(
		node: CollectiveNode,
		parentId: string | null
	): CollectiveNode | null {
		// Filter children first
		const filteredChildren: CollectiveNode[] = [];

		for (const child of node.children) {
			const filteredChild = filterNodeRecursive(child as CollectiveNode, node.id);
			if (filteredChild) {
				filteredChildren.push(filteredChild);
			}
		}

		// Check if current node should be kept
		const keepNode = shouldKeepNode(node);
		const hasValidChildren = filteredChildren.length > 0;

		if (!keepNode && !hasValidChildren) {
			// Node and all children fail criteria - remove entirely
			const nodeAllocations = capacityAllocation.node_capacity_allocations[node.id] || {};
			const totalAllocation = Object.values(nodeAllocations).reduce((sum, val) => sum + val, 0);

			removedNodes.push({
				node_id: node.id,
				node_name: node.name,
				reason: `Below minimum capacity allocation (${minimumCapacityValue}, has ${totalAllocation.toFixed(3)})`,
				original_weight: node.type === 'CollectiveRootNode' ? 1.0 : node.weight_percentage,
				contributor_count: getNodeContributors(node).length
			});

			if (node.type === 'CollectiveNonRootNode') {
				totalWeightRemoved += node.weight_percentage;
			}

			// Track affected contributors
			const nodeContributors = getNodeContributors(node);
			nodeContributors.forEach((id) => contributorsAffected.add(id));

			return null;
		}

		if (!keepNode && hasValidChildren) {
			// Keep node as structural parent
			return {
				...node,
				children: filteredChildren,
				name: `${node.name} (low capacity)`
			};
		}

		if (keepNode) {
			// Keep node and update children
			return {
				...node,
				children: filteredChildren
			};
		}

		return null;
	}

	const originalNodeCount = countNodesInTree(collectiveTree.root);
	const filteredRoot = filterNodeRecursive(collectiveTree.root, null);

	if (!filteredRoot) {
		throw new Error('Cannot filter root node - tree would be empty');
	}

	// Create filtered tree and renormalize to ensure children weights sum to 100%
	const preNormalizedTree: CollectiveTree = {
		...collectiveTree,
		root: filteredRoot as CollectiveRootNode,
		last_updated: new Date().toISOString()
	};

	const filteredTree = renormalizeCollectiveTree(preNormalizedTree);

	const filteredNodeCount = countNodesInTree(filteredTree.root);

	return {
		filtered_tree: filteredTree,
		removed_nodes: removedNodes,
		filter_stats: {
			original_node_count: originalNodeCount,
			filtered_node_count: filteredNodeCount,
			nodes_removed: removedNodes.length,
			total_weight_removed: totalWeightRemoved,
			contributors_affected: Array.from(contributorsAffected)
		}
	};
}

// Create a summary of what contributors remain after filtering
function analyzeFilteredContributors(
	originalTree: CollectiveTree,
	filteredResult: FilteredTreeResult
): {
	remaining_contributors: string[];
	removed_contributors: string[];
	contributor_node_counts: Record<string, { original: number; filtered: number }>;
} {
	const originalContributors = new Set(originalTree.contributors);
	const filteredContributors = new Set(filteredResult.filtered_tree.contributors);

	const remaining = Array.from(filteredContributors);
	const removed = Array.from(originalContributors).filter((id) => !filteredContributors.has(id));

	// Count nodes per contributor
	const contributorNodeCounts: Record<string, { original: number; filtered: number }> = {};

	function countContributorNodes(node: CollectiveNode, counts: Record<string, number>): void {
		const nodeContributors = getNodeContributors(node);
		for (const contributorId of nodeContributors) {
			counts[contributorId] = (counts[contributorId] || 0) + 1;
		}

		for (const child of node.children) {
			countContributorNodes(child as CollectiveNode, counts);
		}
	}

	const originalCounts: Record<string, number> = {};
	const filteredCounts: Record<string, number> = {};

	countContributorNodes(originalTree.root, originalCounts);
	countContributorNodes(filteredResult.filtered_tree.root, filteredCounts);

	for (const contributorId of Array.from(originalContributors)) {
		contributorNodeCounts[contributorId] = {
			original: originalCounts[contributorId] || 0,
			filtered: filteredCounts[contributorId] || 0
		};
	}

	return {
		remaining_contributors: remaining,
		removed_contributors: removed,
		contributor_node_counts: contributorNodeCounts
	};
}

// Export the filtering functions
export {
	filterTreeByMinimumPercentage,
	filterTreeByMinimumQuorum,
	filterTreeByMultipleCriteria,
	filterTreeByCapacityAllocation,
	analyzeFilteredContributors,
	countNodesInTree,
	renormalizeChildrenWeights,
	renormalizeCollectiveTree,
	type TreeFilterConfig,
	type FilteredTreeResult
};

// === RENORMALIZATION FUNCTIONS ===

/**
 * Renormalize children weights to sum to 100%
 *
 * MATHEMATICAL PRINCIPLE:
 * For any parent node with children, the sum of children weights must equal 100%
 * After filtering, we need to renormalize: new_weight = old_weight / sum_of_remaining_weights
 *
 * Example:
 * Original: [A: 30%, B: 40%, C: 30%] = 100%
 * After removing B: [A: 30%, C: 30%] = 60%
 * Renormalized: [A: 50%, C: 50%] = 100%
 */
function renormalizeChildrenWeights(parent: CollectiveNode): CollectiveNode {
	if (parent.children.length === 0) {
		return parent;
	}

	// Calculate current sum of children weights
	const childrenSum = parent.children.reduce((sum, child) => {
		const childWeight = child.type === 'CollectiveRootNode' ? 1.0 : child.weight_percentage;
		return sum + childWeight;
	}, 0);

	// If sum is already 1.0 (within floating point precision), no normalization needed
	if (Math.abs(childrenSum - 1.0) < 0.0001) {
		return parent;
	}

	// If sum is 0, we can't normalize - this shouldn't happen
	if (childrenSum === 0) {
		console.warn(`Cannot renormalize children of node ${parent.id} - sum is 0`);
		return parent;
	}

	// Renormalize children weights
	const normalizedChildren = parent.children.map((child) => {
		if (child.type === 'CollectiveRootNode') {
			// Root nodes always have weight 1.0
			return child;
		} else {
			// Renormalize non-root nodes
			const normalizedWeight = child.weight_percentage / childrenSum;
			return {
				...child,
				weight_percentage: normalizedWeight
			} as CollectiveNode;
		}
	});

	return {
		...parent,
		children: normalizedChildren
	};
}

/**
 * Recursively renormalize all levels of a collective tree
 * This ensures mathematical consistency throughout the tree structure
 */
function renormalizeCollectiveTree(tree: CollectiveTree): CollectiveTree {
	function renormalizeNodeRecursive(node: CollectiveNode): CollectiveNode {
		// First, recursively renormalize children
		const childrenNormalized = node.children.map((child) =>
			renormalizeNodeRecursive(child as CollectiveNode)
		);

		// Then renormalize this node's children
		const nodeWithNormalizedChildren = {
			...node,
			children: childrenNormalized
		};

		return renormalizeChildrenWeights(nodeWithNormalizedChildren);
	}

	const normalizedRoot = renormalizeNodeRecursive(tree.root);

	return {
		...tree,
		root: normalizedRoot as CollectiveRootNode,
		last_updated: new Date().toISOString()
	};
}

// === UPDATED FILTERING FUNCTIONS WITH RENORMALIZATION ===
