/*
Analyze this code in depth, you are a mathematician seeking to understand how we can use mutual-recognition proportional mathematics to divide the infinite of natural numbers of our own capacities

Okay, so far what this system has achieved is setting up a system where the root of a tree represents an individual who can declare their capacities as well as declare a natural number for the amount of that capacity they have. It recognizes the contributions of others towards the satisfaction of their needs to derive mutual recognition of mutual contribution which is mutual fulfillment. This can be used to divide the capacities across contributors, so we multiply the natural number of capacity by the percentage from the mutual fulfillment. 

Now we want to explore the possibility of an equation. Keep it simple. Look at the simplicity of our existing mutual recognition calculations. The minimum function, normalization, percentages.

We want to understand if there is a way that we can recursively extend proportions into synthetic collectives of individuals if we can somehow extend from mutual fulfillment calculations of individuals derive synthetic collective.
*/

import type { Node, CapacitiesCollection } from '$lib/schema';
import { mutualFulfillment as originalMutualFulfillment } from '$lib/protocol';

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

// Get node capacities (mock implementation - needs to be adapted to your actual capacity system)
function getNodeCapacities(node: Node): CapacitiesCollection {
	// This is a placeholder - you'll need to implement this based on your capacity storage system
	// For now, return empty collection
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
			const memberCap = Object.values(capacities).reduce(
				(acc, cap) => acc + (cap.quantity || 0),
				0
			);
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
