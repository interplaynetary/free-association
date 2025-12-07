/**
 * Shared Utilities for Collective Schedulers
 * 
 * Common data access patterns used by both:
 * - Collective Recognition & Membership Scheduler
 * - Collective Tree Scheduler
 * 
 * This reduces duplication and ensures consistency
 */

import { holsterGet, holsterNextPut, holsterGetArray, ensureAuthenticated } from '$lib/server/holster/db';
import { sharesOfGeneralFulfillmentMap } from '$lib/protocol/core/tree';
import type { RecognitionData } from '$lib/protocol/collective/schemas';
import type { Node } from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// SHARED RECOGNITION DATA EXTRACTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract recognition data from a single tree
 * 
 * Shared by:
 * - Membership computation (needs recognition relationships)
 * - Tree merging (needs recognition shares)
 */
export function extractRecognitionFromTree(
	userId: string,
	tree: Node
): RecognitionData[] {
	const recognitionData: RecognitionData[] = [];
	
	try {
		// Extract recognition shares from tree using protocol function
		const shares = sharesOfGeneralFulfillmentMap(tree);
		
		// Convert to RecognitionData format
		for (const [toId, share] of Object.entries(shares)) {
			recognitionData.push({
				fromId: userId,
				toId,
				percentage: share * 100, // Convert 0-1 to 0-100
				timestamp: new Date()
			});
		}
	} catch (err) {
		console.warn(`[SHARED-UTILS] Could not extract recognition from tree for ${userId}:`, err);
	}
	
	return recognitionData;
}

/**
 * Fetch all recognition data from all user trees
 * 
 * Shared by:
 * - Membership scheduler (fetchRecognitionData)
 * - Could be used by tree scheduler for validation
 */
export async function fetchAllRecognitionData(): Promise<RecognitionData[]> {
	try {
		ensureAuthenticated();
		
		const recognitionData: RecognitionData[] = [];
		
		// Fetch all users' trees
		const treesData = await holsterGet<Record<string, any>>(['trees']);
		
		if (!treesData) {
			console.warn('[SHARED-UTILS] No trees data found');
			return [];
		}
		
		// Extract recognition from each user's tree
		for (const [userId, userTrees] of Object.entries(treesData)) {
			if (userId === '_') continue; // Skip Gun metadata
			
			try {
				const tree = await holsterGet<Node>(['trees', userId, 'recognition_tree']);
				
				if (tree) {
					const userRecognition = extractRecognitionFromTree(userId, tree);
					recognitionData.push(...userRecognition);
				}
			} catch (err) {
				console.warn(`[SHARED-UTILS] Could not process tree for user ${userId}:`, err);
			}
		}
		
		return recognitionData;
	} catch (error) {
		console.error('[SHARED-UTILS] Failed to fetch recognition data:', error);
		return [];
	}
}

// ═══════════════════════════════════════════════════════════════════
// SHARED TREE FETCHING
// ═══════════════════════════════════════════════════════════════════

/**
 * Fetch a single tree from Holster
 * 
 * Shared by both schedulers for tree access
 */
export async function fetchTree(userId: string): Promise<Node | null> {
	try {
		ensureAuthenticated();
		const tree = await holsterGet<Node>(['trees', userId, 'recognition_tree']);
		return tree;
	} catch (err) {
		console.warn(`[SHARED-UTILS] Could not fetch tree for ${userId}:`, err);
		return null;
	}
}

/**
 * Fetch multiple trees from Holster
 * 
 * Shared by:
 * - Membership scheduler (fetchMemberTrees)
 * - Tree scheduler (fetchContributorTrees)
 */
export async function fetchTrees(userIds: string[]): Promise<Map<string, Node>> {
	try {
		ensureAuthenticated();
		
		const trees = new Map<string, Node>();
		
		// Fetch each user's tree
		for (const userId of userIds) {
			const tree = await fetchTree(userId);
			if (tree) {
				trees.set(userId, tree);
			}
		}
		
		return trees;
	} catch (error) {
		console.error('[SHARED-UTILS] Failed to fetch trees:', error);
		return new Map();
	}
}

/**
 * Fetch trees as a record (for tree merging)
 */
export async function fetchTreesAsRecord(userIds: string[]): Promise<Record<string, Node>> {
	const treesMap = await fetchTrees(userIds);
	return Object.fromEntries(treesMap);
}

// ═══════════════════════════════════════════════════════════════════
// SHARED CAPACITY EXTRACTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract capacities from a tree/node
 * 
 * Shared by:
 * - Allocation scheduler (needs capacity slots)
 * - Tree scheduler (needs total capacities)
 */
export function extractCapacitiesFromTree(tree: Node): Record<string, number> {
	const capacities: Record<string, number> = {};
	
	try {
		if ('capacities' in tree && tree.capacities) {
			// Extract capacity totals from slots
			for (const [capacityType, capacity] of Object.entries(tree.capacities as Record<string, any>)) {
				if (capacity.capacity_slots && Array.isArray(capacity.capacity_slots)) {
					const total = capacity.capacity_slots.reduce(
						(sum: number, slot: any) => sum + (slot.quantity || 0),
						0
					);
					if (total > 0) {
						capacities[capacityType] = total;
					}
				}
			}
		}
	} catch (err) {
		console.warn('[SHARED-UTILS] Could not extract capacities from tree:', err);
	}
	
	return capacities;
}

/**
 * Fetch all individual capacities
 * 
 * Shared by:
 * - Tree scheduler (fetchIndividualCapacities)
 * - Could be used by allocation scheduler for validation
 */
export async function fetchAllIndividualCapacities(): Promise<Record<string, Record<string, number>>> {
	try {
		ensureAuthenticated();
		
		const capacities: Record<string, Record<string, number>> = {};
		
		// Fetch all users and their trees
		const treesData = await holsterGet<Record<string, any>>(['trees']);
		
		if (!treesData) {
			console.warn('[SHARED-UTILS] No user data found');
			return {};
		}
		
		// For each user, extract their capacities
		for (const [userId, _] of Object.entries(treesData)) {
			if (userId === '_') continue; // Skip Gun metadata
			
			try {
				const tree = await fetchTree(userId);
				
				if (tree) {
					const userCapacities = extractCapacitiesFromTree(tree);
					
					if (Object.keys(userCapacities).length > 0) {
						capacities[userId] = userCapacities;
					}
				}
			} catch (err) {
				console.warn(`[SHARED-UTILS] Could not fetch capacities for ${userId}:`, err);
			}
		}
		
		return capacities;
	} catch (error) {
		console.error('[SHARED-UTILS] Failed to fetch individual capacities:', error);
		return {};
	}
}

// ═══════════════════════════════════════════════════════════════════
// SHARED LOGGING UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Log computation event to Holster
 * 
 * Shared by both schedulers for consistent logging
 */
export async function logComputationEvent(
	collection: 'computation_logs' | 'collective_tree_computation_logs',
	event: string,
	data: any
): Promise<void> {
	try {
		ensureAuthenticated();
		
		const timestamp = new Date();
		const logKey = `${event}_${timestamp.getTime()}`;
		
		// Store detailed log
		await holsterNextPut(collection, logKey, {
			event,
			data,
			timestamp: timestamp.toISOString()
		});
		
		// Update latest pointer
		const latestCollection = collection === 'computation_logs' 
			? 'computation_logs_latest'
			: 'collective_tree_computation_logs_latest';
			
		await holsterNextPut(latestCollection, event, {
			...data,
			timestamp: timestamp.toISOString()
		});
	} catch (error) {
		console.error('[SHARED-UTILS] Failed to log computation event:', error);
		// Don't throw - logging failures shouldn't break computations
	}
}

// ═══════════════════════════════════════════════════════════════════
// SHARED VALIDATION UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate that basic data is accessible
 * 
 * Shared validation pattern for both schedulers
 */
export async function validateBasicDataAccess(): Promise<{
	treesAccessible: boolean;
	treesCount: number;
	capacitiesAccessible: boolean;
	capacitiesCount: number;
	errors: string[];
}> {
	const errors: string[] = [];
	let treesAccessible = false;
	let treesCount = 0;
	let capacitiesAccessible = false;
	let capacitiesCount = 0;
	
	try {
		// Test tree access
		const treesData = await holsterGet<Record<string, any>>(['trees']);
		if (treesData) {
			treesAccessible = true;
			treesCount = Object.keys(treesData).filter(k => k !== '_').length;
		} else {
			errors.push('Trees data not accessible');
		}
	} catch (err) {
		errors.push(`Tree access failed: ${err}`);
	}
	
	try {
		// Test capacity access
		const capacities = await fetchAllIndividualCapacities();
		capacitiesAccessible = true;
		capacitiesCount = Object.keys(capacities).length;
	} catch (err) {
		errors.push(`Capacity access failed: ${err}`);
	}
	
	return {
		treesAccessible,
		treesCount,
		capacitiesAccessible,
		capacitiesCount,
		errors
	};
}

// ═══════════════════════════════════════════════════════════════════
// SHARED HISTORY UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Save computation result with history tracking
 * 
 * Generic pattern for saving results with timestamps and history
 */
export async function saveComputationResult(
	collection: string,
	id: string,
	result: any,
	latestCollection?: string
): Promise<void> {
	try {
		ensureAuthenticated();
		
		const timestamp = new Date();
		const resultKey = `${id}_${timestamp.getTime()}`;
		
		// Store detailed result
		await holsterNextPut(collection, resultKey, {
			...result,
			timestamp: timestamp.toISOString()
		});
		
		// Update latest pointer if specified
		if (latestCollection) {
			await holsterNextPut(latestCollection, id, {
				result_key: resultKey,
				timestamp: timestamp.toISOString(),
				...extractLatestPointerData(result)
			});
		}
	} catch (error) {
		console.error('[SHARED-UTILS] Failed to save computation result:', error);
		throw error;
	}
}

/**
 * Extract relevant data for latest pointer
 * (Override in specific implementations if needed)
 */
function extractLatestPointerData(result: any): Record<string, any> {
	// Extract common fields that are useful for quick access
	const pointerData: Record<string, any> = {};
	
	if ('total_capacity' in result) pointerData.total_capacity = result.total_capacity;
	if ('total_allocated' in result) pointerData.total_allocated = result.total_allocated;
	if ('member_count' in result) pointerData.member_count = result.member_count;
	if ('efficiency' in result) pointerData.efficiency = result.efficiency;
	if ('fairness' in result) pointerData.fairness = result.fairness;
	
	return pointerData;
}

// ═══════════════════════════════════════════════════════════════════
// EXPORT ALL UTILITIES
// ═══════════════════════════════════════════════════════════════════

export const SharedUtils = {
	// Recognition
	extractRecognitionFromTree,
	fetchAllRecognitionData,
	
	// Trees
	fetchTree,
	fetchTrees,
	fetchTreesAsRecord,
	
	// Capacities
	extractCapacitiesFromTree,
	fetchAllIndividualCapacities,
	
	// Logging
	logComputationEvent,
	
	// Validation
	validateBasicDataAccess,
	
	// Storage
	saveComputationResult
};

