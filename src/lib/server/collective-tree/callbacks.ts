/**
 * Collective Tree Scheduler Callbacks Implementation
 * 
 * Real implementation using Holster database utilities
 */

import type { CollectiveTreeCallbacks, CollectiveDefinition } from './scheduler';
import type { Node } from '$lib/protocol/schemas';
import type { CollectiveTree } from '$lib/protocol/stores/collective-tree.svelte';
import { holsterGet, holsterNextPut, holsterGetArray, ensureAuthenticated } from '$lib/server/holster/db';
import { SharedUtils } from '../collective/shared-utils';

/**
 * Production implementation using Holster
 */
export function createCollectiveTreeCallbacks(): CollectiveTreeCallbacks {
	return {
		/**
		 * Fetch all collective definitions that should be auto-merged
		 */
		async fetchAutoMergeCollectives(): Promise<CollectiveDefinition[]> {
			try {
				ensureAuthenticated();
				
				// Fetch all collectives with auto_merge enabled
				const collectives = await holsterGetArray<CollectiveDefinition>(
					'collective_definitions',
					(collective) => Boolean(collective.auto_merge && collective.contributor_ids?.length > 0)
				);
				
				return collectives;
			} catch (error) {
				console.error('[COLLECTIVE-TREE-CALLBACKS] Failed to fetch auto-merge collectives:', error);
				return [];
			}
		},
		
		/**
		 * Fetch contributor trees for a collective
		 * Uses shared utility to reduce duplication
		 */
		async fetchContributorTrees(contributorIds: string[]): Promise<Record<string, Node>> {
			return SharedUtils.fetchTreesAsRecord(contributorIds);
		},
		
		/**
		 * Save merged collective tree
		 */
		async saveCollectiveTree(
			collectiveId: string,
			tree: CollectiveTree,
			mergeStats: any
		): Promise<void> {
			try {
				ensureAuthenticated();
				
				const timestamp = new Date();
				
				// Store the collective tree
				await holsterNextPut('collective_trees', collectiveId, {
					...tree,
					last_updated: timestamp.toISOString()
				});
				
				// Update the collective definition with last merge time
				const definition = await holsterGet<CollectiveDefinition>(['collective_definitions', collectiveId]);
				if (definition) {
					await holsterNextPut('collective_definitions', collectiveId, {
						...definition,
						last_merge: timestamp.toISOString()
					});
				}
				
				// Store merge history
				const historyKey = `${collectiveId}_${timestamp.getTime()}`;
				await holsterNextPut('collective_tree_merge_history', historyKey, {
					collective_id: collectiveId,
					merge_stats: mergeStats,
					timestamp: timestamp.toISOString()
				});
				
				console.log(
					`[COLLECTIVE-TREE-CALLBACKS] ✓ Saved collective tree ${collectiveId}:\n` +
					`  → Contributors: ${tree.contributors.length}\n` +
					`  → Nodes: ${mergeStats.nodes_merged}`
				);
			} catch (error) {
				console.error('[COLLECTIVE-TREE-CALLBACKS] Failed to save collective tree:', error);
				throw error;
			}
		},
		
		/**
		 * Fetch existing collective trees for recognition computation
		 */
		async fetchCollectiveTrees(): Promise<CollectiveTree[]> {
			try {
				ensureAuthenticated();
				
				const trees = await holsterGetArray<CollectiveTree>('collective_trees');
				return trees;
			} catch (error) {
				console.error('[COLLECTIVE-TREE-CALLBACKS] Failed to fetch collective trees:', error);
				return [];
			}
		},
		
		/**
		 * Save collective recognition results
		 */
		async saveCollectiveRecognition(
			treeId: string,
			recognition: any
		): Promise<void> {
			try {
				ensureAuthenticated();
				
				const timestamp = new Date();
				
				// Store recognition results
				const recognitionKey = `${treeId}_${timestamp.getTime()}`;
				await holsterNextPut('collective_recognition_results', recognitionKey, {
					tree_id: treeId,
					recognition,
					timestamp: timestamp.toISOString()
				});
				
				// Update latest pointer
				await holsterNextPut('collective_recognition_latest', treeId, {
					result_key: recognitionKey,
					timestamp: timestamp.toISOString(),
					node_count: Object.keys(recognition).length
				});
				
				console.log(
					`[COLLECTIVE-TREE-CALLBACKS] ✓ Saved recognition for ${treeId}:\n` +
					`  → Nodes: ${Object.keys(recognition).length}`
				);
			} catch (error) {
				console.error('[COLLECTIVE-TREE-CALLBACKS] Failed to save collective recognition:', error);
				throw error;
			}
		},
		
		/**
		 * Fetch individual capacities for allocation
		 * Uses shared utility to reduce duplication
		 */
		async fetchIndividualCapacities(): Promise<Record<string, Record<string, number>>> {
			return SharedUtils.fetchAllIndividualCapacities();
		},
		
		/**
		 * Save capacity allocation results
		 */
		async saveCapacityAllocation(
			treeId: string,
			allocation: any
		): Promise<void> {
			try {
				ensureAuthenticated();
				
				const timestamp = new Date();
				
				// Store allocation results
				const allocationKey = `${treeId}_${timestamp.getTime()}`;
				await holsterNextPut('collective_capacity_allocations', allocationKey, {
					tree_id: treeId,
					allocation,
					timestamp: timestamp.toISOString()
				});
				
				// Update latest pointer
				await holsterNextPut('collective_capacity_allocation_latest', treeId, {
					result_key: allocationKey,
					timestamp: timestamp.toISOString(),
					efficiency: allocation.allocation_efficiency,
					fairness: allocation.allocation_fairness
				});
				
				console.log(
					`[COLLECTIVE-TREE-CALLBACKS] ✓ Saved allocation for ${treeId}:\n` +
					`  → Efficiency: ${(allocation.allocation_efficiency * 100).toFixed(1)}%\n` +
					`  → Fairness: ${(allocation.allocation_fairness * 100).toFixed(1)}%`
				);
			} catch (error) {
				console.error('[COLLECTIVE-TREE-CALLBACKS] Failed to save capacity allocation:', error);
				throw error;
			}
		},
		
		/**
		 * Log computation events
		 * Uses shared utility for consistent logging
		 */
		async logComputation(event: string, data: any): Promise<void> {
			return SharedUtils.logComputationEvent('collective_tree_computation_logs', event, data);
		}
	};
}

/**
 * Validate callbacks by testing data fetching
 */
export async function validateCollectiveTreeCallbacks(): Promise<{
	success: boolean;
	results: Record<string, any>;
	errors: string[];
}> {
	const callbacks = createCollectiveTreeCallbacks();
	const results: Record<string, any> = {};
	const errors: string[] = [];
	
	console.log('[COLLECTIVE-TREE-CALLBACKS-VALIDATION] 🔍 Starting validation...');
	
	try {
		// Test auto-merge collectives fetch
		try {
			const collectives = await callbacks.fetchAutoMergeCollectives();
			results.autoMergeCollectives = {
				count: collectives.length,
				ids: collectives.map(c => c.id)
			};
			console.log(`[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✓ Auto-merge collectives: ${collectives.length} records`);
		} catch (err) {
			errors.push(`Auto-merge collectives fetch failed: ${err}`);
			console.error('[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✗ Auto-merge collectives fetch failed:', err);
		}
		
		// Test collective trees fetch
		try {
			const trees = await callbacks.fetchCollectiveTrees();
			results.collectiveTrees = {
				count: trees.length,
				ids: trees.map(t => t.id)
			};
			console.log(`[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✓ Collective trees: ${trees.length} records`);
		} catch (err) {
			errors.push(`Collective trees fetch failed: ${err}`);
			console.error('[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✗ Collective trees fetch failed:', err);
		}
		
		// Test individual capacities fetch
		try {
			const capacities = await callbacks.fetchIndividualCapacities();
			results.individualCapacities = {
				count: Object.keys(capacities).length,
				contributors: Object.keys(capacities)
			};
			console.log(`[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✓ Individual capacities: ${Object.keys(capacities).length} records`);
		} catch (err) {
			errors.push(`Individual capacities fetch failed: ${err}`);
			console.error('[COLLECTIVE-TREE-CALLBACKS-VALIDATION]   ✗ Individual capacities fetch failed:', err);
		}
		
		const success = errors.length === 0;
		
		if (success) {
			console.log('[COLLECTIVE-TREE-CALLBACKS-VALIDATION] ✅ All validations passed');
		} else {
			console.log(`[COLLECTIVE-TREE-CALLBACKS-VALIDATION] ⚠️  Validation completed with ${errors.length} errors`);
		}
		
		return { success, results, errors };
		
	} catch (error) {
		console.error('[COLLECTIVE-TREE-CALLBACKS-VALIDATION] ❌ Validation failed:', error);
		return {
			success: false,
			results,
			errors: [...errors, `Validation error: ${error}`]
		};
	}
}

