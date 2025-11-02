/**
 * Collective Recognition Scheduler Callbacks Implementation
 * 
 * Real implementation using Holster database utilities
 */

import type { ComputationCallbacks } from './scheduler';
import type { RecognitionData, BaseCapacity, BaseNeed } from '$lib/protocol/collective/schemas';
import type { Node } from '$lib/protocol/schemas';
import { holsterGet, holsterNextPut, holsterGetArray, ensureAuthenticated } from '$lib/server/holster/db';
import { user } from '$lib/server/holster/core';
import { SharedUtils } from './shared-utils';

/**
 * Production implementation using Holster
 */
export function createCallbacks(): ComputationCallbacks {
	return {
		/**
		 * Fetch all recognition data from user recognition trees
		 * Uses shared utility to reduce duplication
		 */
		async fetchRecognitionData(): Promise<RecognitionData[]> {
			return SharedUtils.fetchAllRecognitionData();
		},
		
		/**
		 * Fetch capacities with auto-update enabled
		 */
		async fetchAutoUpdateCapacities(): Promise<BaseCapacity[]> {
			try {
				ensureAuthenticated();
				
				// Fetch all capacities and filter for auto-update
				const capacities = await holsterGetArray<BaseCapacity>(
					'capacities',
					(capacity) => {
						return Boolean(
							capacity.auto_update_members_by_mrd &&
							capacity.members &&
							capacity.capacity_slots &&
							Array.isArray(capacity.capacity_slots)
						);
					}
				);
				
				return capacities;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch auto-update capacities:', error);
				return [];
			}
		},
		
		/**
		 * Save updated capacity members
		 */
		async saveCapacityMembers(
			capacityId: string,
			members: string[],
			added: string[],
			removed: string[],
			timestamp: Date
		): Promise<void> {
			try {
				ensureAuthenticated();
				
				// Fetch current capacity to preserve other fields
				const currentCapacity = await holsterGet<BaseCapacity>(['capacities', capacityId]);
				
				if (!currentCapacity) {
					console.warn(`[CALLBACKS] Capacity ${capacityId} not found, cannot update`);
					return;
				}
				
				// Update capacity with new members and timestamp
				const updatedCapacity = {
					...currentCapacity,
					members,
					last_membership_update: timestamp.toISOString(),
					updated_at: timestamp.toISOString()
				};
				
				await holsterNextPut('capacities', capacityId, updatedCapacity);
				
				// Store membership change history
				const historyKey = `${capacityId}_${timestamp.getTime()}`;
				await holsterNextPut('capacity_membership_history', historyKey, {
					capacity_id: capacityId,
					members,
					added,
					removed,
					timestamp: timestamp.toISOString()
				});
				
				console.log(
					`[CALLBACKS] ✓ Updated capacity ${capacityId}:\n` +
					`  → Members: ${members.length}\n` +
					`  → Added: ${added.join(', ') || 'none'}\n` +
					`  → Removed: ${removed.join(', ') || 'none'}`
				);
			} catch (error) {
				console.error('[CALLBACKS] Failed to save capacity members:', error);
				throw error;
			}
		},
		
		/**
		 * Fetch capacities for allocation computation
		 */
		async fetchCapacitiesForAllocation(): Promise<BaseCapacity[]> {
			try {
				ensureAuthenticated();
				
				// Fetch all capacities with capacity slots
				const capacities = await holsterGetArray<BaseCapacity>(
					'capacities',
					(capacity) => {
						return Boolean(
							capacity.capacity_slots &&
							Array.isArray(capacity.capacity_slots) &&
							capacity.capacity_slots.length > 0 &&
							capacity.members &&
							Array.isArray(capacity.members) &&
							capacity.members.length > 0
						);
					}
				);
				
				return capacities;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch capacities for allocation:', error);
				return [];
			}
		},
		
		/**
		 * Fetch all needs
		 */
		async fetchNeeds(): Promise<Map<string, BaseNeed>> {
			try {
				ensureAuthenticated();
				
				// Fetch all needs with need slots
				const needs = await holsterGetArray<BaseNeed>(
					'needs',
					(need) => {
						return Boolean(
							need.need_slots &&
							Array.isArray(need.need_slots) &&
							need.need_slots.length > 0 &&
							need.status &&
							['open', 'partially-fulfilled'].includes(need.status)
						);
					}
				);
				
				// Convert to Map keyed by declarer_id
				const needsMap = new Map<string, BaseNeed>();
				for (const need of needs) {
					if (need.declarer_id) {
						needsMap.set(need.declarer_id, need);
					}
				}
				
				return needsMap;
			} catch (error) {
				console.error('[CALLBACKS] Failed to fetch needs:', error);
				return new Map();
			}
		},
		
		/**
		 * Fetch member recognition trees
		 * Uses shared utility to reduce duplication
		 */
		async fetchMemberTrees(memberIds: string[]): Promise<Map<string, Node>> {
			return SharedUtils.fetchTrees(memberIds);
		},
		
		/**
		 * Save computed allocations
		 */
		async saveAllocations(capacityId: string, allocations: any): Promise<void> {
			try {
				ensureAuthenticated();
				
				const timestamp = new Date();
				
				// Store the full computation result
				const resultKey = `${capacityId}_${timestamp.getTime()}`;
				await holsterNextPut('allocation_computations', resultKey, {
					capacity_id: capacityId,
					...allocations,
					timestamp: timestamp.toISOString()
				});
				
				// Update the "latest" pointer for quick access
				await holsterNextPut('allocation_computations_latest', capacityId, {
					timestamp: timestamp.toISOString(),
					result_key: resultKey,
					total_allocated: allocations.total_allocated,
					total_capacity: allocations.total_capacity,
					member_count: allocations.member_set?.length || 0
				});
				
				// Store individual allocations for easy querying
				if (allocations.final_allocations) {
					for (const [memberId, amount] of Object.entries(allocations.final_allocations)) {
						if ((amount as number) > 0) {
							const allocationKey = `${capacityId}_${memberId}_${timestamp.getTime()}`;
							await holsterNextPut('allocations', allocationKey, {
								capacity_id: capacityId,
								member_id: memberId,
								amount: amount,
								timestamp: timestamp.toISOString(),
								computation_result_key: resultKey
							});
						}
					}
				}
				
				console.log(
					`[CALLBACKS] ✓ Saved allocations for ${capacityId}:\n` +
					`  → Total: ${allocations.total_allocated}/${allocations.total_capacity}\n` +
					`  → Members: ${allocations.member_set?.length || 0}\n` +
					`  → Utilization: ${((allocations.total_allocated / allocations.total_capacity) * 100).toFixed(1)}%`
				);
			} catch (error) {
				console.error('[CALLBACKS] Failed to save allocations:', error);
				throw error;
			}
		},
		
		/**
		 * Log computation events for monitoring
		 * Uses shared utility for consistent logging
		 */
		async logComputation(event: string, data: any): Promise<void> {
			return SharedUtils.logComputationEvent('computation_logs', event, data);
		}
	};
}

/**
 * Validate callbacks by testing data fetching
 * Useful for debugging setup issues
 */
export async function validateCallbacks(): Promise<{
	success: boolean;
	results: Record<string, any>;
	errors: string[];
}> {
	const callbacks = createCallbacks();
	const results: Record<string, any> = {};
	const errors: string[] = [];
	
	console.log('[CALLBACKS-VALIDATION] 🔍 Starting validation...');
	
	try {
		// Test recognition data fetch
		try {
			const recognitionData = await callbacks.fetchRecognitionData();
			results.recognitionData = {
				count: recognitionData.length,
				sample: recognitionData.slice(0, 3)
			};
			console.log(`[CALLBACKS-VALIDATION]   ✓ Recognition data: ${recognitionData.length} records`);
		} catch (err) {
			errors.push(`Recognition data fetch failed: ${err}`);
			console.error('[CALLBACKS-VALIDATION]   ✗ Recognition data fetch failed:', err);
		}
		
		// Test auto-update capacities fetch
		try {
			const capacities = await callbacks.fetchAutoUpdateCapacities();
			results.autoUpdateCapacities = {
				count: capacities.length,
				ids: capacities.map(c => c.id)
			};
			console.log(`[CALLBACKS-VALIDATION]   ✓ Auto-update capacities: ${capacities.length} records`);
		} catch (err) {
			errors.push(`Auto-update capacities fetch failed: ${err}`);
			console.error('[CALLBACKS-VALIDATION]   ✗ Auto-update capacities fetch failed:', err);
		}
		
		// Test capacities for allocation fetch
		try {
			const capacities = await callbacks.fetchCapacitiesForAllocation();
			results.allocationCapacities = {
				count: capacities.length,
				ids: capacities.map(c => c.id)
			};
			console.log(`[CALLBACKS-VALIDATION]   ✓ Allocation capacities: ${capacities.length} records`);
		} catch (err) {
			errors.push(`Allocation capacities fetch failed: ${err}`);
			console.error('[CALLBACKS-VALIDATION]   ✗ Allocation capacities fetch failed:', err);
		}
		
		// Test needs fetch
		try {
			const needs = await callbacks.fetchNeeds();
			results.needs = {
				count: needs.size,
				declarers: Array.from(needs.keys())
			};
			console.log(`[CALLBACKS-VALIDATION]   ✓ Needs: ${needs.size} records`);
		} catch (err) {
			errors.push(`Needs fetch failed: ${err}`);
			console.error('[CALLBACKS-VALIDATION]   ✗ Needs fetch failed:', err);
		}
		
		const success = errors.length === 0;
		
		if (success) {
			console.log('[CALLBACKS-VALIDATION] ✅ All validations passed');
		} else {
			console.log(`[CALLBACKS-VALIDATION] ⚠️  Validation completed with ${errors.length} errors`);
		}
		
		return { success, results, errors };
		
	} catch (error) {
		console.error('[CALLBACKS-VALIDATION] ❌ Validation failed:', error);
		return {
			success: false,
			results,
			errors: [...errors, `Validation error: ${error}`]
		};
	}
}

