/**
 * Collective Tree Computation Scheduler
 * 
 * Runs scheduled computations for collective tree operations:
 * - Tree merging (combine individual trees into collective trees)
 * - Collective recognition computation
 * - Capacity allocation across collective nodes
 * 
 * This complements the membership/allocation scheduler with tree-specific logic
 */

import { 
	mergeContributorTrees,
	calculateCollectiveRecognition,
	calculateCollectiveCapacityAllocation,
	type CollectiveTree
} from '$lib/protocol/stores/collective-tree.svelte';
import type { Node } from '$lib/protocol/schemas';
import { collectiveTreeConfig, formatInterval } from './config';

// ═══════════════════════════════════════════════════════════════════
// TYPES
// ═══════════════════════════════════════════════════════════════════

export interface CollectiveDefinition {
	id: string;
	name: string;
	contributor_ids: string[];
	recognition_shares?: Record<string, number>;
	auto_merge: boolean;
	merge_strategy?: string;
	last_merge?: string;
}

export interface CollectiveTreeCallbacks {
	/**
	 * Fetch all collective definitions that should be auto-merged
	 */
	fetchAutoMergeCollectives: () => Promise<CollectiveDefinition[]>;
	
	/**
	 * Fetch contributor trees for a collective
	 */
	fetchContributorTrees: (contributorIds: string[]) => Promise<Record<string, Node>>;
	
	/**
	 * Save merged collective tree
	 */
	saveCollectiveTree: (
		collectiveId: string,
		tree: CollectiveTree,
		mergeStats: any
	) => Promise<void>;
	
	/**
	 * Fetch existing collective trees for recognition computation
	 */
	fetchCollectiveTrees: () => Promise<CollectiveTree[]>;
	
	/**
	 * Save collective recognition results
	 */
	saveCollectiveRecognition: (
		treeId: string,
		recognition: any
	) => Promise<void>;
	
	/**
	 * Fetch individual capacities for allocation
	 */
	fetchIndividualCapacities: () => Promise<Record<string, Record<string, number>>>;
	
	/**
	 * Save capacity allocation results
	 */
	saveCapacityAllocation: (
		treeId: string,
		allocation: any
	) => Promise<void>;
	
	/**
	 * Optional: Log computation events
	 */
	logComputation?: (event: string, data: any) => Promise<void>;
}

// ═══════════════════════════════════════════════════════════════════
// SCHEDULER STATE
// ═══════════════════════════════════════════════════════════════════

interface SchedulerState {
	mergeTimer: NodeJS.Timeout | null;
	recognitionTimer: NodeJS.Timeout | null;
	allocationTimer: NodeJS.Timeout | null;
	isRunning: boolean;
	lastMergeRun: Date | null;
	lastRecognitionRun: Date | null;
	lastAllocationRun: Date | null;
	mergeRunCount: number;
	recognitionRunCount: number;
	allocationRunCount: number;
	callbacks: CollectiveTreeCallbacks | null;
}

const state: SchedulerState = {
	mergeTimer: null,
	recognitionTimer: null,
	allocationTimer: null,
	isRunning: false,
	lastMergeRun: null,
	lastRecognitionRun: null,
	lastAllocationRun: null,
	mergeRunCount: 0,
	recognitionRunCount: 0,
	allocationRunCount: 0,
	callbacks: null
};

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Run tree merge computation for all auto-merge collectives
 */
async function runTreeMergeComputation(): Promise<void> {
	if (!state.callbacks) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ No callbacks registered');
		return;
	}
	
	const startTime = Date.now();
	state.mergeRunCount++;
	
	try {
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER] 🔄 Starting tree merge #${state.mergeRunCount}...`);
		}
		
		// Fetch collectives that need merging
		const collectives = await state.callbacks.fetchAutoMergeCollectives();
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER]   → Found ${collectives.length} auto-merge collectives`);
		}
		
		if (collectives.length === 0) {
			console.log('[COLLECTIVE-TREE-SCHEDULER] ℹ️  No collectives with auto-merge enabled');
			state.lastMergeRun = new Date();
			return;
		}
		
		let mergedCount = 0;
		let totalNodes = 0;
		
		for (const collective of collectives) {
			// Skip if not enough contributors
			if (collective.contributor_ids.length < collectiveTreeConfig.minimumContributors) {
				if (collectiveTreeConfig.verboseLogging) {
					console.log(
						`[COLLECTIVE-TREE-SCHEDULER]   ⊘ Skipping ${collective.id}: ` +
						`${collective.contributor_ids.length} < ${collectiveTreeConfig.minimumContributors} contributors`
					);
				}
				continue;
			}
			
			try {
				// Fetch contributor trees
				const contributorTrees = await state.callbacks.fetchContributorTrees(
					collective.contributor_ids
				);
				
				// Merge trees
				const mergeResult = mergeContributorTrees({
					contributor_trees: contributorTrees,
					recognition_shares: collective.recognition_shares || {},
					merge_strategy: (collective.merge_strategy || collectiveTreeConfig.defaultMergeStrategy) as any,
					conflict_resolution: 'merge',
					name_collision_strategy: collectiveTreeConfig.defaultNameCollisionStrategy as any
				});
				
				// Save merged tree
				await state.callbacks.saveCollectiveTree(
					collective.id,
					mergeResult.collective_tree,
					mergeResult.merge_stats
				);
				
				mergedCount++;
				totalNodes += mergeResult.merge_stats.nodes_merged;
				
				if (collectiveTreeConfig.verboseLogging) {
					console.log(
						`[COLLECTIVE-TREE-SCHEDULER]   ✓ Merged ${collective.id}: ` +
						`${mergeResult.merge_stats.nodes_merged} nodes from ${collective.contributor_ids.length} contributors`
					);
				}
			} catch (err) {
				console.error(`[COLLECTIVE-TREE-SCHEDULER] ✗ Failed to merge ${collective.id}:`, err);
			}
		}
		
		state.lastMergeRun = new Date();
		const duration = Date.now() - startTime;
		
		console.log(
			`[COLLECTIVE-TREE-SCHEDULER] ✅ Tree merge completed in ${duration}ms\n` +
			`  → Processed ${collectives.length} collectives\n` +
			`  → Successfully merged ${mergedCount} trees\n` +
			`  → Total nodes merged: ${totalNodes}`
		);
		
		// Optional: Log to database
		if (state.callbacks.logComputation) {
			await state.callbacks.logComputation('tree_merge', {
				run_number: state.mergeRunCount,
				timestamp: state.lastMergeRun,
				duration_ms: duration,
				collectives_processed: collectives.length,
				trees_merged: mergedCount,
				total_nodes: totalNodes
			});
		}
		
	} catch (error) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ Tree merge computation failed:', error);
		throw error;
	}
}

/**
 * Run collective recognition computation for all collective trees
 */
async function runCollectiveRecognitionComputation(): Promise<void> {
	if (!state.callbacks) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ No callbacks registered');
		return;
	}
	
	const startTime = Date.now();
	state.recognitionRunCount++;
	
	try {
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER] 🔄 Starting collective recognition #${state.recognitionRunCount}...`);
		}
		
		// Fetch all collective trees
		const trees = await state.callbacks.fetchCollectiveTrees();
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER]   → Loaded ${trees.length} collective trees`);
		}
		
		if (trees.length === 0) {
			console.log('[COLLECTIVE-TREE-SCHEDULER] ℹ️  No collective trees to process');
			state.lastRecognitionRun = new Date();
			return;
		}
		
		let computedCount = 0;
		
		for (const tree of trees) {
			try {
				// Walk tree and compute recognition for each node
				const nodeRecognition: Record<string, any> = {};
				
				function computeNodeRecognition(nodeId: string): void {
					const recognition = calculateCollectiveRecognition(tree, nodeId);
					nodeRecognition[nodeId] = recognition;
				}
				
				// Compute for root and all descendants
				function walkTree(node: any): void {
					computeNodeRecognition(node.id);
					for (const child of node.children || []) {
						walkTree(child);
					}
				}
				
				walkTree(tree.root);
				
				// Save recognition results
				await state.callbacks.saveCollectiveRecognition(tree.id, nodeRecognition);
				
				computedCount++;
				
				if (collectiveTreeConfig.verboseLogging) {
					console.log(
						`[COLLECTIVE-TREE-SCHEDULER]   ✓ Computed recognition for ${tree.id}: ` +
						`${Object.keys(nodeRecognition).length} nodes`
					);
				}
			} catch (err) {
				console.error(`[COLLECTIVE-TREE-SCHEDULER] ✗ Failed recognition for ${tree.id}:`, err);
			}
		}
		
		state.lastRecognitionRun = new Date();
		const duration = Date.now() - startTime;
		
		console.log(
			`[COLLECTIVE-TREE-SCHEDULER] ✅ Recognition computation completed in ${duration}ms\n` +
			`  → Processed ${trees.length} trees\n` +
			`  → Computed ${computedCount} recognition sets`
		);
		
		// Optional: Log to database
		if (state.callbacks.logComputation) {
			await state.callbacks.logComputation('collective_recognition', {
				run_number: state.recognitionRunCount,
				timestamp: state.lastRecognitionRun,
				duration_ms: duration,
				trees_processed: trees.length,
				computations: computedCount
			});
		}
		
	} catch (error) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ Recognition computation failed:', error);
		throw error;
	}
}

/**
 * Run capacity allocation computation for all collective trees
 */
async function runCapacityAllocationComputation(): Promise<void> {
	if (!state.callbacks) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ No callbacks registered');
		return;
	}
	
	const startTime = Date.now();
	state.allocationRunCount++;
	
	try {
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER] 🔄 Starting capacity allocation #${state.allocationRunCount}...`);
		}
		
		// Fetch all collective trees
		const trees = await state.callbacks.fetchCollectiveTrees();
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER]   → Loaded ${trees.length} collective trees`);
		}
		
		if (trees.length === 0) {
			console.log('[COLLECTIVE-TREE-SCHEDULER] ℹ️  No collective trees to process');
			state.lastAllocationRun = new Date();
			return;
		}
		
		// Fetch individual capacities
		const individualCapacities = await state.callbacks.fetchIndividualCapacities();
		if (collectiveTreeConfig.verboseLogging) {
			console.log(`[COLLECTIVE-TREE-SCHEDULER]   → Loaded capacities for ${Object.keys(individualCapacities).length} individuals`);
		}
		
		let allocatedCount = 0;
		
		for (const tree of trees) {
			try {
				// Compute capacity allocation
				const allocation = calculateCollectiveCapacityAllocation(
					tree,
					individualCapacities
				);
				
				// Save allocation results
				await state.callbacks.saveCapacityAllocation(tree.id, allocation);
				
				allocatedCount++;
				
				if (collectiveTreeConfig.verboseLogging) {
					console.log(
						`[COLLECTIVE-TREE-SCHEDULER]   ✓ Allocated capacity for ${tree.id}: ` +
						`efficiency=${(allocation.allocation_efficiency * 100).toFixed(1)}%`
					);
				}
			} catch (err) {
				console.error(`[COLLECTIVE-TREE-SCHEDULER] ✗ Failed allocation for ${tree.id}:`, err);
			}
		}
		
		state.lastAllocationRun = new Date();
		const duration = Date.now() - startTime;
		
		console.log(
			`[COLLECTIVE-TREE-SCHEDULER] ✅ Capacity allocation completed in ${duration}ms\n` +
			`  → Processed ${trees.length} trees\n` +
			`  → Computed ${allocatedCount} allocations`
		);
		
		// Optional: Log to database
		if (state.callbacks.logComputation) {
			await state.callbacks.logComputation('capacity_allocation', {
				run_number: state.allocationRunCount,
				timestamp: state.lastAllocationRun,
				duration_ms: duration,
				trees_processed: trees.length,
				allocations: allocatedCount
			});
		}
		
	} catch (error) {
		console.error('[COLLECTIVE-TREE-SCHEDULER] ❌ Capacity allocation failed:', error);
		throw error;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SCHEDULER CONTROL
// ═══════════════════════════════════════════════════════════════════

/**
 * Start the collective tree scheduler
 */
export function startCollectiveTreeScheduler(callbacks: CollectiveTreeCallbacks): void {
	if (state.isRunning) {
		console.warn('[COLLECTIVE-TREE-SCHEDULER] ⚠️  Scheduler already running');
		return;
	}
	
	state.callbacks = callbacks;
	state.isRunning = true;
	
	console.log(
		`[COLLECTIVE-TREE-SCHEDULER] 🚀 Starting scheduler...\n` +
		`  → Tree merge: every ${formatInterval(collectiveTreeConfig.treeMergeInterval)}\n` +
		`  → Collective recognition: every ${formatInterval(collectiveTreeConfig.collectiveRecognitionInterval)}\n` +
		`  → Capacity allocation: every ${formatInterval(collectiveTreeConfig.capacityAllocationInterval)}\n` +
		`  → Startup delay: ${formatInterval(collectiveTreeConfig.startupDelay)}\n` +
		`  → Minimum contributors: ${collectiveTreeConfig.minimumContributors}\n` +
		`  → Auto-merge trees: ${collectiveTreeConfig.autoMergeTrees}\n` +
		`  → Auto-compute recognition: ${collectiveTreeConfig.autoComputeRecognition}\n` +
		`  → Auto-allocate capacity: ${collectiveTreeConfig.autoAllocateCapacity}`
	);
	
	// Schedule with startup delay
	if (collectiveTreeConfig.autoMergeTrees) {
		setTimeout(() => {
			runTreeMergeComputation().catch(console.error);
			state.mergeTimer = setInterval(() => {
				runTreeMergeComputation().catch(console.error);
			}, collectiveTreeConfig.treeMergeInterval);
		}, collectiveTreeConfig.startupDelay);
	}
	
	if (collectiveTreeConfig.autoComputeRecognition) {
		setTimeout(() => {
			runCollectiveRecognitionComputation().catch(console.error);
			state.recognitionTimer = setInterval(() => {
				runCollectiveRecognitionComputation().catch(console.error);
			}, collectiveTreeConfig.collectiveRecognitionInterval);
		}, collectiveTreeConfig.startupDelay);
	}
	
	if (collectiveTreeConfig.autoAllocateCapacity) {
		setTimeout(() => {
			runCapacityAllocationComputation().catch(console.error);
			state.allocationTimer = setInterval(() => {
				runCapacityAllocationComputation().catch(console.error);
			}, collectiveTreeConfig.capacityAllocationInterval);
		}, collectiveTreeConfig.startupDelay);
	}
}

/**
 * Stop the scheduler
 */
export function stopCollectiveTreeScheduler(): void {
	if (!state.isRunning) {
		console.warn('[COLLECTIVE-TREE-SCHEDULER] ⚠️  Scheduler not running');
		return;
	}
	
	console.log('[COLLECTIVE-TREE-SCHEDULER] 🛑 Stopping scheduler...');
	
	if (state.mergeTimer) {
		clearInterval(state.mergeTimer);
		state.mergeTimer = null;
	}
	
	if (state.recognitionTimer) {
		clearInterval(state.recognitionTimer);
		state.recognitionTimer = null;
	}
	
	if (state.allocationTimer) {
		clearInterval(state.allocationTimer);
		state.allocationTimer = null;
	}
	
	state.isRunning = false;
	state.callbacks = null;
	
	console.log('[COLLECTIVE-TREE-SCHEDULER] ✅ Scheduler stopped');
}

/**
 * Get scheduler status
 */
export function getCollectiveTreeSchedulerStatus() {
	return {
		isRunning: state.isRunning,
		lastMergeRun: state.lastMergeRun,
		lastRecognitionRun: state.lastRecognitionRun,
		lastAllocationRun: state.lastAllocationRun,
		mergeRunCount: state.mergeRunCount,
		recognitionRunCount: state.recognitionRunCount,
		allocationRunCount: state.allocationRunCount,
		config: collectiveTreeConfig
	};
}

/**
 * Manual triggers
 */
export async function triggerTreeMerge(): Promise<void> {
	console.log('[COLLECTIVE-TREE-SCHEDULER] 🔧 Manual tree merge triggered');
	await runTreeMergeComputation();
}

export async function triggerCollectiveRecognition(): Promise<void> {
	console.log('[COLLECTIVE-TREE-SCHEDULER] 🔧 Manual collective recognition triggered');
	await runCollectiveRecognitionComputation();
}

export async function triggerCapacityAllocation(): Promise<void> {
	console.log('[COLLECTIVE-TREE-SCHEDULER] 🔧 Manual capacity allocation triggered');
	await runCapacityAllocationComputation();
}

