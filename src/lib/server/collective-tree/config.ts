/**
 * Collective Tree Computation Scheduler Configuration
 * 
 * Settings for server-side collective tree operations:
 * - Tree merging (combine contributor trees into collectives)
 * - Collective recognition computation
 * - Capacity allocation across collective nodes
 */

// Dynamic environment variables (same pattern as collective config)
let env: Record<string, string | undefined> = {};
try {
	if (typeof process !== 'undefined' && process.env) {
		env = process.env;
	}
} catch {
	env = {};
}

/**
 * Load interval from environment with fallback (in milliseconds)
 */
function loadInterval(envVar: string, defaultMs: number): number {
	const value = env[envVar];
	if (!value) return defaultMs;
	
	const parsed = parseInt(value, 10);
	return isNaN(parsed) ? defaultMs : parsed;
}

export const collectiveTreeConfig = {
	// Tree Merge Interval
	// Default: 1 hour = 60 * 60 * 1000
	// How often to recompute collective trees from contributor trees
	treeMergeInterval: loadInterval(
		'COLLECTIVE_TREE_MERGE_INTERVAL_MS',
		60 * 60 * 1000
	),
	
	// Collective Recognition Interval
	// Default: 30 minutes = 30 * 60 * 1000
	// How often to recompute collective recognition values
	collectiveRecognitionInterval: loadInterval(
		'COLLECTIVE_RECOGNITION_INTERVAL_MS',
		30 * 60 * 1000
	),
	
	// Capacity Allocation Interval
	// Default: 1 hour = 60 * 60 * 1000
	// How often to recompute capacity allocation across collective nodes
	capacityAllocationInterval: loadInterval(
		'COLLECTIVE_CAPACITY_ALLOCATION_INTERVAL_MS',
		60 * 60 * 1000
	),
	
	// Enable/disable automatic tree operations
	autoMergeTrees: env.AUTO_MERGE_COLLECTIVE_TREES !== 'false',
	autoComputeRecognition: env.AUTO_COMPUTE_COLLECTIVE_RECOGNITION !== 'false',
	autoAllocateCapacity: env.AUTO_ALLOCATE_COLLECTIVE_CAPACITY !== 'false',
	
	// Startup delay (wait after server startup)
	// Default: 30 seconds
	startupDelay: loadInterval('COLLECTIVE_TREE_STARTUP_DELAY_MS', 30_000),
	
	// Enable detailed logging
	verboseLogging: env.COLLECTIVE_TREE_VERBOSE_LOGGING === 'true',
	
	// Tree merge configuration defaults
	defaultMergeStrategy: env.COLLECTIVE_TREE_MERGE_STRATEGY || 'weighted_average',
	defaultNameCollisionStrategy: env.COLLECTIVE_TREE_NAME_COLLISION || 'weighted_priority',
	
	// Minimum contributors for collective tree creation
	minimumContributors: parseInt(env.MINIMUM_COLLECTIVE_CONTRIBUTORS || '2', 10),
} as const;

/**
 * Helper to format interval for logging
 */
export function formatInterval(ms: number): string {
	const seconds = ms / 1000;
	const minutes = seconds / 60;
	const hours = minutes / 60;
	const days = hours / 24;
	
	if (days >= 1) return `${days.toFixed(1)} days`;
	if (hours >= 1) return `${hours.toFixed(1)} hours`;
	if (minutes >= 1) return `${minutes.toFixed(1)} minutes`;
	return `${seconds.toFixed(1)} seconds`;
}

