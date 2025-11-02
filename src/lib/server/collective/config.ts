/**
 * Collective Recognition & Membership Computation Scheduler Configuration
 * 
 * Defines intervals and settings for server-side scheduled computations
 */

// Dynamic import for server-side environment variables
// This will be undefined during build time, which is fine
let env: Record<string, string | undefined> = {};
try {
	if (typeof process !== 'undefined' && process.env) {
		env = process.env;
	}
} catch {
	// During build, env might not be available
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

export const collectiveConfig = {
	// Membership Computation Interval
	// Default: 7 days (weekly) = 7 * 24 * 60 * 60 * 1000
	membershipComputationInterval: loadInterval(
		'MEMBERSHIP_COMPUTATION_INTERVAL_MS',
		7 * 24 * 60 * 60 * 1000
	),
	
	// Allocation Computation Interval
	// Default: 1 day (daily) = 24 * 60 * 60 * 1000
	allocationComputationInterval: loadInterval(
		'ALLOCATION_COMPUTATION_INTERVAL_MS',
		24 * 60 * 60 * 1000
	),
	
	// MRD Threshold for membership
	// Default: 0.5 (need at least 50% of average mutual recognition)
	mrdThreshold: parseFloat(env.MRD_THRESHOLD || '0.5'),
	
	// Minimum mutual recognition filter
	// Default: 0.0 (any positive mutual recognition counts)
	minimumMutualRecognition: parseFloat(env.MINIMUM_MUTUAL_RECOGNITION || '0.0'),
	
	// Enable/disable automatic membership updates
	// Default: true
	autoUpdateMembership: env.AUTO_UPDATE_MEMBERSHIP !== 'false',
	
	// Enable/disable automatic allocation computations
	// Default: true
	autoComputeAllocations: env.AUTO_COMPUTE_ALLOCATIONS !== 'false',
	
	// Computation start delay (wait after server startup)
	// Default: 30 seconds
	startupDelay: loadInterval('COMPUTATION_STARTUP_DELAY_MS', 30_000),
	
	// Enable detailed logging
	// Default: false (only errors and important events)
	verboseLogging: env.COLLECTIVE_VERBOSE_LOGGING === 'true',
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

