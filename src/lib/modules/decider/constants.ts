/**
 * @module constants
 * Centralized constants for the Decider system
 */

/**
 * Default time configurations
 */
export const TIME = {
	/** Default total time window: 24 hours */
	DEFAULT_WINDOW: 86400000,
	
	/** Quick test time window: 30 seconds */
	TEST_WINDOW: 30000,
	
	/** Timer update interval: 100ms */
	TIMER_INTERVAL: 100,
	
	/** Urgent threshold: 10% of total time remaining */
	URGENT_THRESHOLD: 0.1,
	
	/** Number of phases in equal distribution */
	PHASE_COUNT: 4,
} as const;

/**
 * UI update intervals
 */
export const INTERVALS = {
	/** Timer tick rate (ms) */
	TIMER: 100,
	
	/** Network sync check rate (ms) */
	SYNC_CHECK: 100,
	
	/** Connection timeout (ms) */
	CONNECTION_TIMEOUT: 10000,
} as const;

/**
 * Phase names
 */
export const PHASES = {
	NOT_STARTED: 'not_started',
	PROPOSING: 'proposing',
	CHALLENGING: 'challenging',
	COMMENTING: 'commenting',
	SUPPORTING: 'supporting',
	COMPLETE: 'complete',
} as const;

/**
 * Proposal types
 */
export const PROPOSAL_TYPES = {
	CONTENT: 'content',
	CONFIG: 'config',
	HYBRID: 'hybrid',
} as const;

/**
 * Proposal status values
 */
export const PROPOSAL_STATUS = {
	PASSED_NO_CHALLENGES: 'passed-no-challenges',
	PASSED_AS_IS: 'passed-as-is',
	IN_PROCESS: 'in-process',
	AWAITING_SUPPORT: 'awaiting-support',
	COMPLETE: 'complete',
} as const;

/**
 * UI variant types
 */
export const VARIANTS = {
	COMPACT: 'compact',
	INLINE: 'inline',
	FULL: 'full',
} as const;

/**
 * Default support points for distribution
 */
export const SUPPORT = {
	/** Total points to distribute */
	DEFAULT_POINTS: 10,
	
	/** Minimum support value */
	MIN: 0,
	
	/** Maximum support value (0-1 weights) */
	MAX: 1,
} as const;

/**
 * Responsive breakpoints (px)
 */
export const BREAKPOINTS = {
	MOBILE: 640,
	TABLET: 768,
	DESKTOP: 1024,
	WIDE: 1280,
} as const;

/**
 * Animation durations (ms)
 */
export const ANIMATIONS = {
	FAST: 150,
	NORMAL: 200,
	SLOW: 300,
	PULSE: 1000,
} as const;

/**
 * Z-index layers for consistent stacking
 */
export const Z_INDEX = {
	BASE: 1,
	DROPDOWN: 10,
	STICKY: 100,
	MODAL_BACKDROP: 1000,
	MODAL: 1001,
	TOAST: 2000,
} as const;

