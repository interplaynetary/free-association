/**
 * Server-side Collective Recognition & Membership Module
 * 
 * Exports:
 * - Scheduler functions for automated computations
 * - Configuration
 * - Manual trigger functions
 */

export {
	startScheduler,
	stopScheduler,
	getSchedulerStatus,
	triggerMembershipComputation,
	triggerAllocationComputation,
	type ComputationCallbacks
} from './scheduler';

export { collectiveConfig, formatInterval } from './config';

