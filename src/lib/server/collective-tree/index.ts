/**
 * Server-side Collective Tree Scheduler Module
 * 
 * Exports:
 * - Scheduler functions for tree merging and recognition
 * - Configuration
 * - Manual trigger functions
 */

export {
	startCollectiveTreeScheduler,
	stopCollectiveTreeScheduler,
	getCollectiveTreeSchedulerStatus,
	triggerTreeMerge,
	triggerCollectiveRecognition,
	triggerCapacityAllocation,
	type CollectiveTreeCallbacks,
	type CollectiveDefinition
} from './scheduler';

export { collectiveTreeConfig, formatInterval } from './config';
export { createCollectiveTreeCallbacks, validateCollectiveTreeCallbacks } from './callbacks';

