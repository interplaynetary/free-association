/**
 * Free Association Protocol - Core
 * 
 * Pure, framework-agnostic protocol logic.
 * 
 * For subpath imports, use:
 * - @playnet/free-association/schemas
 * - @playnet/free-association/allocation
 * - @playnet/free-association/tree
 * - @playnet/free-association/itc
 * etc.
 */

// Core exports (main entry point)
export { computeAllocations } from './allocation';
export { calculateCollectiveRecognitionDistribution } from './distribution';
export { mutualFulfillment } from './tree';
export type { Commitment, NeedSlot, AvailabilitySlot, Node } from './schemas';

// ITC (Interval Tree Clocks) for causality tracking
export {
	seed as itcSeed,
	event as itcEvent,
	fork as itcFork,
	join as itcJoin,
	peek as itcPeek,
	leq as itcLeq,
	equals as itcEquals,
	type Stamp as ITCStamp,
	type Id as ITCId,
	type Event as ITCEvent
} from './itc';
