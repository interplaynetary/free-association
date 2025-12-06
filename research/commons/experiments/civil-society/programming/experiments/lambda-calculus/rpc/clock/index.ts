/**
 * Clock Module for RPC System
 * 
 * Uses Interval Tree Clocks for efficient causality tracking
 */

export { ITClock, resolveITCConflict, batchResolveConflicts } from './itc-adapter';
export type { Stamp as ITCStamp } from '../../itc';

