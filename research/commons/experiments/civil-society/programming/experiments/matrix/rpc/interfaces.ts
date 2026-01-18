/**
 * Elegant RPC Interfaces
 * 
 * Focused interfaces following Single Responsibility Principle.
 * Uses patterns from src/lib/protocol/*-rpc.ts for consistency.
 */

import type { RpcStub } from 'capnweb';
import type { NeedSlot, AvailabilitySlot, SlotAllocationRecord } from '../slots/schemas.js';
import type { ConvergenceMetrics } from '../allocation/convergence.js';
import type { MatrixResult } from '../core/matrix-operations.js';

// ═══════════════════════════════════════════════════════════════════
// MATRIX COMPUTATION RPC
// ═══════════════════════════════════════════════════════════════════

/**
 * Matrix Computer RPC Interface
 * 
 * Pure mathematical operations on sparse matrices.
 */
export interface IMatrixRpc {
  /** Set recognition value */
  setRecognition(i: number, j: number, value: number): Promise<void>;

  /** Get recognition value */
  getRecognition(i: number, j: number): Promise<number>;

  /** Compute RS (Recognition-Shares) */
  computeRS(): Promise<number[][]>;

  /** Compute MR (Mutual-Recognition) */
  computeMR(): Promise<number[][]>;

  /** Compute MRS (Mutual-Recognition-Shares) */
  computeMRS(): Promise<number[][]>;

  /** Compute Total MR vector */
  computeTotalMR(): Promise<number[]>;

  /** Validate budget constraint */
  validateBudgetConstraint(): Promise<boolean>;
}

// ═══════════════════════════════════════════════════════════════════
// RECOGNITION BUDGET RPC
// ═══════════════════════════════════════════════════════════════════

/**
 * Recognition Budget RPC Interface
 * 
 * Manages recognition allocation (Axiom 1: Budget Constraint).
 */
export interface IRecognitionBudgetRpc {
  /** Allocate recognition to another participant */
  allocateRecognition(targetId: string, amount: number): Promise<boolean>;

  /** Get recognition allocated to specific participant */
  getRecognitionTo(targetId: string): Promise<number>;

  /** Get all allocations */
  getAllAllocations(): Promise<Record<string, number>>;

  /** Get total allocated so far */
  getTotalAllocated(): Promise<number>;

  /** Get my participant ID */
  getParticipantId(): Promise<string>;

  /** Subscribe to budget changes */
  subscribeToChanges(
    callback: (allocations: Record<string, number>) => void
  ): Promise<() => void>; // Returns unsubscribe function
}

// ═══════════════════════════════════════════════════════════════════
// SLOT MANAGER RPC
// ═══════════════════════════════════════════════════════════════════

/**
 * Slot Manager RPC Interface
 * 
 * Manages need and availability slots.
 */
export interface ISlotManagerRpc {
  /** Add a need slot */
  addNeedSlot(slot: NeedSlot): Promise<void>;

  /** Add an availability slot */
  addAvailabilitySlot(slot: AvailabilitySlot): Promise<void>;

  /** Get all need slots */
  getNeedSlots(): Promise<NeedSlot[]>;

  /** Get all availability slots */
  getAvailabilitySlots(): Promise<AvailabilitySlot[]>;

  /** Remove a need slot */
  removeNeedSlot(slotId: string): Promise<void>;

  /** Remove an availability slot */
  removeAvailabilitySlot(slotId: string): Promise<void>;

  /** Get participant ID */
  getParticipantId(): Promise<string>;

  /** Subscribe to slot changes */
  subscribeToSlots(
    callback: (needs: NeedSlot[], availability: AvailabilitySlot[]) => void
  ): Promise<() => void>;
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION ENGINE RPC
// ═══════════════════════════════════════════════════════════════════

/**
 * Allocation Engine RPC Interface
 * 
 * Runs allocation algorithm and provides results.
 */
export interface IAllocationEngineRpc {
  /** 
   * Request allocation for a specific need slot
   * Uses enhanced algorithm with damping, divisibility, convergence tracking
   */
  requestAllocation(needSlotId: string): Promise<SlotAllocationRecord[]>;

  /** Get all allocations */
  getAllocations(): Promise<SlotAllocationRecord[]>;

  /** Get convergence metrics */
  getConvergenceMetrics(): Promise<ConvergenceMetrics | undefined>;

  /** Subscribe to allocation updates */
  subscribeToAllocations(
    callback: (allocations: SlotAllocationRecord[]) => void
  ): Promise<() => void>;
}

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTER RPC (Pass-by-Reference Pattern!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Mutual Recognition Computer RPC Interface
 * 
 * ELEGANT PATTERN: Pass RPC stubs directly for capability-based security!
 * 
 * Example:
 * ```typescript
 * const alice = connectTo("alice@example.com");
 * const bob = connectTo("bob@example.com");
 * 
 * // Alice computes MR WITH Bob by passing Bob's stub!
 * const mr = await alice.computeMutualWith(bob);
 * ```
 */
export interface IMutualRecognitionRpc {
  /** Get my recognition of a specific participant */
  getRecognitionOf(participantId: string): Promise<number>;

  /** 
   * Compute mutual recognition with another participant
   * 
   * Pass-by-reference: Accepts RPC stub directly!
   * This is true capability-based security.
   */
  computeMutualWith(
    other: RpcStub<IMutualRecognitionRpc>
  ): Promise<number>;

  /** Get my participant ID */
  getParticipantId(): Promise<string>;

  /** Subscribe to recognition changes */
  subscribeToRecognition(
    callback: (recognitions: Record<string, number>) => void
  ): Promise<() => void>;
}

// ═══════════════════════════════════════════════════════════════════
// COLLECTIVE RPC
// ═══════════════════════════════════════════════════════════════════

/**
 * Collective Event
 */
export type CollectiveEvent =
  | { type: 'member-joined'; participantId: string }
  | { type: 'member-left'; participantId: string }
  | { type: 'membership-updated'; members: string[] };

/**
 * Collective RPC Interface
 * 
 * Manages collective membership via MRD threshold.
 */
export interface ICollectiveRpc {
  /** 
   * Attempt to join collective
   * Returns self if successful (capability = membership proof)
   */
  attemptJoin(participantId: string): Promise<RpcStub<ICollectiveRpc>>;

  /** Compute MRD for a participant */
  computeMRDForParticipant(participantId: string): Promise<number>;

  /** Get all members */
  getMembers(): Promise<string[]>;

  /** Check if participant is a member */
  isMember(participantId: string): Promise<boolean>;

  /** Get collective ID */
  getCollectiveId(): Promise<string>;

  /** Subscribe to collective events (type-safe!) */
  subscribeToEvents(
    callback: (event: CollectiveEvent) => void
  ): Promise<() => void>;
}

// ═══════════════════════════════════════════════════════════════════
// NETWORK COORDINATOR RPC (Discovery Service)
// ═══════════════════════════════════════════════════════════════════

/**
 * Network Event
 */
export type NetworkEvent =
  | { type: 'participant-joined'; participantId: string }
  | { type: 'participant-left'; participantId: string }
  | { type: 'slot-updated'; participantId: string; slotType: 'need' | 'availability' };

/**
 * Network Coordinator RPC Interface
 * 
 * Discovery service for finding participants.
 */
export interface INetworkCoordinatorRpc {
  /** 
   * Register yourself in the network
   * Pass your slot manager stub for others to discover you
   */
  registerParticipant(
    participantId: string,
    slotManager: RpcStub<ISlotManagerRpc>
  ): Promise<void>;

  /** Unregister from network */
  unregisterParticipant(participantId: string): Promise<void>;

  /** 
   * Discover providers for a need type
   * Returns RPC stubs you can directly call!
   */
  discoverProviders(resourceTypeId: string): Promise<RpcStub<ISlotManagerRpc>[]>;

  /** Get participant stub by ID */
  getParticipant(participantId: string): Promise<RpcStub<ISlotManagerRpc> | null>;

  /** Get all registered participants */
  getAllParticipants(): Promise<string[]>;

  /** Subscribe to network events (type-safe!) */
  subscribeToNetwork(
    callback: (event: NetworkEvent) => void
  ): Promise<() => void>;

  /** Get network statistics */
  getStats(): Promise<{
    totalParticipants: number;
    byResourceType: Record<string, number>;
  }>;
}

// ═══════════════════════════════════════════════════════════════════
// MAIN PARTICIPANT SESSION (Composes all services)
// ═══════════════════════════════════════════════════════════════════

/**
 * Participant Session RPC Interface
 * 
 * Main entry point that composes all services.
 * Think of this as a "facade" that provides access to all capabilities.
 */
export interface IParticipantSessionRpc {
  /** Get matrix computer */
  getMatrixComputer(): Promise<RpcStub<IMatrixRpc>>;

  /** Get recognition budget */
  getRecognitionBudget(): Promise<RpcStub<IRecognitionBudgetRpc>>;

  /** Get slot manager */
  getSlotManager(): Promise<RpcStub<ISlotManagerRpc>>;

  /** Get allocation engine */
  getAllocationEngine(): Promise<RpcStub<IAllocationEngineRpc>>;

  /** Get mutual recognition computer */
  getMutualRecognitionComputer(): Promise<RpcStub<IMutualRecognitionRpc>>;

  /** Get participant ID */
  getParticipantId(): Promise<string>;
}

