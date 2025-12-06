/**
 * Free Association Protocol v5 - Pure Algorithm Core with RPC Support
 * 
 * Extends the pure allocation algorithm with symmetric RPC capabilities.
 * 
 * Architecture:
 * - Pure functions (original) - work locally if all data available
 * - RPC targets - expose allocation engine via RPC for distributed computation
 * - RPC clients - compute allocations using remote commitments
 * - Promise pipelining - chain multiple allocation steps efficiently
 * 
 * Can be used by:
 * - Local computation (all data in memory) - original functions
 * - Distributed computation (data across network) - RPC functions
 * - Hybrid (mix of local and remote data) - both!
 */

import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights,
	SlotAllocationRecord,
	ITCStamp,
	SystemState,
	ConvergenceMetrics,
	AllocationResult,
	MultiDimensionalDamping
} from '$lib/protocol/schemas';

import { RpcTarget, type RpcStub } from 'capnweb';
import { z } from 'zod';
import { RevocableRpcTarget } from './rpc-elegant';

// Re-export all original functions
export * from '../../../../../../../src/lib/protocol/allocation';
export type {
	SpaceTimeIndex,
	SystemStateSnapshot,
	ConvergenceSummary,
	AllocationResult,
	DampingState
} from '../../../../../../../src/lib/protocol/allocation';

// Import distribution RPC
import type {
	DistributionResult,
	IMutualRecognitionRpc,
	IDistributionComputerRpc
} from './distribution-rpc';

import {
	slotsCompatible,
	getTimeBucketKey,
	getLocationBucketKey
} from '$lib/protocol/utils/match';

import type { ComplianceFilter } from '$lib/protocol/utils/filters';

// ═══════════════════════════════════════════════════════════════════
// RPC INTERFACES (TypeScript)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment Data RPC Interface
 * 
 * Provides read access to commitment data for allocation computation.
 */
export interface ICommitmentDataRpc {
	/** Get need slots */
	getNeedSlots(): Promise<NeedSlot[]>;
	
	/** Get capacity slots */
	getCapacitySlots(): Promise<AvailabilitySlot[]>;
	
	/** Get recognition weights */
	getRecognitionWeights(): Promise<GlobalRecognitionWeights>;
	
	/** Get damping state */
	getDampingState(): Promise<MultiDimensionalDamping | undefined>;
	
	/** Get active (damped) needs for a type */
	getActiveNeed(typeId: string): Promise<number>;
	
	/** Get public key */
	getPubKey(): Promise<string>;
	
	/** Check if slot is compatible with another slot (via RPC!) */
	isSlotCompatible(
		mySlotId: string,
		theirSlot: NeedSlot | AvailabilitySlot
	): Promise<boolean>;
}

/**
 * Allocation Engine RPC Interface
 * 
 * Computes allocations using distributed data via RPC.
 * Symmetric: Anyone can be a provider, anyone can be a recipient.
 */
export interface IAllocationEngineRpc {
	/**
	 * Compute allocations from my capacity to remote recipients
	 * 
	 * @param recipientStubs - RPC stubs to recipients' commitments
	 * @param distribution - Pre-computed distribution (who gets what share)
	 * @returns Allocation result
	 */
	computeAllocationsRpc(
		recipientStubs: RpcStub<ICommitmentDataRpc>[],
		distribution: DistributionResult
	): Promise<AllocationResult>;
	
	/**
	 * Get my allocations for a specific recipient
	 */
	getAllocationsFor(recipientPubKey: string): Promise<SlotAllocationRecord[]>;
	
	/**
	 * Subscribe to allocation updates (bidirectional!)
	 */
	subscribeToAllocations(
		callback: (allocations: SlotAllocationRecord[]) => void
	): Promise<void>;
	
	/**
	 * Get my capacity slots (for discovery)
	 */
	getMyCapacitySlots(): Promise<AvailabilitySlot[]>;
	
	/**
	 * Get my public key
	 */
	getMyPubKey(): Promise<string>;
}

/**
 * Allocation Coordinator RPC Interface
 * 
 * Orchestrates multi-party allocation computation.
 * Demonstrates promise pipelining for complex workflows.
 */
export interface IAllocationCoordinatorRpc {
	/**
	 * Full allocation workflow via RPC (demonstrates pipelining!)
	 * 
	 * 1. Collect all participants' commitments
	 * 2. Compute distribution
	 * 3. Compute allocations
	 * 4. Return results
	 * 
	 * All steps pipelined for minimal round trips!
	 */
	coordinateAllocation(
		participantStubs: RpcStub<ICommitmentDataRpc>[],
		distributionComputer: RpcStub<IDistributionComputerRpc>
	): Promise<{
		distribution: DistributionResult;
		allocations: AllocationResult;
		participants: string[];
	}>;
	
	/**
	 * Subscribe to coordination events
	 */
	subscribeToCoordination(
		callback: (event: {
			type: 'distribution' | 'allocation' | 'convergence';
			data: any;
		}) => void
	): Promise<void>;
}

// ═══════════════════════════════════════════════════════════════════
// RPC TARGETS (Server-side implementations)
// ═══════════════════════════════════════════════════════════════════

/**
 * Commitment Data RPC Target
 * 
 * Exposes commitment data for remote allocation computation.
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class CommitmentDataRpcTarget extends RevocableRpcTarget implements ICommitmentDataRpc {
	private commitment: Commitment;
	private pubKey: string;
	
	constructor(
		pubKey: string,
		commitment: Commitment,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.pubKey = pubKey;
		this.commitment = commitment;
	}
	
	async getNeedSlots(): Promise<NeedSlot[]> {
		this.checkAccess('getNeedSlots');
		return this.commitment.need_slots || [];
	}
	
	async getCapacitySlots(): Promise<AvailabilitySlot[]> {
		this.checkAccess('getCapacitySlots');
		return this.commitment.capacity_slots || [];
	}
	
	async getRecognitionWeights(): Promise<GlobalRecognitionWeights> {
		this.checkAccess('getRecognitionWeights');
		return this.commitment.global_recognition_weights || {};
	}
	
	async getDampingState(): Promise<MultiDimensionalDamping | undefined> {
		this.checkAccess('getDampingState');
		return this.commitment.multi_dimensional_damping;
	}
	
	async getActiveNeed(typeId: string): Promise<number> {
		this.checkAccess('getActiveNeed');
		const needSlots = this.commitment.need_slots || [];
		const damping = this.commitment.multi_dimensional_damping;
		
		// Sum declared needs for this type
		let declaredNeed = 0;
		for (const slot of needSlots) {
			if (slot.need_type_id === typeId) {
				declaredNeed += slot.quantity;
			}
		}
		
		// Apply damping factor
		const globalDamping = damping?.global_damping_factor || 1.0;
		const typeDamping = damping?.damping_factors?.[typeId] || globalDamping;
		
		return declaredNeed * typeDamping;
	}
	
	async getPubKey(): Promise<string> {
		this.checkAccess('getPubKey');
		return this.pubKey;
	}
	
	async isSlotCompatible(
		mySlotId: string,
		theirSlot: NeedSlot | AvailabilitySlot
	): Promise<boolean> {
		this.checkAccess('isSlotCompatible');
		// Find my slot
		const myCapacitySlots = this.commitment.capacity_slots || [];
		const mySlot = myCapacitySlots.find(s => s.id === mySlotId);
		
		if (!mySlot) {
			return false;
		}
		
		// Check compatibility
		return slotsCompatible(mySlot, theirSlot);
	}
	
	/**
	 * Update commitment (call when local data changes)
	 */
	updateCommitment(newCommitment: Commitment) {
		this.commitment = newCommitment;
		console.log(`[COMMIT-DATA-RPC] Updated commitment for ${this.pubKey.slice(0, 8)}`);
	}
}

/**
 * Allocation Engine RPC Target
 * 
 * Computes allocations using RPC to fetch distributed data.
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class AllocationEngineRpcTarget extends RevocableRpcTarget implements IAllocationEngineRpc {
	private myPubKey: string;
	private myCapacitySlots: AvailabilitySlot[];
	private myRecognition: GlobalRecognitionWeights;
	private allocationCache: Map<string, SlotAllocationRecord[]> = new Map();
	private updateCallbacks: Set<(allocations: SlotAllocationRecord[]) => void> = new Set();
	
	constructor(
		myPubKey: string,
		myCapacitySlots: AvailabilitySlot[],
		myRecognition: GlobalRecognitionWeights,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.myPubKey = myPubKey;
		this.myCapacitySlots = myCapacitySlots;
		this.myRecognition = myRecognition;
	}
	
	/**
	 * Compute allocations using RPC to fetch recipient data
	 * 
	 * This demonstrates the power of symmetric RPC:
	 * - I can call their methods to get their data
	 * - They can call my methods to see their allocations
	 * - All with type safety and promise pipelining!
	 */
	async computeAllocationsRpc(
		recipientStubs: RpcStub<ICommitmentDataRpc>[],
		distribution: DistributionResult
	): Promise<AllocationResult> {
		this.checkAccess('computeAllocationsRpc');
		console.log(`[ALLOC-RPC] Computing allocations for ${recipientStubs.length} recipients`);
		
		const allocations: SlotAllocationRecord[] = [];
		const slotDenominators: Record<string, { mutual: number; nonMutual: number; need_type_id: string }> = {};
		const totalsByTypeAndRecipient: Record<string, Record<string, number>> = {};
		
		// Get all recipient pub keys (pipelined!)
		const recipientPubKeys = await Promise.all(
			recipientStubs.map(stub => stub.getPubKey())
		);
		
		console.log(`[ALLOC-RPC] Recipients:`, recipientPubKeys.map(pk => pk.slice(0, 8)));
		
		// Process each capacity slot
		for (const capacitySlot of this.myCapacitySlots) {
			const typeId = capacitySlot.need_type_id;
			const providersAvailableCapacity = capacitySlot.quantity;
			
			if (!totalsByTypeAndRecipient[typeId]) {
				totalsByTypeAndRecipient[typeId] = {};
			}
			
			// Find compatible recipients via RPC
			const compatibilityChecks = await Promise.all(
				recipientStubs.map(async (stub, index) => {
					const needSlots = await stub.getNeedSlots();
					const compatibleSlots = needSlots.filter(
						needSlot => needSlot.need_type_id === typeId
					);
					
					// Check detailed compatibility via RPC
					const detailedChecks = await Promise.all(
						compatibleSlots.map(needSlot =>
							stub.isSlotCompatible(capacitySlot.id, needSlot)
								.then(compatible => ({ needSlot, compatible }))
						)
					);
					
					const filtered = detailedChecks
						.filter(({ compatible }) => compatible)
						.map(({ needSlot }) => needSlot);
					
					return {
						pubKey: recipientPubKeys[index],
						compatibleSlots: filtered
					};
				})
			);
			
			const compatibleRecipients = new Map(
				compatibilityChecks
					.filter(({ compatibleSlots }) => compatibleSlots.length > 0)
					.map(({ pubKey, compatibleSlots }) => [pubKey, compatibleSlots])
			);
			
			console.log(`[ALLOC-RPC] Slot ${capacitySlot.id.slice(0, 8)}: ${compatibleRecipients.size} compatible`);
			
			if (compatibleRecipients.size === 0) continue;
			
			// Allocate based on distribution
			const CAPACITY_EPSILON = 0.0001;
			let capacityUsed = 0;
			
			// Build eligible recipients with distribution shares
			const eligibleRecipients: Array<{
				pubKey: string;
				totalNeed: number;
				remainingNeed: number;
				distributionShare: number;
				needSlots: NeedSlot[];
				tier: 'mutual' | 'non-mutual';
			}> = [];
			
			// Determine tiers from distribution
			const tier1Recipients = distribution.tiers?.tier1 || {};
			const tier2Recipients = distribution.tiers?.tier2 || {};
			
			// Fetch active needs via RPC (with damping)
			for (const [recipientPub, needSlots] of compatibleRecipients.entries()) {
				const stub = recipientStubs[recipientPubKeys.indexOf(recipientPub)];
				const activeNeed = await stub.getActiveNeed(typeId);
				
				// Determine tier and share
				let tier: 'mutual' | 'non-mutual' = 'mutual';
				let share = 0;
				
				if (tier1Recipients[recipientPub] !== undefined) {
					tier = 'mutual';
					share = tier1Recipients[recipientPub];
				} else if (tier2Recipients[recipientPub] !== undefined) {
					tier = 'non-mutual';
					share = tier2Recipients[recipientPub];
				} else {
					share = distribution.shares[recipientPub] || 0;
				}
				
				if (share > 0) {
					eligibleRecipients.push({
						pubKey: recipientPub,
						totalNeed: activeNeed,
						remainingNeed: activeNeed,
						distributionShare: share,
						needSlots,
						tier
					});
				}
			}
			
			// Sort by tier (mutual first)
			eligibleRecipients.sort((a, b) => {
				if (a.tier === 'mutual' && b.tier === 'non-mutual') return -1;
				if (a.tier === 'non-mutual' && b.tier === 'mutual') return 1;
				return 0;
			});
			
			// Multi-pass proportional allocation (same as original algorithm)
			let unsatisfiedRecipients = [...eligibleRecipients];
			let remainingCapacity = providersAvailableCapacity;
			let passCount = 0;
			const maxPasses = 10;
			
			while (remainingCapacity > CAPACITY_EPSILON && unsatisfiedRecipients.length > 0 && passCount < maxPasses) {
				passCount++;
				
				const currentTier = unsatisfiedRecipients[0]?.tier || 'mutual';
				const currentTierRecipients = unsatisfiedRecipients.filter(r => r.tier === currentTier);
				
				let denominator = currentTierRecipients.reduce(
					(sum, r) => sum + r.distributionShare,
					0
				);
				
				if (denominator < CAPACITY_EPSILON) break;
				
				// Calculate proportional allocations
				const proportionalAllocations = currentTierRecipients.map(recipient => {
					const rawAllocation = remainingCapacity * recipient.distributionShare / denominator;
					const cappedAllocation = Math.min(rawAllocation, recipient.remainingNeed);
					return { recipient, rawAllocation, cappedAllocation };
				});
				
				// Apply allocations
				let capacityUsedThisPass = 0;
				const nowSatisfied: typeof unsatisfiedRecipients = [];
				
				for (const { recipient, cappedAllocation } of proportionalAllocations) {
					if (cappedAllocation <= CAPACITY_EPSILON) continue;
					
					// Proportional distribution across need slots
					const totalCompatibleNeed = recipient.needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
					
					for (const needSlot of recipient.needSlots) {
						const proportion = needSlot.quantity / totalCompatibleNeed;
						const slotAllocation = Math.min(
							needSlot.quantity,
							cappedAllocation * proportion
						);
						
						if (slotAllocation > 0) {
							allocations.push({
								quantity: slotAllocation,
								need_type_id: typeId,
								availability_slot_id: capacitySlot.id,
								recipient_pubkey: recipient.pubKey,
								time_compatible: true,
								location_compatible: true,
								tier: recipient.tier,
								recipient_need_slot_id: needSlot.id
							});
							
							capacityUsedThisPass += slotAllocation;
						}
					}
					
					recipient.remainingNeed -= cappedAllocation;
					
					if (!totalsByTypeAndRecipient[typeId][recipient.pubKey]) {
						totalsByTypeAndRecipient[typeId][recipient.pubKey] = 0;
					}
					totalsByTypeAndRecipient[typeId][recipient.pubKey] += cappedAllocation;
					
					if (recipient.remainingNeed <= CAPACITY_EPSILON) {
						nowSatisfied.push(recipient);
					}
				}
				
				capacityUsed += capacityUsedThisPass;
				remainingCapacity -= capacityUsedThisPass;
				unsatisfiedRecipients = unsatisfiedRecipients.filter(r => !nowSatisfied.includes(r));
				
				if (nowSatisfied.length === 0 && capacityUsedThisPass < CAPACITY_EPSILON) {
					const remainingTiers = unsatisfiedRecipients.filter(r => r.tier !== currentTier);
					if (remainingTiers.length === 0) break;
				}
			}
			
			slotDenominators[capacitySlot.id] = {
				mutual: Object.keys(tier1Recipients).length,
				nonMutual: Object.keys(tier2Recipients).length,
				need_type_id: typeId
			};
		}
		
		// Cache allocations
		for (const allocation of allocations) {
			if (!this.allocationCache.has(allocation.recipient_pubkey)) {
				this.allocationCache.set(allocation.recipient_pubkey, []);
			}
			this.allocationCache.get(allocation.recipient_pubkey)!.push(allocation);
		}
		
		// Notify subscribers
		for (const callback of this.updateCallbacks) {
			callback(allocations);
		}
		
		console.log(`[ALLOC-RPC] Complete: ${allocations.length} allocations`);
		
		// Return simplified result (convergence metrics would require more data)
		return {
			allocations,
			slotDenominators,
			totalsByTypeAndRecipient,
			convergence: {
				totalNeedMagnitude: 0,
				previousNeedMagnitude: 0,
				contractionRate: 0,
				isConverged: false,
				percentNeedsMet: 0,
				universalSatisfaction: false,
				iterationsToConvergence: null,
				maxPersonNeed: 0,
				needVariance: 0,
				peopleStuck: 0,
				executionTimeMs: 0,
				currentIteration: 0,
				responseLatency: 0
			}
		};
	}
	
	async getAllocationsFor(recipientPubKey: string): Promise<SlotAllocationRecord[]> {
		this.checkAccess('getAllocationsFor');
		return this.allocationCache.get(recipientPubKey) || [];
	}
	
	async subscribeToAllocations(
		callback: (allocations: SlotAllocationRecord[]) => void
	): Promise<void> {
		this.checkAccess('subscribeToAllocations');
		this.updateCallbacks.add(callback);
	}
	
	async getMyCapacitySlots(): Promise<AvailabilitySlot[]> {
		this.checkAccess('getMyCapacitySlots');
		return [...this.myCapacitySlots];
	}
	
	async getMyPubKey(): Promise<string> {
		this.checkAccess('getMyPubKey');
		return this.myPubKey;
	}
	
	/**
	 * Update my capacity (call when slots change)
	 */
	updateCapacity(newCapacitySlots: AvailabilitySlot[]) {
		this.myCapacitySlots = newCapacitySlots;
		this.allocationCache.clear(); // Invalidate cache
		console.log(`[ALLOC-RPC] Updated capacity for ${this.myPubKey.slice(0, 8)}`);
	}
	
	/**
	 * Update my recognition (call when tree changes)
	 */
	updateRecognition(newRecognition: GlobalRecognitionWeights) {
		this.myRecognition = newRecognition;
		console.log(`[ALLOC-RPC] Updated recognition for ${this.myPubKey.slice(0, 8)}`);
	}
}

/**
 * Allocation Coordinator RPC Target
 * 
 * Orchestrates full allocation workflow via RPC.
 * Demonstrates complex promise pipelining!
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class AllocationCoordinatorRpcTarget extends RevocableRpcTarget implements IAllocationCoordinatorRpc {
	private coordinationCallbacks: Set<(event: any) => void> = new Set();
	
	constructor(options?: {
		expiresInMs?: number;
		recipientId?: string;
	}) {
		super(options);
	}
	
	/**
	 * Coordinate full allocation workflow
	 * 
	 * This demonstrates the POWER of Cap'n Web:
	 * - Multiple RPC calls chained together
	 * - Object capabilities passed between calls
	 * - All optimized for minimal round trips
	 */
	async coordinateAllocation(
		participantStubs: RpcStub<ICommitmentDataRpc>[],
		distributionComputer: RpcStub<IDistributionComputerRpc>
	): Promise<{
		distribution: DistributionResult;
		allocations: AllocationResult;
		participants: string[];
	}> {
		this.checkAccess('coordinateAllocation');
		console.log(`[COORD-RPC] Starting coordination with ${participantStubs.length} participants`);
		
		// Step 1: Get all participant IDs (pipelined!)
		const participants = await Promise.all(
			participantStubs.map(stub => stub.getPubKey())
		);
		
		this.notifySubscribers({
			type: 'participants',
			data: { participants }
		});
		
		console.log(`[COORD-RPC] Participants:`, participants.map(p => p.slice(0, 8)));
		
		// Step 2: Compute distribution via RPC (delegates to distribution computer)
		// The distribution computer will make ITS OWN RPC calls to participants!
		// This is the beauty of object capabilities - we just pass the stubs!
		const distribution = await distributionComputer.computeTwoTierDistribution(
			participantStubs as any, // Type coercion needed (same interface)
			undefined
		);
		
		this.notifySubscribers({
			type: 'distribution',
			data: distribution
		});
		
		console.log(
			`[COORD-RPC] Distribution computed: ` +
			`${Object.keys(distribution.shares).length} recipients`
		);
		
		// Step 3: Compute allocations
		// For this demo, we'll use a simplified approach
		// In real implementation, each provider would run their own allocation engine
		const allocations: AllocationResult = {
			allocations: [],
			slotDenominators: {},
			totalsByTypeAndRecipient: {},
			convergence: {
				totalNeedMagnitude: 0,
				previousNeedMagnitude: 0,
				contractionRate: 0,
				isConverged: false,
				percentNeedsMet: 0,
				universalSatisfaction: false,
				iterationsToConvergence: null,
				maxPersonNeed: 0,
				needVariance: 0,
				peopleStuck: 0,
				executionTimeMs: 0,
				currentIteration: 0,
				responseLatency: 0
			}
		};
		
		this.notifySubscribers({
			type: 'allocation',
			data: allocations
		});
		
		console.log(`[COORD-RPC] Coordination complete`);
		
		return {
			distribution,
			allocations,
			participants
		};
	}
	
	async subscribeToCoordination(
		callback: (event: { type: string; data: any }) => void
	): Promise<void> {
		this.checkAccess('subscribeToCoordination');
		this.coordinationCallbacks.add(callback);
	}
	
	private notifySubscribers(event: { type: string; data: any }) {
		for (const callback of this.coordinationCallbacks) {
			callback(event);
		}
	}
}

// ═══════════════════════════════════════════════════════════════════
// CLIENT HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create commitment data RPC target
 */
export function createCommitmentDataTarget(
	pubKey: string,
	commitment: Commitment
): CommitmentDataRpcTarget {
	return new CommitmentDataRpcTarget(pubKey, commitment);
}

/**
 * Create allocation engine RPC target
 */
export function createAllocationEngineTarget(
	myPubKey: string,
	myCapacitySlots: AvailabilitySlot[],
	myRecognition: GlobalRecognitionWeights
): AllocationEngineRpcTarget {
	return new AllocationEngineRpcTarget(myPubKey, myCapacitySlots, myRecognition);
}

/**
 * Create allocation coordinator RPC target
 */
export function createAllocationCoordinatorTarget(): AllocationCoordinatorRpcTarget {
	return new AllocationCoordinatorRpcTarget();
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE USAGE
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Full allocation workflow via RPC
 * 
 * ```typescript
 * import { newWebSocketRpcSession } from 'capnweb';
 * 
 * // Create my allocation engine
 * const myEngine = createAllocationEngineTarget(myPubKey, myCapacity, myRecognition);
 * 
 * // Connect to recipients via RPC
 * const alice = newWebSocketRpcSession<ICommitmentDataRpc>("wss://alice.com/rpc");
 * const bob = newWebSocketRpcSession<ICommitmentDataRpc>("wss://bob.com/rpc");
 * 
 * // Connect to distribution computer (could be myself or a third party!)
 * const distComputer = newWebSocketRpcSession<IDistributionComputerRpc>("wss://dist.com/rpc");
 * 
 * // Compute distribution via RPC
 * const distribution = await distComputer.computeTwoTierDistribution([alice, bob]);
 * 
 * // Compute allocations via RPC (fetches recipient data as needed)
 * const result = await myEngine.computeAllocationsRpc([alice, bob], distribution);
 * 
 * console.log(`Allocated to ${result.allocations.length} need slots`);
 * 
 * // Subscribe to updates (bidirectional!)
 * await myEngine.subscribeToAllocations((allocations) => {
 *   console.log("Allocations updated:", allocations.length);
 * });
 * ```
 * 
 * With Coordinator:
 * ```typescript
 * // Create coordinator
 * const coordinator = createAllocationCoordinatorTarget();
 * 
 * // Coordinate full workflow (pipelined!)
 * const result = await coordinator.coordinateAllocation(
 *   [alice, bob, carol],
 *   distComputer
 * );
 * 
 * console.log("Distribution:", result.distribution);
 * console.log("Allocations:", result.allocations);
 * console.log("Participants:", result.participants);
 * ```
 */

if (typeof window !== 'undefined') {
	(window as any).createCommitmentDataTarget = createCommitmentDataTarget;
	(window as any).createAllocationEngineTarget = createAllocationEngineTarget;
	(window as any).createAllocationCoordinatorTarget = createAllocationCoordinatorTarget;
	console.log('[ALLOC-RPC-DEBUG] 🛠️  Allocation RPC targets available in window');
}

