/**
 * Distribution Calculation Module - RPC Edition
 * 
 * Uses symmetric Cap'n Web RPC protocol for distributed allocation computation.
 * 
 * Architecture:
 * - Pure functions for local computation (same as before)
 * - RPC interfaces for distributed computation
 * - Promise pipelining for efficient multi-participant calculations
 * - Object capabilities for secure access control
 * 
 * Key Innovation: Distribution can be computed EITHER:
 * 1. Locally (if you have all data) - original functions
 * 2. Via RPC (if data is distributed) - new RPC targets
 */

import type { GlobalRecognitionWeights, Node } from '$lib/protocol/schemas';
import { createMemoCache, createMemoCacheWithKey, hashObject } from '$lib/protocol/utils/memoize';
import { mutualFulfillment } from '$lib/protocol/tree';
import { RpcTarget, type RpcStub } from 'capnweb';
import { z } from 'zod';
import { RevocableRpcTarget } from './rpc-elegant';

// Re-export original types and functions
export * from '../../../../../../../src/lib/protocol/distribution';
import type { DistributionResult } from '../../../../../../../src/lib/protocol/distribution';

// ═══════════════════════════════════════════════════════════════════
// RPC INTERFACES (TypeScript)
// ═══════════════════════════════════════════════════════════════════

/**
 * Mutual Recognition Computer RPC Interface
 * 
 * Allows participants to compute mutual recognition without sharing full data.
 * Uses object capabilities for privacy: only what you explicitly expose is visible.
 */
export interface IMutualRecognitionRpc {
	/** Get my recognition of a specific participant */
	getRecognitionOf(participantId: string): Promise<number>;
	
	/** Get my recognition of all participants */
	getAllRecognition(): Promise<GlobalRecognitionWeights>;
	
	/** 
	 * Compute mutual recognition with another participant (via RPC!)
	 * Note: otherParticipant typed as 'any' to avoid circular type reference
	 * In practice, pass RpcStub<IMutualRecognitionRpc>
	 */
	computeMutualWith(
		otherParticipant: any
	): Promise<number>;
	
	/** Get my participant ID */
	getParticipantId(): Promise<string>;
}

/**
 * Distribution Computer RPC Interface
 * 
 * Symmetric protocol: Anyone can compute distribution and share results.
 */
export interface IDistributionComputerRpc {
	/** 
	 * Compute two-tier mutual recognition distribution
	 * 
	 * @param participants - RPC stubs to all participants
	 * @param compatibleRecipients - Optional filter
	 * @returns Distribution result with tier information
	 */
	computeTwoTierDistribution(
		participants: RpcStub<IMutualRecognitionRpc>[],
		compatibleRecipients?: Set<string>
	): Promise<DistributionResult>;
	
	/**
	 * Compute simple mutual recognition distribution
	 */
	computeMutualDistribution(
		participants: RpcStub<IMutualRecognitionRpc>[],
		compatibleRecipients?: Set<string>
	): Promise<DistributionResult>;
	
	/**
	 * Compute collective recognition distribution
	 * 
	 * @param memberStubs - RPC stubs to collective members
	 * @returns Distribution result
	 */
	computeCollectiveDistribution(
		memberStubs: RpcStub<IMutualRecognitionRpc>[]
	): Promise<DistributionResult>;
	
	/** Subscribe to distribution updates (when participants change) */
	subscribeToUpdates(
		callback: (distribution: DistributionResult) => void
	): Promise<void>;
	
	/** Get my public key */
	getMyPubKey(): Promise<string>;
}

/**
 * Zod Schema for DistributionResult (for validation over RPC)
 */
export const DistributionResultSchema = z.object({
	shares: z.record(z.string(), z.number()),
	method: z.enum(['mutual-recognition', 'collective-recognition', 'equal-shares', 'custom', 'two-tier']),
	tiers: z.object({
		tier1: z.record(z.string(), z.number()),
		tier2: z.record(z.string(), z.number())
	}).optional(),
	metadata: z.record(z.any()).optional()
});

// ═══════════════════════════════════════════════════════════════════
// RPC TARGETS (Server-side implementations)
// ═══════════════════════════════════════════════════════════════════

/**
 * Mutual Recognition RPC Target
 * 
 * Exposes my recognition weights via RPC for distributed MR computation.
 * Privacy: Only exposes what I choose to expose (object capability model).
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class MutualRecognitionRpcTarget extends RevocableRpcTarget implements IMutualRecognitionRpc {
	private participantId: string;
	private recognitionWeights: GlobalRecognitionWeights;
	
	constructor(
		participantId: string,
		recognitionWeights: GlobalRecognitionWeights,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.participantId = participantId;
		this.recognitionWeights = recognitionWeights;
	}
	
	async getRecognitionOf(participantId: string): Promise<number> {
		this.checkAccess('getRecognitionOf');
		return this.recognitionWeights[participantId] || 0;
	}
	
	async getAllRecognition(): Promise<GlobalRecognitionWeights> {
		this.checkAccess('getAllRecognition');
		return { ...this.recognitionWeights };
	}
	
	/**
	 * Compute mutual recognition via RPC (demonstrates symmetric protocol!)
	 * 
	 * Formula: MR(me, them) = min(myRec[them], theirRec[me])
	 * 
	 * This is BIDIRECTIONAL:
	 * 1. I call their RPC method to get their recognition of me
	 * 2. They can call my RPC method to get my recognition of them
	 * 
	 * No need for a central server!
	 */
	async computeMutualWith(
		otherParticipant: any // RpcStub<IMutualRecognitionRpc> - any to avoid circular type
	): Promise<number> {
		this.checkAccess('computeMutualWith');
		
		// Get their ID
		const theirId = await otherParticipant.getParticipantId();
		
		// My recognition of them (local)
		const myRecOfThem = this.recognitionWeights[theirId] || 0;
		
		// Their recognition of me (via RPC!)
		// This is a REMOTE CALL - demonstrates symmetric protocol
		const theirRecOfMe = await otherParticipant.getRecognitionOf(this.participantId);
		
		// Compute MR
		const mr = Math.min(myRecOfThem, theirRecOfMe);
		
		console.log(
			`[MR-RPC] MR(${this.participantId.slice(0, 8)}, ${theirId.slice(0, 8)}): ` +
			`min(${myRecOfThem.toFixed(3)}, ${theirRecOfMe.toFixed(3)}) = ${mr.toFixed(3)}`
		);
		
		return mr;
	}
	
	async getParticipantId(): Promise<string> {
		this.checkAccess('getParticipantId');
		return this.participantId;
	}
	
	/**
	 * Update recognition weights (call this when tree changes)
	 */
	updateRecognitionWeights(newWeights: GlobalRecognitionWeights) {
		this.recognitionWeights = newWeights;
		console.log(`[MR-RPC] Updated recognition weights for ${this.participantId.slice(0, 8)}`);
	}
}

/**
 * Distribution Computer RPC Target
 * 
 * Computes distribution using RPC calls to participants.
 * Demonstrates promise pipelining: chain multiple RPC calls in one round trip!
 * 
 * Now with built-in revocation from RevocableRpcTarget!
 */
export class DistributionComputerRpcTarget extends RevocableRpcTarget implements IDistributionComputerRpc {
	private myPubKey: string;
	private myRecognition: GlobalRecognitionWeights;
	private updateCallbacks: Set<(distribution: DistributionResult) => void> = new Set();
	
	constructor(
		myPubKey: string,
		myRecognition: GlobalRecognitionWeights,
		options?: {
			expiresInMs?: number;
			recipientId?: string;
		}
	) {
		super(options);
		this.myPubKey = myPubKey;
		this.myRecognition = myRecognition;
	}
	
	/**
	 * Compute two-tier distribution via RPC
	 * 
	 * This demonstrates PROMISE PIPELINING:
	 * - Multiple RPC calls chained together
	 * - All done in minimal round trips
	 * - Cap'n Web optimizes the message flow
	 */
	async computeTwoTierDistribution(
		participants: RpcStub<IMutualRecognitionRpc>[],
		compatibleRecipients?: Set<string>
	): Promise<DistributionResult> {
		this.checkAccess('computeTwoTierDistribution');
		console.log(`[DIST-RPC] Computing two-tier distribution with ${participants.length} participants`);
		
		// Build mutual recognition map via RPC calls
		const mutualRecognition: Record<string, number> = {};
		const othersRecognition: Record<string, GlobalRecognitionWeights> = {};
		
		// Get all participant IDs first (pipelined!)
		const participantIds = await Promise.all(
			participants.map(p => p.getParticipantId())
		);
		
		console.log(`[DIST-RPC] Participant IDs:`, participantIds.map(id => id.slice(0, 8)));
		
		// Get all recognition data (pipelined!)
		const allRecognition = await Promise.all(
			participants.map(p => p.getAllRecognition())
		);
		
		// Build others' recognition map
		for (let i = 0; i < participantIds.length; i++) {
			othersRecognition[participantIds[i]] = allRecognition[i];
		}
		
		// Compute mutual recognition for each participant
		for (const participantId of participantIds) {
			const myRecOfThem = this.myRecognition[participantId] || 0;
			
			// Self-recognition
			if (participantId === this.myPubKey) {
				mutualRecognition[participantId] = myRecOfThem;
				continue;
			}
			
			// Mutual recognition
			const theirRecOfMe = othersRecognition[participantId]?.[this.myPubKey] || 0;
			mutualRecognition[participantId] = Math.min(myRecOfThem, theirRecOfMe);
		}
		
		// Separate into tiers
		const tier1Shares: Record<string, number> = {};
		const tier2Shares: Record<string, number> = {};
		const allShares: Record<string, number> = {};
		
		let totalTier1Recognition = 0;
		let totalTier2Recognition = 0;
		
		for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
			// Filter by compatible recipients if provided
			if (compatibleRecipients && !compatibleRecipients.has(recipientId)) {
				continue;
			}
			
			if (mr > 0) {
				// Tier 1: Mutual recognition
				tier1Shares[recipientId] = mr;
				totalTier1Recognition += mr;
			} else {
				// Tier 2: Check if I recognize them (one-way)
				const myRecOfThem = this.myRecognition[recipientId] || 0;
				if (myRecOfThem > 0) {
					tier2Shares[recipientId] = myRecOfThem;
					totalTier2Recognition += myRecOfThem;
				}
			}
		}
		
		// Normalize tier 1
		if (totalTier1Recognition > 0) {
			for (const recipientId in tier1Shares) {
				const normalized = tier1Shares[recipientId] / totalTier1Recognition;
				tier1Shares[recipientId] = normalized;
				allShares[recipientId] = normalized;
			}
		}
		
		// Normalize tier 2
		if (totalTier2Recognition > 0) {
			for (const recipientId in tier2Shares) {
				const normalized = tier2Shares[recipientId] / totalTier2Recognition;
				tier2Shares[recipientId] = normalized;
				allShares[recipientId] = normalized;
			}
		}
		
		const result: DistributionResult = {
			shares: allShares,
			method: 'two-tier',
			tiers: {
				tier1: tier1Shares,
				tier2: tier2Shares
			},
			metadata: {
				totalTier1Recognition,
				totalTier2Recognition,
				timestamp: Date.now(),
				computedViaRpc: true
			}
		};
		
		// Notify subscribers
		for (const callback of this.updateCallbacks) {
			callback(result);
		}
		
		console.log(
			`[DIST-RPC] Distribution complete: ` +
			`${Object.keys(tier1Shares).length} tier1, ` +
			`${Object.keys(tier2Shares).length} tier2`
		);
		
		return result;
	}
	
	async computeMutualDistribution(
		participants: RpcStub<IMutualRecognitionRpc>[],
		compatibleRecipients?: Set<string>
	): Promise<DistributionResult> {
		this.checkAccess('computeMutualDistribution');
		console.log(`[DIST-RPC] Computing mutual distribution with ${participants.length} participants`);
		
		// Similar to two-tier but only tier 1
		const twoTier = await this.computeTwoTierDistribution(participants, compatibleRecipients);
		
		return {
			shares: twoTier.tiers?.tier1 || {},
			method: 'mutual-recognition',
			metadata: {
				...twoTier.metadata,
				timestamp: Date.now()
			}
		};
	}
	
	async computeCollectiveDistribution(
		memberStubs: RpcStub<IMutualRecognitionRpc>[]
	): Promise<DistributionResult> {
		this.checkAccess('computeCollectiveDistribution');
		console.log(`[DIST-RPC] Computing collective distribution with ${memberStubs.length} members`);
		
		// Get all member IDs (pipelined!)
		const memberIds = await Promise.all(
			memberStubs.map(m => m.getParticipantId())
		);
		
		// Build mutual recognition matrix via RPC
		const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
		const memberRecognitionSums: Record<string, number> = {};
		
		let totalPool = 0;
		
		// For each member, compute their mutual recognition with all others
		for (let i = 0; i < memberStubs.length; i++) {
			const memberA = memberStubs[i];
			const memberAId = memberIds[i];
			
			mutualRecognitionMatrix[memberAId] = {};
			let memberSum = 0;
			
			// Compute MR with all other members (pipelined where possible!)
			const mrPromises = memberStubs
				.filter((_, j) => j !== i)
				.map(memberB => memberA.computeMutualWith(memberB));
			
			const mrValues = await Promise.all(mrPromises);
			
			let mrIndex = 0;
			for (let j = 0; j < memberStubs.length; j++) {
				if (j === i) continue;
				
				const memberBId = memberIds[j];
				const mutualRec = mrValues[mrIndex++];
				
				mutualRecognitionMatrix[memberAId][memberBId] = mutualRec;
				memberSum += mutualRec;
			}
			
			memberRecognitionSums[memberAId] = memberSum;
			totalPool += memberSum;
		}
		
		// Normalize to shares
		const shares: Record<string, number> = {};
		
		if (totalPool === 0) {
			// Equal shares fallback
			const equalShare = memberIds.length > 0 ? 1.0 / memberIds.length : 0;
			for (const memberId of memberIds) {
				shares[memberId] = equalShare;
			}
		} else {
			for (const memberId of memberIds) {
				shares[memberId] = (memberRecognitionSums[memberId] || 0) / totalPool;
			}
		}
		
		console.log(
			`[DIST-RPC] Collective distribution complete: ` +
			`${memberIds.length} members, pool=${totalPool.toFixed(3)}`
		);
		
		return {
			shares,
			method: 'collective-recognition',
			metadata: {
				mutualRecognitionMatrix,
				memberRecognitionSums,
				totalPool,
				timestamp: Date.now(),
				computedViaRpc: true
			}
		};
	}
	
	async subscribeToUpdates(
		callback: (distribution: DistributionResult) => void
	): Promise<void> {
		this.checkAccess('subscribeToUpdates');
		this.updateCallbacks.add(callback);
	}
	
	async getMyPubKey(): Promise<string> {
		this.checkAccess('getMyPubKey');
		return this.myPubKey;
	}
	
	/**
	 * Update my recognition (call when tree changes)
	 */
	updateMyRecognition(newRecognition: GlobalRecognitionWeights) {
		this.myRecognition = newRecognition;
		console.log(`[DIST-RPC] Updated my recognition for ${this.myPubKey.slice(0, 8)}`);
	}
}

// ═══════════════════════════════════════════════════════════════════
// CLIENT HELPERS (For using RPC distribution)
// ═══════════════════════════════════════════════════════════════════

/**
 * Create my mutual recognition RPC target
 */
export function createMyMutualRecognitionTarget(
	myPubKey: string,
	myRecognitionWeights: GlobalRecognitionWeights
): MutualRecognitionRpcTarget {
	return new MutualRecognitionRpcTarget(myPubKey, myRecognitionWeights);
}

/**
 * Create my distribution computer RPC target
 */
export function createMyDistributionComputerTarget(
	myPubKey: string,
	myRecognitionWeights: GlobalRecognitionWeights
): DistributionComputerRpcTarget {
	return new DistributionComputerRpcTarget(myPubKey, myRecognitionWeights);
}

/**
 * Connect to a participant's mutual recognition RPC endpoint
 */
export function connectToMutualRecognitionRpc(
	wsUrl: string
): RpcStub<IMutualRecognitionRpc> {
	// In real implementation, use newWebSocketRpcSession
	// For now, just type declaration
	throw new Error('Not implemented - use newWebSocketRpcSession from capnweb');
}

/**
 * Connect to a participant's distribution computer RPC endpoint
 */
export function connectToDistributionComputerRpc(
	wsUrl: string
): RpcStub<IDistributionComputerRpc> {
	throw new Error('Not implemented - use newWebSocketRpcSession from capnweb');
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE USAGE
// ═══════════════════════════════════════════════════════════════════

/**
 * Example: Compute distribution using RPC with promise pipelining
 * 
 * ```typescript
 * // Create my RPC target
 * const myTarget = createMyDistributionComputerTarget(myPubKey, myRecognition);
 * 
 * // Connect to other participants via RPC (WebSocket)
 * const alice = newWebSocketRpcSession<IMutualRecognitionRpc>("wss://alice.com/rpc");
 * const bob = newWebSocketRpcSession<IMutualRecognitionRpc>("wss://bob.com/rpc");
 * const carol = newWebSocketRpcSession<IMutualRecognitionRpc>("wss://carol.com/rpc");
 * 
 * // Compute distribution (all RPC calls pipelined!)
 * const distribution = await myTarget.computeTwoTierDistribution([alice, bob, carol]);
 * 
 * // Subscribe to updates (bidirectional!)
 * await myTarget.subscribeToUpdates((dist) => {
 *   console.log("Distribution updated:", dist);
 * });
 * ```
 */

if (typeof window !== 'undefined') {
	(window as any).createMyMutualRecognitionTarget = createMyMutualRecognitionTarget;
	(window as any).createMyDistributionComputerTarget = createMyDistributionComputerTarget;
	console.log('[DIST-RPC-DEBUG] 🛠️  RPC targets available in window');
}

