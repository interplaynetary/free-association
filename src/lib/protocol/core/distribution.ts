/**
 * Distribution Calculation Module
 * 
 * This module handles the calculation of "WHO gets WHAT share" independently
 * from the allocation engine that handles "HOW to actually allocate slots".
 * 
 * By separating distribution from allocation, we enable:
 * - Multiple distribution methods (mutual recognition, collective recognition, custom)
 * - Pluggable distribution strategies
 * - Easier testing and verification
 * - Cleaner architecture
 * 
 * Architecture:
 * ```
 * Distribution Layer (this file)
 *   ↓ produces DistributionResult
 * Allocation Engine (allocation.ts)
 *   ↓ produces SlotAllocationRecords
 * ```
 */

import type { GlobalRecognitionWeights, Node } from './schemas';
import { createMemoCache, createMemoCacheWithKey, hashObject } from './utils/memoize';
import { mutualFulfillment } from './tree';

// ═══════════════════════════════════════════════════════════════════
// DISTRIBUTION RESULT TYPE
// ═══════════════════════════════════════════════════════════════════

/**
 * Distribution Result
 * 
 * Represents the outcome of ANY distribution calculation method.
 * The allocation engine doesn't care HOW shares were calculated.
 * 
 * This separates "WHO gets WHAT share" (distribution) from
 * "HOW to actually allocate slots" (allocation engine).
 */
export interface DistributionResult {
	/**
	 * Recipient shares (0-1, typically sum to ≤ 1.0)
	 * Share represents: "What proportion of total capacity should this recipient receive?"
	 */
	shares: Record<string, number>;
	
	/**
	 * Method used to calculate distribution (for transparency)
	 */
	method: 'mutual-recognition' | 'collective-recognition' | 'equal-shares' | 'custom' | 'two-tier';
	
	/**
	 * Tier information for two-tier distributions
	 * Tier 1 = mutual recognition (priority)
	 * Tier 2 = non-mutual recognition (fallback)
	 */
	tiers?: {
		tier1: Record<string, number>;  // Mutual recognition shares
		tier2: Record<string, number>;  // Non-mutual recognition shares
	};
	
	/**
	 * Metadata about distribution calculation (for transparency/verification)
	 */
	metadata?: {
		/** For mutual recognition: pairwise MR matrix */
		mutualRecognitionMatrix?: Record<string, Record<string, number>>;
		
		/** For collective recognition: member recognition sums, pool */
		memberRecognitionSums?: Record<string, number>;
		totalPool?: number;
		
		/** Timestamp of calculation */
		timestamp: number;
		
		/** Any other method-specific data */
		[key: string]: any;
	};
}

// ═══════════════════════════════════════════════════════════════════
// MUTUAL RECOGNITION COMPUTATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute mutual recognition between me and others (including self)
 * 
 * MR(A,B) = min(A's recognition of B, B's recognition of A)
 * Special case: MR(me, me) = my recognition of myself
 * 
 * This function computes MR for:
 * 1. Everyone I recognize (Loop 1)
 * 2. Everyone who recognizes me (Loop 2) - even if I don't recognize them (MR will be 0)
 * 
 * This ensures complete network awareness: we know about everyone who has any recognition
 * relationship with us, making the MR map informative for transparency and debugging.
 * 
 * @param myRecognition - My recognition weights: { alice: 0.3, bob: 0.4, me: 0.5 }
 * @param othersRecognition - Others' recognition of me: { alice: { me: 0.5 }, bob: { me: 0.6 } }
 * @param myPubKey - My public key
 * @returns Mutual recognition: { alice: 0.3, bob: 0.4, me: 0.5, carol: 0 (if she recognizes me but I don't recognize her) }
 */
function _computeMutualRecognition(
	myRecognition: GlobalRecognitionWeights,
	othersRecognition: Record<string, GlobalRecognitionWeights>,
	myPubKey: string
): Record<string, number> {
	const mutual: Record<string, number> = {};
	
	// Loop 1: For everyone I recognize (including myself!)
	for (const [otherPubKey, myRecOfThem] of Object.entries(myRecognition)) {
		// Special case: Self-recognition
		// For mutual recognition with myself, "their recognition of me" IS "my recognition of myself"
		if (otherPubKey === myPubKey) {
			mutual[otherPubKey] = myRecOfThem;  // MR(me, me) = myRec[me]
			continue;
		}
		
		// Regular case: Mutual recognition with others
		const theirRecOfMe = othersRecognition[otherPubKey]?.[myPubKey] || 0;
		mutual[otherPubKey] = Math.min(myRecOfThem, theirRecOfMe);
	}
	
	// Loop 2: Also check people who recognize me (but I might not recognize them)
	// This ensures complete network awareness - we know about everyone
	for (const [otherPubKey, theirWeights] of Object.entries(othersRecognition)) {
		if (mutual[otherPubKey] !== undefined) continue; // Already computed in Loop 1
		
		const theirRecOfMe = theirWeights?.[myPubKey] || 0;
		const myRecOfThem = myRecognition[otherPubKey] || 0;
		
		// MR will be 0 if I don't recognize them, but we still include them
		// This shows "I've seen their recognition of me, but I don't mutually recognize them"
		mutual[otherPubKey] = Math.min(myRecOfThem, theirRecOfMe);
	}
	
	return mutual;
}

// Memoized version of computeMutualRecognition
export const computeMutualRecognition = createMemoCacheWithKey(
	_computeMutualRecognition,
	(myRecognition, othersRecognition, myPubKey) => 
		`${hashObject(myRecognition)}:${hashObject(othersRecognition)}:${myPubKey}`,
	100 // Cache up to 100 recognition computations
);

// ═══════════════════════════════════════════════════════════════════
// DISTRIBUTION CALCULATION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate Two-Tier Mutual Recognition Distribution
 * 
 * This separates DISTRIBUTION CALCULATION from ALLOCATION EXECUTION.
 * The result can be passed to any allocation engine.
 * 
 * Two-Tier Strategy:
 * - Tier 1 (Priority): Mutual recognition (people who recognize me back)
 * - Tier 2 (Fallback): Non-mutual recognition (people I recognize but don't recognize me back)
 * 
 * @param myRecognition - My recognition of others
 * @param othersRecognition - Others' recognition of me
 * @param myPubKey - My public key
 * @param compatibleRecipients - Set of recipients who have compatible needs (optional filter)
 * @returns Distribution result with tier information
 */
export function calculateTwoTierMutualRecognitionDistribution(
	myRecognition: GlobalRecognitionWeights,
	othersRecognition: Record<string, GlobalRecognitionWeights>,
	myPubKey: string,
	compatibleRecipients?: Set<string>
): DistributionResult {
	// Calculate mutual recognition for all potential recipients
	const mutualRecognition = computeMutualRecognition(
		myRecognition,
		othersRecognition,
		myPubKey
	);
	
	// Build mutual recognition matrix for transparency
	const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
	mutualRecognitionMatrix[myPubKey] = {};
	
	// Separate into tiers
	const tier1Shares: Record<string, number> = {}; // Mutual recognition
	const tier2Shares: Record<string, number> = {}; // Non-mutual recognition
	const allShares: Record<string, number> = {};
	
	let totalTier1Recognition = 0;
	let totalTier2Recognition = 0;
	
	// Classify recipients into tiers
	for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
		// Skip if not in compatible recipients set (if provided)
		if (compatibleRecipients && !compatibleRecipients.has(recipientId)) {
			continue;
		}
		
		// Add to matrix for transparency
		mutualRecognitionMatrix[myPubKey][recipientId] = mr;
		
		if (mr > 0) {
			// Tier 1: Mutual recognition
			tier1Shares[recipientId] = mr;
			totalTier1Recognition += mr;
		} else {
			// Tier 2: Check if I recognize them (one-way)
			const myRecOfThem = myRecognition[recipientId] || 0;
			if (myRecOfThem > 0) {
				tier2Shares[recipientId] = myRecOfThem;
				totalTier2Recognition += myRecOfThem;
			}
		}
	}
	
	// Normalize tier 1 shares
	if (totalTier1Recognition > 0) {
		for (const recipientId in tier1Shares) {
			const normalized = tier1Shares[recipientId] / totalTier1Recognition;
			tier1Shares[recipientId] = normalized;
			allShares[recipientId] = normalized;
		}
	}
	
	// Normalize tier 2 shares
	if (totalTier2Recognition > 0) {
		for (const recipientId in tier2Shares) {
			const normalized = tier2Shares[recipientId] / totalTier2Recognition;
			tier2Shares[recipientId] = normalized;
			allShares[recipientId] = normalized;
		}
	}
	
	return {
		shares: allShares,
		method: 'two-tier',
		tiers: {
			tier1: tier1Shares,
			tier2: tier2Shares
		},
		metadata: {
			mutualRecognitionMatrix,
			totalTier1Recognition,
			totalTier2Recognition,
			timestamp: Date.now()
		}
	};
}

/**
 * Calculate Simple Mutual Recognition Distribution (Single-Tier)
 * 
 * Simpler version: only allocate to people with mutual recognition.
 * No tier 2 fallback.
 * 
 * @param myRecognition - My recognition of others
 * @param othersRecognition - Others' recognition of me
 * @param myPubKey - My public key
 * @param compatibleRecipients - Set of recipients who have compatible needs (optional filter)
 * @returns Distribution result
 */
export function calculateMutualRecognitionDistribution(
	myRecognition: GlobalRecognitionWeights,
	othersRecognition: Record<string, GlobalRecognitionWeights>,
	myPubKey: string,
	compatibleRecipients?: Set<string>
): DistributionResult {
	// Calculate mutual recognition
	const mutualRecognition = computeMutualRecognition(
		myRecognition,
		othersRecognition,
		myPubKey
	);
	
	// Build matrix for transparency
	const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
	mutualRecognitionMatrix[myPubKey] = {};
	
	const shares: Record<string, number> = {};
	let totalRecognition = 0;
	
	// Only include recipients with mutual recognition > 0
	for (const [recipientId, mr] of Object.entries(mutualRecognition)) {
		// Skip if not in compatible recipients set (if provided)
		if (compatibleRecipients && !compatibleRecipients.has(recipientId)) {
			continue;
		}
		
		mutualRecognitionMatrix[myPubKey][recipientId] = mr;
		
		if (mr > 0) {
			shares[recipientId] = mr;
			totalRecognition += mr;
		}
	}
	
	// Normalize shares
	if (totalRecognition > 0) {
		for (const recipientId in shares) {
			shares[recipientId] = shares[recipientId] / totalRecognition;
		}
	}
	
	return {
		shares,
		method: 'mutual-recognition',
		metadata: {
			mutualRecognitionMatrix,
			totalRecognition,
			timestamp: Date.now()
		}
	};
}

/**
 * Calculate Equal Shares Distribution (Fallback)
 * 
 * Everyone gets an equal share. Useful for testing or when no recognition data is available.
 * 
 * @param recipientIds - List of recipient IDs
 * @returns Distribution result
 */
export function calculateEqualSharesDistribution(
	recipientIds: string[]
): DistributionResult {
	const shares: Record<string, number> = {};
	const equalShare = recipientIds.length > 0 ? 1.0 / recipientIds.length : 0;
	
	for (const id of recipientIds) {
		shares[id] = equalShare;
	}
	
	return {
		shares,
		method: 'equal-shares',
		metadata: {
			timestamp: Date.now(),
			recipientCount: recipientIds.length
		}
	};
}

/**
 * Create Custom Distribution
 * 
 * Allows you to specify shares directly. Useful for:
 * - DAO voting results
 * - Manual overrides
 * - Testing
 * 
 * @param shares - Map of recipient ID to share (0-1)
 * @returns Distribution result
 */
export function createCustomDistribution(
	shares: Record<string, number>
): DistributionResult {
	return {
		shares,
		method: 'custom',
		metadata: {
			timestamp: Date.now()
		}
	};
}

/**
 * Calculate Collective Recognition Distribution
 * 
 * Calculates shares based on collective recognition within a member set.
 * Uses symmetric mutual recognition within a closed group.
 * 
 * Formula:
 * - Pool = Σ MutualRecognition(i, j) for all pairs in member set
 * - Member's Share = (Σ MutualRecognition(Member, Others)) / Pool
 * 
 * @param memberSet - Array of member IDs in the collective
 * @param memberTrees - Map of member ID to their recognition tree
 * @returns Distribution result with collective recognition shares
 */
export function calculateCollectiveRecognitionDistribution(
	memberSet: string[],
	memberTrees: Map<string, Node>
): DistributionResult {
	const shares: Record<string, number> = {};
	const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
	const memberRecognitionSums: Record<string, number> = {};
	
	// Convert map to object for mutualFulfillment function
	const nodesMap = Object.fromEntries(memberTrees);
	
	// Calculate total mutual recognition pool and build pairwise matrix
	let totalPool = 0;
	
	// Calculate sum of mutual recognitions for each member
	for (const memberId of memberSet) {
		const memberTree = memberTrees.get(memberId);
		mutualRecognitionMatrix[memberId] = {};
		
		if (!memberTree) {
			shares[memberId] = 0;
			memberRecognitionSums[memberId] = 0;
			continue;
		}
		
		let memberSum = 0;
		for (const otherId of memberSet) {
			if (otherId === memberId) continue;
			
			const otherTree = memberTrees.get(otherId);
			if (!otherTree) continue;
			
			const mutualRec = mutualFulfillment(memberTree, otherTree, nodesMap);
			
			// Store in pairwise matrix for verification
			mutualRecognitionMatrix[memberId][otherId] = mutualRec;
			
			memberSum += mutualRec;
		}
		
		memberRecognitionSums[memberId] = memberSum;
		totalPool += memberSum;
	}
	
	// Normalize to shares
	if (totalPool === 0) {
		// Equal shares only among members WITH trees
		// Members without trees get 0 share
		const membersWithTrees = memberSet.filter(id => memberTrees.has(id));
		const equalShare = membersWithTrees.length > 0 ? 1.0 / membersWithTrees.length : 0;
		for (const memberId of memberSet) {
			shares[memberId] = memberTrees.has(memberId) ? equalShare : 0;
		}
	} else {
		for (const memberId of memberSet) {
			const memberSum = memberRecognitionSums[memberId] || 0;
			shares[memberId] = memberSum / totalPool;
		}
	}
	
	return {
		shares,
		method: 'collective-recognition',
		metadata: {
			mutualRecognitionMatrix,
			memberRecognitionSums,
			totalPool,
			timestamp: Date.now()
		}
	};
}

