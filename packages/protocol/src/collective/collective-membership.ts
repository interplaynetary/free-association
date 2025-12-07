/**
 * Collective Membership Module - Pure Functions
 * 
 * Pure computational functions for MRD (Mutual Recognition Density) membership computation.
 * These functions operate on plain data structures without Svelte dependencies.
 * 
 * For Svelte store integration, see collective-membership.svelte.ts
 * 
 * Core Concept:
 * - Membership emerges from depth of reciprocal relationships
 * - MRD (Mutual Recognition Density) = total mutual recognition / network average
 * - Members have MRD ≥ threshold (default 0.5)
 * - See extensive documentation in collective-membership.svelte.ts wrapper
 */

import type { RecognitionData, MembershipOutput, HealthMetrics } from './schemas';

export const EPSILON = 1e-9;

// ═══════════════════════════════════════════════════════════════════
// MRD MEMBERSHIP MODULE (Core Class)
// ═══════════════════════════════════════════════════════════════════

/**
 * MRD Membership Module
 * 
 * Computes network membership based on Mutual Recognition Density.
 * Maintains history of computations for tracking membership changes over time.
 */
export class MRDMembershipModule {
    private threshold: number;
    private minimumRecognition: number;
    private history: MembershipOutput[] = [];

    constructor(threshold: number = 0.5, minimumRecognition: number = 0.0) {
        this.threshold = threshold;
        this.minimumRecognition = minimumRecognition;
    }

    setThreshold(newThreshold: number): void {
        if (!(newThreshold >= 0 && newThreshold <= 1)) {
            throw new Error("Threshold must be between 0.0 and 1.0");
        }
        this.threshold = newThreshold;
    }

    setMinimumRecognition(newMinimum: number): void {
        if (!(newMinimum >= 0 && newMinimum <= 100)) {
            throw new Error("Minimum recognition must be between 0.0 and 100.0");
        }
        this.minimumRecognition = newMinimum;
    }

    computeMembership(
        recognitionData: RecognitionData[],
        currentMembers: Set<string> | string[]
    ): MembershipOutput {
        // Build recognition matrix
        const recognitionMatrix: Map<string, Map<string, number>> = new Map();
        const participants: Set<string> = new Set();

        for (const rec of recognitionData) {
            if (!recognitionMatrix.has(rec.fromId)) {
                recognitionMatrix.set(rec.fromId, new Map());
            }
            recognitionMatrix.get(rec.fromId)!.set(rec.toId, rec.percentage);
            participants.add(rec.fromId);
            participants.add(rec.toId);
        }

        // Helper to get directed recognition value
        const getDirectedRecognition = (fromId: string, toId: string): number => {
            return recognitionMatrix.get(fromId)?.get(toId) ?? 0.0;
        };

        // Helper to compute mutual recognition with minimum filter
        const getMutualRecognition = (a: string, b: string): number => {
            if (a === b) return 0.0;
            const aToB = getDirectedRecognition(a, b);
            const bToA = getDirectedRecognition(b, a);
            const mutual = Math.min(aToB, bToA);
            return mutual >= this.minimumRecognition ? mutual : 0.0;
        };

        // Initialize membership from provided seed
        const membershipStatus: Record<string, 'member' | 'candidate' | 'removed'> = {};
        const seedMembers: Set<string> = new Set(Array.isArray(currentMembers) ? currentMembers : Array.from(currentMembers));
        for (const p of participants) {
            membershipStatus[p] = seedMembers.has(p) ? 'member' : 'candidate';
        }

        let iterations = 0;
        const maxIterations = 5;
        let changed = true;

        // Iteratively recompute MRS relative to current members only
        let mrs: Record<string, number> = {};
        let mrdScores: Record<string, number> = {};
        let averageMrs = 0;

        while (changed && iterations < maxIterations) {
            iterations += 1;
            const previousStatus = { ...membershipStatus };

            const currentMembersIter: Set<string> = new Set(
                Object.keys(membershipStatus).filter((p) => membershipStatus[p] === 'member')
            );

            // Compute MRS per participant relative to current members
            mrs = {};
            for (const i of participants) {
                let score = 0;
                if (currentMembersIter.size > 0) {
                    for (const j of currentMembersIter) {
                        if (i !== j) {
                            score += getMutualRecognition(i, j);
                        }
                    }
                } else {
                    // Bootstrap: if no members yet, sum over all participants
                    for (const j of participants) {
                        if (i !== j) {
                            score += getMutualRecognition(i, j);
                        }
                    }
                }
                mrs[i] = score;
            }

            // Compute average MRS using current members or bootstrap fallback
            if (currentMembersIter.size > 0) {
                let sum = 0;
                for (const p of currentMembersIter) sum += mrs[p] ?? 0;
                averageMrs = currentMembersIter.size > 0 ? sum / currentMembersIter.size : 0;
            } else {
                // Fallback: use max MRS as the baseline if we have any participants
                averageMrs = Object.values(mrs).length > 0 ? Math.max(...Object.values(mrs)) : 1.0;
            }

            // Compute MRD and update membership with epsilon-adjusted ≥
            mrdScores = {};
            for (const p of participants) {
                const value = averageMrs > 0 ? (mrs[p] ?? 0) / averageMrs : 0;
                mrdScores[p] = value;
                membershipStatus[p] = value >= (this.threshold - EPSILON) ? 'member' : 'candidate';
            }

            // Detect change
            changed = Object.keys(membershipStatus).some((k) => membershipStatus[k] !== previousStatus[k]);
        }

        // Compute members list, added, and removed
        const members: string[] = Object.keys(membershipStatus).filter((p) => membershipStatus[p] === 'member');
        const added: string[] = [];
        const removed: string[] = [];
        
        // Compare with previous membership to determine added/removed
        if (this.history.length > 0) {
            const previousMembers = this.history[this.history.length - 1].members;
            const previousMemberSet = new Set(previousMembers);
            const currentMemberSet = new Set(members);
            
            for (const member of members) {
                if (!previousMemberSet.has(member)) {
                    added.push(member);
                }
            }
            
            for (const prevMember of previousMembers) {
                if (!currentMemberSet.has(prevMember)) {
                    removed.push(prevMember);
                }
            }
        } else {
            // First computation - all current members are "added"
            added.push(...members);
        }

        // Build mutual recognition matrix for transparency (independent verification)
        const mutualRecognitionMatrix: Record<string, Record<string, number>> = {};
        for (const i of participants) {
            mutualRecognitionMatrix[i] = {};
            for (const j of participants) {
                if (i !== j) {
                    mutualRecognitionMatrix[i][j] = getMutualRecognition(i, j);
                }
            }
        }

        // Compute health metrics
        const mrdScoresArray = Object.values(mrdScores);
        const mrsScoresArray = Object.values(mrs);
        
        const recognitionDensity = participants.size > 0 ? members.length / participants.size : 0;
        const averageMRD = mrdScoresArray.length > 0 ? mrdScoresArray.reduce((a, b) => a + b, 0) / mrdScoresArray.length : 0;
        
        const mrdVariance = mrdScoresArray.length > 0
            ? mrdScoresArray.reduce((sum, score) => sum + Math.pow(score - averageMRD, 2), 0) / mrdScoresArray.length
            : 0;
        
        const memberStability = this.history.length > 0
            ? 1 - (added.length + removed.length) / Math.max(this.history[this.history.length - 1].members.length, 1)
            : 1;

        const output: MembershipOutput = {
            timestamp: new Date(),
            members,
            added,
            removed,
            mrdScores,
            membershipStatus,
            mutualRecognitionScores: mrs,
            networkAverage: averageMrs,
            mutualRecognitionMatrix, // For independent verification
            healthMetrics: {
                recognitionDensity,
                averageMRD,
                mrdVariance,
                memberStability,
                memberCount: members.length
            }
        };

        this.history.push(output);
        return output;
    }

    isMember(participantId: string): boolean {
        if (this.history.length === 0) return false;
        const last = this.history[this.history.length - 1];
        return last.membershipStatus[participantId] === 'member';
    }

    getMrd(participantId: string): number {
        if (this.history.length === 0) return 0.0;
        const last = this.history[this.history.length - 1];
        return last.mrdScores[participantId] ?? 0.0;
    }

    getMutualRecognitionScore(participantId: string): number {
        if (this.history.length === 0) return 0.0;
        const last = this.history[this.history.length - 1];
        return last.mutualRecognitionScores[participantId] ?? 0.0;
    }

    getNetworkAverage(): number {
        if (this.history.length === 0) return 0.0;
        const last = this.history[this.history.length - 1];
        return last.networkAverage;
    }

    getMembershipHistory(participantId: string): Array<{ timestamp: Date; isMember: boolean }>{
        return this.history.map((h) => ({ timestamp: h.timestamp, isMember: !!h.membershipStatus[participantId] }));
    }

    getMrdHistory(participantId: string): Array<{ timestamp: Date; mrd: number }>{
        return this.history.map((h) => ({ timestamp: h.timestamp, mrd: h.mrdScores[participantId] ?? 0.0 }));
    }

    getIntegrationBreakdown(
        participantId: string,
        recognitionData: RecognitionData[]
    ): Record<string, number> {
        const recognitionMatrix: Map<string, Map<string, number>> = new Map();
        for (const rec of recognitionData) {
            if (!recognitionMatrix.has(rec.fromId)) recognitionMatrix.set(rec.fromId, new Map());
            recognitionMatrix.get(rec.fromId)!.set(rec.toId, rec.percentage);
        }

        const getDir = (fromId: string, toId: string): number => recognitionMatrix.get(fromId)?.get(toId) ?? 0.0;
        const breakdown: Record<string, number> = {};

        for (const rec of recognitionData) {
            if (rec.fromId === participantId) {
                const other = rec.toId;
                const mutual = Math.min(rec.percentage, getDir(other, participantId));
                if (mutual >= this.minimumRecognition) breakdown[other] = mutual;
            }
        }

        return breakdown;
    }
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Calculate health metrics from membership output
 */
export function calculateHealthMetrics(output: MembershipOutput): HealthMetrics {
    const mrdScores = Object.values(output.mrdScores);
    const memberCount = output.members.length;
    const participantCount = Object.keys(output.membershipStatus).length;

    // Calculate averageMRD
    const averageMRD = mrdScores.length > 0 
        ? mrdScores.reduce((a, b) => a + b, 0) / mrdScores.length 
        : 0;

    // Calculate mrdVariance
    const mrdVariance = mrdScores.length > 0
        ? mrdScores.reduce((sum, score) => sum + Math.pow(score - averageMRD, 2), 0) / mrdScores.length
        : 0;

    // Calculate recognitionDensity (how many participants are members)
    const recognitionDensity = participantCount > 0 ? memberCount / participantCount : 0;

    // Calculate memberStability (based on added/removed)
    const memberStability = memberCount > 0
        ? 1 - (output.added.length + output.removed.length) / memberCount
        : 1;

    return {
        recognitionDensity,
        averageMRD,
        mrdVariance,
        memberStability,
        memberCount
    };
}

// ═══════════════════════════════════════════════════════════════════
// CAPACITY MEMBERSHIP MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Update capacity membership based on MRD computation
 * 
 * This allows any capacity to become a "collective capacity" by dynamically
 * updating its member list based on mutual recognition density.
 * 
 * Process:
 * 1. Compute MRD for all participants relative to current members
 * 2. Add participants who meet threshold and have mutual recognition with current members
 * 3. Remove current members who fall below threshold
 * 4. Return updated member list
 * 
 * @param currentMembers - Current member list of the capacity
 * @param recognitionData - All recognition data
 * @param threshold - MRD threshold for membership (defaults to 0.5)
 * @param options - Optional configuration
 * @returns Updated member list and computation details
 */
export function updateCapacityMembership(
    currentMembers: string[],
    recognitionData: RecognitionData[],
    threshold: number = 0.5,
    options: {
        minMutualRecognition?: number; // Minimum mutual recognition required with existing members
        maxNewMembers?: number; // Maximum new members to add in one update
        preserveFoundingMembers?: string[]; // Members who cannot be removed
    } = {}
): {
    members: string[];
    added: string[];
    removed: string[];
    mrdScores: Record<string, number>;
    mutualRecognitionScores: Record<string, number>;
    timestamp: Date;
} {
    const minMutualRecognition = options.minMutualRecognition ?? 0.01; // Small positive value
    const maxNewMembers = options.maxNewMembers ?? Infinity;
    const preserveFoundingMembers = new Set(options.preserveFoundingMembers || []);

    // Use MRD module to compute membership relative to current members
    const module = new MRDMembershipModule(threshold, minMutualRecognition);
    const output = module.computeMembership(recognitionData, currentMembers);

    // Determine who should be added
    const potentialNewMembers: Array<{ id: string; mrd: number }> = [];
    for (const [participantId, status] of Object.entries(output.membershipStatus)) {
        if (status === 'member' && !currentMembers.includes(participantId)) {
            potentialNewMembers.push({
                id: participantId,
                mrd: output.mrdScores[participantId]
            });
        }
    }

    // Sort by MRD (highest first) and limit to maxNewMembers
    potentialNewMembers.sort((a, b) => b.mrd - a.mrd);
    const added = potentialNewMembers
        .slice(0, maxNewMembers)
        .map(m => m.id);

    // Determine who should be removed (excluding founding members)
    const removed: string[] = [];
    for (const memberId of currentMembers) {
        if (preserveFoundingMembers.has(memberId)) continue; // Cannot remove founding members
        
        const status = output.membershipStatus[memberId];
        if (status !== 'member') {
            removed.push(memberId);
        }
    }

    // Build updated member list
    const updatedMembers = [
        ...currentMembers.filter(id => !removed.includes(id)),
        ...added
    ];

    return {
        members: updatedMembers,
        added,
        removed,
        mrdScores: output.mrdScores,
        mutualRecognitionScores: output.mutualRecognitionScores,
        timestamp: output.timestamp
    };
}

/**
 * Check if a capacity's membership needs updating
 * 
 * @param capacity - Capacity with dynamic membership settings
 * @param timeSinceLastUpdate - Time since last update in milliseconds
 * @param defaultUpdateFrequencyMs - How often to update (defaults to weekly: 7 days)
 * @returns Whether membership should be updated
 */
export function shouldUpdateCapacityMembership(
	capacity: {
		auto_update_members_by_mrd?: boolean;
		last_membership_update?: string;
		membership_update_frequency_ms?: number;
	},
	timeSinceLastUpdate?: number,
	defaultUpdateFrequencyMs: number = 7 * 24 * 60 * 60 * 1000 // 7 days default
): boolean {
	// Only update if auto-update is enabled
	if (!capacity.auto_update_members_by_mrd) return false;

	// If never updated, should update
	if (!capacity.last_membership_update) return true;

	// Use capacity's custom frequency or the default
	const updateFrequencyMs = capacity.membership_update_frequency_ms || defaultUpdateFrequencyMs;

	// Check if enough time has passed
	if (timeSinceLastUpdate !== undefined) {
		return timeSinceLastUpdate >= updateFrequencyMs;
	}

	// Calculate time since last update
	const lastUpdate = new Date(capacity.last_membership_update);
	const now = new Date();
	const elapsed = now.getTime() - lastUpdate.getTime();
	
	return elapsed >= updateFrequencyMs;
}

/**
 * Manually trigger membership recomputation for a capacity
 * This bypasses the frequency check and forces an immediate update
 * 
 * @param currentMembers - Current member list of the capacity
 * @param recognitionData - All recognition data for MRD computation
 * @param threshold - MRD threshold for membership (defaults to 0.5)
 * @param options - Optional configuration
 * @returns Updated member list and computation details
 */
export function forceUpdateCapacityMembership(
	currentMembers: string[],
	recognitionData: RecognitionData[],
	threshold: number = 0.5,
	options: {
		minMutualRecognition?: number;
		maxNewMembers?: number;
		preserveFoundingMembers?: string[];
	} = {}
): {
	members: string[];
	added: string[];
	removed: string[];
	mrdScores: Record<string, number>;
	mutualRecognitionScores: Record<string, number>;
	timestamp: Date;
} {
	// Just call the regular update function - it doesn't check frequency
	return updateCapacityMembership(currentMembers, recognitionData, threshold, options);
}

