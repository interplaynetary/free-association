/**
 * Convergence Tests for Algorithm v2 (ITC + Event-Driven)
 * 
 * Test Coverage:
 * 1. Hybrid Damping (time-window + fallback)
 * 2. Convergence with Variable Update Timing
 * 3. ITC Causality Handling
 * 4. Allocation Capping (contractiveness)
 * 5. Denominator Floor (Lipschitz continuity)
 * 6. Edge Cases (slow updates, oscillations, sparse networks)
 * 
 * v2 Specific Tests:
 * - Hybrid damping fallback mechanism
 * - Time-based history filtering
 * - Event-driven convergence
 * - ITC stamp handling
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	computeAllocation,
	computeDampingFactor,
	updateCommitmentDamping,
	DENOMINATOR_FLOOR,
	CONVERGENCE_EPSILON,
	DAMPING_HISTORY_WINDOW_MS,
	DAMPING_HISTORY_MAX_COUNT
} from '../algorithm-v2.svelte';
import type { 
	Commitment, 
	AvailabilitySlot, 
	NeedSlot,
	DampingHistoryEntry,
	ITCStamp
} from '../v2/schemas';
import { seed as itcSeed, event as itcEvent } from '../utils/itc';

// ═══════════════════════════════════════════════════════════════════
// TEST HELPERS
// ═══════════════════════════════════════════════════════════════════

interface TestParticipant {
	pubKey: string;
	capacity: number;
	residualNeed: number;
	statedNeed: number;
	mrValues: Record<string, number>;
	recognitionWeights: Record<string, number>;
}

/**
 * Create a simple availability slot with given quantity
 */
function createAvailabilitySlot(id: string, quantity: number): AvailabilitySlot {
	return {
		id,
		name: 'Test Capacity',
		quantity
	};
}

/**
 * Create a simple need slot with given quantity
 */
function createNeedSlot(id: string, quantity: number): NeedSlot {
	return {
		id,
		name: 'Test Need',
		quantity
	};
}

/**
 * Create a test commitment for v2
 */
function createTestCommitment(
	residualNeed: number,
	statedNeed: number,
	capacity: number = 0,
	dampingFactor: number = 1.0,
	dampingHistory: DampingHistoryEntry[] = []
): Commitment {
	const commitment: Commitment = {
		mr_values: {},
		recognition_weights: {},
		damping_factor: dampingFactor,
		damping_history: dampingHistory,
		itcStamp: itcSeed(),
		timestamp: Date.now()
	};
	
	if (capacity > 0) {
		commitment.capacity_slots = [createAvailabilitySlot('slot-1', capacity)];
	}
	
	if (residualNeed > 0 || statedNeed > 0) {
		commitment.need_slots = [createNeedSlot('need-1', residualNeed)];
	}
	
	return commitment;
}

/**
 * Run allocation round for v2
 */
function runAllocationRound(
	providers: TestParticipant[],
	recipients: TestParticipant[],
	networkCommitments: Record<string, Commitment>
): Record<string, Record<string, number>> {
	const allocations: Record<string, Record<string, number>> = {};

	for (const provider of providers) {
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', provider.capacity)],
			mr_values: provider.mrValues,
			recognition_weights: provider.recognitionWeights,
			itcStamp: itcSeed(),
			timestamp: Date.now(),
			damping_factor: 1.0
		};
		
		const result = computeAllocation(
			provider.pubKey,
			providerCommitment,
			provider.mrValues,
			provider.recognitionWeights,
			networkCommitments
		);

		allocations[provider.pubKey] = result.recipient_totals;
	}

	return allocations;
}

/**
 * Aggregate allocations from all providers to a recipient
 */
function aggregateAllocations(
	allocations: Record<string, Record<string, number>>,
	recipientPubKey: string
): number {
	let total = 0;
	for (const providerAllocations of Object.values(allocations)) {
		total += providerAllocations[recipientPubKey] || 0;
	}
	return total;
}

// ═══════════════════════════════════════════════════════════════════
// V2 SPECIFIC: HYBRID DAMPING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Damping (v2 Critical Feature)', () => {
	it('should use time window when updates are fast (< 30s)', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 20000 },  // 20s ago
			{ overAllocation: 50, timestamp: now - 10000 },   // 10s ago
			{ overAllocation: 100, timestamp: now }            // now
		];

		const factor = computeDampingFactor(history);

		// Should detect oscillation (100 → 50 → 100)
		expect(factor).toBe(0.5);
	});

	it('should fall back to count when updates are slow (> 30s)', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 80000 },  // 80s ago (outside window)
			{ overAllocation: 50, timestamp: now - 40000 },   // 40s ago (outside window)
			{ overAllocation: 100, timestamp: now }            // now (only this in window)
		];

		const factor = computeDampingFactor(history);

		// Should STILL detect oscillation using fallback (count-based)
		// This is the critical fix - v1 would have returned 1.0 here
		expect(factor).toBe(0.5);
	});

	it('should detect oscillation even with very slow updates (2+ minutes)', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 240000 }, // 4 min ago
			{ overAllocation: 50, timestamp: now - 120000 },  // 2 min ago
			{ overAllocation: 100, timestamp: now }            // now
		];

		const factor = computeDampingFactor(history);

		// Fallback ensures oscillation is detected
		expect(factor).toBe(0.5);
	});

	it('should prefer time window over count when both available', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 200, timestamp: now - 60000 },  // 60s ago (outside window, but in count)
			{ overAllocation: 100, timestamp: now - 20000 },  // 20s ago (in window)
			{ overAllocation: 80, timestamp: now - 10000 },   // 10s ago (in window)
			{ overAllocation: 60, timestamp: now }             // now (in window)
		];

		const factor = computeDampingFactor(history);

		// Should use time window (last 3 within 30s): 100 → 80 → 60
		// This is smooth convergence, not oscillation
		expect(factor).toBe(1.0);
	});

	it('should handle mixed timing scenarios', () => {
		const now = Date.now();
		
		// Scenario 1: Fast then slow
		const history1: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 50000 },
			{ overAllocation: 50, timestamp: now - 5000 },
			{ overAllocation: 100, timestamp: now }
		];
		expect(computeDampingFactor(history1)).toBe(0.5); // Oscillation detected

		// Scenario 2: Slow then fast
		const history2: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 120000 },
			{ overAllocation: 80, timestamp: now - 10000 },
			{ overAllocation: 60, timestamp: now }
		];
		// Time window only has 2 entries, falls back to count: 100 → 80 → 60
		expect(computeDampingFactor(history2)).toBe(1.0); // Smooth convergence
	});

	it('should handle history with exactly 3 entries', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 20000 },
			{ overAllocation: 50, timestamp: now - 10000 },
			{ overAllocation: 100, timestamp: now }
		];

		expect(history.length).toBe(3);
		expect(computeDampingFactor(history)).toBe(0.5);
	});

	it('should return 1.0 when history has < 3 entries', () => {
		expect(computeDampingFactor([])).toBe(1.0);
		expect(computeDampingFactor([{ overAllocation: 100, timestamp: Date.now() }])).toBe(1.0);
		expect(computeDampingFactor([
			{ overAllocation: 100, timestamp: Date.now() - 10000 },
			{ overAllocation: 50, timestamp: Date.now() }
		])).toBe(1.0);
	});
});

describe('History Management (v2)', () => {
	it('should maintain time-windowed history correctly', () => {
		const commitment = createTestCommitment(500, 500);
		
		// Round 1: Receive 600 (over by 100)
		const updated1 = updateCommitmentDamping(commitment, 600);
		expect(updated1.damping_history).toHaveLength(1);
		expect(updated1.damping_history![0].overAllocation).toBe(100);
		expect(updated1.damping_history![0].timestamp).toBeDefined();

		// Round 2: Receive 550 (over by 50)
		const updated2 = updateCommitmentDamping(updated1, 550);
		expect(updated2.damping_history).toHaveLength(2);
		expect(updated2.damping_history![1].overAllocation).toBe(50);
	});

	it('should keep at least last 3 entries even if outside time window', () => {
		const now = Date.now();
		const oldHistory: DampingHistoryEntry[] = [
			{ overAllocation: 100, timestamp: now - 60000 }, // 1 min ago (outside 30s)
			{ overAllocation: 80, timestamp: now - 50000 },  // 50s ago (outside 30s)
			{ overAllocation: 60, timestamp: now - 40000 }   // 40s ago (outside 30s)
		];
		
		const commitment: Commitment = {
			need_slots: [createNeedSlot('need-1', 500)],
			mr_values: {},
			recognition_weights: {},
			damping_history: oldHistory,
			itcStamp: itcSeed(),
			timestamp: Date.now(),
			damping_factor: 1.0
		};

		const updated = updateCommitmentDamping(commitment, 540); // Over by 40

		// Should keep last 3 entries (fallback) even though they're outside time window
		expect(updated.damping_history!.length).toBeGreaterThanOrEqual(3);
		
		// Last entry should be the new one
		const lastEntry = updated.damping_history![updated.damping_history!.length - 1];
		expect(lastEntry.overAllocation).toBe(40);
	});

	it('should prefer time window when it has 3+ entries', () => {
		const now = Date.now();
		const history: DampingHistoryEntry[] = [
			{ overAllocation: 200, timestamp: now - 60000 }, // Outside window
			{ overAllocation: 100, timestamp: now - 20000 }, // In window
			{ overAllocation: 80, timestamp: now - 10000 },  // In window
			{ overAllocation: 60, timestamp: now - 5000 }    // In window
		];

		const commitment: Commitment = {
			need_slots: [createNeedSlot('need-1', 500)],
			mr_values: {},
			recognition_weights: {},
			damping_history: history,
			itcStamp: itcSeed(),
			timestamp: Date.now(),
			damping_factor: 1.0
		};

		const updated = updateCommitmentDamping(commitment, 480); // Over by 0

		// Should only keep entries in time window (3 most recent)
		expect(updated.damping_history!.length).toBe(3);
		
		// Should NOT include the 60s old entry
		const timestamps = updated.damping_history!.map(h => h.timestamp);
		expect(timestamps.includes(now - 60000)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION CAPPING (UNCHANGED BUT VERIFY WITH V2)
// ═══════════════════════════════════════════════════════════════════

describe('Allocation Capping (v2 Verification)', () => {
	it('should cap allocation when raw exceeds residual need', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(100, 500, 0)
		};

		const providerMR = { recipient1: 1.0 };
		const providerWeights = { recipient1: 1.0 };
		
		const providerCommitment = createTestCommitment(0, 0, 1000);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const allocation = result.recipient_totals['recipient1'] || 0;

		// Should be capped at 100 (need), not 1000 (capacity)
		expect(allocation).toBeLessThanOrEqual(100);
		expect(allocation).toBeGreaterThan(0);
	});

	it('should handle multiple recipients with capping', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(50, 50, 0),
			r2: createTestCommitment(500, 500, 0),
			r3: createTestCommitment(200, 200, 0)
		};

		const providerMR = { r1: 0.33, r2: 0.33, r3: 0.34 };
		const providerWeights = { r1: 0.33, r2: 0.33, r3: 0.34 };
		
		const providerCommitment = createTestCommitment(0, 0, 300);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const allocations = result.recipient_totals;

		// Each allocation capped by need
		expect(allocations['r1'] || 0).toBeLessThanOrEqual(50);
		expect(allocations['r2'] || 0).toBeLessThanOrEqual(500);
		expect(allocations['r3'] || 0).toBeLessThanOrEqual(200);

		// Total should not exceed capacity
		const total = Object.values(allocations).reduce((sum, a) => sum + a, 0);
		expect(total).toBeLessThanOrEqual(300);
	});
});

// ═══════════════════════════════════════════════════════════════════
// DENOMINATOR FLOOR (UNCHANGED BUT VERIFY WITH V2)
// ═══════════════════════════════════════════════════════════════════

describe('Denominator Floor (v2 Verification)', () => {
	it('should never allow zero denominator', () => {
		expect(DENOMINATOR_FLOOR).toBeGreaterThan(0);
		expect(DENOMINATOR_FLOOR).toBe(0.0001);
	});

	it('should handle zero-need scenario without division by zero', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(0, 100, 0),
			recipient2: createTestCommitment(0, 100, 0)
		};

		const providerMR = { recipient1: 0.5, recipient2: 0.5 };
		const providerWeights = { recipient1: 0.5, recipient2: 0.5 };
		
		const providerCommitment = createTestCommitment(0, 0, 100);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		// Should not throw
		expect(() => {
			computeAllocation(
				'provider1',
				providerCommitment,
				providerMR,
				providerWeights,
				networkCommitments
			);
		}).not.toThrow();
	});
});

// ═══════════════════════════════════════════════════════════════════
// CONVERGENCE SIMULATION (V2 WITH HYBRID DAMPING)
// ═══════════════════════════════════════════════════════════════════

describe('Convergence Simulation (v2)', () => {
	it('should converge from simple over-allocation scenario', () => {
		const provider: TestParticipant = {
			pubKey: 'provider1',
			capacity: 1000,
			residualNeed: 0,
			statedNeed: 0,
			mrValues: { recipient1: 1.0 },
			recognitionWeights: { recipient1: 1.0 }
		};

		const recipient: TestParticipant = {
			pubKey: 'recipient1',
			capacity: 0,
			residualNeed: 500,
			statedNeed: 500,
			mrValues: { provider1: 1.0 },
			recognitionWeights: { provider1: 1.0 }
		};

		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(
				recipient.residualNeed,
				recipient.statedNeed
			)
		};

		const allocations = runAllocationRound(
			[provider],
			[recipient],
			networkCommitments
		);

		const allocated = aggregateAllocations(allocations, 'recipient1');

		// Should allocate exactly 500 (capped by need)
		expect(allocated).toBe(500);
		expect(allocated).toBeLessThanOrEqual(recipient.residualNeed);
	});

	it('should converge with multiple rounds and hybrid damping', () => {
		const providers: TestParticipant[] = [
			{
				pubKey: 'p1',
				capacity: 300,
				residualNeed: 0,
				statedNeed: 0,
				mrValues: { r1: 0.6, r2: 0.4 },
				recognitionWeights: { r1: 0.6, r2: 0.4 }
			},
			{
				pubKey: 'p2',
				capacity: 200,
				residualNeed: 0,
				statedNeed: 0,
				mrValues: { r1: 0.3, r2: 0.7 },
				recognitionWeights: { r1: 0.3, r2: 0.7 }
			}
		];

		let recipients: TestParticipant[] = [
			{
				pubKey: 'r1',
				capacity: 0,
				residualNeed: 250,
				statedNeed: 250,
				mrValues: { p1: 0.6, p2: 0.3 },
				recognitionWeights: { p1: 0.6, p2: 0.3 }
			},
			{
				pubKey: 'r2',
				capacity: 0,
				residualNeed: 250,
				statedNeed: 250,
				mrValues: { p1: 0.4, p2: 0.7 },
				recognitionWeights: { p1: 0.4, p2: 0.7 }
			}
		];

		const maxRounds = 20;
		let round = 0;
		let converged = false;

		while (round < maxRounds && !converged) {
			round++;

			// Update commitments
			const networkCommitments: Record<string, Commitment> = {};
			for (const r of recipients) {
				networkCommitments[r.pubKey] = createTestCommitment(
					r.residualNeed,
					r.statedNeed
				);
			}

			// Run allocation
			const allocations = runAllocationRound(
				providers,
				recipients,
				networkCommitments
			);

			// Update residual needs
			let maxResidual = 0;
			for (const r of recipients) {
				const allocated = aggregateAllocations(allocations, r.pubKey);
				r.residualNeed = Math.max(0, r.residualNeed - allocated);
				maxResidual = Math.max(maxResidual, r.residualNeed);
			}

			// Check convergence
			if (maxResidual < CONVERGENCE_EPSILON) {
				converged = true;
			}
		}

		// Should converge
		expect(converged).toBe(true);
		expect(round).toBeLessThan(maxRounds);
		expect(round).toBeLessThanOrEqual(15); // With hybrid damping

		// All needs satisfied
		for (const r of recipients) {
			expect(r.residualNeed).toBeLessThan(CONVERGENCE_EPSILON);
		}
	});

	it('should converge even with slow updates (> 30s)', () => {
		// Simulate slow update scenario where hybrid fallback is critical
		const provider: TestParticipant = {
			pubKey: 'provider1',
			capacity: 600,
			residualNeed: 0,
			statedNeed: 0,
			mrValues: { r1: 1.0 },
			recognitionWeights: { r1: 1.0 }
		};

		let recipient: TestParticipant = {
			pubKey: 'r1',
			capacity: 0,
			residualNeed: 1000,
			statedNeed: 1000,
			mrValues: { provider1: 1.0 },
			recognitionWeights: { provider1: 1.0 }
		};

		// Simulate slow updates (40s apart)
		const now = Date.now();
		let dampingHistory: DampingHistoryEntry[] = [];
		let rounds = 0;
		const maxRounds = 30;

		while (rounds < maxRounds && recipient.residualNeed > CONVERGENCE_EPSILON) {
			rounds++;

			// Create commitment with simulated slow timestamps
			const commitment = createTestCommitment(
				recipient.residualNeed,
				recipient.statedNeed,
				0,
				1.0,
				dampingHistory
			);

			const networkCommitments: Record<string, Commitment> = {
				r1: commitment
			};

			const allocations = runAllocationRound(
				[provider],
				[recipient],
				networkCommitments
			);

			const allocated = aggregateAllocations(allocations, 'r1');
			recipient.residualNeed = Math.max(0, recipient.residualNeed - allocated);

			// Update damping history with slow timestamps (40s apart)
			const updatedCommitment = updateCommitmentDamping(commitment, allocated);
			dampingHistory = updatedCommitment.damping_history || [];
			
			// Simulate 40s passing between rounds
			if (dampingHistory.length > 0) {
				dampingHistory[dampingHistory.length - 1].timestamp = now + (rounds * 40000);
			}
		}

		// Should converge even with slow updates (thanks to hybrid fallback)
		expect(recipient.residualNeed).toBeLessThan(CONVERGENCE_EPSILON);
		expect(rounds).toBeLessThan(maxRounds);
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES (V2)
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases (v2)', () => {
	it('should handle recipient dropout', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
			// r2 dropped out
		};

		const providerMR = { r1: 0.5, r2: 0.5 };
		const providerWeights = { r1: 0.5, r2: 0.5 };
		
		const providerCommitment = createTestCommitment(0, 0, 100);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		expect(result.recipient_totals['r1']).toBeGreaterThan(0);
		expect(result.recipient_totals['r2'] || 0).toBe(0);
	});

	it('should handle zero capacity provider', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
		};

		const providerMR = { r1: 1.0 };
		const providerWeights = { r1: 1.0 };
		
		const providerCommitment = createTestCommitment(0, 0, 0);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		expect(result.recipient_totals['r1'] || 0).toBe(0);
	});

	it('should handle empty recognition', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
		};

		const providerMR = {};
		const providerWeights = {};
		
		const providerCommitment = createTestCommitment(0, 0, 100);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		expect(result.recipient_totals['r1'] || 0).toBe(0);
	});

	it('should maintain capacity conservation', () => {
		const networkCommitments: Record<string, Commitment> = {
			mutual1: createTestCommitment(100, 100),
			mutual2: createTestCommitment(100, 100),
			nonMutual1: createTestCommitment(100, 100),
			nonMutual2: createTestCommitment(100, 100)
		};

		const providerMR = {
			mutual1: 0.3,
			mutual2: 0.3
		};

		const providerWeights = {
			mutual1: 0.3,
			mutual2: 0.3,
			nonMutual1: 0.2,
			nonMutual2: 0.2
		};

		const capacity = 200;
		
		const providerCommitment = createTestCommitment(0, 0, capacity);
		providerCommitment.mr_values = providerMR;
		providerCommitment.recognition_weights = providerWeights;
		
		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const total = Object.values(result.recipient_totals).reduce(
			(sum, a) => sum + a,
			0
		);

		expect(total).toBeLessThanOrEqual(capacity + 0.01);
	});
});

// ═══════════════════════════════════════════════════════════════════
// V2 SPECIFIC: CONSTANTS VERIFICATION
// ═══════════════════════════════════════════════════════════════════

describe('V2 Constants', () => {
	it('should have correct time window constant', () => {
		expect(DAMPING_HISTORY_WINDOW_MS).toBe(30000); // 30 seconds
	});

	it('should have correct max count constant', () => {
		expect(DAMPING_HISTORY_MAX_COUNT).toBe(3);
	});

	it('should have correct convergence epsilon', () => {
		expect(CONVERGENCE_EPSILON).toBe(0.001);
	});

	it('should have correct denominator floor', () => {
		expect(DENOMINATOR_FLOOR).toBe(0.0001);
	});
});

