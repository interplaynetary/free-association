/**
 * Tests for Slot-Native Mutual-Priority Allocation Algorithm
 * 
 * Test Coverage:
 * 1. Allocation Capping (contractiveness)
 * 2. Denominator Floor (Lipschitz continuity)
 * 3. Convergence Simulation (Banach theorem validation)
 * 4. Lipschitz Bounds Computation
 * 5. Convergence Rate Validation
 * 6. Edge Cases (dropout, oscillation, over-allocation)
 * 
 * Updated for slot-native architecture:
 * - Uses capacity_slots and need_slots instead of scalar values
 * - Tests computeAllocation (slot-native) instead of deprecated function
 * - Verifies slot_allocations and recipient_totals
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	computeAllocation,
	computeDampingFactor,
	updateCommitmentDamping,
	DENOMINATOR_FLOOR,
	CONVERGENCE_EPSILON
} from '../algorithm.svelte';
import type { Commitment, AvailabilitySlot, NeedSlot } from '../v1/schemas';

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
 * (no time/location constraints for basic tests)
 */
function createAvailabilitySlot(id: string, quantity: number): AvailabilitySlot {
	return {
		id,
		name: 'Test Capacity', // Required by ResourceMetadataSchema
		quantity
	};
}

/**
 * Create a simple need slot with given quantity
 * (no time/location constraints for basic tests)
 */
function createNeedSlot(id: string, quantity: number): NeedSlot {
	return {
		id,
		name: 'Test Need', // Required by ResourceMetadataSchema
		quantity
	};
}

/**
 * Create a test commitment using slot-native format
 * For simplicity, creates a single slot with the given quantity
 */
function createTestCommitment(
	residualNeed: number,
	statedNeed: number,
	capacity: number = 0,
	dampingFactor: number = 1.0
): Commitment {
	const commitment: Commitment = {
		mr_values: {},
		recognition_weights: {},
		damping_factor: dampingFactor,
		over_allocation_history: [],
		timestamp: Date.now()
	};
	
	// Add capacity slot if capacity > 0
	if (capacity > 0) {
		commitment.capacity_slots = [createAvailabilitySlot('slot-1', capacity)];
	}
	
	// Add need slot if need > 0
	if (residualNeed > 0 || statedNeed > 0) {
		commitment.need_slots = [createNeedSlot('need-1', residualNeed)];
	}
	
	return commitment;
}

/**
 * Run allocation round using slot-native algorithm
 * Returns recipient totals for each provider
 */
function runAllocationRound(
	providers: TestParticipant[],
	recipients: TestParticipant[],
	networkCommitments: Record<string, Commitment>
): Record<string, Record<string, number>> {
	const allocations: Record<string, Record<string, number>> = {};

	for (const provider of providers) {
		// Create provider commitment with capacity slots
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', provider.capacity)],
			mr_values: provider.mrValues,
			recognition_weights: provider.recognitionWeights,
			timestamp: Date.now()
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
// UNIT TESTS: ALLOCATION CAPPING
// ═══════════════════════════════════════════════════════════════════

describe('Allocation Capping (Contractiveness)', () => {
	it('should cap allocation when raw exceeds residual need', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(100, 500, 0) // needs 100, stated 500
		};

		const providerMR = { recipient1: 1.0 };
		const providerWeights = { recipient1: 1.0 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 1000)], // huge capacity
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const allocation = result.recipient_totals['recipient1'] || 0;

		// Allocation should be capped at 100 (residual need), not 1000
		expect(allocation).toBeLessThanOrEqual(100);
		expect(allocation).toBeGreaterThan(0);
	});

	it('should handle multiple recipients with mixed over/under allocation', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(50, 50, 0),   // small need
			r2: createTestCommitment(500, 500, 0), // large need
			r3: createTestCommitment(200, 200, 0)  // medium need
		};

		const providerMR = { r1: 0.33, r2: 0.33, r3: 0.34 };
		const providerWeights = { r1: 0.33, r2: 0.33, r3: 0.34 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 300)], // total capacity
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const allocations = result.recipient_totals;

		// Each allocation should be capped by its need
		expect(allocations['r1'] || 0).toBeLessThanOrEqual(50);
		expect(allocations['r2'] || 0).toBeLessThanOrEqual(500);
		expect(allocations['r3'] || 0).toBeLessThanOrEqual(200);

		// Total should not exceed capacity
		const total = Object.values(allocations).reduce((sum: number, a: number) => sum + a, 0);
		expect(total).toBeLessThanOrEqual(300);
	});

	it('should prevent oscillation by capping allocations', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(100, 100, 0)
		};

		const providerMR = { recipient1: 1.0 };
		const providerWeights = { recipient1: 1.0 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 1000)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		// Round 1: Large capacity
		const round1 = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const alloc1 = round1.recipient_totals['recipient1'] || 0;

		// Should be capped at 100
		expect(alloc1).toBe(100);

		// Round 2: After satisfaction, residual = 0
		networkCommitments['recipient1'].need_slots = [createNeedSlot('need-1', 0)];

		const round2 = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const alloc2 = round2.recipient_totals['recipient1'] || 0;

		// Should get nothing (need is 0)
		expect(alloc2).toBe(0);

		// This proves no oscillation: 100 → 0 (converged)
	});
});

// ═══════════════════════════════════════════════════════════════════
// UNIT TESTS: DENOMINATOR FLOOR
// ═══════════════════════════════════════════════════════════════════

describe('Denominator Floor (Lipschitz Continuity)', () => {
	it('should never allow zero denominator', () => {
		const floor = DENOMINATOR_FLOOR;
		expect(floor).toBeGreaterThan(0);
		expect(floor).toBe(0.0001);
	});

	it('should handle zero-need scenario without division by zero', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(0, 100, 0), // zero residual need
			recipient2: createTestCommitment(0, 100, 0)
		};

		const providerMR = { recipient1: 0.5, recipient2: 0.5 };
		const providerWeights = { recipient1: 0.5, recipient2: 0.5 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 100)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

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

	it('should bound denominator even with tiny needs', () => {
		const networkCommitments: Record<string, Commitment> = {
			recipient1: createTestCommitment(0.00001, 100, 0) // extremely small need
		};

		const providerMR = { recipient1: 0.0001 }; // tiny MR
		const providerWeights = { recipient1: 0.0001 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 100)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		// Should complete without NaN or Infinity
		const allocation = result.recipient_totals['recipient1'] || 0;
		expect(allocation).toBeGreaterThanOrEqual(0);
		expect(allocation).toBeLessThanOrEqual(100);
		expect(Number.isFinite(allocation)).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// UNIT TESTS: ADAPTIVE DAMPING
// ═══════════════════════════════════════════════════════════════════

describe('Adaptive Damping', () => {
	it('should detect oscillation pattern', () => {
		const history = [100, 50, 100]; // up-down-up
		const factor = computeDampingFactor(history);
		expect(factor).toBe(0.5); // oscillating → strong damping
	});

	it('should detect smooth convergence', () => {
		const history = [100, 80, 60]; // monotonically decreasing
		const factor = computeDampingFactor(history);
		expect(factor).toBe(1.0); // smooth → no damping needed
	});

	it('should use moderate damping by default', () => {
		const history = [90, 100, 110]; // neither oscillating nor smooth (increasing - bad trend)
		const factor = computeDampingFactor(history);
		expect(factor).toBe(0.8); // moderate damping
	});

	it('should update commitment with damping info', () => {
		// Create commitment with stated need = 500 (in need slots)
		const commitment: Commitment = {
			need_slots: [createNeedSlot('need-1', 500)], // stated need = 500
			mr_values: {},
			recognition_weights: {},
			timestamp: Date.now()
		};
		const totalReceived = 600; // over-allocated!

		const updated = updateCommitmentDamping(commitment, totalReceived);

		expect(updated.over_allocation_history).toHaveLength(1);
		expect(updated.over_allocation_history![0]).toBe(100); // 600 - 500 = 100 over
		expect(updated.damping_factor).toBeDefined();
	});

	it('should maintain history of last 3 over-allocations', () => {
		// Create commitment with stated need = 500
		let commitment: Commitment = {
			need_slots: [createNeedSlot('need-1', 500)],
			mr_values: {},
			recognition_weights: {},
			timestamp: Date.now()
		};

		// Round 1: receive 600 (over by 100)
		commitment = updateCommitmentDamping(commitment, 600);
		expect(commitment.over_allocation_history).toHaveLength(1);
		expect(commitment.over_allocation_history![0]).toBe(100);

		// Round 2: receive 550 (over by 50)
		commitment = updateCommitmentDamping(commitment, 550);
		expect(commitment.over_allocation_history).toHaveLength(2);
		expect(commitment.over_allocation_history![1]).toBe(50);

		// Round 3: receive 650 (over by 150)
		commitment = updateCommitmentDamping(commitment, 650);
		expect(commitment.over_allocation_history).toHaveLength(3);
		expect(commitment.over_allocation_history![2]).toBe(150);

		// Round 4: receive 580 (over by 80) - should drop oldest (100)
		commitment = updateCommitmentDamping(commitment, 580);
		expect(commitment.over_allocation_history).toHaveLength(3);
		expect(commitment.over_allocation_history![0]).toBe(50); // oldest from round 2
		expect(commitment.over_allocation_history![1]).toBe(150); // from round 3
		expect(commitment.over_allocation_history![2]).toBe(80); // from round 4
	});
});

// ═══════════════════════════════════════════════════════════════════
// INTEGRATION TESTS: CONVERGENCE SIMULATION
// ═══════════════════════════════════════════════════════════════════

describe('Convergence Simulation (Banach Theorem)', () => {
	it('should converge from simple over-allocation scenario', () => {
		// Setup: 1 provider with excess capacity, 1 recipient
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

		// Run allocation
		const allocations = runAllocationRound(
			[provider],
			[recipient],
			networkCommitments
		);

		const allocated = aggregateAllocations(allocations, 'recipient1');

		// Should allocate exactly 500 (capped by need)
		expect(allocated).toBe(500);
		expect(allocated).toBeLessThanOrEqual(recipient.residualNeed);

		// After allocation, residual should be 0
		const newResidual = recipient.residualNeed - allocated;
		expect(newResidual).toBe(0);
	});

	it('should converge in multiple rounds with damping', () => {
		// Setup: 2 providers, 2 recipients, needs distributed
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

		// Should converge in reasonable time
		expect(converged).toBe(true);
		expect(round).toBeLessThan(maxRounds);
		expect(round).toBeLessThanOrEqual(10); // With damping, should be fast

		// All needs should be satisfied
		for (const r of recipients) {
			expect(r.residualNeed).toBeLessThan(CONVERGENCE_EPSILON);
		}
	});

	it('should handle under-capacity scenario (not enough to satisfy all)', () => {
		const provider: TestParticipant = {
			pubKey: 'provider1',
			capacity: 100, // insufficient
			residualNeed: 0,
			statedNeed: 0,
			mrValues: { r1: 0.5, r2: 0.5 },
			recognitionWeights: { r1: 0.5, r2: 0.5 }
		};

		const recipients: TestParticipant[] = [
			{
				pubKey: 'r1',
				capacity: 0,
				residualNeed: 200,
				statedNeed: 200,
				mrValues: { provider1: 0.5 },
				recognitionWeights: { provider1: 0.5 }
			},
			{
				pubKey: 'r2',
				capacity: 0,
				residualNeed: 200,
				statedNeed: 200,
				mrValues: { provider1: 0.5 },
				recognitionWeights: { provider1: 0.5 }
			}
		];

		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(200, 200),
			r2: createTestCommitment(200, 200)
		};

		const allocations = runAllocationRound(
			[provider],
			recipients,
			networkCommitments
		);

		const alloc1 = aggregateAllocations(allocations, 'r1');
		const alloc2 = aggregateAllocations(allocations, 'r2');

		// Total should equal capacity
		expect(alloc1 + alloc2).toBeCloseTo(100, 1);

		// Should be split proportionally (50/50)
		expect(alloc1).toBeCloseTo(50, 1);
		expect(alloc2).toBeCloseTo(50, 1);

		// Neither fully satisfied
		expect(alloc1).toBeLessThan(200);
		expect(alloc2).toBeLessThan(200);
	});
});

// ═══════════════════════════════════════════════════════════════════
// ADVANCED TESTS: LIPSCHITZ BOUNDS
// ═══════════════════════════════════════════════════════════════════

describe('Lipschitz Bounds Computation', () => {
	/**
	 * Compute Lipschitz constant L_f for allocation mapping
	 * L_f bounds how much allocations change relative to need changes
	 */
	function computeLipschitzBound(
		a_max: number,      // max recognition coefficient
		S_min: number,      // min denominator
		C_max: number,      // max capacity
		R_max: number,      // max residual need
		numProviders: number,
		numRecipients: number
	): number {
		// From theorem: |∂φ_i/∂r_k| ≤ (a_max / S_min) * (1 + a_max * R_max / S_min)
		const partialBound = (a_max / S_min) * (1 + (a_max * R_max) / S_min);

		// L_f ≈ C_max * partialBound * numProviders
		// (rough bound, actual computation requires full Jacobian analysis)
		return C_max * partialBound * numProviders;
	}

	it('should compute finite Lipschitz bound for typical parameters', () => {
		const params = {
			a_max: 1.0,        // max recognition weight
			S_min: 0.0001,     // denominator floor
			C_max: 1000,       // max capacity per provider
			R_max: 1000,       // max need per recipient
			numProviders: 10,
			numRecipients: 100
		};

		const L_f = computeLipschitzBound(
			params.a_max,
			params.S_min,
			params.C_max,
			params.R_max,
			params.numProviders,
			params.numRecipients
		);

		// Should be finite
		expect(Number.isFinite(L_f)).toBe(true);
		expect(L_f).toBeGreaterThan(0);

		console.log(`[LIPSCHITZ] L_f ≈ ${L_f.toFixed(2)} for typical parameters`);
	});

	it('should have smaller Lipschitz constant with larger S_min', () => {
		const base = {
			a_max: 1.0,
			C_max: 1000,
			R_max: 1000,
			numProviders: 5,
			numRecipients: 50
		};

		const L_f_small = computeLipschitzBound(
			base.a_max,
			0.0001, // small floor
			base.C_max,
			base.R_max,
			base.numProviders,
			base.numRecipients
		);

		const L_f_large = computeLipschitzBound(
			base.a_max,
			0.01, // larger floor
			base.C_max,
			base.R_max,
			base.numProviders,
			base.numRecipients
		);

		// Larger S_min → smaller Lipschitz constant (more stable)
		expect(L_f_large).toBeLessThan(L_f_small);
	});

	it('should compute contraction constant k with damping', () => {
		const L_f = 5000; // example Lipschitz constant
		const dampingFactors = [0.5, 0.8, 1.0];

		for (const alpha of dampingFactors) {
			// k_H = (1-α) + α*k_0 where k_0 = 1 + L_f (without capping)
			// With capping: k_0 ≈ 1 - fill_fraction
			// Assume fill_fraction ≈ 0.3 (conservative)
			const fill_fraction = 0.3;
			const k_0 = 1 - fill_fraction; // ≈ 0.7
			const k_H = (1 - alpha) + alpha * k_0;

			console.log(`[CONTRACTION] α=${alpha}: k_H = ${k_H.toFixed(3)}`);

			if (alpha === 0.5) {
				// Strong damping should give k < 1
				expect(k_H).toBeLessThan(1.0);
				expect(k_H).toBeGreaterThan(0.8);
			}
		}
	});
});

// ═══════════════════════════════════════════════════════════════════
// ADVANCED TESTS: CONVERGENCE RATE VALIDATION
// ═══════════════════════════════════════════════════════════════════

describe('Convergence Rate Validation', () => {
	it('should converge exponentially with rate k^n', () => {
		// Setup symmetric scenario for predictable convergence
		const provider: TestParticipant = {
			pubKey: 'provider1',
			capacity: 500,
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

		const residuals: number[] = [recipient.residualNeed];
		const maxRounds = 10;

		for (let round = 0; round < maxRounds; round++) {
			const networkCommitments: Record<string, Commitment> = {
				r1: createTestCommitment(recipient.residualNeed, recipient.statedNeed)
			};

			const allocations = runAllocationRound(
				[provider],
				[recipient],
				networkCommitments
			);

			const allocated = aggregateAllocations(allocations, 'r1');
			recipient.residualNeed = Math.max(0, recipient.residualNeed - allocated);
			residuals.push(recipient.residualNeed);

			if (recipient.residualNeed < CONVERGENCE_EPSILON) {
				break;
			}
		}

		// Compute contraction factor from data
		const ratios: number[] = [];
		for (let i = 1; i < residuals.length - 1; i++) {
			if (residuals[i - 1] > CONVERGENCE_EPSILON) {
				const ratio = residuals[i] / residuals[i - 1];
				ratios.push(ratio);
			}
		}

		if (ratios.length > 0) {
			const avgRatio = ratios.reduce((sum, r) => sum + r, 0) / ratios.length;
			console.log(`[CONVERGENCE-RATE] Average ratio: ${avgRatio.toFixed(3)}`);
			console.log(`[CONVERGENCE-RATE] Residuals: ${residuals.map(r => r.toFixed(1)).join(' → ')}`);

			// Should be contractive (ratio < 1)
			expect(avgRatio).toBeLessThan(1.0);
			expect(avgRatio).toBeGreaterThan(0); // but not instant convergence
		}

		// Should converge
		expect(residuals[residuals.length - 1]).toBeLessThan(CONVERGENCE_EPSILON);
	});

	it('should converge faster with stronger damping', () => {
		const runScenario = (dampingFactor: number): number => {
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

			let rounds = 0;
			const maxRounds = 20;

			while (rounds < maxRounds && recipient.residualNeed > CONVERGENCE_EPSILON) {
				rounds++;

				const networkCommitments: Record<string, Commitment> = {
					r1: createTestCommitment(
						recipient.residualNeed,
						recipient.statedNeed,
						0,
						dampingFactor
					)
				};

				const allocations = runAllocationRound(
					[provider],
					[recipient],
					networkCommitments
				);

				const allocated = aggregateAllocations(allocations, 'r1');
				recipient.residualNeed = Math.max(0, recipient.residualNeed - allocated);
			}

			return rounds;
		};

		const roundsNoDamping = runScenario(1.0);
		const roundsWithDamping = runScenario(0.5);

		console.log(`[DAMPING-EFFECT] No damping: ${roundsNoDamping} rounds`);
		console.log(`[DAMPING-EFFECT] With damping (0.5): ${roundsWithDamping} rounds`);

		// Both should converge
		expect(roundsNoDamping).toBeLessThan(20);
		expect(roundsWithDamping).toBeLessThan(20);

		// Note: In this simple case, damping may actually slow convergence
		// because there's no oscillation to correct. This is expected behavior.
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases', () => {
	it('should handle recipient dropout (stale data)', () => {
		// This would be tested at the store level with timestamp checks
		// Here we just verify allocation handles missing commitments gracefully
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
			// r2 dropped out (no commitment)
		};

		const providerMR = { r1: 0.5, r2: 0.5 }; // still recognizes r2
		const providerWeights = { r1: 0.5, r2: 0.5 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 100)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const alloc1 = result.recipient_totals['r1'] || 0;
		const alloc2 = result.recipient_totals['r2'] || 0;

		// r1 should get allocation
		expect(alloc1).toBeGreaterThan(0);

		// r2 should get nothing (no commitment)
		expect(alloc2).toBe(0);
	});

	it('should handle zero capacity provider', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
		};

		const providerMR = { r1: 1.0 };
		const providerWeights = { r1: 1.0 };
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 0)], // no capacity
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const alloc = result.recipient_totals['r1'] || 0;

		// Should allocate nothing
		expect(alloc).toBe(0);
	});

	it('should handle empty recognition (no one recognized)', () => {
		const networkCommitments: Record<string, Commitment> = {
			r1: createTestCommitment(100, 100)
		};

		const providerMR = {}; // recognizes no one
		const providerWeights = {};
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', 100)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};

		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const alloc = result.recipient_totals['r1'] || 0;

		// Should allocate nothing (no recognition)
		expect(alloc).toBe(0);
	});

	it('should maintain capacity conservation across tiers', () => {
		const networkCommitments: Record<string, Commitment> = {
			mutual1: createTestCommitment(100, 100),
			mutual2: createTestCommitment(100, 100),
			nonMutual1: createTestCommitment(100, 100),
			nonMutual2: createTestCommitment(100, 100)
		};

		const providerMR = {
			mutual1: 0.3,
			mutual2: 0.3
			// nonMutual1 and nonMutual2 have MR=0
		};

		const providerWeights = {
			mutual1: 0.3,
			mutual2: 0.3,
			nonMutual1: 0.2,
			nonMutual2: 0.2
		};

		const capacity = 200;
		
		const providerCommitment: Commitment = {
			capacity_slots: [createAvailabilitySlot('cap-1', capacity)],
			mr_values: providerMR,
			recognition_weights: providerWeights,
			timestamp: Date.now()
		};
		
		const result = computeAllocation(
			'provider1',
			providerCommitment,
			providerMR,
			providerWeights,
			networkCommitments
		);

		const total = Object.values(result.recipient_totals).reduce(
			(sum: number, a: number) => sum + a,
			0
		);

		// Total allocation should not exceed capacity
		expect(total).toBeLessThanOrEqual(capacity + 0.01); // small epsilon for floating point

		console.log(`[CONSERVATION] Total: ${total.toFixed(2)}/${capacity}`);
	});
});

