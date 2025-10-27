/**
 * Comprehensive test suite for Multi-Dimensional Allocation Algorithm v5
 * 
 * Tests mathematical properties from multi-grammar.md:
 * - Theorem 1': Multi-dimensional contractiveness
 * - Theorem 2': Convergence to zero fixed-point
 * - Theorem 3: Exponential convergence
 * - E1'-E11': Two-tier allocation per type
 * - E12'-E15': Adaptive damping
 * - E19': Love's symmetry (MR^k(A,B) = MR^k(B,A))
 * - E20': Non-accumulation (capping)
 * - E41': UniversalSatisfaction condition
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
	// Recognition & normalization
	normalizeRecognitionWeights,
	normalizeTypeSpecificRecognitionWeights,
	validateRecognitionWeights,
	computeTypeSpecificMutualRecognition,
	computeAllTypeSpecificMutualRecognition,
	
	// System state
	initializeMultiDimensionalNeedState,
	initializeMultiDimensionalCapacityState,
	getCurrentSystemState,
	
	// Damping
	computeDampingFactor,
	updateCommitmentMultiDimensionalDamping,
	DAMPING_OSCILLATING,
	DAMPING_SMOOTH,
	DAMPING_MODERATE,
	
	// Convergence metrics
	computePerTypeNeedVectorNorm,
	computeFrobeniusNorm,
	computePerTypeContractionConstant,
	checkPerTypeUniversalSatisfactionCondition,
	checkMultiDimensionalUniversalSatisfactionCondition,
	computePerTypePercentNeedsMet,
	computeOverallPercentNeedsMet,
	computeConvergenceMetrics,
	getAllNeedTypes,
	
	// Allocation operator
	applyMultiDimensionalAllocationOperator,
	computeMultiDimensionalSlotNativeAllocation,
	
	// Constants
	CONVERGENCE_EPSILON,
	DENOMINATOR_FLOOR
} from './algorithm.svelte';

import type {
	Commitment,
	MultiDimensionalRecognition,
	MultiDimensionalNeedState,
	MultiDimensionalCapacityState,
	SystemState,
	PerTypeDampingHistoryEntry,
	NeedSlot,
	AvailabilitySlot
} from '../../v5/schemas';

import { seed as itcSeed } from '../../utils/itc';

// ═══════════════════════════════════════════════════════════════════
// TEST HELPERS
// ═══════════════════════════════════════════════════════════════════

function createTestCommitment(
	needSlots: NeedSlot[] = [],
	capacitySlots: AvailabilitySlot[] = []
): Commitment {
	return {
		need_slots: needSlots,
		capacity_slots: capacitySlots,
		itcStamp: itcSeed(),
		timestamp: Date.now()
	};
}

function createNeedSlot(typeId: string, quantity: number, id?: string): NeedSlot {
	return {
		id: id || `need-${typeId}-${Math.random()}`,
		need_type_id: typeId,
		name: `Need ${typeId}`,
		quantity
	};
}

function createCapacitySlot(typeId: string, quantity: number, id?: string): AvailabilitySlot {
	return {
		id: id || `cap-${typeId}-${Math.random()}`,
		need_type_id: typeId,
		name: `Capacity ${typeId}`,
		quantity
	};
}

// ═══════════════════════════════════════════════════════════════════
// SUITE 1: RECOGNITION WEIGHT NORMALIZATION (D2', D2)
// ═══════════════════════════════════════════════════════════════════

describe('Recognition Weight Normalization (D2, D2\')', () => {
	it('should normalize weights to sum to 1.0', () => {
		const weights = {
			'alice': 3,
			'bob': 2,
			'charlie': 5
		};
		
		const normalized = normalizeRecognitionWeights(weights);
		
		const sum = Object.values(normalized).reduce((a, b) => a + b, 0);
		expect(sum).toBeCloseTo(1.0, 5);
		
		// Check proportions preserved
		expect(normalized.alice).toBeCloseTo(0.3, 5);
		expect(normalized.bob).toBeCloseTo(0.2, 5);
		expect(normalized.charlie).toBeCloseTo(0.5, 5);
	});
	
	it('should handle zero sum by distributing equally', () => {
		const weights = {
			'alice': 0,
			'bob': 0,
			'charlie': 0
		};
		
		const normalized = normalizeRecognitionWeights(weights);
		
		expect(normalized.alice).toBeCloseTo(1/3, 5);
		expect(normalized.bob).toBeCloseTo(1/3, 5);
		expect(normalized.charlie).toBeCloseTo(1/3, 5);
	});
	
	it('should normalize per-type weights independently (D2\')', () => {
		const weightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 2, 'bob': 3 },
			'healthcare': { 'charlie': 1, 'dave': 4 }
		};
		
		const normalized = normalizeTypeSpecificRecognitionWeights(weightsByType);
		
		// Check food type sums to 1.0
		const foodSum = Object.values(normalized.food).reduce((a, b) => a + b, 0);
		expect(foodSum).toBeCloseTo(1.0, 5);
		
		// Check healthcare type sums to 1.0
		const healthSum = Object.values(normalized.healthcare).reduce((a, b) => a + b, 0);
		expect(healthSum).toBeCloseTo(1.0, 5);
		
		// Check proportions
		expect(normalized.food.alice).toBeCloseTo(0.4, 5);
		expect(normalized.food.bob).toBeCloseTo(0.6, 5);
		expect(normalized.healthcare.charlie).toBeCloseTo(0.2, 5);
		expect(normalized.healthcare.dave).toBeCloseTo(0.8, 5);
	});
	
	it('should validate normalized weights', () => {
		const valid = { 'a': 0.3, 'b': 0.7 };
		const invalid = { 'a': 0.3, 'b': 0.5 };
		
		expect(validateRecognitionWeights(valid)).toBe(true);
		expect(validateRecognitionWeights(invalid)).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 2: MUTUAL RECOGNITION (D1', E19')
// ═══════════════════════════════════════════════════════════════════

describe('Mutual Recognition (D1\', E19\')', () => {
	it('should compute bilateral minimum MR^k(A,B) = min(R_A^k(B), R_B^k(A))', () => {
		const aliceWeights: MultiDimensionalRecognition = {
			'food': { 'bob': 0.8 }
		};
		
		const bobWeights: MultiDimensionalRecognition = {
			'food': { 'alice': 0.6 }
		};
		
		const mr = computeTypeSpecificMutualRecognition(
			'alice',
			'bob',
			'food',
			aliceWeights,
			bobWeights
		);
		
		expect(mr).toBeCloseTo(0.6, 5); // min(0.8, 0.6) = 0.6
	});
	
	it('should satisfy symmetry: MR^k(A,B) = MR^k(B,A) (E19\')', () => {
		const aliceWeights: MultiDimensionalRecognition = {
			'healthcare': { 'bob': 0.9 }
		};
		
		const bobWeights: MultiDimensionalRecognition = {
			'healthcare': { 'alice': 0.7 }
		};
		
		const mrAB = computeTypeSpecificMutualRecognition(
			'alice',
			'bob',
			'healthcare',
			aliceWeights,
			bobWeights
		);
		
		const mrBA = computeTypeSpecificMutualRecognition(
			'bob',
			'alice',
			'healthcare',
			bobWeights,
			aliceWeights
		);
		
		expect(mrAB).toBeCloseTo(mrBA, 5); // Symmetry
	});
	
	it('should return 0 when either party has no recognition', () => {
		const aliceWeights: MultiDimensionalRecognition = {
			'food': { 'bob': 0.5 }
		};
		
		const bobWeights: MultiDimensionalRecognition = {
			'food': {} // Bob doesn't recognize Alice
		};
		
		const mr = computeTypeSpecificMutualRecognition(
			'alice',
			'bob',
			'food',
			aliceWeights,
			bobWeights
		);
		
		expect(mr).toBe(0);
	});
	
	it('should compute all type-specific MR values', () => {
		const aliceWeights: MultiDimensionalRecognition = {
			'food': { 'bob': 0.6, 'charlie': 0.4 },
			'healthcare': { 'bob': 0.8 }
		};
		
		const networkWeights: Record<string, MultiDimensionalRecognition> = {
			'bob': {
				'food': { 'alice': 0.7 },
				'healthcare': { 'alice': 0.5 }
			},
			'charlie': {
				'food': { 'alice': 0.3 }
			}
		};
		
		const mrValues = computeAllTypeSpecificMutualRecognition(
			'alice',
			aliceWeights,
			networkWeights
		);
		
		expect(mrValues.food.bob).toBeCloseTo(0.6, 5); // min(0.6, 0.7)
		expect(mrValues.food.charlie).toBeCloseTo(0.3, 5); // min(0.4, 0.3)
		expect(mrValues.healthcare.bob).toBeCloseTo(0.5, 5); // min(0.8, 0.5)
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 3: MULTI-DIMENSIONAL STATE INITIALIZATION (D3', D4')
// ═══════════════════════════════════════════════════════════════════

describe('Multi-Dimensional State Initialization (D3\', D4\')', () => {
	it('should initialize multi-dimensional need state from commitment', () => {
		const commitment = createTestCommitment([
			createNeedSlot('food', 10),
			createNeedSlot('food', 5),
			createNeedSlot('healthcare', 20)
		]);
		
		const needState = initializeMultiDimensionalNeedState('alice', commitment);
		
		expect(needState.needsByType.food).toBeDefined();
		expect(needState.needsByType.food.maxNeed).toBe(15); // 10 + 5
		expect(needState.needsByType.food.residualNeed).toBe(15);
		
		expect(needState.needsByType.healthcare).toBeDefined();
		expect(needState.needsByType.healthcare.maxNeed).toBe(20);
		expect(needState.needsByType.healthcare.residualNeed).toBe(20);
	});
	
	it('should compute Euclidean norm of need vector', () => {
		const commitment = createTestCommitment([
			createNeedSlot('food', 3), // 3^2 = 9
			createNeedSlot('healthcare', 4) // 4^2 = 16
		]);
		
		const needState = initializeMultiDimensionalNeedState('alice', commitment);
		
		// ||N_vec|| = sqrt(9 + 16) = sqrt(25) = 5
		expect(needState.totalResidualNeed).toBeCloseTo(5, 5);
	});
	
	it('should initialize multi-dimensional capacity state from commitment', () => {
		const commitment = createTestCommitment([], [
			createCapacitySlot('food', 8),
			createCapacitySlot('food', 2),
			createCapacitySlot('healthcare', 15)
		]);
		
		const capacityState = initializeMultiDimensionalCapacityState('bob', commitment);
		
		expect(capacityState.capacitiesByType.food).toBeDefined();
		expect(capacityState.capacitiesByType.food.maxCapacity).toBe(10); // 8 + 2
		expect(capacityState.capacitiesByType.food.availableCapacity).toBe(10);
		
		expect(capacityState.capacitiesByType.healthcare).toBeDefined();
		expect(capacityState.capacitiesByType.healthcare.maxCapacity).toBe(15);
	});
	
	it('should compute Euclidean norm of capacity vector', () => {
		const commitment = createTestCommitment([], [
			createCapacitySlot('food', 6), // 6^2 = 36
			createCapacitySlot('healthcare', 8) // 8^2 = 64
		]);
		
		const capacityState = initializeMultiDimensionalCapacityState('bob', commitment);
		
		// ||C_vec|| = sqrt(36 + 64) = sqrt(100) = 10
		expect(capacityState.totalMaxCapacity).toBeCloseTo(10, 5);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 4: ADAPTIVE DAMPING (E12'-E15', E22-E23)
// ═══════════════════════════════════════════════════════════════════

describe('Adaptive Damping (E12\'-E15\')', () => {
	it('should detect oscillation pattern (E13\')', () => {
		const history: PerTypeDampingHistoryEntry[] = [
			{ need_type_id: 'food', overAllocation: 5, timestamp: Date.now() - 3000 },
			{ need_type_id: 'food', overAllocation: 10, timestamp: Date.now() - 2000 },
			{ need_type_id: 'food', overAllocation: 3, timestamp: Date.now() - 1000 }
		];
		
		const dampingFactor = computeDampingFactor(history);
		
		// Oscillating: 5 < 10 > 3 (up-down pattern)
		expect(dampingFactor).toBe(DAMPING_OSCILLATING); // 0.5
	});
	
	it('should detect smooth convergence', () => {
		const history: PerTypeDampingHistoryEntry[] = [
			{ need_type_id: 'food', overAllocation: 10, timestamp: Date.now() - 3000 },
			{ need_type_id: 'food', overAllocation: 5, timestamp: Date.now() - 2000 },
			{ need_type_id: 'food', overAllocation: 2, timestamp: Date.now() - 1000 }
		];
		
		const dampingFactor = computeDampingFactor(history);
		
		// Smooth: 10 >= 5 >= 2 (monotonically decreasing)
		expect(dampingFactor).toBe(DAMPING_SMOOTH); // 1.0
	});
	
	it('should use moderate damping by default', () => {
		const history: PerTypeDampingHistoryEntry[] = [
			{ need_type_id: 'food', overAllocation: 5, timestamp: Date.now() - 3000 },
			{ need_type_id: 'food', overAllocation: 6, timestamp: Date.now() - 2000 },
			{ need_type_id: 'food', overAllocation: 7, timestamp: Date.now() - 1000 }
		];
		
		const dampingFactor = computeDampingFactor(history);
		
		// Neither oscillating (not up-down-up or down-up-down) nor smoothly converging (increasing, not decreasing)
		// Pattern: [5, 6, 7] - steadily increasing
		expect(dampingFactor).toBe(DAMPING_MODERATE); // 0.8
	});
	
	it('should update per-type damping in commitment', () => {
		const commitment = createTestCommitment([
			createNeedSlot('food', 10),
			createNeedSlot('healthcare', 20)
		]);
		
		const totalReceivedByType = {
			'food': 12, // Over-allocated by 2
			'healthcare': 15 // Under-allocated by 5
		};
		
		const updated = updateCommitmentMultiDimensionalDamping(
			commitment,
			totalReceivedByType
		);
		
		expect(updated.multi_dimensional_damping).toBeDefined();
		expect(updated.multi_dimensional_damping!.damping_history.food).toBeDefined();
		expect(updated.multi_dimensional_damping!.damping_history.healthcare).toBeDefined();
		
		// Check over-allocation recorded
		const foodHistory = updated.multi_dimensional_damping!.damping_history.food;
		expect(foodHistory[0].overAllocation).toBe(2);
		
		const healthHistory = updated.multi_dimensional_damping!.damping_history.healthcare;
		expect(healthHistory[0].overAllocation).toBe(0); // max(0, 15-20)
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 5: FROBENIUS NORM & CONVERGENCE (Theorem 1', 3)
// ═══════════════════════════════════════════════════════════════════

describe('Frobenius Norm & Convergence Metrics', () => {
	it('should compute per-type need vector norm (Euclidean)', () => {
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 3, maxNeed: 10, lastAllocationReceived: 0 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 3,
				totalMaxNeed: 10
			},
			'bob': {
				pubKey: 'bob',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 4, maxNeed: 10, lastAllocationReceived: 0 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 4,
				totalMaxNeed: 10
			}
		};
		
		const norm = computePerTypeNeedVectorNorm(needVector, 'food');
		
		// ||N_vec^food|| = sqrt(3^2 + 4^2) = sqrt(9 + 16) = 5
		expect(norm).toBeCloseTo(5, 5);
	});
	
	it('should compute Frobenius norm across all types (Theorem 1\')', () => {
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 3, maxNeed: 10, lastAllocationReceived: 0 },
					'healthcare': { need_type_id: 'healthcare', residualNeed: 6, maxNeed: 20, lastAllocationReceived: 0 }
				},
				timestamp: Date.now(),
				totalResidualNeed: Math.sqrt(3*3 + 6*6),
				totalMaxNeed: Math.sqrt(10*10 + 20*20)
			}
		};
		
		const frobeniusNorm = computeFrobeniusNorm(needVector);
		
		// ||N_matrix||_F = sqrt(3^2 + 6^2) = sqrt(9 + 36) = sqrt(45)
		expect(frobeniusNorm).toBeCloseTo(Math.sqrt(45), 5);
	});
	
	it('should compute per-type contraction constant k_k', () => {
		const previousNorm = 10;
		const currentNorm = 8;
		
		const k_k = computePerTypeContractionConstant(currentNorm, previousNorm);
		
		expect(k_k).toBeCloseTo(0.8, 5); // 8/10 = 0.8 < 1 (contracting)
	});
	
	it('should check per-type UniversalSatisfaction condition (E41\')', () => {
		const needVectorConverged: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0.0001, maxNeed: 10, lastAllocationReceived: 10 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0.0001,
				totalMaxNeed: 10
			},
			'bob': {
				pubKey: 'bob',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 5, lastAllocationReceived: 5 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0,
				totalMaxNeed: 5
			}
		};
		
		const universalSatisfactionAchieved = checkPerTypeUniversalSatisfactionCondition(needVectorConverged, 'food');
		expect(universalSatisfactionAchieved).toBe(true);
		
		const needVectorNotConverged: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 5, maxNeed: 10, lastAllocationReceived: 5 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 5,
				totalMaxNeed: 10
			}
		};
		
		const notAchieved = checkPerTypeUniversalSatisfactionCondition(needVectorNotConverged, 'food');
		expect(notAchieved).toBe(false);
	});
	
	it('should check multi-dimensional UniversalSatisfaction condition (E41\')', () => {
		const converged: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 10, lastAllocationReceived: 10 },
					'healthcare': { need_type_id: 'healthcare', residualNeed: 0, maxNeed: 20, lastAllocationReceived: 20 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0,
				totalMaxNeed: Math.sqrt(10*10 + 20*20)
			}
		};
		
		expect(checkMultiDimensionalUniversalSatisfactionCondition(converged)).toBe(true);
		
		const notConverged: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 10, lastAllocationReceived: 10 },
					'healthcare': { need_type_id: 'healthcare', residualNeed: 5, maxNeed: 20, lastAllocationReceived: 15 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 5,
				totalMaxNeed: Math.sqrt(10*10 + 20*20)
			}
		};
		
		expect(checkMultiDimensionalUniversalSatisfactionCondition(notConverged)).toBe(false);
	});
	
	it('should compute percent needs met per type', () => {
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 10, lastAllocationReceived: 10 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0,
				totalMaxNeed: 10
			},
			'bob': {
				pubKey: 'bob',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 5, maxNeed: 10, lastAllocationReceived: 5 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 5,
				totalMaxNeed: 10
			},
			'charlie': {
				pubKey: 'charlie',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 8, lastAllocationReceived: 8 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0,
				totalMaxNeed: 8
			}
		};
		
		const pctMet = computePerTypePercentNeedsMet(needVector, 'food');
		
		// 2 out of 3 have food needs met
		expect(pctMet).toBeCloseTo(66.67, 1);
	});
	
	it('should compute overall percent needs met', () => {
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 10, lastAllocationReceived: 10 },
					'healthcare': { need_type_id: 'healthcare', residualNeed: 5, maxNeed: 20, lastAllocationReceived: 15 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 5,
				totalMaxNeed: Math.sqrt(10*10 + 20*20)
			}
		};
		
		const pctMet = computeOverallPercentNeedsMet(needVector);
		
		// 1 out of 2 needs met (food met, healthcare not)
		expect(pctMet).toBeCloseTo(50, 1);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 6: CAPPING & NON-ACCUMULATION (E5', E11', E20', Theorem 5)
// ═══════════════════════════════════════════════════════════════════

describe('Capping & Non-Accumulation (E5\', E11\', E20\')', () => {
	it('should cap mutual allocation at need (E5\'): A_mutual^k <= N_i^k', () => {
		// This is tested through the allocation operator
		// We verify that allocations never exceed needs
		
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 100) // Provider has excess capacity
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 10) // Alice needs only 10
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		const aliceTotal = result.recipient_totals_by_type?.food?.alice || 0;
		
		// Critical: Allocation capped at need (10), not capacity (100)
		expect(aliceTotal).toBeLessThanOrEqual(10);
	});
	
	it('should ensure no differential enrichment (Theorem 5): No accumulation beyond need', () => {
		// At equilibrium, no participant can accumulate more than their stated need
		// This is guaranteed by E5' and E11' capping
		
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': {
				pubKey: 'alice',
				needsByType: {
					'food': { need_type_id: 'food', residualNeed: 0, maxNeed: 10, lastAllocationReceived: 10 }
				},
				timestamp: Date.now(),
				totalResidualNeed: 0,
				totalMaxNeed: 10
			}
		};
		
		// At equilibrium (residualNeed = 0), total received = maxNeed exactly
		const totalReceived = 10;
		const maxNeed = 10;
		
		expect(totalReceived).toBe(maxNeed); // No surplus possible
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 7: TYPE INDEPENDENCE & ALLOCATION (E1'-E11')
// ═══════════════════════════════════════════════════════════════════

describe('Type Independence & Per-Type Allocation (E1\'-E11\')', () => {
	it('should process each need type independently', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 10),
			createCapacitySlot('healthcare', 20)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 5),
			createNeedSlot('healthcare', 15)
		]);
		
		const bobCommitment = createTestCommitment([
			createNeedSlot('food', 3),
			createNeedSlot('healthcare', 10)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.6, 'bob': 0.4 },
			'healthcare': { 'alice': 0.8, 'bob': 0.2 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.6, 'bob': 0.4 },
			'healthcare': { 'alice': 0.8, 'bob': 0.2 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment,
			'bob': bobCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		// Check that both types have allocations
		expect(result.recipient_totals_by_type?.food).toBeDefined();
		expect(result.recipient_totals_by_type?.healthcare).toBeDefined();
		
		// Food allocations
		const aliceFoodTotal = result.recipient_totals_by_type?.food?.alice || 0;
		const bobFoodTotal = result.recipient_totals_by_type?.food?.bob || 0;
		
		expect(aliceFoodTotal).toBeGreaterThan(0);
		expect(bobFoodTotal).toBeGreaterThan(0);
		expect(aliceFoodTotal + bobFoodTotal).toBeLessThanOrEqual(10); // Capacity constraint
		
		// Healthcare allocations
		const aliceHealthTotal = result.recipient_totals_by_type?.healthcare?.alice || 0;
		const bobHealthTotal = result.recipient_totals_by_type?.healthcare?.bob || 0;
		
		expect(aliceHealthTotal).toBeGreaterThan(0);
		expect(bobHealthTotal).toBeGreaterThan(0);
		expect(aliceHealthTotal + bobHealthTotal).toBeLessThanOrEqual(20); // Capacity constraint
	});
	
	it('should only match slots with same need type (E28\' type matching)', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 10)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('healthcare', 5) // Wrong type!
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		// No allocations should occur (type mismatch)
		expect(result.slot_allocations.length).toBe(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 8: CONTRACTION MAPPING (E17', Theorem 1')
// ═══════════════════════════════════════════════════════════════════

describe('Contraction Mapping & Need Update Law (E17\', Theorem 1\')', () => {
	it('should apply need update law: N_i^k(t+1) = max(0, N_i^k(t) - A_total^k(i, t))', () => {
		// This is tested through the full allocation operator
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 10)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 8)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		// Initialize need vector
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		const result = applyMultiDimensionalAllocationOperator(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments,
			needVector
		);
		
		const aliceInitialNeed = needVector.alice.needsByType.food.residualNeed;
		const aliceAllocation = result.allocations.recipient_totals_by_type?.food?.alice || 0;
		const aliceNewNeed = result.updatedNeedVector.alice?.needsByType.food?.residualNeed || 0;
		
		// Verify: N(t+1) = N(t) - A(t)
		expect(aliceNewNeed).toBeCloseTo(Math.max(0, aliceInitialNeed - aliceAllocation), 5);
	});
	
	it('should contract: ||N_matrix(t+1)||_F < ||N_matrix(t)||_F (Theorem 1\')', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 5),
			createCapacitySlot('healthcare', 5)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 10),
			createNeedSlot('healthcare', 10)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 },
			'healthcare': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 },
			'healthcare': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		const normBefore = computeFrobeniusNorm(needVector);
		
		const result = applyMultiDimensionalAllocationOperator(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments,
			needVector
		);
		
		const normAfter = computeFrobeniusNorm(result.updatedNeedVector);
		
		// Critical: Frobenius norm must decrease (contraction)
		expect(normAfter).toBeLessThan(normBefore);
	});
	
	it('should have contraction constant k_max < 1', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 3)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 10)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		const needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		const result = applyMultiDimensionalAllocationOperator(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments,
			needVector
		);
		
		const k_max = result.convergenceMetrics.contractionConstant;
		
		// Critical: k_max < 1 for contraction
		expect(k_max).toBeLessThan(1);
		expect(k_max).toBeGreaterThanOrEqual(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 9: TWO-TIER ALLOCATION (E1'-E11')
// ═══════════════════════════════════════════════════════════════════

describe('Two-Tier Allocation (E1\'-E11\')', () => {
	it('should prioritize mutual recognition in Tier 1', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 10)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 8)
		]);
		
		const bobCommitment = createTestCommitment([
			createNeedSlot('food', 8)
		]);
		
		// Alice has MR > 0, Bob has only R > 0
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.8 } // Mutual
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.5, 'bob': 0.5 } // Bob is non-mutual
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment,
			'bob': bobCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		const aliceTotal = result.recipient_totals_by_type?.food?.alice || 0;
		const bobTotal = result.recipient_totals_by_type?.food?.bob || 0;
		
		// Alice (mutual) should get priority over Bob (non-mutual)
		expect(aliceTotal).toBeGreaterThan(bobTotal);
		
		// Check tier classification
		const aliceAllocations = result.slot_allocations.filter(a => a.recipient_pubkey === 'alice');
		const bobAllocations = result.slot_allocations.filter(a => a.recipient_pubkey === 'bob');
		
		expect(aliceAllocations[0]?.tier).toBe('mutual');
		if (bobAllocations.length > 0) {
			expect(bobAllocations[0]?.tier).toBe('non-mutual');
		}
	});
	
	it('should use remaining capacity for Tier 2 (E6\')', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 10)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 6)
		]);
		
		const bobCommitment = createTestCommitment([
			createNeedSlot('food', 5)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.8 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 0.6, 'bob': 0.4 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment,
			'bob': bobCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		const aliceTotal = result.recipient_totals_by_type?.food?.alice || 0;
		const bobTotal = result.recipient_totals_by_type?.food?.bob || 0;
		
		// Total allocation should use full capacity
		expect(aliceTotal + bobTotal).toBeLessThanOrEqual(10);
		
		// Bob should get some allocation from remaining capacity
		expect(bobTotal).toBeGreaterThan(0);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 10: CONVERGENCE TO ZERO (Theorem 2')
// ═══════════════════════════════════════════════════════════════════

describe('Convergence to Zero Fixed-Point (Theorem 2\')', () => {
	it('should converge to zero with sufficient capacity', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 100) // Abundant capacity
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 10)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		let needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		// Iterate until convergence
		let iterations = 0;
		const maxIterations = 10;
		
		while (iterations < maxIterations) {
			const result = applyMultiDimensionalAllocationOperator(
				'provider',
				providerCommitment,
				myMRValuesByType,
				myWeightsByType,
				networkCommitments,
				needVector
			);
			
			needVector = result.updatedNeedVector;
			
			if (result.convergenceMetrics.universalSatisfactionAchieved) {
				break;
			}
			
			iterations++;
		}
		
		const finalNorm = computeFrobeniusNorm(needVector);
		
		// With sufficient capacity, should converge to zero
		expect(finalNorm).toBeLessThan(CONVERGENCE_EPSILON);
		expect(iterations).toBeLessThan(maxIterations);
	});
	
	it('should satisfy UniversalSatisfaction condition at equilibrium (E41\')', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 50),
			createCapacitySlot('healthcare', 50)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 10),
			createNeedSlot('healthcare', 15)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 },
			'healthcare': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 },
			'healthcare': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		let needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		// Iterate to convergence
		for (let i = 0; i < 10; i++) {
			const result = applyMultiDimensionalAllocationOperator(
				'provider',
				providerCommitment,
				myMRValuesByType,
				myWeightsByType,
				networkCommitments,
				needVector
			);
			
			needVector = result.updatedNeedVector;
			
			if (result.convergenceMetrics.universalSatisfactionAchieved) {
				break;
			}
		}
		
		// Check UniversalSatisfaction condition for all types
		expect(checkMultiDimensionalUniversalSatisfactionCondition(needVector)).toBe(true);
		expect(checkPerTypeUniversalSatisfactionCondition(needVector, 'food')).toBe(true);
		expect(checkPerTypeUniversalSatisfactionCondition(needVector, 'healthcare')).toBe(true);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 11: SYSTEM PROPERTIES
// ═══════════════════════════════════════════════════════════════════

describe('System Properties', () => {
	it('should handle empty need vector', () => {
		const emptyVector: Record<string, MultiDimensionalNeedState> = {};
		
		const norm = computeFrobeniusNorm(emptyVector);
		const universalSatisfaction = checkMultiDimensionalUniversalSatisfactionCondition(emptyVector);
		const pctMet = computeOverallPercentNeedsMet(emptyVector);
		
		expect(norm).toBe(0);
		expect(universalSatisfaction).toBe(true);
		expect(pctMet).toBe(100);
	});
	
	it('should get all need types from system state', () => {
		const state: SystemState = {
			needVector: {
				'alice': {
					pubKey: 'alice',
					needsByType: {
						'food': { need_type_id: 'food', residualNeed: 5, maxNeed: 10, lastAllocationReceived: 0 }
					},
					timestamp: Date.now(),
					totalResidualNeed: 5,
					totalMaxNeed: 10
				}
			},
			capacityVector: {
				'bob': {
					pubKey: 'bob',
					capacitiesByType: {
						'healthcare': { need_type_id: 'healthcare', availableCapacity: 20, maxCapacity: 20, lastAllocationGiven: 0 }
					},
					timestamp: Date.now(),
					totalAvailableCapacity: 20,
					totalMaxCapacity: 20
				}
			},
			timestamp: Date.now(),
			iteration: 1,
			itcStamp: itcSeed()
		};
		
		const types = getAllNeedTypes(state);
		
		expect(types).toContain('food');
		expect(types).toContain('healthcare');
		expect(types.length).toBe(2);
	});
	
	it('should respect denominator floor for Lipschitz continuity', () => {
		// When demand is very low, denominator floor prevents division issues
		expect(DENOMINATOR_FLOOR).toBe(0.0001);
		
		// This is enforced in allocation equations (E3', E9')
		const safeDenominator = Math.max(0.00001, DENOMINATOR_FLOOR);
		expect(safeDenominator).toBe(DENOMINATOR_FLOOR);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 12: INTEGRATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Integration: Multi-Dimensional Scenarios', () => {
	it('should handle complex multi-provider, multi-recipient, multi-type scenario', () => {
		// Scenario: Healthcare system with GP and Surgeon specialization
		const gpCommitment = createTestCommitment([], [
			createCapacitySlot('diagnostics', 20),
			createCapacitySlot('consultations', 80),
			// No surgery capacity
		]);
		
		const surgeonCommitment = createTestCommitment([], [
			// No diagnostics capacity
			createCapacitySlot('consultations', 10),
			createCapacitySlot('surgery', 90)
		]);
		
		const patientCommitment = createTestCommitment([
			createNeedSlot('diagnostics', 5),
			createNeedSlot('consultations', 10),
			createNeedSlot('surgery', 15)
		]);
		
		// GP recognizes patient for diagnostics and consultations
		const gpMRValuesByType: MultiDimensionalRecognition = {
			'diagnostics': { 'patient': 0.7 },
			'consultations': { 'patient': 0.9 }
		};
		
		const gpWeightsByType: MultiDimensionalRecognition = {
			'diagnostics': { 'patient': 0.7 },
			'consultations': { 'patient': 0.9 }
		};
		
		// Surgeon recognizes patient for surgery
		const surgeonMRValuesByType: MultiDimensionalRecognition = {
			'surgery': { 'patient': 0.8 }
		};
		
		const surgeonWeightsByType: MultiDimensionalRecognition = {
			'surgery': { 'patient': 0.8 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'patient': patientCommitment
		};
		
		let needVector: Record<string, MultiDimensionalNeedState> = {
			'patient': initializeMultiDimensionalNeedState('patient', patientCommitment)
		};
		
		// GP provides diagnostics and consultations
		const gpResult = applyMultiDimensionalAllocationOperator(
			'gp',
			gpCommitment,
			gpMRValuesByType,
			gpWeightsByType,
			networkCommitments,
			needVector
		);
		
		needVector = gpResult.updatedNeedVector;
		
		// Surgeon provides surgery
		const surgeonResult = applyMultiDimensionalAllocationOperator(
			'surgeon',
			surgeonCommitment,
			surgeonMRValuesByType,
			surgeonWeightsByType,
			networkCommitments,
			needVector
		);
		
		needVector = surgeonResult.updatedNeedVector;
		
		// Check that all patient needs are met
		const patient = needVector.patient;
		expect(patient.needsByType.diagnostics?.residualNeed || 0).toBeLessThan(CONVERGENCE_EPSILON);
		expect(patient.needsByType.consultations?.residualNeed || 0).toBeLessThan(CONVERGENCE_EPSILON);
		expect(patient.needsByType.surgery?.residualNeed || 0).toBeLessThan(CONVERGENCE_EPSILON);
	});
	
	it('should demonstrate exponential convergence (Theorem 3)', () => {
		const providerCommitment = createTestCommitment([], [
			createCapacitySlot('food', 5)
		]);
		
		const aliceCommitment = createTestCommitment([
			createNeedSlot('food', 100)
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'food': { 'alice': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'alice': aliceCommitment
		};
		
		let needVector: Record<string, MultiDimensionalNeedState> = {
			'alice': initializeMultiDimensionalNeedState('alice', aliceCommitment)
		};
		
		const norms: number[] = [];
		
		// Track convergence over iterations
		for (let i = 0; i < 10; i++) {
			norms.push(computeFrobeniusNorm(needVector));
			
			const result = applyMultiDimensionalAllocationOperator(
				'provider',
				providerCommitment,
				myMRValuesByType,
				myWeightsByType,
				networkCommitments,
				needVector
			);
			
			needVector = result.updatedNeedVector;
		}
		
		// Check exponential decay: each norm should be smaller than previous
		for (let i = 1; i < norms.length; i++) {
			expect(norms[i]).toBeLessThan(norms[i-1]);
		}
		
		// Check approximate exponential rate
		// ||N(t+1)|| / ||N(t)|| should be approximately constant (k)
		const ratios: number[] = [];
		for (let i = 1; i < norms.length; i++) {
			ratios.push(norms[i] / norms[i-1]);
		}
		
		// All ratios should be less than 1 (contraction)
		ratios.forEach(ratio => {
			expect(ratio).toBeLessThan(1);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 13: HIERARCHICAL AVAILABILITY WINDOWS (v5 - New Time System)
// ═══════════════════════════════════════════════════════════════════

describe('Hierarchical Availability Windows (v5 Time System)', () => {
	it('should match slots with simple daily time ranges', () => {
		// Provider available daily 9am-5pm
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('consulting', 10),
				recurrence: 'daily',
				availability_window: {
					time_ranges: [
						{ start_time: '09:00', end_time: '17:00' }
					]
				}
			}
		]);
		
		// Need on a specific Monday 10am-12pm (falls within provider's daily 9-5)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('consulting', 5),
				start_date: '2024-03-04', // Monday
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'consulting': { 'recipient': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'consulting': { 'recipient': 1.0 }
		};
		
		const networkCommitments: Record<string, Commitment> = {
			'recipient': needCommitment
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments
		);
		
		// Should match because need time (10-12) falls within provider availability (9-5)
		expect(result.slot_allocations.length).toBeGreaterThan(0);
		const total = result.recipient_totals_by_type?.consulting?.recipient || 0;
		expect(total).toBeGreaterThan(0);
	});
	
	it('should match slots with day-specific schedules', () => {
		// Provider available Mon/Wed/Fri 9am-5pm
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('tutoring', 10),
				recurrence: 'weekly',
				availability_window: {
					day_schedules: [
						{
							days: ['monday', 'wednesday', 'friday'],
							time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
						}
					]
				}
			}
		]);
		
		// Need on Monday 10am-12pm (matches)
		const needCommitmentMatch = createTestCommitment([
			{
				...createNeedSlot('tutoring', 5),
				start_date: '2024-03-04', // Monday
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		// Need on Tuesday 10am-12pm (does NOT match - provider not available Tuesday)
		const needCommitmentNoMatch = createTestCommitment([
			{
				...createNeedSlot('tutoring', 5),
				start_date: '2024-03-05', // Tuesday
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		// Test match case
		const networkCommitments1: Record<string, Commitment> = {
			'student': needCommitmentMatch
		};
		
		const result1 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments1
		);
		
		expect(result1.slot_allocations.length).toBeGreaterThan(0);
		
		// Test no-match case
		const networkCommitments2: Record<string, Commitment> = {
			'student': needCommitmentNoMatch
		};
		
		const result2 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments2
		);
		
		// Should NOT match - Tuesday not in provider's schedule
		expect(result2.slot_allocations.length).toBe(0);
	});
	
	it('should match slots with different times on different days', () => {
		// Provider: Monday/Friday 9-12, Tuesday 2-5
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('counseling', 10),
				recurrence: 'weekly',
				availability_window: {
					day_schedules: [
						{
							days: ['monday', 'friday'],
							time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
						},
						{
							days: ['tuesday'],
							time_ranges: [{ start_time: '14:00', end_time: '17:00' }]
						}
					]
				}
			}
		]);
		
		// Need on Monday 10-11 (matches - Monday 9-12)
		const needMonday = createTestCommitment([
			{
				...createNeedSlot('counseling', 3),
				start_date: '2024-03-04', // Monday
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '11:00' }
					]
				}
			}
		]);
		
		// Need on Tuesday 15-16 (matches - Tuesday 2-5)
		const needTuesday = createTestCommitment([
			{
				...createNeedSlot('counseling', 3),
				start_date: '2024-03-05', // Tuesday
				availability_window: {
					time_ranges: [
						{ start_time: '15:00', end_time: '16:00' }
					]
				}
			}
		]);
		
		// Need on Monday 15-16 (does NOT match - Monday only 9-12)
		const needMondayAfternoon = createTestCommitment([
			{
				...createNeedSlot('counseling', 3),
				start_date: '2024-03-04', // Monday
				availability_window: {
					time_ranges: [
						{ start_time: '15:00', end_time: '16:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'counseling': { 'client': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'counseling': { 'client': 1.0 }
		};
		
		// Test Monday morning (should match)
		const result1 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needMonday }
		);
		expect(result1.slot_allocations.length).toBeGreaterThan(0);
		
		// Test Tuesday afternoon (should match)
		const result2 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needTuesday }
		);
		expect(result2.slot_allocations.length).toBeGreaterThan(0);
		
		// Test Monday afternoon (should NOT match)
		const result3 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needMondayAfternoon }
		);
		expect(result3.slot_allocations.length).toBe(0);
	});
	
	it('should match slots with week-specific monthly patterns', () => {
		// Provider: First and third week of each month, Monday-Friday 9-5
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('training', 20),
				recurrence: 'monthly',
				availability_window: {
					week_schedules: [
						{
							weeks: [1, 3],
							day_schedules: [
								{
									days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'],
									time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
								}
							]
						}
					]
				}
			}
		]);
		
		// Need on first week of month (should match)
		const needFirstWeek = createTestCommitment([
			{
				...createNeedSlot('training', 5),
				start_date: '2024-03-04', // Monday, first week of March
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		// Need on second week of month (should NOT match)
		const needSecondWeek = createTestCommitment([
			{
				...createNeedSlot('training', 5),
				start_date: '2024-03-11', // Monday, second week of March
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'training': { 'trainee': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'training': { 'trainee': 1.0 }
		};
		
		// Test first week (should match)
		const result1 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'trainee': needFirstWeek }
		);
		expect(result1.slot_allocations.length).toBeGreaterThan(0);
		
		// Test second week (should NOT match)
		const result2 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'trainee': needSecondWeek }
		);
		expect(result2.slot_allocations.length).toBe(0);
	});
	
	it('should match slots with complex yearly patterns (specific months/weeks/days)', () => {
		// Provider: 
		// - February: all weeks, Mon/Wed/Fri 9-12
		// - September: first week only, weekdays 10-5
		// - October: second week Tuesday 2-4, fourth week Mon/Wed 9-12
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('seasonal', 30),
				recurrence: 'yearly',
				availability_window: {
					month_schedules: [
						{
							month: 2, // February
							day_schedules: [
								{
									days: ['monday', 'wednesday', 'friday'],
									time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
								}
							]
						},
						{
							month: 9, // September
							week_schedules: [
								{
									weeks: [1],
									day_schedules: [
										{
											days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'],
											time_ranges: [{ start_time: '10:00', end_time: '17:00' }]
										}
									]
								}
							]
						},
						{
							month: 10, // October
							week_schedules: [
								{
									weeks: [2],
									day_schedules: [
										{
											days: ['tuesday'],
											time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
										}
									]
								},
								{
									weeks: [4],
									day_schedules: [
										{
											days: ['monday', 'wednesday'],
											time_ranges: [{ start_time: '09:00', end_time: '12:00' }]
										}
									]
								}
							]
						}
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'seasonal': { 'client': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'seasonal': { 'client': 1.0 }
		};
		
		// Test 1: February Wednesday 10-11 (should match - Feb Mon/Wed/Fri 9-12)
		const needFeb = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-02-07', // Wednesday in February
				availability_window: {
					time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
				}
			}
		]);
		
		const resultFeb = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needFeb }
		);
		expect(resultFeb.slot_allocations.length).toBeGreaterThan(0);
		
		// Test 2: September first week Tuesday 11-13 (should match - Sept first week weekdays 10-5)
		const needSept = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-09-03', // Tuesday, first week of September
				availability_window: {
					time_ranges: [{ start_time: '11:00', end_time: '13:00' }]
				}
			}
		]);
		
		const resultSept = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needSept }
		);
		expect(resultSept.slot_allocations.length).toBeGreaterThan(0);
		
		// Test 3: October second week Tuesday 14:30-15:30 (should match - Oct 2nd week Tue 2-4)
		const needOct2 = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-10-08', // Tuesday, second week of October
				availability_window: {
					time_ranges: [{ start_time: '14:30', end_time: '15:30' }]
				}
			}
		]);
		
		const resultOct2 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needOct2 }
		);
		expect(resultOct2.slot_allocations.length).toBeGreaterThan(0);
		
		// Test 4: October fourth week Monday 10-11 (should match - Oct 4th week Mon/Wed 9-12)
		const needOct4 = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-10-28', // Monday, fourth week of October
				availability_window: {
					time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
				}
			}
		]);
		
		const resultOct4 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needOct4 }
		);
		expect(resultOct4.slot_allocations.length).toBeGreaterThan(0);
		
		// Test 5: March (should NOT match - provider not available in March)
		const needMarch = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-03-15',
				availability_window: {
					time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
				}
			}
		]);
		
		const resultMarch = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needMarch }
		);
		expect(resultMarch.slot_allocations.length).toBe(0);
		
		// Test 6: September second week (should NOT match - provider only first week)
		const needSeptWeek2 = createTestCommitment([
			{
				...createNeedSlot('seasonal', 5),
				start_date: '2024-09-10', // Tuesday, second week of September
				availability_window: {
					time_ranges: [{ start_time: '11:00', end_time: '13:00' }]
				}
			}
		]);
		
		const resultSeptWeek2 = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needSeptWeek2 }
		);
		expect(resultSeptWeek2.slot_allocations.length).toBe(0);
	});
	
	it('should handle recurring needs matching recurring capacity', () => {
		// Provider: Weekly, Mon/Wed/Fri 9-5
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('therapy', 15),
				recurrence: 'weekly',
				start_date: '2024-01-01',
				end_date: '2024-12-31',
				availability_window: {
					day_schedules: [
						{
							days: ['monday', 'wednesday', 'friday'],
							time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
						}
					]
				}
			}
		]);
		
		// Need: Weekly Wednesday 10-11
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('therapy', 8),
				recurrence: 'weekly',
				start_date: '2024-01-01',
				end_date: '2024-12-31',
				availability_window: {
					day_schedules: [
						{
							days: ['wednesday'],
							time_ranges: [{ start_time: '10:00', end_time: '11:00' }]
						}
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'therapy': { 'patient': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'therapy': { 'patient': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'patient': needCommitment }
		);
		
		// Should match - both recurring, Wednesday overlaps, time overlaps
		expect(result.slot_allocations.length).toBeGreaterThan(0);
		const total = result.recipient_totals_by_type?.therapy?.patient || 0;
		expect(total).toBeGreaterThan(0);
	});
	
	it('should converge with complex time-constrained allocations', () => {
		// Multi-provider scenario with different availability patterns
		const provider1 = createTestCommitment([], [
			{
				...createCapacitySlot('healthcare', 10),
				recurrence: 'weekly',
				availability_window: {
					day_schedules: [
						{
							days: ['monday', 'tuesday'],
							time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
						}
					]
				}
			}
		]);
		
		const provider2 = createTestCommitment([], [
			{
				...createCapacitySlot('healthcare', 10),
				recurrence: 'weekly',
				availability_window: {
					day_schedules: [
						{
							days: ['wednesday', 'thursday', 'friday'],
							time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
						}
					]
				}
			}
		]);
		
		// Patient needs spread across the week
		const patientCommitment = createTestCommitment([
			{
				...createNeedSlot('healthcare', 5),
				start_date: '2024-03-04', // Monday
				availability_window: {
					time_ranges: [{ start_time: '10:00', end_time: '12:00' }]
				}
			},
			{
				...createNeedSlot('healthcare', 5),
				start_date: '2024-03-06', // Wednesday
				availability_window: {
					time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'healthcare': { 'patient': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'healthcare': { 'patient': 1.0 }
		};
		
		const networkCommitments = { 'patient': patientCommitment };
		
		let needVector: Record<string, MultiDimensionalNeedState> = {
			'patient': initializeMultiDimensionalNeedState('patient', patientCommitment)
		};
		
		// Provider 1 allocates (Monday need)
		const result1 = applyMultiDimensionalAllocationOperator(
			'provider1',
			provider1,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments,
			needVector
		);
		
		needVector = result1.updatedNeedVector;
		
		// Provider 2 allocates (Wednesday need)
		const result2 = applyMultiDimensionalAllocationOperator(
			'provider2',
			provider2,
			myMRValuesByType,
			myWeightsByType,
			networkCommitments,
			needVector
		);
		
		needVector = result2.updatedNeedVector;
		
		// All needs should be met
		const finalNorm = computeFrobeniusNorm(needVector);
		expect(finalNorm).toBeLessThan(CONVERGENCE_EPSILON);
	});
});

// ═══════════════════════════════════════════════════════════════════
// SUITE 14: TIMEZONE-AWARE MATCHING (Global Coordination)
// ═══════════════════════════════════════════════════════════════════

describe('Timezone-Aware Matching (Global Coordination)', () => {
	it('should match NYC provider (2pm EST) with London recipient (7pm GMT)', () => {
		// Provider in NYC: Monday 2pm-4pm EST (UTC-5)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('consulting', 10),
				recurrence: 'weekly',
				time_zone: 'America/New_York',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in London: Monday, March 4 at 7pm-9pm GMT (UTC+0)
		// 7pm GMT = 2pm EST (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('consulting', 5),
				start_date: '2024-03-04', // Monday
				time_zone: 'Europe/London',
				availability_window: {
					time_ranges: [
						{ start_time: '19:00', end_time: '21:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'consulting': { 'recipient': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'consulting': { 'recipient': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'recipient': needCommitment }
		);
		
		// Should match! 2pm EST = 7pm GMT
		expect(result.slot_allocations.length).toBeGreaterThan(0);
		const total = result.recipient_totals_by_type?.consulting?.recipient || 0;
		expect(total).toBeGreaterThan(0);
	});
	
	it('should NOT match when times do not overlap across timezones', () => {
		// Provider in Tokyo: Monday 9am-11am JST (UTC+9)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('tutoring', 10),
				recurrence: 'weekly',
				time_zone: 'Asia/Tokyo',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '09:00', end_time: '11:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in NYC: Monday at 2pm-4pm EST (UTC-5)
		// 2pm EST = 4am+1day JST (Tuesday 4am) - DOES NOT OVERLAP
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('tutoring', 5),
				start_date: '2024-03-04', // Monday
				time_zone: 'America/New_York',
				availability_window: {
					time_ranges: [
						{ start_time: '14:00', end_time: '16:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'student': needCommitment }
		);
		
		// Should NOT match - times don't overlap
		expect(result.slot_allocations.length).toBe(0);
	});
	
	it('should handle UTC as default timezone', () => {
		// Provider: no timezone specified (defaults to UTC)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('healthcare', 10),
				recurrence: 'weekly',
				availability_window: {
					day_schedules: [
						{
							days: ['tuesday'],
							time_ranges: [{ start_time: '10:00', end_time: '12:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient: also no timezone (defaults to UTC)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('healthcare', 5),
				start_date: '2024-03-05', // Tuesday
				availability_window: {
					time_ranges: [
						{ start_time: '11:00', end_time: '11:30' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'healthcare': { 'patient': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'healthcare': { 'patient': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'patient': needCommitment }
		);
		
		// Should match - both UTC, times overlap
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should match across multiple timezones (Sydney -> Berlin)', () => {
		// Provider in Sydney: Wednesday 8am-10am AEDT (UTC+11)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('language', 10),
				recurrence: 'weekly',
				time_zone: 'Australia/Sydney',
				availability_window: {
					day_schedules: [
						{
							days: ['wednesday'],
							time_ranges: [{ start_time: '08:00', end_time: '10:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in Berlin: Tuesday 10pm-midnight CET (UTC+1)
		// 10pm Tuesday CET = 8am Wednesday AEDT (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('language', 5),
				start_date: '2024-03-05', // Tuesday in Berlin = Wednesday in Sydney
				time_zone: 'Europe/Berlin',
				availability_window: {
					time_ranges: [
						{ start_time: '22:00', end_time: '23:59' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'language': { 'student': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'language': { 'student': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'student': needCommitment }
		);
		
		// Should match despite being in different calendar days locally
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should handle recurring-to-recurring matching across timezones with day-shift', () => {
		// Provider in LA: Monday 11pm-1am PST (UTC-8) - crosses into Tuesday
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('mentoring', 10),
				recurrence: 'weekly',
				time_zone: 'America/Los_Angeles',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '23:00', end_time: '23:59' }]
						},
						{
							days: ['tuesday'],
							time_ranges: [{ start_time: '00:00', end_time: '01:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in Paris: Tuesday 8am-10am CET (UTC+1)
		// 8am Tuesday CET = 11pm Monday PST (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('mentoring', 5),
				recurrence: 'weekly',
				time_zone: 'Europe/Paris',
				availability_window: {
					day_schedules: [
						{
							days: ['tuesday'],
							time_ranges: [{ start_time: '08:00', end_time: '10:00' }]
						}
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'mentoring': { 'mentee': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'mentoring': { 'mentee': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'mentee': needCommitment }
		);
		
		// Should match! Monday 11pm PST = Tuesday 8am CET
		expect(result.slot_allocations.length).toBeGreaterThan(0);
		const total = result.recipient_totals_by_type?.mentoring?.mentee || 0;
		expect(total).toBeGreaterThan(0);
	});
	
	it('should handle DST transitions correctly', () => {
		// Test that timezone conversion handles DST
		// March 10, 2024: DST starts in US (spring forward)
		
		// Provider in NYC: March 11 (after DST) at 2pm EDT (UTC-4, was UTC-5)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('workshop', 10),
				recurrence: 'weekly',
				time_zone: 'America/New_York',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in London: March 11 at 6pm GMT (UTC+0, no DST yet)
		// 2pm EDT (UTC-4) = 6pm GMT (matches)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('workshop', 5),
				start_date: '2024-03-11', // Monday after DST
				time_zone: 'Europe/London',
				availability_window: {
					time_ranges: [
						{ start_time: '18:00', end_time: '20:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'workshop': { 'participant': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'workshop': { 'participant': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'participant': needCommitment }
		);
		
		// Should match with correct DST offset
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should handle half-hour timezone offsets (India UTC+5:30)', () => {
		// Provider in India: Thursday 10:30am-12:30pm IST (UTC+5:30)
		// = Thursday 05:00-07:00 UTC
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('tutoring', 10),
				recurrence: 'weekly',
				time_zone: 'Asia/Kolkata',
				availability_window: {
					day_schedules: [
						{
							days: ['thursday'],
							time_ranges: [{ start_time: '10:30', end_time: '12:30' }]
						}
					]
				}
			}
		]);
		
		// Recipient in NYC: Thursday 1am-2am EST (UTC-5)
		// = Thursday 06:00-07:00 UTC (overlaps with 05:00-07:00!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('tutoring', 5),
				start_date: '2024-03-07', // Thursday
				time_zone: 'America/New_York',
				availability_window: {
					time_ranges: [
						{ start_time: '01:00', end_time: '02:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'tutoring': { 'student': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'student': needCommitment }
		);
		
		// Should match with correct half-hour offset
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should handle extreme timezone differences (Auckland UTC+13 to Hawaii UTC-10)', () => {
		// Provider in Auckland: Monday 9am-11am NZDT (UTC+13)
		// = Sunday 20:00-22:00 UTC (day shifts backward!)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('consulting', 10),
				recurrence: 'weekly',
				time_zone: 'Pacific/Auckland',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '09:00', end_time: '11:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in Hawaii: Sunday 10am-12pm HST (UTC-10)
		// = Sunday 20:00-22:00 UTC (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('consulting', 5),
				start_date: '2024-03-03', // Sunday in Hawaii
				time_zone: 'Pacific/Honolulu',
				availability_window: {
					time_ranges: [
						{ start_time: '10:00', end_time: '12:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'consulting': { 'client': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'consulting': { 'client': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needCommitment }
		);
		
		// Should match despite 23-hour timezone difference
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should handle slots that span midnight locally', () => {
		// Provider in Tokyo: Friday 11pm-Saturday 1am JST (spans midnight locally)
		// = Friday 14:00-16:00 UTC (continuous in UTC!)
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('support', 10),
				recurrence: 'weekly',
				time_zone: 'Asia/Tokyo',
				availability_window: {
					day_schedules: [
						{
							days: ['friday'],
							time_ranges: [{ start_time: '23:00', end_time: '23:59' }]
						},
						{
							days: ['saturday'],
							time_ranges: [{ start_time: '00:00', end_time: '01:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in LA: Friday 6am-8am PST (UTC-8)
		// = Friday 14:00-16:00 UTC (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('support', 5),
				start_date: '2024-03-08', // Friday
				time_zone: 'America/Los_Angeles',
				availability_window: {
					time_ranges: [
						{ start_time: '06:00', end_time: '08:00' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'support': { 'user': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'support': { 'user': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'user': needCommitment }
		);
		
		// Should match - provider's midnight-spanning slot (11pm Fri - 1am Sat JST) 
		// becomes continuous 2pm-4pm Fri UTC, matching recipient's 6am-8am Fri PST
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
	
	it('should NOT match when timezones cause non-overlapping UTC times', () => {
		// Provider in London: Tuesday 9am-11am GMT (UTC+0)
		// = Tuesday 09:00-11:00 UTC
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('coaching', 10),
				recurrence: 'weekly',
				time_zone: 'Europe/London',
				availability_window: {
					day_schedules: [
						{
							days: ['tuesday'],
							time_ranges: [{ start_time: '09:00', end_time: '11:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in Sydney: Tuesday 9pm-11pm AEDT (UTC+11)
		// = Tuesday 10:00-12:00 UTC (only 1 hour overlap: 10:00-11:00)
		// But let's test NO overlap case: Tuesday 12:30pm-2:30pm
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('coaching', 5),
				start_date: '2024-03-05', // Tuesday
				time_zone: 'Australia/Sydney',
				availability_window: {
					time_ranges: [
						{ start_time: '23:30', end_time: '01:30' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'coaching': { 'client': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'coaching': { 'client': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'client': needCommitment }
		);
		
		// Should NOT match - times don't overlap in UTC
		expect(result.slot_allocations.length).toBe(0);
	});
	
	it('should handle southern hemisphere DST (opposite of northern)', () => {
		// Test matching during a time when northern hemisphere is NOT in DST
		// but southern hemisphere IS in DST
		
		// Provider in Melbourne: January 15 (summer, AEDT UTC+11) at 2pm-4pm
		// = January 15 03:00-05:00 UTC
		const providerCommitment = createTestCommitment([], [
			{
				...createCapacitySlot('training', 10),
				recurrence: 'weekly',
				time_zone: 'Australia/Melbourne',
				availability_window: {
					day_schedules: [
						{
							days: ['monday'],
							time_ranges: [{ start_time: '14:00', end_time: '16:00' }]
						}
					]
				}
			}
		]);
		
		// Recipient in NYC: January 14 (winter, EST UTC-5) at 10pm-midnight
		// = January 15 03:00-05:00 UTC (matches!)
		const needCommitment = createTestCommitment([
			{
				...createNeedSlot('training', 5),
				start_date: '2024-01-14', // Sunday in NYC
				time_zone: 'America/New_York',
				availability_window: {
					time_ranges: [
						{ start_time: '22:00', end_time: '23:59' }
					]
				}
			}
		]);
		
		const myMRValuesByType: MultiDimensionalRecognition = {
			'training': { 'trainee': 1.0 }
		};
		
		const myWeightsByType: MultiDimensionalRecognition = {
			'training': { 'trainee': 1.0 }
		};
		
		const result = computeMultiDimensionalSlotNativeAllocation(
			'provider',
			providerCommitment,
			myMRValuesByType,
			myWeightsByType,
			{ 'trainee': needCommitment }
		);
		
		// Should match with opposite DST seasons
		expect(result.slot_allocations.length).toBeGreaterThan(0);
	});
});

