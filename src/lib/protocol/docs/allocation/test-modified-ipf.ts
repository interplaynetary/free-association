/**
 * Test: Modified IPF with priority-aware row scaling
 * 
 * Idea: Don't force ALL providers to use full capacity.
 * Instead, scale based on "fair share" of total capacity.
 */

import type { AvailabilitySlot, NeedSlot, Commitment } from '../../schemas.js';
import { calculateSeedValue } from '../../ipf-core.js';

const csA: AvailabilitySlot = {
    id: 'cA',
    name: 'Capacity A (Low Priority)',
    quantity: 100,
    type_id: 'type1',
    priority_distribution: { 'r1': 0.3 }
};

const csB: AvailabilitySlot = {
    id: 'cB',
    name: 'Capacity B (High Priority)',
    quantity: 100,
    type_id: 'type1',
    priority_distribution: { 'r1': 1.0 }
};

const ns: NeedSlot = {
    id: 'n1',
    name: 'Need 1',
    quantity: 100,
    type_id: 'type1',
    priority_distribution: { 'pA': 1.0, 'pB': 1.0 }
};

const commitments: Record<string, Commitment> = {
    'pA': { capacity_slots: [csA], need_slots: [], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} },
    'pB': { capacity_slots: [csB], need_slots: [], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} },
    'r1': { capacity_slots: [], need_slots: [ns], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} }
};

// Calculate seed values
const seedA = calculateSeedValue(csA, ns, commitments);
const seedB = calculateSeedValue(csB, ns, commitments);

console.log('Modified IPF with Priority-Aware Row Scaling');
console.log('='.repeat(50));
console.log('');
console.log('Seed values:');
console.log(`  K_A = ${seedA.toFixed(4)}`);
console.log(`  K_B = ${seedB.toFixed(4)}`);
console.log(`  Ratio: ${(seedB / seedA).toFixed(2)}:1`);
console.log('');

// Initialize matrix with seed values
let matrix = {
    'cA': { 'n1': seedA },
    'cB': { 'n1': seedB }
};

const epsilon = 1e-6;
const maxIter = 100;

for (let iter = 0; iter < maxIter; iter++) {
    let maxDiff = 0;

    // MODIFIED ROW SCALING: Scale to "fair share" not full capacity
    // Fair share = (my seed / total seeds) * total need
    const totalSeeds = seedA + seedB;
    const fairShareA = (seedA / totalSeeds) * ns.quantity;
    const fairShareB = (seedB / totalSeeds) * ns.quantity;

    // Provider A
    const rowSumA = matrix['cA']['n1'];
    if (rowSumA > epsilon) {
        const targetA = Math.min(csA.quantity, fairShareA);
        const scaleA = targetA / rowSumA;
        const oldA = matrix['cA']['n1'];
        matrix['cA']['n1'] *= scaleA;
        maxDiff = Math.max(maxDiff, Math.abs(matrix['cA']['n1'] - oldA));
    }

    // Provider B
    const rowSumB = matrix['cB']['n1'];
    if (rowSumB > epsilon) {
        const targetB = Math.min(csB.quantity, fairShareB);
        const scaleB = targetB / rowSumB;
        const oldB = matrix['cB']['n1'];
        matrix['cB']['n1'] *= scaleB;
        maxDiff = Math.max(maxDiff, Math.abs(matrix['cB']['n1'] - oldB));
    }

    // COLUMN SCALING: Standard (clamp to need)
    const colSum = matrix['cA']['n1'] + matrix['cB']['n1'];
    if (colSum > ns.quantity) {
        const scale = ns.quantity / colSum;
        const oldA = matrix['cA']['n1'];
        const oldB = matrix['cB']['n1'];
        matrix['cA']['n1'] *= scale;
        matrix['cB']['n1'] *= scale;
        maxDiff = Math.max(maxDiff, Math.abs(matrix['cA']['n1'] - oldA));
        maxDiff = Math.max(maxDiff, Math.abs(matrix['cB']['n1'] - oldB));
    }

    if (iter < 5 || maxDiff < 0.001) {
        console.log(`Iteration ${iter}:`);
        console.log(`  Fair shares: A=${fairShareA.toFixed(2)}, B=${fairShareB.toFixed(2)}`);
        console.log(`  Matrix: A=${matrix['cA']['n1'].toFixed(2)}, B=${matrix['cB']['n1'].toFixed(2)}`);
        console.log(`  Total: ${(matrix['cA']['n1'] + matrix['cB']['n1']).toFixed(2)}`);
        console.log(`  MaxDiff: ${maxDiff.toFixed(6)}`);
        console.log('');
    }

    if (maxDiff < 0.001) {
        console.log(`Converged after ${iter + 1} iterations`);
        break;
    }
}

console.log('');
console.log('Final Result:');
console.log(`  Provider A: ${matrix['cA']['n1'].toFixed(2)}`);
console.log(`  Provider B: ${matrix['cB']['n1'].toFixed(2)}`);
console.log(`  Total: ${(matrix['cA']['n1'] + matrix['cB']['n1']).toFixed(2)}`);
console.log(`  Ratio B/A: ${(matrix['cB']['n1'] / matrix['cA']['n1']).toFixed(2)}`);
console.log('');
console.log(`Expected ratio: ${(seedB / seedA).toFixed(2)}`);
console.log(`Match: ${Math.abs(matrix['cB']['n1'] / matrix['cA']['n1'] - seedB / seedA) < 0.1 ? '✅' : '❌'}`);
