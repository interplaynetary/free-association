/**
 * Debug: Check what seed values are being calculated
 */

import { calculateSeedValue } from '../../ipf-core.js';
import type { AvailabilitySlot, NeedSlot, Commitment } from '../../schemas.js';

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
    priority_distribution: { 'pA': 1.0, 'pB': 1.0 } // Equal recipient preference
};

const commitments: Record<string, Commitment> = {
    'pA': { capacity_slots: [csA], need_slots: [], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} },
    'pB': { capacity_slots: [csB], need_slots: [], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} },
    'r1': { capacity_slots: [], need_slots: [ns], timestamp: Date.now(), itcStamp: {}, global_recognition_weights: {} }
};

const seedA = calculateSeedValue(csA, ns, commitments, 1e-6, 0.5);
const seedB = calculateSeedValue(csB, ns, commitments, 1e-6, 0.5);

console.log('Seed Values:');
console.log(`Provider A (priority 0.3): K_pr = ${seedA.toFixed(6)}`);
console.log(`Provider B (priority 1.0): K_pr = ${seedB.toFixed(6)}`);
console.log(`Ratio B/A: ${(seedB / seedA).toFixed(2)}`);
console.log('');
console.log('Formula: K_pr = (ProviderPriority + ε) × (RecipientPreference + ε)^γ');
console.log(`  where γ = 0.5, ε = 1e-6`);
console.log('');
console.log('Provider A:');
console.log(`  ProviderPriority = 0.3`);
console.log(`  RecipientPreference = 1.0`);
console.log(`  K_pr = (0.3 + 1e-6) × (1.0 + 1e-6)^0.5 = ${seedA.toFixed(6)}`);
console.log('');
console.log('Provider B:');
console.log(`  ProviderPriority = 1.0`);
console.log(`  RecipientPreference = 1.0`);
console.log(`  K_pr = (1.0 + 1e-6) × (1.0 + 1e-6)^0.5 = ${seedB.toFixed(6)}`);
