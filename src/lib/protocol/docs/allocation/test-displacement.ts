/**
 * Test displacement: High-priority provider should displace low-priority provider
 * 
 * Scenario:
 * - Provider A (low priority = 0.3) has 100 capacity
 * - Provider B (high priority = 1.0) has 100 capacity
 * - Recipient needs 100
 * 
 * Expected: Provider B should displace Provider A, getting ~77 vs A's ~23
 * (proportional to priorities: 1.0/(1.0+0.3) ≈ 0.77, 0.3/(1.0+0.3) ≈ 0.23)
 */

import { calculateIPFAllocation } from '../../allocation-ipf.js';
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
    'pA': {
        capacity_slots: [csA],
        need_slots: [],
        timestamp: Date.now(),
        itcStamp: {},
        global_recognition_weights: {}
    },
    'pB': {
        capacity_slots: [csB],
        need_slots: [],
        timestamp: Date.now(),
        itcStamp: {},
        global_recognition_weights: {}
    },
    'r1': {
        capacity_slots: [],
        need_slots: [ns],
        timestamp: Date.now(),
        itcStamp: {},
        global_recognition_weights: {}
    }
};

console.log('Testing Displacement: Low-Priority (0.3) vs High-Priority (1.0)');
console.log('Both have 100 capacity, recipient needs 100');
console.log('Expected: ~23 from A, ~77 from B (proportional to priorities)');
console.log('');

const result = calculateIPFAllocation([csA, csB], [ns], commitments, { debug: true });

console.log('');
console.log('Results:');
const allocA = result.find(r => r.capacity_slot_id === 'cA')?.quantity || 0;
const allocB = result.find(r => r.capacity_slot_id === 'cB')?.quantity || 0;
console.log(`Provider A (priority 0.3): ${allocA.toFixed(2)}`);
console.log(`Provider B (priority 1.0): ${allocB.toFixed(2)}`);
console.log(`Total: ${(allocA + allocB).toFixed(2)}`);
console.log('');
console.log(`Ratio B/A: ${(allocB / allocA).toFixed(2)} (expected: ${(1.0 / 0.3).toFixed(2)})`);
console.log(`Displacement working: ${allocB > allocA ? '✅' : '❌'}`);
