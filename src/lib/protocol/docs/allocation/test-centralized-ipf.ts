/**
 * Quick test to see if centralized IPF converges on Provider(200) → Recipient(50)
 */

import { calculateIPFAllocation } from '../experimental/allocation-ipf.js';
import type { AvailabilitySlot, NeedSlot, Commitment } from '../../schemas.js';

const cs: AvailabilitySlot = {
    id: 'c1',
    name: 'Capacity 1',
    quantity: 200,
    type_id: 'type1',
    priority_distribution: { 'r1': 1.0 }
};

const ns: NeedSlot = {
    id: 'n1',
    name: 'Need 1',
    quantity: 50,
    type_id: 'type1',
    priority_distribution: { 'p1': 1.0 }
};

const commitments: Record<string, Commitment> = {
    'p1': {
        capacity_slots: [cs],
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

console.log('Testing Centralized IPF: Provider(200) → Recipient(50)');
console.log('Expected: allocation ≈ 50');
console.log('');

const result = calculateIPFAllocation([cs], [ns], commitments, { debug: true });

console.log('');
console.log('Result:', result);
console.log('Allocation:', result[0]?.quantity);
console.log('Expected: 50');
console.log('Match:', Math.abs((result[0]?.quantity || 0) - 50) < 1 ? '✅' : '❌');
