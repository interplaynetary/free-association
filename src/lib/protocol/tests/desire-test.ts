import { z } from 'zod';
import { SlotRelationshipSchema, type SlotRelationship } from '../desire.js';

const perfectScores = { time: 1, location: 1, skills: 1, travel: 1, resources: 1, affinity: 1, continuity: 1 };
const impossibleScores = { time: 0, location: 1, skills: 1, travel: 1, resources: 1, affinity: 1, continuity: 1 }; // Time failure

// 1. Suggestion (Possible, No Desire)
const suggestion: SlotRelationship = {
    id: 'rel-1',
    capacity_id: 'cap-A',
    need_id: 'need-B',
    feasibility: {
        type: 'possible',
        confidence: 1,
        scores: perfectScores
    },
    status: 'suggestion',
    mutual_desire_quantity: 0
};

// 2. Pruned (Impossible, No Desire)
const pruned: SlotRelationship = {
    id: 'rel-2',
    capacity_id: 'cap-A',
    need_id: 'need-C',
    feasibility: {
        type: 'impossible',
        reasons: ['TIME_MISMATCH'],
        scores: impossibleScores
    },
    status: 'pruned',
    mutual_desire_quantity: 0
};

// 3. Valid Commitment (Possible, Mutual Desire)
const valid: SlotRelationship = {
    id: 'rel-3',
    capacity_id: 'cap-A',
    need_id: 'need-D',
    feasibility: {
        type: 'possible',
        confidence: 1,
        scores: perfectScores
    },
    provider_desire: { quantity: 5, locked: true },
    seeker_desire: { quantity: 5, locked: true },
    status: 'valid',
    mutual_desire_quantity: 5
};

// 4. Contradiction (Impossible, Mutual Desire)
const contradiction: SlotRelationship = {
    id: 'rel-4',
    capacity_id: 'cap-A',
    need_id: 'need-E',
    feasibility: {
        type: 'impossible',
        reasons: ['ALREADY_COMMITTED'],
        scores: { ...perfectScores, time: 0 } // Contradicts time/availability
    },
    provider_desire: { quantity: 5, locked: true, updated_at: 123456 },
    seeker_desire: { quantity: 5, locked: true, updated_at: 123456 },
    status: 'contradiction',
    mutual_desire_quantity: 5
};

// 5. Risky Feasibility (Possible, but with risk)
const risky: SlotRelationship = {
    id: 'rel-5',
    capacity_id: 'cap-A',
    need_id: 'need-F',
    feasibility: {
        type: 'possible',
        confidence: 0.7,
        risk_factors: ['TIGHT_TRAVEL_TIME'],
        scores: { ...perfectScores, travel: 0.7 }
    },
    status: 'suggestion',
    mutual_desire_quantity: 0
};

// 6. Fragmented Continuity (Feasible, but disjointed time)
const fragmented: SlotRelationship = {
    id: 'rel-6',
    capacity_id: 'cap-A',
    need_id: 'need-G',
    feasibility: {
        type: 'possible',
        confidence: 0.8,
        risk_factors: ['FRAGMENTED_TIME'],
        scores: { ...perfectScores, continuity: 0.5 } // 50% continuity (e.g. 2 separate 2hr blocks for a 4hr need)
    },
    status: 'suggestion',
    mutual_desire_quantity: 0
};

// Validate all against schema
const results = [
    SlotRelationshipSchema.safeParse(suggestion),
    SlotRelationshipSchema.safeParse(pruned),
    SlotRelationshipSchema.safeParse(valid),
    SlotRelationshipSchema.safeParse(contradiction),
    SlotRelationshipSchema.safeParse(risky),
    SlotRelationshipSchema.safeParse(fragmented)
];

results.forEach((res, i) => {
    if (res.success) {
        console.log(`[PASS] Quadrant ${i + 1} valid`);
    } else {
        console.error(`[FAIL] Quadrant ${i + 1} invalid:`, res.error);
    }
});
