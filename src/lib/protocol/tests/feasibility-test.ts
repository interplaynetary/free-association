import { z } from 'zod';
import { calculateFeasibility, type FeasibilityContext } from '../feasibility.js';
import { type NeedSlot, type AvailabilitySlot } from '../resources.js';

// Mocks for basic tests
const baseNeed: NeedSlot = {
    id: 'need-1',
    type_id: 'type-1',
    name: 'Need',
    quantity: 1,
    offerer: 'seeker-1',
    time_zone: 'UTC'
};

const baseCapacity: AvailabilitySlot = {
    id: 'cap-1',
    type_id: 'type-1',
    name: 'Capacity',
    quantity: 1,
    offerer: 'provider-1',
    time_zone: 'UTC'
};

function runTest(name: string, need: NeedSlot, capacity: AvailabilitySlot, context: FeasibilityContext = {}, expectedType: 'possible' | 'impossible') {
    const result = calculateFeasibility(need, capacity, context);
    console.log(`[TEST] ${name}: ${result.type.toUpperCase()} (Confidence: ${(result as any).confidence ?? 'N/A'})`);

    if (result.type !== expectedType) {
        console.error(`  ❌ FAILED: Expected ${expectedType}, got ${result.type}`);
        if (result.type === 'impossible') console.error('  Reasons:', result.reasons);
    } else {
        console.log(`  ✅ PASSED`);
    }
    return result;
}

console.log('--- STARTING FEASIBILITY TESTS ---');

// 1. Time (Mismatch)
runTest('Time Mismatch',
    { ...baseNeed, availability_window: { time_ranges: [{ start_time: '12:00', end_time: '13:00' }] } },
    { ...baseCapacity, availability_window: { time_ranges: [{ start_time: '14:00', end_time: '15:00' }] } },
    {},
    'impossible'
);

// 2. Location (Distance)
runTest('Location Mismatch (>50km)',
    { ...baseNeed, latitude: 40.7128, longitude: -74.0060, search_radius_km: 50 }, // NYC
    { ...baseCapacity, latitude: 39.9526, longitude: -75.1652 }, // Philadelphia (~130km away)
    {},
    'impossible'
);

runTest('Location Match (Close)',
    { ...baseNeed, latitude: 40.7128, longitude: -74.0060 }, // NYC
    { ...baseCapacity, latitude: 40.7138, longitude: -74.0070 }, // NYC (Close)
    {},
    'possible'
);

// 3. Affinity (Trust)
const contextWithTrust: FeasibilityContext = {
    providerWeights: { 'seeker-1': 1.0 }, // Provider fully trusts seeker
    seekerWeights: { 'provider-1': 0.5 }  // Seeker partially trusts provider
};
const affinityResult = runTest('Affinity (Partial Trust)', baseNeed, baseCapacity, contextWithTrust, 'possible');
if ((affinityResult as any).scores.affinity !== 0.5) {
    console.error(`  ❌ AFFINITY FAIL: Expected 0.5, got ${(affinityResult as any).scores.affinity}`);
}

// 4. Resources (Partial Quantity)
const partialRes = runTest('Resources (Partial)',
    { ...baseNeed, quantity: 10 },
    { ...baseCapacity, quantity: 5 },
    {},
    'possible'
);
if ((partialRes as any).scores.resources !== 0.5) {
    console.error(`  ❌ RESOURCE FAIL: Expected 0.5, got ${(partialRes as any).scores.resources}`);
}

// 5. Travel (Impossible Speed)
// Previous commitment in Philadelphia (ends 12:00)
// New capacity in NYC (starts 12:30)
// Distance ~130km. Time 30 mins. Speed req = 260km/h. Impossible.
const travelContext: FeasibilityContext = {
    previousCommitment: {
        latitude: 39.9526,
        longitude: -75.1652,
        end_time: '12:00'
    }
};

const travelRes = runTest('Travel (Impossible Speed)',
    baseNeed,
    {
        ...baseCapacity,
        latitude: 40.7128,
        longitude: -74.0060,
        availability_window: { time_ranges: [{ start_time: '12:30', end_time: '13:30' }] }
    },
    travelContext,
    'impossible'
);

if ((travelRes as any).scores.travel !== 0) {
    console.error(`  ❌ TRAVEL FAIL: Expected 0, got ${(travelRes as any).scores.travel}`);
}

if ((travelRes as any).scores.travel !== 0) {
    console.error(`  ❌ TRAVEL FAIL: Expected 0, got ${(travelRes as any).scores.travel}`);
}

// 6. Time Duration (Too Short)
// 15 min overlap vs 30 min required (using min_atomic_size now)
runTest('Time Duration (Too Short)',
    {
        ...baseNeed,
        min_atomic_size: 30,
        availability_window: { time_ranges: [{ start_time: '12:00', end_time: '12:15' }] }
    },
    {
        ...baseCapacity,
        availability_window: { time_ranges: [{ start_time: '12:00', end_time: '13:00' }] }
    },
    {},
    'impossible'
);



// 7. Atomic Size (Granularity)
// Same as duration, but using the new generalized field
runTest('Atomic Size (Granularity)',
    {
        ...baseNeed,
        min_atomic_size: 45, // 45 min atomic chunk
        availability_window: { time_ranges: [{ start_time: '12:00', end_time: '12:30' }] } // only 30 mins
    },
    {
        ...baseCapacity,
        availability_window: { time_ranges: [{ start_time: '12:00', end_time: '13:00' }] }
    },
    {},
    'impossible'
);

console.log('--- TESTS COMPLETE ---');
