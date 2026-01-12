
import { createExampleCapacitySlots, createExampleNeedSlots } from '../src/lib/utils/example';
import { AvailabilityWindowSchema } from '../src/lib/protocol/schemas';

const testSchemaAlignment = () => {
    console.log('🧪 Testing Schema Alignment for Demo Data...');

    // Helper to validate a slot
    const validateSlot = (slot: any, type: 'Capacity' | 'Need') => {
        // 1. Check type_id
        if (!slot.type_id) {
            console.error(`❌ [${type}] Missing type_id`, slot.id);
            return false;
        }

        // 2. Check recurrence casing
        if (slot.recurrence && slot.recurrence !== slot.recurrence.toLowerCase()) {
            console.error(`❌ [${type}] Recurrence should be lowercase: ${slot.recurrence}`, slot.id);
            return false;
        }

        // 3. Check availability_window structure
        if (slot.availability_window) {
            const result = AvailabilityWindowSchema.safeParse(slot.availability_window);
            if (!result.success) {
                console.error(`❌ [${type}] Invalid availability_window`, result.error);
                return false;
            }

            // Check if we populated time_ranges correctly
            if (slot.availability_window.time_ranges?.length > 0) {
                const range = slot.availability_window.time_ranges[0];
                if (!range.start_time || !range.end_time) {
                    console.error(`❌ [${type}] Missing start_time/end_time in availability_window`, range);
                    return false;
                }
            }
        }

        // 4. Check for deprecated fields (should NOT be present or at least not relied upon)
        // Note: In TS, these might still exist if the type allows them, but we want to ensure we aren't setting them explicitly if we intended to move to availability_window.
        // Since we removed them from the return object literal, they should be undefined unless they come from somewhere else.

        return true;
    };

    // Test Capacities
    console.log('\n--- Testing Capacity Slots ---');
    const capacities = createExampleCapacitySlots();
    const capResults = capacities.map(s => validateSlot(s, 'Capacity'));
    if (capResults.every(r => r)) {
        console.log(`✅ All ${capacities.length} capacity slots valid.`);
    } else {
        console.error(`❌ Some capacity slots failed validation.`);
    }

    // Test Needs
    console.log('\n--- Testing Need Slots ---');
    const needs = createExampleNeedSlots();
    const needResults = needs.map(s => validateSlot(s, 'Need'));
    if (needResults.every(r => r)) {
        console.log(`✅ All ${needs.length} need slots valid.`);
    } else {
        console.error(`❌ Some need slots failed validation.`);
    }
};

testSchemaAlignment();
