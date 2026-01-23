import { z } from 'zod';
import { Type } from '$lib/utils/types_helper'; // Assuming this helper exists or we use raw objects
import { ProfferV2Schema, type ProfferV2, type ProfferSlot } from '../proffer-v2';

// NOTE: Ideally we would import 'createZodObject' or similar helpers, 
// but for this example we'll manually construct matching objects to show the structure.

function createId() {
    return Math.random().toString(36).substring(7);
}

// 1. Define a Nested Proffer: "Entertainment"
// This proffer requires a Musician.
const entertainmentProffer: ProfferV2 = {
    id: 'proffer-entertainment-01',
    name: 'Live Music Entertainment',
    description: 'A 2-hour set of jazz music',
    created_at: new Date(),
    updated_at: new Date(),
    status: 'draft',
    slots: [
        {
            // Base properties from NeedSlotSchema
            id: 'slot-musician-01',
            type_id: 'resource-type-musician',
            name: 'Jazz Pianist',
            quantity: 1,
            // Proffer-specific logic
            status: 'empty',
            // Basic acceptance logic
            acceptance_logic: {
                type: 'automatic',
                rule: { "and": [{ ">=": [{ "var": "capacity.skills.level" }, 5] }] }
            }
        } as ProfferSlot // Verification: Type assertion to ensure it matches schema
    ],
    effects: [
        {
            type: 'social_experience',
            description: 'Guests enjoyed live music'
        }
    ]
};

// 2. Define the Main Proffer: "Dinner Party"
// Needs: Food, Venue, AND the Entertainment Proffer
const dinnerPartyProffer: ProfferV2 = {
    id: 'proffer-dinner-01',
    name: 'Annual Charity Dinner',
    description: 'A gala dinner for 50 guests',
    created_at: new Date(),
    updated_at: new Date(),
    status: 'draft',
    slots: [
        {
            id: 'slot-venue-01',
            type_id: 'resource-type-venue',
            name: 'Event Hall',
            quantity: 1,
            status: 'empty',
            acceptance_logic: {
                type: 'automatic',
                rule: { "var": "capacity.attributes.has_kitchen" }
            }
        } as ProfferSlot,
        {
            id: 'slot-catering-01',
            type_id: 'resource-type-catering',
            name: 'Catering Service',
            quantity: 50,
            unit: 'guests',
            status: 'empty'
        } as ProfferSlot,
        {
            // This slot is filled by the Entertainment Proffer
            id: 'slot-entertainment-01',
            type_id: 'resource-type-service', // Generic service type
            name: 'Evening Entertainment',
            quantity: 1,
            status: 'empty',
            nested_proffer_id: entertainmentProffer.id // Linking to nested proffer
        } as ProfferSlot
    ]
};

console.log("✅ Defined Proffers using Need-Slot V2 Schema");
console.log("------------------------------------------------");
console.log(`Main Proffer: ${dinnerPartyProffer.name}`);
console.log(`Needs:`);
dinnerPartyProffer.slots.forEach(slot => {
    console.log(` - [${slot.status}] ${slot.name} (${slot.quantity} ${slot.unit || 'units'})`);
    if (slot.nested_proffer_id) {
        console.log(`   -> Composed of Proffer: ${slot.nested_proffer_id}`);
    }
});

// Verification: Ensure the objects actually pass the Zod validation
try {
    ProfferV2Schema.parse(entertainmentProffer);
    ProfferV2Schema.parse(dinnerPartyProffer);
    console.log("\n✅ Schema Validation Passed");
} catch (e) {
    console.error("\n❌ Schema Validation Failed:", e);
    process.exit(1);
}
