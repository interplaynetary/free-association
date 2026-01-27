import { z } from 'zod';
import { ProfferSchema, type Proffer, type Slot, globalProfferRegistry } from '../proffer-v2';

function createId() {
    return Math.random().toString(36).substring(7);
}

// 1. Define a Nested Proffer: "Entertainment"
const entertainmentProffer: Proffer = {
    id: 'proffer-entertainment-01',
    name: 'Live Music Entertainment',
    description: {
        type: 'templated_strict',
        template: 'template-jazz-gig-v1',
        requirements: { format: '2 sets of 45 mins' }
    },
    created_at: new Date(),
    updated_at: new Date(),
    status: 'potential',
    slots: [
        {
            id: 'slot-musician-01',
            name: 'Jazz Pianist',
            phase: 'proposal',
            status: 'potential',
            input: {
                kind: 'resource',
                type_id: 'resource-type-musician',
                quantity: 1,
            },
            optional: false,
            acceptance_logic: {
                type: 'automatic',
                rule: { "and": [{ ">=": [{ "var": "capacity.skills.level" }, 5] }] }
            }
        },
        // Effect / Completion requirement
        {
            id: 'slot-effect-music-01',
            name: 'Audience Enjoyment Verification',
            phase: 'completion',
            status: 'potential',
            input: {
                kind: 'generic',
                data_type: 'boolean',
                description: 'Did the guests enjoy the live music?'
            },
            optional: false
        }
    ]
};

// 2. Define the Main Proffer: "Dinner Party"
const dinnerPartyProffer: Proffer = {
    id: 'proffer-dinner-01',
    name: 'Annual Charity Dinner',
    description: 'A gala dinner for 50 guests',
    created_at: new Date(),
    updated_at: new Date(),
    status: 'potential',
    slots: [
        {
            id: 'slot-theme-01',
            name: 'Event Theme',
            phase: 'proposal',
            status: 'potential',
            input: {
                kind: 'generic',
                data_type: 'string',
                description: 'The creative theme for the dinner'
            },
            optional: false
        },
        {
            id: 'slot-venue-01',
            name: 'Event Hall',
            phase: 'proposal',
            status: 'potential',
            input: {
                kind: 'resource',
                type_id: 'resource-type-venue',
                quantity: 1
            },
            optional: false,
            acceptance_logic: {
                type: 'automatic',
                rule: { "var": "capacity.attributes.has_kitchen" }
            }
        },
        {
            id: 'slot-catering-01',
            name: 'Catering Service',
            phase: 'proposal',
            status: 'potential',
            input: {
                kind: 'resource',
                type_id: 'resource-type-catering',
                quantity: 50,
                unit: 'guests'
            },
            optional: false
        },
        {
            // Nested Proffer as Input!
            id: 'slot-entertainment-01',
            name: 'Evening Entertainment',
            phase: 'proposal',
            status: 'potential',
            input: {
                kind: 'proffer',
                proffer_id: entertainmentProffer.id
            },
            optional: false
        }
    ]
};

console.log("✅ Defined Proffers using Elegant V2 Schema (Slot + Input)");
console.log("------------------------------------------------");
console.log(`Main Proffer: ${dinnerPartyProffer.name}`);
console.log(`Needs:`);

dinnerPartyProffer.slots.forEach(slot => {
    const phaseLabel = `[${slot.phase}]`;
    const input = slot.input;

    if (input.kind === 'resource') {
        console.log(` - ${phaseLabel} [${slot.status}] (Resource Need) ${slot.name} (${input.quantity} ${input.unit || 'units'})`);
    } else if (input.kind === 'generic') {
        console.log(` - ${phaseLabel} [${slot.status}] (Data Need) ${slot.name} [Type: ${input.data_type}]`);
    } else if (input.kind === 'proffer') {
        console.log(` - ${phaseLabel} [${slot.status}] (Proffer Need) ${slot.name}`);
        console.log(`   -> Target Proffer ID: ${input.proffer_id || 'template:' + input.template_id}`);
    }
});

// Verification loop
try {
    ProfferSchema.parse(entertainmentProffer);
    ProfferSchema.parse(dinnerPartyProffer);
    console.log("\n✅ Schema Validation Passed");

    // Test Registry Logic
    globalProfferRegistry.addProffer(entertainmentProffer);
    globalProfferRegistry.addProffer(dinnerPartyProffer);

    const dagValidation = globalProfferRegistry.validateAllDAGs();
    if (dagValidation.isValid) {
        console.log("✅ DAG Validation Passed (No Cycles)");
    } else {
        console.error("❌ DAG Validation Failed:", dagValidation.errors);
    }

    const progress = globalProfferRegistry.calculateProgress(dinnerPartyProffer);
    console.log(`✅ Progress Calculation: ${progress.completionPercentage}% Complete`);

} catch (e) {
    console.error("\n❌ Schema Validation Failed:", e);
    process.exit(1);
}
