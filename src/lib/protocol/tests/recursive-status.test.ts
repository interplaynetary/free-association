
import { z } from 'zod';
import { ProfferSchema, type Proffer, type Slot, globalProfferRegistry } from '../proffer-v2';

function createId() {
    return Math.random().toString(36).substring(7);
}

// 1. Define a Child Proffer (Initially Potential)
const childProffer: Proffer = {
    id: 'child-proffer',
    name: 'Child Proffer',
    created_at: new Date(),
    updated_at: new Date(),
    status: 'potential',
    slots: [
        {
            id: 'child-slot-1',
            name: 'Child Requirement',
            status: 'potential',
            input: { kind: 'generic', data_type: 'string' },
            optional: false
        }
    ]
};

// 2. Define a Parent Proffer that depends on Child
const parentProffer: Proffer = {
    id: 'parent-proffer',
    name: 'Parent Proffer',
    created_at: new Date(),
    updated_at: new Date(),
    status: 'potential',
    slots: [
        {
            id: 'parent-slot-1',
            name: 'Parent Slot (Needs Child)',
            status: 'potential',
            input: { kind: 'proffer', proffer_id: childProffer.id },
            optional: false,
            // "Filled" with the child proffer ID, but child is not ready yet!
            actually_filled_by_refs: { [childProffer.id]: true }
        }
    ]
};

globalProfferRegistry.addProffer(childProffer);
globalProfferRegistry.addProffer(parentProffer);

console.log("--- Testing Recursive Status Logic ---");

// Test 1: Child is Potential -> Parent Slot should be Potential (even though filled)
let parentSlotStatus = globalProfferRegistry.deriveSlotStatus(parentProffer.slots[0]);
console.log(`Child is Potential. Parent Slot Status: ${parentSlotStatus} (Expected: potential)`);

if (parentSlotStatus !== 'potential') {
    console.error("FAIL: Parent slot should be potential because child is potential");
}

let parentProfferStatus = globalProfferRegistry.deriveProfferStatus(parentProffer);
console.log(`Parent Proffer Status: ${parentProfferStatus} (Expected: potential)`);


// Test 2: Make Child Actual
// To make child actual, we must fill its required slot
childProffer.slots[0].actually_filled_by_refs = { "some-string-input": true };
// Note: "some-string-input" is not a proffer ID, so it defaults to 'actual' (leaf resource/value)
globalProfferRegistry.updateProffer(childProffer);

// Check Child Status
let childStatus = globalProfferRegistry.deriveProfferStatus(childProffer);
console.log(`Child Proffer Status after filling: ${childStatus} (Expected: actual)`);

// Test 3: Check Parent Status again
parentSlotStatus = globalProfferRegistry.deriveSlotStatus(parentProffer.slots[0]);
console.log(`Child is Actual. Parent Slot Status: ${parentSlotStatus} (Expected: actual)`);

if (parentSlotStatus !== 'actual') {
    console.error("FAIL: Parent slot should be actual now that child is actual");
}

parentProfferStatus = globalProfferRegistry.deriveProfferStatus(parentProffer);
console.log(`Parent Proffer Status: ${parentProfferStatus} (Expected: actual)`);

