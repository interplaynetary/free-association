import { describe, it, expect, beforeEach } from 'vitest';
import {
    globalProfferRegistry,
    createSlotWithId,
    createProfferWithId,
    type ProfferInstanceCore,
    type ProfferInstanceMeta
} from './proffer-v2';
import { nanoid } from 'nanoid';

describe('Slot Unfill Propagation', () => {
    beforeEach(() => {
        globalProfferRegistry.clear();
    });

    it('should propagate unfill from dependency to dependent proffers', async () => {
        // Create a simple generic slot
        const genericSlot = await createSlotWithId({
            name: 'Resource Input',
            input: {
                kind: 'generic',
                data_type: 'string',
                description: 'A generic resource'
            },
            optional: false
        });

        // Create Proffer A (base proffer)
        const profferA = await createProfferWithId({
            name: 'Base Service',
            slots: [genericSlot]
        });

        const instanceA_id = nanoid();
        const coreA: ProfferInstanceCore = {
            instance_id: instanceA_id,
            proffer: profferA,
            author: 'did:example:alice',
            slotInstances: {
                [genericSlot.id]: {
                    slot_id: genericSlot.id,
                    instance_id: nanoid(),
                    actually_filled_by_refs: { 'resource_123': 'value' },
                    status: 'actual'
                }
            }
        };

        const metaA: ProfferInstanceMeta = {
            created_at: new Date(),
            updated_at: new Date()
        };

        globalProfferRegistry.addProfferInstance(coreA, metaA);

        // Create a slot that depends on Proffer A
        const profferSlot = await createSlotWithId({
            name: 'Service Input',
            input: {
                kind: 'proffer',
                instance_id: instanceA_id
            },
            optional: false
        });

        // Create Proffer B (depends on A)
        const profferB = await createProfferWithId({
            name: 'Dependent Service',
            slots: [profferSlot]
        });

        const instanceB_id = nanoid();
        const coreB: ProfferInstanceCore = {
            instance_id: instanceB_id,
            proffer: profferB,
            author: 'did:example:bob',
            slotInstances: {
                [profferSlot.id]: {
                    slot_id: profferSlot.id,
                    instance_id: nanoid(),
                    actually_filled_by_refs: { [instanceA_id]: true },
                    status: 'actual'
                }
            }
        };

        const metaB: ProfferInstanceMeta = {
            created_at: new Date(),
            updated_at: new Date()
        };

        globalProfferRegistry.addProfferInstance(coreB, metaB);

        // Verify both are initially actual
        const initialA = globalProfferRegistry.getProfferInstance(instanceA_id);
        const initialB = globalProfferRegistry.getProfferInstance(instanceB_id);

        expect(initialA?.status).toBe('actual');
        expect(initialB?.status).toBe('actual');

        // Unfill the generic slot in Proffer A
        const affected = globalProfferRegistry.unfillSlot(instanceA_id, genericSlot.id);

        // Verify propagation
        expect(affected).toContain(instanceA_id);
        expect(affected).toContain(instanceB_id);
        expect(affected.length).toBe(2);

        // Verify both are now potential
        const updatedA = globalProfferRegistry.getProfferInstance(instanceA_id);
        const updatedB = globalProfferRegistry.getProfferInstance(instanceB_id);

        expect(updatedA?.status).toBe('potential');
        expect(updatedB?.status).toBe('potential');

        // Verify the slot in B no longer references A
        const slotInstanceB = updatedB?.slotInstances[profferSlot.id];
        expect(slotInstanceB?.actually_filled_by_refs).toBeUndefined();
        expect(slotInstanceB?.status).toBe('potential');
    });

    it('should propagate fill from dependency to dependent proffers', async () => {
        // Create a simple generic slot
        const genericSlot = await createSlotWithId({
            name: 'Resource Input',
            input: {
                kind: 'generic',
                data_type: 'string',
                description: 'A generic resource'
            },
            optional: false
        });

        // Create Proffer A (initially potential)
        const profferA = await createProfferWithId({
            name: 'Base Service',
            slots: [genericSlot]
        });

        const instanceA_id = nanoid();
        const coreA: ProfferInstanceCore = {
            instance_id: instanceA_id,
            proffer: profferA,
            author: 'did:example:alice',
            slotInstances: {
                [genericSlot.id]: {
                    slot_id: genericSlot.id,
                    instance_id: nanoid(),
                    potential_filled_by_refs: { 'resource_123': 'value' },
                    status: 'potential'
                }
            }
        };

        const metaA: ProfferInstanceMeta = {
            created_at: new Date(),
            updated_at: new Date()
        };

        globalProfferRegistry.addProfferInstance(coreA, metaA);

        // Create Proffer B (depends on A, also potential)
        const profferSlot = await createSlotWithId({
            name: 'Service Input',
            input: {
                kind: 'proffer',
                instance_id: instanceA_id
            },
            optional: false
        });

        const profferB = await createProfferWithId({
            name: 'Dependent Service',
            slots: [profferSlot]
        });

        const instanceB_id = nanoid();
        const coreB: ProfferInstanceCore = {
            instance_id: instanceB_id,
            proffer: profferB,
            author: 'did:example:bob',
            slotInstances: {
                [profferSlot.id]: {
                    slot_id: profferSlot.id,
                    instance_id: nanoid(),
                    potential_filled_by_refs: { [instanceA_id]: true },
                    status: 'potential'
                }
            }
        };

        const metaB: ProfferInstanceMeta = {
            created_at: new Date(),
            updated_at: new Date()
        };

        globalProfferRegistry.addProfferInstance(coreB, metaB);

        // Verify both are initially potential
        const initialA = globalProfferRegistry.getProfferInstance(instanceA_id);
        const initialB = globalProfferRegistry.getProfferInstance(instanceB_id);

        expect(initialA?.status).toBe('potential');
        expect(initialB?.status).toBe('potential');

        // Fill the generic slot in Proffer A
        const affected = globalProfferRegistry.fillSlot(
            instanceA_id,
            genericSlot.id,
            { 'resource_123': 'value' }
        );

        // Verify propagation
        expect(affected).toContain(instanceA_id);
        expect(affected).toContain(instanceB_id);

        // Verify A is now actual
        const updatedA = globalProfferRegistry.getProfferInstance(instanceA_id);
        expect(updatedA?.status).toBe('actual');

        // Verify B has been updated (promoted potential to actual)
        const updatedB = globalProfferRegistry.getProfferInstance(instanceB_id);
        const slotInstanceB = updatedB?.slotInstances[profferSlot.id];

        expect(slotInstanceB?.actually_filled_by_refs?.[instanceA_id]).toBeDefined();
        expect(slotInstanceB?.status).toBe('actual');
        expect(updatedB?.status).toBe('actual');
    });

    it('should handle multi-level dependency chains', async () => {
        // Create three proffers in a chain: A -> B -> C
        const slotA = await createSlotWithId({
            name: 'Base Resource',
            input: { kind: 'generic', data_type: 'string' },
            optional: false
        });

        const profferA = await createProfferWithId({
            name: 'Level 1',
            slots: [slotA]
        });

        const idA = nanoid();
        globalProfferRegistry.addProfferInstance(
            {
                instance_id: idA,
                proffer: profferA,
                author: 'did:example:alice',
                slotInstances: {
                    [slotA.id]: {
                        slot_id: slotA.id,
                        instance_id: nanoid(),
                        actually_filled_by_refs: { 'res_1': 'val' },
                        status: 'actual'
                    }
                }
            },
            { created_at: new Date(), updated_at: new Date() }
        );

        const slotB = await createSlotWithId({
            name: 'Depends on A',
            input: { kind: 'proffer', instance_id: idA },
            optional: false
        });

        const profferB = await createProfferWithId({
            name: 'Level 2',
            slots: [slotB]
        });

        const idB = nanoid();
        globalProfferRegistry.addProfferInstance(
            {
                instance_id: idB,
                proffer: profferB,
                author: 'did:example:bob',
                slotInstances: {
                    [slotB.id]: {
                        slot_id: slotB.id,
                        instance_id: nanoid(),
                        actually_filled_by_refs: { [idA]: true },
                        status: 'actual'
                    }
                }
            },
            { created_at: new Date(), updated_at: new Date() }
        );

        const slotC = await createSlotWithId({
            name: 'Depends on B',
            input: { kind: 'proffer', instance_id: idB },
            optional: false
        });

        const profferC = await createProfferWithId({
            name: 'Level 3',
            slots: [slotC]
        });

        const idC = nanoid();
        globalProfferRegistry.addProfferInstance(
            {
                instance_id: idC,
                proffer: profferC,
                author: 'did:example:charlie',
                slotInstances: {
                    [slotC.id]: {
                        slot_id: slotC.id,
                        instance_id: nanoid(),
                        actually_filled_by_refs: { [idB]: true },
                        status: 'actual'
                    }
                }
            },
            { created_at: new Date(), updated_at: new Date() }
        );

        // All should be actual
        expect(globalProfferRegistry.getProfferInstance(idA)?.status).toBe('actual');
        expect(globalProfferRegistry.getProfferInstance(idB)?.status).toBe('actual');
        expect(globalProfferRegistry.getProfferInstance(idC)?.status).toBe('actual');

        // Unfill the base resource in A
        const affected = globalProfferRegistry.unfillSlot(idA, slotA.id);

        // All three should be affected
        expect(affected).toContain(idA);
        expect(affected).toContain(idB);
        expect(affected).toContain(idC);
        expect(affected.length).toBe(3);

        // All should now be potential
        expect(globalProfferRegistry.getProfferInstance(idA)?.status).toBe('potential');
        expect(globalProfferRegistry.getProfferInstance(idB)?.status).toBe('potential');
        expect(globalProfferRegistry.getProfferInstance(idC)?.status).toBe('potential');
    });
});
