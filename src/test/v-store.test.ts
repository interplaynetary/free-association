import { describe, it, expect, beforeEach, vi } from 'vitest';
import { get } from 'svelte/store';
import { VersionedStore } from '../lib/utils/primitives/v-store.svelte';

// Test data types
interface TestCommitment {
    need_slots: Array<{ id: string; quantity: number }>;
    capacity_slots: Array<{ id: string; quantity: number }>;
    recognition: Record<string, number>;
    allocations: Array<{ id: string; amount: number }>;
}

describe('VersionedStore - deriveField', () => {
    let store: VersionedStore<string, TestCommitment>;

    beforeEach(() => {
        // Create a store with field extractors matching the real commitment store
        store = new VersionedStore<string, TestCommitment>({
            fields: {
                needs: (c) => c.need_slots,
                capacity: (c) => c.capacity_slots,
                recognition: (c) => c.recognition,
                allocations: (c) => c.allocations,
            },
        });
    });

    describe('Basic field extraction', () => {
        it('should extract a field from a single entity', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [{ id: 'cap1', quantity: 50 }],
                recognition: { user1: 0.5 },
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(1);
            expect(needsMap.get('user1')).toEqual([{ id: 'need1', quantity: 100 }]);
        });

        it('should extract fields from multiple entities', () => {
            const commitment1: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            const commitment2: TestCommitment = {
                need_slots: [{ id: 'need2', quantity: 200 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment1);
            store.update('user2', commitment2);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(2);
            expect(needsMap.get('user1')).toEqual([{ id: 'need1', quantity: 100 }]);
            expect(needsMap.get('user2')).toEqual([{ id: 'need2', quantity: 200 }]);
        });

        it('should handle empty arrays', () => {
            const commitment: TestCommitment = {
                need_slots: [],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(1);
            expect(needsMap.get('user1')).toEqual([]);
        });
    });

    describe('Field version tracking', () => {
        it('should only update when the specific field changes', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [{ id: 'cap1', quantity: 50 }],
                recognition: { user1: 0.5 },
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const subscriber = vi.fn();
            needsStore.subscribe(subscriber);

            // Initial call
            expect(subscriber).toHaveBeenCalledTimes(1);

            // Update a different field (capacity)
            store.update('user1', {
                ...commitment,
                capacity_slots: [{ id: 'cap2', quantity: 75 }],
            });

            // Should NOT trigger needs subscriber since needs didn't change
            expect(subscriber).toHaveBeenCalledTimes(1);

            // Update the needs field
            store.update('user1', {
                ...commitment,
                need_slots: [{ id: 'need2', quantity: 200 }],
            });

            // Should trigger needs subscriber
            expect(subscriber).toHaveBeenCalledTimes(2);
        });

        it('should track versions independently for each entity', () => {
            const commitment1: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            const commitment2: TestCommitment = {
                need_slots: [{ id: 'need2', quantity: 200 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment1);
            store.update('user2', commitment2);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const subscriber = vi.fn();
            needsStore.subscribe(subscriber);

            // Initial call
            expect(subscriber).toHaveBeenCalledTimes(1);

            // Update user1's needs
            store.update('user1', {
                ...commitment1,
                need_slots: [{ id: 'need1_updated', quantity: 150 }],
            });

            // Should trigger subscriber
            expect(subscriber).toHaveBeenCalledTimes(2);

            // Verify user2's needs are unchanged
            const needsMap = get(needsStore);
            expect(needsMap.get('user2')).toEqual([{ id: 'need2', quantity: 200 }]);
        });
    });

    describe('Reactivity', () => {
        it('should update derived store when field changes', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');

            // Initial state
            let needsMap = get(needsStore);
            expect(needsMap.get('user1')).toEqual([{ id: 'need1', quantity: 100 }]);

            // Update needs
            store.update('user1', {
                ...commitment,
                need_slots: [{ id: 'need2', quantity: 200 }],
            });

            // Should reflect new value
            needsMap = get(needsStore);
            expect(needsMap.get('user1')).toEqual([{ id: 'need2', quantity: 200 }]);
        });

        it('should handle entity deletion', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment);
            store.update('user2', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');

            // Initial state
            let needsMap = get(needsStore);
            expect(needsMap.size).toBe(2);

            // Delete user1
            store.delete('user1');

            // Should reflect deletion
            needsMap = get(needsStore);
            expect(needsMap.size).toBe(1);
            expect(needsMap.has('user1')).toBe(false);
            expect(needsMap.has('user2')).toBe(true);
        });
    });

    describe('Edge cases', () => {
        it('should handle undefined field values', () => {
            const commitment = {
                capacity_slots: [],
                recognition: {},
                allocations: [],
            } as any; // Missing need_slots

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(1);
            expect(needsMap.get('user1')).toBeUndefined();
        });

        it('should handle null field values', () => {
            const commitment = {
                need_slots: null,
                capacity_slots: [],
                recognition: {},
                allocations: [],
            } as any;

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(1);
            expect(needsMap.get('user1')).toBeNull();
        });

        it('should throw error for non-existent field', () => {
            expect(() => {
                store.deriveField('nonexistent');
            }).toThrow('Field "nonexistent" not found in store configuration');
        });
    });

    describe('Multiple derived stores', () => {
        it('should support multiple independent derived stores', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [{ id: 'cap1', quantity: 50 }],
                recognition: { user1: 0.5 },
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const capacityStore = store.deriveField<Array<{ id: string; quantity: number }>>('capacity');

            const needsMap = get(needsStore);
            const capacityMap = get(capacityStore);

            expect(needsMap.get('user1')).toEqual([{ id: 'need1', quantity: 100 }]);
            expect(capacityMap.get('user1')).toEqual([{ id: 'cap1', quantity: 50 }]);
        });

        it('should update only the relevant derived store', () => {
            const commitment: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [{ id: 'cap1', quantity: 50 }],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const capacityStore = store.deriveField<Array<{ id: string; quantity: number }>>('capacity');

            const needsSubscriber = vi.fn();
            const capacitySubscriber = vi.fn();

            needsStore.subscribe(needsSubscriber);
            capacityStore.subscribe(capacitySubscriber);

            // Initial calls
            expect(needsSubscriber).toHaveBeenCalledTimes(1);
            expect(capacitySubscriber).toHaveBeenCalledTimes(1);

            // Update only capacity
            store.update('user1', {
                ...commitment,
                capacity_slots: [{ id: 'cap2', quantity: 75 }],
            });

            // Only capacity subscriber should be called
            expect(needsSubscriber).toHaveBeenCalledTimes(1);
            expect(capacitySubscriber).toHaveBeenCalledTimes(2);
        });
    });

    describe('Real-world scenario: Network commitments', () => {
        it('should handle the exact scenario from the bug report', () => {
            // Simulate the needer's commitment being received by the provider
            const neederCommitment: TestCommitment = {
                need_slots: [
                    {
                        id: 'need_1766793865125_0.20096940607423786',
                        quantity: 100,
                    },
                ],
                capacity_slots: [
                    {
                        id: 'capacity_1766793859084_0.9819772125972978',
                        quantity: 20,
                    },
                ],
                recognition: {
                    'uS-ytluRW3AtvnnTvJ6V...': 0.3696,
                },
                allocations: [],
            };

            // Provider receives the needer's commitment
            store.update('uS-ytluRW3AtvnnTvJ6V...', neederCommitment);

            // Provider derives the needs field
            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            // This should NOT be empty!
            expect(needsMap.size).toBe(1);
            expect(needsMap.get('uS-ytluRW3AtvnnTvJ6V...')).toEqual([
                {
                    id: 'need_1766793865125_0.20096940607423786',
                    quantity: 100,
                },
            ]);
        });

        it('should handle multiple network commitments', () => {
            const commitment1: TestCommitment = {
                need_slots: [{ id: 'need1', quantity: 100 }],
                capacity_slots: [{ id: 'cap1', quantity: 50 }],
                recognition: {},
                allocations: [],
            };

            const commitment2: TestCommitment = {
                need_slots: [{ id: 'need2', quantity: 200 }],
                capacity_slots: [{ id: 'cap2', quantity: 75 }],
                recognition: {},
                allocations: [],
            };

            const commitment3: TestCommitment = {
                need_slots: [],
                capacity_slots: [{ id: 'cap3', quantity: 100 }],
                recognition: {},
                allocations: [],
            };

            store.update('user1', commitment1);
            store.update('user2', commitment2);
            store.update('user3', commitment3);

            const needsStore = store.deriveField<Array<{ id: string; quantity: number }>>('needs');
            const needsMap = get(needsStore);

            expect(needsMap.size).toBe(3);
            expect(needsMap.get('user1')).toEqual([{ id: 'need1', quantity: 100 }]);
            expect(needsMap.get('user2')).toEqual([{ id: 'need2', quantity: 200 }]);
            expect(needsMap.get('user3')).toEqual([]);
        });
    });
});
