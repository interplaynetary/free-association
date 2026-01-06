
import { describe, it, expect, vi } from 'vitest';
import { get } from 'svelte/store';
import { createVersionedStore, type VersionedStore } from '$lib/utils/primitives/v-store.svelte';
import { seed, event } from '$lib/utils/primitives/itc';

interface TestEntity {
    id: string;
    description?: string;
    tags?: string[];
    itcStamp?: any;
}

describe('VersionedStore Tombstone/Deletion Logic', () => {
    it('should correctly handle field deletions (updates to undefined)', () => {
        const store = createVersionedStore<TestEntity>({
            fields: {
                description: (e) => e.description,
                tags: (e) => e.tags
            },
            itcExtractor: (e) => e.itcStamp,
            enableLogging: false
        });

        const id = 'test-1';
        let itc = seed();

        // 1. Initial State: Has description
        const entity1: TestEntity = { id, description: 'Initial', itcStamp: itc };
        store.update(id, entity1);

        const descStore = store.deriveField<string>('description');
        expect(get(descStore).get(id)).toBe('Initial');

        // 2. Update forces deletion (undefined)
        itc = event(itc); // Increment ITC to ensure it's accepted
        const entity2: TestEntity = { id, description: undefined, itcStamp: itc }; // Explicit undefined

        const result = store.update(id, entity2);

        // EXPECTATION: Update applied, field marked as changed
        expect(result.applied).toBe(true);
        expect(result.changedFields?.has('description')).toBe(true);

        // EXPECTATION: Derived store reflects deletion (missing from map or undefined)
        // deriveField sets the value returned by extractor. extractor returns undefined.
        // Map.set(key, undefined) puts undefined in the map.
        expect(get(descStore).has(id)).toBe(true);
        expect(get(descStore).get(id)).toBeUndefined();
    });

    it('should correctly handle field deletion in Aggregated Map', () => {
        // Setup specialized store for map aggregation
        interface MapEntity {
            mapField?: Record<string, number>;
            itcStamp?: any;
        }

        const store = createVersionedStore<MapEntity>({
            fields: {
                mapField: (e) => e.mapField
            },
            itcExtractor: (e) => e.itcStamp,
            enableLogging: false
        });

        const aggStore = store.deriveAggregatedMap<number>('mapField');

        // Provider A provides { x: 1 }
        let itcA = seed();
        store.update('A', { mapField: { x: 1 }, itcStamp: itcA });

        expect(get(aggStore)).toEqual({ x: 1 });

        // Provider A removes x (sends empty object) - simulates deletion of content
        itcA = event(itcA);
        store.update('A', { mapField: {}, itcStamp: itcA });

        // EXPECTATION: x is gone
        expect(get(aggStore)).toEqual({});
    });

    it('should correctly handle explicit NULL update (if supported by types)', () => {
        // Some systems might send null instead of undefined
        const store = createVersionedStore<TestEntity>({
            fields: {
                description: (e) => e.description,
            },
            itcExtractor: (e) => e.itcStamp,
            enableLogging: false
        });

        const id = 'test-null';
        let itc = seed();
        store.update(id, { id, description: 'Present', itcStamp: itc });

        itc = event(itc);
        // @ts-ignore - simulating runtime null
        store.update(id, { id, description: null, itcStamp: itc });

        const descStore = store.deriveField<string>('description');

        // If extractor returns null, Map has null.
        expect(get(descStore).get(id)).toBeNull();
    });
});
