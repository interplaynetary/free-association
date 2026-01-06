
import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { get } from 'svelte/store';
import { createVersionedStore } from '$lib/utils/primitives/v-store.svelte';

describe('VersionedStore Liveness (Ghost/Zombie Filtering)', () => {
    let mockDateNow: any;
    let currentTime = 1000;

    beforeEach(() => {
        // Mock Date.now() to control time
        mockDateNow = vi.spyOn(Date, 'now').mockImplementation(() => currentTime);
    });

    afterEach(() => {
        mockDateNow.mockRestore();
    });

    const advanceTime = (ms: number) => {
        currentTime += ms;
    };

    interface TestEntity {
        id: string;
        value: number;
    }

    it('should filter out stale entities from deriveLiveField', () => {
        const store = createVersionedStore<TestEntity>({
            fields: { value: (e) => e.value },
            enableLogging: false
        });

        // 1. T=1000: Add two active entities
        store.update('A', { id: 'A', value: 10 });
        store.update('B', { id: 'B', value: 20 });

        // Create live derived store with 5000ms horizon
        const liveStore = store.deriveLiveField<number>('value', 5000);

        // Check initial state
        let map = get(liveStore);
        expect(map.size).toBe(2);
        expect(map.get('A')).toBe(10);
        expect(map.get('B')).toBe(20);

        // 2. T=3000: Update A only
        advanceTime(2000);
        store.update('A', { id: 'A', value: 11 }); // A's lastUpdate becomes 3000

        map = get(liveStore);
        expect(map.get('A')).toBe(11);
        expect(map.get('B')).toBe(20); // B is still valid (lastUpdate=1000, age=2000 < 5000)

        // 3. T=7000: B becomes stale
        // Current Time = 7000
        // A: lastUpdate=3000, age=4000 (VALID)
        // B: lastUpdate=1000, age=6000 (STALE > 5000)
        advanceTime(4000);

        // Trigger a store update to refresh derived stores (derived stores react to store changes)
        // In a real app, we need a mechanism to trigger re-evaluation periodically or on any update.
        // For this test, updating A again triggers evaluation.
        store.update('A', { id: 'A', value: 12 });

        map = get(liveStore);
        expect(map.has('A')).toBe(true);
        expect(map.has('B')).toBe(false); // B should be gone
    });

    it('should filter out stale entries from deriveLiveAggregatedMap', () => {
        interface MapEntity {
            metrics: Record<string, number>;
        }

        const store = createVersionedStore<MapEntity>({
            fields: { metrics: (e) => e.metrics },
            enableLogging: false
        });

        const liveAggStore = store.deriveLiveAggregatedMap<number>('metrics', 5000);

        // T=1000
        store.update('P1', { metrics: { need_1: 100 } });
        store.update('P2', { metrics: { need_1: 50, need_2: 50 } });

        let agg = get(liveAggStore);
        expect(agg.need_1).toBe(50); // Last write wins usually, or merge logic. deriveAggregatedMap merges.
        // Wait, standard deriveAggregatedMap just iterates and assigns. Order depends on Map iteration order.
        // P2 overwrote P1 for need_1? deriveAggregatedMap implementation:
        // for (const [_, entity] of $dataMap) Object.assign(aggregated, fieldValue);
        // Yes, last visited overwrites.

        // T=7000: P1 updates, P2 goes stale
        advanceTime(6000);
        store.update('P1', { metrics: { need_1: 110 } }); // P1 alive

        agg = get(liveAggStore);

        // P2 (lastUpdate=1000) is stale (age 6000 > 5000) -> Should be ignored
        // P1 (lastUpdate=7000) is alive -> Should be included
        expect(agg.need_1).toBe(110);
        expect(agg.need_2).toBeUndefined(); // need_2 came only from P2
    });
});
