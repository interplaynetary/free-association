/**
 * Comprehensive Test Suite for Generic Versioned Store
 * 
 * Tests all critical functionality:
 * - ITC causality (join, staleness, concurrent updates)
 * - Field versioning (monotonic, independent, change detection)
 * - Derived stores (version-aware, deletion handling)
 * - Equality checking (default enhanced, custom checkers, Zod)
 * - Config validation (Zod schemas)
 * - Edge cases (empty fields, no extractors, etc.)
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { get } from 'svelte/store';
import { createVersionedStore, type VersionedStore } from '../v-store.svelte';
import {
  dateEquals,
  mapEquals,
  setEquals,
  regexpEquals,
  arrayByIdEquals,
  arrayByIdEqualsDeep,
  numericEqualsWithTolerance,
  zodEquals,
  zodArrayEquals,
  commonCheckers
} from '../utils/v-store-equality-checkers';
import { seed as itcSeed, event as itcEvent, join as itcJoin, leq as itcLeq, type Stamp as ITCStamp } from '../../utils/itc';
import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// TEST DATA TYPES
// ═══════════════════════════════════════════════════════════════════

interface TestEntity {
  id: string;
  fieldA: string;
  fieldB: number;
  timestamp: number;
  itcStamp?: ITCStamp;
}

interface EntityWithDate {
  id: string;
  name: string;
  lastSeen: Date;
  timestamp: number;
}

interface EntityWithCollections {
  id: string;
  tags: Set<string>;
  metadata: Map<string, any>;
  timestamp: number;
}

interface EntityWithSlots {
  id: string;
  slots: Array<{ id: string; value: number }>;
  timestamp: number;
  itcStamp?: ITCStamp;
}

// ═══════════════════════════════════════════════════════════════════
// TEST 1: ITC CAUSALITY - JOIN PRESERVES CONCURRENT HISTORY
// ═══════════════════════════════════════════════════════════════════

describe('ITC Causality - Join', () => {
  it('should merge concurrent ITC stamps with join', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      itcExtractor: (e) => e.itcStamp,
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    // Create concurrent stamps
    const stampA = itcEvent(itcSeed());
    const stampB = itcEvent(itcSeed()); // Concurrent with A

    // Apply first update
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000,
      itcStamp: stampA
    });

    // Apply concurrent update
    store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 2,
      timestamp: 1001,
      itcStamp: stampB
    });

    // Verify merged stamp includes both
    const metadata = store.getMetadata('key1');
    expect(metadata).toBeDefined();
    expect(metadata!.itcStamp).toBeDefined();
    
    // Both original stamps should be ≤ merged stamp
    expect(itcLeq(stampA, metadata!.itcStamp!)).toBe(true);
    expect(itcLeq(stampB, metadata!.itcStamp!)).toBe(true);
  });

  it('should correctly reject causally stale updates', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      itcExtractor: (e) => e.itcStamp,
      enableLogging: false
    });

    // Create causal chain: seed → A → B
    const seed = itcSeed();
    const stampA = itcEvent(seed);
    const stampB = itcEvent(stampA);

    // Apply latest
    const result1 = store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 2,
      timestamp: 2000,
      itcStamp: stampB
    });
    expect(result1.applied).toBe(true);

    // Try to apply earlier (stale)
    const result2 = store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000,
      itcStamp: stampA
    });
    expect(result2.applied).toBe(false);
    expect(result2.reason).toBe('ITC causal staleness');
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 2: CLOCK SKEW HANDLING
// ═══════════════════════════════════════════════════════════════════

describe('Clock Skew Resilience', () => {
  it('should accept concurrent updates despite older timestamp', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      itcExtractor: (e) => e.itcStamp,
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    // Create concurrent stamps
    const stampA = itcEvent(itcSeed());
    const stampB = itcEvent(itcSeed()); // Concurrent

    // Device A: timestamp 1000
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000,
      itcStamp: stampA
    });

    // Device B: timestamp 999 (older due to clock skew)
    const result = store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 2,
      timestamp: 999, // Older timestamp!
      itcStamp: stampB
    });

    // Should accept because ITC shows concurrent, not stale
    expect(result.applied).toBe(true);
  });

  it('should use timestamp only when ITC not available', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    // No ITC stamps - timestamp is authority
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000
    });

    // Older timestamp should be rejected
    const result = store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 2,
      timestamp: 999
    });

    expect(result.applied).toBe(false);
    expect(result.reason).toBe('Timestamp staleness');
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 3: DERIVED STORES - DELETION HANDLING
// ═══════════════════════════════════════════════════════════════════

describe('Derived Stores - Deletion', () => {
  it('should remove deleted entities from derived stores', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const fieldAStore = store.deriveField<string>('fieldA');

    // Add entity
    store.update('key1', {
      id: 'key1',
      fieldA: 'test',
      fieldB: 1,
      timestamp: 1000
    });

    let fieldAMap = get(fieldAStore);
    expect(fieldAMap.has('key1')).toBe(true);
    expect(fieldAMap.get('key1')).toBe('test');

    // Delete entity
    store.delete('key1');

    fieldAMap = get(fieldAStore);
    expect(fieldAMap.has('key1')).toBe(false);
  });

  it('should handle multiple deletions correctly', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const fieldStore = store.deriveField<string>('fieldA');

    // Add multiple entities
    store.update('key1', { id: 'key1', fieldA: 'A', fieldB: 1, timestamp: 1000 });
    store.update('key2', { id: 'key2', fieldA: 'B', fieldB: 2, timestamp: 1001 });
    store.update('key3', { id: 'key3', fieldA: 'C', fieldB: 3, timestamp: 1002 });

    let fieldMap = get(fieldStore);
    expect(fieldMap.size).toBe(3);

    // Delete two
    store.delete('key1');
    store.delete('key3');

    fieldMap = get(fieldStore);
    expect(fieldMap.size).toBe(1);
    expect(fieldMap.has('key2')).toBe(true);
    expect(fieldMap.has('key1')).toBe(false);
    expect(fieldMap.has('key3')).toBe(false);
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 4: DERIVED STORES - VERSION-AWARE TRIGGERING
// ═══════════════════════════════════════════════════════════════════

describe('Derived Stores - Version-Aware Triggering', () => {
  it('should NOT trigger when unrelated field changes', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const fieldAStore = store.deriveField<string>('fieldA');

    let triggerCount = 0;
    fieldAStore.subscribe(() => {
      triggerCount++;
    });

    const initialCount = triggerCount;

    // Update entity with fieldA unchanged
    store.update('key1', { id: 'key1', fieldA: 'A', fieldB: 1, timestamp: 1000 });
    const countAfterFirst = triggerCount;

    // Change only fieldB
    store.update('key1', { id: 'key1', fieldA: 'A', fieldB: 2, timestamp: 1001 });
    const countAfterSecond = triggerCount;

    // Should have triggered once for initial update, not for fieldB change
    expect(countAfterFirst).toBeGreaterThan(initialCount);
    expect(countAfterSecond).toBe(countAfterFirst); // NO additional trigger
  });

  it('should trigger when tracked field changes', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const fieldAStore = store.deriveField<string>('fieldA');

    let triggerCount = 0;
    let lastValue: string | undefined;
    fieldAStore.subscribe((map) => {
      triggerCount++;
      lastValue = map.get('key1');
    });

    const initialCount = triggerCount;

    // Update fieldA
    store.update('key1', { id: 'key1', fieldA: 'A', fieldB: 1, timestamp: 1000 });
    const countAfterFirst = triggerCount;
    expect(lastValue).toBe('A');

    // Change fieldA again
    store.update('key1', { id: 'key1', fieldA: 'B', fieldB: 1, timestamp: 1001 });
    const countAfterSecond = triggerCount;
    expect(lastValue).toBe('B');

    // Should have triggered twice (once per fieldA change)
    expect(countAfterFirst).toBeGreaterThan(initialCount);
    expect(countAfterSecond).toBeGreaterThan(countAfterFirst);
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 5: ENHANCED DEFAULT EQUALITY CHECKER
// ═══════════════════════════════════════════════════════════════════

describe('Enhanced Default Equality Checker', () => {
  it('should compare Date objects by timestamp', () => {
    const store = createVersionedStore<EntityWithDate>({
      fields: {
        lastSeen: (e) => e.lastSeen
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const date1 = new Date('2025-01-01T00:00:00.000Z');
    const date2 = new Date('2025-01-01T00:00:00.000Z'); // Different object, same time

    store.update('key1', {
      id: 'key1',
      name: 'Alice',
      lastSeen: date1,
      timestamp: 1000
    });

    const result = store.update('key1', {
      id: 'key1',
      name: 'Alice',
      lastSeen: date2, // Different object reference
      timestamp: 1001
    });

    // Should NOT apply because dates are equal by value
    expect(result.applied).toBe(false);
    expect(result.reason).toBe('No field changes');
  });

  it('should compare Set objects by values', () => {
    const store = createVersionedStore<EntityWithCollections>({
      fields: {
        tags: (e) => e.tags
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const set1 = new Set(['a', 'b', 'c']);
    const set2 = new Set(['a', 'b', 'c']); // Different object, same values

    store.update('key1', {
      id: 'key1',
      tags: set1,
      metadata: new Map(),
      timestamp: 1000
    });

    const result = store.update('key1', {
      id: 'key1',
      tags: set2,
      metadata: new Map(),
      timestamp: 1001
    });

    expect(result.applied).toBe(false);
    expect(result.reason).toBe('No field changes');
  });

  it('should compare Map objects by entries', () => {
    const store = createVersionedStore<EntityWithCollections>({
      fields: {
        metadata: (e) => e.metadata
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const map1 = new Map([['key1', 'val1'], ['key2', 'val2']]);
    const map2 = new Map([['key1', 'val1'], ['key2', 'val2']]); // Same entries

    store.update('key1', {
      id: 'key1',
      tags: new Set(),
      metadata: map1,
      timestamp: 1000
    });

    const result = store.update('key1', {
      id: 'key1',
      tags: new Set(),
      metadata: map2,
      timestamp: 1001
    });

    expect(result.applied).toBe(false);
    expect(result.reason).toBe('No field changes');
  });

  it('should detect Date changes correctly', () => {
    const store = createVersionedStore<EntityWithDate>({
      fields: {
        lastSeen: (e) => e.lastSeen
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const date1 = new Date('2025-01-01');
    const date2 = new Date('2025-01-02'); // Different timestamp

    store.update('key1', {
      id: 'key1',
      name: 'Alice',
      lastSeen: date1,
      timestamp: 1000
    });

    const result = store.update('key1', {
      id: 'key1',
      name: 'Alice',
      lastSeen: date2,
      timestamp: 1001
    });

    expect(result.applied).toBe(true);
    expect(result.changedFields).toContain('lastSeen');
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 6: CUSTOM EQUALITY CHECKERS
// ═══════════════════════════════════════════════════════════════════

describe('Custom Equality Checkers', () => {
  it('should use custom checker for numeric tolerance', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldB: (e) => e.fieldB
      },
      fieldEqualityCheckers: {
        fieldB: numericEqualsWithTolerance(0.01)
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1.000,
      timestamp: 1000
    });

    // Within tolerance
    const result1 = store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1.009,
      timestamp: 1001
    });
    expect(result1.applied).toBe(false); // Within tolerance

    // Outside tolerance
    const result2 = store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1.02,
      timestamp: 1002
    });
    expect(result2.applied).toBe(true); // Outside tolerance
    expect(result2.changedFields).toContain('fieldB');
  });

  it('should use arrayByIdEquals for ID-only comparison', () => {
    const store = createVersionedStore<EntityWithSlots>({
      fields: {
        slots: (e) => e.slots
      },
      fieldEqualityCheckers: {
        slots: arrayByIdEquals('id')
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      slots: [
        { id: 'slot1', value: 10 },
        { id: 'slot2', value: 20 }
      ],
      timestamp: 1000
    });

    // Same IDs, different values - should NOT trigger (ID-only comparison)
    const result1 = store.update('key1', {
      id: 'key1',
      slots: [
        { id: 'slot1', value: 15 }, // Value changed!
        { id: 'slot2', value: 25 }  // Value changed!
      ],
      timestamp: 1001
    });
    expect(result1.applied).toBe(false); // IDs unchanged

    // Different IDs - should trigger
    const result2 = store.update('key1', {
      id: 'key1',
      slots: [
        { id: 'slot1', value: 15 },
        { id: 'slot3', value: 30 } // New ID!
      ],
      timestamp: 1002
    });
    expect(result2.applied).toBe(true);
    expect(result2.changedFields).toContain('slots');
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 7: ZOD-POWERED EQUALITY
// ═══════════════════════════════════════════════════════════════════

describe('Zod-Powered Equality', () => {
  it('should normalize with Zod schema transformation', () => {
    const SlotSchema = z.object({
      id: z.string(),
      value: z.number().transform(n => Math.round(n * 100) / 100) // Round to 2 decimals
    });

    const store = createVersionedStore<EntityWithSlots>({
      fields: {
        slots: (e) => e.slots
      },
      fieldEqualityCheckers: {
        slots: zodArrayEquals(SlotSchema)
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      slots: [{ id: 'slot1', value: 1.001 }],
      timestamp: 1000
    });

    // 1.002 rounds to same as 1.001 (both become 1.00)
    const result = store.update('key1', {
      id: 'key1',
      slots: [{ id: 'slot1', value: 1.002 }],
      timestamp: 1001
    });

    // Should NOT apply because normalized values are equal
    expect(result.applied).toBe(false);
    expect(result.reason).toBe('No field changes');
  });

  it('should detect real changes after normalization', () => {
    const SlotSchema = z.object({
      id: z.string(),
      value: z.number().transform(n => Math.round(n * 100) / 100)
    });

    const store = createVersionedStore<EntityWithSlots>({
      fields: {
        slots: (e) => e.slots
      },
      fieldEqualityCheckers: {
        slots: zodArrayEquals(SlotSchema)
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      slots: [{ id: 'slot1', value: 1.00 }],
      timestamp: 1000
    });

    // 1.05 is different from 1.00 after rounding
    const result = store.update('key1', {
      id: 'key1',
      slots: [{ id: 'slot1', value: 1.05 }],
      timestamp: 1001
    });

    expect(result.applied).toBe(true);
    expect(result.changedFields).toContain('slots');
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 8: CONFIG VALIDATION
// ═══════════════════════════════════════════════════════════════════

describe('Config Validation', () => {
	it('should throw error for invalid config', () => {
		// Suppress expected error logs during this test
		const errorSpy = vi.spyOn(console, 'error').mockImplementation(() => {});
		
		expect(() => {
			createVersionedStore({
				fields: 'not an object' as any, // Invalid!
				timestampExtractor: (e: any) => e.timestamp,
				enableLogging: false
			});
		}).toThrow();
		
		errorSpy.mockRestore();
	});

	it('should warn when no extractors provided', () => {
		const consoleSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});

		createVersionedStore<TestEntity>({
			fields: {
				fieldA: (e) => e.fieldA
			},
			enableLogging: true
			// No itcExtractor or timestampExtractor!
		});

		expect(consoleSpy).toHaveBeenCalledWith(
			expect.stringContaining('No itcExtractor or timestampExtractor provided')
		);

		consoleSpy.mockRestore();
	});

	it('should warn when fields are empty', () => {
		// Suppress expected warnings during this test
		const warnSpy = vi.spyOn(console, 'warn').mockImplementation(() => {});
		const infoSpy = vi.spyOn(console, 'info').mockImplementation(() => {});

		createVersionedStore<TestEntity>({
			fields: {}, // Empty!
			timestampExtractor: (e) => e.timestamp,
			enableLogging: true
		});

		expect(warnSpy).toHaveBeenCalledWith(
			expect.stringContaining('No fields defined')
		);

		warnSpy.mockRestore();
		infoSpy.mockRestore();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST 9: FIELD VERSION TRACKING
// ═══════════════════════════════════════════════════════════════════

describe('Field Version Tracking', () => {
  it('should increment field version when field changes', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    // Initial update
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000
    });

    const v1 = store.getFieldVersion('key1', 'fieldA');
    expect(v1).toBe(1);

    // Change fieldA
    store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 1,
      timestamp: 1001
    });

    const v2 = store.getFieldVersion('key1', 'fieldA');
    expect(v2).toBe(2);
  });

  it('should NOT increment version when field unchanged', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000
    });

    const v1 = store.getFieldVersion('key1', 'fieldA');

    // Change only fieldB
    store.update('key1', {
      id: 'key1',
      fieldA: 'A', // Unchanged
      fieldB: 2,
      timestamp: 1001
    });

    const v2 = store.getFieldVersion('key1', 'fieldA');
    expect(v2).toBe(v1); // Should NOT increment
  });

  it('should track versions independently per field', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    // Initial
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000
    });

    // Change only A
    store.update('key1', {
      id: 'key1',
      fieldA: 'A2',
      fieldB: 1,
      timestamp: 1001
    });

    // Change only B
    store.update('key1', {
      id: 'key1',
      fieldA: 'A2',
      fieldB: 2,
      timestamp: 1002
    });

    // Change only A again
    store.update('key1', {
      id: 'key1',
      fieldA: 'A3',
      fieldB: 2,
      timestamp: 1003
    });

    const vA = store.getFieldVersion('key1', 'fieldA');
    const vB = store.getFieldVersion('key1', 'fieldB');

    expect(vA).toBe(3); // Changed 3 times
    expect(vB).toBe(2); // Changed 2 times
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 10: EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases', () => {
  it('should handle empty fields config (metadata-only)', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {}, // No fields!
      itcExtractor: (e) => e.itcStamp,
      enableLogging: false
    });

    const stamp1 = itcEvent(itcSeed());

    const result = store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000,
      itcStamp: stamp1
    });

    // Should apply (first update)
    expect(result.applied).toBe(true);

    // But subsequent update with no field changes should be rejected
    const stamp2 = itcEvent(stamp1);
    const result2 = store.update('key1', {
      id: 'key1',
      fieldA: 'B', // Changed but not tracked
      fieldB: 2,
      timestamp: 1001,
      itcStamp: stamp2
    });

    expect(result2.applied).toBe(false);
    expect(result2.reason).toBe('No field changes');
  });

  it('should handle undefined values correctly', () => {
    interface EntityWithOptional {
      id: string;
      required: string;
      optional?: string;
      timestamp: number;
    }

    const store = createVersionedStore<EntityWithOptional>({
      fields: {
        required: (e) => e.required,
        optional: (e) => e.optional
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      required: 'req',
      timestamp: 1000
      // optional is undefined
    });

    const result = store.update('key1', {
      id: 'key1',
      required: 'req',
      timestamp: 1001
      // optional still undefined
    });

    expect(result.applied).toBe(false);
    expect(result.reason).toBe('No field changes');
  });

  it('should handle null values correctly', () => {
    interface EntityWithNull {
      id: string;
      value: string | null;
      timestamp: number;
    }

    const store = createVersionedStore<EntityWithNull>({
      fields: {
        value: (e) => e.value
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    store.update('key1', {
      id: 'key1',
      value: null,
      timestamp: 1000
    });

    const result = store.update('key1', {
      id: 'key1',
      value: null,
      timestamp: 1001
    });

    expect(result.applied).toBe(false);
  });

  it('should handle entity not found in getData', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const data = store.getData('nonexistent');
    expect(data).toBeUndefined();
  });

  it('should handle delete of non-existent entity', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    const result = store.delete('nonexistent');
    expect(result).toBe(false);
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 11: SUBSCRIBE TO FIELD FOR KEY
// ═══════════════════════════════════════════════════════════════════

describe('Subscribe to Field for Key', () => {
  it('should only fire callback when specific field changes', () => {
    const store = createVersionedStore<TestEntity>({
      fields: {
        fieldA: (e) => e.fieldA,
        fieldB: (e) => e.fieldB
      },
      timestampExtractor: (e) => e.timestamp,
      enableLogging: false
    });

    let callbackCount = 0;
    let lastValue: string | undefined;
    let lastVersion: number = -1;

    const unsub = store.subscribeToFieldForKey<string>(
      'key1',
      'fieldA',
      (value, version) => {
        callbackCount++;
        lastValue = value;
        lastVersion = version;
      }
    );

    const initialCount = callbackCount;

    // Initial update
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 1,
      timestamp: 1000
    });

    expect(callbackCount).toBeGreaterThan(initialCount);
    expect(lastValue).toBe('A');
    const countAfterA = callbackCount;

    // Change only fieldB - should NOT fire callback
    store.update('key1', {
      id: 'key1',
      fieldA: 'A',
      fieldB: 2,
      timestamp: 1001
    });

    expect(callbackCount).toBe(countAfterA); // No additional callback

    // Change fieldA - should fire callback
    store.update('key1', {
      id: 'key1',
      fieldA: 'B',
      fieldB: 2,
      timestamp: 1002
    });

    expect(callbackCount).toBeGreaterThan(countAfterA);
    expect(lastValue).toBe('B');

    unsub();
  });
});

// ═══════════════════════════════════════════════════════════════════
// TEST 12: REAL-WORLD SCENARIO
// ═══════════════════════════════════════════════════════════════════

describe('Real-World Scenario - Multi-Device Collaboration', () => {
  it('should handle complex multi-device update sequence', () => {
    interface Document {
      id: string;
      title: string;
      content: string;
      lastModified: Date;
      tags: Set<string>;
      timestamp: number;
      itcStamp?: ITCStamp;
    }

    const store = createVersionedStore<Document>({
      fields: {
        title: (d) => d.title,
        content: (d) => d.content,
        lastModified: (d) => d.lastModified,
        tags: (d) => d.tags
      },
      itcExtractor: (d) => d.itcStamp,
      timestampExtractor: (d) => d.timestamp,
      enableLogging: false
    });

    // Device A: Initial document
    const stampA1 = itcEvent(itcSeed());
    store.update('doc1', {
      id: 'doc1',
      title: 'Draft',
      content: 'Hello',
      lastModified: new Date('2025-01-01'),
      tags: new Set(['draft']),
      timestamp: 1000,
      itcStamp: stampA1
    });

    // Device B: Concurrent edit (different seed, concurrent)
    const stampB1 = itcEvent(itcSeed());
    const resultB1 = store.update('doc1', {
      id: 'doc1',
      title: 'Draft',
      content: 'Hello World', // Different content
      lastModified: new Date('2025-01-01'),
      tags: new Set(['draft', 'wip']), // Added tag
      timestamp: 999, // Clock skew!
      itcStamp: stampB1
    });

    // Should accept despite older timestamp (concurrent per ITC)
    expect(resultB1.applied).toBe(true);

    // Device A: Another edit
    const stampA2 = itcEvent(stampA1);
    const resultA2 = store.update('doc1', {
      id: 'doc1',
      title: 'Final',
      content: 'Hello World',
      lastModified: new Date('2025-01-02'),
      tags: new Set(['draft', 'wip']),
      timestamp: 1001,
      itcStamp: stampA2
    });

    expect(resultA2.applied).toBe(true);

    // Verify final state
    const finalData = store.getData('doc1');
    expect(finalData?.title).toBe('Final');
    expect(finalData?.tags.has('wip')).toBe(true);

    // Verify ITC stamp includes all history
    const metadata = store.getMetadata('doc1');
    expect(itcLeq(stampA1, metadata!.itcStamp!)).toBe(true);
    expect(itcLeq(stampA2, metadata!.itcStamp!)).toBe(true);
    expect(itcLeq(stampB1, metadata!.itcStamp!)).toBe(true);
  });
});

