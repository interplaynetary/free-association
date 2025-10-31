/**
 * Custom Equality Checkers for Versioned Store
 * 
 * Ready-to-use equality checkers for common data types that the default
 * deep equality checker doesn't handle well.
 * 
 * @example
 * ```typescript
 * import { dateEquals, arrayByIdEquals } from './versioned-store-equality-checkers';
 * 
 * const store = createVersionedStore({
 *   fields: {
 *     lastSeen: (u) => u.lastSeen,
 *     slots: (c) => c.need_slots
 *   },
 *   fieldEqualityCheckers: {
 *     lastSeen: dateEquals,
 *     slots: arrayByIdEquals('id')
 *   }
 * });
 * ```
 */

import type { ZodSchema } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// DATE COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare Date objects by their timestamp value
 */
export function dateEquals(a: Date | undefined | null, b: Date | undefined | null): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!(a instanceof Date) || !(b instanceof Date)) return false;
  return a.getTime() === b.getTime();
}

// ═══════════════════════════════════════════════════════════════════
// MAP COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare Map objects (shallow - values compared by reference)
 */
export function mapEquals<K, V>(
  a: Map<K, V> | undefined | null,
  b: Map<K, V> | undefined | null
): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!(a instanceof Map) || !(b instanceof Map)) return false;
  if (a.size !== b.size) return false;
  
  for (const [key, value] of a.entries()) {
    if (!b.has(key)) return false;
    if (b.get(key) !== value) return false;
  }
  
  return true;
}

/**
 * Compare Map objects (deep - values compared recursively)
 */
export function mapEqualsDeep<K, V>(
  a: Map<K, V> | undefined | null,
  b: Map<K, V> | undefined | null,
  valueEquals: (a: V, b: V) => boolean = (x, y) => JSON.stringify(x) === JSON.stringify(y)
): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!(a instanceof Map) || !(b instanceof Map)) return false;
  if (a.size !== b.size) return false;
  
  for (const [key, value] of a.entries()) {
    if (!b.has(key)) return false;
    if (!valueEquals(value, b.get(key)!)) return false;
  }
  
  return true;
}

// ═══════════════════════════════════════════════════════════════════
// SET COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare Set objects
 */
export function setEquals<T>(
  a: Set<T> | undefined | null,
  b: Set<T> | undefined | null
): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!(a instanceof Set) || !(b instanceof Set)) return false;
  if (a.size !== b.size) return false;
  
  for (const value of a) {
    if (!b.has(value)) return false;
  }
  
  return true;
}

// ═══════════════════════════════════════════════════════════════════
// REGEXP COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare RegExp objects by source and flags
 */
export function regexpEquals(
  a: RegExp | undefined | null,
  b: RegExp | undefined | null
): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!(a instanceof RegExp) || !(b instanceof RegExp)) return false;
  return a.source === b.source && a.flags === b.flags;
}

// ═══════════════════════════════════════════════════════════════════
// ARRAY COMPARISON (ADVANCED)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare arrays by length only (useful for change detection without deep comparison)
 */
export function arrayLengthEquals(a: any[] | undefined | null, b: any[] | undefined | null): boolean {
  if (a === undefined && b === undefined) return true;
  if (a === null && b === null) return true;
  if (a === undefined || b === undefined) return false;
  if (a === null || b === null) return false;
  if (!Array.isArray(a) || !Array.isArray(b)) return false;
  return a.length === b.length;
}

/**
 * Compare arrays of objects by a specific ID field
 * 
 * @example
 * ```typescript
 * const slots = [{ id: 'a', value: 1 }, { id: 'b', value: 2 }];
 * arrayByIdEquals('id')(slots, slots); // true
 * ```
 */
export function arrayByIdEquals<T extends Record<string, any>>(
  idField: keyof T
): (a: T[] | undefined | null, b: T[] | undefined | null) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    if (!Array.isArray(a) || !Array.isArray(b)) return false;
    if (a.length !== b.length) return false;
    
    const aIds = new Set(a.map(item => item[idField]));
    const bIds = new Set(b.map(item => item[idField]));
    
    if (aIds.size !== bIds.size) return false;
    
    for (const id of aIds) {
      if (!bIds.has(id)) return false;
    }
    
    return true;
  };
}

/**
 * Compare arrays of objects by a specific ID field AND deep compare the objects
 */
export function arrayByIdEqualsDeep<T extends Record<string, any>>(
  idField: keyof T
): (a: T[] | undefined | null, b: T[] | undefined | null) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    if (!Array.isArray(a) || !Array.isArray(b)) return false;
    if (a.length !== b.length) return false;
    
    // Build maps by ID
    const aMap = new Map(a.map(item => [item[idField], item]));
    const bMap = new Map(b.map(item => [item[idField], item]));
    
    if (aMap.size !== bMap.size) return false;
    
    for (const [id, aItem] of aMap.entries()) {
      const bItem = bMap.get(id);
      if (!bItem) return false;
      
      // Deep compare objects
      if (JSON.stringify(aItem) !== JSON.stringify(bItem)) return false;
    }
    
    return true;
  };
}

// ═══════════════════════════════════════════════════════════════════
// NUMERIC COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare numbers with tolerance (for floating point comparisons)
 * 
 * @example
 * ```typescript
 * numericEqualsWithTolerance(0.0001)(1.0, 1.00009); // true
 * numericEqualsWithTolerance(0.0001)(1.0, 1.0002); // false
 * ```
 */
export function numericEqualsWithTolerance(
  tolerance: number
): (a: number | undefined | null, b: number | undefined | null) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    if (typeof a !== 'number' || typeof b !== 'number') return false;
    return Math.abs(a - b) <= tolerance;
  };
}

// ═══════════════════════════════════════════════════════════════════
// JSON SERIALIZATION (FALLBACK)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare by JSON serialization (works for most cases, but not optimal)
 * 
 * WARNING: Property order matters! Use only when appropriate.
 */
export function jsonEquals(a: any, b: any): boolean {
  try {
    return JSON.stringify(a) === JSON.stringify(b);
  } catch {
    return false; // Circular references or non-serializable values
  }
}

// ═══════════════════════════════════════════════════════════════════
// ZOD SCHEMA-BASED COMPARISON
// ═══════════════════════════════════════════════════════════════════

/**
 * Compare using Zod schema normalization
 * 
 * This is POWERFUL! Zod will:
 * - Parse both values
 * - Apply transformations
 * - Apply defaults
 * - Then compare the normalized results
 * 
 * @example
 * ```typescript
 * import { z } from 'zod';
 * 
 * const SlotSchema = z.object({
 *   id: z.string(),
 *   quantity: z.number().transform(n => Math.round(n * 100) / 100), // Round to 2 decimals
 *   location: z.object({ lat: z.number(), lng: z.number() }).optional()
 * });
 * 
 * const store = createVersionedStore({
 *   fields: {
 *     slots: (c) => c.capacity_slots
 *   },
 *   fieldEqualityCheckers: {
 *     slots: zodArrayEquals(SlotSchema)
 *   }
 * });
 * 
 * // Now 1.001 and 1.002 will both become 1.00 and be considered equal!
 * ```
 */
export function zodEquals<T>(schema: ZodSchema<T>): (a: any, b: any) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    
    try {
      const aParsed = schema.parse(a);
      const bParsed = schema.parse(b);
      return JSON.stringify(aParsed) === JSON.stringify(bParsed);
    } catch {
      // If parsing fails, fall back to reference equality
      return a === b;
    }
  };
}

/**
 * Compare arrays using Zod schema for each element
 */
export function zodArrayEquals<T>(elementSchema: ZodSchema<T>): (a: any, b: any) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    if (!Array.isArray(a) || !Array.isArray(b)) return false;
    if (a.length !== b.length) return false;
    
    try {
      for (let i = 0; i < a.length; i++) {
        const aParsed = elementSchema.parse(a[i]);
        const bParsed = elementSchema.parse(b[i]);
        if (JSON.stringify(aParsed) !== JSON.stringify(bParsed)) {
          return false;
        }
      }
      return true;
    } catch {
      // If parsing fails, fall back to JSON comparison
      return JSON.stringify(a) === JSON.stringify(b);
    }
  };
}

// ═══════════════════════════════════════════════════════════════════
// SPECIAL CASES
// ═══════════════════════════════════════════════════════════════════

/**
 * Always returns false (forces update every time)
 * 
 * Use when you want a field to ALWAYS trigger updates.
 */
export function alwaysDifferent(a: any, b: any): boolean {
  return false;
}

/**
 * Always returns true (ignores changes)
 * 
 * Use when you want to track a field but never trigger updates based on it.
 */
export function alwaysSame(a: any, b: any): boolean {
  return true;
}

// ═══════════════════════════════════════════════════════════════════
// COMBINATORS
// ═══════════════════════════════════════════════════════════════════

/**
 * Combine multiple equality checkers with AND logic
 * 
 * @example
 * ```typescript
 * const checker = andEquals(
 *   (a, b) => a.id === b.id,
 *   (a, b) => a.timestamp === b.timestamp
 * );
 * ```
 */
export function andEquals(
  ...checkers: Array<(a: any, b: any) => boolean>
): (a: any, b: any) => boolean {
  return (a, b) => {
    for (const checker of checkers) {
      if (!checker(a, b)) return false;
    }
    return true;
  };
}

/**
 * Combine multiple equality checkers with OR logic
 */
export function orEquals(
  ...checkers: Array<(a: any, b: any) => boolean>
): (a: any, b: any) => boolean {
  return (a, b) => {
    for (const checker of checkers) {
      if (checker(a, b)) return true;
    }
    return false;
  };
}

/**
 * Negate an equality checker
 */
export function notEquals(
  checker: (a: any, b: any) => boolean
): (a: any, b: any) => boolean {
  return (a, b) => !checker(a, b);
}

// ═══════════════════════════════════════════════════════════════════
// RECOMMENDED PRESETS
// ═══════════════════════════════════════════════════════════════════

/**
 * Recommended equality checkers for common Free Association types
 */
export const commonCheckers = {
  /** For timestamp fields (Date objects) */
  timestamp: dateEquals,
  
  /** For need/capacity slot arrays (compare by ID only) */
  slots: arrayByIdEquals('id'),
  
  /** For need/capacity slot arrays (deep comparison) */
  slotsDeep: arrayByIdEqualsDeep('id'),
  
  /** For recognition weight objects (JSON comparison) */
  recognitionWeights: jsonEquals,
  
  /** For numeric fields with floating point (0.0001 tolerance) */
  numericFloat: numericEqualsWithTolerance(0.0001),
  
  /** For any complex object (JSON fallback) */
  complex: jsonEquals
};

/**
 * Create a checker that compares objects by specific fields only
 * 
 * @example
 * ```typescript
 * const checker = byFields('id', 'timestamp');
 * checker({ id: 1, timestamp: 100, other: 'x' }, { id: 1, timestamp: 100, other: 'y' }); // true
 * ```
 */
export function byFields<T extends Record<string, any>>(
  ...fields: Array<keyof T>
): (a: T | undefined | null, b: T | undefined | null) => boolean {
  return (a, b) => {
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    
    for (const field of fields) {
      if (a[field] !== b[field]) return false;
    }
    
    return true;
  };
}

