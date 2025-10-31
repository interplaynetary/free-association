/**
 * Generic Versioned Store System
 * 
 * Combines ITC causality (entity-level) with field versions (fine-grained tracking)
 * 
 * KEY INSIGHT: ITC tracks "happened-before" for entire entity,
 *              Field versions track "what changed" within entity.
 * 
 * This enables:
 * 1. ✅ Causal consistency (ITC)
 * 2. ✅ Fine-grained reactivity (field versions)
 * 3. ✅ Selective updates (only changed fields)
 * 4. ✅ Generic (works for ANY data type)
 * 
 * Usage:
 * ```typescript
 * const commitmentStore = createVersionedStore<Commitment>({
 *   fields: {
 *     recognition: (c) => c.global_recognition_weights,
 *     needs: (c) => c.need_slots,
 *     capacity: (c) => c.capacity_slots
 *   },
 *   itcExtractor: (c) => c.itcStamp,
 *   timestampExtractor: (c) => c.timestamp
 * });
 * ```
 */

import { writable, derived, readable, get } from 'svelte/store';
import type { Readable, Writable } from 'svelte/store';
import { leq as itcLeq, equals as itcEquals, join as itcJoin, type Stamp as ITCStamp } from './itc';
import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// ZOD VALIDATION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Zod schema for validating VersionedStoreConfig
 * Provides runtime validation with helpful error messages
 * 
 * Note: Having no extractors (itcExtractor/timestampExtractor) is technically valid,
 * though not recommended. The constructor will warn about this.
 */
const VersionedStoreConfigSchema = z.object({
  fields: z.record(z.function()),
  itcExtractor: z.function().optional(),
  timestampExtractor: z.function().optional(),
  fieldEqualityCheckers: z.record(z.function()).optional(),
  enableLogging: z.boolean().optional()
});

// ═══════════════════════════════════════════════════════════════════
// GENERIC TYPES
// ═══════════════════════════════════════════════════════════════════

/**
 * Field extractor - extracts a field value from entity
 */
export type FieldExtractor<T, F> = (entity: T) => F;

/**
 * Field extractors map - defines all tracked fields
 */
export type FieldExtractors<T> = {
  [fieldName: string]: FieldExtractor<T, any>;
};

/**
 * Versioned metadata for an entity
 */
export interface VersionedMetadata {
  /** ITC stamp for causal ordering */
  itcStamp?: ITCStamp;
  
  /** Timestamp for temporal ordering (fallback) */
  timestamp?: number;
  
  /** Field versions (monotonic counters) */
  fieldVersions: Record<string, number>;
  
  /** When this was last updated locally */
  lastUpdate: number;
}

/**
 * Versioned entity wrapper
 */
export interface VersionedEntity<T> {
  /** The actual entity data */
  data: T;
  
  /** Versioning metadata */
  metadata: VersionedMetadata;
}

/**
 * Field change detection result
 */
export interface FieldChanges {
  /** Which fields changed */
  changedFields: Set<string>;
  
  /** Old field versions */
  oldVersions: Record<string, number>;
  
  /** New field versions */
  newVersions: Record<string, number>;
}

/**
 * Update result
 */
export interface UpdateResult {
  /** Whether update was applied */
  applied: boolean;
  
  /** Reason for skip (if not applied) */
  reason?: string;
  
  /** Which fields changed (if applied) */
  changedFields?: Set<string>;
}

/**
 * Configuration for versioned store
 */
export interface VersionedStoreConfig<T> {
  /** Field extractors - defines which fields to track */
  fields: FieldExtractors<T>;
  
  /** Extract ITC stamp from entity (optional) */
  itcExtractor?: (entity: T) => ITCStamp | undefined;
  
  /** Extract timestamp from entity (optional) */
  timestampExtractor?: (entity: T) => number | undefined;
  
  /** Custom equality checker per field (optional) */
  fieldEqualityCheckers?: {
    [fieldName: string]: (a: any, b: any) => boolean;
  };
  
  /** Zod schema for defensive validation (optional but recommended) */
  schema?: z.ZodType<T>;
  
  /** Log updates? (default: true) */
  enableLogging?: boolean;
}

/**
 * Internal config with defaults applied
 */
interface NormalizedStoreConfig<T> {
  fields: FieldExtractors<T>;
  itcExtractor?: (entity: T) => ITCStamp | undefined;
  timestampExtractor?: (entity: T) => number | undefined;
  fieldEqualityCheckers: {
    [fieldName: string]: (a: any, b: any) => boolean;
  };
  schema?: z.ZodType<T>;
  enableLogging: boolean;
}

// ═══════════════════════════════════════════════════════════════════
// GENERIC VERSIONED STORE
// ═══════════════════════════════════════════════════════════════════

/**
 * Generic Versioned Store
 * 
 * Manages a map of entities with ITC causality and field versioning.
 * 
 * @template T - Entity type (e.g., Commitment, RootNode, AllocationState)
 * @template K - Key type (usually string - pubKey, id, etc.)
 */
export class VersionedStore<T, K extends string = string> {
  /** Main data store (entity ID → versioned entity) */
  private dataStore: Writable<Map<K, VersionedEntity<T>>>;
  
  /** Configuration */
  private config: NormalizedStoreConfig<T>;
  
  /** Field names (extracted from config) */
  private fieldNames: string[];
  
  constructor(config: VersionedStoreConfig<T>) {
    // ✅ Zod validation with helpful error messages
    const validationResult = VersionedStoreConfigSchema.safeParse(config);
    if (!validationResult.success) {
      console.error('[VERSIONED-STORE] Invalid configuration:', validationResult.error.format());
      throw new Error(
        `[VERSIONED-STORE] Configuration validation failed: ${validationResult.error.issues
          .map(i => i.message)
          .join(', ')}`
      );
    }
    
    this.dataStore = writable(new Map());
    this.config = {
      fields: config.fields,
      itcExtractor: config.itcExtractor,
      timestampExtractor: config.timestampExtractor,
      fieldEqualityCheckers: config.fieldEqualityCheckers || {},
      schema: config.schema,
      enableLogging: config.enableLogging ?? true
    };
    this.fieldNames = Object.keys(config.fields);
    
    // ✅ Helpful warnings for edge cases
    if (this.config.enableLogging) {
      // Warn about empty fields (valid but unusual)
      if (this.fieldNames.length === 0) {
        console.warn(
          '[VERSIONED-STORE] ⚠️  No fields defined. Store will only track ITC causality metadata. ' +
          'All updates will be marked as "No field changes" unless entity is new.'
        );
      }
      
      // Warn if neither ITC nor timestamp provided (weak staleness checking)
      if (!config.itcExtractor && !config.timestampExtractor) {
        console.warn(
          '[VERSIONED-STORE] ⚠️  No itcExtractor or timestampExtractor provided. ' +
          'Staleness checking will be disabled. Consider adding at least one for better performance.'
        );
      }
      
      // Recommend ITC over timestamp for distributed systems
      if (!config.itcExtractor && config.timestampExtractor) {
        console.info(
          '[VERSIONED-STORE] 💡 Using timestamp-based staleness checking. ' +
          'Consider using ITC for better causality tracking in distributed systems (clock skew resistant).'
        );
      }
    }
  }
  
  // ═══════════════════════════════════════════════════════════════════
  // PUBLIC API
  // ═══════════════════════════════════════════════════════════════════
  
  /**
   * Get the underlying Svelte store (read-only)
   */
  get store(): Readable<Map<K, VersionedEntity<T>>> {
    return this.dataStore;
  }
  
  /**
   * Subscribe to store changes
   */
  subscribe(run: (value: Map<K, VersionedEntity<T>>) => void) {
    return this.dataStore.subscribe(run);
  }
  
  /**
   * Get current value (snapshot)
   */
  get(): Map<K, VersionedEntity<T>> {
    return get(this.dataStore);
  }
  
  /**
   * Update entity with ITC + field version tracking
   * 
   * This is the CORE function - combines causality check with field change detection!
   */
  update(key: K, entity: T): UpdateResult {
    const currentMap = get(this.dataStore);
    const existing = currentMap.get(key);
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 0: DEFENSIVE SCHEMA VALIDATION (Optional but recommended)
    // ═══════════════════════════════════════════════════════════════
    
    if (this.config.schema) {
      const validation = this.config.schema.safeParse(entity);
      if (!validation.success) {
        if (this.config.enableLogging) {
          console.error(
            `[VERSIONED-STORE] ❌ Schema validation failed for ${key.slice(0, 20)}:`,
            validation.error.format()
          );
        }
        return { 
          applied: false, 
          reason: 'Schema validation failed: ' + validation.error.issues.map(i => i.message).join(', ')
        };
      }
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 1: ITC CAUSALITY CHECK (Entity-level)
    // ═══════════════════════════════════════════════════════════════
    
    let entityITC = this.config.itcExtractor?.(entity);
    const entityTimestamp = this.config.timestampExtractor?.(entity);
    
    if (existing) {
      // ITC check (primary - most reliable)
      if (entityITC && existing.metadata.itcStamp) {
        // Check if incoming is causally stale (already seen)
        if (itcLeq(entityITC, existing.metadata.itcStamp) &&
            !itcEquals(entityITC, existing.metadata.itcStamp)) {
          if (this.config.enableLogging) {
            console.log(`[VERSIONED-STORE] ⏭️  ITC stale: ${key.slice(0, 20)}...`);
          }
          return { applied: false, reason: 'ITC causal staleness' };
        }
        
        // ✅ Merge ITC stamps to preserve causal history
        // For sequential updates (incoming > existing): join returns incoming
        // For concurrent updates (incoming || existing): join returns merged stamp
        // This preserves full causal history from both branches
        entityITC = itcJoin(existing.metadata.itcStamp, entityITC);
        
        if (this.config.enableLogging) {
          console.log(`[VERSIONED-STORE] 🔀 Merged ITC stamps for ${key.slice(0, 20)}...`);
        }
        
        // ✅ FIX: When ITC available, it's the source of truth
        // Don't use timestamp check - concurrent updates can have clock skew
        // ITC correctly handles causality regardless of timestamps
      } else {
        // ✅ ONLY use timestamp when NO ITC stamps available (fallback mode)
        // This handles legacy data or systems without ITC
        if (entityTimestamp && existing.metadata.timestamp) {
          if (entityTimestamp <= existing.metadata.timestamp) {
            if (this.config.enableLogging) {
              console.log(`[VERSIONED-STORE] ⏭️  Timestamp stale: ${key.slice(0, 20)}...`);
            }
            return { applied: false, reason: 'Timestamp staleness' };
          }
        }
      }
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 2: FIELD CHANGE DETECTION (Fine-grained)
    // ═══════════════════════════════════════════════════════════════
    
    const changes = this.detectFieldChanges(existing, entity);
    
    // No fields changed? Skip update (but update causality metadata)
    if (changes.changedFields.size === 0 && existing) {
      // Update causality metadata only
      this.dataStore.update(map => {
        const newMap = new Map(map);
        newMap.set(key, {
          ...existing,
          metadata: {
            ...existing.metadata,
            itcStamp: entityITC,
            timestamp: entityTimestamp,
            lastUpdate: Date.now()
          }
        });
        return newMap;
      });
      
      if (this.config.enableLogging) {
        console.log(`[VERSIONED-STORE] ⏭️  No field changes: ${key.slice(0, 20)}... (causality updated)`);
      }
      return { applied: false, reason: 'No field changes' };
    }
    
    // ═══════════════════════════════════════════════════════════════
    // STEP 3: UPDATE ENTITY + METADATA
    // ═══════════════════════════════════════════════════════════════
    
    this.dataStore.update(map => {
      const newMap = new Map(map);
      newMap.set(key, {
        data: entity,
        metadata: {
          itcStamp: entityITC,
          timestamp: entityTimestamp,
          fieldVersions: changes.newVersions,
          lastUpdate: Date.now()
        }
      });
      return newMap;
    });
    
    if (this.config.enableLogging) {
      const changedFieldList = Array.from(changes.changedFields).join(', ');
      console.log(`[VERSIONED-STORE] ✅ Updated [${changedFieldList}]: ${key.slice(0, 20)}...`);
    }
    
    return {
      applied: true,
      changedFields: changes.changedFields
    };
  }
  
  /**
   * Delete entity
   */
  delete(key: K): boolean {
    const currentMap = get(this.dataStore);
    if (!currentMap.has(key)) {
      return false; // Already absent
    }
    
    this.dataStore.update(map => {
      const newMap = new Map(map);
      newMap.delete(key);
      return newMap;
    });
    
    if (this.config.enableLogging) {
      console.log(`[VERSIONED-STORE] 🗑️  Deleted: ${key.slice(0, 20)}...`);
    }
    
    return true;
  }
  
  /**
   * Get entity data (without metadata)
   */
  getData(key: K): T | undefined {
    return get(this.dataStore).get(key)?.data;
  }
  
  /**
   * Get entity metadata
   */
  getMetadata(key: K): VersionedMetadata | undefined {
    return get(this.dataStore).get(key)?.metadata;
  }
  
  /**
   * Get field version for specific key and field
   */
  getFieldVersion(key: K, fieldName: string): number | undefined {
    return this.getMetadata(key)?.fieldVersions[fieldName];
  }
  
  // ═══════════════════════════════════════════════════════════════════
  // DERIVED STORES (Field-Specific)
  // ═══════════════════════════════════════════════════════════════════
  
  /**
   * Create a derived store for a specific field
   * 
   * This enables fine-grained reactivity!
   * Only recalculates when THIS FIELD changes.
   * 
   * Uses version-aware updates:
   * - Tracks field version per entity
   * - Only updates when version increments
   * - Handles entity deletions
   * - Returns same Map reference if no changes (prevents downstream triggers)
   * 
   * @example
   * ```typescript
   * const recognitionStore = commitmentStore.deriveField('recognition');
   * // Only updates when recognition field changes!
   * ```
   */
  deriveField<F>(fieldName: string): Readable<Map<K, F>> {
    const extractor = this.config.fields[fieldName];
    if (!extractor) {
      throw new Error(`Field "${fieldName}" not found in store configuration`);
    }
    
    // State maintained across updates
    let fieldMap = new Map<K, F>();
    let lastVersions = new Map<K, number>();
    
    return readable(fieldMap, (set) => {
      return this.dataStore.subscribe(($dataMap) => {
        let changed = false;
        
        // Check each entity for field version changes
        for (const [key, versionedEntity] of $dataMap.entries()) {
          const currentVersion = versionedEntity.metadata.fieldVersions[fieldName] || 0;
          const lastVersion = lastVersions.get(key) || -1;
          
          // Only update if field version changed
          if (currentVersion !== lastVersion) {
            changed = true;
            lastVersions.set(key, currentVersion);
            const fieldValue = extractor(versionedEntity.data);
            fieldMap.set(key, fieldValue);
          }
        }
        
        // ✅ FIX: Handle entity deletions
        for (const key of fieldMap.keys()) {
          if (!$dataMap.has(key)) {
            changed = true;
            fieldMap.delete(key);
            lastVersions.delete(key);
          }
        }
        
        // Only notify subscribers if field data actually changed
        if (changed) {
          fieldMap = new Map(fieldMap); // Clone for reactivity
          set(fieldMap);
        }
        // If !changed, return same Map reference (prevents downstream triggers)
      });
    });
  }
  
  /**
   * Subscribe to changes for a specific field
   * 
   * Callback only fires when THIS FIELD changes!
   * 
   * @example
   * ```typescript
   * const unsubscribe = commitmentStore.subscribeToField('recognition', (recognitionMap) => {
   *   console.log('Recognition changed!', recognitionMap);
   * });
   * ```
   */
  subscribeToField<F>(
    fieldName: string,
    callback: (fieldMap: Map<K, F>) => void
  ): () => void {
    const fieldStore = this.deriveField<F>(fieldName);
    return fieldStore.subscribe(callback);
  }
  
  /**
   * Subscribe to field changes for specific key
   * 
   * Callback only fires when THIS KEY's THIS FIELD changes!
   * 
   * @example
   * ```typescript
   * commitmentStore.subscribeToFieldForKey('alice_pub', 'recognition', (weights) => {
   *   console.log('Alice recognition changed!', weights);
   * });
   * ```
   */
  subscribeToFieldForKey<F>(
    key: K,
    fieldName: string,
    callback: (fieldValue: F | undefined, version: number) => void
  ): () => void {
    const extractor = this.config.fields[fieldName];
    if (!extractor) {
      throw new Error(`Field "${fieldName}" not found in store configuration`);
    }
    
    let lastVersion = -1;
    
    return this.dataStore.subscribe(($dataMap) => {
      const versionedEntity = $dataMap.get(key);
      if (!versionedEntity) {
        callback(undefined, -1);
        return;
      }
      
      const currentVersion = versionedEntity.metadata.fieldVersions[fieldName] || 0;
      
      // Only fire callback if version changed
      if (currentVersion !== lastVersion) {
        lastVersion = currentVersion;
        const fieldValue = extractor(versionedEntity.data);
        callback(fieldValue, currentVersion);
      }
    });
  }
  
  // ═══════════════════════════════════════════════════════════════════
  // PRIVATE HELPERS
  // ═══════════════════════════════════════════════════════════════════
  
  /**
   * Detect which fields changed
   */
  private detectFieldChanges(
    existing: VersionedEntity<T> | undefined,
    incoming: T
  ): FieldChanges {
    const changedFields = new Set<string>();
    const oldVersions: Record<string, number> = {};
    const newVersions: Record<string, number> = {};
    
    for (const fieldName of this.fieldNames) {
      const extractor = this.config.fields[fieldName];
      // ✅ FIX: Bind this context to prevent 'this' being undefined
      const equalityChecker = this.config.fieldEqualityCheckers[fieldName] || this.defaultEquals.bind(this);
      
      const oldVersion = existing?.metadata.fieldVersions[fieldName] || 0;
      oldVersions[fieldName] = oldVersion;
      
      // Extract field values
      const oldValue = existing ? extractor(existing.data) : undefined;
      const newValue = extractor(incoming);
      
      // Compare
      const changed = !equalityChecker(oldValue, newValue);
      
      if (changed) {
        changedFields.add(fieldName);
        newVersions[fieldName] = oldVersion + 1; // Increment version
      } else {
        newVersions[fieldName] = oldVersion; // Keep same version
      }
    }
    
    return { changedFields, oldVersions, newVersions };
  }
  
  /**
   * Enhanced default equality checker (deep equals with special type support)
   * 
   * Handles:
   * - ✅ Primitives (string, number, boolean, null, undefined)
   * - ✅ Plain objects (recursive)
   * - ✅ Arrays (recursive)
   * - ✅ Date objects (by timestamp)
   * - ✅ Map objects (by entries)
   * - ✅ Set objects (by values)
   * - ✅ RegExp objects (by source and flags)
   * 
   * Limitations:
   * - ❌ Functions (compared by reference)
   * - ❌ Class instances (compared by reference)
   * - ❌ Circular references (will cause stack overflow)
   * - ❌ WeakMap, WeakSet (not iterable)
   * 
   * For these special cases, provide a custom equality checker via config.
   */
  private defaultEquals(a: any, b: any): boolean {
    // Handle undefined/null
    if (a === undefined && b === undefined) return true;
    if (a === null && b === null) return true;
    if (a === undefined || b === undefined) return false;
    if (a === null || b === null) return false;
    
    // Primitive types
    if (typeof a !== 'object' || typeof b !== 'object') {
      return a === b;
    }
    
    // ✅ Date objects - compare by timestamp
    if (a instanceof Date && b instanceof Date) {
      return a.getTime() === b.getTime();
    }
    
    // ✅ RegExp objects - compare by source and flags
    if (a instanceof RegExp && b instanceof RegExp) {
      return a.source === b.source && a.flags === b.flags;
    }
    
    // ✅ Map objects - compare entries
    if (a instanceof Map && b instanceof Map) {
      if (a.size !== b.size) return false;
      for (const [key, value] of a.entries()) {
        if (!b.has(key)) return false;
        if (!this.defaultEquals(value, b.get(key))) return false;
      }
      return true;
    }
    
    // ✅ Set objects - compare values
    if (a instanceof Set && b instanceof Set) {
      if (a.size !== b.size) return false;
      for (const value of a) {
        if (!b.has(value)) return false;
      }
      return true;
    }
    
    // Arrays
    if (Array.isArray(a) && Array.isArray(b)) {
      if (a.length !== b.length) return false;
      for (let i = 0; i < a.length; i++) {
        if (!this.defaultEquals(a[i], b[i])) return false;
      }
      return true;
    }
    
    // Plain objects (recursive)
    const keysA = Object.keys(a);
    const keysB = Object.keys(b);
    if (keysA.length !== keysB.length) return false;
    
    for (const key of keysA) {
      if (!this.defaultEquals(a[key], b[key])) return false;
    }
    
    return true;
  }
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE FACTORY FUNCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a versioned store (convenience function)
 * 
 * @example
 * ```typescript
 * const store = createVersionedStore<Commitment>({
 *   fields: {
 *     recognition: (c) => c.global_recognition_weights,
 *     needs: (c) => c.need_slots,
 *     capacity: (c) => c.capacity_slots
 *   },
 *   itcExtractor: (c) => c.itcStamp,
 *   timestampExtractor: (c) => c.timestamp
 * });
 * ```
 */
export function createVersionedStore<T, K extends string = string>(
  config: VersionedStoreConfig<T>
): VersionedStore<T, K> {
  return new VersionedStore<T, K>(config);
}

