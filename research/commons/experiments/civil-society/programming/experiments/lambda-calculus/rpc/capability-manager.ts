/**
 * Capability Manager for Cap'n Web RPC
 * 
 * Manages export/import tables for capability references.
 * Tracks what has been shared with remote peer and what has been received.
 * 
 * Export ID scheme:
 * - 0: Initial export (main interface - EntitySession)
 * - Negative (-1, -2, ...): Local objects/functions sent in messages
 * - Positive (1, 2, ...): Results of "push" operations
 * 
 * Import ID scheme:
 * - 0: Initial import (remote's main interface)
 * - Negative: Remote's objects/functions (from their exports)
 * - Positive: Results we can pull
 */

import type { ExportEntry, ImportEntry } from './types';

export class CapabilityManager {
  private exports = new Map<number, ExportEntry>();
  private imports = new Map<number, ImportEntry>();
  private nextNegativeId = -1;
  private nextPositiveId = 1;

  constructor() {
    // Reserve ID 0 for main export
  }

  // ============================================================================
  // Export Operations (What we share)
  // ============================================================================

  /**
   * Export main interface at ID 0
   * Called once during connection setup
   */
  exportMain(target: unknown): void {
    this.exports.set(0, {
      id: 0,
      target,
      type: 'object',
      refCount: 1
    });
  }

  /**
   * Export local object/function
   * Returns negative ID to send to remote peer
   */
  exportLocal(target: unknown, type: 'object' | 'function' = 'object'): number {
    const id = this.nextNegativeId--;
    
    this.exports.set(id, {
      id,
      target,
      type,
      refCount: 1
    });
    
    return id;
  }

  /**
   * Reserve positive ID for push operation result
   * Called when we make a call that returns a promise
   */
  reservePositiveId(): number {
    return this.nextPositiveId++;
  }

  /**
   * Store push result at reserved ID
   */
  storePushResult(id: number, result: unknown): void {
    this.exports.set(id, {
      id,
      target: result,
      type: 'value',
      refCount: 1
    });
  }

  /**
   * Get exported target by ID
   * Used when remote peer calls our methods
   */
  getExport(id: number): unknown | undefined {
    const entry = this.exports.get(id);
    return entry?.target;
  }

  /**
   * Check if ID is exported
   */
  hasExport(id: number): boolean {
    return this.exports.has(id);
  }

  /**
   * Increment reference count
   */
  retainExport(id: number): void {
    const entry = this.exports.get(id);
    if (entry) {
      entry.refCount++;
    }
  }

  /**
   * Decrement reference count and remove if 0
   */
  releaseExport(id: number): void {
    const entry = this.exports.get(id);
    if (entry) {
      entry.refCount--;
      if (entry.refCount <= 0) {
        this.exports.delete(id);
      }
    }
  }

  // ============================================================================
  // Import Operations (What we receive)
  // ============================================================================

  /**
   * Import main interface from remote at ID 0
   * Called once during connection setup
   */
  importMain(stub: unknown): void {
    this.imports.set(0, {
      id: 0,
      stub,
      type: 'object',
      lastUsed: Date.now()
    });
  }

  /**
   * Import remote capability
   * Called when we receive a ["ref", id] from remote
   */
  importRemote(id: number, stub: unknown, type: 'object' | 'function' = 'object'): void {
    this.imports.set(id, {
      id,
      stub,
      type,
      lastUsed: Date.now()
    });
  }

  /**
   * Get imported stub by ID
   * Used when we want to call remote methods
   */
  getImport(id: number): unknown | undefined {
    const entry = this.imports.get(id);
    if (entry) {
      entry.lastUsed = Date.now();
    }
    return entry?.stub;
  }

  /**
   * Check if ID is imported
   */
  hasImport(id: number): boolean {
    return this.imports.has(id);
  }

  /**
   * Remove import
   */
  releaseImport(id: number): void {
    this.imports.delete(id);
  }

  // ============================================================================
  // Utility & Monitoring
  // ============================================================================

  /**
   * Get all export IDs
   */
  getExportIds(): number[] {
    return Array.from(this.exports.keys());
  }

  /**
   * Get all import IDs
   */
  getImportIds(): number[] {
    return Array.from(this.imports.keys());
  }

  /**
   * Get statistics
   */
  getStats(): {
    exports: number;
    imports: number;
    nextNegativeId: number;
    nextPositiveId: number;
  } {
    return {
      exports: this.exports.size,
      imports: this.imports.size,
      nextNegativeId: this.nextNegativeId,
      nextPositiveId: this.nextPositiveId
    };
  }

  /**
   * Clean up stale imports (LRU-based)
   * Removes imports not used in specified time
   */
  cleanupStaleImports(maxAge: number = 30 * 60 * 1000): number {
    const now = Date.now();
    let removed = 0;

    for (const [id, entry] of this.imports) {
      // Don't remove main import (ID 0)
      if (id === 0) continue;

      const age = now - entry.lastUsed;
      if (age > maxAge) {
        this.imports.delete(id);
        removed++;
      }
    }

    return removed;
  }

  /**
   * Clean up exports with 0 ref count
   */
  cleanupUnreferencedExports(): number {
    let removed = 0;

    for (const [id, entry] of this.exports) {
      // Don't remove main export (ID 0)
      if (id === 0) continue;

      if (entry.refCount <= 0) {
        this.exports.delete(id);
        removed++;
      }
    }

    return removed;
  }

  /**
   * Clear all capabilities (on disconnect)
   */
  releaseAll(): void {
    this.exports.clear();
    this.imports.clear();
    this.nextNegativeId = -1;
    this.nextPositiveId = 1;
  }

  /**
   * Debug: Print capability tables
   */
  debug(): {
    exports: Array<{id: number; type: string; refCount: number}>;
    imports: Array<{id: number; type: string; lastUsed: number}>;
  } {
    return {
      exports: Array.from(this.exports.values()).map(e => ({
        id: e.id,
        type: e.type,
        refCount: e.refCount
      })),
      imports: Array.from(this.imports.values()).map(i => ({
        id: i.id,
        type: i.type,
        lastUsed: i.lastUsed
      }))
    };
  }
}

