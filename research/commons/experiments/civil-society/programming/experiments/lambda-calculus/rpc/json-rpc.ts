/**
 * Simple JSON-RPC Serialization (Cap'n Web Style)
 * 
 * "Just JSON" with minimal escape sequences.
 * Much simpler than our previous serialization.ts!
 * 
 * Handles:
 * - Maps (as arrays of entries)
 * - ITC Stamps (as objects)
 * - Sparse Graphs (as edge lists)
 * - Dates (as timestamps)
 * 
 * Everything else: standard JSON
 */

import type { Stamp as ITCStamp } from '../itc';
import type { SparseRecognitionGraph } from '../src/sparse/types';

// ============================================================================
// Type Tags
// ============================================================================

const TYPE_MAP = 'Map';
const TYPE_STAMP = 'Stamp';
const TYPE_SPARSE = 'Sparse';
const TYPE_DATE = 'Date';

// ============================================================================
// Serialization (JSON.stringify replacer)
// ============================================================================

function replacer(key: string, value: unknown): unknown {
  // Map → { _type: 'Map', entries: [...] }
  if (value instanceof Map) {
    return {
      _type: TYPE_MAP,
      entries: Array.from(value.entries())
    };
  }

  // ITC Stamp → { _type: 'Stamp', id, event }
  if (isStamp(value)) {
    return {
      _type: TYPE_STAMP,
      id: value.id,
      event: value.event
    };
  }

  // Sparse Graph → { _type: 'Sparse', edges: [...] }
  if (isSparseGraph(value)) {
    const edges: [string, string, number][] = [];
    for (const [from, targets] of value.edges) {
      for (const [to, weight] of targets) {
        edges.push([from, to, weight]);
      }
    }
    return {
      _type: TYPE_SPARSE,
      edges,
      metadata: value.metadata
    };
  }

  // Date → { _type: 'Date', timestamp }
  if (value instanceof Date) {
    return {
      _type: TYPE_DATE,
      timestamp: value.getTime()
    };
  }

  // Everything else: standard JSON
  return value;
}

// ============================================================================
// Deserialization (JSON.parse reviver)
// ============================================================================

function reviver(key: string, value: unknown): unknown {
  // Not an object with _type? Return as-is
  if (typeof value !== 'object' || value === null || !('_type' in value)) {
    return value;
  }

  const typed = value as { _type: string; [key: string]: unknown };

  switch (typed._type) {
    case TYPE_MAP:
      return new Map(typed.entries as [unknown, unknown][]);

    case TYPE_STAMP:
      return {
        id: typed.id,
        event: typed.event
      } as ITCStamp;

    case TYPE_SPARSE:
      const graph: SparseRecognitionGraph = {
        edges: new Map(),
        metadata: typed.metadata as any
      };
      for (const [from, to, weight] of typed.edges as [string, string, number][]) {
        if (!graph.edges.has(from)) {
          graph.edges.set(from, new Map());
        }
        graph.edges.get(from)!.set(to, weight);
      }
      return graph;

    case TYPE_DATE:
      return new Date(typed.timestamp as number);

    default:
      return value;
  }
}

// ============================================================================
// Type Guards
// ============================================================================

function isStamp(value: unknown): value is ITCStamp {
  return (
    typeof value === 'object' &&
    value !== null &&
    'id' in value &&
    'event' in value
  );
}

function isSparseGraph(value: unknown): value is SparseRecognitionGraph {
  return (
    typeof value === 'object' &&
    value !== null &&
    'edges' in value &&
    value.edges instanceof Map
  );
}

// ============================================================================
// Public API
// ============================================================================

/**
 * Simple JSON serialization with minimal escape sequences
 * 
 * Usage:
 * ```typescript
 * const json = RpcJSON.stringify(complexObject);
 * const obj = RpcJSON.parse(json);
 * ```
 */
export const RpcJSON = {
  /**
   * Serialize to JSON string
   */
  stringify(value: unknown): string {
    return JSON.stringify(value, replacer);
  },

  /**
   * Deserialize from JSON string
   */
  parse<T = unknown>(json: string): T {
    return JSON.parse(json, reviver) as T;
  },

  /**
   * Serialize to object (for embedding in other JSON)
   */
  toObject(value: unknown): unknown {
    return JSON.parse(JSON.stringify(value, replacer));
  },

  /**
   * Deserialize from object
   */
  fromObject<T = unknown>(obj: unknown): T {
    return JSON.parse(JSON.stringify(obj), reviver) as T;
  }
};

/**
 * Estimate serialized size (approximate)
 */
export function estimateSize(value: unknown): number {
  try {
    return RpcJSON.stringify(value).length;
  } catch {
    return 0;
  }
}

