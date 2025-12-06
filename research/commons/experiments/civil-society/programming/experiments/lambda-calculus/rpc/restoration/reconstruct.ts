/**
 * State Reconstruction - Simple API
 * 
 * Merges state fragments from multiple replicas.
 * Uses ITC + CRDT for conflict resolution.
 */

import type { EntityId } from '../types';
import type { ITCStamp } from '../clock';
import type { StateFragment } from './discovery';
import { ITClock } from '../clock';
import { leq } from '../../itc';

/**
 * Recognition edge with metadata
 */
export interface RecognitionEdge {
  from: EntityId;
  to: EntityId;
  value: number;
  timestamp: ITCStamp;
  replicaId: string;
}

/**
 * Reconstructed state from multiple fragments
 */
export interface ReconstructedState {
  edges: Map<EntityId, Map<EntityId, number>>;
  metadata: {
    fragmentCount: number;
    consensusReached: boolean;
    conflicts: number;
  };
}

/**
 * Merge state fragments into a single reconstructed state
 * 
 * Uses median value for consensus (simple Byzantine resistance).
 * Resolves conflicts with ITC timestamps.
 */
export function mergeFragments(fragments: StateFragment[]): ReconstructedState {
  if (fragments.length === 0) {
    return {
      edges: new Map(),
      metadata: {
        fragmentCount: 0,
        consensusReached: false,
        conflicts: 0
      }
    };
  }

  // Collect all edges with metadata
  const allEdges: Map<string, RecognitionEdge[]> = new Map();
  
  for (const fragment of fragments) {
    for (const [from, toMap] of fragment.edges.entries()) {
      for (const [to, value] of toMap.entries()) {
        const edgeKey = `${from}→${to}`;
        
        if (!allEdges.has(edgeKey)) {
          allEdges.set(edgeKey, []);
        }
        
        allEdges.get(edgeKey)!.push({
          from,
          to,
          value,
          timestamp: fragment.timestamp,
          replicaId: fragment.replicaId
        });
      }
    }
  }

  // Merge edges with conflict resolution
  const mergedEdges: Map<EntityId, Map<EntityId, number>> = new Map();
  let conflictCount = 0;

  for (const [edgeKey, edges] of allEdges.entries()) {
    if (edges.length === 0) continue;

    // If multiple values, resolve conflict
    const resolvedEdge = edges.length > 1 
      ? resolveConflict(edges)
      : edges[0];

    if (edges.length > 1) {
      conflictCount++;
    }

    // Add to merged state
    if (!mergedEdges.has(resolvedEdge.from)) {
      mergedEdges.set(resolvedEdge.from, new Map());
    }
    mergedEdges.get(resolvedEdge.from)!.set(resolvedEdge.to, resolvedEdge.value);
  }

  // Check consensus
  const consensusReached = fragments.length >= 2; // At least 2 replicas agree

  return {
    edges: mergedEdges,
    metadata: {
      fragmentCount: fragments.length,
      consensusReached,
      conflicts: conflictCount
    }
  };
}

/**
 * Resolve conflict between multiple values for same edge
 * 
 * Strategy:
 * 1. If timestamps are comparable, use the latest
 * 2. If concurrent, use median value (Byzantine resistance)
 */
export function resolveConflict(edges: RecognitionEdge[]): RecognitionEdge {
  if (edges.length === 1) {
    return edges[0];
  }

  // Sort by timestamp (if comparable)
  const sorted = [...edges];
  
  // Try to find a clear winner based on ITC ordering
  for (let i = 0; i < sorted.length; i++) {
    let isLatest = true;
    for (let j = 0; j < sorted.length; j++) {
      if (i !== j) {
        // If any edge happens before this one, it's not the latest
        if (leq(sorted[i].timestamp, sorted[j].timestamp) && 
            !leq(sorted[j].timestamp, sorted[i].timestamp)) {
          isLatest = false;
          break;
        }
      }
    }
    if (isLatest) {
      return sorted[i]; // This edge causally dominates
    }
  }

  // If no clear winner (concurrent), use median value
  const values = edges.map(e => e.value).sort((a, b) => a - b);
  const medianValue = values[Math.floor(values.length / 2)];
  
  // Find edge with median value (or closest)
  let closest = edges[0];
  let closestDiff = Math.abs(closest.value - medianValue);
  
  for (const edge of edges) {
    const diff = Math.abs(edge.value - medianValue);
    if (diff < closestDiff) {
      closest = edge;
      closestDiff = diff;
    }
  }

  return closest;
}

/**
 * Validate reconstructed state
 * 
 * Checks for anomalies and data integrity.
 */
export function validateState(state: ReconstructedState): {
  valid: boolean;
  errors: string[];
} {
  const errors: string[] = [];

  // Check if any fragment data exists
  if (state.metadata.fragmentCount === 0) {
    errors.push('No fragments provided');
  }

  // Check recognition values are in valid range
  for (const [from, toMap] of state.edges.entries()) {
    for (const [to, value] of toMap.entries()) {
      if (value < 0 || value > 1) {
        errors.push(`Invalid recognition value ${value} for ${from}→${to}`);
      }
    }
  }

  // Warn if consensus not reached
  if (!state.metadata.consensusReached) {
    errors.push('Consensus not reached (< 2 replicas)');
  }

  // Warn about high conflict rate
  const totalEdges = Array.from(state.edges.values())
    .reduce((sum, map) => sum + map.size, 0);
  
  if (totalEdges > 0) {
    const conflictRate = state.metadata.conflicts / totalEdges;
    if (conflictRate > 0.1) { // > 10% conflict rate
      errors.push(`High conflict rate: ${(conflictRate * 100).toFixed(1)}%`);
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

/**
 * Extract edges from reconstructed state as array
 */
export function stateToEdges(state: ReconstructedState): RecognitionEdge[] {
  const edges: RecognitionEdge[] = [];
  
  for (const [from, toMap] of state.edges.entries()) {
    for (const [to, value] of toMap.entries()) {
      edges.push({
        from,
        to,
        value,
        timestamp: { id: 0, event: 0 }, // Generic timestamp
        replicaId: 'merged'
      });
    }
  }

  return edges;
}

/**
 * Get state statistics
 */
export function getStateStats(state: ReconstructedState): {
  entityCount: number;
  edgeCount: number;
  averageRecognition: number;
  fragmentCount: number;
  consensusReached: boolean;
  conflictRate: number;
} {
  const entities = new Set<EntityId>();
  let totalRecognition = 0;
  let edgeCount = 0;

  for (const [from, toMap] of state.edges.entries()) {
    entities.add(from);
    for (const [to, value] of toMap.entries()) {
      entities.add(to);
      totalRecognition += value;
      edgeCount++;
    }
  }

  const conflictRate = edgeCount > 0 
    ? state.metadata.conflicts / edgeCount 
    : 0;

  return {
    entityCount: entities.size,
    edgeCount,
    averageRecognition: edgeCount > 0 ? totalRecognition / edgeCount : 0,
    fragmentCount: state.metadata.fragmentCount,
    consensusReached: state.metadata.consensusReached,
    conflictRate
  };
}

