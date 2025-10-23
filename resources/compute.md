# Distributed Computation Protocol - Implementation Guide

A **peer-to-peer distributed computation system** where work is distributed based on peer capabilities, mutual trust, and computational capacity using the Generic Distributed Reactive Protocol pattern.

**Use Cases**: MapReduce jobs, scientific simulations, rendering farms, data processing pipelines, machine learning training, blockchain validation, protein folding, climate modeling.

---

## Protocol Overview

### Core Concepts

**Work Units**: Discrete computational tasks (e.g., render frame 42, process CSV rows 1000-2000, train on batch 5)

**Peer Capabilities**: What each peer can compute (CPU cores, GPU memory, supported algorithms, data locality)

**Trust Relationships**: Bidirectional trust determines priority work assignment (prevent free-riding)

**Result Verification**: Redundant computation + consensus for Byzantine fault tolerance

---

## Domain Mapping (Generic → Compute)

| Generic Term | Compute Protocol Term | Example |
|--------------|----------------------|---------|
| State Declaration | `ComputeCapability` | "I have 8 CPU cores, 16GB RAM, GPU with 12GB VRAM" |
| Need Declaration | `WorkRequest` | "I need 100 frames rendered, each requires 2GB RAM" |
| Relationship Metric | `TrustScore` | Bidirectional trust based on result quality + contribution history |
| Result | `WorkAssignment` | "I assigned you frames 42-51 (10 units, 20GB total)" |
| Unit-Native Processing | Slot-level assignment | Each work unit assigned independently based on capabilities |
| Convergence | Load balancing equilibrium | Work distributed optimally across available peers |

---

## Schema Definitions (Layer 1)

### Core Schemas

```typescript
import { z } from 'zod';

// ============================================================================
// CAPABILITY DECLARATIONS
// ============================================================================

export const ComputeResourceSchema = z.object({
  cpu_cores: z.number().min(0),
  ram_gb: z.number().min(0),
  gpu_vram_gb: z.number().min(0).optional(),
  disk_gb: z.number().min(0).optional(),
  network_mbps: z.number().min(0).optional(),
});

export const AlgorithmCapabilitySchema = z.object({
  algorithm_id: z.string(), // "video-render", "ml-training", "data-processing"
  version: z.string(),
  supported: z.boolean(),
  performance_score: z.number().min(0).max(1), // 0-1, self-reported speed
});

export const DataLocalitySchema = z.object({
  dataset_id: z.string(),
  has_local_copy: z.boolean(),
  download_speed_mbps: z.number().min(0).optional(),
});

export const AvailabilitySlotSchema = z.object({
  slot_id: z.string(),
  start_time: z.string().datetime(),
  end_time: z.string().datetime(),
  resources: ComputeResourceSchema,
  algorithms: z.array(AlgorithmCapabilitySchema),
  data_locality: z.array(DataLocalitySchema).optional(),
  priority: z.number().min(0).max(1), // How eager to contribute (0-1)
});

// ============================================================================
// WORK REQUESTS
// ============================================================================

export const WorkUnitSchema = z.object({
  unit_id: z.string(),
  job_id: z.string(),
  algorithm_id: z.string(),
  algorithm_version: z.string(),
  required_resources: ComputeResourceSchema,
  required_dataset: z.string().optional(),
  estimated_duration_seconds: z.number().min(0),
  priority: z.number().min(0).max(1), // How urgent (0-1)
  verification_redundancy: z.number().int().min(1).default(1), // How many peers should compute this
});

export const WorkRequestSchema = z.object({
  request_id: z.string(),
  requester_id: z.string(),
  job_id: z.string(),
  work_units: z.array(WorkUnitSchema),
  deadline: z.string().datetime().optional(),
  timestamp: z.string().datetime(),
});

// ============================================================================
// COMMITMENT (State Declaration)
// ============================================================================

export const ComputeCommitmentSchema = z.object({
  peer_id: z.string(),
  availability_slots: z.array(AvailabilitySlotSchema),
  work_requests: z.array(WorkRequestSchema),
  trust_weights: z.record(z.string(), z.number().min(0).max(1)), // peerId → trust score
  contribution_history: z.object({
    total_work_units_completed: z.number().int().min(0),
    total_compute_hours: z.number().min(0),
    average_result_quality: z.number().min(0).max(1), // Based on verification
    reliability_score: z.number().min(0).max(1), // % of assignments completed
  }),
  damping_factor: z.number().min(0).max(1).default(1.0),
  over_allocation_history: z.array(z.number()).max(3),
  timestamp: z.string().datetime(),
  vector_clock: z.record(z.string(), z.number().int()),
});

// ============================================================================
// WORK ASSIGNMENT (Result)
// ============================================================================

export const WorkAssignmentSchema = z.object({
  assignment_id: z.string(),
  work_unit_id: z.string(),
  assigned_to_peer: z.string(),
  assigned_by_peer: z.string(),
  slot_id: z.string(),
  expected_start: z.string().datetime(),
  expected_end: z.string().datetime(),
  resources_allocated: ComputeResourceSchema,
  verification_role: z.enum(['primary', 'verification']), // Primary or verification compute
  timestamp: z.string().datetime(),
});

export const SlotAssignmentStateSchema = z.object({
  slot_id: z.string(),
  available_resources: ComputeResourceSchema,
  // Two-tier allocation
  tier1_assignments: z.array(WorkAssignmentSchema), // Assigned to trusted peers
  tier2_assignments: z.array(WorkAssignmentSchema), // Assigned to semi-trusted peers
  total_allocated_resources: ComputeResourceSchema,
  utilization_percent: z.number().min(0).max(100),
});

export const ComputeAllocationStateSchema = z.object({
  peer_id: z.string(),
  slot_assignments: z.array(SlotAssignmentStateSchema),
  total_work_units_assigned: z.number().int().min(0),
  timestamp: z.string().datetime(),
  vector_clock: z.record(z.string(), z.number().int()),
});

// ============================================================================
// WORK RESULTS
// ============================================================================

export const WorkResultSchema = z.object({
  result_id: z.string(),
  work_unit_id: z.string(),
  assignment_id: z.string(),
  computed_by_peer: z.string(),
  result_data: z.any(), // Protocol-specific (hash, file reference, inline data)
  computation_time_seconds: z.number().min(0),
  resources_used: ComputeResourceSchema,
  verification_hash: z.string(), // For consensus
  timestamp: z.string().datetime(),
});

export const VerificationConsensusSchema = z.object({
  work_unit_id: z.string(),
  results: z.array(WorkResultSchema),
  consensus_result: z.string().optional(), // Hash of agreed-upon result
  agreement_count: z.number().int().min(0),
  total_verifiers: z.number().int().min(1),
  is_verified: z.boolean(),
  timestamp: z.string().datetime(),
});

// ============================================================================
// COORDINATION
// ============================================================================

export const ComputeRoundStateSchema = z.object({
  peer_id: z.string(),
  round: z.number().int().min(0),
  vector_clock: z.record(z.string(), z.number().int()),
  active_jobs: z.array(z.string()), // Job IDs this peer is participating in
  timestamp: z.string().datetime(),
});

// ============================================================================
// TYPE EXPORTS
// ============================================================================

export type ComputeResource = z.infer<typeof ComputeResourceSchema>;
export type AlgorithmCapability = z.infer<typeof AlgorithmCapabilitySchema>;
export type DataLocality = z.infer<typeof DataLocalitySchema>;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
export type WorkUnit = z.infer<typeof WorkUnitSchema>;
export type WorkRequest = z.infer<typeof WorkRequestSchema>;
export type ComputeCommitment = z.infer<typeof ComputeCommitmentSchema>;
export type WorkAssignment = z.infer<typeof WorkAssignmentSchema>;
export type SlotAssignmentState = z.infer<typeof SlotAssignmentStateSchema>;
export type ComputeAllocationState = z.infer<typeof ComputeAllocationStateSchema>;
export type WorkResult = z.infer<typeof WorkResultSchema>;
export type VerificationConsensus = z.infer<typeof VerificationConsensusSchema>;
export type ComputeRoundState = z.infer<typeof ComputeRoundStateSchema>;
```

---

## Store Architecture (Layer 2 & 3)

### My Data Stores (Published to Network)

```typescript
// File: stores.ts

import { createStore } from './store'; // Generic store factory
import {
  ComputeCommitmentSchema,
  ComputeAllocationStateSchema,
  ComputeRoundStateSchema,
  WorkResultSchema,
} from './schemas';

// ============================================================================
// MY LOCAL STORES
// ============================================================================

export const myComputeCommitment = createStore({
  networkPath: 'compute/commitment',
  schema: ComputeCommitmentSchema,
  cacheable: true,
  persistDebounce: 100,
});

export const myComputeAllocation = createStore({
  networkPath: 'compute/allocation',
  schema: ComputeAllocationStateSchema,
  cacheable: true,
  persistDebounce: 100,
});

export const myComputeRoundState = createStore({
  networkPath: 'compute/round',
  schema: ComputeRoundStateSchema,
  cacheable: true,
  persistDebounce: 100,
});

export const myWorkResults = createStore({
  networkPath: 'compute/results',
  schema: WorkResultSchema.array(),
  cacheable: true,
  persistDebounce: 100,
});

// ============================================================================
// NETWORK DATA MAPS
// ============================================================================

export const networkCommitments = new Map<string, ComputeCommitment>();
export const networkAllocations = new Map<string, ComputeAllocationState>();
export const networkRoundStates = new Map<string, ComputeRoundState>();
export const networkResults = new Map<string, WorkResult[]>();

// ============================================================================
// SUBSCRIPTION MANAGEMENT
// ============================================================================

export function subscribeToComputeCommitment(peerId: string) {
  myComputeCommitment.subscribeToUser(peerId, (commitment) => {
    if (commitment) {
      networkCommitments.set(peerId, commitment);
    }
  });
}

export function subscribeToComputeAllocation(peerId: string) {
  myComputeAllocation.subscribeToUser(peerId, (allocation) => {
    if (allocation) {
      networkAllocations.set(peerId, allocation);
    }
  });
}

export function subscribeToComputeRoundState(peerId: string) {
  myComputeRoundState.subscribeToUser(peerId, (roundState) => {
    if (roundState) {
      networkRoundStates.set(peerId, roundState);
    }
  });
}

export function subscribeToWorkResults(peerId: string) {
  myWorkResults.subscribeToUser(peerId, (results) => {
    if (results) {
      networkResults.set(peerId, results);
    }
  });
}

export function subscribeToFullComputePeer(peerId: string) {
  subscribeToComputeCommitment(peerId);
  subscribeToComputeAllocation(peerId);
  subscribeToComputeRoundState(peerId);
  subscribeToWorkResults(peerId);
}

// ============================================================================
// INITIALIZATION
// ============================================================================

export function initializeComputeStores() {
  myComputeCommitment.initialize();
  myComputeAllocation.initialize();
  myComputeRoundState.initialize();
  myWorkResults.initialize();
}

export function cleanupComputeStores() {
  networkCommitments.clear();
  networkAllocations.clear();
  networkRoundStates.clear();
  networkResults.clear();
}
```

---

## Algorithm Implementation (Layer 4)

### Derived Stores & Core Logic

```typescript
// File: compute-algorithm.ts

import { derived } from 'svelte/store'; // Or your framework's reactive primitives
import {
  myComputeCommitment,
  myComputeAllocation,
  networkCommitments,
  networkAllocations,
  subscribeToFullComputePeer,
  subscribeToComputeCommitment,
} from './stores';
import type {
  ComputeCommitment,
  WorkUnit,
  AvailabilitySlot,
  WorkAssignment,
  ComputeResource,
} from './schemas';

// ============================================================================
// DERIVED STORES (REACTIVE SUBGROUPS)
// ============================================================================

// Compute bilateral trust scores (min of mutual trust)
export const myTrustScores = derived(
  [myComputeCommitment, () => Array.from(networkCommitments.entries())],
  ([$myCommitment, $networkCommitments]) => {
    const scores = new Map<string, number>();
    
    for (const [peerId, theirCommitment] of $networkCommitments) {
      const myTrustOfThem = $myCommitment?.trust_weights?.[peerId] ?? 0;
      const theirTrustOfMe = theirCommitment?.trust_weights?.[$myCommitment?.peer_id ?? ''] ?? 0;
      
      // Bilateral trust is minimum of both directions
      scores.set(peerId, Math.min(myTrustOfThem, theirTrustOfMe));
    }
    
    return scores;
  }
);

// Trusted peers (bilateral trust > 0)
export const trustedPeers = derived(
  myTrustScores,
  ($scores) => {
    return Array.from($scores.entries())
      .filter(([_, score]) => score > 0)
      .map(([peerId]) => peerId);
  }
);

// Semi-trusted peers (I trust them one-way)
export const semiTrustedPeers = derived(
  [myComputeCommitment, myTrustScores],
  ([$myCommitment, $trustScores]) => {
    return Object.entries($myCommitment?.trust_weights ?? {})
      .filter(([peerId, myTrust]) => myTrust > 0 && ($trustScores.get(peerId) ?? 0) === 0)
      .map(([peerId]) => peerId);
  }
);

// Peers requesting work that I trust
export const trustedWorkRequesters = derived(
  [trustedPeers, () => Array.from(networkCommitments.values())],
  ([$trusted, $commitments]) => {
    return $commitments.filter(c => 
      $trusted.includes(c.peer_id) && 
      c.work_requests.length > 0
    );
  }
);

// Peers with capacity that I trust
export const trustedCapacityProviders = derived(
  [trustedPeers, () => Array.from(networkCommitments.values())],
  ([$trusted, $commitments]) => {
    return $commitments.filter(c => 
      $trusted.includes(c.peer_id) && 
      c.availability_slots.length > 0
    );
  }
);

// Active participants (fresh commitments < 60s)
export const activeComputePeers = derived(
  () => Array.from(networkCommitments.values()),
  ($commitments) => {
    const now = Date.now();
    const ACTIVE_THRESHOLD_MS = 60_000;
    
    return $commitments.filter(c => {
      const timestamp = new Date(c.timestamp).getTime();
      return (now - timestamp) < ACTIVE_THRESHOLD_MS;
    });
  }
);

// ============================================================================
// MATCHING LOGIC
// ============================================================================

function resourcesSufficient(required: ComputeResource, available: ComputeResource): boolean {
  return (
    available.cpu_cores >= required.cpu_cores &&
    available.ram_gb >= required.ram_gb &&
    (required.gpu_vram_gb ? (available.gpu_vram_gb ?? 0) >= required.gpu_vram_gb : true) &&
    (required.disk_gb ? (available.disk_gb ?? 0) >= required.disk_gb : true)
  );
}

function timeSlotCompatible(workUnit: WorkUnit, slot: AvailabilitySlot): boolean {
  const workDuration = workUnit.estimated_duration_seconds;
  const slotStart = new Date(slot.start_time).getTime();
  const slotEnd = new Date(slot.end_time).getTime();
  const slotDuration = (slotEnd - slotStart) / 1000; // seconds
  
  return slotDuration >= workDuration;
}

function algorithmSupported(workUnit: WorkUnit, slot: AvailabilitySlot): boolean {
  return slot.algorithms.some(algo => 
    algo.algorithm_id === workUnit.algorithm_id &&
    algo.version === workUnit.algorithm_version &&
    algo.supported
  );
}

function dataLocalityScore(workUnit: WorkUnit, slot: AvailabilitySlot): number {
  if (!workUnit.required_dataset) return 1.0; // No dataset needed
  if (!slot.data_locality) return 0.5; // Unknown locality
  
  const locality = slot.data_locality.find(d => d.dataset_id === workUnit.required_dataset);
  if (!locality) return 0.5; // Dataset not mentioned
  
  return locality.has_local_copy ? 1.0 : 0.3; // Strong preference for local data
}

function slotWorkUnitCompatible(workUnit: WorkUnit, slot: AvailabilitySlot): boolean {
  return (
    resourcesSufficient(workUnit.required_resources, slot.resources) &&
    timeSlotCompatible(workUnit, slot) &&
    algorithmSupported(workUnit, slot)
  );
}

// ============================================================================
// WORK ASSIGNMENT ALGORITHM
// ============================================================================

export function computeWorkAssignments(
  myCommitment: ComputeCommitment,
  trustScores: Map<string, number>,
  networkCommitments: Map<string, ComputeCommitment>
): ComputeAllocationState {
  
  const slotAssignments: SlotAssignmentState[] = [];
  
  // For each of my availability slots
  for (const slot of myCommitment.availability_slots) {
    const tier1Assignments: WorkAssignment[] = [];
    const tier2Assignments: WorkAssignment[] = [];
    
    let remainingResources = { ...slot.resources };
    
    // Collect all work units from all requesters
    const allWorkUnits: Array<{ unit: WorkUnit; requesterId: string; trustScore: number }> = [];
    
    for (const [peerId, commitment] of networkCommitments.entries()) {
      const trust = trustScores.get(peerId) ?? 0;
      
      for (const request of commitment.work_requests) {
        for (const unit of request.work_units) {
          if (slotWorkUnitCompatible(unit, slot)) {
            allWorkUnits.push({
              unit,
              requesterId: peerId,
              trustScore: trust,
            });
          }
        }
      }
    }
    
    // ========================================================================
    // TIER 1: TRUSTED PEERS (Bilateral Trust > 0)
    // ========================================================================
    
    const tier1Units = allWorkUnits
      .filter(({ trustScore }) => trustScore > 0)
      .sort((a, b) => {
        // Sort by: trust score DESC, then priority DESC, then data locality DESC
        if (a.trustScore !== b.trustScore) return b.trustScore - a.trustScore;
        if (a.unit.priority !== b.unit.priority) return b.unit.priority - a.unit.priority;
        
        const localityA = dataLocalityScore(a.unit, slot);
        const localityB = dataLocalityScore(b.unit, slot);
        return localityB - localityA;
      });
    
    for (const { unit, requesterId, trustScore } of tier1Units) {
      if (!resourcesSufficient(unit.required_resources, remainingResources)) {
        continue; // Not enough resources left
      }
      
      // Create assignment
      const assignment: WorkAssignment = {
        assignment_id: `${slot.slot_id}-${unit.unit_id}-${Date.now()}`,
        work_unit_id: unit.unit_id,
        assigned_to_peer: requesterId,
        assigned_by_peer: myCommitment.peer_id,
        slot_id: slot.slot_id,
        expected_start: slot.start_time,
        expected_end: slot.end_time,
        resources_allocated: unit.required_resources,
        verification_role: 'primary',
        timestamp: new Date().toISOString(),
      };
      
      tier1Assignments.push(assignment);
      
      // Deduct resources
      remainingResources.cpu_cores -= unit.required_resources.cpu_cores;
      remainingResources.ram_gb -= unit.required_resources.ram_gb;
      if (unit.required_resources.gpu_vram_gb) {
        remainingResources.gpu_vram_gb = (remainingResources.gpu_vram_gb ?? 0) - unit.required_resources.gpu_vram_gb;
      }
    }
    
    // ========================================================================
    // TIER 2: SEMI-TRUSTED PEERS (One-way Trust)
    // ========================================================================
    
    const tier2Units = allWorkUnits
      .filter(({ trustScore, requesterId }) => {
        const myTrust = myCommitment.trust_weights[requesterId] ?? 0;
        return trustScore === 0 && myTrust > 0;
      })
      .sort((a, b) => {
        const myTrustA = myCommitment.trust_weights[a.requesterId] ?? 0;
        const myTrustB = myCommitment.trust_weights[b.requesterId] ?? 0;
        
        if (myTrustA !== myTrustB) return myTrustB - myTrustA;
        if (a.unit.priority !== b.unit.priority) return b.unit.priority - a.unit.priority;
        
        const localityA = dataLocalityScore(a.unit, slot);
        const localityB = dataLocalityScore(b.unit, slot);
        return localityB - localityA;
      });
    
    for (const { unit, requesterId } of tier2Units) {
      if (!resourcesSufficient(unit.required_resources, remainingResources)) {
        continue;
      }
      
      const assignment: WorkAssignment = {
        assignment_id: `${slot.slot_id}-${unit.unit_id}-${Date.now()}`,
        work_unit_id: unit.unit_id,
        assigned_to_peer: requesterId,
        assigned_by_peer: myCommitment.peer_id,
        slot_id: slot.slot_id,
        expected_start: slot.start_time,
        expected_end: slot.end_time,
        resources_allocated: unit.required_resources,
        verification_role: 'primary',
        timestamp: new Date().toISOString(),
      };
      
      tier2Assignments.push(assignment);
      
      remainingResources.cpu_cores -= unit.required_resources.cpu_cores;
      remainingResources.ram_gb -= unit.required_resources.ram_gb;
      if (unit.required_resources.gpu_vram_gb) {
        remainingResources.gpu_vram_gb = (remainingResources.gpu_vram_gb ?? 0) - unit.required_resources.gpu_vram_gb;
      }
    }
    
    // Calculate utilization
    const totalAllocated: ComputeResource = {
      cpu_cores: slot.resources.cpu_cores - remainingResources.cpu_cores,
      ram_gb: slot.resources.ram_gb - remainingResources.ram_gb,
      gpu_vram_gb: slot.resources.gpu_vram_gb 
        ? (slot.resources.gpu_vram_gb - (remainingResources.gpu_vram_gb ?? 0))
        : undefined,
    };
    
    const utilizationPercent = (totalAllocated.cpu_cores / slot.resources.cpu_cores) * 100;
    
    slotAssignments.push({
      slot_id: slot.slot_id,
      available_resources: slot.resources,
      tier1_assignments: tier1Assignments,
      tier2_assignments: tier2Assignments,
      total_allocated_resources: totalAllocated,
      utilization_percent: Math.round(utilizationPercent),
    });
  }
  
  const totalAssignments = slotAssignments.reduce(
    (sum, slot) => sum + slot.tier1_assignments.length + slot.tier2_assignments.length,
    0
  );
  
  return {
    peer_id: myCommitment.peer_id,
    slot_assignments: slotAssignments,
    total_work_units_assigned: totalAssignments,
    timestamp: new Date().toISOString(),
    vector_clock: myCommitment.vector_clock,
  };
}

// ============================================================================
// ALGORITHM-DRIVEN SUBSCRIPTIONS
// ============================================================================

let subscriptionCleanup: (() => void)[] = [];

export function initializeComputeAlgorithmSubscriptions() {
  // Subscribe to all trusted peers (full data)
  const unsubTrusted = trustedPeers.subscribe($trusted => {
    $trusted.forEach(peerId => subscribeToFullComputePeer(peerId));
  });
  
  // Subscribe to semi-trusted peers (commitments only for now)
  const unsubSemiTrusted = semiTrustedPeers.subscribe($semiTrusted => {
    $semiTrusted.forEach(peerId => subscribeToComputeCommitment(peerId));
  });
  
  subscriptionCleanup.push(unsubTrusted, unsubSemiTrusted);
}

export function cleanupComputeAlgorithmSubscriptions() {
  subscriptionCleanup.forEach(cleanup => cleanup());
  subscriptionCleanup = [];
}

// ============================================================================
// WORK EXECUTION (When I receive assignments)
// ============================================================================

export async function executeWorkUnit(assignment: WorkAssignment): Promise<WorkResult> {
  // This would contain the actual computation logic
  // For now, a placeholder that shows the structure
  
  const startTime = Date.now();
  
  try {
    // 1. Fetch work unit details
    // 2. Download required data (if not local)
    // 3. Execute computation
    // 4. Generate result
    
    const result = await performComputation(assignment);
    
    const endTime = Date.now();
    const computationTime = (endTime - startTime) / 1000;
    
    return {
      result_id: `result-${assignment.assignment_id}`,
      work_unit_id: assignment.work_unit_id,
      assignment_id: assignment.assignment_id,
      computed_by_peer: assignment.assigned_to_peer,
      result_data: result,
      computation_time_seconds: computationTime,
      resources_used: assignment.resources_allocated,
      verification_hash: hashResult(result),
      timestamp: new Date().toISOString(),
    };
  } catch (error) {
    throw new Error(`Work unit execution failed: ${error}`);
  }
}

async function performComputation(assignment: WorkAssignment): Promise<any> {
  // Protocol-specific computation logic
  throw new Error('Not implemented - replace with actual algorithm');
}

function hashResult(result: any): string {
  // Create verification hash for consensus
  // Use crypto.subtle.digest or similar
  return 'hash-placeholder';
}
```

---

## Phase Timing (Sync/Async Matrix)

### Round Execution Breakdown

| Phase | Duration | Sync | Critical Data | Action |
|-------|----------|------|---------------|--------|
| **0. Capability Update** | 0-5s | Async | My availability_slots, work_requests, trust_weights | User updates their compute capacity/needs |
| **1. State Publishing** | 5-10s | Async | myComputeCommitment + vector_clock | Broadcast updated commitment to network |
| **2. Peer State Collection** | 10-30s | Async | All active peers' commitments | Gather network state (debounced) |
| **3. Trust Score Computation** | 30-35s | Sync (Local) | My trust_weights + peers' trust_weights | Compute bilateral trust scores |
| **4. Compatibility Matrix** | 35-45s | Sync (Local) | All work_units + all availability_slots | Pre-compute which units can run on which slots |
| **5. Work Assignment** | 45-60s | Sync (Local) | Trust scores + compatibility matrix | Two-tier assignment (trusted → semi-trusted) |
| **6. Assignment Publishing** | 60-65s | Async | myComputeAllocation | Broadcast work assignments to requesters |
| **7. Work Execution** | 65s-hours | Async | Assigned work_units + data | Perform actual computation |
| **8. Result Publishing** | On completion | Async | WorkResult + verification_hash | Submit results for verification |
| **9. Result Verification** | Variable | Async | Multiple WorkResults for same unit | Consensus via redundant computation |
| **10. Trust Update** | After verification | Sync (Local) | Result quality + reliability | Update trust_weights based on performance |

---

## Example Usage

### Scenario: Video Rendering Farm

```typescript
// Alice has a powerful GPU and wants to contribute
const aliceCommitment: ComputeCommitment = {
  peer_id: 'alice-peer-123',
  availability_slots: [
    {
      slot_id: 'alice-slot-1',
      start_time: '2025-10-24T00:00:00Z',
      end_time: '2025-10-24T08:00:00Z',
      resources: {
        cpu_cores: 16,
        ram_gb: 64,
        gpu_vram_gb: 24, // RTX 4090
      },
      algorithms: [
        {
          algorithm_id: 'video-render',
          version: '1.0.0',
          supported: true,
          performance_score: 0.95,
        },
      ],
      priority: 0.8, // Eager to help
    },
  ],
  work_requests: [], // Not requesting work, just providing
  trust_weights: {
    'bob-peer-456': 0.9, // Alice trusts Bob highly
    'charlie-peer-789': 0.6, // Alice somewhat trusts Charlie
  },