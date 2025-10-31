/**
 * Provenance System - High-Level API
 * 
 * This is the main entry point for the provenance system.
 * It integrates all provenance modules into a clean, easy-to-use API.
 * 
 * Modules:
 * - provenance-event-schema.ts: Event structure and schemas
 * - provenance-signing.svelte.ts: SEA signing/verification
 * - provenance-dag.svelte.ts: DAG storage and traversal
 * - provenance-verification.svelte.ts: Event verification engine
 * - provenance-proof.svelte.ts: Proof construction
 * 
 * Usage:
 * ```typescript
 * import { createComputationEvent, verifyEvent, constructProof } from './provenance.svelte';
 * 
 * // Create and store an event
 * const event = await createComputationEvent({
 *   payload: { ... },
 *   parents: [parentId1, parentId2]
 * });
 * 
 * // Verify an event
 * const result = await verifyEvent(event);
 * 
 * // Build a proof
 * const proof = await constructProof(event.id, { proofType: 'full' });
 * ```
 */

import { get } from 'svelte/store';
import { holsterUserPub } from '$lib/network/holster.svelte';
import { getMyITCStamp, incrementMyITCStamp } from '../v2/algorithm.svelte';

// Re-export types
export type {
	ProvenanceEvent,
	ComputationEvent,
	EventBody,
	EventMetadata,
	ComputationPayload,
	InputProvenance,
	OutputProvenance,
	Hash,
	ProvenancePath,
	ProvenanceProof
} from './provenance/provenance-event-schema';

export type {
	VerificationResult,
	RecursiveVerificationResult
} from './provenance-verification.svelte';

// Re-export key functions
export {
	// Signing
	signEvent,
	verifyEventSignature,
	verifyEventIntegrity,
	getCurrentUserPubKey,
	canSignEvents
} from './provenance-signing.svelte';

export {
	// DAG
	storeEvent,
	getEvent,
	hasEvent,
	getEventsBatch,
	getHeadEvents,
	getChildren,
	traverseToRoots,
	traverseToLeaves,
	findPath
} from './provenance-dag.svelte';

export {
	// Verification
	verifyEvent,
	verifyEventRecursive,
	verifyITCCausality,
	getCausalRelationship,
	isRootEvent,
	isMergeEvent
} from './provenance-verification.svelte';

export {
	// Proofs
	buildProvenancePath,
	buildPathBetween,
	constructProof,
	verifyProof,
	verifyProofDetailed,
	getProofSize,
	getProofDepth,
	createProofSummary
} from './provenance-proof.svelte';

// Import internals
import { signEvent, hashData } from './provenance-signing.svelte';
import { storeEvent } from './provenance-dag.svelte';
import type {
	ProvenanceEvent,
	ComputationEvent,
	EventBody,
	ComputationPayload,
	EventType,
	Hash
} from './provenance/provenance-event-schema';

// ═══════════════════════════════════════════════════════════════════
// HIGH-LEVEL EVENT CREATION
// ═══════════════════════════════════════════════════════════════════

export interface CreateEventOptions {
	/** Event payload (application-specific data) */
	payload: any;
	
	/** Parent event IDs (empty for seed events) */
	parents: Hash[];
	
	/** Event type (default: 'generic') */
	eventType?: EventType;
	
	/** Optional: Merkle root for attachments */
	merkleRoot?: Hash;
	
	/** Optional: Event tags */
	tags?: string[];
	
	/** Whether to auto-store the event (default: true) */
	autoStore?: boolean;
	
	/** Whether to verify before storing (default: false) */
	verifyBeforeStore?: boolean;
}

/**
 * Create a signed provenance event
 * 
 * This is the main function for creating provenance events.
 * It handles:
 * - ITC stamp management (increment)
 * - Signing with SEA
 * - Optional storage in Holster
 * - Optional verification
 * 
 * @param options - Event creation options
 * @returns Signed event
 * @throws Error if user not authenticated or signing fails
 */
export async function createEvent(options: CreateEventOptions): Promise<ProvenanceEvent> {
	const {
		payload,
		parents,
		eventType = 'generic',
		merkleRoot,
		tags = [],
		autoStore = true,
		verifyBeforeStore = false
	} = options;
	
	// Check authentication
	const author = get(holsterUserPub);
	if (!author) {
		throw new Error('User must be authenticated to create events');
	}
	
	// Get and increment ITC stamp
	const itcStamp = getMyITCStamp();
	incrementMyITCStamp(); // Increment for next event
	
	// Build event body
	const eventBody: EventBody = {
		author,
		payload,
		itc: itcStamp,
		parents,
		merkle_root: merkleRoot,
		timestamp: new Date().toISOString(),
		meta: {
			type: eventType,
			version: '1.0.0',
			tags
		}
	};
	
	// Sign event
	const { eventId, signature } = await signEvent(eventBody);
	
	// Construct full event
	const event: ProvenanceEvent = {
		id: eventId,
		...eventBody,
		sig: signature
	};
	
	// Optional: Verify before storing
	if (verifyBeforeStore) {
		const { verifyEventIntegrity } = await import('./provenance-signing.svelte');
		const result = await verifyEventIntegrity(event);
		if (!result.valid) {
			throw new Error(`Event verification failed: ${result.errors.join(', ')}`);
		}
	}
	
	// Optional: Auto-store
	if (autoStore) {
		await storeEvent(event, verifyBeforeStore);
	}
	
	console.log(`[PROVENANCE] Created event: ${eventId.substring(0, 16)}...`);
	
	return event;
}

/**
 * Create a computation event (RDL-specific)
 * 
 * Convenience wrapper for creating computation provenance events.
 * 
 * @param options - Computation event options
 * @returns Signed computation event
 */
export async function createComputationEvent(options: {
	payload: ComputationPayload;
	parents: Hash[];
	tags?: string[];
	autoStore?: boolean;
	verifyBeforeStore?: boolean;
}): Promise<ComputationEvent> {
	const event = await createEvent({
		...options,
		eventType: 'computation'
	});
	
	return event as ComputationEvent;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS FOR COMPUTE.SVELTE.TS INTEGRATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Create computation provenance event from legacy provenance record
 * 
 * This bridges the old provenance system (compute.svelte.ts) with the new
 * event-based system.
 * 
 * @param legacyProvenance - Old-style provenance record
 * @param parentEventIds - Parent event IDs (from input provenance)
 * @returns New-style computation event
 */
export async function createComputationEventFromLegacy(
	legacyProvenance: {
		programHash: string;
		computationId: string;
		computationHash: string;
		inputs: Record<string, any>;
		outputs: Record<string, any>;
		deterministicHash: string;
	},
	parentEventIds: Hash[] = []
): Promise<ComputationEvent> {
	// Convert legacy provenance to new payload format
	const payload: ComputationPayload = {
		programHash: legacyProvenance.programHash as Hash,
		computationId: legacyProvenance.computationId,
		computationHash: legacyProvenance.computationHash as Hash,
		inputs: legacyProvenance.inputs,
		outputs: legacyProvenance.outputs,
		deterministicHash: legacyProvenance.deterministicHash as Hash
	};
	
	// Create event
	return await createComputationEvent({
		payload,
		parents: parentEventIds,
		tags: ['computation', legacyProvenance.programHash, legacyProvenance.computationId],
		autoStore: true,
		verifyBeforeStore: false
	});
}

// ═══════════════════════════════════════════════════════════════════
// QUERY HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get latest computation events for a program
 * 
 * @param programHash - Program hash
 * @param limit - Maximum number of events to return
 * @returns Array of computation events
 */
export async function getLatestComputationEvents(
	programHash: Hash,
	limit: number = 10
): Promise<ComputationEvent[]> {
	// TODO: Implement efficient query using head index
	console.warn('[PROVENANCE] getLatestComputationEvents() not yet fully implemented');
	return [];
}

/**
 * Get computation lineage (all ancestors of a computation)
 * 
 * @param computationEventId - Computation event ID
 * @param maxDepth - Maximum traversal depth
 * @returns Array of events in lineage
 */
export async function getComputationLineage(
	computationEventId: Hash,
	maxDepth: number = 100
): Promise<ProvenanceEvent[]> {
	const { traverseToRoots } = await import('./provenance-dag.svelte');
	return await traverseToRoots(computationEventId, maxDepth);
}

/**
 * Check if a computation is verified
 * 
 * @param computationEventId - Computation event ID
 * @returns Verification result
 */
export async function isComputationVerified(
	computationEventId: Hash
): Promise<{ verified: boolean; errors: string[] }> {
	const { verifyEvent } = await import('./provenance-verification.svelte');
	const { getEvent } = await import('./provenance-dag.svelte');
	
	const event = await getEvent(computationEventId);
	if (!event) {
		return {
			verified: false,
			errors: ['Event not found']
		};
	}
	
	const result = await verifyEvent(event, true);
	return {
		verified: result.valid,
		errors: result.errors
	};
}

// ═══════════════════════════════════════════════════════════════════
// SYSTEM UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize provenance system
 * 
 * Call this once at application startup.
 */
export function initializeProvenanceSystem(): void {
	console.log('[PROVENANCE] Initializing provenance system...');
	console.log('[PROVENANCE] ✅ Event-based provenance with SEA signatures enabled');
	console.log('[PROVENANCE] ✅ Merkle-DAG storage active');
	console.log('[PROVENANCE] ✅ ITC causality tracking enabled');
	console.log('[PROVENANCE] ✅ Verification engine ready');
	console.log('[PROVENANCE] ✅ Proof construction available');
}

/**
 * Get provenance system status
 * 
 * @returns Status information
 */
export function getProvenanceSystemStatus(): {
	enabled: boolean;
	authenticated: boolean;
	itcAvailable: boolean;
	version: string;
} {
	const author = get(holsterUserPub);
	
	return {
		enabled: true,
		authenticated: !!author,
		itcAvailable: true,
		version: '1.0.0'
	};
}

// ═══════════════════════════════════════════════════════════════════
// EXPORT DEFAULT
// ═══════════════════════════════════════════════════════════════════

export default {
	// Event creation
	createEvent,
	createComputationEvent,
	createComputationEventFromLegacy,
	
	// Queries
	getLatestComputationEvents,
	getComputationLineage,
	isComputationVerified,
	
	// System
	initializeProvenanceSystem,
	getProvenanceSystemStatus
};

