/**
 * Provenance Module - Barrel Export
 * 
 * Centralized exports for the complete provenance system.
 * 
 * This module provides a Merkle-DAG event-based provenance system with:
 * - Content-addressed events (event ID = SHA-256 hash)
 * - Cryptographic signatures (SEA/ECDSA)
 * - ITC causality tracking
 * - DAG storage and traversal
 * - Verification engine
 * - Proof construction
 * 
 * Architecture:
 * - Event-based (not just records)
 * - Merkle-DAG (hash-linked parents)
 * - Signed (cryptographic authenticity)
 * - Causal (ITC + parent links)
 * - Verifiable (full verification chain)
 * 
 * Usage:
 * ```typescript
 * import { createComputationEvent, verifyEvent, constructProof } from './provenance';
 * 
 * // Create event
 * const event = await createComputationEvent({
 *   payload: { ... },
 *   parents: [parent1, parent2]
 * });
 * 
 * // Verify event
 * const result = await verifyEvent(event);
 * 
 * // Build proof
 * const proof = await constructProof(event.id);
 * ```
 */

// ═══════════════════════════════════════════════════════════════════
// MAIN API (High-Level)
// ═══════════════════════════════════════════════════════════════════

export {
	// Event creation
	createEvent,
	createComputationEvent,
	createComputationEventFromLegacy,
	
	// Signing
	signEvent,
	verifyEventSignature,
	verifyEventIntegrity,
	getCurrentUserPubKey,
	canSignEvents,
	
	// DAG operations
	storeEvent,
	getEvent,
	hasEvent,
	getEventsBatch,
	getHeadEvents,
	getChildren,
	traverseToRoots,
	traverseToLeaves,
	findPath,
	
	// Verification
	verifyEvent,
	verifyEventRecursive,
	verifyITCCausality,
	getCausalRelationship,
	isRootEvent,
	isMergeEvent,
	
	// Proofs
	buildProvenancePath,
	buildPathBetween,
	constructProof,
	verifyProof,
	verifyProofDetailed,
	getProofSize,
	getProofDepth,
	createProofSummary,
	
	// Queries
	getLatestComputationEvents,
	getComputationLineage,
	isComputationVerified,
	
	// System
	initializeProvenanceSystem,
	getProvenanceSystemStatus,
	
	// Types
	type ProvenanceEvent,
	type ComputationEvent,
	type EventBody,
	type EventMetadata,
	type ComputationPayload,
	type InputProvenance,
	type OutputProvenance,
	type Hash,
	type ProvenancePath,
	type ProvenanceProof,
	type VerificationResult,
	type RecursiveVerificationResult
} from './provenance.svelte';

// ═══════════════════════════════════════════════════════════════════
// SCHEMAS (Zod Validation)
// ═══════════════════════════════════════════════════════════════════

export {
	// Primitive schemas
	HashSchema,
	AuthorIdSchema,
	TimestampSchema,
	
	// Event schemas
	ProvenanceEventSchema,
	ComputationEventSchema,
	EventBodySchema,
	EventMetadataSchema,
	EventTypeSchema,
	
	// Provenance schemas
	InputProvenanceSchema,
	OutputProvenanceSchema,
	ComputationPayloadSchema,
	
	// Signature schema
	SEASignatureSchema,
	
	// DAG schemas
	EventStoreEntrySchema,
	HeadIndexEntrySchema,
	ParentIndexEntrySchema,
	
	// Proof schemas
	ProvenancePathSchema,
	ProvenanceProofSchema,
	
	// Validation helpers
	parseProvenanceEvent,
	parseComputationEvent,
	parseEventStoreEntry,
	parseProvenanceProof,
	
	// Additional types
	type EventType,
	type SEASignature,
	type EventStoreEntry,
	type HeadIndexEntry,
	type ParentIndexEntry
} from './provenance-event-schema';

// ═══════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════

export {
	// Hashing & serialization
	deterministicStringify,
	hashData,
	hashString,
	extractEventBody,
	createEventBody,
	computeEventId,
	
	// Validation
	isValidHash
} from './provenance-signing.svelte';

export {
	// Formatting
	formatVerificationResult,
	getVerificationSummary
} from './provenance-verification.svelte';

export {
	// Proof utilities
	exportProofJSON,
	importProofJSON
} from './provenance-proof.svelte';

// ═══════════════════════════════════════════════════════════════════
// PATH HELPERS
// ═══════════════════════════════════════════════════════════════════

export {
	ProvenancePaths
} from './provenance-dag.svelte';

// ═══════════════════════════════════════════════════════════════════
// DEFAULT EXPORT
// ═══════════════════════════════════════════════════════════════════

import Provenance from './provenance.svelte';
export default Provenance;

