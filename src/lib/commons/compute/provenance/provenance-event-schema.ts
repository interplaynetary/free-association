/**
 * Provenance Event Schemas (Merkle-DAG Event Model)
 * 
 * This module defines the event-based provenance system using:
 * - Content-addressed events (event ID = hash)
 * - Merkle-DAG structure (parent links)
 * - Cryptographic signatures (SEA)
 * - ITC causality tracking
 * 
 * Follows the architecture from the provenance design document:
 * https://github.com/ricardobcl/Interval-Tree-Clocks
 */

import * as z from 'zod';
import { ITCStampSchema } from '../../v2/schemas';

// ═══════════════════════════════════════════════════════════════════
// PRIMITIVE SCHEMAS
// ═══════════════════════════════════════════════════════════════════

/**
 * Content hash (SHA-256 hex string)
 */
export const HashSchema = z.string().regex(/^[0-9a-f]{64}$/, 'Invalid SHA-256 hash');

/**
 * Public key identifier (DID or pubkey)
 */
export const AuthorIdSchema = z.string().min(1);

/**
 * ISO 8601 timestamp
 */
export const TimestampSchema = z.string().datetime();

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE EVENT PAYLOAD (Application Data)
// ═══════════════════════════════════════════════════════════════════

/**
 * Input Provenance - tracks where an input came from
 */
export const InputProvenanceSchema = z.object({
	source: z.enum(['value', 'subscription', 'fetch', 'local', 'derived']),
	path: z.string().optional(),
	contentHash: HashSchema,
	provenance: z.string().optional() // Reference to parent event ID
});

/**
 * Output Provenance - tracks what was produced
 */
export const OutputProvenanceSchema = z.object({
	path: z.string(),
	contentHash: HashSchema
});

/**
 * Computation Payload - RDL-specific event data
 * 
 * This is the "payload" field in the Event structure.
 * For other language runtimes (SQL, WASM), define their own payload schemas.
 */
export const ComputationPayloadSchema = z.object({
	// Computation identity (what)
	programHash: HashSchema,
	computationId: z.string(),
	computationHash: HashSchema, // Hash of the computation definition
	
	// Input provenance (what data was used)
	inputs: z.record(z.string(), InputProvenanceSchema),
	
	// Output provenance (what was produced)
	outputs: z.record(z.string(), OutputProvenanceSchema),
	
	// Deterministic verification hash
	// Hash of (computationHash + sorted input hashes)
	deterministicHash: HashSchema
});

// ═══════════════════════════════════════════════════════════════════
// EVENT METADATA
// ═══════════════════════════════════════════════════════════════════

/**
 * Event Type - discriminates between different event kinds
 */
export const EventTypeSchema = z.enum([
	'computation',  // RDL computation execution
	'allocation',   // Resource allocation
	'commitment',   // Commitment update
	'recognition',  // Recognition weights
	'generic'       // Generic event (future extension)
]);

/**
 * Event Metadata - additional context about the event
 */
export const EventMetadataSchema = z.object({
	type: EventTypeSchema,
	version: z.string().default('1.0.0'),
	contentType: z.string().optional(), // MIME type or semantic type
	tags: z.array(z.string()).optional() // Searchable tags
});

// ═══════════════════════════════════════════════════════════════════
// SIGNATURE WRAPPER (SEA Format)
// ═══════════════════════════════════════════════════════════════════

/**
 * SEA Signature Format
 * 
 * SEA.sign() returns an object with:
 * - m: message (the signed data)
 * - s: signature (base64-encoded ECDSA signature)
 */
export const SEASignatureSchema = z.object({
	m: z.string(), // The message that was signed (base64 or hex)
	s: z.string()  // The signature (base64)
});

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE EVENT (Full Event Structure)
// ═══════════════════════════════════════════════════════════════════

/**
 * Provenance Event (Merkle-DAG Node)
 * 
 * This is the core event structure that forms a Merkle-DAG:
 * 
 * Event Structure:
 * {
 *   id: <content-hash>,           // SHA-256 of canonical event body
 *   author: <pubkey>,              // Who created this event
 *   payload: <any>,                // Application-specific data
 *   itc: <ITCStamp>,               // Causality tracking
 *   parents: [<hash>, ...],        // Parent event hashes (Merkle links)
 *   merkle_root?: <hash>,          // Optional: root of attachments tree
 *   timestamp: <ISO8601>,          // Wall-clock time (non-authoritative)
 *   meta: <EventMetadata>,         // Event type and tags
 *   sig: <SEASignature>            // ECDSA signature over event body
 * }
 * 
 * Key Properties:
 * - Content-addressed: id = H(body)
 * - Immutable: changing any field changes the id
 * - Verifiable: signature proves authorship
 * - Causal: ITC + parents track causality
 */
export const ProvenanceEventSchema = z.object({
	// Content-addressed identifier (SHA-256 of canonical event body)
	id: HashSchema,
	
	// Author public key (who created this event)
	author: AuthorIdSchema,
	
	// Application-specific payload
	payload: z.any(), // Can be ComputationPayloadSchema or other types
	
	// ITC stamp (causality tracking)
	itc: ITCStampSchema,
	
	// Parent event hashes (Merkle-DAG links)
	// Empty array for seed events, multiple parents for merges
	parents: z.array(HashSchema),
	
	// Optional: Merkle root of attachments/blobs
	merkle_root: HashSchema.optional(),
	
	// Wall-clock timestamp (non-authoritative, for UX only)
	timestamp: TimestampSchema,
	
	// Event metadata (type, version, tags)
	meta: EventMetadataSchema,
	
	// SEA signature over canonical event body (everything except sig field)
	sig: SEASignatureSchema
});

/**
 * Computation Event (Typed Event for RDL)
 * 
 * This is a ProvenanceEvent with a typed payload for RDL computations.
 */
export const ComputationEventSchema = ProvenanceEventSchema.extend({
	payload: ComputationPayloadSchema,
	meta: EventMetadataSchema.extend({
		type: z.literal('computation')
	})
});

// ═══════════════════════════════════════════════════════════════════
// EVENT BODY (For Signing/Hashing)
// ═══════════════════════════════════════════════════════════════════

/**
 * Event Body (Canonical representation for hashing/signing)
 * 
 * This is the subset of event fields that are hashed and signed.
 * The signature is computed over this canonical body.
 * 
 * Field order matters! Must be deterministic.
 */
export const EventBodySchema = z.object({
	author: AuthorIdSchema,
	payload: z.any(),
	itc: ITCStampSchema,
	parents: z.array(HashSchema),
	merkle_root: HashSchema.optional(),
	timestamp: TimestampSchema,
	meta: EventMetadataSchema
});

// ═══════════════════════════════════════════════════════════════════
// DAG STRUCTURES
// ═══════════════════════════════════════════════════════════════════

/**
 * Event Store Entry
 * 
 * What gets persisted to storage (Holster)
 */
export const EventStoreEntrySchema = z.object({
	event: ProvenanceEventSchema,
	
	// Storage metadata
	stored_at: z.number().int().positive(),
	verified: z.boolean().default(false),
	
	// Optional: cached verification result
	verification_result: z.object({
		valid: z.boolean(),
		timestamp: z.number().int().positive(),
		errors: z.array(z.string()).optional()
	}).optional()
});

/**
 * Head Index Entry
 * 
 * Tracks the latest event(s) for each author
 */
export const HeadIndexEntrySchema = z.object({
	author: AuthorIdSchema,
	event_ids: z.array(HashSchema), // Can have multiple heads (concurrent updates)
	updated_at: z.number().int().positive()
});

/**
 * Parent Index Entry
 * 
 * Reverse edges for DAG traversal (parent → children)
 */
export const ParentIndexEntrySchema = z.object({
	parent_id: HashSchema,
	child_ids: z.array(HashSchema),
	updated_at: z.number().int().positive()
});

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE PROOF STRUCTURES
// ═══════════════════════════════════════════════════════════════════

/**
 * Provenance Path
 * 
 * A sequence of events from target to root
 */
export const ProvenancePathSchema = z.object({
	target_id: HashSchema,
	root_id: HashSchema,
	events: z.array(ProvenanceEventSchema),
	
	// Path metadata
	length: z.number().int().positive(),
	computed_at: z.number().int().positive()
});

/**
 * Provenance Proof
 * 
 * A compact proof of provenance from target to trusted roots
 */
export const ProvenanceProofSchema = z.object({
	target_id: HashSchema,
	
	// Minimal set of events needed to verify provenance
	events: z.array(ProvenanceEventSchema),
	
	// Verification steps (for transparency)
	verification_steps: z.array(z.object({
		event_id: HashSchema,
		step: z.string(),
		result: z.boolean()
	})),
	
	// Proof metadata
	proof_type: z.enum(['full', 'shallow', 'witness']),
	computed_at: z.number().int().positive(),
	valid: z.boolean()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export type Hash = z.infer<typeof HashSchema>;
export type AuthorId = z.infer<typeof AuthorIdSchema>;
export type Timestamp = z.infer<typeof TimestampSchema>;
export type InputProvenance = z.infer<typeof InputProvenanceSchema>;
export type OutputProvenance = z.infer<typeof OutputProvenanceSchema>;
export type ComputationPayload = z.infer<typeof ComputationPayloadSchema>;
export type EventType = z.infer<typeof EventTypeSchema>;
export type EventMetadata = z.infer<typeof EventMetadataSchema>;
export type SEASignature = z.infer<typeof SEASignatureSchema>;
export type ProvenanceEvent = z.infer<typeof ProvenanceEventSchema>;
export type ComputationEvent = z.infer<typeof ComputationEventSchema>;
export type EventBody = z.infer<typeof EventBodySchema>;
export type EventStoreEntry = z.infer<typeof EventStoreEntrySchema>;
export type HeadIndexEntry = z.infer<typeof HeadIndexEntrySchema>;
export type ParentIndexEntry = z.infer<typeof ParentIndexEntrySchema>;
export type ProvenancePath = z.infer<typeof ProvenancePathSchema>;
export type ProvenanceProof = z.infer<typeof ProvenanceProofSchema>;

// ═══════════════════════════════════════════════════════════════════
// VALIDATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse and validate a provenance event
 */
export function parseProvenanceEvent(data: unknown): ProvenanceEvent | null {
	const result = ProvenanceEventSchema.safeParse(data);
	if (!result.success) {
		console.warn('[PROVENANCE-EVENT] Invalid event:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate a computation event
 */
export function parseComputationEvent(data: unknown): ComputationEvent | null {
	const result = ComputationEventSchema.safeParse(data);
	if (!result.success) {
		console.warn('[PROVENANCE-EVENT] Invalid computation event:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate an event store entry
 */
export function parseEventStoreEntry(data: unknown): EventStoreEntry | null {
	const result = EventStoreEntrySchema.safeParse(data);
	if (!result.success) {
		console.warn('[PROVENANCE-EVENT] Invalid event store entry:', result.error);
		return null;
	}
	return result.data;
}

/**
 * Parse and validate a provenance proof
 */
export function parseProvenanceProof(data: unknown): ProvenanceProof | null {
	const result = ProvenanceProofSchema.safeParse(data);
	if (!result.success) {
		console.warn('[PROVENANCE-EVENT] Invalid provenance proof:', result.error);
		return null;
	}
	return result.data;
}

