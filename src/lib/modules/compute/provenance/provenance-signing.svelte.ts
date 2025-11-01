/**
 * Provenance Signing (SEA Integration)
 * 
 * This module provides cryptographic signing and verification for provenance events:
 * - Canonical serialization for hashing/signing
 * - Content-addressed event ID generation
 * - SEA signature creation and verification
 * - Event integrity validation
 * 
 * Uses Gun's SEA library for ECDSA signatures.
 */

import { createHash } from 'crypto';
import { holster, holsterUser } from '$lib/network/holster.svelte';
import type {
	ProvenanceEvent,
	EventBody,
	SEASignature,
	Hash
} from './provenance-event-schema';

// ═══════════════════════════════════════════════════════════════════
// CANONICAL SERIALIZATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Deterministic JSON serialization
 * 
 * Ensures same data → same hash by:
 * - Sorting object keys alphabetically
 * - Consistent spacing/formatting
 * - Handling special values (undefined, NaN, etc.)
 * 
 * @param data - Any JSON-serializable data
 * @returns Canonical JSON string
 */
export function deterministicStringify(data: any): string {
	if (data === null) return 'null';
	if (data === undefined) return 'undefined';
	if (typeof data === 'number') {
		if (isNaN(data)) return 'NaN';
		if (!isFinite(data)) return data > 0 ? 'Infinity' : '-Infinity';
		return String(data);
	}
	if (typeof data === 'boolean') return String(data);
	if (typeof data === 'string') return JSON.stringify(data);
	
	if (Array.isArray(data)) {
		const items = data.map(item => deterministicStringify(item));
		return '[' + items.join(',') + ']';
	}
	
	if (typeof data === 'object') {
		const keys = Object.keys(data).sort();
		const pairs = keys.map(key => {
			const value = deterministicStringify(data[key]);
			return JSON.stringify(key) + ':' + value;
		});
		return '{' + pairs.join(',') + '}';
	}
	
	// Fallback for other types
	return JSON.stringify(data);
}

// ═══════════════════════════════════════════════════════════════════
// HASHING
// ═══════════════════════════════════════════════════════════════════

/**
 * Create SHA-256 hash of data
 * 
 * @param data - Any data to hash
 * @returns SHA-256 hash (64 hex chars)
 */
export function hashData(data: any): Hash {
	const canonical = deterministicStringify(data);
	return createHash('sha256').update(canonical, 'utf8').digest('hex') as Hash;
}

/**
 * Create SHA-256 hash of a string
 * 
 * @param str - String to hash
 * @returns SHA-256 hash (64 hex chars)
 */
export function hashString(str: string): Hash {
	return createHash('sha256').update(str, 'utf8').digest('hex') as Hash;
}

// ═══════════════════════════════════════════════════════════════════
// EVENT BODY CONSTRUCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract event body from a full event (for hashing/signing)
 * 
 * The event body excludes the 'id' and 'sig' fields since:
 * - id is computed FROM the body hash
 * - sig is computed FROM the body hash
 * 
 * @param event - Provenance event (with or without id/sig)
 * @returns Canonical event body
 */
export function extractEventBody(event: Partial<ProvenanceEvent>): EventBody {
	// Extract id and sig if they exist, keeping only the body fields
	const eventRecord = event as Record<string, unknown>;
	const { id, sig, ...body } = eventRecord;
	return body as EventBody;
}

/**
 * Create canonical event body from components
 * 
 * Field order is critical for deterministic hashing!
 * 
 * @param components - Event body components
 * @returns Event body with fields in canonical order
 */
export function createEventBody(components: {
	author: string;
	payload: any;
	itc: any;
	parents: string[];
	merkle_root?: string;
	timestamp: string;
	meta: any;
}): EventBody {
	return {
		author: components.author,
		payload: components.payload,
		itc: components.itc,
		parents: components.parents,
		merkle_root: components.merkle_root,
		timestamp: components.timestamp,
		meta: components.meta
	};
}

// ═══════════════════════════════════════════════════════════════════
// EVENT ID GENERATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compute content-addressed event ID
 * 
 * Event ID = SHA-256(canonical(event body))
 * 
 * @param eventBody - Event body (without id/sig)
 * @returns Content-addressed event ID
 */
export function computeEventId(eventBody: EventBody): Hash {
	return hashData(eventBody);
}

// ═══════════════════════════════════════════════════════════════════
// SEA SIGNING
// ═══════════════════════════════════════════════════════════════════

/**
 * Sign an event with SEA
 * 
 * Process:
 * 1. Compute event ID (content hash)
 * 2. Sign the event ID with SEA
 * 3. Return SEA signature format {m, s}
 * 
 * @param eventBody - Event body to sign
 * @returns SEA signature {m: eventId, s: signature}
 * @throws Error if user not authenticated or signing fails
 */
export async function signEvent(eventBody: EventBody): Promise<{ eventId: Hash; signature: SEASignature }> {
	// Check authentication
	if (!holsterUser.is?.pub) {
		throw new Error('User must be authenticated to sign events');
	}
	
	// Compute event ID
	const eventId = computeEventId(eventBody);
	
	try {
		// Sign the event ID with SEA
		// SEA.sign returns {m: message, s: signature}
		const signed = await holster.SEA.sign(eventId, holsterUser.is);
		
		if (!signed || !signed.m || !signed.s) {
			throw new Error('SEA signing failed - invalid signature format');
		}
		
		return {
			eventId: eventId,
			signature: {
				m: signed.m,
				s: signed.s
			}
		};
	} catch (error) {
		console.error('[PROVENANCE-SIGNING] SEA signing error:', error);
		throw new Error(`Failed to sign event: ${error}`);
	}
}

/**
 * Sign an event with a specific keypair (for testing/advanced usage)
 * 
 * @param eventBody - Event body to sign
 * @param keypair - SEA keypair {pub, priv, epub, epriv}
 * @returns SEA signature
 */
export async function signEventWithKeypair(
	eventBody: EventBody,
	keypair: any
): Promise<{ eventId: Hash; signature: SEASignature }> {
	// Compute event ID
	const eventId = computeEventId(eventBody);
	
	try {
		// Sign with provided keypair
		const signed = await holster.SEA.sign(eventId, keypair);
		
		if (!signed || !signed.m || !signed.s) {
			throw new Error('SEA signing failed - invalid signature format');
		}
		
		return {
			eventId: eventId,
			signature: {
				m: signed.m,
				s: signed.s
			}
		};
	} catch (error) {
		console.error('[PROVENANCE-SIGNING] SEA signing error:', error);
		throw new Error(`Failed to sign event with keypair: ${error}`);
	}
}

// ═══════════════════════════════════════════════════════════════════
// SEA VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify a provenance event's signature
 * 
 * Process:
 * 1. Extract event body (without id/sig)
 * 2. Recompute event ID
 * 3. Verify event ID matches
 * 4. Verify SEA signature with author's public key
 * 
 * @param event - Provenance event to verify
 * @returns true if signature is valid, false otherwise
 */
export async function verifyEventSignature(event: ProvenanceEvent): Promise<boolean> {
	try {
		// Step 1: Extract event body
		const eventBody = extractEventBody(event);
		
		// Step 2: Recompute event ID
		const recomputedId = computeEventId(eventBody);
		
		// Step 3: Check event ID matches
		if (recomputedId !== event.id) {
			console.warn('[PROVENANCE-SIGNING] Event ID mismatch:', {
				provided: event.id,
				recomputed: recomputedId
			});
			return false;
		}
		
		// Step 4: Verify SEA signature
		const verified = await holster.SEA.verify(event.sig, event.author);
		
		// SEA.verify returns the message on success, undefined on failure
		// The message should be the event ID
		if (verified !== event.id) {
			console.warn('[PROVENANCE-SIGNING] Signature verification failed:', {
				eventId: event.id,
				author: event.author,
				verified: verified
			});
			return false;
		}
		
		return true;
		
	} catch (error) {
		console.error('[PROVENANCE-SIGNING] Verification error:', error);
		return false;
	}
}

/**
 * Verify event integrity (hash chain)
 * 
 * Checks that:
 * - Event ID matches content hash
 * - Signature is valid
 * - Event structure is well-formed
 * 
 * @param event - Provenance event to verify
 * @returns Verification result with details
 */
export async function verifyEventIntegrity(event: ProvenanceEvent): Promise<{
	valid: boolean;
	errors: string[];
}> {
	const errors: string[] = [];
	
	try {
		// Check 1: Event ID matches content hash
		const eventBody = extractEventBody(event);
		const recomputedId = computeEventId(eventBody);
		if (recomputedId !== event.id) {
			errors.push(`Event ID mismatch: ${event.id} !== ${recomputedId}`);
		}
		
		// Check 2: Signature is valid
		const sigValid = await verifyEventSignature(event);
		if (!sigValid) {
			errors.push('Invalid signature');
		}
		
		// Check 3: Author matches signature
		if (event.author !== event.sig.m && event.id !== event.sig.m) {
			errors.push('Signature message does not match event ID');
		}
		
		// Check 4: Required fields present
		if (!event.author) errors.push('Missing author');
		if (!event.payload) errors.push('Missing payload');
		if (!event.itc) errors.push('Missing ITC stamp');
		if (!event.parents) errors.push('Missing parents array');
		if (!event.timestamp) errors.push('Missing timestamp');
		if (!event.meta) errors.push('Missing metadata');
		
		return {
			valid: errors.length === 0,
			errors
		};
		
	} catch (error) {
		errors.push(`Verification exception: ${error}`);
		return {
			valid: false,
			errors
		};
	}
}

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Get current user's public key
 * 
 * @returns Public key or null if not authenticated
 */
export function getCurrentUserPubKey(): string | null {
	return holsterUser.is?.pub || null;
}

/**
 * Check if user is authenticated and can sign events
 * 
 * @returns true if authenticated, false otherwise
 */
export function canSignEvents(): boolean {
	return !!(holsterUser.is?.pub && holsterUser.is?.priv);
}

/**
 * Validate hash format
 * 
 * @param hash - Hash to validate
 * @returns true if valid SHA-256 hash
 */
export function isValidHash(hash: string): boolean {
	return /^[0-9a-f]{64}$/.test(hash);
}

// ═══════════════════════════════════════════════════════════════════
// BATCH OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Sign multiple events in batch
 * 
 * @param eventBodies - Array of event bodies to sign
 * @returns Array of signed events with IDs and signatures
 */
export async function signEventsBatch(
	eventBodies: EventBody[]
): Promise<Array<{ eventId: Hash; signature: SEASignature }>> {
	const results = [];
	
	for (const body of eventBodies) {
		try {
			const signed = await signEvent(body);
			results.push(signed);
		} catch (error) {
			console.error('[PROVENANCE-SIGNING] Batch signing error:', error);
			throw error;
		}
	}
	
	return results;
}

/**
 * Verify multiple events in batch
 * 
 * @param events - Array of events to verify
 * @returns Array of verification results
 */
export async function verifyEventsBatch(
	events: ProvenanceEvent[]
): Promise<Array<{ eventId: Hash; valid: boolean; errors: string[] }>> {
	const results = [];
	
	for (const event of events) {
		const result = await verifyEventIntegrity(event);
		results.push({
			eventId: event.id,
			...result
		});
	}
	
	return results;
}

