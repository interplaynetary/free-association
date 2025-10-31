/**
 * Provenance Verification Engine
 * 
 * This module provides comprehensive verification of provenance events:
 * - Hash integrity (event ID = content hash)
 * - Cryptographic signatures (SEA verification)
 * - ITC causality validation
 * - Parent chain validation
 * - Full recursive verification
 * 
 * Implements the verification algorithm from the provenance design:
 * 1. Recompute canonical hash
 * 2. Verify signature with author's public key
 * 3. Verify ITC merge semantics
 * 4. Recursively verify parents
 */

import type { ProvenanceEvent, Hash } from './provenance-event-schema';
import { verifyEventSignature, verifyEventIntegrity } from './provenance-signing.svelte';
import { getEvent, getEventsBatch } from './provenance-dag.svelte';
import * as ITC from '$lib/utils/primitives/itc';
import type { Stamp as ITCStamp } from '$lib/utils/primitives/itc';

// ═══════════════════════════════════════════════════════════════════
// VERIFICATION RESULT TYPES
// ═══════════════════════════════════════════════════════════════════

export interface VerificationResult {
	valid: boolean;
	errors: string[];
	warnings: string[];
	checks: {
		hash: boolean;
		signature: boolean;
		itc: boolean;
		parents: boolean;
		structure: boolean;
	};
	timestamp: number;
}

export interface RecursiveVerificationResult extends VerificationResult {
	eventId: Hash;
	depth: number;
	parentResults: RecursiveVerificationResult[];
}

// ═══════════════════════════════════════════════════════════════════
// SINGLE EVENT VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify a single provenance event
 * 
 * Checks:
 * 1. Hash integrity (event ID matches content)
 * 2. Signature validity (author signed this event)
 * 3. Structure validity (all required fields present)
 * 4. ITC stamp validity (well-formed)
 * 5. Parents exist (optional - can skip for efficiency)
 * 
 * @param event - Event to verify
 * @param checkParentsExist - Whether to verify parent events exist (default: false)
 * @param fromUser - Optional: verify using another user's DAG
 * @returns Verification result with detailed checks
 */
export async function verifyEvent(
	event: ProvenanceEvent,
	checkParentsExist: boolean = false,
	fromUser?: string
): Promise<VerificationResult> {
	const errors: string[] = [];
	const warnings: string[] = [];
	const checks = {
		hash: false,
		signature: false,
		itc: false,
		parents: false,
		structure: false
	};
	
	try {
		// Check 1: Hash integrity and signature
		const integrityResult = await verifyEventIntegrity(event);
		if (!integrityResult.valid) {
			errors.push(...integrityResult.errors);
		} else {
			checks.hash = true;
			checks.signature = true;
		}
		
		// Check 2: Structure validity
		try {
			validateEventStructure(event);
			checks.structure = true;
		} catch (error) {
			errors.push(`Structure validation failed: ${error}`);
		}
		
		// Check 3: ITC stamp validity
		try {
			validateITCStamp(event.itc);
			checks.itc = true;
		} catch (error) {
			errors.push(`ITC validation failed: ${error}`);
		}
		
		// Check 4: Parents exist (optional)
		if (checkParentsExist && event.parents.length > 0) {
			const parentsExist = await verifyParentsExist(event.parents, fromUser);
			if (!parentsExist.allExist) {
				errors.push(`Missing parent events: ${parentsExist.missing.join(', ')}`);
			} else {
				checks.parents = true;
			}
		} else {
			checks.parents = true; // Skip check
		}
		
		return {
			valid: errors.length === 0,
			errors,
			warnings,
			checks,
			timestamp: Date.now()
		};
		
	} catch (error) {
		errors.push(`Verification exception: ${error}`);
		return {
			valid: false,
			errors,
			warnings,
			checks,
			timestamp: Date.now()
		};
	}
}

/**
 * Verify event structure (required fields)
 * 
 * @param event - Event to validate
 * @throws Error if structure is invalid
 */
function validateEventStructure(event: ProvenanceEvent): void {
	if (!event.id || typeof event.id !== 'string') {
		throw new Error('Invalid or missing id');
	}
	
	if (!event.author || typeof event.author !== 'string') {
		throw new Error('Invalid or missing author');
	}
	
	if (!event.payload) {
		throw new Error('Missing payload');
	}
	
	if (!event.itc) {
		throw new Error('Missing ITC stamp');
	}
	
	if (!Array.isArray(event.parents)) {
		throw new Error('Invalid parents array');
	}
	
	if (!event.timestamp) {
		throw new Error('Missing timestamp');
	}
	
	if (!event.meta || !event.meta.type) {
		throw new Error('Invalid or missing metadata');
	}
	
	if (!event.sig || !event.sig.m || !event.sig.s) {
		throw new Error('Invalid or missing signature');
	}
}

/**
 * Verify ITC stamp is well-formed
 * 
 * @param stamp - ITC stamp to validate
 * @throws Error if stamp is invalid
 */
function validateITCStamp(stamp: ITCStamp): void {
	if (!stamp || typeof stamp !== 'object') {
		throw new Error('ITC stamp must be an object');
	}
	
	if (!('id' in stamp) || !('event' in stamp)) {
		throw new Error('ITC stamp missing id or event field');
	}
	
	// Validate id component
	const id = stamp.id;
	if (id !== 0 && id !== 1 && typeof id !== 'object') {
		throw new Error('ITC id must be 0, 1, or {l, r}');
	}
	
	// Validate event component
	const event = stamp.event;
	if (typeof event !== 'number' && typeof event !== 'object') {
		throw new Error('ITC event must be number or {n, l, r}');
	}
}

// ═══════════════════════════════════════════════════════════════════
// ITC CAUSALITY VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify ITC causality between events
 * 
 * Checks that the child event's ITC stamp is consistent with:
 * - Merging parent stamps
 * - Incrementing for the author's event
 * 
 * @param childEvent - Child event to verify
 * @param parentEvents - Parent events
 * @returns Verification result
 */
export async function verifyITCCausality(
	childEvent: ProvenanceEvent,
	parentEvents: ProvenanceEvent[]
): Promise<{ valid: boolean; errors: string[] }> {
	const errors: string[] = [];
	
	try {
		// Case 1: Seed event (no parents)
		if (parentEvents.length === 0) {
			// Seed events can have any ITC stamp
			// Typically should be seed() or close to it
			return { valid: true, errors: [] };
		}
		
		// Case 2: Single parent (linear history)
		if (parentEvents.length === 1) {
			const parentStamp = parentEvents[0].itc;
			const childStamp = childEvent.itc;
			
			// Child should be at least as advanced as parent
			if (!ITC.leq(parentStamp, childStamp)) {
				errors.push('Child ITC stamp does not advance parent stamp');
			}
			
			// Child should not be equal to parent (unless concurrent update)
			if (ITC.equals(parentStamp, childStamp)) {
				errors.push('Child ITC stamp equals parent stamp (no advancement)');
			}
		}
		
		// Case 3: Multiple parents (merge)
		if (parentEvents.length > 1) {
			// Compute expected merged stamp
			let mergedStamp = parentEvents[0].itc;
			for (let i = 1; i < parentEvents.length; i++) {
				mergedStamp = ITC.join(mergedStamp, parentEvents[i].itc);
			}
			
			// Child stamp should be at least as advanced as merged stamp
			const childStamp = childEvent.itc;
			if (!ITC.leq(mergedStamp, childStamp)) {
				errors.push('Child ITC stamp does not advance merged parent stamps');
			}
		}
		
		return {
			valid: errors.length === 0,
			errors
		};
		
	} catch (error) {
		errors.push(`ITC causality check exception: ${error}`);
		return { valid: false, errors };
	}
}

/**
 * Check if two events are causally related
 * 
 * @param event1 - First event
 * @param event2 - Second event
 * @returns Causal relationship: 'before', 'after', 'concurrent', 'equal'
 */
export function getCausalRelationship(
	event1: ProvenanceEvent,
	event2: ProvenanceEvent
): 'before' | 'after' | 'concurrent' | 'equal' {
	const stamp1 = event1.itc;
	const stamp2 = event2.itc;
	
	if (ITC.equals(stamp1, stamp2)) {
		return 'equal';
	}
	
	if (ITC.leq(stamp1, stamp2)) {
		return 'before';
	}
	
	if (ITC.leq(stamp2, stamp1)) {
		return 'after';
	}
	
	return 'concurrent';
}

// ═══════════════════════════════════════════════════════════════════
// PARENT CHAIN VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify that all parent events exist
 * 
 * @param parentIds - Array of parent event IDs
 * @param fromUser - Optional: check in another user's DAG
 * @returns Result with list of missing parents
 */
async function verifyParentsExist(
	parentIds: Hash[],
	fromUser?: string
): Promise<{ allExist: boolean; missing: Hash[] }> {
	const missing: Hash[] = [];
	
	for (const parentId of parentIds) {
		const parent = await getEvent(parentId, fromUser);
		if (!parent) {
			missing.push(parentId);
		}
	}
	
	return {
		allExist: missing.length === 0,
		missing
	};
}

/**
 * Recursively verify event and all ancestors
 * 
 * Performs full verification of the entire provenance chain.
 * 
 * @param eventId - Event ID to verify
 * @param maxDepth - Maximum recursion depth (default: 100)
 * @param fromUser - Optional: verify in another user's DAG
 * @returns Recursive verification result with parent results
 */
export async function verifyEventRecursive(
	eventId: Hash,
	maxDepth: number = 100,
	fromUser?: string
): Promise<RecursiveVerificationResult> {
	return await verifyEventRecursiveInternal(eventId, 0, maxDepth, new Set(), fromUser);
}

async function verifyEventRecursiveInternal(
	eventId: Hash,
	currentDepth: number,
	maxDepth: number,
	visited: Set<Hash>,
	fromUser?: string
): Promise<RecursiveVerificationResult> {
	// Base case: max depth reached
	if (currentDepth > maxDepth) {
		return {
			eventId,
			depth: currentDepth,
			valid: false,
			errors: ['Maximum recursion depth reached'],
			warnings: [],
			checks: {
				hash: false,
				signature: false,
				itc: false,
				parents: false,
				structure: false
			},
			parentResults: [],
			timestamp: Date.now()
		};
	}
	
	// Base case: already visited (cycle detection)
	if (visited.has(eventId)) {
		return {
			eventId,
			depth: currentDepth,
			valid: false,
			errors: ['Cycle detected in DAG'],
			warnings: [],
			checks: {
				hash: false,
				signature: false,
				itc: false,
				parents: false,
				structure: false
			},
			parentResults: [],
			timestamp: Date.now()
		};
	}
	
	visited.add(eventId);
	
	// Get event
	const event = await getEvent(eventId, fromUser);
	if (!event) {
		return {
			eventId,
			depth: currentDepth,
			valid: false,
			errors: ['Event not found'],
			warnings: [],
			checks: {
				hash: false,
				signature: false,
				itc: false,
				parents: false,
				structure: false
			},
			parentResults: [],
			timestamp: Date.now()
		};
	}
	
	// Verify this event
	const result = await verifyEvent(event, false, fromUser);
	
	// Recursively verify parents
	const parentResults: RecursiveVerificationResult[] = [];
	if (event.parents.length > 0) {
		const parentEvents = await getEventsBatch(event.parents, fromUser);
		
		// Verify ITC causality
		const itcResult = await verifyITCCausality(event, Array.from(parentEvents.values()));
		if (!itcResult.valid) {
			result.errors.push(...itcResult.errors);
			result.valid = false;
		}
		
		// Recursively verify each parent
		for (const parentId of event.parents) {
			const parentResult = await verifyEventRecursiveInternal(
				parentId,
				currentDepth + 1,
				maxDepth,
				visited,
				fromUser
			);
			parentResults.push(parentResult);
			
			// Propagate invalidity upward
			if (!parentResult.valid) {
				result.valid = false;
				result.warnings.push(`Parent ${parentId.substring(0, 16)}... is invalid`);
			}
		}
	}
	
	return {
		eventId,
		depth: currentDepth,
		...result,
		parentResults
	};
}

// ═══════════════════════════════════════════════════════════════════
// BATCH VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify multiple events in batch
 * 
 * @param events - Array of events to verify
 * @param checkParentsExist - Whether to verify parent existence
 * @param fromUser - Optional: verify using another user's DAG
 * @returns Map of eventId -> verification result
 */
export async function verifyEventsBatch(
	events: ProvenanceEvent[],
	checkParentsExist: boolean = false,
	fromUser?: string
): Promise<Map<Hash, VerificationResult>> {
	const results = new Map<Hash, VerificationResult>();
	
	for (const event of events) {
		const result = await verifyEvent(event, checkParentsExist, fromUser);
		results.set(event.id, result);
	}
	
	return results;
}

// ═══════════════════════════════════════════════════════════════════
// VERIFICATION UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if an event is a root (has no parents)
 * 
 * @param event - Event to check
 * @returns true if event is a root
 */
export function isRootEvent(event: ProvenanceEvent): boolean {
	return event.parents.length === 0;
}

/**
 * Check if an event is a merge (has multiple parents)
 * 
 * @param event - Event to check
 * @returns true if event is a merge
 */
export function isMergeEvent(event: ProvenanceEvent): boolean {
	return event.parents.length > 1;
}

/**
 * Get verification summary statistics
 * 
 * @param result - Verification result
 * @returns Summary statistics
 */
export function getVerificationSummary(result: VerificationResult): {
	passedChecks: number;
	totalChecks: number;
	errorCount: number;
	warningCount: number;
} {
	const checks = Object.values(result.checks);
	const passedChecks = checks.filter(c => c).length;
	const totalChecks = checks.length;
	
	return {
		passedChecks,
		totalChecks,
		errorCount: result.errors.length,
		warningCount: result.warnings.length
	};
}

/**
 * Format verification result as human-readable string
 * 
 * @param result - Verification result
 * @returns Formatted string
 */
export function formatVerificationResult(result: VerificationResult): string {
	const summary = getVerificationSummary(result);
	const status = result.valid ? '✅ VALID' : '❌ INVALID';
	
	let output = `${status}\n`;
	output += `Checks: ${summary.passedChecks}/${summary.totalChecks} passed\n`;
	
	if (result.errors.length > 0) {
		output += `\nErrors (${result.errors.length}):\n`;
		result.errors.forEach((error, i) => {
			output += `  ${i + 1}. ${error}\n`;
		});
	}
	
	if (result.warnings.length > 0) {
		output += `\nWarnings (${result.warnings.length}):\n`;
		result.warnings.forEach((warning, i) => {
			output += `  ${i + 1}. ${warning}\n`;
		});
	}
	
	return output;
}

