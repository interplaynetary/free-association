/**
 * Provenance Proof Construction
 * 
 * This module provides utilities for building compact provenance proofs:
 * - Path construction (target → roots)
 * - Minimal proof generation
 * - Proof verification
 * - Proof serialization/deserialization
 * 
 * A provenance proof demonstrates that a computation is legitimate by:
 * 1. Showing the chain of events from target back to trusted roots
 * 2. Including all necessary verification data (events, signatures)
 * 3. Providing a verification sequence that can be checked
 */

import type {
	ProvenanceEvent,
	ProvenancePath,
	ProvenanceProof,
	Hash
} from './provenance-event-schema';
import { traverseToRoots, getEvent, findPath } from './provenance-dag.svelte';
import {
	verifyEvent,
	verifyEventRecursive,
	verifyITCCausality,
	type VerificationResult
} from './provenance-verification.svelte';

// ═══════════════════════════════════════════════════════════════════
// PROOF TYPES
// ═══════════════════════════════════════════════════════════════════

export type ProofType = 'full' | 'shallow' | 'witness';

export interface ProofConstructionOptions {
	/** Type of proof to construct */
	proofType?: ProofType;
	
	/** Maximum depth to traverse */
	maxDepth?: number;
	
	/** Target root event IDs (stop traversal at these) */
	targetRoots?: Hash[];
	
	/** Whether to verify events during construction */
	verifyDuringConstruction?: boolean;
	
	/** Optional: source user's DAG */
	fromUser?: string;
}

// ═══════════════════════════════════════════════════════════════════
// PATH CONSTRUCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Build provenance path from target event to root(s)
 * 
 * @param targetId - Target event ID
 * @param options - Path construction options
 * @returns Provenance path with events in sequence
 */
export async function buildProvenancePath(
	targetId: Hash,
	options: ProofConstructionOptions = {}
): Promise<ProvenancePath | null> {
	const {
		maxDepth = 100,
		targetRoots = [],
		fromUser
	} = options;
	
	try {
		// Traverse from target to roots
		const events = await traverseToRoots(targetId, maxDepth, fromUser);
		
		if (events.length === 0) {
			return null;
		}
		
		// If target roots specified, filter events
		let filteredEvents = events;
		if (targetRoots.length > 0) {
			// Find the first root in the traversal that matches target roots
			const rootIndex = events.findIndex(e => targetRoots.includes(e.id));
			if (rootIndex !== -1) {
				filteredEvents = events.slice(0, rootIndex + 1);
			}
		}
		
		// Get actual root (last event in traversal)
		const rootEvent = filteredEvents[filteredEvents.length - 1];
		
		return {
			target_id: targetId,
			root_id: rootEvent.id,
			events: filteredEvents,
			length: filteredEvents.length,
			computed_at: Date.now()
		};
		
	} catch (error) {
		console.error('[PROVENANCE-PROOF] Error building path:', error);
		return null;
	}
}

/**
 * Build path between two specific events
 * 
 * @param fromId - Starting event ID
 * @param toId - Ending event ID
 * @param fromUser - Optional: source user's DAG
 * @returns Path or null if no path exists
 */
export async function buildPathBetween(
	fromId: Hash,
	toId: Hash,
	fromUser?: string
): Promise<ProvenancePath | null> {
	try {
		const events = await findPath(fromId, toId, fromUser);
		
		if (!events || events.length === 0) {
			return null;
		}
		
		return {
			target_id: fromId,
			root_id: toId,
			events,
			length: events.length,
			computed_at: Date.now()
		};
		
	} catch (error) {
		console.error('[PROVENANCE-PROOF] Error building path between events:', error);
		return null;
	}
}

// ═══════════════════════════════════════════════════════════════════
// PROOF CONSTRUCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Construct a provenance proof for a target event
 * 
 * Proof Types:
 * - **full**: Complete verification with all ancestors
 * - **shallow**: Only direct parents (depth=1)
 * - **witness**: Minimal proof with verification steps
 * 
 * @param targetId - Target event ID to prove
 * @param options - Proof construction options
 * @returns Provenance proof or null if construction fails
 */
export async function constructProof(
	targetId: Hash,
	options: ProofConstructionOptions = {}
): Promise<ProvenanceProof | null> {
	const {
		proofType = 'full',
		maxDepth = 100,
		verifyDuringConstruction = true,
		fromUser
	} = options;
	
	try {
		// Get target event
		const targetEvent = await getEvent(targetId, fromUser);
		if (!targetEvent) {
			console.error('[PROVENANCE-PROOF] Target event not found:', targetId);
			return null;
		}
		
		// Construct proof based on type
		switch (proofType) {
			case 'full':
				return await constructFullProof(targetEvent, maxDepth, verifyDuringConstruction, fromUser);
			
			case 'shallow':
				return await constructShallowProof(targetEvent, verifyDuringConstruction, fromUser);
			
			case 'witness':
				return await constructWitnessProof(targetEvent, verifyDuringConstruction, fromUser);
			
			default:
				throw new Error(`Unknown proof type: ${proofType}`);
		}
		
	} catch (error) {
		console.error('[PROVENANCE-PROOF] Error constructing proof:', error);
		return null;
	}
}

/**
 * Construct full proof (complete ancestor chain)
 */
async function constructFullProof(
	targetEvent: ProvenanceEvent,
	maxDepth: number,
	verify: boolean,
	fromUser?: string
): Promise<ProvenanceProof> {
	// Traverse to all ancestors
	const events = await traverseToRoots(targetEvent.id, maxDepth, fromUser);
	
	// Build verification steps
	const verificationSteps: Array<{ event_id: Hash; step: string; result: boolean }> = [];
	let isValid = true;
	
	if (verify) {
		// Verify each event
		for (const event of events) {
			const result = await verifyEvent(event, false, fromUser);
			
			verificationSteps.push({
				event_id: event.id,
				step: 'verify_event',
				result: result.valid
			});
			
			if (!result.valid) {
				isValid = false;
			}
		}
		
		// Verify ITC causality between parents and children
		for (const event of events) {
			if (event.parents.length > 0) {
				const parents = events.filter(e => event.parents.includes(e.id));
				const itcResult = await verifyITCCausality(event, parents);
				
				verificationSteps.push({
					event_id: event.id,
					step: 'verify_itc_causality',
					result: itcResult.valid
				});
				
				if (!itcResult.valid) {
					isValid = false;
				}
			}
		}
	}
	
	return {
		target_id: targetEvent.id,
		events,
		verification_steps: verificationSteps,
		proof_type: 'full',
		computed_at: Date.now(),
		valid: isValid
	};
}

/**
 * Construct shallow proof (only direct parents)
 */
async function constructShallowProof(
	targetEvent: ProvenanceEvent,
	verify: boolean,
	fromUser?: string
): Promise<ProvenanceProof> {
	// Only include target and direct parents
	const events: ProvenanceEvent[] = [targetEvent];
	
	for (const parentId of targetEvent.parents) {
		const parent = await getEvent(parentId, fromUser);
		if (parent) {
			events.push(parent);
		}
	}
	
	// Build verification steps
	const verificationSteps: Array<{ event_id: Hash; step: string; result: boolean }> = [];
	let isValid = true;
	
	if (verify) {
		// Verify target
		const targetResult = await verifyEvent(targetEvent, false, fromUser);
		verificationSteps.push({
			event_id: targetEvent.id,
			step: 'verify_target',
			result: targetResult.valid
		});
		
		if (!targetResult.valid) {
			isValid = false;
		}
		
		// Verify parents
		const parents = events.slice(1);
		for (const parent of parents) {
			const result = await verifyEvent(parent, false, fromUser);
			verificationSteps.push({
				event_id: parent.id,
				step: 'verify_parent',
				result: result.valid
			});
			
			if (!result.valid) {
				isValid = false;
			}
		}
		
		// Verify ITC causality
		if (parents.length > 0) {
			const itcResult = await verifyITCCausality(targetEvent, parents);
			verificationSteps.push({
				event_id: targetEvent.id,
				step: 'verify_itc_causality',
				result: itcResult.valid
			});
			
			if (!itcResult.valid) {
				isValid = false;
			}
		}
	}
	
	return {
		target_id: targetEvent.id,
		events,
		verification_steps: verificationSteps,
		proof_type: 'shallow',
		computed_at: Date.now(),
		valid: isValid
	};
}

/**
 * Construct witness proof (minimal headers only)
 */
async function constructWitnessProof(
	targetEvent: ProvenanceEvent,
	verify: boolean,
	fromUser?: string
): Promise<ProvenanceProof> {
	// Only include target event (minimal proof)
	const events: ProvenanceEvent[] = [targetEvent];
	
	// Build verification steps
	const verificationSteps: Array<{ event_id: Hash; step: string; result: boolean }> = [];
	let isValid = true;
	
	if (verify) {
		const result = await verifyEvent(targetEvent, false, fromUser);
		verificationSteps.push({
			event_id: targetEvent.id,
			step: 'verify_signature',
			result: result.checks.signature
		});
		
		verificationSteps.push({
			event_id: targetEvent.id,
			step: 'verify_hash',
			result: result.checks.hash
		});
		
		if (!result.valid) {
			isValid = false;
		}
	}
	
	return {
		target_id: targetEvent.id,
		events,
		verification_steps: verificationSteps,
		proof_type: 'witness',
		computed_at: Date.now(),
		valid: isValid
	};
}

// ═══════════════════════════════════════════════════════════════════
// PROOF VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Verify a provenance proof
 * 
 * Re-runs all verification steps to ensure proof is valid.
 * 
 * @param proof - Proof to verify
 * @param fromUser - Optional: source user's DAG
 * @returns Verification result
 */
export async function verifyProof(
	proof: ProvenanceProof,
	fromUser?: string
): Promise<{ valid: boolean; errors: string[] }> {
	const errors: string[] = [];
	
	try {
		// Check proof structure
		if (!proof.target_id || !proof.events || proof.events.length === 0) {
			errors.push('Invalid proof structure');
			return { valid: false, errors };
		}
		
		// Verify target event exists in proof
		const targetEvent = proof.events.find(e => e.id === proof.target_id);
		if (!targetEvent) {
			errors.push('Target event not found in proof');
			return { valid: false, errors };
		}
		
		// Re-verify all events in proof
		for (const event of proof.events) {
			const result = await verifyEvent(event, false, fromUser);
			if (!result.valid) {
				errors.push(`Event ${event.id.substring(0, 16)}... verification failed: ${result.errors.join(', ')}`);
			}
		}
		
		// Verify parent relationships (if full or shallow proof)
		if (proof.proof_type !== 'witness') {
			for (const event of proof.events) {
				for (const parentId of event.parents) {
					const parentExists = proof.events.some(e => e.id === parentId);
					if (!parentExists && proof.proof_type === 'full') {
						errors.push(`Parent ${parentId.substring(0, 16)}... missing from full proof`);
					}
				}
			}
		}
		
		return {
			valid: errors.length === 0,
			errors
		};
		
	} catch (error) {
		errors.push(`Proof verification exception: ${error}`);
		return { valid: false, errors };
	}
}

/**
 * Verify proof and return detailed result
 * 
 * @param proof - Proof to verify
 * @param fromUser - Optional: source user's DAG
 * @returns Detailed verification result
 */
export async function verifyProofDetailed(
	proof: ProvenanceProof,
	fromUser?: string
): Promise<{
	valid: boolean;
	errors: string[];
	warnings: string[];
	eventResults: Map<Hash, VerificationResult>;
}> {
	const errors: string[] = [];
	const warnings: string[] = [];
	const eventResults = new Map<Hash, VerificationResult>();
	
	try {
		// Verify each event individually
		for (const event of proof.events) {
			const result = await verifyEvent(event, false, fromUser);
			eventResults.set(event.id, result);
			
			if (!result.valid) {
				errors.push(...result.errors);
			}
			
			warnings.push(...result.warnings);
		}
		
		return {
			valid: errors.length === 0,
			errors,
			warnings,
			eventResults
		};
		
	} catch (error) {
		errors.push(`Detailed proof verification exception: ${error}`);
		return {
			valid: false,
			errors,
			warnings,
			eventResults
		};
	}
}

// ═══════════════════════════════════════════════════════════════════
// PROOF UTILITIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get proof size (number of events)
 * 
 * @param proof - Proof to measure
 * @returns Number of events in proof
 */
export function getProofSize(proof: ProvenanceProof): number {
	return proof.events.length;
}

/**
 * Get proof depth (longest chain in proof)
 * 
 * @param proof - Proof to measure
 * @returns Maximum depth
 */
export function getProofDepth(proof: ProvenanceProof): number {
	if (proof.events.length === 0) return 0;
	
	const depths = new Map<Hash, number>();
	
	// Compute depth for each event
	function computeDepth(event: ProvenanceEvent): number {
		if (depths.has(event.id)) {
			return depths.get(event.id)!;
		}
		
		if (event.parents.length === 0) {
			depths.set(event.id, 0);
			return 0;
		}
		
		let maxParentDepth = -1;
		for (const parentId of event.parents) {
			const parent = proof.events.find(e => e.id === parentId);
			if (parent) {
				const parentDepth = computeDepth(parent);
				maxParentDepth = Math.max(maxParentDepth, parentDepth);
			}
		}
		
		const depth = maxParentDepth + 1;
		depths.set(event.id, depth);
		return depth;
	}
	
	// Find target and compute its depth
	const target = proof.events.find(e => e.id === proof.target_id);
	return target ? computeDepth(target) : 0;
}

/**
 * Export proof to JSON (for transmission/storage)
 * 
 * @param proof - Proof to export
 * @returns JSON string
 */
export function exportProofJSON(proof: ProvenanceProof): string {
	return JSON.stringify(proof, null, 2);
}

/**
 * Import proof from JSON
 * 
 * @param json - JSON string
 * @returns Proof or null if invalid
 */
export function importProofJSON(json: string): ProvenanceProof | null {
	try {
		const data = JSON.parse(json);
		// TODO: Validate against schema
		return data as ProvenanceProof;
	} catch (error) {
		console.error('[PROVENANCE-PROOF] Error importing proof:', error);
		return null;
	}
}

/**
 * Create proof summary for display
 * 
 * @param proof - Proof to summarize
 * @returns Human-readable summary
 */
export function createProofSummary(proof: ProvenanceProof): string {
	const size = getProofSize(proof);
	const depth = getProofDepth(proof);
	const status = proof.valid ? '✅ VALID' : '❌ INVALID';
	
	let summary = `Provenance Proof Summary\n`;
	summary += `${'='.repeat(50)}\n`;
	summary += `Status: ${status}\n`;
	summary += `Type: ${proof.proof_type}\n`;
	summary += `Target: ${proof.target_id.substring(0, 16)}...\n`;
	summary += `Size: ${size} events\n`;
	summary += `Depth: ${depth}\n`;
	summary += `Verification Steps: ${proof.verification_steps.length}\n`;
	
	if (proof.verification_steps.length > 0) {
		const passed = proof.verification_steps.filter(s => s.result).length;
		summary += `  Passed: ${passed}/${proof.verification_steps.length}\n`;
	}
	
	return summary;
}

// ═══════════════════════════════════════════════════════════════════
// BATCH PROOF OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Construct proofs for multiple targets
 * 
 * @param targetIds - Array of target event IDs
 * @param options - Proof construction options
 * @returns Map of targetId -> proof
 */
export async function constructProofsBatch(
	targetIds: Hash[],
	options: ProofConstructionOptions = {}
): Promise<Map<Hash, ProvenanceProof>> {
	const proofs = new Map<Hash, ProvenanceProof>();
	
	for (const targetId of targetIds) {
		const proof = await constructProof(targetId, options);
		if (proof) {
			proofs.set(targetId, proof);
		}
	}
	
	return proofs;
}

/**
 * Verify multiple proofs in batch
 * 
 * @param proofs - Array of proofs to verify
 * @param fromUser - Optional: source user's DAG
 * @returns Map of targetId -> verification result
 */
export async function verifyProofsBatch(
	proofs: ProvenanceProof[],
	fromUser?: string
): Promise<Map<Hash, { valid: boolean; errors: string[] }>> {
	const results = new Map<Hash, { valid: boolean; errors: string[] }>();
	
	for (const proof of proofs) {
		const result = await verifyProof(proof, fromUser);
		results.set(proof.target_id, result);
	}
	
	return results;
}

