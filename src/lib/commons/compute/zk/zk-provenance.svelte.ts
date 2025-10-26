/**
 * ZK Provenance Integration
 * 
 * Simple integration between provenance system and ZK circuits.
 * Follows DRY principle - minimal conversion overhead.
 * 
 * Reference: https://docs.o1labs.org/o1js
 */

import { Field } from 'o1js';
import type { ProvenanceEvent, Hash } from '../provenance/provenance-event-schema';
import { hashData } from '../provenance/provenance-signing.svelte';
import {
	EventIntegrityProgram,
	EventChainProgram,
	AllocationProgram,
	AllocationRollupProgram,
	compileZkPrograms,
	verifyProof
} from './zk-programs';

// ═══════════════════════════════════════════════════════════════════
// CONVERSIONS (Minimal Overhead)
// ═══════════════════════════════════════════════════════════════════

/**
 * Convert hash string to Field
 * Takes first 254 bits to fit in Field safely
 */
function hashToField(hash: string): Field {
	const truncated = hash.substring(0, 63); // 252 bits
	return Field(BigInt('0x' + truncated));
}

/**
 * Convert number to Field (with fixed precision)
 * Uses 6 decimal places of precision
 */
function numberToField(n: number): Field {
	return Field(BigInt(Math.floor(n * 1_000_000)));
}

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE
// ═══════════════════════════════════════════════════════════════════

let zkInitialized = false;
let compiledKeys: Awaited<ReturnType<typeof compileZkPrograms>> | null = null;

/**
 * Initialize ZK system (compile once, cache keys)
 * WARNING: Takes 1-5 minutes!
 */
export async function initializeZkSystem(): Promise<void> {
	if (zkInitialized) return;
	
	console.log('[ZK] Initializing (this takes 1-5 minutes)...');
	compiledKeys = await compileZkPrograms();
	zkInitialized = true;
	console.log('[ZK] ✅ Ready');
}

export function isZkReady(): boolean {
	return zkInitialized && compiledKeys !== null;
}

// ═══════════════════════════════════════════════════════════════════
// EVENT INTEGRITY PROOFS
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate proof that event hash is correct
 * Privacy: Event contents stay private
 */
export async function proveEventIntegrity(event: ProvenanceEvent) {
	if (!isZkReady()) throw new Error('ZK not initialized');
	
	console.log(`[ZK] Proving event ${event.id.substring(0, 16)}...`);
	
	// Convert to Fields
	const eventId = hashToField(event.id);
	const author = hashToField(hashData(event.author) as Hash);
	const parent0 = event.parents[0] ? hashToField(event.parents[0]) : Field(0);
	const parent1 = event.parents[1] ? hashToField(event.parents[1]) : Field(0);
	const payloadHash = hashToField(hashData(event.payload) as Hash);
	const timestamp = Field(BigInt(new Date(event.timestamp).getTime()));
	
	// Generate proof
	const result = await EventIntegrityProgram.verify(
		eventId,
		author,
		parent0,
		parent1,
		payloadHash,
		timestamp
	);
	
	console.log('[ZK] ✅ Event proof generated');
	
	return {
		proof: result.proof.toJSON(),
		verificationKey: compiledKeys!.eventIntegrity.verificationKey
	};
}

/**
 * Verify event integrity proof
 */
export async function verifyEventIntegrity(proofJSON: string, vkey: string): Promise<boolean> {
	return await verifyProof(proofJSON, vkey);
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION PROOFS ⭐ KILLER APP
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate proof that allocation is correct WITHOUT revealing MR values
 * 
 * This is the killer app!
 * Proves: allocation = (mrValue / mrSum) * totalCapacity
 * Privacy: mrValue and mrSum stay PRIVATE!
 */
export async function proveAllocation(params: {
	recipientId: string;
	mrValue: number;        // PRIVATE!
	mrSum: number;          // PRIVATE!
	totalCapacity: number;  // PRIVATE!
}) {
	if (!isZkReady()) throw new Error('ZK not initialized');
	
	console.log(`[ZK] Proving allocation for ${params.recipientId.substring(0, 16)}...`);
	console.log(`[ZK]   MR values kept PRIVATE!`);
	
	// Convert to Fields
	const recipientId = hashToField(hashData(params.recipientId) as Hash);
	const mrValue = numberToField(params.mrValue);
	const mrSum = numberToField(params.mrSum);
	const totalCapacity = numberToField(params.totalCapacity);
	
	// Generate proof
	const result = await AllocationProgram.verify(
		recipientId,
		mrValue,
		mrSum,
		totalCapacity
	);
	
	// Extract public output (allocated amount)
	const allocatedAmount = Number(result.proof.publicOutput.toBigInt()) / 1_000_000;
	
	console.log('[ZK] ✅ Allocation proof generated');
	console.log(`[ZK]   Allocated: ${allocatedAmount.toFixed(2)}`);
	
	return {
		proof: result.proof.toJSON(),
		allocatedAmount,
		verificationKey: compiledKeys!.allocation.verificationKey
	};
}

/**
 * Verify allocation proof
 */
export async function verifyAllocation(proofJSON: string, vkey: string): Promise<boolean> {
	return await verifyProof(proofJSON, vkey);
}

// ═══════════════════════════════════════════════════════════════════
// RECURSIVE CHAIN PROOFS
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate recursive chain proof (linear recursion)
 * Compresses event chain into constant-size proof
 */
export async function proveEventChain(events: ProvenanceEvent[]) {
	if (!isZkReady()) throw new Error('ZK not initialized');
	if (events.length === 0) throw new Error('Empty event chain');
	
	console.log(`[ZK] Proving chain of ${events.length} events...`);
	
	// Base case: First event
	const first = events[0];
	const eventId0 = hashToField(first.id);
	const author0 = hashToField(hashData(first.author) as Hash);
	const payload0 = hashToField(hashData(first.payload) as Hash);
	const timestamp0 = Field(BigInt(new Date(first.timestamp).getTime()));
	
	let result = await EventChainProgram.baseCase(eventId0, author0, payload0, timestamp0);
	let proof = result.proof;
	
	// Recursive step: Each subsequent event
	for (let i = 1; i < events.length; i++) {
		const event = events[i];
		const eventId = hashToField(event.id);
		const author = hashToField(hashData(event.author) as Hash);
		const parentId = event.parents[0] ? hashToField(event.parents[0]) : Field(0);
		const payloadHash = hashToField(hashData(event.payload) as Hash);
		const timestamp = Field(BigInt(new Date(event.timestamp).getTime()));
		
		result = await EventChainProgram.step(
			eventId,
			proof,
			author,
			parentId,
			payloadHash,
			timestamp
		);
		proof = result.proof;
	}
	
	console.log('[ZK] ✅ Chain proof generated (constant size!)');
	
	return {
		proof: proof.toJSON(),
		verificationKey: compiledKeys!.eventChain.verificationKey
	};
}

/**
 * Verify chain proof
 */
export async function verifyChain(proofJSON: string, vkey: string): Promise<boolean> {
	return await verifyProof(proofJSON, vkey);
}

// ═══════════════════════════════════════════════════════════════════
// TREE-BASED ROLLUP
// ═══════════════════════════════════════════════════════════════════

/**
 * Generate rollup proof (tree recursion)
 * Compresses N allocations into O(log N) proof
 * 
 * This uses tree-based recursion like Mina's rollup mechanism
 */
export async function proveAllocationRollup(
	allocations: number[],
	totalCapacity: number
) {
	if (!isZkReady()) throw new Error('ZK not initialized');
	if (allocations.length === 0) throw new Error('Empty allocations');
	
	console.log(`[ZK] Proving rollup of ${allocations.length} allocations...`);
	
	const capacityField = numberToField(totalCapacity);
	
	// Build proofs in tree structure (bottom-up)
	const baseResults = await Promise.all(
		allocations.map(amount =>
			AllocationRollupProgram.baseCase(capacityField, numberToField(amount))
		)
	);
	
	// Start with base proofs
	type ProofResult = Awaited<ReturnType<typeof AllocationRollupProgram.baseCase>>;
	let currentLevel: ProofResult[] = baseResults;
	
	// Merge in pairs (tree recursion)
	while (currentLevel.length > 1) {
		const nextLevel: ProofResult[] = [];
		for (let i = 0; i < currentLevel.length; i += 2) {
			if (i + 1 < currentLevel.length) {
				// Merge pair
				const merged = await AllocationRollupProgram.merge(
					capacityField,
					currentLevel[i].proof,
					currentLevel[i + 1].proof
				);
				nextLevel.push(merged);
			} else {
				// Odd one out
				nextLevel.push(currentLevel[i]);
			}
		}
		currentLevel = nextLevel;
	}
	
	const finalResult = currentLevel[0];
	const totalAllocated = Number(finalResult.proof.publicOutput.toBigInt()) / 1_000_000;
	
	console.log('[ZK] ✅ Rollup proof generated');
	console.log(`[ZK]   Total allocated: ${totalAllocated.toFixed(2)} / ${totalCapacity}`);
	
	return {
		proof: finalResult.proof.toJSON(),
		totalAllocated,
		verificationKey: compiledKeys!.rollup.verificationKey
	};
}

/**
 * Verify rollup proof
 */
export async function verifyRollup(proofJSON: string, vkey: string): Promise<boolean> {
	return await verifyProof(proofJSON, vkey);
}

// ═══════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function getZkStatus() {
	return {
		initialized: zkInitialized,
		ready: isZkReady(),
		circuits: compiledKeys ? ['event-integrity', 'event-chain', 'allocation', 'rollup'] : []
	};
}

/**
 * Export proof for sharing/storage
 */
export function exportProof(proof: { proof: string; verificationKey: string }) {
	return JSON.stringify(proof);
}

/**
 * Import proof
 */
export function importProof(json: string) {
	return JSON.parse(json);
}
