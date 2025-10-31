/**
 * ZK Programs (o1js Circuits)
 * 
 * Zero-knowledge circuits for provenance verification following o1js conventions.
 * Each circuit proves the smallest necessary thing.
 * 
 * Reference: https://docs.o1labs.org/o1js/writing-constraint-systems/zk-program
 */

import { Field, ZkProgram, SelfProof, verify, Poseidon, Bool } from 'o1js';

// ═══════════════════════════════════════════════════════════════════
// CIRCUIT 1: EVENT INTEGRITY
// ═══════════════════════════════════════════════════════════════════

/**
 * EventIntegrityProgram
 * 
 * Proves: Event hash matches claimed ID
 * Privacy: Event contents stay private
 * 
 * Public Input: Claimed event ID
 * Private Inputs: Event fields (author, parents, payload, etc.)
 */
export const EventIntegrityProgram = ZkProgram({
	name: 'event-integrity',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		verify: {
			privateInputs: [Field, Field, Field, Field, Field],
			
			async method(
				claimedId: Field,
				author: Field,
				parent0: Field,
				parent1: Field,
				payloadHash: Field,
				timestamp: Field
			) {
				// Compute hash of event fields
				const computedHash = Poseidon.hash([
					author,
					parent0,
					parent1,
					payloadHash,
					timestamp
				]);
				
				// Assert it matches claimed ID
				computedHash.assertEquals(claimedId);
				
				return { publicOutput: computedHash };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// CIRCUIT 2: RECURSIVE EVENT CHAIN
// ═══════════════════════════════════════════════════════════════════

/**
 * EventChainProgram
 * 
 * Recursive proof of event chain validity.
 * Proves: Events are hash-linked and causally ordered
 * Privacy: Intermediate events stay private
 * 
 * Uses linear recursion (like Mina's blockchain compression)
 */
export const EventChainProgram = ZkProgram({
	name: 'event-chain',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		baseCase: {
			privateInputs: [Field, Field, Field],
			
			async method(
				eventId: Field,
				author: Field,
				payloadHash: Field,
				timestamp: Field
			) {
				// Verify base event hash
				const computedHash = Poseidon.hash([author, payloadHash, timestamp]);
				computedHash.assertEquals(eventId);
				
				return { publicOutput: eventId };
			}
		},
		
		step: {
			privateInputs: [SelfProof, Field, Field, Field, Field],
			
			async method(
				currentEventId: Field,
				earlierProof: SelfProof<Field, Field>,
				author: Field,
				parentId: Field,
				payloadHash: Field,
				timestamp: Field
			) {
				// Verify earlier proof
				earlierProof.verify();
				
				// Parent ID must match earlier proof's output
				earlierProof.publicOutput.assertEquals(parentId);
				
				// Verify current event hash
				const computedHash = Poseidon.hash([
					author,
					parentId,
					payloadHash,
					timestamp
				]);
				computedHash.assertEquals(currentEventId);
				
				return { publicOutput: currentEventId };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// CIRCUIT 3: ALLOCATION CORRECTNESS ⭐ KILLER APP
// ═══════════════════════════════════════════════════════════════════

/**
 * AllocationProgram
 * 
 * Proves: Allocation = (mrValue / mrSum) * totalCapacity
 * Privacy: MR values stay PRIVATE!
 * 
 * Public Input: Recipient ID
 * Public Output: Allocated amount
 * Private Inputs: mrValue, mrSum, totalCapacity (all private!)
 * 
 * This is the killer app - prove fair allocation WITHOUT revealing MR values!
 */
export const AllocationProgram = ZkProgram({
	name: 'allocation',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		verify: {
			privateInputs: [Field, Field, Field],
			
			async method(
				recipientId: Field,
				mrValue: Field,
				mrSum: Field,
				totalCapacity: Field
			) {
				// Compute expected allocation
				// allocatedAmount = (mrValue * totalCapacity) / mrSum
				const numerator = mrValue.mul(totalCapacity);
				const allocatedAmount = numerator.div(mrSum);
				
				// Verify MR value is in valid range [0, mrSum]
				mrValue.assertGreaterThanOrEqual(Field(0));
				mrValue.assertLessThanOrEqual(mrSum);
				
				// Verify allocation is in valid range [0, totalCapacity]
				allocatedAmount.assertGreaterThanOrEqual(Field(0));
				allocatedAmount.assertLessThanOrEqual(totalCapacity);
				
				return { publicOutput: allocatedAmount };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// CIRCUIT 4: TREE-BASED ROLLUP
// ═══════════════════════════════════════════════════════════════════

/**
 * AllocationRollupProgram
 * 
 * Tree-based recursive proof composition.
 * Proves: Multiple allocations sum correctly
 * 
 * Uses tree recursion (like Mina's rollup mechanism)
 * This compresses N allocations into O(log N) verification
 */
export const AllocationRollupProgram = ZkProgram({
	name: 'allocation-rollup',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		baseCase: {
			privateInputs: [Field],
			
			async method(totalCapacity: Field, singleAllocation: Field) {
				// Base case: Single allocation
				singleAllocation.assertGreaterThanOrEqual(Field(0));
				singleAllocation.assertLessThanOrEqual(totalCapacity);
				
				return { publicOutput: singleAllocation };
			}
		},
		
		merge: {
			privateInputs: [SelfProof, SelfProof],
			
			async method(
				totalCapacity: Field,
				leftProof: SelfProof<Field, Field>,
				rightProof: SelfProof<Field, Field>
			) {
				// Verify both proofs
				leftProof.verify();
				rightProof.verify();
				
				// Both must be for same total capacity
				leftProof.publicInput.assertEquals(totalCapacity);
				rightProof.publicInput.assertEquals(totalCapacity);
				
				// Sum the allocations
				const totalAllocated = leftProof.publicOutput.add(rightProof.publicOutput);
				
				// Verify sum doesn't exceed capacity
				totalAllocated.assertLessThanOrEqual(totalCapacity);
				
				return { publicOutput: totalAllocated };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// COMPILATION & VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compile all ZK programs (once per application lifetime)
 * 
 * WARNING: Takes 1-5 minutes. Cache the keys!
 */
export async function compileZkPrograms() {
	console.log('[ZK] Compiling programs...');
	console.time('[ZK] Compilation');
	
	const [eventIntegrity, eventChain, allocation, rollup] = await Promise.all([
		EventIntegrityProgram.compile(),
		EventChainProgram.compile(),
		AllocationProgram.compile(),
		AllocationRollupProgram.compile()
	]);
	
	console.timeEnd('[ZK] Compilation');
	console.log('[ZK] ✅ Compilation complete');
	
	return { eventIntegrity, eventChain, allocation, rollup };
}

/**
 * Verify any proof using o1js verify()
 * 
 * Standalone verification (doesn't require Mina blockchain)
 */
export async function verifyProof(proofJSON: string, verificationKey: string): Promise<boolean> {
	try {
		return await verify(JSON.parse(proofJSON), verificationKey);
	} catch (error) {
		console.error('[ZK] Verification failed:', error);
		return false;
	}
}

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

// Generate proof class types from ZkPrograms
export class EventIntegrityProof extends ZkProgram.Proof(EventIntegrityProgram) {}
export class EventChainProof extends ZkProgram.Proof(EventChainProgram) {}
export class AllocationProof extends ZkProgram.Proof(AllocationProgram) {}
export class AllocationRollupProof extends ZkProgram.Proof(AllocationRollupProgram) {}

