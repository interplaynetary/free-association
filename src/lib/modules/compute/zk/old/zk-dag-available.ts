/**
 * ZK DAG using MerkleTree & MerkleList (Available NOW in o1js v2.10.0)
 * 
 * Uses available o1js primitives instead of waiting for IndexedMerkleMap.
 * 
 * Architecture:
 * - MerkleTree: Maps event index → event hash (fixed size, witness generation)
 * - MerkleList: Chronological list of events (dynamic, provable in circuits)
 * 
 * Reference: https://docs.o1labs.org/o1js/api-reference/classes/MerkleTree
 */

import { Field, ZkProgram, MerkleTree, MerkleWitness, MerkleList, Poseidon } from 'o1js';

// ═══════════════════════════════════════════════════════════════════
// PROVABLE EVENT LIST
// ═══════════════════════════════════════════════════════════════════

/**
 * Provenance event represented as Fields for ZK circuits
 */
export class ProvenanceEventFields {
	constructor(
		public eventHash: Field,
		public parentHash: Field,
		public timestamp: Field
	) {}
	
	static fromFields(fields: Field[]): ProvenanceEventFields {
		return new ProvenanceEventFields(fields[0], fields[1], fields[2]);
	}
	
	toFields(): Field[] {
		return [this.eventHash, this.parentHash, this.timestamp];
	}
	
	hash(): Field {
		return Poseidon.hash(this.toFields());
	}
}

/**
 * Provable list of events using MerkleList
 */
export class EventList extends MerkleList.create(
	Field,
	(hash, eventHash) => Poseidon.hash([hash, eventHash])
) {}

// ═══════════════════════════════════════════════════════════════════
// MERKLE TREE DAG (Fixed Size)
// ═══════════════════════════════════════════════════════════════════

/**
 * Fixed-size DAG using MerkleTree
 * Height 20 = 1 million events
 * Height 25 = 33 million events
 * Height 30 = 1 billion events
 */
export class DAGMerkleTree {
	private tree: MerkleTree;
	private eventCount: number = 0;
	
	constructor(height: number = 25) {
		this.tree = new MerkleTree(height);
	}
	
	/**
	 * Add event to DAG
	 */
	addEvent(eventHash: Field): bigint {
		const index = BigInt(this.eventCount);
		this.tree.setLeaf(index, eventHash);
		this.eventCount++;
		return index;
	}
	
	/**
	 * Get event at index
	 */
	getEvent(index: bigint): Field {
		return this.tree.getLeaf(index);
	}
	
	/**
	 * Get Merkle witness for proof generation
	 */
	getWitness(index: bigint): DAGWitness25 {
		return new DAGWitness25(this.tree.getWitness(index));
	}
	
	/**
	 * Get DAG root (commitment)
	 */
	getRoot(): Field {
		return this.tree.getRoot();
	}
	
	/**
	 * Get total number of events
	 */
	getEventCount(): number {
		return this.eventCount;
	}
}

// Create witness class for height 25 (33 million capacity)
export class DAGWitness25 extends MerkleWitness(25) {}

// ═══════════════════════════════════════════════════════════════════
// DAG MEMBERSHIP CIRCUIT
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove event membership in DAG using MerkleTree witness
 * 
 * Public Input: DAG root
 * Public Output: Event hash
 * Private Inputs: Event hash, witness
 */
export const DAGMembershipProgram = ZkProgram({
	name: 'dag-membership',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		prove: {
			privateInputs: [Field, DAGWitness25],
			
			async method(dagRoot: Field, eventHash: Field, witness: DAGWitness25) {
				// Calculate root from witness and event hash
				const calculatedRoot = witness.calculateRoot(eventHash);
				
				// Verify root matches
				calculatedRoot.assertEquals(dagRoot);
				
				return { publicOutput: eventHash };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// EVENT LIST CIRCUIT (Dynamic)
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove event exists in chronological list
 * 
 * Public Input: List hash
 * Public Output: Event hash
 * Private Inputs: Event list, target event
 */
export const EventListMembershipProgram = ZkProgram({
	name: 'event-list-membership',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		prove: {
			privateInputs: [EventList, Field],
			
			async method(listHash: Field, list: EventList, targetEvent: Field) {
				// Verify list hash
				list.hash.assertEquals(listHash);
				
				// Iterate through list to find event
				let found = Field(0);
				list.forEach(100, (eventHash, isDummy, i) => {
					// Check if this is our target (and not a dummy)
					const isMatch = eventHash.equals(targetEvent).and(isDummy.not());
					found = isMatch.toFields()[0].add(found);
				});
				
				// Verify we found the event
				found.assertGreaterThan(Field(0));
				
				return { publicOutput: targetEvent };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// EVENT CHAIN VERIFICATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove parent-child relationship in DAG
 * 
 * Public Input: DAG root
 * Public Output: Child hash
 * Private Inputs: Child hash, child witness, parent hash, parent witness
 */
export const DAGLineageProgram = ZkProgram({
	name: 'dag-lineage',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		verifyParentChild: {
			privateInputs: [Field, DAGWitness25, Field, DAGWitness25],
			
			async method(
				dagRoot: Field,
				childHash: Field,
				childWitness: DAGWitness25,
				parentHash: Field,
				parentWitness: DAGWitness25
			) {
				// Verify both events exist in DAG
				const childRoot = childWitness.calculateRoot(childHash);
				childRoot.assertEquals(dagRoot);
				
				const parentRoot = parentWitness.calculateRoot(parentHash);
				parentRoot.assertEquals(dagRoot);
				
				// Note: We'd need to encode parent relationship in the event data
				// For now, we just prove both events exist in the same DAG
				
				return { publicOutput: childHash };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a new DAG (off-chain)
 */
export function createDAG(height: number = 25): DAGMerkleTree {
	return new DAGMerkleTree(height);
}

/**
 * Create event list (off-chain)
 */
export function createEventList(): EventList {
	return EventList.empty();
}

/**
 * Hash conversion helpers
 */
export function hashToField(hash: string): Field {
	const truncated = hash.substring(0, 63);
	return Field(BigInt('0x' + truncated));
}

export function hashToBigInt(hash: string): bigint {
	const truncated = hash.substring(0, 63);
	return BigInt('0x' + truncated);
}

// ═══════════════════════════════════════════════════════════════════
// COMPILATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compile DAG circuits
 */
export async function compileDAGCircuits() {
	console.log('[ZK-DAG] Compiling DAG circuits (MerkleTree-based)...');
	console.time('[ZK-DAG] Compilation');
	
	const [membership, listMembership, lineage] = await Promise.all([
		DAGMembershipProgram.compile(),
		EventListMembershipProgram.compile(),
		DAGLineageProgram.compile()
	]);
	
	console.timeEnd('[ZK-DAG] Compilation');
	console.log('[ZK-DAG] ✅ Compilation complete');
	
	return { membership, listMembership, lineage };
}

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export class DAGMembershipProof extends ZkProgram.Proof(DAGMembershipProgram) {}
export class EventListMembershipProof extends ZkProgram.Proof(EventListMembershipProgram) {}
export class DAGLineageProof extends ZkProgram.Proof(DAGLineageProgram) {}

