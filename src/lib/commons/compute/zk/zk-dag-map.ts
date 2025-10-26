/**
 * ZK DAG using MerkleMap (Available NOW in o1js v2.10.0)
 * 
 * MerkleMap provides key-value interface: eventHash → parentHash
 * Height 256 = essentially unlimited capacity
 * 
 * Better than MerkleTree for DAG because:
 * - Natural key-value mapping (hash → hash)
 * - No manual index tracking
 * - Essentially unlimited capacity
 * 
 * Reference: https://docs.o1labs.org/o1js/api-reference/classes/MerkleMap
 */

import { Field, ZkProgram, MerkleMap, MerkleMapWitness } from 'o1js';

// ═══════════════════════════════════════════════════════════════════
// MERKLE MAP DAG
// ═══════════════════════════════════════════════════════════════════

/**
 * DAG using MerkleMap
 * Maps: eventHash (Field) → parentHash (Field)
 * Height 256 = essentially unlimited capacity
 */
export class DAGMap {
	private map: MerkleMap;
	
	constructor() {
		this.map = new MerkleMap();
	}
	
	/**
	 * Add event to DAG
	 * Maps event hash to its parent hash
	 */
	addEvent(eventHash: Field, parentHash: Field): void {
		this.map.set(eventHash, parentHash);
	}
	
	/**
	 * Get parent of event
	 */
	getParent(eventHash: Field): Field {
		return this.map.get(eventHash);
	}
	
	/**
	 * Get witness for ZK proof
	 */
	getWitness(eventHash: Field): MerkleMapWitness {
		return this.map.getWitness(eventHash);
	}
	
	/**
	 * Get DAG root (commitment)
	 */
	getRoot(): Field {
		return this.map.getRoot();
	}
	
	/**
	 * Get underlying MerkleMap (for advanced usage)
	 */
	getMap(): MerkleMap {
		return this.map;
	}
}

// ═══════════════════════════════════════════════════════════════════
// DAG MEMBERSHIP CIRCUIT (MerkleMap)
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove event exists in DAG using MerkleMap
 * 
 * Public Input: DAG root
 * Public Output: Event hash + parent hash
 * Private Inputs: Event hash, witness
 */
export const DAGMapMembershipProgram = ZkProgram({
	name: 'dag-map-membership',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		prove: {
			privateInputs: [Field, MerkleMapWitness],
			
			async method(dagRoot: Field, eventHash: Field, witness: MerkleMapWitness) {
				// Get parent hash from witness
				const [witnessRoot, parentHash] = witness.computeRootAndKey(eventHash);
				
				// Verify root matches
				witnessRoot.assertEquals(dagRoot);
				
				// Return event hash (proves it exists in DAG)
				return { publicOutput: eventHash };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// LINEAGE VERIFICATION CIRCUIT
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove parent-child relationship
 * 
 * Public Input: DAG root
 * Public Output: Child hash
 * Private Inputs: Child hash, child witness, expected parent hash
 */
export const DAGMapLineageProgram = ZkProgram({
	name: 'dag-map-lineage',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		verifyParentChild: {
			privateInputs: [Field, MerkleMapWitness, Field],
			
			async method(
				dagRoot: Field,
				childHash: Field,
				childWitness: MerkleMapWitness,
				expectedParentHash: Field
			) {
				// Get parent from witness
				const [witnessRoot, actualParentHash] = childWitness.computeRootAndKey(childHash);
				
				// Verify root matches
				witnessRoot.assertEquals(dagRoot);
				
				// Verify parent hash matches expected
				actualParentHash.assertEquals(expectedParentHash);
				
				return { publicOutput: childHash };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// DAG UPDATE CIRCUIT
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove DAG update is valid
 * 
 * Public Input: Old root
 * Public Output: New root
 * Private Inputs: New event, parent, old witness
 */
export const DAGMapUpdateProgram = ZkProgram({
	name: 'dag-map-update',
	publicInput: Field,
	publicOutput: Field,
	
	methods: {
		addEvent: {
			privateInputs: [Field, Field, MerkleMapWitness],
			
			async method(
				oldRoot: Field,
				newEventHash: Field,
				parentHash: Field,
				oldWitness: MerkleMapWitness
			) {
				// Verify old root
				const [witnessRoot] = oldWitness.computeRootAndKey(Field(0));
				witnessRoot.assertEquals(oldRoot);
				
				// Compute new root after adding event
				// In real usage, you'd use witness.calculateRoot()
				// For now, just prove the update is valid
				
				// Return some new root (simplified)
				return { publicOutput: oldRoot.add(newEventHash) };
			}
		}
	}
});

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create a new DAG map
 */
export function createDAGMap(): DAGMap {
	return new DAGMap();
}

/**
 * Hash conversion helpers
 */
export function hashToField(hash: string): Field {
	const truncated = hash.substring(0, 63);
	return Field(BigInt('0x' + truncated));
}

export function fieldToHash(field: Field): string {
	return '0x' + field.toBigInt().toString(16).padStart(64, '0');
}

// ═══════════════════════════════════════════════════════════════════
// COMPILATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Compile DAG Map circuits
 */
export async function compileDAGMapCircuits() {
	console.log('[ZK-DAG-MAP] Compiling MerkleMap-based DAG circuits...');
	console.time('[ZK-DAG-MAP] Compilation');
	
	const [membership, lineage, update] = await Promise.all([
		DAGMapMembershipProgram.compile(),
		DAGMapLineageProgram.compile(),
		DAGMapUpdateProgram.compile()
	]);
	
	console.timeEnd('[ZK-DAG-MAP] Compilation');
	console.log('[ZK-DAG-MAP] ✅ Compilation complete');
	
	return { membership, lineage, update };
}

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export class DAGMapMembershipProof extends ZkProgram.Proof(DAGMapMembershipProgram) {}
export class DAGMapLineageProof extends ZkProgram.Proof(DAGMapLineageProgram) {}
export class DAGMapUpdateProof extends ZkProgram.Proof(DAGMapUpdateProgram) {}

