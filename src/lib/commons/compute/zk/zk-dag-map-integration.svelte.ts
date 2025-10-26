/**
 * ZK DAG Map Integration (MerkleMap - Available NOW)
 * 
 * Integrates MerkleMap-based DAG with provenance system.
 * Simpler than MerkleTree because it's naturally key-value.
 */

import { Field } from 'o1js';
import type { ProvenanceEvent } from '../provenance/provenance-event-schema';
import {
	DAGMap,
	createDAGMap,
	hashToField,
	fieldToHash,
	DAGMapMembershipProgram,
	DAGMapLineageProgram,
	compileDAGMapCircuits
} from './zk-dag-map';

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE
// ═══════════════════════════════════════════════════════════════════

let dagMapCircuitsCompiled = false;
let compiledMapKeys: Awaited<ReturnType<typeof compileDAGMapCircuits>> | null = null;

/**
 * Initialize DAG Map circuits
 */
export async function initializeDAGMapCircuits(): Promise<void> {
	if (dagMapCircuitsCompiled) return;
	
	console.log('[ZK-DAG-MAP] Initializing (MerkleMap-based)...');
	compiledMapKeys = await compileDAGMapCircuits();
	dagMapCircuitsCompiled = true;
	console.log('[ZK-DAG-MAP] ✅ Ready');
}

export function areDAGMapCircuitsReady(): boolean {
	return dagMapCircuitsCompiled && compiledMapKeys !== null;
}

// ═══════════════════════════════════════════════════════════════════
// DAG CONSTRUCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Build MerkleMap DAG from events
 * Maps: eventHash → parentHash
 */
export function buildDAGMapFromEvents(events: ProvenanceEvent[]): DAGMap {
	const dagMap = createDAGMap();
	
	// Sort by timestamp
	const sorted = [...events].sort((a, b) => 
		new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
	);
	
	// Add each event
	for (const event of sorted) {
		const eventHash = hashToField(event.id);
		const parentHash = event.parents[0] 
			? hashToField(event.parents[0])
			: Field(0); // Root event has parent = 0
		
		dagMap.addEvent(eventHash, parentHash);
	}
	
	return dagMap;
}

// ═══════════════════════════════════════════════════════════════════
// PROVABLE OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove event membership in DAG
 */
export async function proveMembershipInDAGMap(
	dagMap: DAGMap,
	eventId: string
) {
	if (!areDAGMapCircuitsReady()) throw new Error('DAG Map circuits not initialized');
	
	console.log(`[ZK-DAG-MAP] Proving membership for ${eventId.substring(0, 16)}...`);
	
	const dagRoot = dagMap.getRoot();
	const eventHash = hashToField(eventId);
	const witness = dagMap.getWitness(eventHash);
	
	// Generate proof
	const result = await DAGMapMembershipProgram.prove(dagRoot, eventHash, witness);
	
	console.log('[ZK-DAG-MAP] ✅ Membership proof generated');
	
	return {
		proof: result.proof.toJSON(),
		dagRoot: dagRoot.toString(),
		eventHash: eventHash.toString(),
		verificationKey: compiledMapKeys!.membership.verificationKey
	};
}

/**
 * Prove parent-child lineage
 */
export async function proveLineageInDAGMap(
	dagMap: DAGMap,
	childId: string,
	parentId: string
) {
	if (!areDAGMapCircuitsReady()) throw new Error('DAG Map circuits not initialized');
	
	console.log(`[ZK-DAG-MAP] Proving lineage...`);
	
	const dagRoot = dagMap.getRoot();
	const childHash = hashToField(childId);
	const expectedParentHash = hashToField(parentId);
	const childWitness = dagMap.getWitness(childHash);
	
	// Generate proof
	const result = await DAGMapLineageProgram.verifyParentChild(
		dagRoot,
		childHash,
		childWitness,
		expectedParentHash
	);
	
	console.log('[ZK-DAG-MAP] ✅ Lineage proof generated');
	
	return {
		proof: result.proof.toJSON(),
		dagRoot: dagRoot.toString(),
		childHash: childHash.toString(),
		parentHash: expectedParentHash.toString(),
		verificationKey: compiledMapKeys!.lineage.verificationKey
	};
}

// ═══════════════════════════════════════════════════════════════════
// OFF-CHAIN QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get parent of event (off-chain)
 */
export function queryParentInDAGMap(dagMap: DAGMap, eventId: string): string | null {
	const eventHash = hashToField(eventId);
	const parentField = dagMap.getParent(eventHash);
	
	// If parent is Field(0), this is a root event
	if (parentField.equals(Field(0)).toBoolean()) {
		return null;
	}
	
	return fieldToHash(parentField);
}

/**
 * Get DAG root
 */
export function queryDAGMapRoot(dagMap: DAGMap): string {
	return dagMap.getRoot().toString();
}

/**
 * Get DAG Map statistics
 */
export function getDAGMapStats(dagMap: DAGMap) {
	return {
		root: queryDAGMapRoot(dagMap),
		maxCapacity: 2 ** 254, // ~1.8e76 events (essentially unlimited)
		type: 'MerkleMap',
		description: 'Key-value MerkleMap DAG (height 256, available in o1js v2.10.0)'
	};
}

// ═══════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function getDAGMapStatus() {
	return {
		circuitsCompiled: dagMapCircuitsCompiled,
		ready: areDAGMapCircuitsReady(),
		implementation: 'MerkleMap (key-value)',
		available: 'NOW (o1js v2.10.0)',
		capacity: 'Essentially unlimited (height 256)',
		circuits: compiledMapKeys ? ['membership', 'lineage', 'update'] : []
	};
}

