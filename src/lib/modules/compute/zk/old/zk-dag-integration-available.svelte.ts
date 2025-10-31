/**
 * ZK DAG Integration (MerkleTree/MerkleList - Available NOW)
 * 
 * Integrates MerkleTree/MerkleList-based DAG with provenance system.
 */

import { Field } from 'o1js';
import type { ProvenanceEvent } from '../../provenance/provenance-event-schema';
import {
	DAGMerkleTree,
	EventList,
	createDAG,
	createEventList,
	hashToField,
	hashToBigInt,
	DAGMembershipProgram,
	EventListMembershipProgram,
	DAGLineageProgram,
	compileDAGCircuits,
	type DAGWitness25
} from './zk-dag-available';

// ═══════════════════════════════════════════════════════════════════
// SYSTEM STATE
// ═══════════════════════════════════════════════════════════════════

let dagCircuitsCompiled = false;
let compiledDAGKeys: Awaited<ReturnType<typeof compileDAGCircuits>> | null = null;

/**
 * Initialize DAG circuits
 */
export async function initializeDAGCircuits(): Promise<void> {
	if (dagCircuitsCompiled) return;
	
	console.log('[ZK-DAG] Initializing (MerkleTree-based)...');
	compiledDAGKeys = await compileDAGCircuits();
	dagCircuitsCompiled = true;
	console.log('[ZK-DAG] ✅ Ready');
}

export function areDAGCircuitsReady(): boolean {
	return dagCircuitsCompiled && compiledDAGKeys !== null;
}

// ═══════════════════════════════════════════════════════════════════
// DAG CONSTRUCTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Build MerkleTree DAG from events
 */
export function buildDAGFromEvents(events: ProvenanceEvent[]): DAGMerkleTree {
	const dag = createDAG();
	
	// Sort by timestamp
	const sorted = [...events].sort((a, b) => 
		new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
	);
	
	// Add each event
	for (const event of sorted) {
		const eventHash = hashToField(event.id);
		dag.addEvent(eventHash);
	}
	
	return dag;
}

/**
 * Build EventList from events (dynamic, provable)
 */
export function buildEventListFromEvents(events: ProvenanceEvent[]): EventList {
	const list = createEventList();
	
	// Sort by timestamp
	const sorted = [...events].sort((a, b) => 
		new Date(a.timestamp).getTime() - new Date(b.timestamp).getTime()
	);
	
	// Push each event
	for (const event of sorted) {
		const eventHash = hashToField(event.id);
		list.push(eventHash);
	}
	
	return list;
}

// ═══════════════════════════════════════════════════════════════════
// PROVABLE OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Prove event membership in DAG
 */
export async function proveMembershipInDAG(
	dag: DAGMerkleTree,
	eventId: string,
	eventIndex: bigint
) {
	if (!areDAGCircuitsReady()) throw new Error('DAG circuits not initialized');
	
	console.log(`[ZK-DAG] Proving membership for event ${eventId.substring(0, 16)}...`);
	
	const dagRoot = dag.getRoot();
	const eventHash = hashToField(eventId);
	const witness = dag.getWitness(eventIndex);
	
	// Generate proof
	const result = await DAGMembershipProgram.prove(dagRoot, eventHash, witness);
	
	console.log('[ZK-DAG] ✅ Membership proof generated');
	
	return {
		proof: result.proof.toJSON(),
		dagRoot: dagRoot.toString(),
		eventHash: eventHash.toString(),
		verificationKey: compiledDAGKeys!.membership.verificationKey
	};
}

/**
 * Prove event membership in dynamic list
 */
export async function proveMembershipInList(
	list: EventList,
	eventId: string
) {
	if (!areDAGCircuitsReady()) throw new Error('DAG circuits not initialized');
	
	console.log(`[ZK-DAG] Proving list membership for ${eventId.substring(0, 16)}...`);
	
	const listHash = list.hash;
	const eventHash = hashToField(eventId);
	
	// Generate proof
	const result = await EventListMembershipProgram.prove(listHash, list, eventHash);
	
	console.log('[ZK-DAG] ✅ List membership proof generated');
	
	return {
		proof: result.proof.toJSON(),
		listHash: listHash.toString(),
		eventHash: eventHash.toString(),
		verificationKey: compiledDAGKeys!.listMembership.verificationKey
	};
}

/**
 * Prove parent-child lineage
 */
export async function proveLineage(
	dag: DAGMerkleTree,
	childId: string,
	childIndex: bigint,
	parentId: string,
	parentIndex: bigint
) {
	if (!areDAGCircuitsReady()) throw new Error('DAG circuits not initialized');
	
	console.log(`[ZK-DAG] Proving lineage...`);
	
	const dagRoot = dag.getRoot();
	const childHash = hashToField(childId);
	const childWitness = dag.getWitness(childIndex);
	const parentHash = hashToField(parentId);
	const parentWitness = dag.getWitness(parentIndex);
	
	// Generate proof
	const result = await DAGLineageProgram.verifyParentChild(
		dagRoot,
		childHash,
		childWitness,
		parentHash,
		parentWitness
	);
	
	console.log('[ZK-DAG] ✅ Lineage proof generated');
	
	return {
		proof: result.proof.toJSON(),
		dagRoot: dagRoot.toString(),
		verificationKey: compiledDAGKeys!.lineage.verificationKey
	};
}

// ═══════════════════════════════════════════════════════════════════
// OFF-CHAIN QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get event at index (off-chain)
 */
export function queryEventAtIndex(dag: DAGMerkleTree, index: bigint): string {
	const eventHash = dag.getEvent(index);
	return '0x' + eventHash.toBigInt().toString(16).padStart(64, '0');
}

/**
 * Get DAG root
 */
export function queryDAGRoot(dag: DAGMerkleTree): string {
	return dag.getRoot().toString();
}

/**
 * Get DAG statistics
 */
export function getDAGStats(dag: DAGMerkleTree) {
	return {
		root: queryDAGRoot(dag),
		eventCount: dag.getEventCount(),
		maxCapacity: 2 ** 25, // 33 million events
		type: 'MerkleTree',
		description: 'Fixed-size MerkleTree DAG (available in o1js v2.10.0)'
	};
}

/**
 * Get EventList statistics
 */
export function getEventListStats(list: EventList) {
	return {
		hash: list.hash.toString(),
		type: 'MerkleList',
		description: 'Dynamic-length MerkleList (available in o1js v2.10.0)'
	};
}

// ═══════════════════════════════════════════════════════════════════
// UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function getDAGStatus() {
	return {
		circuitsCompiled: dagCircuitsCompiled,
		ready: areDAGCircuitsReady(),
		implementation: 'MerkleTree + MerkleList',
		available: 'NOW (o1js v2.10.0)',
		circuits: compiledDAGKeys ? ['membership', 'list-membership', 'lineage'] : []
	};
}

