/**
 * Program Hashing Utilities
 * 
 * Provides deterministic hashing for RDL programs to:
 * - Create unique identifiers for programs
 * - Namespace program data in Holster
 * - Enable program versioning
 * - Isolate program instances
 * 
 * Architecture:
 * All program data is stored at: ~pubkey/<program_hash>/<holster_path>
 * 
 * This ensures:
 * - Different programs don't collide
 * - Same program can run multiple instances
 * - Program updates create new namespaces (different hash)
 * - Data isolation and organization
 */

import type { ReactiveComputationGraph } from './schema';
import { createHash } from 'crypto';

// ═══════════════════════════════════════════════════════════════════
// PROGRAM HASHING
// ═══════════════════════════════════════════════════════════════════

/**
 * Deterministic JSON stringifier - sorts keys recursively
 */
function deterministicStringify(obj: any): string {
	if (obj === null) {
		return 'null';
	}
	
	if (obj === undefined) {
		return 'undefined';  // Explicitly return string "undefined"
	}
	
	if (typeof obj !== 'object') {
		return JSON.stringify(obj);
	}
	
	if (Array.isArray(obj)) {
		return '[' + obj.map(item => deterministicStringify(item)).join(',') + ']';
	}
	
	// Sort object keys and stringify recursively
	const sortedKeys = Object.keys(obj).sort();
	const pairs = sortedKeys.map(key => {
		return JSON.stringify(key) + ':' + deterministicStringify(obj[key]);
	});
	
	return '{' + pairs.join(',') + '}';
}

/**
 * Create a deterministic hash of an RDL program
 * 
 * The hash is computed from:
 * - Program structure (variables, computations)
 * - NOT from metadata (id, version, description)
 * - NOT from program_hash field itself
 * 
 * This means:
 * - Same logic = same hash
 * - Different metadata = same hash (unless logic changes)
 * - Can recreate hash from program structure
 */
export function hashProgram(program: ReactiveComputationGraph): string {
	// Create canonical representation (exclude metadata)
	const canonical = {
		variables: program.variables,
		computations: program.computations
	};
	
	// Deterministic JSON serialization with sorted keys
	const json = deterministicStringify(canonical);
	
	// Create SHA-256 hash (first 16 chars for brevity)
	const hash = createHash('sha256').update(json).digest('hex').substring(0, 16);
	
	return hash;
}

/**
 * Get or compute program hash
 * 
 * If program.program_hash is set, use it (allows manual override)
 * Otherwise, compute from program structure
 */
export function getProgramHash(program: ReactiveComputationGraph): string {
	// Use hasOwnProperty to distinguish undefined from empty string
	if (program.program_hash !== undefined) {
		return program.program_hash;
	}
	return hashProgram(program);
}

/**
 * Verify program hash matches its structure
 * 
 * Returns true if program_hash field matches computed hash
 * Returns true if program_hash is not set (nothing to verify)
 */
export function verifyProgramHash(program: ReactiveComputationGraph): boolean {
	if (!program.program_hash) {
		return true; // No hash to verify
	}
	
	const computed = hashProgram(program);
	return program.program_hash === computed;
}

// ═══════════════════════════════════════════════════════════════════
// PATH PREFIXING
// ═══════════════════════════════════════════════════════════════════

/**
 * Prefix a holster path with program hash
 * 
 * Examples:
 * - prefixPath("abc123", "tree") → "abc123/tree"
 * - prefixPath("abc123", "nodes/node1") → "abc123/nodes/node1"
 */
export function prefixHolsterPath(programHash: string, holsterPath: string): string {
	// Remove leading/trailing slashes from holster path
	const cleanPath = holsterPath.replace(/^\/+|\/+$/g, '');
	
	// Combine with program hash
	return `${programHash}/${cleanPath}`;
}

/**
 * Extract program hash from prefixed path
 * 
 * Examples:
 * - extractProgramHash("abc123/tree") → "abc123"
 * - extractProgramHash("abc123/nodes/node1") → "abc123"
 */
export function extractProgramHash(prefixedPath: string): string | null {
	const parts = prefixedPath.split('/');
	return parts.length > 1 ? parts[0] : null;
}

/**
 * Remove program hash prefix from path
 * 
 * Examples:
 * - unprefixPath("abc123/tree") → "tree"
 * - unprefixPath("abc123/nodes/node1") → "nodes/node1"
 */
export function unprefixHolsterPath(prefixedPath: string): string {
	const parts = prefixedPath.split('/');
	return parts.length > 1 ? parts.slice(1).join('/') : prefixedPath;
}

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION PROVENANCE HASHING
// ═══════════════════════════════════════════════════════════════════

import type { 
	Computation, 
	ComputationProvenance
} from './schema';

/**
 * Create content hash of any data (for input/output verification)
 * 
 * Uses deterministic JSON serialization to ensure same data → same hash
 * Returns first 16 chars of SHA-256 hash for brevity
 */
export function hashContent(data: any): string {
	const json = deterministicStringify(data);
	return createHash('sha256').update(json).digest('hex').substring(0, 16);
}

/**
 * Create hash of computation definition
 * 
 * Hash includes:
 * - Computation ID
 * - Compute function name
 * - Input names (sorted)
 * - Output names (sorted)
 * 
 * This allows tracking computation logic changes
 */
export function hashComputation(computation: Computation): string {
	const canonical = {
		id: computation.id,
		compute_fn: computation.compute_fn,
		inputs: Object.keys(computation.inputs).sort(),
		outputs: Object.keys(computation.outputs).sort()
	};
	
	const json = deterministicStringify(canonical);
	return createHash('sha256').update(json).digest('hex').substring(0, 16);
}

/**
 * Create deterministic hash for computation verification
 * 
 * Combines:
 * - Computation hash (what logic)
 * - Input hashes (what data)
 * 
 * This allows others to verify: "If I had these inputs, would I get this output?"
 */
export function createDeterministicHash(
	computationHash: string,
	inputHashes: Record<string, string>
): string {
	const sortedInputs = Object.entries(inputHashes)
		.sort(([a], [b]) => a.localeCompare(b))
		.map(([name, hash]) => `${name}:${hash}`)
		.join(',');
	
	const canonical = {
		computation: computationHash,
		inputs: sortedInputs
	};
	
	const json = deterministicStringify(canonical);
	return createHash('sha256').update(json).digest('hex').substring(0, 16);
}

/**
 * Create compact signature from ITC stamp (V2)
 * 
 * Serializes ITC stamp into compact string format
 * Format: itc:<hash of stamp>
 */
export function itcStampSignature(stamp: any): string {
	if (!stamp) return '';
	
	// Hash the ITC stamp for compactness
	const stampJson = deterministicStringify(stamp);
	const stampHash = createHash('sha256').update(stampJson).digest('hex').substring(0, 16);
	
	return `itc:${stampHash}`;
}

/**
 * Create compact provenance signature (V2 - ITC-based)
 * 
 * Format: p_<itc>_comp:<compId>_det:<hash>
 * 
 * Example: p_itc:7a3f2b1c_comp:count_det:9d4e3a2b
 */
export function createProvenanceSignature(prov: ComputationProvenance): string {
	const itcSig = itcStampSignature(prov.itcStamp);
	const compId = prov.computationId.substring(0, 8);
	const detHash = prov.deterministicHash.substring(0, 8);
	
	return `p_${itcSig}_comp:${compId}_det:${detHash}`;
}

/**
 * Parse provenance signature (V2)
 * 
 * Returns parsed components or null if invalid
 */
export function parseProvenanceSignature(sig: string): {
	itcStamp: string;  // Just the hash signature
	computationId: string;
	deterministicHash: string;
} | null {
	if (!sig.startsWith('p_')) return null;
	
	// Remove 'p_' prefix and split by component markers
	const content = sig.substring(2);
	
	// Find component boundaries
	const compMatch = content.match(/_comp:([^_]+)/);
	const detMatch = content.match(/_det:([^_]+)$/);
	
	if (!compMatch || !detMatch) return null;
	
	const computationId = compMatch[1];
	const deterministicHash = detMatch[1];
	
	// Extract ITC stamp signature (everything before _comp:)
	const itcStamp = content.split('_comp:')[0];
	
	if (!itcStamp) return null;
	
	return { itcStamp, computationId, deterministicHash };
}

/**
 * Build versioned data path with provenance
 * 
 * Full path: ~<pubkey>/<program_hash>/<holster_path>/<provenance_signature>
 */
export function buildProvenancePath(
	pubkey: string | null,
	programHash: string,
	holsterPath: string,
	provenanceSignature: string
): string | [string, string] {
	const base = buildProgramDataPath(pubkey, programHash, holsterPath);
	
	if (Array.isArray(base)) {
		// Cross-user: [pubkey, path]
		return [base[0], `${base[1]}/${provenanceSignature}`];
	} else {
		// Current user: path
		return `${base}/${provenanceSignature}`;
	}
}

// ═══════════════════════════════════════════════════════════════════
// PROGRAM INDEXING
// ═══════════════════════════════════════════════════════════════════

/**
 * In-memory program registry
 * Maps program hash → program
 */
const programRegistry = new Map<string, ReactiveComputationGraph>();

/**
 * Register a program by its hash
 */
export function registerProgram(program: ReactiveComputationGraph): string {
	const hash = getProgramHash(program);
	programRegistry.set(hash, program);
	console.log(`[PROGRAM-REGISTRY] Registered program: ${program.id} → ${hash}`);
	return hash;
}

/**
 * Get a program by its hash
 */
export function getProgramByHash(hash: string): ReactiveComputationGraph | null {
	return programRegistry.get(hash) || null;
}

/**
 * List all registered program hashes
 */
export function listRegisteredPrograms(): string[] {
	return Array.from(programRegistry.keys());
}

/**
 * Unregister a program
 */
export function unregisterProgram(hash: string): boolean {
	return programRegistry.delete(hash);
}

/**
 * Clear all registered programs
 */
export function clearProgramRegistry(): void {
	programRegistry.clear();
}

// ═══════════════════════════════════════════════════════════════════
// HOLSTER PATH HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Build full Holster path for program data
 * 
 * Format: ~pubkey/<program_hash>/<data_path>
 * 
 * Examples:
 * - buildProgramDataPath("user123", "abc456", "tree") → ["user123", "abc456/tree"]
 * - buildProgramDataPath(null, "abc456", "tree") → ["abc456/tree"] (current user)
 */
export function buildProgramDataPath(
	pubkey: string | null,
	programHash: string,
	dataPath: string
): string | [string, string] {
	const prefixedPath = prefixHolsterPath(programHash, dataPath);
	
	if (pubkey) {
		// Cross-user access: ~pubkey/program_hash/data
		return [pubkey, prefixedPath];
	} else {
		// Current user: program_hash/data
		return prefixedPath;
	}
}

// ═══════════════════════════════════════════════════════════════════
// PROGRAM METADATA
// ═══════════════════════════════════════════════════════════════════

/**
 * Program metadata for registry
 */
export interface ProgramMetadata {
	hash: string;
	id: string;
	version?: string;
	description?: string;
	registered_at: number;
	computation_count: number;
	variable_count: number;
}

/**
 * Get metadata for a registered program
 */
export function getProgramMetadata(hash: string): ProgramMetadata | null {
	const program = programRegistry.get(hash);
	if (!program) return null;
	
	return {
		hash,
		id: program.id,
		version: program.version,
		description: program.description,
		registered_at: Date.now(), // TODO: Store actual registration time
		computation_count: program.computations.length,
		variable_count: Object.keys(program.variables).length
	};
}

/**
 * Get metadata for all registered programs
 */
export function getAllProgramMetadata(): ProgramMetadata[] {
	const hashes = listRegisteredPrograms();
	return hashes.map(hash => getProgramMetadata(hash)).filter(Boolean) as ProgramMetadata[];
}

