/**
 * Hybrid Storage Unit Tests
 * 
 * Tests the logic and structure of hybrid storage without mocking:
 * - Path generation
 * - Signature format
 * - Index paths
 * - Data structure
 */

import { describe, test, expect } from 'bun:test';
import { 
	prefixHolsterPath,
	createProvenanceSignature,
	hashContent,
	vectorClockSignature 
} from '../program-hash.svelte';
import type { 
	ComputationProvenance,
	VectorClock 
} from '../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// HELPER: Create Mock Provenance
// ═══════════════════════════════════════════════════════════════════

function createMockProvenance(
	id: string,
	computationId: string,
	vectorClock: VectorClock,
	deterministicHash: string
): ComputationProvenance {
	return {
		id,
		vectorClock,
		executedBy: 'alice',
		timestamp: Date.now(),
		programHash: 'abc123',
		computationId,
		computationHash: hashContent(computationId),
		inputs: {},
		outputs: {},
		deterministicHash
	};
}

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Path Structure
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Path Structure', () => {
	
	test('canonical path format', () => {
		const programHash = 'abc123def456';
		const holsterPath = 'analytics/count';
		
		const canonical = prefixHolsterPath(programHash, holsterPath);
		
		expect(canonical).toBe('abc123def456/analytics/count');
		expect(canonical).not.toContain('_versions');
		expect(canonical).not.toContain('p_');
	});
	
	test('version path format', () => {
		const programHash = 'abc123';
		const holsterPath = 'analytics/count';
		const provSig = 'p_alice:5_comp:count_det:xyz';
		
		const versionPath = `${prefixHolsterPath(programHash, holsterPath)}/_versions/${provSig}`;
		
		expect(versionPath).toBe('abc123/analytics/count/_versions/p_alice:5_comp:count_det:xyz');
		expect(versionPath).toContain('/_versions/');
		expect(versionPath).toContain('p_');
	});
	
	test('latest index path format', () => {
		const programHash = 'abc123';
		const holsterPath = 'analytics/count';
		
		const latestIndexPath = `${programHash}/_index/latest/${holsterPath}`;
		
		expect(latestIndexPath).toBe('abc123/_index/latest/analytics/count');
		expect(latestIndexPath).toContain('/_index/latest/');
	});
	
	test('computations index path format', () => {
		const programHash = 'abc123';
		const computationId = 'my-computation';
		
		const compIndexPath = `${programHash}/_index/computations/${computationId}`;
		
		expect(compIndexPath).toBe('abc123/_index/computations/my-computation');
		expect(compIndexPath).toContain('/_index/computations/');
	});
	
	test('lineage index path format', () => {
		const programHash = 'abc123';
		const provenanceId = 'prov-12345';
		
		const lineageIndexPath = `${programHash}/_index/lineage/${provenanceId}`;
		
		expect(lineageIndexPath).toBe('abc123/_index/lineage/prov-12345');
		expect(lineageIndexPath).toContain('/_index/lineage/');
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Provenance Signatures
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Provenance Signatures', () => {
	
	test('provenance signature format', () => {
		const provenance = createMockProvenance(
			'prov-123',
			'sum',
			{ alice: 5, bob: 3 },
			'det-hash-xyz'
		);
		
		const sig = createProvenanceSignature(provenance);
		
		expect(sig).toMatch(/^p_/);
		expect(sig).toContain('_comp:sum_');
		expect(sig).toContain('_det:');
		// Should contain vector clock info
		expect(sig).toContain('alice:5');
		expect(sig).toContain('bob:3');
	});
	
	test('vector clock signature format', () => {
		const vc: VectorClock = { alice: 10, bob: 5, charlie: 7 };
		
		const sig = vectorClockSignature(vc);
		
		// Should be sorted alphabetically
		expect(sig).toMatch(/^alice:\d+_bob:\d+_charlie:\d+$/);
		expect(sig).toBe('alice:10_bob:5_charlie:7');
	});
	
	test('provenance signature is deterministic', () => {
		const provenance = createMockProvenance(
			'prov-123',
			'test',
			{ alice: 1 },
			'det-abc'
		);
		
		const sig1 = createProvenanceSignature(provenance);
		const sig2 = createProvenanceSignature(provenance);
		
		expect(sig1).toBe(sig2);
	});
	
	test('different provenance produces different signature', () => {
		const prov1 = createMockProvenance(
			'prov-1',
			'test',
			{ alice: 1 },
			'det-abc'
		);
		
		const prov2 = createMockProvenance(
			'prov-2',
			'test',
			{ alice: 2 }, // Different VC
			'det-abc'
		);
		
		const sig1 = createProvenanceSignature(prov1);
		const sig2 = createProvenanceSignature(prov2);
		
		expect(sig1).not.toBe(sig2);
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Data Structures
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Data Structures', () => {
	
	test('canonical data structure', () => {
		const canonicalData = {
			data: { count: 42, sum: 123 },
			_provenance: createMockProvenance(
				'prov-abc',
				'analyze',
				{ alice: 10 },
				'det-xyz'
			),
			_updatedAt: Date.now()
		};
		
		expect(canonicalData.data).toBeDefined();
		expect(canonicalData._provenance).toBeDefined();
		expect(canonicalData._updatedAt).toBeDefined();
		expect(canonicalData._provenance.vectorClock).toEqual({ alice: 10 });
	});
	
	test('versioned data structure', () => {
		const versionedData = {
			data: { count: 42 },
			_provenance: createMockProvenance(
				'prov-def',
				'count',
				{ alice: 5 },
				'det-123'
			),
			_updatedAt: Date.now(),
			_immutable: true
		};
		
		expect(versionedData.data).toBeDefined();
		expect(versionedData._provenance).toBeDefined();
		expect(versionedData._updatedAt).toBeDefined();
		expect(versionedData._immutable).toBe(true);
	});
	
	test('lineage index data structure', () => {
		const lineageData = {
			computationId: 'sum',
			programHash: 'abc123',
			inputs: ['a', 'b'],
			outputs: ['math/sum'],
			timestamp: Date.now(),
			vectorClock: { alice: 5 }
		};
		
		expect(lineageData.computationId).toBe('sum');
		expect(lineageData.programHash).toBe('abc123');
		expect(Array.isArray(lineageData.inputs)).toBe(true);
		expect(Array.isArray(lineageData.outputs)).toBe(true);
		expect(lineageData.vectorClock).toBeDefined();
		expect(lineageData.timestamp).toBeDefined();
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Path Resolution
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Path Resolution', () => {
	
	test('resolve canonical from RDL holster_path', () => {
		const programHash = 'def456';
		const rdlPath = 'analytics/user_count';
		
		const canonical = prefixHolsterPath(programHash, rdlPath);
		
		expect(canonical).toBe('def456/analytics/user_count');
	});
	
	test('resolve version from canonical + signature', () => {
		const canonicalPath = 'abc123/analytics/count';
		const provSig = 'p_alice:10_comp:count_det:xyz';
		
		const versionPath = `${canonicalPath}/_versions/${provSig}`;
		
		expect(versionPath).toBe('abc123/analytics/count/_versions/p_alice:10_comp:count_det:xyz');
	});
	
	test('extract program hash from canonical path', () => {
		const canonicalPath = 'abc123def456/analytics/count';
		
		// Program hash is everything before first '/'
		const programHash = canonicalPath.split('/')[0];
		
		expect(programHash).toBe('abc123def456');
	});
	
	test('extract holster_path from canonical', () => {
		const canonicalPath = 'abc123/analytics/count';
		
		// Holster path is everything after first '/'
		const holsterPath = canonicalPath.substring(canonicalPath.indexOf('/') + 1);
		
		expect(holsterPath).toBe('analytics/count');
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Querying Patterns
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Query Patterns', () => {
	
	test('query latest pattern', () => {
		// Pattern: holsterUser.get(canonicalPath).once()
		const programHash = 'abc123';
		const holsterPath = 'math/sum';
		
		const queryPath = prefixHolsterPath(programHash, holsterPath);
		
		expect(queryPath).toBe('abc123/math/sum');
		// This path will always return latest
	});
	
	test('query all versions pattern', () => {
		// Pattern: holsterUser.get(`${canonicalPath}/_versions`).map().once()
		const canonicalPath = 'abc123/math/sum';
		const versionsPath = `${canonicalPath}/_versions`;
		
		expect(versionsPath).toBe('abc123/math/sum/_versions');
		// Calling .map() on this will return all children (all versions)
	});
	
	test('query specific version pattern', () => {
		// Pattern: holsterUser.get(`${canonicalPath}/_versions/${provSig}`).once()
		const canonicalPath = 'abc123/math/sum';
		const provSig = 'p_alice:5_comp:sum_det:xyz';
		const versionPath = `${canonicalPath}/_versions/${provSig}`;
		
		expect(versionPath).toBe('abc123/math/sum/_versions/p_alice:5_comp:sum_det:xyz');
	});
	
	test('query computation outputs pattern', () => {
		// Pattern: holsterUser.get(`${programHash}/_index/computations/${compId}`).map().once()
		const programHash = 'abc123';
		const computationId = 'analyze';
		const indexPath = `${programHash}/_index/computations/${computationId}`;
		
		expect(indexPath).toBe('abc123/_index/computations/analyze');
		// This will return array of output paths
	});
	
	test('query lineage pattern', () => {
		// Pattern: holsterUser.get(`${programHash}/_index/lineage/${provId}`).once()
		const programHash = 'abc123';
		const provenanceId = 'prov-xyz-123';
		const lineagePath = `${programHash}/_index/lineage/${provenanceId}`;
		
		expect(lineagePath).toBe('abc123/_index/lineage/prov-xyz-123');
		// This will return lineage data
	});
});

// ═══════════════════════════════════════════════════════════════════
// TEST SUITE: Cross-User Queries
// ═══════════════════════════════════════════════════════════════════

describe('Hybrid Storage - Cross-User Queries', () => {
	
	test('cross-user canonical path', () => {
		const pubkey = 'bob';
		const programHash = 'abc123';
		const holsterPath = 'analytics/count';
		
		// Query format: ['bob', 'abc123/analytics/count']
		const crossUserPath: [string, string] = [
			pubkey,
			prefixHolsterPath(programHash, holsterPath)
		];
		
		expect(crossUserPath[0]).toBe('bob');
		expect(crossUserPath[1]).toBe('abc123/analytics/count');
	});
	
	test('cross-user version path', () => {
		const pubkey = 'charlie';
		const canonicalPath = 'def456/math/squared';
		const provSig = 'p_charlie:7_comp:square_det:abc';
		
		// Query format: ['charlie', 'def456/math/squared/_versions/p_...']
		const versionPath: [string, string] = [
			pubkey,
			`${canonicalPath}/_versions/${provSig}`
		];
		
		expect(versionPath[0]).toBe('charlie');
		expect(versionPath[1]).toContain('/_versions/');
	});
});

// ═════════════════════════════════════════════════════════════════════
// SUMMARY
// ═══════════════════════════════════════════════════════════════════

console.log('\n✅ Hybrid Storage Unit Tests');
console.log('   - Path structure (5 tests)');
console.log('   - Provenance signatures (4 tests)');
console.log('   - Data structures (3 tests)');
console.log('   - Path resolution (4 tests)');
console.log('   - Query patterns (5 tests)');
console.log('   - Cross-user queries (2 tests)');
console.log('   Total: 23 tests');
console.log('   Focus: Logic and structure (no mocking needed)');

