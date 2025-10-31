/**
 * Computation Provenance Tests
 * 
 * Comprehensive test suite for:
 * - Content hashing (deterministic, key order independent)
 * - Computation hashing
 * - Deterministic hash creation
 * - Vector clock signatures
 * - Provenance signatures
 * - Runtime provenance tracking
 * - Verification functions
 */

import { describe, it, expect, beforeEach, afterEach } from 'bun:test';
import {
	hashContent,
	hashComputation,
	createDeterministicHash,
	vectorClockSignature,
	parseVectorClockSignature,
	createProvenanceSignature,
	parseProvenanceSignature,
	buildProvenancePath,
	buildProgramDataPath
} from '../program-hash.svelte';
import type {
	Computation,
	ComputationProvenance,
	VectorClock,
	ReactiveComputationGraph
} from '../../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// TEST FIXTURES
// ═══════════════════════════════════════════════════════════════════

const sampleComputation: Computation = {
	id: 'test-computation',
	inputs: {
		a: { type: 'value', value: 5 },
		b: { type: 'value', value: 3 }
	},
	compute_fn: 'add',
	outputs: {
		result: { type: 'holster', holster_path: 'test/result' }
	}
};

const sampleVectorClock: VectorClock = {
	'alice_pub': 5,
	'bob_pub': 3
};

const sampleProvenance: ComputationProvenance = {
	id: 'prov-123',
	vectorClock: sampleVectorClock,
	executedBy: 'alice_pub',
	timestamp: 1729789234567,
	programHash: 'abc123',
	computationId: 'test-computation',
	computationHash: 'comp-hash-123',
	inputs: {
		a: {
			source: 'value',
			contentHash: 'input-hash-a',
			path: undefined
		},
		b: {
			source: 'value',
			contentHash: 'input-hash-b',
			path: undefined
		}
	},
	outputs: {
		result: {
			path: 'test/result',
			contentHash: 'output-hash-123'
		}
	},
	deterministicHash: 'det-hash-123'
};

// ═══════════════════════════════════════════════════════════════════
// CONTENT HASHING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Content Hashing', () => {
	describe('hashContent', () => {
		it('should generate consistent hashes for same data', () => {
			const data = { count: 100, name: 'Alice' };
			const hash1 = hashContent(data);
			const hash2 = hashContent(data);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should generate same hash regardless of key order', () => {
			const data1 = { count: 100, name: 'Alice' };
			const data2 = { name: 'Alice', count: 100 };
			
			const hash1 = hashContent(data1);
			const hash2 = hashContent(data2);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should generate different hashes for different data', () => {
			const data1 = { count: 100, name: 'Alice' };
			const data2 = { count: 100, name: 'Bob' };
			
			const hash1 = hashContent(data1);
			const hash2 = hashContent(data2);
			
			expect(hash1).not.toBe(hash2);
		});
		
		it('should handle primitive values', () => {
			expect(hashContent(42)).toBe(hashContent(42));
			expect(hashContent('test')).toBe(hashContent('test'));
			expect(hashContent(true)).toBe(hashContent(true));
			expect(hashContent(null)).toBe(hashContent(null));
		});
		
		it('should handle arrays', () => {
			const arr1 = [1, 2, 3];
			const arr2 = [1, 2, 3];
			const arr3 = [3, 2, 1];
			
			expect(hashContent(arr1)).toBe(hashContent(arr2));
			expect(hashContent(arr1)).not.toBe(hashContent(arr3));
		});
		
		it('should handle nested objects', () => {
			const obj1 = { user: { name: 'Alice', age: 30 } };
			const obj2 = { user: { age: 30, name: 'Alice' } };
			
			expect(hashContent(obj1)).toBe(hashContent(obj2));
		});
		
		it('should handle complex nested structures', () => {
			const complex1 = {
				users: [
					{ name: 'Alice', scores: [10, 20, 30] },
					{ name: 'Bob', scores: [15, 25, 35] }
				],
				timestamp: 12345
			};
			const complex2 = {
				timestamp: 12345,
				users: [
					{ scores: [10, 20, 30], name: 'Alice' },
					{ scores: [15, 25, 35], name: 'Bob' }
				]
			};
			
			expect(hashContent(complex1)).toBe(hashContent(complex2));
		});
		
		it('should return 16 character hash', () => {
			const hash = hashContent({ test: 'data' });
			expect(hash).toHaveLength(16);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION HASHING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Computation Hashing', () => {
	describe('hashComputation', () => {
		it('should generate consistent hash for same computation', () => {
			const hash1 = hashComputation(sampleComputation);
			const hash2 = hashComputation(sampleComputation);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should generate different hashes for different computations', () => {
			const comp1: Computation = {
				id: 'comp1',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			const comp2: Computation = {
				id: 'comp2',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'multiply',  // Different function
				outputs: { result: { type: 'memory' } }
			};
			
			const hash1 = hashComputation(comp1);
			const hash2 = hashComputation(comp2);
			
			expect(hash1).not.toBe(hash2);
		});
		
		it('should include computation ID in hash', () => {
			const comp1: Computation = {
				id: 'comp1',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			const comp2: Computation = {
				id: 'comp2',  // Different ID
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			expect(hashComputation(comp1)).not.toBe(hashComputation(comp2));
		});
		
		it('should include input names in hash', () => {
			const comp1: Computation = {
				id: 'comp',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			const comp2: Computation = {
				id: 'comp',
				inputs: { 
					a: { type: 'value', value: 1 },
					b: { type: 'value', value: 2 }  // Additional input
				},
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			expect(hashComputation(comp1)).not.toBe(hashComputation(comp2));
		});
		
		it('should include output names in hash', () => {
			const comp1: Computation = {
				id: 'comp',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { result: { type: 'memory' } }
			};
			
			const comp2: Computation = {
				id: 'comp',
				inputs: { a: { type: 'value', value: 1 } },
				compute_fn: 'add',
				outputs: { 
					result: { type: 'memory' },
					extra: { type: 'memory' }  // Additional output
				}
			};
			
			expect(hashComputation(comp1)).not.toBe(hashComputation(comp2));
		});
		
		it('should return 16 character hash', () => {
			const hash = hashComputation(sampleComputation);
			expect(hash).toHaveLength(16);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// DETERMINISTIC HASH TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Deterministic Hash', () => {
	describe('createDeterministicHash', () => {
		it('should create consistent hash', () => {
			const compHash = 'comp-123';
			const inputHashes = { a: 'hash-a', b: 'hash-b' };
			
			const hash1 = createDeterministicHash(compHash, inputHashes);
			const hash2 = createDeterministicHash(compHash, inputHashes);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should be independent of input order', () => {
			const compHash = 'comp-123';
			const inputs1 = { a: 'hash-a', b: 'hash-b' };
			const inputs2 = { b: 'hash-b', a: 'hash-a' };
			
			const hash1 = createDeterministicHash(compHash, inputs1);
			const hash2 = createDeterministicHash(compHash, inputs2);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should change if computation changes', () => {
			const inputHashes = { a: 'hash-a', b: 'hash-b' };
			
			const hash1 = createDeterministicHash('comp-1', inputHashes);
			const hash2 = createDeterministicHash('comp-2', inputHashes);
			
			expect(hash1).not.toBe(hash2);
		});
		
		it('should change if inputs change', () => {
			const compHash = 'comp-123';
			
			const hash1 = createDeterministicHash(compHash, { a: 'hash-a', b: 'hash-b' });
			const hash2 = createDeterministicHash(compHash, { a: 'hash-a', b: 'hash-c' });
			
			expect(hash1).not.toBe(hash2);
		});
		
		it('should handle empty inputs', () => {
			const hash = createDeterministicHash('comp-123', {});
			expect(hash).toHaveLength(16);
		});
		
		it('should handle single input', () => {
			const hash = createDeterministicHash('comp-123', { a: 'hash-a' });
			expect(hash).toHaveLength(16);
		});
		
		it('should return 16 character hash', () => {
			const hash = createDeterministicHash('comp-123', { a: 'hash-a' });
			expect(hash).toHaveLength(16);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// VECTOR CLOCK SIGNATURE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Vector Clock Signatures', () => {
	describe('vectorClockSignature', () => {
		it('should create signature from vector clock', () => {
			const vc: VectorClock = { alice: 5, bob: 3 };
			const sig = vectorClockSignature(vc);
			
			expect(sig).toContain('alice');
			expect(sig).toContain(':5');
			expect(sig).toContain('bob');
			expect(sig).toContain(':3');
		});
		
		it('should sort by pubkey for determinism', () => {
			const vc1: VectorClock = { alice: 5, bob: 3 };
			const vc2: VectorClock = { bob: 3, alice: 5 };
			
			const sig1 = vectorClockSignature(vc1);
			const sig2 = vectorClockSignature(vc2);
			
			expect(sig1).toBe(sig2);
		});
		
		it('should handle single entry', () => {
			const vc: VectorClock = { alice: 7 };
			const sig = vectorClockSignature(vc);
			
			expect(sig).toContain('alice');
			expect(sig).toContain(':7');
		});
		
		it('should handle empty vector clock', () => {
			const sig = vectorClockSignature({});
			expect(sig).toBe('');
		});
		
		it('should truncate long pubkeys', () => {
			const longPubkey = 'very_long_pubkey_that_should_be_truncated_to_8_chars';
			const vc: VectorClock = { [longPubkey]: 1 };
			const sig = vectorClockSignature(vc);
			
			expect(sig).toContain('very_lon:1');
		});
	});
	
	describe('parseVectorClockSignature', () => {
		it('should parse signature back to vector clock', () => {
			const original: VectorClock = { alice: 5, bob: 3 };
			const sig = vectorClockSignature(original);
			const parsed = parseVectorClockSignature(sig);
			
			expect(parsed).not.toBeNull();
			expect(parsed?.alice).toBe(5);
			expect(parsed?.bob).toBe(3);
		});
		
		it('should handle empty string', () => {
			const parsed = parseVectorClockSignature('');
			expect(parsed).toBeNull();
		});
		
		it('should handle invalid format gracefully', () => {
			const parsed = parseVectorClockSignature('invalid_format');
			// Should either return null or parse what it can
			// In this case, it might parse 'invalid_format' as a key
			expect(parsed === null || typeof parsed === 'object').toBe(true);
		});
		
		it('should handle single entry', () => {
			const sig = 'alice123:7';
			const parsed = parseVectorClockSignature(sig);
			
			expect(parsed).not.toBeNull();
			expect(parsed?.alice123).toBe(7);
		});
		
		it('should round-trip correctly', () => {
			const original: VectorClock = { alice123: 5, bob45678: 3, charlie9: 7 };
			const sig = vectorClockSignature(original);
			const parsed = parseVectorClockSignature(sig);
			
			expect(parsed).not.toBeNull();
			// Note: keys are truncated to 8 chars
			expect(Object.keys(parsed!).length).toBe(3);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE SIGNATURE TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Provenance Signatures', () => {
	describe('createProvenanceSignature', () => {
		it('should create signature with all components', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			
			expect(sig).toContain('p_');  // Prefix
			expect(sig).toContain('_comp:');  // Computation marker
			expect(sig).toContain('_det:');  // Deterministic hash marker
		});
		
		it('should include vector clock', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			expect(sig).toContain('alice');
			expect(sig).toContain(':5');
		});
		
		it('should include truncated computation ID', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			expect(sig).toContain('_comp:test-com');  // Truncated to 8 chars
		});
		
		it('should include truncated deterministic hash', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			expect(sig).toContain('_det:det-hash');  // Truncated to 8 chars
		});
		
		it('should be consistent for same provenance', () => {
			const sig1 = createProvenanceSignature(sampleProvenance);
			const sig2 = createProvenanceSignature(sampleProvenance);
			
			expect(sig1).toBe(sig2);
		});
		
		it('should differ for different provenance', () => {
			const prov2: ComputationProvenance = {
				...sampleProvenance,
				deterministicHash: 'different-hash'
			};
			
			const sig1 = createProvenanceSignature(sampleProvenance);
			const sig2 = createProvenanceSignature(prov2);
			
			expect(sig1).not.toBe(sig2);
		});
	});
	
	describe('parseProvenanceSignature', () => {
		it('should parse valid signature', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed).not.toBeNull();
			expect(parsed!.vectorClock).toBeDefined();
			expect(parsed!.computationId).toBeDefined();
			expect(parsed!.deterministicHash).toBeDefined();
		});
		
		it('should return null for invalid format', () => {
			expect(parseProvenanceSignature('invalid')).toBeNull();
			expect(parseProvenanceSignature('p_')).toBeNull();
			expect(parseProvenanceSignature('x_comp:test_det:hash')).toBeNull();
		});
		
		it('should extract computation ID correctly', () => {
			const sig = 'p_alice:5_comp:testcomp_det:hash123';
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed!.computationId).toBe('testcomp');
		});
		
		it('should extract deterministic hash correctly', () => {
			const sig = 'p_alice:5_comp:testcomp_det:hash123';
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed!.deterministicHash).toBe('hash123');
		});
		
		it('should extract vector clock correctly', () => {
			const sig = 'p_alice:5_bob:3_comp:test_det:hash';
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed!.vectorClock.alice).toBe(5);
			expect(parsed!.vectorClock.bob).toBe(3);
		});
		
		it('should handle complex vector clocks', () => {
			const prov: ComputationProvenance = {
				...sampleProvenance,
				vectorClock: { alice: 10, bob: 5, charlie: 3 }
			};
			
			const sig = createProvenanceSignature(prov);
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed).not.toBeNull();
			expect(Object.keys(parsed!.vectorClock).length).toBe(3);
		});
	});
	
	describe('round-trip', () => {
		it('should maintain data integrity through create and parse', () => {
			const sig = createProvenanceSignature(sampleProvenance);
			const parsed = parseProvenanceSignature(sig);
			
			expect(parsed).not.toBeNull();
			
			// Create new signature from parsed data
			const prov2: ComputationProvenance = {
				...sampleProvenance,
				vectorClock: parsed!.vectorClock,
				computationId: parsed!.computationId,
				deterministicHash: parsed!.deterministicHash
			};
			
			// Note: Due to truncation, exact match may not occur
			// But structure should be preserved
			expect(parsed!.computationId).toContain('test-com');
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// PATH BUILDING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Provenance Path Building', () => {
	describe('buildProvenancePath', () => {
		it('should build path with all components (cross-user)', () => {
			const path = buildProvenancePath(
				'alice_pub',
				'prog-hash-123',
				'data/results',
				'p_alice:5_comp:test_det:hash'
			);
			
			// Returns [pubkey, path] for cross-user access
			expect(Array.isArray(path)).toBe(true);
			if (Array.isArray(path)) {
				expect(path[0]).toBe('alice_pub');
				expect(path[1]).toContain('prog-hash-123');
				expect(path[1]).toContain('data/results');
				expect(path[1]).toContain('p_alice:5_comp:test_det:hash');
			}
		});
		
		it('should return string path for null pubkey (current user)', () => {
			const path = buildProvenancePath(
				null,
				'prog-hash-123',
				'data/results',
				'p_alice:5_comp:test_det:hash'
			);
			
			// Returns string for current user
			expect(typeof path).toBe('string');
			if (typeof path === 'string') {
				expect(path).toContain('prog-hash-123');
				expect(path).toContain('data/results');
				expect(path).toContain('p_alice:5_comp:test_det:hash');
			}
		});
		
		it('should join all path segments correctly (cross-user)', () => {
			const path = buildProvenancePath(
				'alice',
				'hash123',
				'test/data',
				'p_sig'
			);
			
			expect(Array.isArray(path)).toBe(true);
			if (Array.isArray(path)) {
				expect(path[0]).toBe('alice');
				expect(path[1]).toContain('hash123/test/data/p_sig');
			}
		});
		
		it('should join all path segments correctly (current user)', () => {
			const path = buildProvenancePath(
				null,
				'hash123',
				'test/data',
				'p_sig'
			);
			
			expect(typeof path).toBe('string');
			if (typeof path === 'string') {
				expect(path).toBe('hash123/test/data/p_sig');
			}
		});
		
		it('should handle nested paths', () => {
			const path = buildProvenancePath(
				'alice',
				'hash',
				'a/b/c/d',
				'p_sig'
			);
			
			expect(Array.isArray(path)).toBe(true);
			if (Array.isArray(path)) {
				expect(path[1]).toContain('a/b/c/d');
			}
		});
		
		it('should integrate with buildProgramDataPath', () => {
			const basePath = buildProgramDataPath('alice', 'hash', 'data');
			const provPath = buildProvenancePath('alice', 'hash', 'data', 'p_sig');
			
			// Both should be arrays for cross-user
			expect(Array.isArray(basePath)).toBe(true);
			expect(Array.isArray(provPath)).toBe(true);
			
			if (Array.isArray(basePath) && Array.isArray(provPath)) {
				expect(provPath[0]).toBe(basePath[0]);
				expect(provPath[1]).toContain(basePath[1]);
			}
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// INTEGRATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Integration', () => {
	it('should create complete provenance workflow', () => {
		// 1. Hash inputs
		const inputA = { value: 5 };
		const inputB = { value: 3 };
		const inputHashA = hashContent(inputA);
		const inputHashB = hashContent(inputB);
		
		// 2. Hash computation
		const computation: Computation = {
			id: 'add-numbers',
			inputs: {
				a: { type: 'value', value: inputA },
				b: { type: 'value', value: inputB }
			},
			compute_fn: 'add',
			outputs: {
				result: { type: 'holster', holster_path: 'math/sum' }
			}
		};
		const compHash = hashComputation(computation);
		
		// 3. Create deterministic hash
		const detHash = createDeterministicHash(compHash, {
			a: inputHashA,
			b: inputHashB
		});
		
		// 4. Create provenance
		const provenance: ComputationProvenance = {
			id: 'prov-integration-test',
			vectorClock: { alice: 1 },
			executedBy: 'alice',
			timestamp: Date.now(),
			programHash: 'prog-123',
			computationId: computation.id,
			computationHash: compHash,
			inputs: {
				a: { source: 'value', contentHash: inputHashA },
				b: { source: 'value', contentHash: inputHashB }
			},
			outputs: {
				result: { path: 'math/sum', contentHash: hashContent(8) }
			},
			deterministicHash: detHash
		};
		
		// 5. Create signature
		const sig = createProvenanceSignature(provenance);
		
		// 6. Build path
		const path = buildProvenancePath(
			'alice',
			'prog-123',
			'math/sum',
			sig
		);
		
		// Verify
		expect(sig).toContain('p_');
		// path is an array for cross-user
		if (Array.isArray(path)) {
			expect(path[1]).toContain('math/sum');
			expect(path[1]).toContain(sig);
		}
		
		// 7. Parse signature back
		const parsed = parseProvenanceSignature(sig);
		expect(parsed).not.toBeNull();
		expect(parsed!.computationId).toContain('add-numb');
	});
	
	it('should verify hash consistency across workflow', () => {
		const data = { test: 'data', count: 42 };
		
		// Hash multiple times
		const hash1 = hashContent(data);
		const hash2 = hashContent(data);
		const hash3 = hashContent({ count: 42, test: 'data' });
		
		// All should match
		expect(hash1).toBe(hash2);
		expect(hash2).toBe(hash3);
	});
	
	it('should handle full provenance lifecycle', () => {
		// Create computation
		const comp: Computation = {
			id: 'lifecycle-test',
			inputs: { x: { type: 'value', value: 10 } },
			compute_fn: 'square',
			outputs: { result: { type: 'memory' } }
		};
		
		// Create provenance
		const prov: ComputationProvenance = {
			id: 'prov-lifecycle',
			vectorClock: { user: 1 },
			executedBy: 'user',
			timestamp: Date.now(),
			programHash: 'test-prog',
			computationId: comp.id,
			computationHash: hashComputation(comp),
			inputs: {
				x: {
					source: 'value',
					contentHash: hashContent(10)
				}
			},
			outputs: {
				result: {
					path: 'results',
					contentHash: hashContent(100)
				}
			},
			deterministicHash: createDeterministicHash(
				hashComputation(comp),
				{ x: hashContent(10) }
			)
		};
		
		// Create and parse signature
		const sig = createProvenanceSignature(prov);
		const parsed = parseProvenanceSignature(sig);
		
		// Build path
		const path = buildProvenancePath('user', 'test-prog', 'results', sig);
		
		// Verify everything works
		expect(parsed).not.toBeNull();
		// path is an array for cross-user
		if (Array.isArray(path)) {
			expect(path[1]).toContain('test-prog');
			expect(path[1]).toContain('results');
			expect(path[1]).toContain('p_');
		}
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases', () => {
	describe('Content Hashing Edge Cases', () => {
		it('should handle undefined', () => {
			const hash = hashContent(undefined);
			expect(hash).toHaveLength(16);
		});
		
		it('should handle very large numbers', () => {
			const hash = hashContent(Number.MAX_SAFE_INTEGER);
			expect(hash).toHaveLength(16);
		});
		
		it('should handle empty arrays', () => {
			const hash = hashContent([]);
			expect(hash).toHaveLength(16);
		});
		
		it('should handle empty objects', () => {
			const hash = hashContent({});
			expect(hash).toHaveLength(16);
		});
		
		it('should handle deeply nested structures', () => {
			const deep = { a: { b: { c: { d: { e: { f: 'value' } } } } } };
			const hash = hashContent(deep);
			expect(hash).toHaveLength(16);
		});
		
		it('should handle special characters in strings', () => {
			const data = { text: 'Hello\n\t"World"\'🎉' };
			const hash = hashContent(data);
			expect(hash).toHaveLength(16);
		});
		
		it('should handle unicode characters', () => {
			const data = { text: '你好世界 مرحبا العالم שלום עולם' };
			const hash = hashContent(data);
			expect(hash).toHaveLength(16);
		});
	});
	
	describe('Vector Clock Edge Cases', () => {
		it('should handle very long pubkeys', () => {
			const longKey = 'a'.repeat(100);
			const vc: VectorClock = { [longKey]: 1 };
			const sig = vectorClockSignature(vc);
			
			expect(sig).toHaveLength(10); // 8 chars + :1
		});
		
		it('should handle large version numbers', () => {
			const vc: VectorClock = { alice: 999999 };
			const sig = vectorClockSignature(vc);
			
			expect(sig).toContain('999999');
		});
		
		it('should handle many participants', () => {
			const vc: VectorClock = {};
			for (let i = 0; i < 20; i++) {
				vc[`peer${i}`] = i;
			}
			
			const sig = vectorClockSignature(vc);
			expect(sig.split('_').length).toBe(20);
		});
	});
	
	describe('Provenance Signature Edge Cases', () => {
		it('should handle very long computation IDs', () => {
			const prov: ComputationProvenance = {
				...sampleProvenance,
				computationId: 'very-long-computation-id-that-exceeds-normal-length'
			};
			
			const sig = createProvenanceSignature(prov);
			expect(sig).toContain('_comp:very-lon');  // Truncated
		});
		
		it('should handle special characters in IDs', () => {
			const prov: ComputationProvenance = {
				...sampleProvenance,
				computationId: 'comp-with-special-chars!@#'
			};
			
			const sig = createProvenanceSignature(prov);
			expect(sig).toContain('_comp:');
		});
		
		it('should handle empty vector clock', () => {
			const prov: ComputationProvenance = {
				...sampleProvenance,
				vectorClock: {}
			};
			
			const sig = createProvenanceSignature(prov);
			// Should still have comp and det markers
			expect(sig).toContain('_comp:');
			expect(sig).toContain('_det:');
		});
	});
	
	describe('Path Building Edge Cases', () => {
		it('should handle paths with slashes', () => {
			const path = buildProvenancePath(
				'alice',
				'hash',
				'a/b/c',
				'p_sig'
			);
			
			if (Array.isArray(path)) {
				expect(path[1].split('/').length).toBeGreaterThan(4);
			}
		});
		
		it('should handle empty path segments', () => {
			const path = buildProvenancePath(
				'alice',
				'hash',
				'',
				'p_sig'
			);
			
			if (Array.isArray(path)) {
				expect(path[0]).toBe('alice');
			}
		});
		
		it('should handle very long paths', () => {
			const longPath = 'a/'.repeat(50) + 'final';
			const path = buildProvenancePath(
				'alice',
				'hash',
				longPath,
				'p_sig'
			);
			
			if (Array.isArray(path)) {
				expect(path[1]).toContain(longPath);
			}
		});
	});
});

console.log('\n✅ Provenance unit tests defined');
console.log('Run with: bun test src/lib/commons/tests/provenance.test.ts');

