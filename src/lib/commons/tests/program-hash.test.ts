/**
 * Program Hashing Tests
 * 
 * Comprehensive test suite for:
 * - Program hashing (deterministic, collision-free)
 * - Path manipulation (prefix/unprefix/extract)
 * - Program registry (register/get/list)
 * - Schema validation
 * - Edge cases and error handling
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
	hashProgram,
	getProgramHash,
	verifyProgramHash,
	prefixHolsterPath,
	unprefixHolsterPath,
	extractProgramHash,
	buildProgramDataPath,
	registerProgram,
	getProgramByHash,
	listRegisteredPrograms,
	unregisterProgram,
	clearProgramRegistry,
	getProgramMetadata,
	getAllProgramMetadata
} from '../program-hash.svelte';
import type { ReactiveComputationGraph } from '../v1/schemas';

// ═══════════════════════════════════════════════════════════════════
// TEST FIXTURES
// ═══════════════════════════════════════════════════════════════════

const simpleProgram: ReactiveComputationGraph = {
	id: 'simple-counter',
	variables: {
		count: {
			type: 'value',
			value: 0
		}
	},
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'value', value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: { type: 'memory' }
			}
		}
	]
};

const differentProgram: ReactiveComputationGraph = {
	id: 'different-counter',
	variables: {
		count: {
			type: 'value',
			value: 10  // Different value
		}
	},
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'value', value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: { type: 'memory' }
			}
		}
	]
};

const programWithMetadata: ReactiveComputationGraph = {
	id: 'with-metadata',
	version: '1.0.0',
	description: 'A program with metadata',
	variables: {
		count: {
			type: 'value',
			value: 0
		}
	},
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'value', value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: { type: 'memory' }
			}
		}
	]
};

const programWithManualHash: ReactiveComputationGraph = {
	id: 'manual-hash',
	program_hash: 'custom-hash-v1',
	variables: {
		count: {
			type: 'value',
			value: 0
		}
	},
	computations: [
		{
			id: 'increment',
			inputs: {
				value: { type: 'value', value: 0 }
			},
			compute_fn: 'increment',
			outputs: {
				result: { type: 'memory' }
			}
		}
	]
};

// ═══════════════════════════════════════════════════════════════════
// HASHING TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Program Hashing', () => {
	describe('hashProgram', () => {
		it('should generate a hash for a program', () => {
			const hash = hashProgram(simpleProgram);
			
			expect(hash).toBeDefined();
			expect(typeof hash).toBe('string');
			expect(hash.length).toBe(16);
			expect(hash).toMatch(/^[0-9a-f]{16}$/);
		});
		
		it('should generate consistent hashes for the same program', () => {
			const hash1 = hashProgram(simpleProgram);
			const hash2 = hashProgram(simpleProgram);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should generate different hashes for different programs', () => {
			const hash1 = hashProgram(simpleProgram);
			const hash2 = hashProgram(differentProgram);
			
			expect(hash1).not.toBe(hash2);
		});
		
		it('should ignore metadata in hash computation', () => {
			const withoutMetadata: ReactiveComputationGraph = {
				id: 'test',
				variables: simpleProgram.variables,
				computations: simpleProgram.computations
			};
			
			const withMetadata: ReactiveComputationGraph = {
				id: 'different-id',
				version: '2.0.0',
				description: 'Different description',
				variables: simpleProgram.variables,
				computations: simpleProgram.computations
			};
			
			const hash1 = hashProgram(withoutMetadata);
			const hash2 = hashProgram(withMetadata);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should ignore program_hash field in hash computation', () => {
			const withHash: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: 'custom-hash'
			};
			
			const hash1 = hashProgram(simpleProgram);
			const hash2 = hashProgram(withHash);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should be deterministic across multiple calls', () => {
			const hashes = new Set<string>();
			
			for (let i = 0; i < 100; i++) {
				hashes.add(hashProgram(simpleProgram));
			}
			
			expect(hashes.size).toBe(1);
		});
	});
	
	describe('getProgramHash', () => {
		it('should use program_hash if provided', () => {
			const hash = getProgramHash(programWithManualHash);
			
			expect(hash).toBe('custom-hash-v1');
		});
		
		it('should compute hash if program_hash not provided', () => {
			const hash = getProgramHash(simpleProgram);
			const computed = hashProgram(simpleProgram);
			
			expect(hash).toBe(computed);
		});
		
		it('should prefer program_hash over computed hash', () => {
			const hash = getProgramHash(programWithManualHash);
			const computed = hashProgram(programWithManualHash);
			
			expect(hash).toBe('custom-hash-v1');
			expect(hash).not.toBe(computed);
		});
	});
	
	describe('verifyProgramHash', () => {
		it('should return true if program_hash is not set', () => {
			const result = verifyProgramHash(simpleProgram);
			
			expect(result).toBe(true);
		});
		
		it('should return true if program_hash matches computed hash', () => {
			const computed = hashProgram(simpleProgram);
			const programWithCorrectHash: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: computed
			};
			
			const result = verifyProgramHash(programWithCorrectHash);
			
			expect(result).toBe(true);
		});
		
		it('should return false if program_hash does not match computed hash', () => {
			const programWithWrongHash: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: 'wrong-hash'
			};
			
			const result = verifyProgramHash(programWithWrongHash);
			
			expect(result).toBe(false);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// PATH MANIPULATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Path Manipulation', () => {
	const testHash = 'abc123def456';
	
	describe('prefixHolsterPath', () => {
		it('should prefix a simple path', () => {
			const result = prefixHolsterPath(testHash, 'tree');
			
			expect(result).toBe('abc123def456/tree');
		});
		
		it('should prefix a nested path', () => {
			const result = prefixHolsterPath(testHash, 'nodes/node1/data');
			
			expect(result).toBe('abc123def456/nodes/node1/data');
		});
		
		it('should handle leading slashes', () => {
			const result = prefixHolsterPath(testHash, '/tree');
			
			expect(result).toBe('abc123def456/tree');
		});
		
		it('should handle trailing slashes', () => {
			const result = prefixHolsterPath(testHash, 'tree/');
			
			expect(result).toBe('abc123def456/tree');
		});
		
		it('should handle multiple slashes', () => {
			const result = prefixHolsterPath(testHash, '//tree//');
			
			expect(result).toBe('abc123def456/tree');
		});
		
		it('should handle empty path', () => {
			const result = prefixHolsterPath(testHash, '');
			
			expect(result).toBe('abc123def456/');
		});
	});
	
	describe('unprefixHolsterPath', () => {
		it('should unprefix a simple path', () => {
			const result = unprefixHolsterPath('abc123def456/tree');
			
			expect(result).toBe('tree');
		});
		
		it('should unprefix a nested path', () => {
			const result = unprefixHolsterPath('abc123def456/nodes/node1/data');
			
			expect(result).toBe('nodes/node1/data');
		});
		
		it('should return original path if no prefix', () => {
			const result = unprefixHolsterPath('tree');
			
			expect(result).toBe('tree');
		});
		
		it('should handle paths with multiple segments', () => {
			const result = unprefixHolsterPath('hash/a/b/c/d');
			
			expect(result).toBe('a/b/c/d');
		});
	});
	
	describe('extractProgramHash', () => {
		it('should extract hash from prefixed path', () => {
			const result = extractProgramHash('abc123def456/tree');
			
			expect(result).toBe('abc123def456');
		});
		
		it('should extract hash from nested path', () => {
			const result = extractProgramHash('abc123def456/nodes/node1/data');
			
			expect(result).toBe('abc123def456');
		});
		
		it('should return null for unprefixed path', () => {
			const result = extractProgramHash('tree');
			
			expect(result).toBeNull();
		});
		
		it('should handle single segment paths', () => {
			const result = extractProgramHash('onlyhash');
			
			expect(result).toBeNull();
		});
	});
	
	describe('buildProgramDataPath', () => {
		const hash = 'abc123';
		
		it('should build path for current user', () => {
			const result = buildProgramDataPath(null, hash, 'tree');
			
			expect(result).toBe('abc123/tree');
		});
		
		it('should build path for other user', () => {
			const result = buildProgramDataPath('user-pubkey', hash, 'tree');
			
			expect(result).toEqual(['user-pubkey', 'abc123/tree']);
		});
		
		it('should handle nested paths', () => {
			const result = buildProgramDataPath('user-pubkey', hash, 'nodes/node1');
			
			expect(result).toEqual(['user-pubkey', 'abc123/nodes/node1']);
		});
		
		it('should work with null pubkey', () => {
			const result = buildProgramDataPath(null, hash, 'data');
			
			expect(result).toBe('abc123/data');
		});
	});
	
	describe('Path round-trip', () => {
		it('should maintain path integrity through prefix/unprefix', () => {
			const originalPath = 'nodes/node1/data/value';
			const hash = 'testhash123';
			
			const prefixed = prefixHolsterPath(hash, originalPath);
			const unprefixed = unprefixHolsterPath(prefixed);
			
			expect(unprefixed).toBe(originalPath);
		});
		
		it('should maintain hash through extract after prefix', () => {
			const hash = 'testhash456';
			const path = 'tree';
			
			const prefixed = prefixHolsterPath(hash, path);
			const extracted = extractProgramHash(prefixed);
			
			expect(extracted).toBe(hash);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// REGISTRY TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Program Registry', () => {
	beforeEach(() => {
		clearProgramRegistry();
	});
	
	afterEach(() => {
		clearProgramRegistry();
	});
	
	describe('registerProgram', () => {
		it('should register a program and return hash', () => {
			const hash = registerProgram(simpleProgram);
			
			expect(hash).toBeDefined();
			expect(typeof hash).toBe('string');
		});
		
		it('should return same hash for same program', () => {
			const hash1 = registerProgram(simpleProgram);
			const hash2 = registerProgram(simpleProgram);
			
			expect(hash1).toBe(hash2);
		});
		
		it('should use manual hash if provided', () => {
			const hash = registerProgram(programWithManualHash);
			
			expect(hash).toBe('custom-hash-v1');
		});
		
		it('should allow re-registration (update)', () => {
			const hash1 = registerProgram(simpleProgram);
			const hash2 = registerProgram(simpleProgram);
			
			expect(hash1).toBe(hash2);
			expect(listRegisteredPrograms()).toHaveLength(1);
		});
	});
	
	describe('getProgramByHash', () => {
		it('should retrieve registered program', () => {
			const hash = registerProgram(simpleProgram);
			const retrieved = getProgramByHash(hash);
			
			expect(retrieved).toBeDefined();
			expect(retrieved?.id).toBe(simpleProgram.id);
		});
		
		it('should return null for unregistered hash', () => {
			const retrieved = getProgramByHash('nonexistent-hash');
			
			expect(retrieved).toBeNull();
		});
		
		it('should retrieve correct program when multiple registered', () => {
			const hash1 = registerProgram(simpleProgram);
			const hash2 = registerProgram(differentProgram);
			
			const retrieved1 = getProgramByHash(hash1);
			const retrieved2 = getProgramByHash(hash2);
			
			expect(retrieved1?.id).toBe(simpleProgram.id);
			expect(retrieved2?.id).toBe(differentProgram.id);
		});
	});
	
	describe('listRegisteredPrograms', () => {
		it('should return empty array when no programs registered', () => {
			const list = listRegisteredPrograms();
			
			expect(list).toEqual([]);
		});
		
		it('should list registered program hash', () => {
			const hash = registerProgram(simpleProgram);
			const list = listRegisteredPrograms();
			
			expect(list).toContain(hash);
			expect(list).toHaveLength(1);
		});
		
		it('should list multiple registered programs', () => {
			const hash1 = registerProgram(simpleProgram);
			const hash2 = registerProgram(differentProgram);
			const list = listRegisteredPrograms();
			
			expect(list).toContain(hash1);
			expect(list).toContain(hash2);
			expect(list).toHaveLength(2);
		});
		
		it('should not list duplicates', () => {
			registerProgram(simpleProgram);
			registerProgram(simpleProgram);
			const list = listRegisteredPrograms();
			
			expect(list).toHaveLength(1);
		});
	});
	
	describe('unregisterProgram', () => {
		it('should unregister a program', () => {
			const hash = registerProgram(simpleProgram);
			const result = unregisterProgram(hash);
			
			expect(result).toBe(true);
			expect(getProgramByHash(hash)).toBeNull();
		});
		
		it('should return false for non-existent hash', () => {
			const result = unregisterProgram('nonexistent-hash');
			
			expect(result).toBe(false);
		});
		
		it('should remove program from list', () => {
			const hash = registerProgram(simpleProgram);
			unregisterProgram(hash);
			const list = listRegisteredPrograms();
			
			expect(list).not.toContain(hash);
		});
		
		it('should only remove specified program', () => {
			const hash1 = registerProgram(simpleProgram);
			const hash2 = registerProgram(differentProgram);
			
			unregisterProgram(hash1);
			
			expect(getProgramByHash(hash1)).toBeNull();
			expect(getProgramByHash(hash2)).toBeDefined();
		});
	});
	
	describe('clearProgramRegistry', () => {
		it('should clear all registered programs', () => {
			registerProgram(simpleProgram);
			registerProgram(differentProgram);
			registerProgram(programWithMetadata);
			
			clearProgramRegistry();
			
			expect(listRegisteredPrograms()).toHaveLength(0);
		});
		
		it('should allow re-registration after clear', () => {
			const hash1 = registerProgram(simpleProgram);
			clearProgramRegistry();
			const hash2 = registerProgram(simpleProgram);
			
			expect(hash1).toBe(hash2);
			expect(listRegisteredPrograms()).toHaveLength(1);
		});
	});
	
	describe('getProgramMetadata', () => {
		it('should return metadata for registered program', () => {
			const hash = registerProgram(programWithMetadata);
			const metadata = getProgramMetadata(hash);
			
			expect(metadata).toBeDefined();
			expect(metadata?.hash).toBe(hash);
			expect(metadata?.id).toBe(programWithMetadata.id);
			expect(metadata?.version).toBe(programWithMetadata.version);
			expect(metadata?.description).toBe(programWithMetadata.description);
		});
		
		it('should return null for unregistered hash', () => {
			const metadata = getProgramMetadata('nonexistent-hash');
			
			expect(metadata).toBeNull();
		});
		
		it('should include computation and variable counts', () => {
			const hash = registerProgram(simpleProgram);
			const metadata = getProgramMetadata(hash);
			
			expect(metadata?.computation_count).toBe(1);
			expect(metadata?.variable_count).toBe(1);
		});
	});
	
	describe('getAllProgramMetadata', () => {
		it('should return empty array when no programs registered', () => {
			const metadata = getAllProgramMetadata();
			
			expect(metadata).toEqual([]);
		});
		
		it('should return metadata for all registered programs', () => {
			registerProgram(simpleProgram);
			registerProgram(differentProgram);
			
			const metadata = getAllProgramMetadata();
			
			expect(metadata).toHaveLength(2);
			expect(metadata[0]).toHaveProperty('hash');
			expect(metadata[0]).toHaveProperty('id');
			expect(metadata[1]).toHaveProperty('hash');
			expect(metadata[1]).toHaveProperty('id');
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// EDGE CASES AND ERROR HANDLING
// ═══════════════════════════════════════════════════════════════════

describe('Edge Cases', () => {
	beforeEach(() => {
		clearProgramRegistry();
	});
	
	afterEach(() => {
		clearProgramRegistry();
	});
	
	describe('Empty programs', () => {
		it('should hash program with no variables', () => {
			const emptyVars: ReactiveComputationGraph = {
				id: 'empty-vars',
				variables: {},
				computations: []
			};
			
			const hash = hashProgram(emptyVars);
			
			expect(hash).toBeDefined();
			expect(hash.length).toBe(16);
		});
		
		it('should hash program with no computations', () => {
			const emptyComps: ReactiveComputationGraph = {
				id: 'empty-comps',
				variables: {
					test: { type: 'value', value: 1 }
				},
				computations: []
			};
			
			const hash = hashProgram(emptyComps);
			
			expect(hash).toBeDefined();
			expect(hash.length).toBe(16);
		});
	});
	
	describe('Special characters in paths', () => {
		it('should handle paths with spaces', () => {
			const hash = 'testhash';
			const path = 'my data/with spaces';
			
			const prefixed = prefixHolsterPath(hash, path);
			expect(prefixed).toBe('testhash/my data/with spaces');
			
			const unprefixed = unprefixHolsterPath(prefixed);
			expect(unprefixed).toBe(path);
		});
		
		it('should handle paths with special characters', () => {
			const hash = 'testhash';
			const path = 'data-with_special.chars';
			
			const prefixed = prefixHolsterPath(hash, path);
			const unprefixed = unprefixHolsterPath(prefixed);
			
			expect(unprefixed).toBe(path);
		});
	});
	
	describe('Hash collisions', () => {
		it('should generate unique hashes for similar programs', () => {
			const programs: ReactiveComputationGraph[] = [];
			
			for (let i = 0; i < 10; i++) {
				programs.push({
					id: `program-${i}`,
					variables: {
						count: { type: 'value', value: i }
					},
					computations: []
				});
			}
			
			const hashes = programs.map(p => hashProgram(p));
			const uniqueHashes = new Set(hashes);
			
			expect(uniqueHashes.size).toBe(programs.length);
		});
	});
	
	describe('Very long paths', () => {
		it('should handle very long nested paths', () => {
			const hash = 'testhash';
			const longPath = Array(100).fill('segment').join('/');
			
			const prefixed = prefixHolsterPath(hash, longPath);
			const unprefixed = unprefixHolsterPath(prefixed);
			
			expect(unprefixed).toBe(longPath);
		});
	});
	
	describe('Manual hash edge cases', () => {
		it('should handle empty string as manual hash', () => {
			const program: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: ''
			};
			
			const hash = getProgramHash(program);
			expect(hash).toBe('');
		});
		
		it('should handle manual hash with special characters', () => {
			const program: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: 'v1.0.0-beta.1'
			};
			
			const hash = getProgramHash(program);
			expect(hash).toBe('v1.0.0-beta.1');
		});
		
		it('should handle very long manual hash', () => {
			const longHash = 'a'.repeat(100);
			const program: ReactiveComputationGraph = {
				...simpleProgram,
				program_hash: longHash
			};
			
			const hash = getProgramHash(program);
			expect(hash).toBe(longHash);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// INTEGRATION TESTS
// ═══════════════════════════════════════════════════════════════════

describe('Integration', () => {
	beforeEach(() => {
		clearProgramRegistry();
	});
	
	afterEach(() => {
		clearProgramRegistry();
	});
	
	it('should support full workflow: register, retrieve, use hash', () => {
		// Register
		const hash = registerProgram(simpleProgram);
		
		// Verify registration
		expect(listRegisteredPrograms()).toContain(hash);
		
		// Retrieve
		const retrieved = getProgramByHash(hash);
		expect(retrieved).toBeDefined();
		expect(retrieved?.id).toBe(simpleProgram.id);
		
		// Use hash for path
		const path = prefixHolsterPath(hash, 'tree');
		expect(path).toBe(`${hash}/tree`);
		
		// Extract hash from path
		const extracted = extractProgramHash(path);
		expect(extracted).toBe(hash);
		
		// Unregister
		const success = unregisterProgram(hash);
		expect(success).toBe(true);
		
		// Verify unregistration
		expect(listRegisteredPrograms()).not.toContain(hash);
	});
	
	it('should handle multiple programs with different hashes', () => {
		const hash1 = registerProgram(simpleProgram);
		const hash2 = registerProgram(differentProgram);
		const hash3 = registerProgram(programWithMetadata);
		
		// simpleProgram and programWithMetadata have same variables/computations (different metadata only)
		// So they should have the same hash (metadata is ignored)
		expect(hash1).not.toBe(hash2);
		expect(hash2).not.toBe(hash3);
		expect(hash1).toBe(hash3);  // Same structure, same hash
		
		const list = listRegisteredPrograms();
		expect(list).toHaveLength(2);  // Only 2 unique hashes
		expect(list).toContain(hash1);
		expect(list).toContain(hash2);
	});
	
	it('should maintain data isolation with different hashes', () => {
		const hash1 = registerProgram(simpleProgram);
		const hash2 = registerProgram(differentProgram);
		
		const path1 = prefixHolsterPath(hash1, 'data');
		const path2 = prefixHolsterPath(hash2, 'data');
		
		expect(path1).not.toBe(path2);
		expect(path1).toBe(`${hash1}/data`);
		expect(path2).toBe(`${hash2}/data`);
	});
});

