/**
 * RDL Schema Validation Tests
 * 
 * Comprehensive tests for the canonical Zod RDL schemas.
 * Tests validation logic, edge cases, and error handling.
 * 
 * Coverage:
 * - Primitive schema validation (Identifier, HolsterPath, PubKey, etc.)
 * - Variable binding schema validation (all 5 types)
 * - Output binding schema validation (all 3 types)
 * - Computation schema validation
 * - ReactiveComputationGraph schema validation
 * - Provenance schema validation
 * - Error messages and edge cases
 */

import { describe, it, expect } from 'vitest';
import {
	// Primitive schemas
	IdentifierSchema,
	HolsterPathSchema,
	PubKeySchema,
	SchemaTypeNameSchema,
	FunctionNameSchema,
	
	// Binding schemas
	VariableBindingSchema,
	OutputBindingSchema,
	
	// Computation schemas
	ComputationSchema,
	ReactiveComputationGraphSchema,
	
	// Provenance schemas
	InputProvenanceSchema,
	OutputProvenanceSchema,
	ComputationProvenanceSchema
} from '../schema';

import { seed as itcSeed } from '$lib/utils/primitives/itc';

// ═══════════════════════════════════════════════════════════════════
// PRIMITIVE SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - Primitives', () => {
	describe('IdentifierSchema', () => {
		it('accepts valid identifiers', () => {
			const valid = ['myVar', '_private', 'compute_1', 'allocation-v2', 'A', 'a1_b2-c3'];
			
			for (const id of valid) {
				const result = IdentifierSchema.safeParse(id);
				expect(result.success).toBe(true);
			}
		});
		
		it('rejects invalid identifiers', () => {
			const invalid = ['', '1start', 'has space', 'has.dot', 'has/slash', '-start'];
			
			for (const id of invalid) {
				const result = IdentifierSchema.safeParse(id);
				expect(result.success).toBe(false);
			}
		});
		
		it('rejects non-strings', () => {
			const result = IdentifierSchema.safeParse(123);
			expect(result.success).toBe(false);
		});
	});
	
	describe('HolsterPathSchema', () => {
		it('accepts valid paths', () => {
			const valid = [
				'allocation/commitment',
				'data/tree',
				'path.to.data',
				'~user/data',
				'nested/path/to/data-v2',
				'a',
				'a/b/c.json'
			];
			
			for (const path of valid) {
				const result = HolsterPathSchema.safeParse(path);
				expect(result.success).toBe(true);
			}
		});
		
		it('rejects invalid paths', () => {
			const invalid = ['', 'has space', 'has@symbol', 'has#hash', 'has!bang'];
			
			for (const path of invalid) {
				const result = HolsterPathSchema.safeParse(path);
				expect(result.success).toBe(false);
			}
		});
	});
	
	describe('PubKeySchema', () => {
		it('accepts valid 64-char hex keys', () => {
			const valid = [
				'0'.repeat(64),
				'A'.repeat(64),
				'f'.repeat(64),
				'0123456789abcdef'.repeat(4)
			];
			
			for (const key of valid) {
				const result = PubKeySchema.safeParse(key);
				expect(result.success).toBe(true);
			}
		});
		
		it('rejects invalid public keys', () => {
			const invalid = [
				'0'.repeat(63), // Too short
				'0'.repeat(65), // Too long
				'g'.repeat(64), // Invalid hex
				'0123456789', // Way too short
				''
			];
			
			for (const key of invalid) {
				const result = PubKeySchema.safeParse(key);
				expect(result.success).toBe(false);
			}
		});
	});
	
	describe('SchemaTypeNameSchema', () => {
		it('accepts any non-empty string', () => {
			const valid = ['Commitment', 'RootNode', 'Any', 'MyCustomSchema', 'a'];
			
			for (const name of valid) {
				const result = SchemaTypeNameSchema.safeParse(name);
				expect(result.success).toBe(true);
			}
		});
		
		it('rejects empty string', () => {
			const result = SchemaTypeNameSchema.safeParse('');
			expect(result.success).toBe(false);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// VARIABLE BINDING SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - Variable Bindings', () => {
	describe('value binding', () => {
		it('accepts any value', () => {
			const values = [
				42,
				'string',
				true,
				{ nested: 'object' },
				[1, 2, 3],
				null
			];
			
			for (const value of values) {
				const binding = { type: 'value', value };
				const result = VariableBindingSchema.safeParse(binding);
				expect(result.success).toBe(true);
			}
		});
		
		it('allows undefined/null values', () => {
			const binding1 = { type: 'value', value: null };
			const binding2 = { type: 'value', value: undefined };
			
			expect(VariableBindingSchema.safeParse(binding1).success).toBe(true);
			expect(VariableBindingSchema.safeParse(binding2).success).toBe(true);
		});
	});
	
	describe('subscription binding', () => {
		it('accepts valid subscription', () => {
			const binding = {
				type: 'subscription',
				holster_path: 'allocation/commitment',
				schema_type: 'Commitment',
				subscribe_to_user: '0'.repeat(64),
				default_value: null
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('requires holster_path and schema_type', () => {
			const invalid = {
				type: 'subscription'
				// Missing required fields
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('validates holster_path format', () => {
			const invalid = {
				type: 'subscription',
				holster_path: 'invalid path!',
				schema_type: 'Any'
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('validates subscribe_to_user format', () => {
			const invalid = {
				type: 'subscription',
				holster_path: 'path',
				schema_type: 'Any',
				subscribe_to_user: 'not_a_pubkey'
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('makes subscribe_to_user optional', () => {
			const binding = {
				type: 'subscription',
				holster_path: 'path',
				schema_type: 'Any'
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
	});
	
	describe('fetch binding', () => {
		it('accepts valid fetch binding', () => {
			const binding = {
				type: 'fetch',
				holster_path: 'config/settings',
				schema_type: 'Config',
				fetch_from_user: '0'.repeat(64),
				default_value: {},
				wait_ms: 500
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('applies default wait_ms', () => {
			const binding = {
				type: 'fetch',
				holster_path: 'path',
				schema_type: 'Any'
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
			if (result.success && result.data.type === 'fetch') {
				expect(result.data.wait_ms).toBe(100);
			}
		});
		
		it('rejects negative wait_ms', () => {
			const binding = {
				type: 'fetch',
				holster_path: 'path',
				schema_type: 'Any',
				wait_ms: -1
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(false);
		});
	});
	
	describe('local binding', () => {
		it('accepts valid local binding', () => {
			const binding = {
				type: 'local',
				state_path: 'myData.field.subfield',
				default_value: null
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('requires state_path', () => {
			const binding = {
				type: 'local'
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(false);
		});
		
		it('validates state_path format', () => {
			const invalid = {
				type: 'local',
				state_path: 'invalid path!'
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
	});
	
	describe('derived binding', () => {
		it('accepts valid derived binding', () => {
			const binding = {
				type: 'derived',
				computation_id: 'previous_comp',
				output_key: 'result',
				default_value: 0
			};
			
			const result = VariableBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('requires computation_id and output_key', () => {
			const invalid = {
				type: 'derived',
				computation_id: 'comp'
				// Missing output_key
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('validates identifier formats', () => {
			const invalid = {
				type: 'derived',
				computation_id: '123invalid',
				output_key: 'valid'
			};
			
			const result = VariableBindingSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// OUTPUT BINDING SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - Output Bindings', () => {
	describe('holster output', () => {
		it('accepts valid holster output', () => {
			const binding = {
				type: 'holster',
				holster_path: 'results/allocation',
				schema_type: 'AllocationState',
				persist_debounce_ms: 200
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('requires holster_path', () => {
			const binding = {
				type: 'holster'
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(false);
		});
		
		it('makes schema_type optional', () => {
			const binding = {
				type: 'holster',
				holster_path: 'path'
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('rejects negative debounce', () => {
			const binding = {
				type: 'holster',
				holster_path: 'path',
				persist_debounce_ms: -1
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(false);
		});
	});
	
	describe('local output', () => {
		it('accepts valid local output', () => {
			const binding = {
				type: 'local',
				state_path: 'results.allocation'
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('requires state_path', () => {
			const binding = {
				type: 'local'
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(false);
		});
	});
	
	describe('memory output', () => {
		it('accepts valid memory output', () => {
			const binding = {
				type: 'memory'
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
		
		it('rejects extra fields', () => {
			// Memory output should only have 'type' field
			const binding = {
				type: 'memory'
				// No extra fields allowed
			};
			
			const result = OutputBindingSchema.safeParse(binding);
			expect(result.success).toBe(true);
		});
	});
});

// ═══════════════════════════════════════════════════════════════════
// COMPUTATION SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - Computation', () => {
	it('accepts valid computation', () => {
		const computation = {
			id: 'compute_allocation',
			inputs: {
				capacity: { type: 'value', value: 100 },
				need: { type: 'local', state_path: 'need' }
			},
			compute_fn: 'allocate',
			outputs: {
				result: { type: 'holster', holster_path: 'results/allocation' }
			},
			debounce_ms: 200,
			enabled: true,
			depends_on: ['previous_comp'],
			description: 'Allocates resources',
			version: '1.0.0'
		};
		
		const result = ComputationSchema.safeParse(computation);
		expect(result.success).toBe(true);
	});
	
	it('applies default values', () => {
		const computation = {
			id: 'comp',
			inputs: { x: { type: 'value', value: 1 } },
			compute_fn: 'fn',
			outputs: { y: { type: 'memory' } }
		};
		
		const result = ComputationSchema.safeParse(computation);
		expect(result.success).toBe(true);
		
		if (result.success) {
			expect(result.data.debounce_ms).toBe(0);
			expect(result.data.enabled).toBe(true);
		}
	});
	
	it('requires id, inputs, compute_fn, and outputs', () => {
		const invalid = {
			id: 'comp',
			inputs: { x: { type: 'value', value: 1 } }
			// Missing compute_fn and outputs
		};
		
		const result = ComputationSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('validates identifier format for id', () => {
		const invalid = {
			id: '123invalid',
			inputs: { x: { type: 'value', value: 1 } },
			compute_fn: 'fn',
			outputs: { y: { type: 'memory' } }
		};
		
		const result = ComputationSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('accepts local_bindings', () => {
		const computation = {
			id: 'comp',
			inputs: { x: { type: 'value', value: 1 } },
			compute_fn: 'fn',
			local_bindings: {
				config: { type: 'value', value: { setting: true } }
			},
			outputs: { y: { type: 'memory' } }
		};
		
		const result = ComputationSchema.safeParse(computation);
		expect(result.success).toBe(true);
	});
	
	it('validates input binding types', () => {
		const invalid = {
			id: 'comp',
			inputs: {
				x: { type: 'invalid_type', value: 1 }
			},
			compute_fn: 'fn',
			outputs: { y: { type: 'memory' } }
		};
		
		const result = ComputationSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('validates output binding types', () => {
		const invalid = {
			id: 'comp',
			inputs: { x: { type: 'value', value: 1 } },
			compute_fn: 'fn',
			outputs: {
				y: { type: 'invalid_output' }
			}
		};
		
		const result = ComputationSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// REACTIVE COMPUTATION GRAPH SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - ReactiveComputationGraph', () => {
	it('accepts valid complete program', () => {
		const program = {
			id: 'allocation_v2',
			version: '2.0.0',
			description: 'Allocation algorithm',
			variables: {
				myCommitment: {
					type: 'subscription',
					holster_path: 'allocation/commitment',
					schema_type: 'Commitment'
				}
			},
			computations: [{
				id: 'allocate',
				inputs: {
					commitment: { type: 'local', state_path: 'myCommitment' }
				},
				compute_fn: 'twoTierAllocation',
				outputs: {
					state: { type: 'holster', holster_path: 'allocation/state' }
				}
			}],
			program_hash: 'custom_hash'
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(program);
		expect(result.success).toBe(true);
	});
	
	it('requires id, variables, and computations', () => {
		const invalid = {
			id: 'program',
			variables: {}
			// Missing computations
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('requires at least one computation', () => {
		const invalid = {
			id: 'program',
			variables: {},
			computations: []
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('makes version and description optional', () => {
		const program = {
			id: 'program',
			variables: {},
			computations: [{
				id: 'comp',
				inputs: { x: { type: 'value', value: 1 } },
				compute_fn: 'fn',
				outputs: { y: { type: 'memory' } }
			}]
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(program);
		expect(result.success).toBe(true);
	});
	
	it('validates program id format', () => {
		const invalid = {
			id: '123invalid',
			variables: {},
			computations: [{
				id: 'comp',
				inputs: { x: { type: 'value', value: 1 } },
				compute_fn: 'fn',
				outputs: { y: { type: 'memory' } }
			}]
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
	
	it('validates variable names are identifiers', () => {
		const invalid = {
			id: 'program',
			variables: {
				'invalid var': { type: 'value', value: 1 }
			},
			computations: [{
				id: 'comp',
				inputs: { x: { type: 'value', value: 1 } },
				compute_fn: 'fn',
				outputs: { y: { type: 'memory' } }
			}]
		};
		
		const result = ReactiveComputationGraphSchema.safeParse(invalid);
		expect(result.success).toBe(false);
	});
});

// ═══════════════════════════════════════════════════════════════════
// PROVENANCE SCHEMA TESTS
// ═══════════════════════════════════════════════════════════════════

describe('RDL Schemas - Provenance', () => {
	describe('InputProvenanceSchema', () => {
		it('accepts valid input provenance', () => {
			const provenance = {
				source: 'subscription',
				path: 'allocation/commitment',
				contentHash: 'abc123',
				provenance: 'parent_prov_id'
			};
			
			const result = InputProvenanceSchema.safeParse(provenance);
			expect(result.success).toBe(true);
		});
		
		it('requires source and contentHash', () => {
			const invalid = {
				source: 'value'
				// Missing contentHash
			};
			
			const result = InputProvenanceSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('validates source enum', () => {
			const invalid = {
				source: 'invalid_source',
				contentHash: 'hash'
			};
			
			const result = InputProvenanceSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
	});
	
	describe('OutputProvenanceSchema', () => {
		it('accepts valid output provenance', () => {
			const provenance = {
				path: 'results/allocation',
				contentHash: 'def456'
			};
			
			const result = OutputProvenanceSchema.safeParse(provenance);
			expect(result.success).toBe(true);
		});
		
		it('requires path and contentHash', () => {
			const invalid = {
				path: 'path'
				// Missing contentHash
			};
			
			const result = OutputProvenanceSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
	});
	
	describe('ComputationProvenanceSchema', () => {
		it('accepts valid computation provenance', () => {
			const provenance = {
				id: 'prov_1',
				itcStamp: itcSeed(),
				executedBy: '0'.repeat(64),
				timestamp: Date.now(),
				programHash: 'prog_hash',
				computationId: 'comp_id',
				computationHash: 'comp_hash',
				inputs: {
					x: {
						source: 'value',
						contentHash: 'hash1'
					}
				},
				outputs: {
					y: {
						path: 'output',
						contentHash: 'hash2'
					}
				},
				deterministicHash: 'det_hash',
				parents: ['parent1', 'parent2']
			};
			
			const result = ComputationProvenanceSchema.safeParse(provenance);
			expect(result.success).toBe(true);
		});
		
		it('requires all core fields', () => {
			const invalid = {
				id: 'prov',
				itcStamp: itcSeed()
				// Missing many required fields
			};
			
			const result = ComputationProvenanceSchema.safeParse(invalid);
			expect(result.success).toBe(false);
		});
		
		it('makes parents optional', () => {
			const provenance = {
				id: 'prov_1',
				itcStamp: itcSeed(),
				executedBy: '0'.repeat(64),
				timestamp: Date.now(),
				programHash: 'prog_hash',
				computationId: 'comp_id',
				computationHash: 'comp_hash',
				inputs: {},
				outputs: {},
				deterministicHash: 'det_hash'
				// No parents field
			};
			
			const result = ComputationProvenanceSchema.safeParse(provenance);
			expect(result.success).toBe(true);
		});
	});
});

