/**
 * RDL Validator - Validates Reactive Dataflow Language programs
 * 
 * Provides:
 * - JSON Schema validation
 * - Zod schema validation
 * - Semantic validation (cycles, references, etc.)
 * - Helpful error messages
 */

import type { ReactiveComputationGraph, Computation, VariableBinding } from './v1/schemas';
import {
	ReactiveComputationGraphSchema,
	ComputationSchema,
	VariableBindingSchema
} from './v1/schemas';
import { getSchema, hasSchemaType } from './node-store.svelte';
import { getComputationFunction, listComputationFunctions } from './compute.svelte';

// ═══════════════════════════════════════════════════════════════════
// VALIDATION RESULT TYPES
// ═══════════════════════════════════════════════════════════════════

export interface ValidationError {
	code: string;
	message: string;
	path?: string;
	severity: 'error' | 'warning';
}

export interface ValidationResult {
	valid: boolean;
	errors: ValidationError[];
	warnings: ValidationError[];
}

// ═══════════════════════════════════════════════════════════════════
// SCHEMA VALIDATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate RDL program against Zod schema
 */
export function validateSchema(program: unknown): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	const result = ReactiveComputationGraphSchema.safeParse(program);
	
	if (!result.success) {
		for (const issue of result.error.issues) {
			errors.push({
				code: 'SCHEMA_VALIDATION_ERROR',
				message: issue.message,
				path: issue.path.join('.'),
				severity: 'error'
			});
		}
	}
	
	return {
		valid: result.success,
		errors,
		warnings
	};
}

// ═══════════════════════════════════════════════════════════════════
// SEMANTIC VALIDATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate computation graph semantics
 */
export function validateSemantics(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	// Check for circular dependencies
	const cycleResult = detectCycles(graph);
	if (!cycleResult.valid) {
		errors.push(...cycleResult.errors);
	}
	
	// Check schema type references
	const schemaResult = validateSchemaReferences(graph);
	errors.push(...schemaResult.errors);
	warnings.push(...schemaResult.warnings);
	
	// Check function references
	const functionResult = validateFunctionReferences(graph);
	errors.push(...functionResult.errors);
	warnings.push(...functionResult.warnings);
	
	// Check derived references
	const derivedResult = validateDerivedReferences(graph);
	errors.push(...derivedResult.errors);
	
	// Check dependency references
	const dependencyResult = validateDependencyReferences(graph);
	errors.push(...dependencyResult.errors);
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

/**
 * Detect circular dependencies in computation graph
 */
function detectCycles(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	const visited = new Set<string>();
	const visiting = new Set<string>();
	const computationMap = new Map(graph.computations.map(c => [c.id, c]));
	
	function visit(comp: Computation, path: string[] = []): boolean {
		if (visited.has(comp.id)) return false;
		if (visiting.has(comp.id)) {
			// Found cycle
			const cyclePath = [...path, comp.id].join(' → ');
			errors.push({
				code: 'CIRCULAR_DEPENDENCY',
				message: `Circular dependency detected: ${cyclePath}`,
				path: `computations.${comp.id}`,
				severity: 'error'
			});
			return true;
		}
		
		visiting.add(comp.id);
		
		// Check explicit dependencies
		if (comp.depends_on) {
			for (const depId of comp.depends_on) {
				const depComp = computationMap.get(depId);
				if (depComp && visit(depComp, [...path, comp.id])) {
					return true;
				}
			}
		}
		
		// Check implicit dependencies (derived bindings)
		for (const binding of Object.values(comp.inputs)) {
			if (binding.type === 'derived') {
				const depComp = computationMap.get(binding.computation_id);
				if (depComp && visit(depComp, [...path, comp.id])) {
					return true;
				}
			}
		}
		
		visiting.delete(comp.id);
		visited.add(comp.id);
		return false;
	}
	
	for (const comp of graph.computations) {
		if (visit(comp)) {
			break; // Stop after first cycle detected
		}
	}
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

/**
 * Validate schema type references
 */
function validateSchemaReferences(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	// Check variables
	for (const [varName, binding] of Object.entries(graph.variables)) {
		if (binding.type === 'subscription') {
			if (!hasSchemaType(binding.schema_type)) {
				errors.push({
					code: 'UNKNOWN_SCHEMA_TYPE',
					message: `Unknown schema type: ${binding.schema_type}`,
					path: `variables.${varName}.schema_type`,
					severity: 'error'
				});
			}
		}
	}
	
	// Check computation inputs
	for (const comp of graph.computations) {
		for (const [inputName, binding] of Object.entries(comp.inputs)) {
			if (binding.type === 'subscription') {
				if (!hasSchemaType(binding.schema_type)) {
					errors.push({
						code: 'UNKNOWN_SCHEMA_TYPE',
						message: `Unknown schema type: ${binding.schema_type}`,
						path: `computations.${comp.id}.inputs.${inputName}.schema_type`,
						severity: 'error'
					});
				}
			}
		}
		
		// Check outputs
		for (const [outputName, binding] of Object.entries(comp.outputs)) {
			if (binding.type === 'holster' && binding.schema_type) {
				if (!hasSchemaType(binding.schema_type)) {
					warnings.push({
						code: 'UNKNOWN_SCHEMA_TYPE',
						message: `Unknown schema type: ${binding.schema_type} (will use 'Any')`,
						path: `computations.${comp.id}.outputs.${outputName}.schema_type`,
						severity: 'warning'
					});
				}
			}
		}
	}
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

/**
 * Validate function references
 */
function validateFunctionReferences(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	const registeredFunctions = new Set(listComputationFunctions());
	
	for (const comp of graph.computations) {
		if (!registeredFunctions.has(comp.compute_fn)) {
			errors.push({
				code: 'UNKNOWN_FUNCTION',
				message: `Unknown computation function: ${comp.compute_fn}. Register it with registerComputationFunction().`,
				path: `computations.${comp.id}.compute_fn`,
				severity: 'error'
			});
		}
	}
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

/**
 * Validate derived binding references
 */
function validateDerivedReferences(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	const computationIds = new Set(graph.computations.map(c => c.id));
	const computationOutputs = new Map(
		graph.computations.map(c => [c.id, new Set(Object.keys(c.outputs))])
	);
	
	for (const comp of graph.computations) {
		for (const [inputName, binding] of Object.entries(comp.inputs)) {
			if (binding.type === 'derived') {
				// Check if referenced computation exists
				if (!computationIds.has(binding.computation_id)) {
					errors.push({
						code: 'UNKNOWN_COMPUTATION_ID',
						message: `Referenced computation does not exist: ${binding.computation_id}`,
						path: `computations.${comp.id}.inputs.${inputName}.computation_id`,
						severity: 'error'
					});
				} else {
					// Check if output key exists
					const outputs = computationOutputs.get(binding.computation_id);
					if (outputs && !outputs.has(binding.output_key)) {
						errors.push({
							code: 'UNKNOWN_OUTPUT_KEY',
							message: `Computation ${binding.computation_id} does not have output: ${binding.output_key}`,
							path: `computations.${comp.id}.inputs.${inputName}.output_key`,
							severity: 'error'
						});
					}
				}
			}
		}
	}
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

/**
 * Validate depends_on references
 */
function validateDependencyReferences(graph: ReactiveComputationGraph): ValidationResult {
	const errors: ValidationError[] = [];
	const warnings: ValidationError[] = [];
	
	const computationIds = new Set(graph.computations.map(c => c.id));
	
	for (const comp of graph.computations) {
		if (comp.depends_on) {
			for (const depId of comp.depends_on) {
				if (!computationIds.has(depId)) {
					errors.push({
						code: 'UNKNOWN_DEPENDENCY',
						message: `Referenced dependency does not exist: ${depId}`,
						path: `computations.${comp.id}.depends_on`,
						severity: 'error'
					});
				}
			}
		}
	}
	
	return {
		valid: errors.length === 0,
		errors,
		warnings
	};
}

// ═══════════════════════════════════════════════════════════════════
// COMPLETE VALIDATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Complete validation (schema + semantics)
 */
export function validateRDL(program: unknown): ValidationResult {
	// First validate schema
	const schemaResult = validateSchema(program);
	if (!schemaResult.valid) {
		return schemaResult;
	}
	
	// Then validate semantics
	const semanticResult = validateSemantics(program as ReactiveComputationGraph);
	
	return {
		valid: semanticResult.valid,
		errors: [...schemaResult.errors, ...semanticResult.errors],
		warnings: [...schemaResult.warnings, ...semanticResult.warnings]
	};
}

/**
 * Validate and throw on error
 */
export function validateRDLOrThrow(program: unknown): ReactiveComputationGraph {
	const result = validateRDL(program);
	
	if (!result.valid) {
		const errorMessages = result.errors.map(
			e => `[${e.code}] ${e.path ? e.path + ': ' : ''}${e.message}`
		).join('\n');
		
		throw new Error(`RDL Validation Failed:\n${errorMessages}`);
	}
	
	// Print warnings
	if (result.warnings.length > 0) {
		const warningMessages = result.warnings.map(
			w => `[${w.code}] ${w.path ? w.path + ': ' : ''}${w.message}`
		).join('\n');
		
		console.warn(`RDL Validation Warnings:\n${warningMessages}`);
	}
	
	return program as ReactiveComputationGraph;
}

/**
 * Pretty-print validation result
 */
export function formatValidationResult(result: ValidationResult): string {
	const lines: string[] = [];
	
	if (result.valid) {
		lines.push('✓ RDL program is valid');
	} else {
		lines.push('✗ RDL program is invalid');
	}
	
	if (result.errors.length > 0) {
		lines.push('');
		lines.push('Errors:');
		for (const error of result.errors) {
			lines.push(`  [${error.code}] ${error.path ? error.path + ': ' : ''}${error.message}`);
		}
	}
	
	if (result.warnings.length > 0) {
		lines.push('');
		lines.push('Warnings:');
		for (const warning of result.warnings) {
			lines.push(`  [${warning.code}] ${warning.path ? warning.path + ': ' : ''}${warning.message}`);
		}
	}
	
	return lines.join('\n');
}

