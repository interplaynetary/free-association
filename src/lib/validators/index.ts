/**
 * Validators for Free Association Protocol
 *
 * This file exports all schema and validation functions.
 */

// Export all schemas
export * from '../schema';

// Export all validation functions
export * from './validation';

// Export convenience functions for common validations
import { parseTree, parseCapacities, parseShareMap, parseRecognitionCache } from './validation';

/**
 * Validate and parse JSON data using the appropriate validator
 * @param data The data to parse and validate
 * @param type The type of data to validate
 * @returns Validated data or null/empty object if invalid
 */
export function validateData(
	data: unknown,
	type: 'tree' | 'capacities' | 'shareMap' | 'recognitionCache'
) {
	switch (type) {
		case 'tree':
			return parseTree(data);
		case 'capacities':
			return parseCapacities(data);
		case 'shareMap':
			return parseShareMap(data);
		case 'recognitionCache':
			return parseRecognitionCache(data);
		default:
			throw new Error(`Unknown validation type: ${type}`);
	}
}
