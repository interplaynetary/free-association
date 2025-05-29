/**
 * Validation utilities for the Free Association Protocol
 */
import { z } from 'zod/v4-mini';
import {
	RootNodeSchema,
	NodeSchema,
	CapacitySchema,
	CapacitiesCollectionSchema,
	ShareMapSchema,
	RecognitionCacheSchema
} from './schema';

/**
 * Filter out Gun.js metadata properties from an object
 * @param data Object to filter
 * @returns Filtered object without Gun.js metadata
 */
function filterGunMetadata(data: unknown): unknown {
	if (!data || typeof data !== 'object' || Array.isArray(data)) {
		return data;
	}

	const filteredData: Record<string, any> = {};
	for (const [key, value] of Object.entries(data)) {
		// Filter out Gun.js metadata properties (keys starting with "_")
		if (!key.startsWith('_')) {
			filteredData[key] = value;
		}
	}
	return filteredData;
}

/**
 * Parse and validate a tree from Gun database or other source
 * @param treeData Raw tree data (either string or object)
 * @returns Validated RootNode or null if validation fails
 */
export function parseTree(treeData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof treeData === 'string') {
			try {
				parsedData = JSON.parse(treeData);
			} catch (error) {
				console.error('Error parsing tree data JSON:', error);
				return null;
			}
		} else {
			parsedData = treeData;
		}

		// Validate with Zod schema
		const result = RootNodeSchema.safeParse(parsedData);

		if (result.success) {
			return result.data;
		} else {
			console.error('Tree data validation failed:', result.error);
			return null;
		}
	} catch (err) {
		console.error('Error parsing tree data:', err);
		return null;
	}
}

/**
 * Parse and validate capacities data
 * @param capacitiesData Raw capacities data
 * @returns Validated CapacitiesCollection or empty object if validation fails
 */
export function parseCapacities(capacitiesData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof capacitiesData === 'string') {
			try {
				parsedData = JSON.parse(capacitiesData);
			} catch (error) {
				console.error('Error parsing capacities data JSON:', error);
				return {};
			}
		} else {
			parsedData = capacitiesData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Validating capacities data:', parsedData);

		// Validate with Zod schema
		const result = CapacitiesCollectionSchema.safeParse(parsedData);

		if (result.success) {
			return result.data;
		} else {
			console.error('[VALIDATION] Capacities data validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error parsing capacities data:', err);
		return {};
	}
}

/**
 * Parse and validate a share map
 * @param shareMapData Raw share map data
 * @returns Validated ShareMap or empty object if validation fails
 */
export function parseShareMap(shareMapData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof shareMapData === 'string') {
			try {
				parsedData = JSON.parse(shareMapData);
			} catch (error) {
				console.error('Error parsing share map JSON:', error);
				return {};
			}
		} else {
			parsedData = shareMapData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Validate with Zod schema
		const result = ShareMapSchema.safeParse(parsedData);

		if (result.success) {
			return result.data;
		} else {
			console.error('Share map validation failed:', result.error);
			return {};
		}
	} catch (err) {
		console.error('Error parsing share map:', err);
		return {};
	}
}

/**
 * Parse and validate recognition cache data
 * @param cacheData Raw recognition cache data
 * @returns Validated RecognitionCache or empty object if validation fails
 */
export function parseRecognitionCache(cacheData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof cacheData === 'string') {
			try {
				parsedData = JSON.parse(cacheData);
			} catch (error) {
				console.error('Error parsing recognition cache JSON:', error);
				return {};
			}
		} else {
			parsedData = cacheData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Validate with Zod schema
		const result = RecognitionCacheSchema.safeParse(parsedData);

		if (result.success) {
			return result.data;
		} else {
			console.error('Recognition cache validation failed:', result.error);
			return {};
		}
	} catch (err) {
		console.error('Error parsing recognition cache:', err);
		return {};
	}
}

/**
 * Validate a single node
 * @param node Node to validate
 * @returns Boolean indicating if node is valid
 */
export function isValidNode(node: unknown): boolean {
	const result = NodeSchema.safeParse(node);
	return result.success;
}

/**
 * Validate a capacity
 * @param capacity Capacity to validate
 * @returns Boolean indicating if capacity is valid
 */
export function isValidCapacity(capacity: unknown): boolean {
	const result = CapacitySchema.safeParse(capacity);
	return result.success;
}

/**
 * Get validation errors for debugging
 * @param schema Zod schema to use
 * @param data Data to validate
 * @returns Error message or null if valid
 */
export function getValidationErrors(schema: z.ZodMiniType, data: unknown): string | null {
	const result = schema.safeParse(data);
	if (!result.success) {
		return JSON.stringify(result.error, null, 2);
	}
	return null;
}
