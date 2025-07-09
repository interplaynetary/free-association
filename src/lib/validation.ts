/**
 * Validation utilities for the Free Association Protocol
 */
import { z } from 'zod/v4';
import {
	RootNodeSchema,
	NodeSchema,
	CapacitySchema,
	CapacitiesCollectionSchema,
	ShareMapSchema,
	RecognitionCacheSchema,
	UserCompositionSchema,
	NetworkCompositionSchema,
	ContactsCollectionSchema
} from './schema';

/*
GENERAL PATTERN:

  export function parseX(data: unknown) {
    // 1. Handle string/object input
    // 2. Filter Gun.js metadata
    // 3. Apply Zod schema validation
    // 4. Return validated data or safe fallback
    // 5. Log validation errors
  }

*/

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
				console.error('[VALIDATION] Error parsing capacities data JSON:', error);
				return {};
			}
		} else {
			parsedData = capacitiesData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation capacities data:', parsedData);

		// Validate with Zod schema
		const result = CapacitiesCollectionSchema.safeParse(parsedData);

		if (result.success) {
			console.log('[VALIDATION] Capacities validation successful');
			// Check if validation changed any values
			const differences = findDifferences(parsedData, result.data);
			if (Object.keys(differences).length > 0) {
				console.log('[VALIDATION] Validation made the following changes:', differences);
			} else {
				console.log('[VALIDATION] Validation made no changes to data');
			}

			function findDifferences(
				original: any,
				validated: any,
				path: string[] = []
			): Record<string, { original: any; validated: any }> {
				const diffs: Record<string, { original: any; validated: any }> = {};

				if (typeof original !== typeof validated) {
					diffs[path.join('.')] = { original, validated };
					return diffs;
				}

				if (typeof original === 'object' && original !== null) {
					Object.keys({ ...original, ...validated }).forEach((key) => {
						const newDiffs = findDifferences(original[key], validated[key], [...path, key]);
						Object.assign(diffs, newDiffs);
					});
				} else if (original !== validated) {
					diffs[path.join('.')] = { original, validated };
				}

				return diffs;
			}
			console.log('[VALIDATION] Validated Capacities data:', result.data);
			return result.data;
		} else {
			console.error('[VALIDATION] Capacities validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			console.error(
				'[VALIDATION] Validation error paths:',
				result.error.issues.map((issue) => issue.path)
			);
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error during capacities validation:', err);
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
		console.log('[VALIDATION] Raw share map input:', shareMapData);
		console.log('[VALIDATION] Share map input type:', typeof shareMapData);
		
		// Parse if it's a string
		let parsedData;
		if (typeof shareMapData === 'string') {
			try {
				parsedData = JSON.parse(shareMapData);
				console.log('[VALIDATION] Parsed from JSON string:', parsedData);
			} catch (error) {
				console.error('[VALIDATION] Error parsing share map JSON:', error);
				return {};
			}
		} else {
			parsedData = shareMapData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);
		console.log('[VALIDATION] After filtering Gun metadata:', parsedData);

		// Convert string values to numbers (common Gun.js issue)
		if (parsedData && typeof parsedData === 'object' && !Array.isArray(parsedData)) {
			const convertedData: Record<string, number> = {};
			Object.entries(parsedData).forEach(([key, value]) => {
				console.log(`[VALIDATION] Processing key: ${key}, value: ${value}, type: ${typeof value}`);
				
				// Convert string numbers to actual numbers
				if (typeof value === 'string') {
					const numValue = parseFloat(value);
					console.log(`[VALIDATION] String "${value}" converted to: ${numValue}, isNaN: ${isNaN(numValue)}`);
					
					if (!isNaN(numValue)) {
						convertedData[key] = numValue;
						console.log(`[VALIDATION] Successfully converted ${key}: "${value}" -> ${numValue}`);
					} else {
						console.warn(`[VALIDATION] Invalid share value for ${key}: ${value}`);
					}
				} else if (typeof value === 'number') {
					convertedData[key] = value;
					console.log(`[VALIDATION] Number value ${key}: ${value}`);
				} else {
					console.warn(`[VALIDATION] Invalid share type for ${key}:`, typeof value, value);
				}
			});
			parsedData = convertedData;
			console.log('[VALIDATION] Final converted data:', convertedData);
		}

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation share map data:', parsedData);

		// Validate with Zod schema
		const result = ShareMapSchema.safeParse(parsedData);

		if (result.success) {
			console.log('[VALIDATION] Share map validation successful');
			return result.data;
		} else {
			console.error('[VALIDATION] Share map validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			console.error('[VALIDATION] Detailed error breakdown:');
			result.error.issues.forEach(issue => {
				console.error(issue);
			});
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error parsing share map:', err);
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
export function getValidationErrors(schema: z.ZodType, data: unknown): string | null {
	const result = schema.safeParse(data);
	if (!result.success) {
		return JSON.stringify(result.error, null, 2);
	}
	return null;
}

/**
 * Parse and validate user composition data (desiredComposeFrom/desiredComposeInto)
 * @param compositionData Raw composition data
 * @returns Validated UserComposition or empty object if validation fails
 */
export function parseUserComposition(compositionData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof compositionData === 'string') {
			try {
				parsedData = JSON.parse(compositionData);
			} catch (error) {
				console.error('[VALIDATION] Error parsing user composition data JSON:', error);
				return {};
			}
		} else {
			parsedData = compositionData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation user composition data:', parsedData);

		// Validate with Zod schema
		const result = UserCompositionSchema.safeParse(parsedData);

		if (result.success) {
			console.log('[VALIDATION] User composition validation successful');
			return result.data;
		} else {
			console.error('[VALIDATION] User composition validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error during user composition validation:', err);
		return {};
	}
}

/**
 * Parse and validate network composition data (from contributors)
 * @param compositionData Raw network composition data
 * @returns Validated NetworkComposition or empty object if validation fails
 */
export function parseNetworkComposition(compositionData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof compositionData === 'string') {
			try {
				parsedData = JSON.parse(compositionData);
			} catch (error) {
				console.error('[VALIDATION] Error parsing network composition data JSON:', error);
				return {};
			}
		} else {
			parsedData = compositionData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation network composition data:', parsedData);

		// Validate with Zod schema
		const result = NetworkCompositionSchema.safeParse(parsedData);

		if (result.success) {
			console.log('[VALIDATION] Network composition validation successful');
			return result.data;
		} else {
			console.error('[VALIDATION] Network composition validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error during network composition validation:', err);
		return {};
	}
}

/**
 * Parse and validate contacts data
 * @param contactsData Raw contacts data
 * @returns Validated ContactsCollection or empty object if validation fails
 */
export function parseContacts(contactsData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof contactsData === 'string') {
			try {
				parsedData = JSON.parse(contactsData);
			} catch (error) {
				console.error('[VALIDATION] Error parsing contacts data JSON:', error);
				return {};
			}
		} else {
			parsedData = contactsData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation contacts data:', parsedData);

		// Validate with Zod schema
		const result = ContactsCollectionSchema.safeParse(parsedData);

		if (result.success) {
			console.log('[VALIDATION] Contacts validation successful');
			return result.data;
		} else {
			console.error('[VALIDATION] Contacts validation failed. Data:', parsedData);
			console.error('[VALIDATION] Validation errors:', result.error.issues);
			return {};
		}
	} catch (err) {
		console.error('[VALIDATION] Error during contacts validation:', err);
		return {};
	}
}

/**
 * Parse and validate capacity shares data (percentage shares for capacities)
 * @param sharesData Raw capacity shares data
 * @returns Validated capacity shares or empty object if validation fails
 */
export function parseCapacityShares(sharesData: unknown) {
	try {
		// Parse if it's a string
		let parsedData;
		if (typeof sharesData === 'string') {
			try {
				parsedData = JSON.parse(sharesData);
			} catch (error) {
				console.error('[VALIDATION] Error parsing capacity shares data JSON:', error);
				return {};
			}
		} else {
			parsedData = sharesData;
		}

		// Filter out Gun.js metadata properties
		parsedData = filterGunMetadata(parsedData);

		// Log the data being validated
		console.log('[VALIDATION] Pre-validation capacity shares data:', parsedData);

		// Validate that it's an object
		if (!parsedData || typeof parsedData !== 'object') {
			console.error('[VALIDATION] Capacity shares data is not an object:', parsedData);
			return {};
		}

		// Validate each capacity share entry
		const validatedShares: Record<string, number> = {};
		Object.entries(parsedData).forEach(([capacityId, share]) => {
			// Validate capacity ID (must be non-empty string)
			if (typeof capacityId !== 'string' || capacityId.trim() === '') {
				console.warn('[VALIDATION] Invalid capacity ID in shares:', capacityId);
				return;
			}

			// Validate share (must be number between 0 and 1)
			if (typeof share === 'number' && share >= 0 && share <= 1 && !isNaN(share)) {
				validatedShares[capacityId] = share;
			} else {
				console.warn('[VALIDATION] Invalid share value for capacity', capacityId, ':', share);
			}
		});

		console.log('[VALIDATION] Capacity shares validation successful');
		return validatedShares;
	} catch (err) {
		console.error('[VALIDATION] Error during capacity shares validation:', err);
		return {};
	}
}
