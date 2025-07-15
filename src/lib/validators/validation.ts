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
	ContactsCollectionSchema,
	ChatReadStatesSchema
} from '../schema';

/**
 * Generic parser options for customizing validation behavior
 */
interface ParseOptions<T> {
	/** Schema to validate against */
	schema: z.ZodType<T>;
	/** Default value to return on validation failure */
	defaultValue: T;
	/** Function name for logging context */
	functionName: string;
	/** Whether to enable detailed logging */
	enableLogging?: boolean;
	/** Custom preprocessing function to transform data before validation */
	preprocess?: (data: unknown) => unknown;
}

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
 * Generic data parser that handles the common validation pattern
 * @param data Raw data to parse and validate
 * @param options Parsing options including schema and defaults
 * @returns Validated data or default value if validation fails
 */
function parseData<T>(data: unknown, options: ParseOptions<T>): T {
	const { schema, defaultValue, functionName, enableLogging = false, preprocess } = options;

	try {
		// 1. Handle string/object input
		let parsedData;
		if (typeof data === 'string') {
			try {
				parsedData = JSON.parse(data);
			} catch (error) {
				if (enableLogging) {
					console.error(`[VALIDATION] Error parsing ${functionName} JSON:`, error);
				}
				return defaultValue;
			}
		} else {
			parsedData = data;
		}

		// 2. Filter Gun.js metadata
		parsedData = filterGunMetadata(parsedData);

		// 3. Apply custom preprocessing if provided
		if (preprocess) {
			parsedData = preprocess(parsedData);
		}

		if (enableLogging) {
			console.log(`[VALIDATION] Pre-validation ${functionName} data:`, parsedData);
		}

		// 4. Apply Zod schema validation
		const result = schema.safeParse(parsedData);

		if (result.success) {
			if (enableLogging) {
				console.log(`[VALIDATION] ${functionName} validation successful`);
			}
			return result.data;
		} else {
			if (enableLogging) {
				console.error(`[VALIDATION] ${functionName} validation failed. Data:`, parsedData);
				console.error(`[VALIDATION] Validation errors:`, result.error.issues);
			}
			return defaultValue;
		}
	} catch (err) {
		if (enableLogging) {
			console.error(`[VALIDATION] Error during ${functionName} validation:`, err);
		}
		return defaultValue;
	}
}

/**
 * Parse and validate a tree from Gun database or other source
 * @param treeData Raw tree data (either string or object)
 * @returns Validated RootNode or null if validation fails
 */
export function parseTree(treeData: unknown) {
	return parseData(treeData, {
		schema: RootNodeSchema,
		defaultValue: null,
		functionName: 'tree'
	});
}

/**
 * Parse and validate capacities data
 * @param capacitiesData Raw capacities data
 * @returns Validated CapacitiesCollection or empty object if validation fails
 */
export function parseCapacities(capacitiesData: unknown) {
	return parseData(capacitiesData, {
		schema: CapacitiesCollectionSchema,
		defaultValue: {},
		functionName: 'capacities',
		enableLogging: true
	});
}

/**
 * Parse and validate a share map
 * @param shareMapData Raw share map data
 * @returns Validated ShareMap or empty object if validation fails
 */
export function parseShareMap(shareMapData: unknown) {
	return parseData(shareMapData, {
		schema: ShareMapSchema,
		defaultValue: {},
		functionName: 'share map',
		enableLogging: true,
		preprocess: (data) => {
			// Convert string values to numbers (common Gun.js issue)
			if (data && typeof data === 'object' && !Array.isArray(data)) {
				const convertedData: Record<string, number> = {};
				Object.entries(data).forEach(([key, value]) => {
					// Convert string numbers to actual numbers
					if (typeof value === 'string') {
						const numValue = parseFloat(value);
						if (!isNaN(numValue)) {
							convertedData[key] = numValue;
						}
					} else if (typeof value === 'number') {
						convertedData[key] = value;
					}
				});
				return convertedData;
			}
			return data;
		}
	});
}

/**
 * Parse and validate recognition cache data
 * @param cacheData Raw recognition cache data
 * @returns Validated RecognitionCache or empty object if validation fails
 */
export function parseRecognitionCache(cacheData: unknown) {
	return parseData(cacheData, {
		schema: RecognitionCacheSchema,
		defaultValue: {},
		functionName: 'recognition cache'
	});
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
	return parseData(compositionData, {
		schema: UserCompositionSchema,
		defaultValue: {},
		functionName: 'user composition',
		enableLogging: true
	});
}

/**
 * Parse and validate network composition data (from contributors)
 * @param compositionData Raw network composition data
 * @returns Validated NetworkComposition or empty object if validation fails
 */
export function parseNetworkComposition(compositionData: unknown) {
	return parseData(compositionData, {
		schema: NetworkCompositionSchema,
		defaultValue: {},
		functionName: 'network composition',
		enableLogging: true
	});
}

/**
 * Parse and validate contacts data
 * @param contactsData Raw contacts data
 * @returns Validated ContactsCollection or empty object if validation fails
 */
export function parseContacts(contactsData: unknown) {
	return parseData(contactsData, {
		schema: ContactsCollectionSchema,
		defaultValue: {},
		functionName: 'contacts',
		enableLogging: true
	});
}

/**
 * Parse and validate capacity shares data (percentage shares for capacities)
 * @param sharesData Raw capacity shares data
 * @returns Validated capacity shares or empty object if validation fails
 */
export function parseCapacityShares(sharesData: unknown) {
	return parseData(sharesData, {
		schema: ShareMapSchema,
		defaultValue: {},
		functionName: 'capacity shares',
		enableLogging: true
	});
}

/**
 * Parse and validate chat read states data
 * @param readStatesData Raw chat read states data
 * @returns Validated ChatReadStates or empty object if validation fails
 */
export function parseChatReadStates(readStatesData: unknown) {
	return parseData(readStatesData, {
		schema: ChatReadStatesSchema,
		defaultValue: {},
		functionName: 'chat read states',
		enableLogging: true
	});
}
