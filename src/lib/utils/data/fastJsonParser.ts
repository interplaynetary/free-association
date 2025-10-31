/**
 * Fast JSON Parser
 * 
 * Efficient JSON parsing using fast-json library for streaming/selective parsing.
 * Falls back to standard JSON.parse for simple cases.
 * 
 * Use cases:
 * - Large JSON payloads where you only need specific fields
 * - Network messages where you want to extract IDs/metadata before full parsing
 * - Store persistence where you want to check timestamps before full deserialization
 */

import { FastJson } from 'fast-json';

/**
 * Standard synchronous parse - use for simple cases
 */
export function fastParse<T = any>(jsonString: string): T {
	return JSON.parse(jsonString);
}

/**
 * Parse with selective field extraction
 * Useful for large objects where you only need specific fields
 * 
 * @example
 * const result = await fastParseSelective(jsonString, {
 *   '_updatedAt': (value) => console.log('Timestamp:', value),
 *   'need_slots[0].name': (value) => console.log('First slot:', value)
 * });
 */
export async function fastParseSelective(
	jsonString: string,
	paths: Record<string, (value: any) => void>
): Promise<void> {
	const parser = new FastJson();
	
	// Register all path listeners
	for (const [path, callback] of Object.entries(paths)) {
		parser.on(path, callback);
	}
	
	// Parse the JSON
	parser.write(jsonString);
}

/**
 * Parse and extract specific fields before full parsing
 * Returns both extracted values and full parsed object
 * 
 * Useful for checking metadata (like timestamps) before deciding whether
 * to do expensive validation/processing on the full object
 * 
 * @example
 * const { extracted, full } = await fastParseWithExtraction(jsonString, ['_updatedAt', 'id']);
 * if (extracted._updatedAt > lastSeen) {
 *   // Process the full object
 *   processData(full);
 * }
 */
export async function fastParseWithExtraction<T = any>(
	jsonString: string,
	extractPaths: string[]
): Promise<{ extracted: Record<string, any>; full: T }> {
	const extracted: Record<string, any> = {};
	const parser = new FastJson();
	
	// Set up listeners for paths we want to extract
	for (const path of extractPaths) {
		parser.on(path, (value: any) => {
			extracted[path] = value;
		});
	}
	
	// Parse
	parser.write(jsonString);
	
	// Also do full parse
	const full = JSON.parse(jsonString) as T;
	
	return { extracted, full };
}

/**
 * Parse large arrays efficiently with early termination
 * Useful for finding specific items without parsing entire array
 * 
 * @example
 * // Find first slot with specific ID, skip rest
 * const found = await fastParseArrayFind(
 *   jsonString,
 *   'need_slots[*].id',
 *   (value) => value === targetId
 * );
 */
export async function fastParseArrayFind(
	jsonString: string,
	arrayPath: string,
	predicate: (item: any) => boolean
): Promise<any | null> {
	let found: any = null;
	const parser = new FastJson();
	
	parser.on(arrayPath, (value: any) => {
		if (predicate(value)) {
			found = value;
			// Skip the rest of the JSON for performance
			parser.skip();
		}
	});
	
	parser.write(jsonString);
	return found;
}

/**
 * Check if JSON contains specific fields without full parsing
 * Useful for validation/type checking before expensive operations
 * 
 * @example
 * const hasRequired = await fastParseHasFields(jsonString, ['id', '_updatedAt', 'need_slots']);
 * if (hasRequired) {
 *   // Safe to proceed with full parsing
 * }
 */
export async function fastParseHasFields(
	jsonString: string,
	requiredFields: string[]
): Promise<boolean> {
	const foundFields = new Set<string>();
	const parser = new FastJson();
	
	for (const field of requiredFields) {
		parser.on(field, () => {
			foundFields.add(field);
			// Skip if we found everything
			if (foundFields.size === requiredFields.length) {
				parser.skip();
			}
		});
	}
	
	parser.write(jsonString);
	return foundFields.size === requiredFields.length;
}

/**
 * Parse network message with early ID extraction
 * Common pattern: extract tracking ID before processing full message
 * 
 * @example
 * const { trackId, message } = await fastParseNetworkMessage(jsonString);
 * console.log('Message ID:', trackId);
 * processMessage(message);
 */
export async function fastParseNetworkMessage<T = any>(
	jsonString: string
): Promise<{ trackId?: string; message: T }> {
	let trackId: string | undefined;
	const parser = new FastJson();
	
	// Extract tracking ID commonly used in wire protocol
	parser.on('#', (value: any) => {
		trackId = value;
	});
	
	parser.write(jsonString);
	
	// Full parse for the message
	const message = JSON.parse(jsonString) as T;
	
	return { trackId, message };
}

/**
 * Validate JSON structure before full parse
 * Checks for required top-level fields efficiently
 * 
 * @example
 * const isValid = await fastValidateStructure(jsonString, {
 *   requiredFields: ['id', 'type'],
 *   optionalFields: ['metadata']
 * });
 */
export async function fastValidateStructure(
	jsonString: string,
	options: {
		requiredFields: string[];
		optionalFields?: string[];
		maxDepth?: number;
	}
): Promise<{ valid: boolean; missing?: string[] }> {
	const found = new Set<string>();
	const parser = new FastJson();
	
	const allFields = [
		...options.requiredFields,
		...(options.optionalFields || [])
	];
	
	for (const field of allFields) {
		parser.on(field, () => {
			found.add(field);
		});
	}
	
	parser.write(jsonString);
	
	const missing = options.requiredFields.filter(f => !found.has(f));
	
	return {
		valid: missing.length === 0,
		missing: missing.length > 0 ? missing : undefined
	};
}

/**
 * Extract timestamp from JSON without full parsing
 * Common optimization for checking if data needs update
 * 
 * @example
 * const timestamp = await fastExtractTimestamp(jsonString);
 * if (timestamp > lastSeen) {
 *   const full = JSON.parse(jsonString);
 *   processUpdate(full);
 * }
 */
export async function fastExtractTimestamp(
	jsonString: string,
	timestampField: string = '_updatedAt'
): Promise<number | null> {
	let timestamp: number | null = null;
	const parser = new FastJson();
	
	parser.on(timestampField, (value: any) => {
		timestamp = typeof value === 'number' ? value : null;
		parser.skip(); // We got what we need
	});
	
	parser.write(jsonString);
	return timestamp;
}

/**
 * Parse with size limit check
 * Prevents parsing overly large JSON that could cause performance issues
 * 
 * @example
 * const result = await fastParseSafe(jsonString, 1024 * 1024); // 1MB limit
 * if (result.error) {
 *   console.error('JSON too large or invalid:', result.error);
 * } else {
 *   processData(result.data);
 * }
 */
export async function fastParseSafe<T = any>(
	jsonString: string,
	maxSize: number = 10 * 1024 * 1024 // 10MB default
): Promise<{ success: boolean; data?: T; error?: string }> {
	try {
		if (jsonString.length > maxSize) {
			return {
				success: false,
				error: `JSON too large: ${jsonString.length} bytes (max: ${maxSize})`
			};
		}
		
		const data = JSON.parse(jsonString) as T;
		return { success: true, data };
	} catch (error: any) {
		return {
			success: false,
			error: error.message || 'Parse error'
		};
	}
}

/**
 * Batch parse multiple JSON strings efficiently
 * 
 * @example
 * const results = await fastParseBatch([json1, json2, json3]);
 * results.forEach((result, index) => {
 *   if (result.success) {
 *     processData(result.data);
 *   }
 * });
 */
export async function fastParseBatch<T = any>(
	jsonStrings: string[]
): Promise<Array<{ success: boolean; data?: T; error?: string }>> {
	return Promise.all(
		jsonStrings.map(async (json) => {
			try {
				const data = JSON.parse(json) as T;
				return { success: true, data };
			} catch (error: any) {
				return { success: false, error: error.message };
			}
		})
	);
}

