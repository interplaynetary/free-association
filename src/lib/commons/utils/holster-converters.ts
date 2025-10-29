/**
 * Holster Format Converters
 * 
 * Holster (built on Gun) is a graph database that cannot persist arrays.
 * Arrays must be converted to Records for persistence.
 * 
 * This module provides bidirectional conversion at the persistence boundary:
 * - toHolsterFormat(): Internal (arrays) → Holster (Records) 
 * - fromHolsterFormat(): Holster (Records) → Internal (arrays)
 * 
 * Usage:
 * ```typescript
 * const store = createStore({
 *   schema: CommitmentSchema,
 *   toHolsterFormat: toHolsterFormat,
 *   fromHolsterFormat: fromHolsterFormat
 * });
 * ```
 */

// ═══════════════════════════════════════════════════════════════════
// TYPE DETECTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if value is a plain array (not a typed array, Map, Set, etc.)
 */
function isPlainArray(value: any): value is any[] {
	return Array.isArray(value);
}

/**
 * Check if value is a plain object (not Date, RegExp, null, etc.)
 */
function isPlainObject(value: any): value is Record<string, any> {
	return value !== null && 
	       typeof value === 'object' && 
	       !Array.isArray(value) &&
	       !(value instanceof Date) &&
	       !(value instanceof RegExp) &&
	       !(value instanceof Map) &&
	       !(value instanceof Set);
}

/**
 * Check if an object looks like an array converted to Record
 * (has numeric string keys: "0", "1", "2", ...)
 */
function looksLikeArrayRecord(obj: Record<string, any>): boolean {
	const keys = Object.keys(obj);
	if (keys.length === 0) return false;
	
	// Check if all keys are numeric strings
	const allNumeric = keys.every(k => /^\d+$/.test(k));
	if (!allNumeric) return false;
	
	// Check if keys form a continuous sequence starting from 0
	const numbers = keys.map(k => parseInt(k, 10)).sort((a, b) => a - b);
	for (let i = 0; i < numbers.length; i++) {
		if (numbers[i] !== i) return false;
	}
	
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// ARRAY → RECORD CONVERSION (for Holster persistence)
// ═══════════════════════════════════════════════════════════════════

/**
 * Convert arrays to Records for Holster compatibility
 * 
 * Strategy:
 * - Arrays with objects that have 'id' field → Record keyed by id
 * - Other arrays → Record with numeric string keys ("0", "1", "2", ...)
 * - Empty arrays → null (Holster/Gun doesn't like empty objects)
 * - undefined values → removed (Holster/Gun rejects undefined)
 * - Recursively process nested structures
 * 
 * Examples:
 * ```typescript
 * // Array with id field
 * [{ id: 'a', name: 'Alice' }, { id: 'b', name: 'Bob' }]
 * → { 'a': { id: 'a', name: 'Alice' }, 'b': { id: 'b', name: 'Bob' } }
 * 
 * // Array without id field
 * ['red', 'green', 'blue']
 * → { '0': 'red', '1': 'green', '2': 'blue' }
 * 
 * // Empty array → null (Holster compatible)
 * [] → null
 * 
 * // Nested structure
 * { slots: [{ id: 's1', times: ['09:00', '10:00'] }] }
 * → { slots: { 's1': { id: 's1', times: { '0': '09:00', '1': '10:00' } } } }
 * ```
 */
export function toHolsterFormat(data: any): any {
	// Primitives pass through
	if (data === null || data === undefined) return null; // Holster: convert undefined to null
	if (typeof data !== 'object') return data;
	
	// Handle arrays
	if (isPlainArray(data)) {
		// Empty array → null (Holster/Gun doesn't like {})
		if (data.length === 0) return null;
		
		// Check if array elements have 'id' field (common pattern)
		const hasIdField = data.every(item => 
			isPlainObject(item) && typeof item.id === 'string'
		);
		
		if (hasIdField) {
			// Convert to Record keyed by id
			const record: Record<string, any> = {};
			for (const item of data) {
				const converted = toHolsterFormat(item);
				if (converted !== null) { // Skip null entries
					record[item.id] = converted;
				}
			}
			return Object.keys(record).length > 0 ? record : null;
		} else {
			// Convert to Record with numeric keys
			const record: Record<string, any> = {};
			for (let i = 0; i < data.length; i++) {
				const converted = toHolsterFormat(data[i]);
				if (converted !== null) { // Skip null entries
					record[String(i)] = converted;
				}
			}
			return Object.keys(record).length > 0 ? record : null;
		}
	}
	
	// Handle plain objects (recursively process values)
	if (isPlainObject(data)) {
		const result: Record<string, any> = {};
		for (const key in data) {
			const value = data[key];
			
			// Skip undefined values (Holster/Gun rejects them)
			if (value === undefined) continue;
			
			const converted = toHolsterFormat(value);
			
			// Skip null/empty results for optional fields
			if (converted !== null) {
				result[key] = converted;
			}
		}
		
		// Return null if object is empty after conversion (Holster compatible)
		return Object.keys(result).length > 0 ? result : null;
	}
	
	// Other types (Date, RegExp, etc.) pass through
	return data;
}

// ═══════════════════════════════════════════════════════════════════
// RECORD → ARRAY CONVERSION (from Holster data)
// ═══════════════════════════════════════════════════════════════════

/**
 * Normalize enum values to lowercase and map legacy values (fix old data)
 * Handles deeply nested values (in arrays, objects, etc.)
 * 
 * Legacy value mappings:
 * - 'Bi-weekly', 'Weekends', 'Weekdays' → 'weekly'
 * - 'Daily' → 'daily'
 * - 'Weekly' → 'weekly'
 * - 'Monthly' → 'monthly'
 * - 'Yearly' → 'yearly'
 * - 'Does not repeat', null, undefined → null
 */
function normalizeSlotEnums(obj: any): any {
	if (obj === null || obj === undefined) return obj;
	
	// Primitives
	if (typeof obj !== 'object') return obj;
	
	// Arrays - recursively normalize each item
	if (Array.isArray(obj)) {
		return obj.map(item => normalizeSlotEnums(item));
	}
	
	// Objects - recursively normalize each property
	if (isPlainObject(obj)) {
		const result: any = {};
		for (const key in obj) {
			let value = obj[key];
			
			// Normalize recurrence enum
			if (key === 'recurrence' && typeof value === 'string') {
				const normalized = normalizeRecurrenceValue(value);
				value = normalized;
			}
			
			// Recursively normalize nested structures
			result[key] = normalizeSlotEnums(value);
		}
		return result;
	}
	
	return obj;
}

/**
 * Normalize a single recurrence value (maps legacy values to schema-valid values)
 */
function normalizeRecurrenceValue(value: string): string | null {
	if (!value) return null;
	
	// Handle special cases first (before lowercasing)
	const trimmed = value.trim();
	
	// Map legacy/invalid values to valid schema values
	const legacyMappings: Record<string, string> = {
		'bi-weekly': 'weekly',     // Bi-weekly is a weekly pattern
		'biweekly': 'weekly',
		'weekends': 'weekly',      // Weekends is a weekly pattern
		'weekdays': 'weekly',      // Weekdays is a weekly pattern
		'does not repeat': 'null', // Special marker for no recurrence
		'never': 'null'
	};
	
	const lowerTrimmed = trimmed.toLowerCase();
	
	// Check legacy mappings
	if (legacyMappings[lowerTrimmed]) {
		const mapped = legacyMappings[lowerTrimmed];
		return mapped === 'null' ? null : mapped;
	}
	
	// Check if it's a valid enum value (lowercase)
	if (['daily', 'weekly', 'monthly', 'yearly'].includes(lowerTrimmed)) {
		return lowerTrimmed;
	}
	
	// If we have a value but it's not recognized, log a warning and default to weekly
	// (most flexible default that doesn't assume too much)
	console.warn(`[HOLSTER-CONVERTERS] Unknown recurrence value "${value}", defaulting to "weekly"`);
	return 'weekly';
}

/**
 * Convert Records back to arrays when appropriate
 * 
 * Strategy:
 * - null → [] ONLY for known array fields (capacity_slots, need_slots, children, contributors)
 * - Records with numeric keys ("0", "1", "2", ...) → Array
 * - Records with 'id' keys and all values having that id → Array (slot collections)
 * - Other Records → Keep as Record
 * - Recursively process nested structures
 * - Normalize enum values (fix old data) - applied at the END
 * 
 * Examples:
 * ```typescript
 * // Numeric keys → Array
 * { '0': 'red', '1': 'green', '2': 'blue' }
 * → ['red', 'green', 'blue']
 * 
 * // Id-keyed Records → Array (v5 uses arrays for slots)
 * { 'slot-1': { id: 'slot-1', name: 'Food' }, 'slot-2': { id: 'slot-2', name: 'Housing' } }
 * → [{ id: 'slot-1', name: 'Food' }, { id: 'slot-2', name: 'Housing' }]
 * 
 * // null → empty array (for known array fields)
 * null (with parentKey='capacity_slots') → []
 * 
 * // Enum normalization (applied after all conversions)
 * { recurrence: 'Daily' } → { recurrence: 'daily' }
 * 
 * // Nested
 * { slots: { '0': { id: 's1', times: { '0': '09:00', '1': '10:00' } } } }
 * → { slots: [{ id: 's1', times: ['09:00', '10:00'] }] }
 * ```
 */
function fromHolsterFormatInternal(data: any, parentKey?: string): any {
	// null → empty array ONLY for known array fields
	// This fixes the issue where empty arrays stored as null fail schema validation
	if (data === null || data === undefined) {
		// Known array fields that should become [] when null
		const arrayFields = new Set([
			'capacity_slots',
			'need_slots',
			'children',
			'contributors',
			'anti_contributors',
			'time_ranges',
			'day_schedules',
			'week_schedules',
			'month_schedules'
		]);
		
		if (parentKey && arrayFields.has(parentKey)) {
			return []; // Convert null → [] for array fields
		}
		
		return null; // Keep null for other fields
	}
	if (typeof data !== 'object') return data;
	
	// Handle plain objects
	if (isPlainObject(data)) {
		// Empty object handling
		if (Object.keys(data).length === 0) {
			// For known array fields, return empty array
			const arrayFields = new Set([
				'capacity_slots',
				'need_slots',
				'children',
				'contributors',
				'anti_contributors',
				'time_ranges',
				'day_schedules',
				'week_schedules',
				'month_schedules'
			]);
			
			if (parentKey && arrayFields.has(parentKey)) {
				return []; // Empty object → empty array for array fields
			}
			
			return null; // Empty object → null for other fields
		}
		
		// Check if this looks like an array stored as Record (numeric keys)
		if (looksLikeArrayRecord(data)) {
			// Convert back to array
			const keys = Object.keys(data).map(k => parseInt(k, 10)).sort((a, b) => a - b);
			const array: any[] = [];
			for (const key of keys) {
				const converted = fromHolsterFormatInternal(data[String(key)], parentKey);
				array.push(converted);
			}
			return array; // Always return array (even if empty after filtering)
		}
		
		// Check if this is an id-keyed collection (all values have 'id' field matching key)
		const keys = Object.keys(data).filter(k => k !== '_' && k !== '#'); // Skip Holster/Gun metadata
		if (keys.length > 0) {
			const allHaveMatchingId = keys.every(key => {
				const value = data[key];
				return isPlainObject(value) && value.id === key;
			});
			
			if (allHaveMatchingId) {
				// Convert to array (v5 uses arrays for slot collections)
				const array: any[] = [];
				for (const key of keys) {
					const converted = fromHolsterFormatInternal(data[key], parentKey);
					if (converted !== null) { // Skip null entries
						array.push(converted);
					}
				}
				// Always return array (empty arrays are valid!)
				// Fixes issue where empty capacity_slots/need_slots fail schema validation
				return array;
			}
		}
		
		// Regular object - recursively process values
		const result: Record<string, any> = {};
		for (const key in data) {
			// Skip Holster/Gun metadata fields
			if (key === '_' || key === '#') continue;
			
			const converted = fromHolsterFormatInternal(data[key], key);
			
			// Preserve null values (they might be intentional)
			result[key] = converted;
		}
		return result;
	}
	
	// Handle arrays (shouldn't happen if Holster is working correctly, but be defensive)
	if (isPlainArray(data)) {
		return data.map(item => fromHolsterFormatInternal(item, parentKey));
	}
	
	// Other types pass through
	return data;
}

/**
 * Public wrapper that converts AND normalizes
 */
export function fromHolsterFormat(data: any): any {
	// Step 1: Convert Records → Arrays
	const converted = fromHolsterFormatInternal(data);
	
	// Step 2: Normalize enums (fix old data)
	const normalized = normalizeSlotEnums(converted);
	
	return normalized;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER: NO-OP CONVERTERS (for stores that don't need conversion)
// ═══════════════════════════════════════════════════════════════════

/**
 * Identity function - no conversion
 * Use for stores that already use Holster-compatible formats
 */
export function noConversion(data: any): any {
	return data;
}
