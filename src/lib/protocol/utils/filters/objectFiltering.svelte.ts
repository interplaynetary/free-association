/**
 * Core Generic Object Filtering System
 *
 * This module provides universal filtering capabilities for JavaScript objects with:
 * - Nested property path support (e.g., 'user.profile.name')
 * - Numeric ranges with custom breakpoints
 * - Array element indexing
 * - Custom predicate functions
 * - Fast lookup tables for complex queries
 *
 * @example Basic Usage
 * ```typescript
 * const lookups = createObjectLookups(objects, {
 *   properties: ['name', 'user.profile.email'],
 *   ranges: { age: [
 *     { label: '18-30', min: 18, max: 30 },
 *     { label: '30-50', min: 30, max: 50 }
 *   ]},
 *   arrays: ['tags', 'categories'],
 *   custom: { hasEmail: (obj) => !!obj.email }
 * });
 *
 * const results = findObjectsWithAll(lookups, {
 *   properties: { name: ['John', 'Jane'] },
 *   ranges: { age: ['18-30'] },
 *   custom: ['hasEmail']
 * });
 * ```
 */

// ===== TYPES & INTERFACES =====

export interface RangeDefinition {
	label: string;
	min?: number;
	max?: number;
	inclusive?: boolean;
}

export interface ObjectLookupConfig {
	properties?: string[]; // Property paths to index (supports nested like 'user.profile.name')
	ranges?: Record<string, RangeDefinition[]>; // Numeric properties with range definitions
	arrays?: string[]; // Array properties to index individual elements
	custom?: Record<string, (obj: any) => boolean>; // Custom predicate functions
	caseSensitive?: boolean; // Whether string comparisons are case sensitive
}

export interface ObjectLookups {
	properties: Record<string, Record<string, string[]>>; // propertyPath -> value -> objectIds
	ranges: Record<string, Record<string, string[]>>; // propertyPath -> rangeLabel -> objectIds
	arrays: Record<string, Record<string, string[]>>; // propertyPath -> arrayElement -> objectIds
	custom: Record<string, string[]>; // customKey -> objectIds
}

export interface SearchCriteria {
	properties?: Record<string, string[]>; // propertyPath -> values to match
	ranges?: Record<string, string[]>; // propertyPath -> rangeLabels to match
	arrays?: Record<string, string[]>; // propertyPath -> arrayElements to match
	custom?: string[]; // customKeys to match
}

// ===== UTILITY FUNCTIONS =====

export const getNestedValue = (obj: any, path: string): any =>
	path.split('.').reduce((current, key) => current?.[key], obj);

export const valueInRange = (value: number, range: RangeDefinition): boolean => {
	if (range.min !== undefined && range.max !== undefined) {
		return range.inclusive
			? value >= range.min && value <= range.max
			: value >= range.min && value < range.max;
	} else if (range.min !== undefined) {
		return value >= range.min;
	} else if (range.max !== undefined) {
		return range.inclusive ? value <= range.max : value < range.max;
	}
	return true;
};

export const intersect = (a: string[], b: string[]): string[] => {
	const setB = new Set(b);
	return a.filter((x) => setB.has(x));
};

export const union = (arrays: string[][]): string[] => {
	const combined = new Set<string>();
	arrays.forEach((arr) => arr.forEach((id) => combined.add(id)));
	return Array.from(combined);
};

// ===== RANGE CREATION UTILITIES =====

export function createRanges(breakpoints: number[], labelPrefix: string = ''): RangeDefinition[] {
	const ranges: RangeDefinition[] = [];

	for (let i = 0; i < breakpoints.length; i++) {
		if (i === 0) {
			ranges.push({
				label: `${labelPrefix}0-${breakpoints[i]}`,
				min: 0,
				max: breakpoints[i],
				inclusive: true
			});
		} else {
			ranges.push({
				label: `${labelPrefix}${breakpoints[i - 1]}-${breakpoints[i]}`,
				min: breakpoints[i - 1],
				max: breakpoints[i],
				inclusive: false
			});
		}
	}

	// Add final range for values above the last breakpoint
	const lastBreakpoint = breakpoints[breakpoints.length - 1];
	ranges.push({
		label: `${labelPrefix}${lastBreakpoint}+`,
		min: lastBreakpoint,
		inclusive: false
	});

	return ranges;
}

export function createPercentageRanges(breakpoints: number[]): RangeDefinition[] {
	const ranges: RangeDefinition[] = [];

	for (let i = 0; i < breakpoints.length; i++) {
		if (i === 0) {
			ranges.push({
				label: `0-${Math.round(breakpoints[i] * 100)}%`,
				min: 0,
				max: breakpoints[i],
				inclusive: true
			});
		} else {
			ranges.push({
				label: `${Math.round(breakpoints[i - 1] * 100)}-${Math.round(breakpoints[i] * 100)}%`,
				min: breakpoints[i - 1],
				max: breakpoints[i],
				inclusive: false
			});
		}
	}

	// Add final range for values above the last breakpoint
	const lastBreakpoint = breakpoints[breakpoints.length - 1];
	ranges.push({
		label: `${Math.round(lastBreakpoint * 100)}%+`,
		min: lastBreakpoint,
		inclusive: false
	});

	return ranges;
}

// ===== CORE FILTERING FUNCTIONS =====

export function createObjectLookups(
	objects: Record<string, any>,
	config: ObjectLookupConfig
): ObjectLookups {
	const lookups: ObjectLookups = {
		properties: {},
		ranges: {},
		arrays: {},
		custom: {}
	};

	// Initialize lookup structures
	config.properties?.forEach((path) => {
		lookups.properties[path] = {};
	});
	config.arrays?.forEach((path) => {
		lookups.arrays[path] = {};
	});
	Object.keys(config.custom || {}).forEach((key) => {
		lookups.custom[key] = [];
	});

	Object.entries(config.ranges || {}).forEach(([path, ranges]) => {
		lookups.ranges[path] = {};
		ranges.forEach((range) => {
			lookups.ranges[path][range.label] = [];
		});
	});

	// Process each object
	Object.entries(objects).forEach(([objectId, obj]) => {
		// Index regular properties
		config.properties?.forEach((path) => {
			const value = getNestedValue(obj, path);
			if (value != null && value !== '') {
				const stringValue = config.caseSensitive ? String(value) : String(value).toLowerCase();
				(lookups.properties[path][stringValue] ??= []).push(objectId);
			}
		});

		// Index range properties
		Object.entries(config.ranges || {}).forEach(([path, ranges]) => {
			const value = getNestedValue(obj, path);
			if (typeof value === 'number') {
				ranges.forEach((range) => {
					if (valueInRange(value, range)) {
						lookups.ranges[path][range.label].push(objectId);
					}
				});
			}
		});

		// Index array properties
		config.arrays?.forEach((path) => {
			const value = getNestedValue(obj, path);
			if (Array.isArray(value)) {
				value.forEach((element) => {
					const stringElement = config.caseSensitive
						? String(element)
						: String(element).toLowerCase();
					(lookups.arrays[path][stringElement] ??= []).push(objectId);
				});
			}
		});

		// Index custom predicates
		Object.entries(config.custom || {}).forEach(([key, predicate]) => {
			if (predicate(obj)) {
				lookups.custom[key].push(objectId);
			}
		});
	});

	return lookups;
}

export function findObjectsWithAll(lookups: ObjectLookups, criteria: SearchCriteria): string[] {
	let result: string[] | null = null;

	// Process each criteria type
	const processCriteria = (
		criteriaMap: Record<string, string[]> | undefined,
		lookupMap: Record<string, Record<string, string[]>>
	) => {
		Object.entries(criteriaMap || {}).forEach(([path, values]) => {
			if (values?.length > 0) {
				const pathLookups = lookupMap[path] || {};
				const matchingIds = union(values.map((value) => pathLookups[value] || []));
				result = result ? intersect(result, matchingIds) : matchingIds;
			}
		});
	};

	processCriteria(criteria.properties, lookups.properties);
	processCriteria(criteria.ranges, lookups.ranges);
	processCriteria(criteria.arrays, lookups.arrays);

	// Process custom criteria
	criteria.custom?.forEach((key) => {
		const customIds = lookups.custom[key] || [];
		result = result ? intersect(result, customIds) : customIds;
	});

	return result || [];
}

export function findObjectsWithAny(lookups: ObjectLookups, criteria: SearchCriteria): string[] {
	const allMatchingIds: string[][] = [];

	// Process each criteria type
	const processCriteria = (
		criteriaMap: Record<string, string[]> | undefined,
		lookupMap: Record<string, Record<string, string[]>>
	) => {
		Object.entries(criteriaMap || {}).forEach(([path, values]) => {
			if (values?.length > 0) {
				const pathLookups = lookupMap[path] || {};
				values.forEach((value) => {
					const matchingIds = pathLookups[value] || [];
					if (matchingIds.length > 0) allMatchingIds.push(matchingIds);
				});
			}
		});
	};

	processCriteria(criteria.properties, lookups.properties);
	processCriteria(criteria.ranges, lookups.ranges);
	processCriteria(criteria.arrays, lookups.arrays);

	// Process custom criteria
	criteria.custom?.forEach((key) => {
		const customIds = lookups.custom[key] || [];
		if (customIds.length > 0) allMatchingIds.push(customIds);
	});

	return union(allMatchingIds);
} 