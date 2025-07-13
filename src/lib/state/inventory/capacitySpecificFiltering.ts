import { derived } from 'svelte/store';
import { userNetworkCapacitiesWithShares } from '../core.svelte';
import type {
	CapacitiesCollection,
	RecipientCapacity,
	ProviderCapacity,
	BaseCapacity
} from '$lib/schema';

// Import from the three modular systems
import {
	type RangeDefinition,
	type ObjectLookupConfig,
	type ObjectLookups,
	type SearchCriteria,
	createObjectLookups,
	findObjectsWithAll,
	findObjectsWithAny,
	createRanges,
	createPercentageRanges
} from './objectFiltering.svelte';

import {
	type TimeSlot,
	type TimeIndexConfig,
	type TimeIndex,
	createTimeIndex,
	findObjectsInTimeRange,
	getObjectTimeSlots
} from './time.svelte';

import {
	type LocationPoint,
	type LocationSortConfig,
	type LocationResult,
	sortObjectsByProximity,
	findObjectsWithinTravelTime,
	groupObjectsByTravelTime
} from './space.svelte';

/**
 * Capacity-Specific Implementation & Reactive Stores
 *
 * This module provides the capacity-specific layer on top of the generic filtering system,
 * plus reactive Svelte stores for automatic updates when capacity data changes.
 */

// ===== CAPACITY-SPECIFIC TYPES =====

// Property definitions
const BASE_PROPERTIES = [
	'name',
	'unit',
	'description',
	'emoji',
	'location_type',
	'city',
	'state_province',
	'country',
	'recurrence',
	'time_zone',
	'owner_id'
] as const;

const RECIPIENT_PROPERTIES = ['provider_id'] as const;
const INDEXABLE_PROPERTIES = [...BASE_PROPERTIES, ...RECIPIENT_PROPERTIES] as const;
const NUMERIC_PROPERTIES = ['quantity', 'share_percentage', 'computed_quantity'] as const;

export type BaseProperty = (typeof BASE_PROPERTIES)[number];
export type RecipientProperty = (typeof RECIPIENT_PROPERTIES)[number];
export type IndexableProperty = (typeof INDEXABLE_PROPERTIES)[number];
export type NumericProperty = (typeof NUMERIC_PROPERTIES)[number];

// Default ranges
export const DEFAULT_QUANTITY_RANGES: RangeDefinition[] = [
	{ label: '0-10', min: 0, max: 10, inclusive: true },
	{ label: '10-50', min: 10, max: 50, inclusive: false },
	{ label: '50-100', min: 50, max: 100, inclusive: false },
	{ label: '100-500', min: 100, max: 500, inclusive: false },
	{ label: '500+', min: 500, inclusive: false }
];

export const DEFAULT_PERCENTAGE_RANGES: RangeDefinition[] = [
	{ label: '0-10%', min: 0, max: 0.1, inclusive: true },
	{ label: '10-25%', min: 0.1, max: 0.25, inclusive: false },
	{ label: '25-50%', min: 0.25, max: 0.5, inclusive: false },
	{ label: '50-75%', min: 0.5, max: 0.75, inclusive: false },
	{ label: '75-100%', min: 0.75, max: 1.0, inclusive: false }
];

// Legacy types for backward compatibility
export type PropertyLookupTables = Record<IndexableProperty, Record<string, string[]>>;
export type NumericRangeLookup = Record<string, string[]>;

export interface CapacityLookups {
	properties: PropertyLookupTables;
	numericRanges: Record<NumericProperty, NumericRangeLookup>;
	hasLocation: string[];
	hasSchedule: string[];
	hasRecurrence: string[];
	hasQuantity: string[];
}

export interface CapacityCriteria {
	properties?: Partial<Record<IndexableProperty, string[]>>;
	numericRanges?: Partial<Record<NumericProperty, string[]>>;
	hasLocation?: boolean;
	hasSchedule?: boolean;
	hasRecurrence?: boolean;
	hasQuantity?: boolean;
}

// ===== CAPACITY-SPECIFIC HELPER FUNCTIONS =====

const hasLocationInfo = (capacity: any) =>
	!!(
		capacity.location_type ||
		capacity.longitude ||
		capacity.latitude ||
		capacity.street_address ||
		capacity.city ||
		capacity.state_province ||
		capacity.country
	);

const hasScheduleInfo = (capacity: any) =>
	!!(
		capacity.start_date ||
		capacity.start_time ||
		capacity.end_date ||
		capacity.end_time ||
		capacity.all_day
	);

const hasRecurrenceInfo = (capacity: any) =>
	!!(capacity.recurrence || capacity.custom_recurrence_repeat_every);

const hasQuantityInfo = (capacity: any) => capacity.quantity != null;

// ===== CAPACITY-SPECIFIC FUNCTIONS =====

export function createCapacityLookups(
	capacities: CapacitiesCollection,
	customRanges?: Partial<Record<NumericProperty, RangeDefinition[]>>
): CapacityLookups {
	const ranges = {
		quantity: customRanges?.quantity || DEFAULT_QUANTITY_RANGES,
		share_percentage: customRanges?.share_percentage || DEFAULT_PERCENTAGE_RANGES,
		computed_quantity: customRanges?.computed_quantity || DEFAULT_QUANTITY_RANGES
	};

	const config: ObjectLookupConfig = {
		properties: [...INDEXABLE_PROPERTIES],
		ranges,
		custom: {
			hasLocation: hasLocationInfo,
			hasSchedule: hasScheduleInfo,
			hasRecurrence: hasRecurrenceInfo,
			hasQuantity: hasQuantityInfo
		}
	};

	const genericLookups = createObjectLookups(capacities, config);

	// Transform to legacy format
	const properties = {} as PropertyLookupTables;
	INDEXABLE_PROPERTIES.forEach((prop) => {
		properties[prop] = genericLookups.properties[prop] || {};
	});

	return {
		properties,
		numericRanges: {
			quantity: genericLookups.ranges.quantity || {},
			share_percentage: genericLookups.ranges.share_percentage || {},
			computed_quantity: genericLookups.ranges.computed_quantity || {}
		},
		hasLocation: genericLookups.custom.hasLocation || [],
		hasSchedule: genericLookups.custom.hasSchedule || [],
		hasRecurrence: genericLookups.custom.hasRecurrence || [],
		hasQuantity: genericLookups.custom.hasQuantity || []
	};
}

// Convert capacity criteria to generic search criteria
const convertCapacityCriteria = (
	criteria: CapacityCriteria,
	lookups: CapacityLookups
): { genericCriteria: SearchCriteria; genericLookups: ObjectLookups } => {
	const genericCriteria: SearchCriteria = {
		properties: criteria.properties,
		ranges: criteria.numericRanges,
		custom: []
	};

	if (criteria.hasLocation) genericCriteria.custom!.push('hasLocation');
	if (criteria.hasSchedule) genericCriteria.custom!.push('hasSchedule');
	if (criteria.hasRecurrence) genericCriteria.custom!.push('hasRecurrence');
	if (criteria.hasQuantity) genericCriteria.custom!.push('hasQuantity');

	const genericLookups: ObjectLookups = {
		properties: lookups.properties,
		ranges: lookups.numericRanges,
		arrays: {},
		custom: {
			hasLocation: lookups.hasLocation,
			hasSchedule: lookups.hasSchedule,
			hasRecurrence: lookups.hasRecurrence,
			hasQuantity: lookups.hasQuantity
		}
	};

	return { genericCriteria, genericLookups };
};

export const findCapacitiesWithAllCriteria = (
	lookups: CapacityLookups,
	criteria: CapacityCriteria
): string[] => {
	const { genericCriteria, genericLookups } = convertCapacityCriteria(criteria, lookups);
	return findObjectsWithAll(genericLookups, genericCriteria);
};

export const findCapacitiesWithAnyCriteria = (
	lookups: CapacityLookups,
	criteria: CapacityCriteria
): string[] => {
	const { genericCriteria, genericLookups } = convertCapacityCriteria(criteria, lookups);
	return findObjectsWithAny(genericLookups, genericCriteria);
};

// ===== REACTIVE STORES =====

export const capacityLookups = derived(
	[userNetworkCapacitiesWithShares],
	([$userNetworkCapacitiesWithShares]) => {
		if (
			!$userNetworkCapacitiesWithShares ||
			Object.keys($userNetworkCapacitiesWithShares).length === 0
		) {
			return createCapacityLookups({});
		}
		return createCapacityLookups($userNetworkCapacitiesWithShares);
	}
);

export const createCapacityLookupsWithCustomRanges = (
	customRanges: Partial<Record<NumericProperty, RangeDefinition[]>>
) =>
	derived([userNetworkCapacitiesWithShares], ([$userNetworkCapacitiesWithShares]) => {
		if (
			!$userNetworkCapacitiesWithShares ||
			Object.keys($userNetworkCapacitiesWithShares).length === 0
		) {
			return createCapacityLookups({}, customRanges);
		}
		return createCapacityLookups($userNetworkCapacitiesWithShares, customRanges);
	});

export const findCapacitiesWithAll = derived(
	[capacityLookups],
	([$capacityLookups]) =>
		(criteria: CapacityCriteria) =>
			findCapacitiesWithAllCriteria($capacityLookups, criteria)
);

export const findCapacitiesWithAny = derived(
	[capacityLookups],
	([$capacityLookups]) =>
		(criteria: CapacityCriteria) =>
			findCapacitiesWithAnyCriteria($capacityLookups, criteria)
);

// ===== REACTIVE TIME & LOCATION STORES =====

/**
 * Create reactive time index for capacities
 */
export const createCapacityTimeIndex = (config: TimeIndexConfig = {}) =>
	derived([userNetworkCapacitiesWithShares], ([$userNetworkCapacitiesWithShares]) => {
		if (
			!$userNetworkCapacitiesWithShares ||
			Object.keys($userNetworkCapacitiesWithShares).length === 0
		) {
			return createTimeIndex({}, config);
		}
		return createTimeIndex($userNetworkCapacitiesWithShares, config);
	});

/**
 * Create reactive proximity-sorted capacities
 */
export const createCapacityProximitySort = (config: LocationSortConfig) =>
	derived([userNetworkCapacitiesWithShares], ([$userNetworkCapacitiesWithShares]) => {
		if (
			!$userNetworkCapacitiesWithShares ||
			Object.keys($userNetworkCapacitiesWithShares).length === 0
		) {
			return [];
		}
		return sortObjectsByProximity($userNetworkCapacitiesWithShares, config);
	});

// ===== LEGACY ALIASES =====

export const createQuantityRanges = createRanges;

// ===== EXPORTS =====

// Re-export all types and functions from the modular systems
export type {
	RangeDefinition,
	ObjectLookupConfig,
	ObjectLookups,
	SearchCriteria,
	TimeSlot,
	TimeIndexConfig,
	TimeIndex,
	LocationPoint,
	LocationSortConfig,
	LocationResult
};

export {
	// Core generic functions
	createObjectLookups,
	findObjectsWithAll,
	findObjectsWithAny,
	
	// Time functions
	createTimeIndex,
	findObjectsInTimeRange,
	getObjectTimeSlots,
	
	// Location functions
	sortObjectsByProximity,
	findObjectsWithinTravelTime,
	groupObjectsByTravelTime,
	
	// Utility functions
	createRanges,
	createPercentageRanges
};