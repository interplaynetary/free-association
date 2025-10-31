import { derived } from 'svelte/store';
import { userNetworkCapacitiesWithSlotQuantities } from '../core.svelte';
import type { CapacitiesCollection, ProviderCapacity, BaseCapacity } from '$lib/protocol/schemas';

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
	getObjectTimeSlots,
	createSlotAwareTimeIndex,
	findCapacitiesInTimeRange,
	getCapacityAvailableSlots
} from './time.svelte';

import {
	type LocationPoint,
	type LocationSortConfig,
	type LocationResult,
	sortObjectsByProximity,
	findObjectsWithinTravelTime,
	groupObjectsByTravelTime,
	extractCapacityLocations,
	extractCapacityPrimaryLocation,
	sortCapacitiesByProximity,
	findCapacitiesWithinTravelTime,
	groupCapacitiesByTravelTime
} from './space.svelte';

/**
 * Capacity-Specific Implementation & Reactive Stores
 *
 * This module provides the capacity-specific layer on top of the generic filtering system,
 * plus reactive Svelte stores for automatic updates when capacity data changes.
 */

// ===== CAPACITY-SPECIFIC TYPES =====

// Property definitions
const BASE_PROPERTIES = ['name', 'unit', 'description', 'emoji', 'owner_id'] as const;

const RECIPIENT_PROPERTIES = ['provider_id'] as const;
const INDEXABLE_PROPERTIES = [...BASE_PROPERTIES, ...RECIPIENT_PROPERTIES] as const;
// Numeric properties for efficient algorithm (slot-level quantities)
// We now work with allocated_quantity and available_quantity per slot, aggregated at capacity level
const NUMERIC_PROPERTIES = ['total_allocated_quantity', 'total_available_quantity'] as const;

export type BaseProperty = (typeof BASE_PROPERTIES)[number];
export type RecipientProperty = (typeof RECIPIENT_PROPERTIES)[number];
export type IndexableProperty = (typeof INDEXABLE_PROPERTIES)[number];
export type NumericProperty = (typeof NUMERIC_PROPERTIES)[number];

// Default ranges
export const DEFAULT_ALLOCATED_QUANTITY_RANGES: RangeDefinition[] = [
	{ label: '0 (None)', min: 0, max: 0, inclusive: true },
	{ label: '1-5', min: 1, max: 5, inclusive: true },
	{ label: '6-20', min: 6, max: 20, inclusive: false },
	{ label: '21-50', min: 21, max: 50, inclusive: false },
	{ label: '51+', min: 51, inclusive: false }
];

export const DEFAULT_AVAILABLE_QUANTITY_RANGES: RangeDefinition[] = [
	{ label: '1-10', min: 1, max: 10, inclusive: true },
	{ label: '11-50', min: 11, max: 50, inclusive: false },
	{ label: '51-100', min: 51, max: 100, inclusive: false },
	{ label: '101-500', min: 101, max: 500, inclusive: false },
	{ label: '500+', min: 500, inclusive: false }
];

// Legacy types for backward compatibility
export type PropertyLookupTables = Record<IndexableProperty, Record<string, string[]>>;
export type NumericRangeLookup = Record<string, string[]>;

export interface CapacityLookups {
	properties: PropertyLookupTables;
	numericRanges: {
		total_allocated_quantity: NumericRangeLookup;
		total_available_quantity: NumericRangeLookup;
	};
	hasLocation: string[];
	hasSchedule: string[];
	hasRecurrence: string[];
	hasQuantity: string[];
}

export interface CapacityCriteria {
	properties?: Partial<Record<IndexableProperty, string[]>>;
	numericRanges?: {
		total_allocated_quantity?: string[];
		total_available_quantity?: string[];
	};
	hasLocation?: boolean;
	hasSchedule?: boolean;
	hasRecurrence?: boolean;
	hasQuantity?: boolean;
}

// ===== CAPACITY-SPECIFIC HELPER FUNCTIONS =====

const hasLocationInfo = (capacity: any) =>
	capacity.capacity_slots?.some(
		(slot: any) =>
			!!(
				slot.location_type ||
				slot.longitude ||
				slot.latitude ||
				slot.street_address ||
				slot.city ||
				slot.state_province ||
				slot.country
			)
	) ?? false;

const hasScheduleInfo = (capacity: any) =>
	capacity.capacity_slots?.some(
		(slot: any) =>
			!!(slot.start_date || slot.start_time || slot.end_date || slot.end_time || slot.all_day)
	) ?? false;

const hasRecurrenceInfo = (capacity: any) =>
	capacity.capacity_slots?.some(
		(slot: any) => !!(slot.recurrence || slot.custom_recurrence_repeat_every)
	) ?? false;

const hasQuantityInfo = (capacity: any) =>
	capacity.capacity_slots?.some((slot: any) => slot.quantity != null) ?? false;

// ===== CAPACITY-SPECIFIC FUNCTIONS =====

export function createCapacityLookups(
	capacities: CapacitiesCollection,
	customRanges?: Partial<Record<NumericProperty, RangeDefinition[]>>
): CapacityLookups {
	// First, augment capacities with aggregated quantities for filtering
	const augmentedCapacities: Record<string, any> = {};

	Object.entries(capacities).forEach(([capacityId, capacity]) => {
		// Handle both timestamped and direct capacity data
		const actualCapacity = (capacity as any)?.data || capacity;

		// Calculate total allocated and available quantities across all slots
		let totalAllocated = 0;
		let totalAvailable = 0;

		actualCapacity.capacity_slots?.forEach((slot: any) => {
			totalAllocated += slot.allocated_quantity || 0;
			totalAvailable += slot.available_quantity || slot.quantity || 0;
		});

		augmentedCapacities[capacityId] = {
			...actualCapacity,
			total_allocated_quantity: totalAllocated,
			total_available_quantity: totalAvailable
		};
	});

	const ranges = {
		total_allocated_quantity:
			customRanges?.total_allocated_quantity || DEFAULT_ALLOCATED_QUANTITY_RANGES,
		total_available_quantity:
			customRanges?.total_available_quantity || DEFAULT_AVAILABLE_QUANTITY_RANGES
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

	const genericLookups = createObjectLookups(augmentedCapacities, config);

	// Transform to legacy format
	const properties = {} as PropertyLookupTables;
	INDEXABLE_PROPERTIES.forEach((prop) => {
		properties[prop] = genericLookups.properties[prop] || {};
	});

	return {
		properties,
		numericRanges: {
			total_allocated_quantity: genericLookups.ranges.total_allocated_quantity || {},
			total_available_quantity: genericLookups.ranges.total_available_quantity || {}
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
		ranges: {
			total_allocated_quantity: lookups.numericRanges.total_allocated_quantity,
			total_available_quantity: lookups.numericRanges.total_available_quantity
		},
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
	[userNetworkCapacitiesWithSlotQuantities],
	([$userNetworkCapacitiesWithSlotQuantities]) => {
		if (
			!$userNetworkCapacitiesWithSlotQuantities ||
			Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
		) {
			return createCapacityLookups({} as CapacitiesCollection);
		}
		return createCapacityLookups($userNetworkCapacitiesWithSlotQuantities as CapacitiesCollection);
	}
);

export const createCapacityLookupsWithCustomRanges = (
	customRanges: Partial<Record<NumericProperty, RangeDefinition[]>>
) =>
	derived(
		[userNetworkCapacitiesWithSlotQuantities],
		([$userNetworkCapacitiesWithSlotQuantities]) => {
			if (
				!$userNetworkCapacitiesWithSlotQuantities ||
				Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
			) {
				return createCapacityLookups({} as CapacitiesCollection, customRanges);
			}
			return createCapacityLookups(
				$userNetworkCapacitiesWithSlotQuantities as CapacitiesCollection,
				customRanges
			);
		}
	);

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
	derived(
		[userNetworkCapacitiesWithSlotQuantities],
		([$userNetworkCapacitiesWithSlotQuantities]) => {
			if (
				!$userNetworkCapacitiesWithSlotQuantities ||
				Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
			) {
				return createTimeIndex({}, config);
			}
			return createTimeIndex($userNetworkCapacitiesWithSlotQuantities, config);
		}
	);

/**
 * Create reactive proximity-sorted capacities
 */
export const createCapacityProximitySort = (config: LocationSortConfig) =>
	derived(
		[userNetworkCapacitiesWithSlotQuantities],
		([$userNetworkCapacitiesWithSlotQuantities]) => {
			if (
				!$userNetworkCapacitiesWithSlotQuantities ||
				Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
			) {
				return [];
			}
			return sortObjectsByProximity($userNetworkCapacitiesWithSlotQuantities, config);
		}
	);

/**
 * Create reactive time index for capacities (slot-aware)
 */
export const createCapacitySlotTimeIndex = (config: TimeIndexConfig = {}) =>
	derived(
		[userNetworkCapacitiesWithSlotQuantities],
		([$userNetworkCapacitiesWithSlotQuantities]) => {
			if (
				!$userNetworkCapacitiesWithSlotQuantities ||
				Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
			) {
				return createSlotAwareTimeIndex({}, config);
			}
			return createSlotAwareTimeIndex($userNetworkCapacitiesWithSlotQuantities, config);
		}
	);

/**
 * Create reactive proximity-sorted capacities (slot-aware)
 */
export const createCapacitySlotProximitySort = (config: LocationSortConfig) =>
	derived(
		[userNetworkCapacitiesWithSlotQuantities],
		([$userNetworkCapacitiesWithSlotQuantities]) => {
			if (
				!$userNetworkCapacitiesWithSlotQuantities ||
				Object.keys($userNetworkCapacitiesWithSlotQuantities).length === 0
			) {
				return [];
			}
			return sortCapacitiesByProximity($userNetworkCapacitiesWithSlotQuantities, config);
		}
	);

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
	createSlotAwareTimeIndex,
	findCapacitiesInTimeRange,
	getCapacityAvailableSlots,

	// Location functions
	sortObjectsByProximity,
	findObjectsWithinTravelTime,
	groupObjectsByTravelTime,
	extractCapacityLocations,
	extractCapacityPrimaryLocation,
	sortCapacitiesByProximity,
	findCapacitiesWithinTravelTime,
	groupCapacitiesByTravelTime,

	// Utility functions
	createRanges,
	createPercentageRanges
};
