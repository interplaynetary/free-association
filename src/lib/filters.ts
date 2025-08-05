/**
 * Filter System
 *
 * This module provides two complementary filtering approaches:
 * 1. Runtime Filters: Pure functions for in-memory filtering (ShareFilter type)
 * 2. Serializable Rules: JSON Logic rules that can be stored and transmitted (JsonLogicRule type)
 *
 * The primary filtering context is subtree-based, allowing filters to operate on
 * contributor groups within tree structures.
 */

import type { Node, ShareMap, BaseCapacity } from './schema';
// @ts-ignore
import jsonLogic from 'json-logic-js';

// Core types for runtime filtering
export type FilterContext = {
	subtreeContributors: Record<string, Record<string, boolean>>;
};

export type ShareFilter = (nodeId: string, share: number, context?: FilterContext) => boolean;

// Core types for serializable rules
export type JsonLogicRule = {
	[operator: string]: any[] | JsonLogicRule;
};

/**
 * Runtime Filtering System
 * These functions operate in memory and cannot be serialized
 */

export function filter(
	shareMap: ShareMap,
	filterFn: ShareFilter,
	context?: FilterContext
): ShareMap {
	const entries = Object.entries(shareMap).filter(([nodeId, share]) =>
		filterFn(nodeId, share, context)
	);

	return normalizeShareMap(Object.fromEntries(entries));
}

// Runtime filter composition
export const compose =
	(...filters: ShareFilter[]): ShareFilter =>
	(nodeId, share, context) =>
		filters.every((f) => f(nodeId, share, context));

export const or =
	(...filters: ShareFilter[]): ShareFilter =>
	(nodeId, share, context) =>
		filters.some((f) => f(nodeId, share, context));

export const not =
	(filter: ShareFilter): ShareFilter =>
	(nodeId, share, context) =>
		!filter(nodeId, share, context);

// Runtime filter generators
export const Filters = {
	includeNodes:
		(nodeIds: string[]): ShareFilter =>
		(nodeId) =>
			nodeIds.includes(nodeId),

	excludeNodes:
		(nodeIds: string[]): ShareFilter =>
		(nodeId) =>
			!nodeIds.includes(nodeId),

	aboveThreshold:
		(threshold: number): ShareFilter =>
		(_, share) =>
			share >= threshold

	// Note: byCategory removed as Node schema doesn't have categories property
};

/**
 * Serializable Rule System
 * These rules can be stored as JSON and transmitted between systems
 */

// Serializable rule generators
export const Rules = {
	includeNodes: (nodeIds: string[]): JsonLogicRule => ({
		in: [{ var: 'nodeId' }, nodeIds]
	}),

	excludeNodes: (nodeIds: string[]): JsonLogicRule => ({
		'!': { in: [{ var: 'nodeId' }, nodeIds] }
	}),

	aboveThreshold: (threshold: number): JsonLogicRule => ({
		'>=': [{ var: 'share' }, threshold]
	}),

	// Note: byCategory removed as Node schema doesn't have categories property

	inSubtrees: (subtreeIds: string[]): JsonLogicRule => ({
		some: [
			subtreeIds,
			{
				'!!': { var: ['subtreeContributors', { var: '' }, { var: 'nodeId' }] }
			}
		]
	}),

	inSubtree: (subtreeId: string): JsonLogicRule => ({
		in: [{ var: 'nodeId' }, { var: ['subtreeContributors', subtreeId] }]
	}),

	// Rule composition
	and: (...rules: JsonLogicRule[]): JsonLogicRule => ({
		and: rules
	}),

	or: (...rules: JsonLogicRule[]): JsonLogicRule => ({
		or: rules
	}),

	not: (rule: JsonLogicRule): JsonLogicRule => ({
		'!': rule
	})
} as const;

/**
 * Bridge between Runtime Filters and Serializable Rules
 */

// Convert a JSON Logic rule to a runtime filter
export function ruleToFilter(rule: JsonLogicRule): ShareFilter {
	return (nodeId: string, share: number, context: FilterContext = { subtreeContributors: {} }) => {
		const data = {
			nodeId,
			share,
			subtreeContributors: context.subtreeContributors
		};
		return Boolean(jsonLogic.apply(rule, data));
	};
}

/**
 * Apply capacity-level filter rules to a share map.
 *
 * IMPORTANT: This function ONLY handles logical filtering (e.g., include/exclude nodes, thresholds).
 * For slot-based divisibility constraints (e.g., max_percentage_div, max_natural_div),
 * use applySlotConstraints() in slots.svelte.ts instead.
 *
 * This separation ensures:
 * 1. Logical filters work at capacity level (who can participate)
 * 2. Divisibility constraints work at slot level (how resources are divided)
 */
export function applyCapacityFilter(
	capacity: BaseCapacity,
	shareMap: ShareMap,
	context?: FilterContext
): ShareMap {
	if (!capacity.filter_rule) {
		return normalizeShareMap({ ...shareMap });
	}

	return filter(shareMap, ruleToFilter(capacity.filter_rule), context);
}

// Normalize a ShareMap so values sum to 1
export function normalizeShareMap(map: ShareMap): ShareMap {
	const total = Object.values(map).reduce((sum, v) => sum + v, 0);
	if (total === 0) return map;

	return Object.fromEntries(Object.entries(map).map(([k, v]) => [k, v / total]));
}
