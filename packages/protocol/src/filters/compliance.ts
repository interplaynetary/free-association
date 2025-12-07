/**
 * Compliance Filters - Numeric Capacity Limits (JsonLogic-based)
 * 
 * This module handles OVERALL capacity limits per recipient using JsonLogic.
 * 
 * USE CASES:
 * - Regulatory compliance: "No more than $50K per person per year"
 * - Risk management: "Cap exposure to any single recipient"
 * - Blocking: "This specific person gets nothing"
 * - Tiered caps: "Premium tier gets $100K, regular gets $50K"
 * - Dynamic caps: "Cap increases by $10K per month"
 * 
 * UNION PRINCIPLE:
 * When multiple compliance filters apply, the most restrictive wins:
 * - min(filter1, filter2, ..., filterN)
 * - Blocked (0) always wins
 * - null (unlimited) is least restrictive
 * 
 * JSONLOGIC USAGE:
 * Compliance filters are JsonLogic rules that return numbers:
 * ```typescript
 * // Simple cap
 * const filter = 50000;
 * 
 * // Conditional cap based on tier
 * const filter = {
 *   "if": [
 *     {"==": [{"var": "attributes.tier"}, "premium"]},
 *     100000,
 *     50000
 *   ]
 * };
 * 
 * // Dynamic cap based on recognition
 * const filter = {
 *   "*": [{"var": "mutualRecognition"}, 100000]
 * };
 * 
 * // Blocked
 * const filter = 0;
 * 
 * // Unlimited
 * const filter = null;
 * ```
 */

import { z } from 'zod';
// @ts-ignore - json-logic-js doesn't have perfect types
import jsonLogic from 'json-logic-js';
import {
	ComplianceFilterSchema,
	type ComplianceFilter,
	type FilterContext,
	FilterContextSchema,
	type FilterResult,
	FilterResultSchema
} from './types';

// ═══════════════════════════════════════════════════════════════════
// CACHING FOR PERFORMANCE
// ═══════════════════════════════════════════════════════════════════

/**
 * LRU cache for filter evaluation results
 * 
 * Caching is critical for performance since filters are evaluated
 * 100-5000 times per allocation cycle. With caching, we get:
 * - First evaluation: ~0.01-0.5ms (depending on complexity)
 * - Cached evaluations: ~0.001ms (10-100x faster)
 */
class FilterCache {
	private cache = new Map<string, number>();
	private maxSize = 1000; // Prevent unbounded growth
	
	get(key: string): number | undefined {
		return this.cache.get(key);
	}
	
	set(key: string, value: number): void {
		// Simple LRU: if cache is full, delete oldest entry
		if (this.cache.size >= this.maxSize) {
			const firstKey = this.cache.keys().next().value;
			this.cache.delete(firstKey || '');
		}
		this.cache.set(key, value);
	}
	
	clear(): void {
		this.cache.clear();
	}
	
	get size(): number {
		return this.cache.size;
	}
}

const complianceFilterCache = new FilterCache();

/**
 * Generate cache key for compliance filter evaluation
 * 
 * Key includes:
 * - Filter rule (stringified)
 * - Recipient pubKey
 * - Current total (for cap calculations)
 * - Mutual recognition (for dynamic caps)
 */
function getCacheKey(filter: ComplianceFilter, context: FilterContext): string {
	// For literals, cache key is just the value (same for all contexts)
	if (typeof filter === 'number') return `num:${filter}`;
	if (filter === null) return 'null:unlimited';
	
	// For JsonLogic rules, include context in key
	return `${JSON.stringify(filter)}:${context.pubKey}:${context.currentTotal || 0}:${context.mutualRecognition || 0}`;
}

// ═══════════════════════════════════════════════════════════════════
// EVALUATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Evaluate compliance filter to get numeric limit
 * 
 * @param filter - Compliance filter (JsonLogic rule, number, or null)
 * @param context - Filter context
 * @returns Numeric limit (0 for blocked, Infinity for unlimited)
 */
export function evaluateComplianceFilter(
	filter: ComplianceFilter,
	context: FilterContext,
	useCache: boolean = true
): number {
	// Validate inputs
	ComplianceFilterSchema.parse(filter);
	FilterContextSchema.parse(context);
	
	// FAST PATH: Literal values (no caching needed)
	if (filter === null) {
		return Infinity;
	}
	if (typeof filter === 'number') {
		return filter;
	}
	
	// Check cache first
	if (useCache) {
		const cacheKey = getCacheKey(filter, context);
		const cached = complianceFilterCache.get(cacheKey);
		if (cached !== undefined) {
			return cached;
		}
	}
	
	// SLOW PATH: JsonLogic evaluation
	let result: number;
	try {
		const evaluated = jsonLogic.apply(filter, context);
		
		if (evaluated === null || evaluated === undefined) {
			result = Infinity;
		} else if (typeof evaluated === 'number') {
			result = evaluated;
		} else {
			console.error('[COMPLIANCE-FILTER] Invalid result type:', typeof evaluated, evaluated);
			result = 0;
		}
	} catch (error) {
		console.error('[COMPLIANCE-FILTER] Error evaluating filter:', error);
		result = 0;
	}
	
	// Cache the result
	if (useCache) {
		const cacheKey = getCacheKey(filter, context);
		complianceFilterCache.set(cacheKey, result);
	}
	
	return result;
}

/**
 * Clear the compliance filter cache
 * 
 * Call this when:
 * - Starting a new allocation cycle
 * - Context data has changed significantly
 */
export function clearComplianceFilterCache(): void {
	complianceFilterCache.clear();
}

/**
 * Get cache statistics for monitoring/debugging
 */
export function getComplianceFilterCacheStats(): { size: number; maxSize: number } {
	return {
		size: complianceFilterCache.size,
		maxSize: 1000
	};
}

/**
 * Get numeric value from compliance filter (legacy compatibility)
 * 
 * This function provides backward compatibility with the old discriminated union format:
 * - { type: 'blocked', value: 0 } → 0
 * - { type: 'capped', value: X } → X
 * - { type: 'unlimited' } → Infinity
 * 
 * And supports the new JsonLogic format:
 * - 0 → 0
 * - 50000 → 50000
 * - null → Infinity
 * - {"if": [...]} → evaluated result
 * 
 * @param filter - Compliance filter (old or new format)
 * @param context - Filter context (optional, defaults to empty context)
 * @returns Numeric limit
 */
export function getFilterValue(filter: any, context: FilterContext = { pubKey: '' }): number {
	// Handle legacy discriminated union format
	if (filter && typeof filter === 'object' && 'type' in filter) {
		if (filter.type === 'blocked') return 0;
		if (filter.type === 'capped') return filter.value;
		if (filter.type === 'unlimited') return Infinity;
	}
	
	// Handle new JsonLogic format
	return evaluateComplianceFilter(filter, context);
}

// ═══════════════════════════════════════════════════════════════════
// CREATION HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create compliance filter from numeric value
 * 
 * @param value - Numeric limit
 * @returns Compliance filter
 */
export function createFilter(value: number): ComplianceFilter {
	if (value === 0) return 0; // Blocked
	if (value === Infinity || value < 0) return null; // Unlimited
	return value; // Simple cap
}

/**
 * Create conditional compliance filter based on attribute
 * 
 * @param attribute - Attribute path (e.g., "tier", "attributes.membership")
 * @param conditions - Map of attribute values to caps
 * @param defaultCap - Default cap if attribute doesn't match
 */
export function createConditionalFilter(
	attribute: string,
	conditions: Record<string, number>,
	defaultCap: number = 0
): ComplianceFilter {
	const entries = Object.entries(conditions);
	
	if (entries.length === 0) {
		return defaultCap;
	}
	
	// Build if-then-else chain
	let rule: any = defaultCap;
	
	for (let i = entries.length - 1; i >= 0; i--) {
		const [value, cap] = entries[i];
		rule = {
			"if": [
				{ "==": [{ "var": attribute }, value] },
				cap,
				rule
			]
		};
	}
	
	return rule;
}

// ═══════════════════════════════════════════════════════════════════
// UNION (MOST RESTRICTIVE WINS)
// ═══════════════════════════════════════════════════════════════════

/**
 * Union of compliance filters - most restrictive wins
 * 
 * PRINCIPLE: min(filter1, filter2)
 * 
 * This implements the "union of constraints" principle where
 * multiple filters combine to create the strictest possible limit.
 * 
 * NOTE: This evaluates filters immediately with the given context.
 * If you need lazy evaluation, store filters separately and evaluate later.
 * 
 * Supports both legacy and new filter formats.
 * 
 * @param filter1 - First compliance filter (old or new format)
 * @param filter2 - Second compliance filter (old or new format)
 * @param context - Context for evaluation (optional)
 * @returns Combined filter (most restrictive as literal number)
 */
export function unionOfFilters(
	filter1: any,
	filter2: any,
	context: FilterContext = { pubKey: '' }
): ComplianceFilter {
	const val1 = getFilterValue(filter1, context);
	const val2 = getFilterValue(filter2, context);
	return createFilter(Math.min(val1, val2));
}

/**
 * Union of multiple compliance filters
 * 
 * @param filters - Array of compliance filters
 * @param context - Context for evaluation
 * @returns Combined filter (most restrictive)
 */
export function unionOfMultipleFilters(
	filters: ComplianceFilter[],
	context: FilterContext = { pubKey: '' }
): ComplianceFilter {
	if (filters.length === 0) return null; // Unlimited
	if (filters.length === 1) return filters[0];
	
	// Evaluate all filters and take minimum
	const values = filters.map(f => evaluateComplianceFilter(f, context));
	const minValue = Math.min(...values);
	return createFilter(minValue);
}

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION CHECKING
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if a proposed allocation passes compliance filter
 * 
 * @param proposedAmount - Amount to allocate
 * @param currentTotal - Amount already allocated to this recipient
 * @param filter - Compliance filter to check
 * @param context - Context for evaluation (must include currentTotal, proposedAmount)
 * @returns FilterResult with pass/fail, reason, and effective limit
 */
export function passesComplianceFilter(
	proposedAmount: number,
	currentTotal: number,
	filter: ComplianceFilter,
	context?: FilterContext
): FilterResult {
	// Build context with allocation amounts
	const evalContext: FilterContext = {
		pubKey: context?.pubKey || '',
		commitment: context?.commitment,
		mutualRecognition: context?.mutualRecognition,
		attributes: context?.attributes,
		currentTotal,
		proposedAmount
	};
	
	const limit = evaluateComplianceFilter(filter, evalContext);
	const newTotal = currentTotal + proposedAmount;
	
	if (newTotal > limit) {
		return {
			passed: false,
			reason: limit === 0
				? 'Recipient is blocked'
				: `Would exceed cap: ${newTotal} > ${limit}`,
			effectiveLimit: limit,
			rawResult: limit
		};
	}
	
	return {
		passed: true,
		effectiveLimit: limit,
		rawResult: limit
	};
}

/**
 * Calculate remaining allocation room under compliance filter
 * 
 * @param currentTotal - Amount already allocated
 * @param filter - Compliance filter
 * @param context - Context for evaluation
 * @returns Remaining allocation room (0 if blocked, Infinity if unlimited)
 */
export function getRemainingRoom(
	currentTotal: number,
	filter: ComplianceFilter,
	context: FilterContext = { pubKey: '', currentTotal }
): number {
	const limit = evaluateComplianceFilter(filter, context);
	return Math.max(0, limit - currentTotal);
}

/**
 * Apply compliance filter to target allocation
 * 
 * Caps the target allocation at the filter limit.
 * 
 * @param targetAmount - Desired allocation amount
 * @param currentTotal - Amount already allocated
 * @param filter - Compliance filter
 * @param context - Context for evaluation
 * @returns Capped allocation amount
 */
export function applyComplianceFilter(
	targetAmount: number,
	currentTotal: number,
	filter: ComplianceFilter,
	context: FilterContext = { pubKey: '', currentTotal, proposedAmount: targetAmount }
): number {
	const remainingRoom = getRemainingRoom(currentTotal, filter, context);
	return Math.min(targetAmount, remainingRoom);
}

// ═══════════════════════════════════════════════════════════════════
// COMMON FILTER PATTERNS
// ═══════════════════════════════════════════════════════════════════

/**
 * Common compliance filter patterns
 */
export const ComplianceFilters = {
	/** Block a specific recipient */
	blocked: () => 0 as ComplianceFilter,
	
	/** Unlimited allocation */
	unlimited: () => null as ComplianceFilter,
	
	/** Simple numeric cap */
	cap: (amount: number) => amount as ComplianceFilter,
	
	/** Cap based on recipient's tier */
	tieredCap: (tiers: Record<string, number>, defaultCap: number = 0) =>
		createConditionalFilter('attributes.tier', tiers, defaultCap),
	
	/** Cap based on mutual recognition (dynamic) */
	trustBasedCap: (maxCap: number) => ({
		"*": [{ "var": "mutualRecognition" }, maxCap]
	}) as ComplianceFilter,
	
	/** Cap that increases with time (for rate limiting) */
	timeBasedCap: (baseAmount: number, perDayIncrease: number) => ({
		"+": [
			baseAmount,
			{
				"*": [
					{ "/": [{ "-": [Date.now(), 0] }, 86400000] }, // Days since epoch
					perDayIncrease
				]
			}
		]
	}) as ComplianceFilter
};
