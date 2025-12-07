/**
 * Eligibility Filters - Boolean Slot-Level Matching (JsonLogic-based)
 * 
 * This module handles WHETHER a specific slot can be allocated (yes/no) using JsonLogic.
 * 
 * USE CASES:
 * - Trust requirements: "Only allocate to people I mutually recognize"
 * - Location restrictions: "Only serve people in SF, NYC, or Berlin"
 * - Certification requirements: "Provider must be licensed"
 * - Attribute requirements: "Must have specific qualification"
 * - Complex rules: "Must have mutual recognition AND (be in SF OR be certified)"
 * 
 * UNION PRINCIPLE:
 * When multiple eligibility filters apply, ALL must pass (AND logic):
 * - filter1 AND filter2 AND ... AND filterN
 * - Any rejection blocks the allocation
 * 
 * BILATERAL FILTERING:
 * - Provider's filter: "Who can receive from my slot?"
 * - Recipient's filter: "Who can provide to my need?"
 * - Both must pass for slot-to-slot allocation
 * 
 * JSONLOGIC USAGE:
 * Eligibility filters are JsonLogic rules that return booleans:
 * ```typescript
 * // Trust requirement
 * const filter = {">=": [{"var": "mutualRecognition"}, 0.1]};
 * 
 * // Location requirement
 * const filter = {"in": [{"var": "commitment.city"}, ["SF", "NYC", "Berlin"]]};
 * 
 * // Certification requirement
 * const filter = {"in": ["licensed", {"var": "attributes.certifications"}]};
 * 
 * // Complex composite
 * const filter = {
 *   "and": [
 *     {">=": [{"var": "mutualRecognition"}, 0.1]},
 *     {"or": [
 *       {"==": [{"var": "commitment.city"}, "SF"]},
 *       {"in": ["licensed", {"var": "attributes.certifications"}]}
 *     ]}
 *   ]
 * };
 * 
 * // Allow all
 * const filter = true;
 * 
 * // Deny all
 * const filter = false;
 * ```
 */

import { z } from 'zod';
// @ts-ignore - json-logic-js doesn't have perfect types
import jsonLogic from 'json-logic-js';
import {
	EligibilityFilterSchema,
	type EligibilityFilter,
	type FilterContext,
	FilterContextSchema,
	type FilterResult,
	FilterResultSchema
} from './types';

// ═══════════════════════════════════════════════════════════════════
// CACHING FOR PERFORMANCE
// ═══════════════════════════════════════════════════════════════════

/**
 * LRU cache for eligibility filter evaluation results
 * 
 * Caching is CRITICAL here because eligibility filters are evaluated
 * 500-20,000 times per allocation cycle (every slot pair check).
 * 
 * With caching, we get:
 * - First evaluation: ~0.01-0.5ms (depending on complexity)
 * - Cached evaluations: ~0.001ms (10-100x faster)
 * 
 * Potential savings: 10-500ms → 1-2ms per allocation cycle
 */
class EligibilityFilterCache {
	private cache = new Map<string, boolean>();
	private maxSize = 2000; // Higher limit than compliance (more combinations)
	
	get(key: string): boolean | undefined {
		return this.cache.get(key);
	}
	
	set(key: string, value: boolean): void {
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

const eligibilityFilterCache = new EligibilityFilterCache();

/**
 * Generate cache key for eligibility filter evaluation
 * 
 * Key includes:
 * - Filter rule (stringified)
 * - Context pubKey
 * - Mutual recognition (for trust filters)
 * - City (for location filters)
 */
function getEligibilityCacheKey(filter: EligibilityFilter, context: FilterContext): string {
	// For boolean literals, cache key is just the value
	if (typeof filter === 'boolean') return `bool:${filter}`;
	
	// For JsonLogic rules, include relevant context
	const mr = context.mutualRecognition || 0;
	const city = context.commitment?.city || 'none';
	return `${JSON.stringify(filter)}:${context.pubKey}:${mr}:${city}`;
}

// ═══════════════════════════════════════════════════════════════════
// EVALUATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Evaluate eligibility filter to get boolean result
 * 
 * @param filter - Eligibility filter (JsonLogic rule or boolean)
 * @param context - Filter context
 * @returns Boolean (true = pass, false = reject)
 */
export function evaluateEligibilityFilter(
	filter: EligibilityFilter,
	context: FilterContext,
	useCache: boolean = true
): boolean {
	// Validate inputs
	try {
		EligibilityFilterSchema.parse(filter);
		FilterContextSchema.parse(context);
	} catch (error) {
		console.error('[ELIGIBILITY-FILTER] Validation error:', error);
		return false;
	}
	
	// FAST PATH: Simple literals (no caching needed)
	if (filter === null || filter === undefined) {
		return true;
	}
	if (typeof filter === 'boolean') {
		return filter;
	}
	
	// Check cache first
	if (useCache) {
		const cacheKey = getEligibilityCacheKey(filter, context);
		const cached = eligibilityFilterCache.get(cacheKey);
		if (cached !== undefined) {
			return cached;
		}
	}
	
	// SLOW PATH: JsonLogic evaluation
	let result: boolean;
	try {
		const evaluated = jsonLogic.apply(filter, context);
		
		if (typeof evaluated === 'boolean') {
			result = evaluated;
		} else if (Array.isArray(evaluated)) {
			result = evaluated.length > 0;
		} else {
			result = Boolean(evaluated);
		}
	} catch (error) {
		console.error('[ELIGIBILITY-FILTER] Error evaluating filter:', error, filter);
		result = false;
	}
	
	// Cache the result
	if (useCache) {
		const cacheKey = getEligibilityCacheKey(filter, context);
		eligibilityFilterCache.set(cacheKey, result);
	}
	
	return result;
}

/**
 * Clear the eligibility filter cache
 * 
 * Call this when:
 * - Starting a new allocation cycle
 * - Context data has changed significantly
 */
export function clearEligibilityFilterCache(): void {
	eligibilityFilterCache.clear();
}

/**
 * Get cache statistics for monitoring/debugging
 */
export function getEligibilityFilterCacheStats(): { size: number; maxSize: number } {
	return {
		size: eligibilityFilterCache.size,
		maxSize: 2000
	};
}

/**
 * Evaluate filter with detailed result
 * 
 * @param filter - Eligibility filter
 * @param context - Filter context
 * @returns FilterResult with pass/fail and reason
 */
export function evaluateFilter(
	filter: EligibilityFilter | null | undefined,
	context: FilterContext
): FilterResult {
	// No filter = pass by default (optimistic)
	if (filter === null || filter === undefined) {
		return {
			passed: true,
			rawResult: true
		};
	}
	
	const passed = evaluateEligibilityFilter(filter, context);
	
	return {
		passed,
		reason: passed ? undefined : 'Filter rejected',
		rawResult: passed
	};
}

// ═══════════════════════════════════════════════════════════════════
// BILATERAL FILTERING
// ═══════════════════════════════════════════════════════════════════

/**
 * Check if a provider-recipient pair passes bilateral filters
 * 
 * BILATERAL FILTER CHECKING:
 * - Capacity filter (availabilitySlot.filter_rule): Does recipient pass provider's filter?
 * - Need filter (needSlot.filter_rule): Does provider pass recipient's filter?
 * - Both must pass for allocation to occur
 * 
 * @param providerFilter - Provider's filter on recipients
 * @param recipientFilter - Recipient's filter on providers
 * @param providerContext - Provider's context for evaluation
 * @param recipientContext - Recipient's context for evaluation
 * @returns FilterResult with pass/fail and reason
 */
export function passesSlotFilters(
	providerFilter: EligibilityFilter | null | undefined,
	recipientFilter: EligibilityFilter | null | undefined,
	providerContext: FilterContext,
	recipientContext: FilterContext
): FilterResult {
	// Check provider's filter (does recipient pass?)
	if (providerFilter) {
		const providerResult = evaluateFilter(providerFilter, recipientContext);
		if (!providerResult.passed) {
			return {
				passed: false,
				reason: `Recipient failed provider's filter: ${providerResult.reason || 'rejected'}`,
				rawResult: providerResult
			};
		}
	}
	
	// Check recipient's filter (does provider pass?)
	if (recipientFilter) {
		const recipientResult = evaluateFilter(recipientFilter, providerContext);
		if (!recipientResult.passed) {
			return {
				passed: false,
				reason: `Provider failed recipient's filter: ${recipientResult.reason || 'rejected'}`,
				rawResult: recipientResult
			};
		}
	}
	
	// Both filters passed (or no filters present)
	return {
		passed: true,
		rawResult: true
	};
}

// ═══════════════════════════════════════════════════════════════════
// UNION (ALL MUST PASS)
// ═══════════════════════════════════════════════════════════════════

/**
 * Union of eligibility filters - ALL must pass (AND logic)
 * 
 * Unlike compliance filters (which use min/most restrictive),
 * eligibility filters use AND logic (all must pass).
 * 
 * @param filters - Array of eligibility filters
 * @param context - Context to evaluate against
 * @returns FilterResult with pass/fail and reason
 */
export function passesAllEligibilityFilters(
	filters: EligibilityFilter[],
	context: FilterContext
): FilterResult {
	for (const filter of filters) {
		const result = evaluateFilter(filter, context);
		if (!result.passed) {
			return result;
		}
	}
	
	return {
		passed: true,
		rawResult: true
	};
}

/**
 * Create a composite eligibility filter from multiple filters
 * 
 * Returns a JsonLogic rule that ANDs all filters together.
 * 
 * @param filters - Array of filters to combine
 * @returns Combined filter (JsonLogic AND rule)
 */
export function createCompositeFilter(filters: EligibilityFilter[]): EligibilityFilter {
	if (filters.length === 0) return true; // No filters = allow all
	if (filters.length === 1) return filters[0]; // Single filter
	
	// Convert all to JsonLogic rules
	const rules = filters.map(f => {
		if (typeof f === 'boolean') {
			return f; // Keep booleans as-is
		}
		return f; // Already a JsonLogic rule
	});
	
	// Build AND rule
	return {
		"and": rules
	};
}

// ═══════════════════════════════════════════════════════════════════
// COMMON FILTER PATTERNS
// ═══════════════════════════════════════════════════════════════════

/**
 * Common eligibility filter patterns
 */
export const EligibilityFilters = {
	/** Allow all recipients/providers */
	allowAll: () => true as EligibilityFilter,
	
	/** Deny all recipients/providers */
	denyAll: () => false as EligibilityFilter,
	
	/** Require minimum mutual recognition */
	trust: (minMR: number) => ({
		">=": [{ "var": "mutualRecognition" }, minMR]
	}) as EligibilityFilter,
	
	/** Require only mutual recognition (MR > 0) */
	onlyMutual: () => ({
		">": [{ "var": "mutualRecognition" }, 0]
	}) as EligibilityFilter,
	
	/** Require city in list */
	cityIn: (cities: string[]) => ({
		"in": [{ "var": "commitment.city" }, cities]
	}) as EligibilityFilter,
	
	/** Require country in list */
	countryIn: (countries: string[]) => ({
		"in": [{ "var": "commitment.country" }, countries]
	}) as EligibilityFilter,
	
	/** Require specific attribute exists */
	hasAttribute: (attrPath: string) => ({
		"!!": { "var": attrPath }
	}) as EligibilityFilter,
	
	/** Require specific certification */
	hasCertification: (cert: string) => ({
		"in": [cert, { "var": "attributes.certifications" }]
	}) as EligibilityFilter,
	
	/** Require any of multiple certifications */
	hasAnyCertification: (certs: string[]) => ({
		"some": [
			certs,
			{
				"in": [{ "var": "" }, { "var": "attributes.certifications" }]
			}
		]
	}) as EligibilityFilter,
	
	/** Require all of multiple certifications */
	hasAllCertifications: (certs: string[]) => ({
		"all": [
			certs,
			{
				"in": [{ "var": "" }, { "var": "attributes.certifications" }]
			}
		]
	}) as EligibilityFilter,
	
	/** Require resource type in list */
	resourceTypeIn: (types: string[]) => ({
		"in": [{ "var": "commitment.resource_type" }, types]
	}) as EligibilityFilter,
	
	/** AND combination of multiple filters */
	and: (...filters: EligibilityFilter[]) => ({
		"and": filters
	}) as EligibilityFilter,
	
	/** OR combination of multiple filters */
	or: (...filters: EligibilityFilter[]) => ({
		"or": filters
	}) as EligibilityFilter,
	
	/** NOT negation */
	not: (filter: EligibilityFilter) => ({
		"!": filter
	}) as EligibilityFilter
};
