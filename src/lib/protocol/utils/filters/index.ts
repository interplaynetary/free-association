/**
 * Unified Filter System - Main Entry Point
 * 
 * This module provides a complete filter system for the Free Association Protocol:
 * 
 * 1. **Zod Schemas**: Runtime validation and type safety
 * 2. **JsonLogic**: Dynamic, serializable filter rules
 * 3. **Compliance Filters**: Numeric capacity limits (how much)
 * 4. **Eligibility Filters**: Boolean slot matching (who/whether)
 * 
 * ## Core Principle: "Union of Filters = Most Restrictive Wins"
 * 
 * - **Compliance filters**: min(filter1, filter2) → strictest limit
 * - **Eligibility filters**: filter1 AND filter2 → intersection of allowed
 * 
 * ## JsonLogic Integration
 * 
 * Filters use JsonLogic for maximum expressiveness:
 * - **See**: https://jsonlogic.com/operations.html
 * - **Why**: Serializable, dynamic, extensible, language-agnostic
 * 
 * ## Example Usage
 * 
 * ```typescript
 * import {
 *   ComplianceFilters,
 *   EligibilityFilters,
 *   evaluateComplianceFilter,
 *   evaluateEligibilityFilter
 * } from '$lib/protocol/utils/filters';
 * 
 * // Compliance filter: Cap at $50K for regular, $100K for premium
 * const complianceFilter = ComplianceFilters.tieredCap({
 *   premium: 100000,
 *   regular: 50000
 * });
 * 
 * // Eligibility filter: Require mutual recognition AND (SF OR certified)
 * const eligibilityFilter = EligibilityFilters.and(
 *   EligibilityFilters.trust(0.1),
 *   EligibilityFilters.or(
 *     EligibilityFilters.cityIn(['SF']),
 *     EligibilityFilters.hasCertification('licensed')
 *   )
 * );
 * 
 * // Evaluate
 * const context = {
 *   pubKey: 'alice123',
 *   mutualRecognition: 0.15,
 *   attributes: { tier: 'premium', certifications: ['licensed'] },
 *   commitment: { city: 'SF' },
 *   currentTotal: 0
 * };
 * 
 * const limit = evaluateComplianceFilter(complianceFilter, context);
 * // → 100000 (premium tier)
 * 
 * const eligible = evaluateEligibilityFilter(eligibilityFilter, context);
 * // → true (has MR ≥ 0.1 AND is in SF)
 * ```
 * 
 * ## Architecture
 * 
 * ```
 * src/lib/protocol/utils/filters/
 * ├── index.ts          // This file - unified exports
 * ├── types.ts          // Zod schemas and type definitions
 * ├── compliance.ts     // Numeric capacity limits (how much)
 * └── eligibility.ts    // Boolean slot matching (who/whether)
 * ```
 */

// ═══════════════════════════════════════════════════════════════════
// TYPE EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
	// Core types
	type JsonLogicRule,
	type FilterContext,
	type FilterResult,
	type ComplianceFilter,
	type EligibilityFilter,
	
	// Zod schemas
	JsonLogicRuleSchema,
	FilterContextSchema,
	FilterResultSchema,
	ComplianceFilterSchema,
	EligibilityFilterSchema,
	
	// Helper patterns
	FilterPatterns
} from './types';

// ═══════════════════════════════════════════════════════════════════
// COMPLIANCE FILTER EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
	// Evaluation
	evaluateComplianceFilter,
	getFilterValue,
	
	// Creation
	createFilter,
	createConditionalFilter,
	
	// Union (most restrictive wins)
	unionOfFilters,
	unionOfMultipleFilters,
	
	// Allocation checking
	passesComplianceFilter,
	getRemainingRoom,
	applyComplianceFilter,
	
	// Cache management
	clearComplianceFilterCache,
	getComplianceFilterCacheStats,
	
	// Common patterns
	ComplianceFilters
} from './compliance';

// ═══════════════════════════════════════════════════════════════════
// ELIGIBILITY FILTER EXPORTS
// ═══════════════════════════════════════════════════════════════════

export {
	// Evaluation
	evaluateEligibilityFilter,
	evaluateFilter,
	
	// Bilateral filtering
	passesSlotFilters,
	
	// Union (all must pass)
	passesAllEligibilityFilters,
	createCompositeFilter,
	
	// Cache management
	clearEligibilityFilterCache,
	getEligibilityFilterCacheStats,
	
	// Common patterns
	EligibilityFilters
} from './eligibility';

// ═══════════════════════════════════════════════════════════════════
// UTILITY FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Validate a compliance filter
 * 
 * @param filter - Filter to validate
 * @returns Validation result
 */
export function validateComplianceFilter(filter: unknown): {
	valid: boolean;
	error?: string;
} {
	const { ComplianceFilterSchema } = require('./types');
	const result = ComplianceFilterSchema.safeParse(filter);
	
	if (result.success) {
		return { valid: true };
	}
	
	return {
		valid: false,
		error: result.error.message
	};
}

/**
 * Validate an eligibility filter
 * 
 * @param filter - Filter to validate
 * @returns Validation result
 */
export function validateEligibilityFilter(filter: unknown): {
	valid: boolean;
	error?: string;
} {
	const { EligibilityFilterSchema } = require('./types');
	const result = EligibilityFilterSchema.safeParse(filter);
	
	if (result.success) {
		return { valid: true };
	}
	
	return {
		valid: false,
		error: result.error.message
	};
}

/**
 * Validate a filter context
 * 
 * @param context - Context to validate
 * @returns Validation result
 */
export function validateFilterContext(context: unknown): {
	valid: boolean;
	error?: string;
} {
	const { FilterContextSchema } = require('./types');
	const result = FilterContextSchema.safeParse(context);
	
	if (result.success) {
		return { valid: true };
	}
	
	return {
		valid: false,
		error: result.error.message
	};
}

// ═══════════════════════════════════════════════════════════════════
// CACHE MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Clear all filter caches
 * 
 * Call this at the start of each allocation cycle to ensure fresh evaluations.
 * Caches use ~300KB memory for 3000 entries, so clearing periodically is recommended.
 */
export function clearAllFilterCaches(): void {
	const { clearComplianceFilterCache } = require('./compliance');
	const { clearEligibilityFilterCache } = require('./eligibility');
	
	clearComplianceFilterCache();
	clearEligibilityFilterCache();
}

/**
 * Get statistics for all filter caches
 * 
 * Useful for monitoring cache hit rates and memory usage.
 */
export function getAllFilterCacheStats(): {
	compliance: { size: number; maxSize: number };
	eligibility: { size: number; maxSize: number };
} {
	const { getComplianceFilterCacheStats } = require('./compliance');
	const { getEligibilityFilterCacheStats } = require('./eligibility');
	
	return {
		compliance: getComplianceFilterCacheStats(),
		eligibility: getEligibilityFilterCacheStats()
	};
}
