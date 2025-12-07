/**
 * Unified Filter System - Core Types (Zod-based)
 * 
 * This module defines Zod schemas for all filter types, enabling:
 * - Runtime validation
 * - Type inference
 * - JSON serialization/deserialization
 * - Integration with the protocol's schema system
 * 
 * FILTER PHILOSOPHY:
 * All filters implement the "union of filters = most restrictive wins" principle:
 * - Numeric filters: min(filter1, filter2) → most restrictive limit
 * - Boolean filters: filter1 AND filter2 → intersection of allowed recipients
 * 
 * JSONLOGIC INTEGRATION:
 * Filters use JsonLogic for maximum expressiveness and serializability.
 * See: https://jsonlogic.com/operations.html
 */

import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// JSONLOGIC CORE SCHEMA
// ═══════════════════════════════════════════════════════════════════

/**
 * JsonLogic rule schema (recursive)
 * 
 * JsonLogic rules are JSON objects where:
 * - Keys are operators (e.g., "==", "and", "var", ">", "if")
 * - Values are arguments (primitives, arrays, or nested rules)
 * 
 * Examples:
 * - {"var": "mutualRecognition"} → Get variable
 * - {">=": [{"var": "mutualRecognition"}, 0.1]} → Check if >= 0.1
 * - {"and": [...rules]} → Logical AND of multiple rules
 */
export const JsonLogicRuleSchema: z.ZodType<any> = z.lazy(() =>
	z.union([
		z.record(z.string(), z.any()), // Object with operator key
		z.array(JsonLogicRuleSchema),   // Array of rules
		z.string(),                     // Primitive
		z.number(),
		z.boolean(),
		z.null()
	])
);

export type JsonLogicRule = z.infer<typeof JsonLogicRuleSchema>;

// ═══════════════════════════════════════════════════════════════════
// FILTER CONTEXT SCHEMA
// ═══════════════════════════════════════════════════════════════════

/**
 * Filter context schema
 * 
 * This is the data object passed to JsonLogic.apply() when evaluating filters.
 * It contains all information needed to make filtering decisions.
 * 
 * Context variables accessible in JsonLogic rules:
 * - {"var": "pubKey"} → Entity's public key
 * - {"var": "mutualRecognition"} → Mutual recognition value
 * - {"var": "commitment.city"} → Nested commitment data
 * - {"var": "attributes.certifications"} → Custom attributes
 */
export const FilterContextSchema = z.object({
	/** Public key or identifier of the entity being evaluated */
	pubKey: z.string(),
	
	/** Their commitment data (location, resource info, etc.) */
	commitment: z.record(z.string(), z.any()).optional(),
	
	/** Mutual recognition value (0-1 range) */
	mutualRecognition: z.number().optional(),
	
	/** Custom attributes (certifications, roles, etc.) */
	attributes: z.record(z.string(), z.any()).optional(),
	
	/** Current allocation total (for compliance checks) */
	currentTotal: z.number().optional(),
	
	/** Proposed allocation amount (for compliance checks) */
	proposedAmount: z.number().optional()
});

export type FilterContext = z.infer<typeof FilterContextSchema>;

// ═══════════════════════════════════════════════════════════════════
// COMPLIANCE FILTER SCHEMA (Numeric Capacity Limits)
// ═══════════════════════════════════════════════════════════════════

/**
 * Compliance Filter Schema
 * 
 * Determines HOW MUCH a recipient can receive overall (not per-slot).
 * 
 * A compliance filter is a JsonLogic rule that returns a number:
 * - 0 = blocked
 * - Positive number = capped at that value
 * - Infinity = unlimited (represented as null in JSON)
 * 
 * Examples:
 * - Simple cap: {"if": [{"var": "pubKey"}, 50000, 0]} → Cap at $50K
 * - Conditional: {"if": [{"==": [{"var": "attributes.tier"}, "premium"]}, 100000, 50000]}
 * - Dynamic: {"+": [{"var": "currentTotal"}, 10000]} → Allow $10K more
 * - Blocked: 0 (literal)
 * - Unlimited: null (interpreted as Infinity)
 * 
 * Union principle: When multiple compliance filters apply, min(filter1, filter2)
 */
export const ComplianceFilterSchema = z.union([
	JsonLogicRuleSchema,  // JsonLogic rule that returns a number
	z.number(),           // Literal number (simple cap)
	z.null()              // null = unlimited (Infinity)
]);

export type ComplianceFilter = z.infer<typeof ComplianceFilterSchema>;

// ═══════════════════════════════════════════════════════════════════
// ELIGIBILITY FILTER SCHEMA (Boolean Slot-Level Matching)
// ═══════════════════════════════════════════════════════════════════

/**
 * Eligibility Filter Schema
 * 
 * Determines WHETHER a specific slot can be allocated (yes/no per slot).
 * 
 * An eligibility filter is a JsonLogic rule that returns a boolean.
 * 
 * Examples:
 * - Trust requirement: {">=": [{"var": "mutualRecognition"}, 0.1]}
 * - Location requirement: {"in": [{"var": "commitment.city"}, ["SF", "NYC", "Berlin"]]}
 * - Certification: {"in": ["licensed", {"var": "attributes.certifications"}]}
 * - Complex: {"and": [
 *     {">=": [{"var": "mutualRecognition"}, 0.1]},
 *     {"in": [{"var": "commitment.city"}, ["SF", "NYC"]]}
 *   ]}
 * - Allow all: true (literal)
 * - Deny all: false (literal)
 * 
 * Union principle: When multiple eligibility filters apply, filter1 AND filter2
 */
export const EligibilityFilterSchema = z.union([
	JsonLogicRuleSchema,  // JsonLogic rule that returns a boolean
	z.boolean()           // Literal boolean (allow_all=true, deny_all=false)
]);

export type EligibilityFilter = z.infer<typeof EligibilityFilterSchema>;

// ═══════════════════════════════════════════════════════════════════
// FILTER EVALUATION RESULT SCHEMA
// ═══════════════════════════════════════════════════════════════════

/**
 * Filter evaluation result schema
 */
export const FilterResultSchema = z.object({
	/** Whether the filter passed */
	passed: z.boolean(),
	
	/** Reason for rejection (if failed) */
	reason: z.string().optional(),
	
	/** Effective limit (for compliance filters) */
	effectiveLimit: z.number().optional(),
	
	/** Raw result from JsonLogic evaluation */
	rawResult: z.any().optional()
});

export type FilterResult = z.infer<typeof FilterResultSchema>;

// ═══════════════════════════════════════════════════════════════════
// HELPER SCHEMAS FOR COMMON FILTER PATTERNS
// ═══════════════════════════════════════════════════════════════════

/**
 * Common filter patterns (for convenience, not required)
 * 
 * These are helpers to construct JsonLogic rules more easily.
 * Users can also write raw JsonLogic directly.
 */
export const FilterPatterns = {
	/** Trust-based filter: require minimum mutual recognition */
	trust: (minMR: number) => ({
		">=": [{ "var": "mutualRecognition" }, minMR]
	}),
	
	/** Location filter: require city in list */
	cityIn: (cities: string[]) => ({
		"in": [{ "var": "commitment.city" }, cities]
	}),
	
	/** Attribute filter: require attribute exists */
	hasAttribute: (attr: string) => ({
		"!!": { "var": `attributes.${attr}` }
	}),
	
	/** Certification filter: require certification in list */
	hasCertification: (cert: string) => ({
		"in": [cert, { "var": "attributes.certifications" }]
	}),
	
	/** Compliance cap: simple numeric cap */
	cap: (amount: number) => amount,
	
	/** Compliance cap: conditional based on tier */
	tieredCap: (premiumCap: number, regularCap: number) => ({
		"if": [
			{ "==": [{ "var": "attributes.tier" }, "premium"] },
			premiumCap,
			regularCap
		]
	}),
	
	/** Logical AND of multiple filters */
	and: (...rules: JsonLogicRule[]) => ({
		"and": rules
	}),
	
	/** Logical OR of multiple filters */
	or: (...rules: JsonLogicRule[]) => ({
		"or": rules
	}),
	
	/** Logical NOT */
	not: (rule: JsonLogicRule) => ({
		"!": rule
	})
};

// All schemas and types are already exported at their definition sites above.
// No need for additional exports here.
