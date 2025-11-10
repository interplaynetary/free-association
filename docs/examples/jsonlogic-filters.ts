/**
 * JsonLogic Filter System - Usage Examples
 * 
 * This file demonstrates how to use the unified JsonLogic-based filter system
 * for compliance filters (numeric limits) and eligibility filters (boolean matching).
 * 
 * See: docs/UNIFIED_FILTER_SYSTEM.md for full documentation
 */

import {
	// Compliance filters (how much)
	ComplianceFilters,
	evaluateComplianceFilter,
	unionOfFilters,
	passesComplianceFilter,
	
	// Eligibility filters (who/whether)
	EligibilityFilters,
	evaluateEligibilityFilter,
	passesSlotFilters,
	
	// Types
	type ComplianceFilter,
	type EligibilityFilter,
	type FilterContext
} from '$lib/protocol/utils/filters';

// ═══════════════════════════════════════════════════════════════════
// COMPLIANCE FILTERS (NUMERIC CAPACITY LIMITS)
// ═══════════════════════════════════════════════════════════════════

console.log('═══════════════════════════════════════════════════════════');
console.log('COMPLIANCE FILTERS - How much a recipient can receive');
console.log('═══════════════════════════════════════════════════════════\n');

// Example 1: Simple numeric cap
const simpleCap: ComplianceFilter = 50000;

const context1: FilterContext = {
	pubKey: 'alice',
	mutualRecognition: 0.5,
	attributes: { tier: 'regular' }
};

const limit1 = evaluateComplianceFilter(simpleCap, context1);
console.log('Example 1: Simple cap');
console.log(`  Filter: 50000`);
console.log(`  Result: ${limit1}`);
console.log(`  Interpretation: Alice is capped at $50,000\n`);

// Example 2: Conditional cap based on tier
const tieredCap: ComplianceFilter = {
	"if": [
		{ "==": [{ "var": "attributes.tier" }, "premium"] },
		100000,  // Premium tier: $100K
		50000    // Regular tier: $50K
	]
};

const context2a: FilterContext = {
	pubKey: 'bob',
	attributes: { tier: 'premium' }
};

const context2b: FilterContext = {
	pubKey: 'carol',
	attributes: { tier: 'regular' }
};

console.log('Example 2: Conditional cap based on tier');
console.log(`  Filter: {"if": [{"==": [{"var": "tier"}, "premium"]}, 100000, 50000]}`);
console.log(`  Bob (premium): ${evaluateComplianceFilter(tieredCap, context2a)}`);
console.log(`  Carol (regular): ${evaluateComplianceFilter(tieredCap, context2b)}`);
console.log(`  Interpretation: Premium members get higher caps\n`);

// Example 3: Dynamic cap based on mutual recognition (trust-based)
const trustBasedCap: ComplianceFilter = {
	"*": [{ "var": "mutualRecognition" }, 100000]
};

const context3a: FilterContext = {
	pubKey: 'dave',
	mutualRecognition: 0.8
};

const context3b: FilterContext = {
	pubKey: 'eve',
	mutualRecognition: 0.2
};

console.log('Example 3: Dynamic cap based on mutual recognition');
console.log(`  Filter: {"*": [{"var": "mutualRecognition"}, 100000]}`);
console.log(`  Dave (MR=0.8): $${evaluateComplianceFilter(trustBasedCap, context3a)}`);
console.log(`  Eve (MR=0.2): $${evaluateComplianceFilter(trustBasedCap, context3b)}`);
console.log(`  Interpretation: Cap scales with trust level\n`);

// Example 4: Blocked recipient
const blocked: ComplianceFilter = 0;

const context4: FilterContext = { pubKey: 'mallory' };

console.log('Example 4: Blocked recipient');
console.log(`  Filter: 0`);
console.log(`  Result: ${evaluateComplianceFilter(blocked, context4)}`);
console.log(`  Interpretation: Mallory receives nothing\n`);

// Example 5: Unlimited allocation
const unlimited: ComplianceFilter = null;

const context5: FilterContext = { pubKey: 'alice' };

console.log('Example 5: Unlimited allocation');
console.log(`  Filter: null`);
console.log(`  Result: ${evaluateComplianceFilter(unlimited, context5)}`);
console.log(`  Interpretation: Alice has no cap\n`);

// Example 6: Union of filters (most restrictive wins)
const providerCap = 100000;
const entityCap = 50000;

const context6: FilterContext = { pubKey: 'alice' };

const combinedCap = unionOfFilters(providerCap, entityCap, context6);

console.log('Example 6: Union of filters (most restrictive wins)');
console.log(`  Provider cap: $100,000`);
console.log(`  Entity cap: $50,000`);
console.log(`  Combined cap: $${evaluateComplianceFilter(combinedCap, context6)}`);
console.log(`  Interpretation: The lower cap (entity's $50K) prevails\n`);

// Example 7: Check if allocation passes compliance filter
const proposedAllocation = 30000;
const currentTotal = 25000;
const cap = 50000;

const context7: FilterContext = { pubKey: 'alice', currentTotal, proposedAmount: proposedAllocation };

const result7 = passesComplianceFilter(proposedAllocation, currentTotal, cap, context7);

console.log('Example 7: Check allocation against filter');
console.log(`  Current total: $${currentTotal}`);
console.log(`  Proposed: $${proposedAllocation}`);
console.log(`  Cap: $${cap}`);
console.log(`  Passes: ${result7.passed}`);
console.log(`  Reason: ${result7.reason || 'Within limits'}`);
console.log(`  New total would be: $${currentTotal + proposedAllocation}\n`);

// Example 8: Using helper patterns
const helper1 = ComplianceFilters.cap(50000);
const helper2 = ComplianceFilters.blocked();
const helper3 = ComplianceFilters.unlimited();
const helper4 = ComplianceFilters.tieredCap({
	premium: 100000,
	regular: 50000,
	basic: 25000
}, 10000);

console.log('Example 8: Using helper patterns');
console.log(`  ComplianceFilters.cap(50000): ${evaluateComplianceFilter(helper1, { pubKey: 'test' })}`);
console.log(`  ComplianceFilters.blocked(): ${evaluateComplianceFilter(helper2, { pubKey: 'test' })}`);
console.log(`  ComplianceFilters.unlimited(): ${evaluateComplianceFilter(helper3, { pubKey: 'test' })}`);
console.log(`  ComplianceFilters.tieredCap() [premium]: ${evaluateComplianceFilter(helper4, { pubKey: 'test', attributes: { tier: 'premium' } })}`);
console.log(`  ComplianceFilters.tieredCap() [regular]: ${evaluateComplianceFilter(helper4, { pubKey: 'test', attributes: { tier: 'regular' } })}`);
console.log(`  ComplianceFilters.tieredCap() [unknown]: ${evaluateComplianceFilter(helper4, { pubKey: 'test', attributes: { tier: 'unknown' } })}\n`);

// ═══════════════════════════════════════════════════════════════════
// ELIGIBILITY FILTERS (BOOLEAN SLOT-LEVEL MATCHING)
// ═══════════════════════════════════════════════════════════════════

console.log('\n═══════════════════════════════════════════════════════════');
console.log('ELIGIBILITY FILTERS - Who can participate in allocation');
console.log('═══════════════════════════════════════════════════════════\n');

// Example 9: Trust-based eligibility
const trustFilter: EligibilityFilter = {
	">=": [{ "var": "mutualRecognition" }, 0.1]
};

const context9a: FilterContext = {
	pubKey: 'alice',
	mutualRecognition: 0.15
};

const context9b: FilterContext = {
	pubKey: 'bob',
	mutualRecognition: 0.05
};

console.log('Example 9: Trust-based eligibility (require MR ≥ 0.1)');
console.log(`  Filter: {">=": [{"var": "mutualRecognition"}, 0.1]}`);
console.log(`  Alice (MR=0.15): ${evaluateEligibilityFilter(trustFilter, context9a)}`);
console.log(`  Bob (MR=0.05): ${evaluateEligibilityFilter(trustFilter, context9b)}`);
console.log(`  Interpretation: Only Alice passes the trust threshold\n`);

// Example 10: Location-based eligibility
const locationFilter: EligibilityFilter = {
	"in": [{ "var": "commitment.city" }, ["SF", "NYC", "Berlin"]]
};

const context10a: FilterContext = {
	pubKey: 'carol',
	commitment: { city: 'SF' }
};

const context10b: FilterContext = {
	pubKey: 'dave',
	commitment: { city: 'LA' }
};

console.log('Example 10: Location-based eligibility');
console.log(`  Filter: {"in": [{"var": "commitment.city"}, ["SF", "NYC", "Berlin"]]}`);
console.log(`  Carol (SF): ${evaluateEligibilityFilter(locationFilter, context10a)}`);
console.log(`  Dave (LA): ${evaluateEligibilityFilter(locationFilter, context10b)}`);
console.log(`  Interpretation: Only allowed cities can participate\n`);

// Example 11: Certification requirement
const certFilter: EligibilityFilter = {
	"in": ["licensed", { "var": "attributes.certifications" }]
};

const context11a: FilterContext = {
	pubKey: 'eve',
	attributes: { certifications: ['licensed', 'insured'] }
};

const context11b: FilterContext = {
	pubKey: 'frank',
	attributes: { certifications: ['insured'] }
};

console.log('Example 11: Certification requirement');
console.log(`  Filter: {"in": ["licensed", {"var": "attributes.certifications"}]}`);
console.log(`  Eve (licensed + insured): ${evaluateEligibilityFilter(certFilter, context11a)}`);
console.log(`  Frank (insured only): ${evaluateEligibilityFilter(certFilter, context11b)}`);
console.log(`  Interpretation: Must have 'licensed' certification\n`);

// Example 12: Complex composite filter (AND + OR)
const complexFilter: EligibilityFilter = {
	"and": [
		{ ">=": [{ "var": "mutualRecognition" }, 0.1] },  // Must have trust
		{
			"or": [
				{ "==": [{ "var": "commitment.city" }, "SF"] },  // Either in SF
				{ "in": ["licensed", { "var": "attributes.certifications" }] }  // Or licensed
			]
		}
	]
};

const context12a: FilterContext = {
	pubKey: 'grace',
	mutualRecognition: 0.15,
	commitment: { city: 'SF' },
	attributes: { certifications: [] }
};

const context12b: FilterContext = {
	pubKey: 'heidi',
	mutualRecognition: 0.15,
	commitment: { city: 'NYC' },
	attributes: { certifications: ['licensed'] }
};

const context12c: FilterContext = {
	pubKey: 'ivan',
	mutualRecognition: 0.05,  // Too low!
	commitment: { city: 'SF' },
	attributes: { certifications: ['licensed'] }
};

console.log('Example 12: Complex composite filter');
console.log(`  Filter: MR ≥ 0.1 AND (city=SF OR has 'licensed')`);
console.log(`  Grace (MR=0.15, SF, no cert): ${evaluateEligibilityFilter(complexFilter, context12a)}`);
console.log(`  Heidi (MR=0.15, NYC, licensed): ${evaluateEligibilityFilter(complexFilter, context12b)}`);
console.log(`  Ivan (MR=0.05, SF, licensed): ${evaluateEligibilityFilter(complexFilter, context12c)}`);
console.log(`  Interpretation: Grace and Heidi pass, Ivan fails (insufficient trust)\n`);

// Example 13: Bilateral filtering (provider + recipient filters)
const providerFilter: EligibilityFilter = {
	">=": [{ "var": "mutualRecognition" }, 0.1]  // Provider requires MR ≥ 0.1
};

const recipientFilter: EligibilityFilter = {
	"in": ["licensed", { "var": "attributes.certifications" }]  // Recipient requires licensed provider
};

const providerContext: FilterContext = {
	pubKey: 'provider-alice',
	attributes: { certifications: ['licensed'] }
};

const recipientContext13a: FilterContext = {
	pubKey: 'recipient-bob',
	mutualRecognition: 0.15
};

const recipientContext13b: FilterContext = {
	pubKey: 'recipient-charlie',
	mutualRecognition: 0.05  // Too low!
};

const result13a = passesSlotFilters(providerFilter, recipientFilter, providerContext, recipientContext13a);
const result13b = passesSlotFilters(providerFilter, recipientFilter, providerContext, recipientContext13b);

console.log('Example 13: Bilateral filtering');
console.log(`  Provider filter: Require MR ≥ 0.1`);
console.log(`  Recipient filter: Require 'licensed' certification`);
console.log(`  Provider: Alice (licensed)`);
console.log(`  Recipient Bob (MR=0.15): ${result13a.passed} - ${result13a.reason || 'Both filters pass'}`);
console.log(`  Recipient Charlie (MR=0.05): ${result13b.passed} - ${result13b.reason}`);
console.log(`  Interpretation: Both provider's and recipient's filters must pass\n`);

// Example 14: Using helper patterns
const helperTrust = EligibilityFilters.trust(0.1);
const helperCity = EligibilityFilters.cityIn(['SF', 'NYC']);
const helperCert = EligibilityFilters.hasCertification('licensed');
const helperComposite = EligibilityFilters.and(
	EligibilityFilters.trust(0.1),
	EligibilityFilters.or(
		EligibilityFilters.cityIn(['SF']),
		EligibilityFilters.hasCertification('licensed')
	)
);

console.log('Example 14: Using helper patterns');
console.log(`  EligibilityFilters.trust(0.1):`);
console.log(`    - Alice (MR=0.15): ${evaluateEligibilityFilter(helperTrust, { pubKey: 'alice', mutualRecognition: 0.15 })}`);
console.log(`    - Bob (MR=0.05): ${evaluateEligibilityFilter(helperTrust, { pubKey: 'bob', mutualRecognition: 0.05 })}`);
console.log(`  EligibilityFilters.cityIn(['SF', 'NYC']):`);
console.log(`    - Carol (SF): ${evaluateEligibilityFilter(helperCity, { pubKey: 'carol', commitment: { city: 'SF' } })}`);
console.log(`    - Dave (LA): ${evaluateEligibilityFilter(helperCity, { pubKey: 'dave', commitment: { city: 'LA' } })}`);
console.log(`  EligibilityFilters.hasCertification('licensed'):`);
console.log(`    - Eve (licensed): ${evaluateEligibilityFilter(helperCert, { pubKey: 'eve', attributes: { certifications: ['licensed'] } })}`);
console.log(`    - Frank (not licensed): ${evaluateEligibilityFilter(helperCert, { pubKey: 'frank', attributes: { certifications: [] } })}`);
console.log(`  EligibilityFilters.and(trust, or(city, cert)):`);
console.log(`    - Grace (MR=0.15, SF): ${evaluateEligibilityFilter(helperComposite, { pubKey: 'grace', mutualRecognition: 0.15, commitment: { city: 'SF' }, attributes: { certifications: [] } })}`);
console.log(`    - Ivan (MR=0.05, SF): ${evaluateEligibilityFilter(helperComposite, { pubKey: 'ivan', mutualRecognition: 0.05, commitment: { city: 'SF' }, attributes: { certifications: [] } })}\n`);

// ═══════════════════════════════════════════════════════════════════
// ADVANCED PATTERNS
// ═══════════════════════════════════════════════════════════════════

console.log('\n═══════════════════════════════════════════════════════════');
console.log('ADVANCED PATTERNS');
console.log('═══════════════════════════════════════════════════════════\n');

// Example 15: Nested conditional logic
const nestedFilter: ComplianceFilter = {
	"if": [
		{ ">=": [{ "var": "mutualRecognition" }, 0.5] },  // High trust
		{
			"if": [
				{ "==": [{ "var": "attributes.tier" }, "premium"] },
				200000,  // High trust + premium
				150000   // High trust + regular
			]
		},
		{
			"if": [
				{ ">=": [{ "var": "mutualRecognition" }, 0.1] },  // Medium trust
				{
					"if": [
						{ "==": [{ "var": "attributes.tier" }, "premium"] },
						100000,  // Medium trust + premium
						75000    // Medium trust + regular
					]
				},
				25000  // Low trust (any tier)
			]
		}
	]
};

const contexts15 = [
	{ desc: 'High trust + premium', ctx: { pubKey: 'a', mutualRecognition: 0.6, attributes: { tier: 'premium' } } },
	{ desc: 'High trust + regular', ctx: { pubKey: 'b', mutualRecognition: 0.6, attributes: { tier: 'regular' } } },
	{ desc: 'Medium trust + premium', ctx: { pubKey: 'c', mutualRecognition: 0.2, attributes: { tier: 'premium' } } },
	{ desc: 'Medium trust + regular', ctx: { pubKey: 'd', mutualRecognition: 0.2, attributes: { tier: 'regular' } } },
	{ desc: 'Low trust', ctx: { pubKey: 'e', mutualRecognition: 0.05, attributes: { tier: 'premium' } } }
];

console.log('Example 15: Nested conditional logic (trust + tier matrix)');
contexts15.forEach(({ desc, ctx }) => {
	console.log(`  ${desc}: $${evaluateComplianceFilter(nestedFilter, ctx)}`);
});
console.log(`  Interpretation: Caps based on both trust level and tier\n`);

// Example 16: Rate limiting (time-based cap)
const rateLimitFilter: ComplianceFilter = {
	"+": [
		10000,  // Base allocation
		{
			"*": [
				{ "/": [{ "-": [Date.now(), 1704067200000] }, 86400000] },  // Days since 2024-01-01
				1000  // $1000 per day increase
			]
		}
	]
};

const context16: FilterContext = { pubKey: 'alice' };

console.log('Example 16: Rate limiting (time-based cap)');
console.log(`  Filter: Base $10K + $1K per day since 2024-01-01`);
console.log(`  Current cap: $${evaluateComplianceFilter(rateLimitFilter, context16).toFixed(2)}`);
console.log(`  Interpretation: Cap increases over time\n`);

// Example 17: Array operations in eligibility filters
const multiCertFilter: EligibilityFilter = {
	"all": [
		["licensed", "insured"],
		{
			"in": [{ "var": "" }, { "var": "attributes.certifications" }]
		}
	]
};

const context17a: FilterContext = {
	pubKey: 'alice',
	attributes: { certifications: ['licensed', 'insured', 'bonded'] }
};

const context17b: FilterContext = {
	pubKey: 'bob',
	attributes: { certifications: ['licensed'] }
};

console.log('Example 17: Array operations - require ALL certifications');
console.log(`  Filter: Require both 'licensed' AND 'insured'`);
console.log(`  Alice (licensed + insured + bonded): ${evaluateEligibilityFilter(multiCertFilter, context17a)}`);
console.log(`  Bob (licensed only): ${evaluateEligibilityFilter(multiCertFilter, context17b)}`);
console.log(`  Interpretation: All required certifications must be present\n`);

console.log('\n✅ All examples completed successfully!');
console.log('See docs/UNIFIED_FILTER_SYSTEM.md for full documentation.');

