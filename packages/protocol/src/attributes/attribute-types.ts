/**
 * Attribute Types Module - Type-Specific Helpers
 * 
 * Provides parsing and validation helpers for common attribute types.
 * Uses Zod schemas for validation (single source of truth).
 * 
 * Common Attribute Types:
 * - membership: Array of member IDs (pubkeys, org_ids, contact_ids)
 * - capacity:{type}: Array of AvailabilitySlot objects
 * - need:{type}: Array of NeedSlot objects
 * - skill:{name}: Skill level object
 * - location: Location object
 * 
 * Each helper validates and normalizes the attribute value using Zod.
 */

import {
	AvailabilitySlotSchema,
	NeedSlotSchema,
	MembershipListSchema,
	SkillValueSchema,
	LocationValueSchema,
	type AvailabilitySlot,
	type NeedSlot,
	type MembershipList,
	type SkillValue,
	type LocationValue
} from '../schemas';
import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse membership attribute
 * 
 * Validates and normalizes a membership attribute value using Zod.
 * 
 * @param value - Attribute value (should be array of member IDs)
 * @returns Normalized membership list (deduplicated)
 * @throws ZodError if value is invalid
 * 
 * @example
 * ```typescript
 * const members = parseMembershipAttribute([
 *   "pubkey_alice",
 *   "pubkey_bob",
 *   "org_abc123"
 * ]);
 * ```
 */
export function parseMembershipAttribute(value: any): MembershipList {
	const parsed = MembershipListSchema.parse(value);
	// Deduplicate
	return Array.from(new Set(parsed));
}

/**
 * Validate membership attribute
 * 
 * Checks if a value is a valid membership attribute.
 * 
 * @param value - Value to validate
 * @returns True if valid
 */
export function isMembershipAttribute(value: any): boolean {
	return MembershipListSchema.safeParse(value).success;
}

/**
 * Create membership attribute
 * 
 * Helper to create a membership attribute value.
 * 
 * @param members - Array of member IDs
 * @returns Normalized membership list
 */
export function createMembershipAttribute(members: string[]): MembershipList {
	return parseMembershipAttribute(members);
}

// ═══════════════════════════════════════════════════════════════════
// CAPACITY ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse capacity attribute
 * 
 * Validates and normalizes a capacity attribute value using Zod.
 * 
 * @param value - Attribute value (should be array of AvailabilitySlot objects)
 * @returns Validated capacity slots
 * @throws ZodError if value is invalid
 * 
 * @example
 * ```typescript
 * const slots = parseCapacityAttribute([
 *   { id: "slot1", quantity: 100, need_type_id: "food", name: "Food", ... },
 *   { id: "slot2", quantity: 50, need_type_id: "food", name: "Snacks", ... }
 * ]);
 * ```
 */
export function parseCapacityAttribute(value: any): AvailabilitySlot[] {
	return z.array(AvailabilitySlotSchema).parse(value);
}

/**
 * Validate capacity attribute
 * 
 * Checks if a value is a valid capacity attribute.
 * 
 * @param value - Value to validate
 * @returns True if valid
 */
export function isCapacityAttribute(value: any): boolean {
	return z.array(AvailabilitySlotSchema).safeParse(value).success;
}

// ═══════════════════════════════════════════════════════════════════
// NEED ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse need attribute
 * 
 * Validates and normalizes a need attribute value using Zod.
 * 
 * @param value - Attribute value (should be array of NeedSlot objects)
 * @returns Validated need slots
 * @throws ZodError if value is invalid
 * 
 * @example
 * ```typescript
 * const slots = parseNeedAttribute([
 *   { id: "need1", quantity: 10, need_type_id: "housing", name: "Housing", ... },
 *   { id: "need2", quantity: 5, need_type_id: "housing", name: "Shelter", ... }
 * ]);
 * ```
 */
export function parseNeedAttribute(value: any): NeedSlot[] {
	return z.array(NeedSlotSchema).parse(value);
}

/**
 * Validate need attribute
 * 
 * Checks if a value is a valid need attribute.
 * 
 * @param value - Value to validate
 * @returns True if valid
 */
export function isNeedAttribute(value: any): boolean {
	return z.array(NeedSlotSchema).safeParse(value).success;
}

// ═══════════════════════════════════════════════════════════════════
// SKILL ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse skill attribute
 * 
 * Validates and normalizes a skill attribute value using Zod.
 * 
 * @param value - Attribute value (should be SkillValue object)
 * @returns Validated skill value
 * @throws ZodError if value is invalid
 * 
 * @example
 * ```typescript
 * const skill = parseSkillAttribute({
 *   level: 8,
 *   years: 5,
 *   description: "Expert in TypeScript development",
 *   verified: true
 * });
 * ```
 */
export function parseSkillAttribute(value: any): SkillValue {
	return SkillValueSchema.parse(value);
}

/**
 * Validate skill attribute
 * 
 * Checks if a value is a valid skill attribute.
 * 
 * @param value - Value to validate
 * @returns True if valid
 */
export function isSkillAttribute(value: any): boolean {
	return SkillValueSchema.safeParse(value).success;
}

// ═══════════════════════════════════════════════════════════════════
// LOCATION ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse location attribute
 * 
 * Validates and normalizes a location attribute value using Zod.
 * 
 * @param value - Attribute value (should be LocationValue object)
 * @returns Validated location value
 * @throws ZodError if value is invalid
 * 
 * @example
 * ```typescript
 * const location = parseLocationAttribute({
 *   city: "Berlin",
 *   country: "Germany",
 *   coords: [52.5200, 13.4050]
 * });
 * ```
 */
export function parseLocationAttribute(value: any): LocationValue {
	return LocationValueSchema.parse(value);
}

/**
 * Validate location attribute
 * 
 * Checks if a value is a valid location attribute.
 * 
 * @param value - Value to validate
 * @returns True if valid
 */
export function isLocationAttribute(value: any): boolean {
	return LocationValueSchema.safeParse(value).success;
}

// ═══════════════════════════════════════════════════════════════════
// GENERIC ATTRIBUTE TYPE DETECTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Detect attribute type from attribute name
 * 
 * Uses naming conventions to detect attribute type:
 * - "membership" → membership
 * - "capacity:*" → capacity
 * - "need:*" → need
 * - "skill:*" → skill
 * - "location" → location
 * - "*" → generic
 * 
 * @param attribute_name - Attribute name
 * @returns Detected type
 */
export function detectAttributeType(attribute_name: string): 
	'membership' | 'capacity' | 'need' | 'skill' | 'location' | 'generic' {
	
	if (attribute_name === 'membership') return 'membership';
	if (attribute_name.startsWith('capacity:')) return 'capacity';
	if (attribute_name.startsWith('need:')) return 'need';
	if (attribute_name.startsWith('skill:')) return 'skill';
	if (attribute_name === 'location') return 'location';
	
	return 'generic';
}

/**
 * Parse attribute value based on type
 * 
 * Automatically detects type and parses accordingly.
 * 
 * @param attribute_name - Attribute name
 * @param value - Attribute value
 * @returns Parsed value
 * @throws Error if value doesn't match detected type
 */
export function parseAttributeValue(attribute_name: string, value: any): any {
	const type = detectAttributeType(attribute_name);
	
	switch (type) {
		case 'membership':
			return parseMembershipAttribute(value);
		case 'capacity':
			return parseCapacityAttribute(value);
		case 'need':
			return parseNeedAttribute(value);
		case 'skill':
			return parseSkillAttribute(value);
		case 'location':
			return parseLocationAttribute(value);
		case 'generic':
			// No validation for generic types
			return value;
	}
}

/**
 * Validate attribute value based on type
 * 
 * Automatically detects type and validates accordingly using Zod.
 * 
 * @param attribute_name - Attribute name
 * @param value - Attribute value
 * @returns True if valid
 */
export function validateAttributeValue(attribute_name: string, value: any): boolean {
	const type = detectAttributeType(attribute_name);
	
	switch (type) {
		case 'membership':
			return MembershipListSchema.safeParse(value).success;
		case 'capacity':
			return z.array(AvailabilitySlotSchema).safeParse(value).success;
		case 'need':
			return z.array(NeedSlotSchema).safeParse(value).success;
		case 'skill':
			return SkillValueSchema.safeParse(value).success;
		case 'location':
			return LocationValueSchema.safeParse(value).success;
		case 'generic':
			// No validation for generic types - always valid
			return true;
	}
}

// ═══════════════════════════════════════════════════════════════════
// ATTRIBUTE NAME HELPERS
// ═══════════════════════════════════════════════════════════════════

/**
 * Extract need type from capacity/need attribute name
 * 
 * @param attribute_name - Attribute name (e.g., "capacity:food", "need:housing")
 * @returns Need type or undefined
 * 
 * @example
 * ```typescript
 * extractNeedType("capacity:food") // → "food"
 * extractNeedType("need:housing") // → "housing"
 * extractNeedType("membership") // → undefined
 * ```
 */
export function extractNeedType(attribute_name: string): string | undefined {
	if (attribute_name.startsWith('capacity:')) {
		return attribute_name.substring('capacity:'.length);
	}
	if (attribute_name.startsWith('need:')) {
		return attribute_name.substring('need:'.length);
	}
	return undefined;
}

/**
 * Extract skill name from skill attribute name
 * 
 * @param attribute_name - Attribute name (e.g., "skill:javascript")
 * @returns Skill name or undefined
 * 
 * @example
 * ```typescript
 * extractSkillName("skill:javascript") // → "javascript"
 * extractSkillName("membership") // → undefined
 * ```
 */
export function extractSkillName(attribute_name: string): string | undefined {
	if (attribute_name.startsWith('skill:')) {
		return attribute_name.substring('skill:'.length);
	}
	return undefined;
}

/**
 * Create capacity attribute name
 * 
 * @param need_type_id - Need type ID
 * @returns Attribute name (e.g., "capacity:food")
 */
export function createCapacityAttributeName(need_type_id: string): string {
	return `capacity:${need_type_id}`;
}

/**
 * Create need attribute name
 * 
 * @param need_type_id - Need type ID
 * @returns Attribute name (e.g., "need:housing")
 */
export function createNeedAttributeName(need_type_id: string): string {
	return `need:${need_type_id}`;
}

/**
 * Create skill attribute name
 * 
 * @param skill_name - Skill name
 * @returns Attribute name (e.g., "skill:javascript")
 */
export function createSkillAttributeName(skill_name: string): string {
	return `skill:${skill_name}`;
}

// ═══════════════════════════════════════════════════════════════════
// CUSTOM EQUALITY CHECKERS (for change detection)
// ═══════════════════════════════════════════════════════════════════

/**
 * Membership array equality (order-independent)
 * 
 * Since membership is typically a set of pubkeys,
 * we compare as sets rather than arrays for order-independence.
 * 
 * @example
 * ```typescript
 * membershipEquals(['alice', 'bob'], ['bob', 'alice']) // → true
 * membershipEquals(['alice'], ['alice', 'bob']) // → false
 * ```
 */
export function membershipEquals(a: any, b: any): boolean {
	if (!Array.isArray(a) || !Array.isArray(b)) return false;
	if (a.length !== b.length) return false;
	
	// Convert to sets for order-independent comparison
	const setA = new Set(a);
	const setB = new Set(b);
	
	if (setA.size !== setB.size) return false;
	
	for (const item of setA) {
		if (!setB.has(item)) return false;
	}
	
	return true;
}

/**
 * Slot array equality (by slot ID)
 * 
 * Capacity/need slots have IDs - compare by ID set and key properties.
 * This is more efficient than deep equality for large slot arrays.
 * 
 * @example
 * ```typescript
 * slotArrayEquals(
 *   [{ id: 's1', quantity: 100 }],
 *   [{ id: 's1', quantity: 100 }]
 * ) // → true
 * ```
 */
export function slotArrayEquals(a: any, b: any): boolean {
	if (!Array.isArray(a) || !Array.isArray(b)) return false;
	if (a.length !== b.length) return false;
	
	// Create maps by slot ID for O(n) comparison
	const mapA = new Map(a.map((slot: any) => [slot.id, slot]));
	const mapB = new Map(b.map((slot: any) => [slot.id, slot]));
	
	if (mapA.size !== mapB.size) return false;
	
	// Compare each slot by ID
	for (const [id, slotA] of mapA.entries()) {
		const slotB = mapB.get(id);
		if (!slotB) return false;
		
		// Compare key properties
		if (slotA.quantity !== slotB.quantity) return false;
		if (slotA.need_type_id !== slotB.need_type_id) return false;
		
		// Compare optional time constraints
		if (JSON.stringify(slotA.time_constraint) !== JSON.stringify(slotB.time_constraint)) {
			return false;
		}
		
		// Compare optional location constraints
		if (JSON.stringify(slotA.location_constraint) !== JSON.stringify(slotB.location_constraint)) {
			return false;
		}
	}
	
	return true;
}

/**
 * Get equality checker for attribute type
 * 
 * Returns specialized equality checker based on attribute type.
 * Returns undefined for types that should use default deepEquals.
 * 
 * @param attribute_name - Attribute name (e.g., "membership", "capacity:food")
 * @returns Custom equality checker or undefined (use default)
 * 
 * @example
 * ```typescript
 * const checker = getEqualityChecker('membership');
 * const isSame = checker(['alice', 'bob'], ['bob', 'alice']); // → true
 * ```
 */
export function getEqualityChecker(attribute_name: string): 
	((a: any, b: any) => boolean) | undefined {
	
	const type = detectAttributeType(attribute_name);
	
	switch (type) {
		case 'membership':
			return membershipEquals;
		
		case 'capacity':
		case 'need':
			return slotArrayEquals;
		
		default:
			return undefined; // Use default deepEquals
	}
}

