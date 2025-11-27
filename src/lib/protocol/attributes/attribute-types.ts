/**
 * Attribute Types Module - Type-Specific Helpers
 * 
 * Provides parsing and validation helpers for common attribute types.
 * Extensible for future attribute types.
 * 
 * Common Attribute Types:
 * - membership: Array of member IDs (pubkeys, org_ids, contact_ids)
 * - capacity:{type}: Array of AvailabilitySlot objects
 * - need:{type}: Array of NeedSlot objects
 * - skill:{name}: Skill level object
 * - location: Location object
 * 
 * Each helper validates and normalizes the attribute value.
 */

import type { AvailabilitySlot, NeedSlot, MembershipList } from '$lib/protocol/schemas';

// ═══════════════════════════════════════════════════════════════════
// MEMBERSHIP ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse membership attribute
 * 
 * Validates and normalizes a membership attribute value.
 * 
 * @param value - Attribute value (should be array of member IDs)
 * @returns Normalized membership list
 * @throws Error if value is invalid
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
	if (!Array.isArray(value)) {
		throw new Error('Membership attribute must be an array');
	}
	
	// Validate each member ID is a string
	for (const member of value) {
		if (typeof member !== 'string' || member.length === 0) {
			throw new Error('Each member ID must be a non-empty string');
		}
	}
	
	// Deduplicate
	return Array.from(new Set(value));
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
	try {
		parseMembershipAttribute(value);
		return true;
	} catch {
		return false;
	}
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
 * Validates and normalizes a capacity attribute value.
 * 
 * @param value - Attribute value (should be array of AvailabilitySlot objects)
 * @returns Normalized capacity slots
 * @throws Error if value is invalid
 * 
 * @example
 * ```typescript
 * const slots = parseCapacityAttribute([
 *   { id: "slot1", quantity: 100, need_type_id: "food", ... },
 *   { id: "slot2", quantity: 50, need_type_id: "food", ... }
 * ]);
 * ```
 */
export function parseCapacityAttribute(value: any): AvailabilitySlot[] {
	if (!Array.isArray(value)) {
		throw new Error('Capacity attribute must be an array of AvailabilitySlot objects');
	}
	
	// Basic validation - check required fields
	for (const slot of value) {
		if (typeof slot !== 'object' || slot === null) {
			throw new Error('Each capacity slot must be an object');
		}
		
		if (!slot.id || typeof slot.id !== 'string') {
			throw new Error('Each capacity slot must have an id (string)');
		}
		
		if (typeof slot.quantity !== 'number' || slot.quantity < 0) {
			throw new Error('Each capacity slot must have a non-negative quantity (number)');
		}
		
		if (!slot.need_type_id || typeof slot.need_type_id !== 'string') {
			throw new Error('Each capacity slot must have a need_type_id (string)');
		}
	}
	
	return value as AvailabilitySlot[];
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
	try {
		parseCapacityAttribute(value);
		return true;
	} catch {
		return false;
	}
}

// ═══════════════════════════════════════════════════════════════════
// NEED ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Parse need attribute
 * 
 * Validates and normalizes a need attribute value.
 * 
 * @param value - Attribute value (should be array of NeedSlot objects)
 * @returns Normalized need slots
 * @throws Error if value is invalid
 * 
 * @example
 * ```typescript
 * const slots = parseNeedAttribute([
 *   { id: "need1", quantity: 10, need_type_id: "housing", ... },
 *   { id: "need2", quantity: 5, need_type_id: "housing", ... }
 * ]);
 * ```
 */
export function parseNeedAttribute(value: any): NeedSlot[] {
	if (!Array.isArray(value)) {
		throw new Error('Need attribute must be an array of NeedSlot objects');
	}
	
	// Basic validation - check required fields
	for (const slot of value) {
		if (typeof slot !== 'object' || slot === null) {
			throw new Error('Each need slot must be an object');
		}
		
		if (!slot.id || typeof slot.id !== 'string') {
			throw new Error('Each need slot must have an id (string)');
		}
		
		if (typeof slot.quantity !== 'number' || slot.quantity < 0) {
			throw new Error('Each need slot must have a non-negative quantity (number)');
		}
		
		if (!slot.need_type_id || typeof slot.need_type_id !== 'string') {
			throw new Error('Each need slot must have a need_type_id (string)');
		}
	}
	
	return value as NeedSlot[];
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
	try {
		parseNeedAttribute(value);
		return true;
	} catch {
		return false;
	}
}

// ═══════════════════════════════════════════════════════════════════
// SKILL ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Skill object
 */
export interface SkillValue {
	level: number; // 1-10
	years?: number; // Years of experience
	description?: string;
	verified?: boolean;
	endorsements?: string[]; // Array of pubkeys who endorse this skill
}

/**
 * Parse skill attribute
 * 
 * Validates and normalizes a skill attribute value.
 * 
 * @param value - Attribute value (should be SkillValue object)
 * @returns Normalized skill value
 * @throws Error if value is invalid
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
	if (typeof value !== 'object' || value === null) {
		throw new Error('Skill attribute must be an object');
	}
	
	if (typeof value.level !== 'number' || value.level < 1 || value.level > 10) {
		throw new Error('Skill level must be a number between 1 and 10');
	}
	
	if (value.years !== undefined && (typeof value.years !== 'number' || value.years < 0)) {
		throw new Error('Skill years must be a non-negative number');
	}
	
	if (value.description !== undefined && typeof value.description !== 'string') {
		throw new Error('Skill description must be a string');
	}
	
	if (value.verified !== undefined && typeof value.verified !== 'boolean') {
		throw new Error('Skill verified must be a boolean');
	}
	
	if (value.endorsements !== undefined) {
		if (!Array.isArray(value.endorsements)) {
			throw new Error('Skill endorsements must be an array');
		}
		for (const endorsement of value.endorsements) {
			if (typeof endorsement !== 'string') {
				throw new Error('Each endorsement must be a string (pubkey)');
			}
		}
	}
	
	return {
		level: value.level,
		years: value.years,
		description: value.description,
		verified: value.verified || false,
		endorsements: value.endorsements || []
	};
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
	try {
		parseSkillAttribute(value);
		return true;
	} catch {
		return false;
	}
}

// ═══════════════════════════════════════════════════════════════════
// LOCATION ATTRIBUTES
// ═══════════════════════════════════════════════════════════════════

/**
 * Location object
 */
export interface LocationValue {
	city?: string;
	state_province?: string;
	country?: string;
	coords?: [number, number]; // [latitude, longitude]
	postal_code?: string;
	street_address?: string;
	online?: boolean; // True if remote/online
}

/**
 * Parse location attribute
 * 
 * Validates and normalizes a location attribute value.
 * 
 * @param value - Attribute value (should be LocationValue object)
 * @returns Normalized location value
 * @throws Error if value is invalid
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
	if (typeof value !== 'object' || value === null) {
		throw new Error('Location attribute must be an object');
	}
	
	const location: LocationValue = {};
	
	if (value.city !== undefined) {
		if (typeof value.city !== 'string') {
			throw new Error('Location city must be a string');
		}
		location.city = value.city;
	}
	
	if (value.state_province !== undefined) {
		if (typeof value.state_province !== 'string') {
			throw new Error('Location state_province must be a string');
		}
		location.state_province = value.state_province;
	}
	
	if (value.country !== undefined) {
		if (typeof value.country !== 'string') {
			throw new Error('Location country must be a string');
		}
		location.country = value.country;
	}
	
	if (value.coords !== undefined) {
		if (!Array.isArray(value.coords) || value.coords.length !== 2) {
			throw new Error('Location coords must be an array of [latitude, longitude]');
		}
		if (typeof value.coords[0] !== 'number' || typeof value.coords[1] !== 'number') {
			throw new Error('Location coords must be numbers');
		}
		if (value.coords[0] < -90 || value.coords[0] > 90) {
			throw new Error('Latitude must be between -90 and 90');
		}
		if (value.coords[1] < -180 || value.coords[1] > 180) {
			throw new Error('Longitude must be between -180 and 180');
		}
		location.coords = [value.coords[0], value.coords[1]];
	}
	
	if (value.postal_code !== undefined) {
		if (typeof value.postal_code !== 'string') {
			throw new Error('Location postal_code must be a string');
		}
		location.postal_code = value.postal_code;
	}
	
	if (value.street_address !== undefined) {
		if (typeof value.street_address !== 'string') {
			throw new Error('Location street_address must be a string');
		}
		location.street_address = value.street_address;
	}
	
	if (value.online !== undefined) {
		if (typeof value.online !== 'boolean') {
			throw new Error('Location online must be a boolean');
		}
		location.online = value.online;
	}
	
	return location;
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
	try {
		parseLocationAttribute(value);
		return true;
	} catch {
		return false;
	}
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
 * Automatically detects type and validates accordingly.
 * 
 * @param attribute_name - Attribute name
 * @param value - Attribute value
 * @returns True if valid
 */
export function validateAttributeValue(attribute_name: string, value: any): boolean {
	try {
		parseAttributeValue(attribute_name, value);
		return true;
	} catch {
		return false;
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

