/**
 * Attribute Recognition Module - Pure Functions
 * 
 * Core functions for recognizing and managing entity attributes.
 * Enables any entity (user/org) to recognize any attribute of any other entity.
 * 
 * Key Features:
 * - Fully general key-value attribute storage
 * - ITC causality tracking per attribute
 * - Provenance tracking (source_pubkey)
 * - Confidence/weight support
 * 
 * For Svelte store integration, see attribute-recognition.svelte.ts
 */

import type {
	AttributeValue,
	AttributeRecognitionsCollection,
	AttributeSubscriptions,
	EntityIdMappings
} from '../schemas';
import { seed as itcSeed, event as itcEvent, join as itcJoin, type Stamp as ITCStamp } from '../itc';

// ═══════════════════════════════════════════════════════════════════
// DEEP EQUALITY CHECKER (ported from VersionedStore)
// ═══════════════════════════════════════════════════════════════════

/**
 * Deep equality checker for change detection
 * 
 * Prevents unnecessary reactive updates when value hasn't changed.
 * 
 * Handles:
 * - ✅ Primitives (string, number, boolean, null, undefined)
 * - ✅ Plain objects (recursive)
 * - ✅ Arrays (recursive)
 * - ✅ Date objects (by timestamp)
 * - ✅ Map objects (by entries)
 * - ✅ Set objects (by values)
 * - ✅ RegExp objects (by source and flags)
 * 
 * Limitations:
 * - ❌ Functions (compared by reference)
 * - ❌ Class instances (compared by reference)
 * - ❌ Circular references (will cause stack overflow)
 * 
 * For special cases, provide custom equality checker to updateAttributeInCollection.
 */
export function deepEquals(a: any, b: any): boolean {
	// Handle undefined/null
	if (a === undefined && b === undefined) return true;
	if (a === null && b === null) return true;
	if (a === undefined || b === undefined) return false;
	if (a === null || b === null) return false;
	
	// Primitive types
	if (typeof a !== 'object' || typeof b !== 'object') {
		return a === b;
	}
	
	// Date objects - compare by timestamp
	if (a instanceof Date && b instanceof Date) {
		return a.getTime() === b.getTime();
	}
	
	// RegExp objects - compare by source and flags
	if (a instanceof RegExp && b instanceof RegExp) {
		return a.source === b.source && a.flags === b.flags;
	}
	
	// Map objects - compare entries
	if (a instanceof Map && b instanceof Map) {
		if (a.size !== b.size) return false;
		for (const [key, value] of a.entries()) {
			if (!b.has(key)) return false;
			if (!deepEquals(value, b.get(key))) return false;
		}
		return true;
	}
	
	// Set objects - compare values
	if (a instanceof Set && b instanceof Set) {
		if (a.size !== b.size) return false;
		for (const value of a) {
			if (!b.has(value)) return false;
		}
		return true;
	}
	
	// Arrays
	if (Array.isArray(a) && Array.isArray(b)) {
		if (a.length !== b.length) return false;
		for (let i = 0; i < a.length; i++) {
			if (!deepEquals(a[i], b[i])) return false;
		}
		return true;
	}
	
	// Plain objects (recursive)
	const keysA = Object.keys(a);
	const keysB = Object.keys(b);
	if (keysA.length !== keysB.length) return false;
	
	for (const key of keysA) {
		if (!deepEquals(a[key], b[key])) return false;
	}
	
	return true;
}

// ═══════════════════════════════════════════════════════════════════
// CORE RECOGNITION FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Recognize an attribute of an entity
 * 
 * Creates an AttributeValue with metadata and ITC causality tracking.
 * 
 * @param entity_id - Entity being recognized (pubkey, org_id, contact_id, uuid)
 * @param attribute_name - Attribute name (e.g., "membership", "capacity:food", "need:housing")
 * @param value - Attribute value (flexible - array, object, primitive)
 * @param source_pubkey - Who declared this (optional - undefined means self-recognition)
 * @param confidence - Confidence level 0-1 (default: 1.0)
 * @param existingITC - Existing ITC stamp to increment (for updates)
 * @returns AttributeValue with ITC stamp
 * 
 * @example
 * ```typescript
 * // Recognize org membership
 * const attr = recognizeAttribute(
 *   "org_abc123",
 *   "membership",
 *   ["pubkey_alice", "pubkey_bob"]
 * );
 * 
 * // Recognize capacity with source
 * const attr = recognizeAttribute(
 *   "pubkey_bob",
 *   "capacity:food",
 *   [slot1, slot2],
 *   "pubkey_bob"
 * );
 * ```
 */
export function recognizeAttribute(
	entity_id: string,
	attribute_name: string,
	value: any,
	source_pubkey?: string,
	confidence: number = 1.0,
	existingITC?: ITCStamp
): AttributeValue {
	// Validate confidence
	if (confidence < 0 || confidence > 1) {
		throw new Error(`Confidence must be between 0 and 1, got: ${confidence}`);
	}
	
	// Generate or increment ITC stamp
	const itcStamp = existingITC ? itcEvent(existingITC) : itcSeed();
	
	return {
		value,
		source_pubkey,
		confidence,
		timestamp: Date.now(),
		itcStamp
	};
}

/**
 * Update attribute in collection with change detection
 * 
 * ✅ NEW: Value change detection!
 * - Compares new value with existing value using deep equality
 * - If unchanged: Updates ITC/timestamp only (no reactive trigger)
 * - If changed: Full update with new value
 * 
 * This prevents unnecessary reactive updates when:
 * - Network sends same value with updated ITC
 * - Concurrent updates converge to same value
 * - Subscription data is re-received
 * 
 * @param collection - Current attribute recognitions collection
 * @param entity_id - Entity being recognized
 * @param attribute_name - Attribute name
 * @param value - New attribute value
 * @param source_pubkey - Who declared this
 * @param confidence - Confidence level
 * @param existingITC - Existing ITC to increment (for updates)
 * @param customEqualityChecker - Optional custom equality checker for this value
 * @returns Updated collection with new attribute
 */
export function updateAttributeInCollection(
	collection: AttributeRecognitionsCollection,
	entity_id: string,
	attribute_name: string,
	value: any,
	source_pubkey?: string,
	confidence: number = 1.0,
	existingITC?: ITCStamp,
	customEqualityChecker?: (a: any, b: any) => boolean
): AttributeRecognitionsCollection {
	// Get existing attribute
	const existingEntity = collection[entity_id] as Record<string, AttributeValue> | undefined;
	const existingAttr = existingEntity?.[attribute_name];
	
	// ═══════════════════════════════════════════════════════════════════
	// ✅ VALUE CHANGE DETECTION
	// ═══════════════════════════════════════════════════════════════════
	
	if (existingAttr) {
		// Use custom equality checker if provided, otherwise use deepEquals
		const equalityChecker = customEqualityChecker || deepEquals;
		const valueUnchanged = equalityChecker(existingAttr.value, value);
		
		if (valueUnchanged) {
			// ✅ VALUE UNCHANGED: Update metadata only
			// This prevents unnecessary reactive updates!
			
			// Update ITC (use provided or increment existing, or seed if missing)
			const updatedITC = existingITC || 
				(existingAttr.itcStamp ? itcEvent(existingAttr.itcStamp) : itcSeed());
			
			// Update collection-level ITC
			const collectionITC = collection._itcStamp ? 
				itcJoin(collection._itcStamp, updatedITC) : 
				updatedITC;
			
			return {
				...collection,
				[entity_id]: {
					...existingEntity,
					[attribute_name]: {
						...existingAttr,
						// Keep same value!
						itcStamp: updatedITC,
						timestamp: Date.now()
						// source_pubkey, confidence unchanged
					}
				},
				_itcStamp: collectionITC,
				_timestamp: Date.now()
			};
		}
	}
	
	// ═══════════════════════════════════════════════════════════════════
	// VALUE CHANGED (or new attribute): Full update
	// ═══════════════════════════════════════════════════════════════════
	
	// Use provided ITC or existing attribute's ITC
	const baseITC = existingITC || existingAttr?.itcStamp;
	
	// Create new attribute value (increments ITC if exists)
	const newAttr = recognizeAttribute(
		entity_id,
		attribute_name,
		value,
		source_pubkey,
		confidence,
		baseITC
	);
	
	// recognizeAttribute always returns an itcStamp, but TypeScript doesn't know that
	// Add assertion for type safety
	if (!newAttr.itcStamp) {
		throw new Error('recognizeAttribute must always return an itcStamp');
	}
	
	// Update collection
	const updatedCollection = {
		...collection,
		[entity_id]: {
			...(existingEntity || {}),
			[attribute_name]: newAttr
		}
	};
	
	// Update collection-level ITC
	const collectionITC = collection._itcStamp ? 
		itcJoin(collection._itcStamp, newAttr.itcStamp) : 
		newAttr.itcStamp;
	
	return {
		...updatedCollection,
		_itcStamp: collectionITC,
		_timestamp: Date.now()
	};
}

/**
 * Get attribute value from collection
 * 
 * Retrieves an entity's attribute from the recognition collection.
 * 
 * @param collection - Attribute recognitions collection
 * @param entity_id - Entity to get attribute for
 * @param attribute_name - Attribute name
 * @returns AttributeValue or undefined if not found
 */
export function getAttributeFromCollection(
	collection: AttributeRecognitionsCollection,
	entity_id: string,
	attribute_name: string
): AttributeValue | undefined {
	const entityAttrs = collection[entity_id] as Record<string, AttributeValue> | undefined;
	return entityAttrs?.[attribute_name];
}

/**
 * Remove attribute from collection
 * 
 * Removes an entity's attribute from the recognition collection.
 * 
 * @param collection - Current attribute recognitions collection
 * @param entity_id - Entity to remove attribute from
 * @param attribute_name - Attribute name to remove
 * @returns Updated collection without the attribute
 */
export function removeAttributeFromCollection(
	collection: AttributeRecognitionsCollection,
	entity_id: string,
	attribute_name: string
): AttributeRecognitionsCollection {
	const entityAttrs = collection[entity_id] as Record<string, AttributeValue> | undefined;
	
	if (!entityAttrs || !entityAttrs[attribute_name]) {
		// Attribute doesn't exist, return unchanged
		return collection;
	}
	
	// Remove attribute
	const { [attribute_name]: removed, ...remaining } = entityAttrs;
	
	// If entity has no more attributes, remove entity entry
	if (Object.keys(remaining).length === 0) {
		const { [entity_id]: removedEntity, ...collectionRemaining } = collection;
		return {
			...collectionRemaining,
			_itcStamp: collection._itcStamp ? itcEvent(collection._itcStamp) : itcSeed(),
			_timestamp: Date.now()
		} as AttributeRecognitionsCollection;
	}
	
	// Update collection
	return {
		...collection,
		[entity_id]: remaining,
		_itcStamp: collection._itcStamp ? itcEvent(collection._itcStamp) : itcSeed(),
		_timestamp: Date.now()
	};
}

/**
 * Get all attributes for an entity
 * 
 * Retrieves all attributes recognized for a specific entity.
 * 
 * @param collection - Attribute recognitions collection
 * @param entity_id - Entity to get attributes for
 * @returns Record of attribute_name → AttributeValue
 */
export function getAllAttributesForEntity(
	collection: AttributeRecognitionsCollection,
	entity_id: string
): Record<string, AttributeValue> {
	const entityAttrs = collection[entity_id];
	
	// Filter out metadata fields (_itcStamp, _timestamp)
	if (!entityAttrs || typeof entityAttrs !== 'object') {
		return {};
	}
	
	// Return only AttributeValue entries
	const result: Record<string, AttributeValue> = {};
	for (const [key, value] of Object.entries(entityAttrs)) {
		if (key !== '_itcStamp' && key !== '_timestamp' && isAttributeValue(value)) {
			result[key] = value as AttributeValue;
		}
	}
	
	return result;
}

/**
 * Get all entities with a specific attribute
 * 
 * Finds all entities that have the specified attribute.
 * 
 * @param collection - Attribute recognitions collection
 * @param attribute_name - Attribute name to search for
 * @returns Array of entity_ids that have this attribute
 */
export function getEntitiesWithAttribute(
	collection: AttributeRecognitionsCollection,
	attribute_name: string
): string[] {
	const entities: string[] = [];
	
	for (const [entity_id, entityAttrs] of Object.entries(collection)) {
		// Skip metadata fields
		if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;
		
		if (typeof entityAttrs === 'object' && entityAttrs !== null) {
			const attrs = entityAttrs as Record<string, AttributeValue>;
			if (attribute_name in attrs && isAttributeValue(attrs[attribute_name])) {
				entities.push(entity_id);
			}
		}
	}
	
	return entities;
}

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

/**
 * Subscribe to an entity's attribute
 * 
 * Configures subscription to fetch an entity's attribute from a specific source.
 * 
 * @param subscriptions - Current subscriptions
 * @param entity_id - Entity to subscribe to
 * @param attribute_name - Attribute name
 * @param source_pubkey - Source pubkey to subscribe to
 * @returns Updated subscriptions
 * 
 * @example
 * ```typescript
 * // Subscribe to Alice's view of org membership
 * const subs = subscribeToAttribute(
 *   currentSubs,
 *   "org_abc123",
 *   "membership",
 *   "pubkey_alice"
 * );
 * ```
 */
export function subscribeToAttribute(
	subscriptions: AttributeSubscriptions,
	entity_id: string,
	attribute_name: string,
	source_pubkey: string
): AttributeSubscriptions {
	return {
		...subscriptions,
		[entity_id]: {
			...(subscriptions[entity_id] || {}),
			[attribute_name]: source_pubkey
		}
	};
}

/**
 * Unsubscribe from an entity's attribute
 * 
 * Removes subscription for a specific entity's attribute.
 * 
 * @param subscriptions - Current subscriptions
 * @param entity_id - Entity to unsubscribe from
 * @param attribute_name - Attribute name
 * @returns Updated subscriptions
 */
export function unsubscribeFromAttribute(
	subscriptions: AttributeSubscriptions,
	entity_id: string,
	attribute_name: string
): AttributeSubscriptions {
	const entitySubs = subscriptions[entity_id];
	
	if (!entitySubs || !entitySubs[attribute_name]) {
		return subscriptions;
	}
	
	const { [attribute_name]: removed, ...remaining } = entitySubs;
	
	// If entity has no more subscriptions, remove entity entry
	if (Object.keys(remaining).length === 0) {
		const { [entity_id]: removedEntity, ...subsRemaining } = subscriptions;
		return subsRemaining;
	}
	
	return {
		...subscriptions,
		[entity_id]: remaining
	};
}

/**
 * Get subscription source for an entity's attribute
 * 
 * Retrieves the source pubkey configured for an entity's attribute.
 * 
 * @param subscriptions - Subscriptions
 * @param entity_id - Entity
 * @param attribute_name - Attribute name
 * @returns Source pubkey or undefined if not subscribed
 */
export function getSubscriptionSource(
	subscriptions: AttributeSubscriptions,
	entity_id: string,
	attribute_name: string
): string | undefined {
	return subscriptions[entity_id]?.[attribute_name];
}

/**
 * Get all subscriptions for an entity
 * 
 * Retrieves all attribute subscriptions for a specific entity.
 * 
 * @param subscriptions - Subscriptions
 * @param entity_id - Entity
 * @returns Record of attribute_name → source_pubkey
 */
export function getAllSubscriptionsForEntity(
	subscriptions: AttributeSubscriptions,
	entity_id: string
): Record<string, string> {
	return subscriptions[entity_id] || {};
}

// ═══════════════════════════════════════════════════════════════════
// ENTITY ID RESOLUTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolve entity ID to public key
 * 
 * Resolves uuid/contact_id to pubkey using mappings.
 * Returns the input if already a pubkey or no mapping found.
 * 
 * @param id - Entity identifier (pubkey, uuid, contact_id, org_id)
 * @param mappings - Entity ID mappings
 * @returns Resolved pubkey or original id if not mapped
 * 
 * @example
 * ```typescript
 * const pubkey = resolveEntityId("contact_alice_123", mappings);
 * // Returns: "pubkey_abc..."
 * ```
 */
export function resolveEntityId(
	id: string,
	mappings: EntityIdMappings
): string {
	return mappings[id] || id;
}

/**
 * Add entity ID mapping
 * 
 * Maps a local identifier (uuid/contact_id) to a public key.
 * 
 * @param mappings - Current mappings
 * @param local_id - Local identifier (uuid or contact_id)
 * @param pubkey - Public key
 * @returns Updated mappings
 */
export function addEntityIdMapping(
	mappings: EntityIdMappings,
	local_id: string,
	pubkey: string
): EntityIdMappings {
	return {
		...mappings,
		[local_id]: pubkey
	};
}

/**
 * Remove entity ID mapping
 * 
 * Removes a local identifier mapping.
 * 
 * @param mappings - Current mappings
 * @param local_id - Local identifier to remove
 * @returns Updated mappings
 */
export function removeEntityIdMapping(
	mappings: EntityIdMappings,
	local_id: string
): EntityIdMappings {
	const { [local_id]: removed, ...remaining } = mappings;
	return remaining;
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Type guard: Check if value is an AttributeValue
 */
function isAttributeValue(value: any): value is AttributeValue {
	return (
		value !== null &&
		typeof value === 'object' &&
		'value' in value &&
		'timestamp' in value &&
		'confidence' in value
	);
}

/**
 * Merge two attribute collections
 * 
 * Merges two attribute recognition collections, preferring newer values (by timestamp).
 * 
 * @param collection1 - First collection
 * @param collection2 - Second collection
 * @returns Merged collection
 */
export function mergeAttributeCollections(
	collection1: AttributeRecognitionsCollection,
	collection2: AttributeRecognitionsCollection
): AttributeRecognitionsCollection {
	const merged: AttributeRecognitionsCollection = { ...collection1 };
	
	for (const [entity_id, entityAttrs] of Object.entries(collection2)) {
		// Skip metadata fields
		if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;
		
		if (typeof entityAttrs === 'object' && entityAttrs !== null) {
			const existingEntityAttrs = merged[entity_id] as Record<string, AttributeValue> | undefined;
			
			merged[entity_id] = {
				...(existingEntityAttrs || {}),
				...(entityAttrs as Record<string, AttributeValue>)
			};
			
			// For each attribute, keep the one with newer timestamp
			for (const [attr_name, attr_value] of Object.entries(entityAttrs)) {
				if (isAttributeValue(attr_value)) {
					const existing = existingEntityAttrs?.[attr_name];
					if (existing && isAttributeValue(existing)) {
						// Keep newer one
						if (existing.timestamp > attr_value.timestamp) {
							(merged[entity_id] as Record<string, AttributeValue>)[attr_name] = existing;
						}
					}
				}
			}
		}
	}
	
	// Update collection-level metadata (use newer timestamp)
	merged._timestamp = Math.max(
		collection1._timestamp || 0,
		collection2._timestamp || 0
	);
	
	return merged;
}

