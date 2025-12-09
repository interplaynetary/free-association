/**
 * Unified Entity System
 * 
 * All entities are just: UUID/pubkey → attributes
 * 
 * Architecture:
 * - **Entity ID** = UUID (contact_id, org_id) OR pubkey
 * - **Contacts** = Person entities (UUID → {name, pubkey?, emoji, notes})
 * - **Organizations** = Group entities (UUID → {names, emoji, description})
 * - **Attributes** = What we recognize about entities (skills, location, membership, etc.)
 * 
 * Key insights:
 * - UUIDs are NOT "local" - both contact_id and org_id can be shared across network
 * - The prefixes (contact_, org_) are naming conventions, not semantic barriers
 * - Entity semantics emerge from attributes, not from type system
 * - Has 'public_key' attribute? → Person entity
 * - Has 'membership' attribute? → Organization entity
 */

import { get } from 'svelte/store';
import {
	myAttributeRecognitions,
	myAttributeSubscriptions
} from '$lib/protocol/stores/attributes.svelte';
import {
	updateAttributeInCollection,
	getAttributeFromCollection,
	removeAttributeFromCollection,
	getAllAttributesForEntity,
	getEntitiesWithAttribute
} from '@playnet/free-association/attributes/attribute-recognition';
import { holsterUserPub } from './holster.svelte';
import { getPublicKeyFromContactId } from './users.svelte';

// ═══════════════════════════════════════════════════════════════════
// CORE ENTITY ATTRIBUTE OPERATIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Set an attribute for any entity (contact_id, org_id, or pubkey)
 */
export function setEntityAttribute(
	entity_id: string,
	attribute_name: string,
	value: any
): void {
	const selfPubkey = get(holsterUserPub);
	let current = get(myAttributeRecognitions) || { _timestamp: Date.now() };

	current = updateAttributeInCollection(
		current,
		entity_id,
		attribute_name,
		value,
		selfPubkey
	);

	myAttributeRecognitions.set(current);
}

/**
 * Get an attribute for any entity
 */
export function getEntityAttribute(
	entity_id: string,
	attribute_name: string
): any | undefined {
	const collection = get(myAttributeRecognitions);
	if (!collection) return undefined;

	const attr = getAttributeFromCollection(collection, entity_id, attribute_name);
	return attr?.value;
}

/**
 * Remove an attribute from an entity
 */
export function removeEntityAttribute(
	entity_id: string,
	attribute_name: string
): void {
	let current = get(myAttributeRecognitions) || { _timestamp: Date.now() };
	current = removeAttributeFromCollection(current, entity_id, attribute_name);
	myAttributeRecognitions.set(current);
}

/**
 * Get all attributes for an entity
 */
export function getEntityAttributes(entity_id: string): Record<string, any> {
	const collection = get(myAttributeRecognitions);
	if (!collection) return {};

	return getAllAttributesForEntity(collection, entity_id);
}

/**
 * Resolve any entity ID to its pubkey (if possible)
 * 
 * Resolution order:
 * 1. If already a pubkey → return as-is
 * 2. If contact_id → lookup public_key in contacts
 * 3. If org_id → orgs don't have direct pubkeys (use membership instead)
 * 4. Otherwise → return undefined
 */
export function resolveEntityPubkey(entity_id: string): string | undefined {
	// If it's already a pubkey (not a UUID with prefix), return as-is
	if (!entity_id.startsWith('contact_') && !entity_id.startsWith('org_')) {
		return entity_id;
	}

	// If it's a contact_id, lookup public_key in contacts
	if (entity_id.startsWith('contact_')) {
		return getPublicKeyFromContactId(entity_id);
	}

	// org_id entities don't have direct pubkeys
	// (use membership attribute to get member pubkeys instead)
	return undefined;
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE QUERIES
// ═══════════════════════════════════════════════════════════════════

/**
 * Get all entities with a specific attribute
 */
export function getEntitiesWithAttributeName(attribute_name: string): string[] {
	const collection = get(myAttributeRecognitions);
	if (!collection) return [];

	return getEntitiesWithAttribute(collection, attribute_name);
}

/**
 * Check if an entity has a specific attribute
 */
export function hasAttribute(entity_id: string, attribute_name: string): boolean {
	return getEntityAttribute(entity_id, attribute_name) !== undefined;
}

