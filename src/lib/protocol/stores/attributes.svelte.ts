/**
 * Attribute Recognition - Svelte Store Integration
 * 
 * Provides reactive stores for the attribute recognition system using:
 * - createStore for local data (my recognitions, subscriptions, mappings)
 * - VersionedStore for network data (others' recognitions)
 * - ITC causality tracking like stores.svelte.ts
 * - No $effect - uses store.subscribe() pattern
 * 
 * Architecture follows stores.svelte.ts pattern:
 * - Local stores persist to Holster with ITC
 * - Network stores use VersionedStore for fine-grained reactivity
 * - Auto-subscription system (no $effect, just subscribe())
 */

import { derived, get } from 'svelte/store';
import type { Readable } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
import {
	AttributeRecognitionsCollectionSchema,
	AttributeSubscriptionsSchema,
	EntityIdMappingsSchema,
	type AttributeRecognitionsCollection,
	type AttributeSubscriptions,
	type EntityIdMappings,
	type AttributeValue
} from '@playnet/free-association/schemas';
import {
	getAttributeFromCollection,
	updateAttributeInCollection,
	removeAttributeFromCollection
} from '@playnet/free-association/attributes/attribute-recognition';
import { getEqualityChecker } from '@playnet/free-association/attributes/attribute-types';
import { leq as itcLeq, equals as itcEquals, join as itcJoin, type Stamp as ITCStamp } from '$lib/utils/primitives/itc';

import { holsterUserPub } from '$lib/network/holster.svelte';

// ═══════════════════════════════════════════════════════════════════
// LOCAL STORES (My Data) - Using createStore pattern
// ═══════════════════════════════════════════════════════════════════

/**
 * My Attribute Recognitions Store
 * 
 * Stores attributes I recognize about entities (users, orgs, etc.)
 * - Persists to Holster at 'attributes/recognitions'
 * - ITC causality tracking built-in
 * - Debounced persistence (100ms)
 * 
 * Example data:
 * {
 *   "org_abc123": {
 *     "membership": { value: ["alice", "bob"], confidence: 1.0, ... }
 *   },
 *   "pubkey_bob": {
 *     "capacity:food": { value: [...slots...], confidence: 1.0, ... }
 *   },
 *   _itcStamp: {...},
 *   _timestamp: 1234567890
 * }
 */
export const myAttributeRecognitions = createStore({
	holsterPath: 'attributes/recognitions',
	schema: AttributeRecognitionsCollectionSchema,
	persistDebounce: 100
});

/**
 * My Attribute Subscriptions Store
 * 
 * Configures which sources to subscribe to for entity attributes.
 * - Persists to Holster at 'attributes/subscriptions'
 * - Debounced persistence (100ms)
 * 
 * Example data:
 * {
 *   "org_abc123": {
 *     "membership": "pubkey_alice"  // Subscribe to Alice's view
 *   },
 *   "pubkey_bob": {
 *     "capacity:food": "pubkey_bob"  // Subscribe to Bob's capacity
 *   }
 * }
 */
export const myAttributeSubscriptions = createStore({
	holsterPath: 'attributes/subscriptions',
	schema: AttributeSubscriptionsSchema,
	persistDebounce: 100
});

/**
 * My Entity ID Mappings Store
 * 
 * Maps local identifiers (uuid, contact_id) to public keys.
 * - Persists to Holster at 'attributes/id_mappings'
 * - Debounced persistence (100ms)
 * 
 * Example data:
 * {
 *   "contact_alice_123": "pubkey_abc...",
 *   "uuid_def_456": "pubkey_xyz..."
 * }
 */
export const myEntityIdMappings = createStore({
	holsterPath: 'attributes/id_mappings',
	schema: EntityIdMappingsSchema,
	persistDebounce: 100
});

// ═══════════════════════════════════════════════════════════════════
// NO SEPARATE NETWORK CACHE! (Unified Storage Pattern)
// ═══════════════════════════════════════════════════════════════════

/**
 * UNIFIED STORAGE ARCHITECTURE
 * 
 * Like stores.svelte.ts pattern:
 * - Subscription data writes INTO myAttributeRecognitions
 * - ITC causality prevents stale updates
 * - source_pubkey tracks where data came from
 * - Manual edits can "win" if causally newer
 * 
 * Similar to:
 *   myCommitmentStore.others_recognition_of_me ← cache in commitment
 *   myAttributeRecognitions ← cache for all attributes
 * 
 * No separate networkAttributeCache needed!
 */

// ═══════════════════════════════════════════════════════════════════
// SUBSCRIPTION MANAGEMENT
// ═══════════════════════════════════════════════════════════════════

const activeSubscriptions = new Set<string>();

/**
 * Subscribe to a user's attribute recognitions
 * 
 * UNIFIED STORAGE: Writes INTO myAttributeRecognitions with ITC checking!
 * 
 * Pattern (like stores.svelte.ts networkCommitments subscriber):
 * 1. Receive their attribute recognitions
 * 2. For each attribute they declare:
 *    a. Check our current value's ITC stamp
 *    b. Skip if theirs is causally stale
 *    c. Merge ITC if concurrent
 *    d. Write to myAttributeRecognitions with their source_pubkey
 * 
 * Manual edits win if causally newer! (ITC handles this)
 * 
 * @param pubkey - User's public key
 */
export function subscribeToAttributeRecognitions(pubkey: string) {
	if (activeSubscriptions.has(pubkey)) return;

	myAttributeRecognitions.subscribeToUser(pubkey, (theirRecognitions) => {
		console.log(`[📡 ATTR-SUB] Received recognitions from ${pubkey.slice(0, 20)}...`);

		// Handle deletion - remove attributes with this source
		if (!theirRecognitions) {
			const ourCurrent = get(myAttributeRecognitions);
			if (!ourCurrent) return;

			const { updated, count } = removeAttributesBySource(ourCurrent, pubkey);

			if (count > 0) {
				myAttributeRecognitions.set(updated);
				console.log(`[📡 ATTR-SUB] 🗑️  Removed ${count} attributes from ${pubkey.slice(0, 20)}...`);
			}
			return;
		}

		// Process each entity's attributes from their recognitions
		const ourCurrent = get(myAttributeRecognitions) || { _timestamp: Date.now() };
		let updated = ourCurrent;
		let appliedCount = 0;
		let skippedCount = 0;

		for (const [entity_id, entityAttrs] of Object.entries(theirRecognitions)) {
			if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;
			if (typeof entityAttrs !== 'object' || entityAttrs === null) continue;

			for (const [attr_name, theirAttr] of Object.entries(entityAttrs)) {
				if (typeof theirAttr !== 'object' || theirAttr === null) continue;
				if (!('value' in theirAttr) || !('timestamp' in theirAttr)) continue;

				const theirValue = theirAttr as AttributeValue;
				const ourAttr = getAttributeFromCollection(ourCurrent, entity_id, attr_name);

				// ✅ ITC CAUSALITY CHECK
				if (!checkAndMergeITC(ourAttr, theirValue, entity_id, attr_name, pubkey)) {
					skippedCount++;
					continue;
				}

				// ✅ Get custom equality checker for this attribute type
				const equalityChecker = getEqualityChecker(attr_name);

				// ✅ Write to unified collection with source tracking AND change detection
				updated = updateAttributeInCollection(
					updated,
					entity_id,
					attr_name,
					theirValue.value,
					pubkey,
					theirValue.confidence,
					theirValue.itcStamp,
					equalityChecker // ← Custom equality checker for optimized change detection
				);

				appliedCount++;
			}
		}

		// Apply updates if any
		if (appliedCount > 0) {
			myAttributeRecognitions.set(updated);
			console.log(`[📡 ATTR-SUB] ✅ Applied ${appliedCount} attributes from ${pubkey.slice(0, 20)}... (skipped ${skippedCount} stale)`);
		} else {
			console.log(`[📡 ATTR-SUB] ⏭️  No updates from ${pubkey.slice(0, 20)}... (all stale or empty)`);
		}
	});

	activeSubscriptions.add(pubkey);
	console.log(`[📡 ATTR-SUB] ✅ Subscribed to ${pubkey.slice(0, 20)}... attribute recognitions`);
}

/**
 * Unsubscribe from a user's attribute recognitions
 * 
 * Removes attributes sourced from this pubkey.
 * 
 * @param pubkey - User's public key
 */
export function unsubscribeFromAttributeRecognitions(pubkey: string) {
	activeSubscriptions.delete(pubkey);

	const ourCurrent = get(myAttributeRecognitions);
	if (!ourCurrent) return;

	const { updated, count } = removeAttributesBySource(ourCurrent, pubkey);

	if (count > 0) {
		myAttributeRecognitions.set(updated);
		console.log(`[📡 ATTR-SUB] 🗑️  Removed ${count} attributes from ${pubkey.slice(0, 20)}...`);
	}

	console.log(`[📡 ATTR-SUB] Unsubscribed from ${pubkey.slice(0, 20)}...`);
}

/**
 * Get list of subscribed pubkeys
 */
export function getSubscribedPubkeys(): string[] {
	return Array.from(activeSubscriptions);
}

// ═══════════════════════════════════════════════════════════════════
// AUTO-SUBSCRIPTION SYSTEM (No $effect - uses store.subscribe())
// ═══════════════════════════════════════════════════════════════════

/**
 * Enable automatic subscription syncing
 * 
 * Watches myAttributeSubscriptions and auto-subscribes to configured sources.
 * Similar pattern to enableAutoMembershipSync() in stores.svelte.ts.
 * 
 * NO $EFFECT - uses store.subscribe() pattern!
 * 
 * Flow:
 * 1. User configures subscription: myAttributeSubscriptions.update(...)
 * 2. This function detects the change via .subscribe()
 * 3. Subscribes to source's attribute recognitions via Holster
 * 4. When their data arrives, updates networkAttributeRecognitions
 * 5. Resolution functions use networkAttributeCache automatically
 * 
 * Returns unsubscribe function to disable auto-syncing
 */
export function enableAutoAttributeSync(): () => void {
	console.log('[AUTO-ATTR-SYNC] 🔄 Enabling automatic attribute syncing');

	// Track active subscriptions to avoid duplicates
	const activeSubs = new Map<string, () => void>();

	// Subscribe to changes in attribute subscriptions
	const unsubscribe = myAttributeSubscriptions.subscribe(($subs) => {
		if (!$subs) return;

		// Get all unique source pubkeys
		const sourcePubkeys = new Set<string>();
		for (const entitySubs of Object.values($subs)) {
			for (const source_pubkey of Object.values(entitySubs)) {
				sourcePubkeys.add(source_pubkey);
			}
		}

		console.log(`[AUTO-ATTR-SYNC] Processing ${sourcePubkeys.size} source pubkeys`);

		// Subscribe to new sources
		for (const pubkey of sourcePubkeys) {
			if (activeSubs.has(pubkey)) continue;

			console.log(`[AUTO-ATTR-SYNC] ➕ Subscribing to ${pubkey.slice(0, 20)}...'s attribute recognitions`);

			subscribeToAttributeRecognitions(pubkey);

			// Track this subscription
			activeSubs.set(pubkey, () => {
				console.log(`[AUTO-ATTR-SYNC] ⏸️  Unsubscribed from ${pubkey.slice(0, 20)}...`);
			});
		}

		// Cleanup removed subscriptions
		const currentKeys = sourcePubkeys;

		for (const [pubkey, cleanup] of activeSubs.entries()) {
			if (!currentKeys.has(pubkey)) {
				console.log(`[AUTO-ATTR-SYNC] ➖ Removing subscription: ${pubkey.slice(0, 20)}...`);
				cleanup();
				unsubscribeFromAttributeRecognitions(pubkey);
				activeSubs.delete(pubkey);
			}
		}
	});

	return () => {
		unsubscribe();
		activeSubs.clear();
		console.log('[AUTO-ATTR-SYNC] ⏸️  Disabled automatic attribute syncing');
	};
}

// ═══════════════════════════════════════════════════════════════════
// CONVENIENCE HELPERS (SIMPLIFIED - Unified Storage!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Resolution Result - Tracks data provenance
 */
export interface ResolutionResult {
	/** Resolved attribute value (undefined if not found) */
	value: any;

	/** Source pubkey that provided this value */
	source_pubkey?: string;

	/** How was this resolved? */
	resolution_type: 'subscription' | 'self' | 'local' | 'not_found';

	/** Confidence level (0-1) */
	confidence: number;

	/** Timestamp when value was declared */
	timestamp?: number;
}

/**
 * Determine resolution type from source_pubkey
 * 
 * Pure helper function that encapsulates the resolution type logic.
 */
function getResolutionType(
	source_pubkey: string | undefined,
	resolved_entity_id: string,
	subscribedSource: string | undefined
): 'subscription' | 'self' | 'local' {
	if (!source_pubkey) return 'local';

	if (subscribedSource && source_pubkey === subscribedSource) return 'subscription';
	if (source_pubkey === resolved_entity_id) return 'self';
	return 'local';
}

/**
 * Remove all attributes from a specific source
 * 
 * Pure helper function for cleanup operations.
 */
function removeAttributesBySource(
	collection: AttributeRecognitionsCollection,
	source_pubkey: string
): { updated: AttributeRecognitionsCollection; count: number } {
	let updated = collection;
	let count = 0;

	for (const [entity_id, entityAttrs] of Object.entries(collection)) {
		if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;

		if (typeof entityAttrs === 'object' && entityAttrs !== null) {
			for (const [attr_name, attr_value] of Object.entries(entityAttrs)) {
				if (typeof attr_value === 'object' && attr_value !== null &&
					'source_pubkey' in attr_value && attr_value.source_pubkey === source_pubkey) {
					updated = removeAttributeFromCollection(updated, entity_id, attr_name);
					count++;
				}
			}
		}
	}

	return { updated, count };
}

/**
 * Check ITC causality and merge if needed
 * 
 * Returns true if the incoming value should be applied.
 * Mutates theirValue.itcStamp if concurrent (merges stamps).
 */
function checkAndMergeITC(
	ourAttr: AttributeValue | undefined,
	theirValue: AttributeValue,
	entity_id: string,
	attr_name: string,
	source_pubkey: string
): boolean {
	if (!ourAttr?.itcStamp || !theirValue.itcStamp) return true;

	// Skip if theirs is causally stale
	if (itcLeq(theirValue.itcStamp, ourAttr.itcStamp) &&
		!itcEquals(theirValue.itcStamp, ourAttr.itcStamp)) {
		console.log(`[📡 ATTR-SUB] ⏭️  ITC stale: ${entity_id}/${attr_name} from ${source_pubkey.slice(0, 20)}...`);
		return false;
	}

	// Merge ITC stamps (concurrent updates)
	theirValue.itcStamp = itcJoin(ourAttr.itcStamp, theirValue.itcStamp);
	console.log(`[📡 ATTR-SUB] 🔀 Merged ITC for ${entity_id}/${attr_name}`);
	return true;
}

/**
 * Resolve attribute value with provenance
 * 
 * SIMPLIFIED: Just reads from myAttributeRecognitions!
 * Resolution type determined by source_pubkey + subscription config.
 * 
 * @param entity_id - Entity to resolve attribute for
 * @param attribute_name - Attribute name
 * @returns Resolution result with provenance
 * 
 * @example
 * ```typescript
 * const result = resolveAttribute("org_abc123", "membership");
 * console.log(result.value); // ["alice", "bob"]
 * console.log(result.resolution_type); // "subscription"
 * ```
 */
export function resolveAttribute(
	entity_id: string,
	attribute_name: string
): ResolutionResult {
	const subscriptions = get(myAttributeSubscriptions) || {};
	const recognitions = get(myAttributeRecognitions) || { _timestamp: Date.now() };
	const idMappings = get(myEntityIdMappings) || {};

	const resolved_entity_id = idMappings[entity_id] || entity_id;
	const attr = getAttributeFromCollection(recognitions, resolved_entity_id, attribute_name);

	if (!attr) {
		return { value: undefined, resolution_type: 'not_found', confidence: 0 };
	}

	const subscribedSource = subscriptions[entity_id]?.[attribute_name];
	const resolution_type = getResolutionType(attr.source_pubkey, resolved_entity_id, subscribedSource);

	return {
		value: attr.value,
		source_pubkey: attr.source_pubkey,
		resolution_type,
		confidence: attr.confidence ?? 0,
		timestamp: attr.timestamp
	};
}

/**
 * Get attribute value (simple version)
 * 
 * Just returns the value without provenance details.
 * 
 * @param entity_id - Entity to get attribute for
 * @param attribute_name - Attribute name
 * @returns Attribute value or undefined
 * 
 * @example
 * ```typescript
 * const members = getAttribute("org_abc123", "membership");
 * // Returns: ["alice", "bob"] or undefined
 * ```
 */
export function getAttribute(
	entity_id: string,
	attribute_name: string
): any | undefined {
	return resolveAttribute(entity_id, attribute_name).value;
}

/**
 * Reactive attribute value store
 * 
 * Creates a derived store that reactively resolves an attribute value.
 * Updates automatically when myAttributeRecognitions changes.
 * 
 * @param entity_id - Entity to resolve attribute for
 * @param attribute_name - Attribute name
 * @returns Readable store with resolved value
 * 
 * @example
 * ```typescript
 * const members = createAttributeStore("org_abc123", "membership");
 * // members updates automatically when data changes
 * ```
 */
export function createAttributeStore(
	entity_id: string,
	attribute_name: string
): Readable<any | undefined> {
	// Simpler: Just wrap getAttribute() reactively
	return derived(
		[myAttributeRecognitions, myEntityIdMappings],
		() => getAttribute(entity_id, attribute_name)
	);
}

/**
 * Reactive resolution result store
 * 
 * Creates a derived store that reactively resolves with full provenance.
 * 
 * @param entity_id - Entity to resolve attribute for
 * @param attribute_name - Attribute name
 * @returns Readable store with ResolutionResult
 */
export function createResolutionStore(
	entity_id: string,
	attribute_name: string
): Readable<ResolutionResult> {
	// Just reactively wrap resolveAttribute - simpler and DRY!
	return derived(
		[myAttributeRecognitions, myAttributeSubscriptions, myEntityIdMappings],
		() => resolveAttribute(entity_id, attribute_name)
	);
}

// ═══════════════════════════════════════════════════════════════════
// INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize all attribute stores
 * 
 * Call this after holster authentication.
 * Similar to initializeAllocationStores() in stores.svelte.ts.
 */
export function initializeAttributeStores() {
	console.log('[ATTR-STORES] Initializing stores...');

	myAttributeRecognitions.initialize();
	myAttributeSubscriptions.initialize();
	myEntityIdMappings.initialize();

	console.log('[ATTR-STORES] Stores initialized:');
	console.log('  - My attribute recognitions (persistent)');
	console.log('  - My attribute subscriptions (persistent)');
	console.log('  - My entity ID mappings (persistent)');
	console.log('  - Network attribute recognitions (versioned)');
}

/**
 * Cleanup all attribute stores
 * 
 * Call this before logout.
 */
export async function cleanupAttributeStores() {
	console.log('[ATTR-STORES] Cleaning up stores...');

	await myAttributeRecognitions.cleanup();
	await myAttributeSubscriptions.cleanup();
	await myEntityIdMappings.cleanup();

	console.log('[ATTR-STORES] Stores cleaned up');
}

// ═══════════════════════════════════════════════════════════════════
// FINE-GRAINED DERIVED STORES (VersionedStore parity!)
// ═══════════════════════════════════════════════════════════════════

/**
 * Derive a store for a specific attribute across all entities
 * 
 * ✅ Fine-grained reactivity: Only updates when THIS attribute changes
 * ✅ Prevents unnecessary re-renders when other attributes change
 * 
 * Comparable to VersionedStore's `deriveField()` but for attributes.
 * 
 * @param attributeName - Attribute to track (e.g., "membership", "capacity:food")
 * @returns Readable store with Map of entity_id → AttributeValue
 * 
 * @example
 * ```typescript
 * // Only updates when ANY entity's membership changes
 * const membershipStore = deriveAttribute('membership');
 * membershipStore.subscribe(membershipMap => {
 *   console.log('Membership changed!', membershipMap);
 *   // Map<entity_id, AttributeValue>
 * });
 * ```
 */
export function deriveAttribute(attributeName: string): Readable<Map<string, AttributeValue>> {
	// State maintained across updates
	let attributeMap = new Map<string, AttributeValue>();
	let lastITCStamps = new Map<string, ITCStamp>();
	let isFirstRun = true;

	return derived(myAttributeRecognitions, ($collection, set) => {
		if (!$collection) {
			// Empty collection
			attributeMap = new Map();
			lastITCStamps = new Map();
			set(attributeMap);
			isFirstRun = false;
			return;
		}

		let changed = isFirstRun;

		// Check each entity for this attribute
		for (const [entity_id, entityAttrs] of Object.entries($collection)) {
			if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;
			if (typeof entityAttrs !== 'object' || entityAttrs === null) continue;

			const attr = (entityAttrs as Record<string, AttributeValue>)[attributeName];

			if (attr && typeof attr === 'object' && 'value' in attr && 'itcStamp' in attr && attr.itcStamp) {
				// Attribute exists - check if ITC changed
				const currentITC = attr.itcStamp;
				const lastITC = lastITCStamps.get(entity_id);

				// ✅ Use ITC equality check instead of timestamp
				if (!lastITC || !itcEquals(currentITC, lastITC)) {
					changed = true;
					lastITCStamps.set(entity_id, currentITC);
					attributeMap.set(entity_id, attr);
				}
			} else if (attributeMap.has(entity_id)) {
				// Attribute was deleted
				changed = true;
				attributeMap.delete(entity_id);
				lastITCStamps.delete(entity_id);
			}
		}

		// Check for entity deletions
		for (const entity_id of attributeMap.keys()) {
			if (!$collection[entity_id]) {
				changed = true;
				attributeMap.delete(entity_id);
				lastITCStamps.delete(entity_id);
			}
		}

		// ✅ Always notify on first run, then only if actually changed
		if (changed) {
			attributeMap = new Map(attributeMap); // Clone for reactivity
			set(attributeMap);
			isFirstRun = false;
		}
	});
}

/**
 * Derive a store for a specific attribute of a specific entity
 * 
 * ✅ Maximum fine-grained reactivity: Only updates when THIS entity's THIS attribute changes
 * ✅ Prevents unnecessary re-renders for all other changes
 * 
 * Comparable to VersionedStore's `subscribeToFieldForKey()`.
 * 
 * @param entityId - Entity to track
 * @param attributeName - Attribute to track
 * @returns Readable store with AttributeValue or undefined
 * 
 * @example
 * ```typescript
 * // Only updates when org_123's membership changes
 * const orgMembershipStore = deriveEntityAttribute('org_123', 'membership');
 * orgMembershipStore.subscribe(membership => {
 *   console.log('Org membership:', membership?.value);
 * });
 * ```
 */
export function deriveEntityAttribute(
	entityId: string,
	attributeName: string
): Readable<AttributeValue | undefined> {
	let lastITC: ITCStamp | null = null;
	let isFirstRun = true;

	return derived(myAttributeRecognitions, ($collection, set) => {
		if (!$collection) {
			lastITC = null;
			set(undefined);
			isFirstRun = false;
			return;
		}

		const attr = getAttributeFromCollection($collection, entityId, attributeName);

		if (attr && attr.itcStamp) {
			// ✅ Check if ITC changed
			const currentITC = attr.itcStamp;
			if (!lastITC || !itcEquals(currentITC, lastITC) || isFirstRun) {
				lastITC = currentITC;
				set(attr);
				isFirstRun = false;
			}
		} else if (lastITC !== null || isFirstRun) {
			// Was deleted or doesn't exist or first run
			lastITC = null;
			set(undefined);
			isFirstRun = false;
		}
	});
}

/**
 * Derive a store for all attributes of a specific entity
 * 
 * ✅ Entity-level reactivity: Only updates when THIS entity's attributes change
 * ✅ Prevents unnecessary re-renders when other entities change
 * 
 * @param entityId - Entity to track
 * @returns Readable store with Record<attribute_name, AttributeValue>
 * 
 * @example
 * ```typescript
 * // Only updates when contact_alice's attributes change
 * const aliceStore = deriveEntity('contact_alice');
 * aliceStore.subscribe(attrs => {
 *   console.log('Alice name:', attrs.name?.value);
 *   console.log('Alice email:', attrs.email?.value);
 * });
 * ```
 */
export function deriveEntity(entityId: string): Readable<Record<string, AttributeValue>> {
	let lastAttributeITCs = new Map<string, ITCStamp>();
	let lastAttributeCount = 0;
	let lastAttributes: Record<string, AttributeValue> = {};
	let isFirstRun = true;
	let lastExisted = false;

	return derived(myAttributeRecognitions, ($collection, set) => {
		if (!$collection) {
			if (isFirstRun || lastExisted) {
				lastAttributes = {};
				lastAttributeITCs = new Map();
				lastAttributeCount = 0;
				lastExisted = false;
				set(lastAttributes);
			}
			isFirstRun = false;
			return;
		}

		const entityAttrs = $collection[entityId];

		if (!entityAttrs || typeof entityAttrs !== 'object') {
			// Entity doesn't exist
			if (isFirstRun || lastExisted) {
				lastAttributes = {};
				lastAttributeITCs = new Map();
				lastAttributeCount = 0;
				lastExisted = false;
				set(lastAttributes);
			}
			isFirstRun = false;
			return;
		}

		// ✅ Check if any attribute's ITC changed
		let changed = isFirstRun;
		const currentAttrs: Record<string, AttributeValue> = {};

		for (const [attr_name, attr_value] of Object.entries(entityAttrs)) {
			if (typeof attr_value === 'object' && attr_value !== null && 'value' in attr_value && 'itcStamp' in attr_value) {
				const attrVal = attr_value as AttributeValue;
				currentAttrs[attr_name] = attrVal;

				const currentITC = attrVal.itcStamp;
				const lastITC = lastAttributeITCs.get(attr_name);

				// ✅ Use ITC equality check
				if (currentITC && (!lastITC || !itcEquals(currentITC, lastITC))) {
					changed = true;
				}
			}
		}

		const currentAttributeCount = Object.keys(currentAttrs).length;

		// Also trigger if attribute count changed
		if (currentAttributeCount !== lastAttributeCount) {
			changed = true;
		}

		// ✅ Always notify on first run, then only if entity actually changed
		if (changed) {
			// Update all ITC stamps
			lastAttributeITCs = new Map();
			for (const [attr_name, attr_value] of Object.entries(currentAttrs)) {
				if (attr_value.itcStamp) {
					lastAttributeITCs.set(attr_name, attr_value.itcStamp);
				}
			}

			lastAttributeCount = currentAttributeCount;
			lastAttributes = currentAttrs;
			lastExisted = true;
			set(currentAttrs);
			isFirstRun = false;
		}
	});
}

// ═══════════════════════════════════════════════════════════════════
// DEBUGGING
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined') {
	(window as any).debugAttributeStores = () => {
		const subs = get(myAttributeSubscriptions);
		const recognitions = get(myAttributeRecognitions);
		const mappings = get(myEntityIdMappings);

		// Count attributes by source
		const sourceStats: Record<string, number> = {};
		if (recognitions) {
			for (const [entity_id, entityAttrs] of Object.entries(recognitions)) {
				if (entity_id === '_itcStamp' || entity_id === '_timestamp') continue;

				if (typeof entityAttrs === 'object' && entityAttrs !== null) {
					for (const attr_value of Object.values(entityAttrs)) {
						if (typeof attr_value === 'object' && attr_value !== null && 'source_pubkey' in attr_value) {
							const source = (attr_value as any).source_pubkey || 'local';
							sourceStats[source] = (sourceStats[source] || 0) + 1;
						}
					}
				}
			}
		}

		console.log('[ATTR-DEBUG] My Subscriptions:', subs);
		console.log('[ATTR-DEBUG] My Recognitions:', recognitions);
		console.log('[ATTR-DEBUG] My ID Mappings:', mappings);
		console.log('[ATTR-DEBUG] Attributes by Source:', sourceStats);
		console.log('[ATTR-DEBUG] Active Subscriptions:', getSubscribedPubkeys());
	};

	console.log('[ATTR-DEBUG] 🛠️  Debug utility available: window.debugAttributeStores()');
}

