/**
 * Contacts Module - Holster Implementation
 *
 * Migrated from Gun to Holster using the validated patterns from notes.svelte.ts:
 * - Application-level timestamps via _updatedAt fields
 * - Conflict resolution using shouldPersist()
 * - Real-time sync with subscription management
 * - Compatible with existing Contact schema
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from '$lib/network/holster.svelte';
// V5: Import from v5 schemas
import type { ContactsCollection, Contact } from '$lib/protocol/schemas';
import { ContactsCollectionSchema } from '$lib/protocol/schemas';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/data/holsterTimestamp';

// ============================================================================
// State
// ============================================================================

// Local contacts state
export const holsterContacts = writable<ContactsCollection>({});

// Loading flag
export const isLoadingHolsterContacts = writable(false);

// Track last known network timestamp for contacts collection
let lastNetworkTimestamp: number | null = null;
let hasReceivedRealData = false;

// Prevent duplicate initialization
let isInitialized: boolean = false;

// ============================================================================
// Subscription Management
// ============================================================================

let contactsCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's contacts from Holster
 */
function subscribeToContacts() {
	if (!holsterUser.is) {
		console.log('[CONTACTS-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}


	contactsCallback = (data: any) => {
		if (!data) {
			if (!hasReceivedRealData) {
				console.log('[CONTACTS-HOLSTER] Subscription returned null, waiting for network data...');
			}
			return;
		}

		if (!hasReceivedRealData) {
			console.log('[CONTACTS-HOLSTER] First real data received from network');
			hasReceivedRealData = true;
		}

		// Helper to check if a value is "deleted" (null or object with all null fields)
		const isDeleted = (value: any): boolean => {
			if (value === null) return true;
			if (typeof value === 'object' && value !== null) {
				// Check if all fields are null (Gun/Holster deletion pattern)
				return Object.values(value).every(v => v === null);
			}
			return false;
		};

		// Extract timestamp and filter out metadata fields AND null/deleted values
		const networkTimestamp = getTimestamp(data);
		const contactsOnly: any = {};
		for (const [key, value] of Object.entries(data)) {
			if (!isDeleted(value) && !key.startsWith('_')) {
				contactsOnly[key] = value;
			}
		}

		// Parse and validate (without timestamp)
		const parseResult = ContactsCollectionSchema.safeParse(contactsOnly);
		if (!parseResult.success) {
			console.error('[CONTACTS-HOLSTER] Invalid contacts data:', parseResult.error);
			return;
		}

		const networkContacts = parseResult.data;

		if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
			holsterContacts.set(networkContacts);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
			isLoadingHolsterContacts.set(false);
		}
	};

	holsterUser.get('contacts').on(contactsCallback, true);
}

/**
 * Initialize contacts subscription when user logs in
 */
export function initializeHolsterContacts() {
	if (!holsterUser.is) {
		console.log('[CONTACTS-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitialized) {
		console.log('[CONTACTS-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[CONTACTS-HOLSTER] Initializing contacts...');
	isInitialized = true;
	isLoadingHolsterContacts.set(true);

	subscribeToContacts();
}

/**
 * Cleanup subscription
 */
export function cleanupHolsterContacts() {
	if (contactsCallback && holsterUser.is) {
		holsterUser.get('contacts').off(contactsCallback);
		contactsCallback = null;
	}
	holsterContacts.set({});
	lastNetworkTimestamp = null;
	isInitialized = false;
	hasReceivedRealData = false;
	console.log('[CONTACTS-HOLSTER] Cleaned up');
}

/**
 * Reset initialization state (for logout/re-login in same session)
 */
export function resetInitialization() {
	console.log('[CONTACTS-HOLSTER] Resetting initialization state');
	cleanupHolsterContacts();
}

// ============================================================================
// Persistence
// ============================================================================

/**
 * Clean contacts data by removing undefined values
 * Holster/Gun cannot handle undefined - must use null or omit the field
 */
function cleanContactsData(contacts: ContactsCollection): ContactsCollection {
	const cleaned: ContactsCollection = {};

	for (const [contactId, contact] of Object.entries(contacts)) {
		cleaned[contactId] = {
			contact_id: contact.contact_id,
			name: contact.name,
			created_at: contact.created_at,
			updated_at: contact.updated_at
		};

		// Only include public_key if it's a non-empty string
		if (contact.public_key && contact.public_key.trim() !== '') {
			cleaned[contactId].public_key = contact.public_key;
		}
	}

	return cleaned;
}

/**
 * Persist contacts collection to Holster with conflict detection
 */
export async function persistHolsterContacts(
	contacts?: ContactsCollection
): Promise<void> {
	if (!holsterUser.is) {
		console.log('[CONTACTS-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const contactsToSave = contacts || get(holsterContacts);

	if (!contactsToSave || Object.keys(contactsToSave).length === 0) {
		console.log('[CONTACTS-HOLSTER] No contacts to persist');
		return;
	}

	// Clean the data (remove undefined values)
	const cleanedContacts = cleanContactsData(contactsToSave);

	// Add timestamp to the entire collection
	const timestampedContacts = addTimestamp(cleanedContacts);
	const localTimestamp = getTimestamp(timestampedContacts);

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
		console.warn('[CONTACTS-HOLSTER] Skipping persist - network has newer data');
		return;
	}

	console.log('[CONTACTS-HOLSTER] Persisting contacts...', timestampedContacts);

	return new Promise((resolve, reject) => {
		holsterUser.get('contacts').put(timestampedContacts, (err: any) => {
			if (err) {
				console.error('[CONTACTS-HOLSTER] Persist error:', err);
				reject(err);
			} else {
				if (localTimestamp) {
					lastNetworkTimestamp = localTimestamp;
				}
				resolve();
			}
		});
	});
}

// ============================================================================
// CRUD Operations (for compatibility with existing API)
// ============================================================================

/**
 * Delete a contact from Holster
 */
export async function deleteHolsterContact(contactId: string): Promise<void> {
	if (!holsterUser.is) {
		console.log('[CONTACTS-HOLSTER] Not authenticated, skipping delete');
		return;
	}

	console.log('[CONTACTS-HOLSTER] Deleting contact:', contactId);

	// Optimistically update local state
	holsterContacts.update((contacts) => {
		const { [contactId]: deleted, ...remaining } = contacts;
		return remaining;
	});

	// Remove from Holster by setting to null
	return new Promise((resolve, reject) => {
		holsterUser.get('contacts').next(contactId).put(null, (err: any) => {
			if (err) {
				console.error('[CONTACTS-HOLSTER] Delete error:', err);
				reject(err);
			} else {
				resolve();
			}
		});
	});
}

/**
 * Update the contacts store and persist
 * This is a helper that maintains the same API as the Gun version
 */
export async function updateHolsterContactsStore(
	updatedContacts: ContactsCollection
): Promise<void> {
	holsterContacts.set(updatedContacts);
	await persistHolsterContacts(updatedContacts);
}
