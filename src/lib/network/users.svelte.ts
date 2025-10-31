import { get, writable, derived } from 'svelte/store';
import type { Writable } from 'svelte/store';
// V5: Import from v5 schemas
import type { Contact, ContactsCollectionData } from '$lib/protocol/schemas';
import { ContactSchema } from '$lib/protocol/schemas';

// V5: Import Holster contacts module (from v5 commons)
import {
	holsterContacts,
	isLoadingHolsterContacts,
	initializeHolsterContacts,
	cleanupHolsterContacts,
	persistHolsterContacts,
	updateHolsterContactsStore,
	deleteHolsterContact
} from './contacts.svelte';

// ================================
// USERS LIST SUBSCRIPTION (Holster)
// ================================

import { holster } from '$lib/network/holster.svelte';

let usersListCallback: ((data: any) => void) | null = null;
let isUsersListInitialized = false;

/**
 * Subscribe to freely-associating-players list from Holster
 */
function subscribeToUsersList() {
	if (isUsersListInitialized) {
		console.log('[USERS-LIST] Already subscribed');
		return;
	}

	usersListCallback = (data: any) => {
		if (!data) return;

		// Filter out metadata fields and deleted entries
		const usersData: Record<string, {alias: string, lastSeen: number}> = {};
		for (const [key, value] of Object.entries(data)) {
			if (value && typeof value === 'object' && !key.startsWith('_')) {
				usersData[key] = value as any;
			}
		}

		// Extract pub keys and aliases
		const pubKeys = Object.keys(usersData);
		const aliases: Record<string, string> = {};
		
		pubKeys.forEach(pubKey => {
			const userData = usersData[pubKey];
			if (userData?.alias) {
				aliases[pubKey] = userData.alias;
			}
		});

		// Update stores
		userPubKeys.set(pubKeys);
		userAliasesCache.set(aliases);
		
		console.log('[USERS-LIST] Updated:', {
			count: pubKeys.length,
			aliases: Object.keys(aliases).length
		});
	};

	holster.get('freely-associating-players').on(usersListCallback, true);
	isUsersListInitialized = true;
}

/**
 * Initialize users list subscription
 */
export function initializeUsersList() {
	console.log('[USERS-LIST] Initializing...');
	subscribeToUsersList();
}

/**
 * Cleanup users list subscription
 */
export function cleanupUsersList() {
	if (usersListCallback) {
		holster.get('freely-associating-players').off(usersListCallback);
		usersListCallback = null;
	}
	userPubKeys.set([]);
	userAliasesCache.set({});
	isUsersListInitialized = false;
	console.log('[USERS-LIST] Cleaned up');
}

// ================================
// CORE USER & CONTACT STORES (V5: Holster-Only)
// ================================

// User tracking stores
export const userPubKeys = writable<string[]>([]);

// V5: Contact management stores (Holster-only)
export const userContacts: Writable<ContactsCollectionData> = holsterContacts;
export const isLoadingContacts = isLoadingHolsterContacts;
export const contactSearchQuery = writable('');

// User name/alias caching stores
export const userAliasesCache = writable<Record<string, string>>({});
export const userNamesCache = writable<Record<string, string>>({});

// ================================
// DERIVED STORES
// ================================

// Contacts derived stores
export const contactsArray = derived(userContacts, ($userContacts) => {
	return Object.values($userContacts);
});

export const filteredContacts = derived(
	[contactsArray, contactSearchQuery],
	([$contactsArray, $searchQuery]) => {
		if (!$searchQuery.trim()) {
			return $contactsArray;
		}

		const query = $searchQuery.toLowerCase();
		return $contactsArray.filter(
			(contact) =>
				contact.name.toLowerCase().includes(query) ||
				contact.public_key?.toLowerCase().includes(query)
		);
	}
);

// Derived store that combines names and aliases, prioritizing names over aliases
export const userNamesOrAliasesCache = derived(
	[userNamesCache, userAliasesCache],
	([$userNamesCache, $userAliasesCache]) => {
		// Start with aliases as the base
		const combined = { ...$userAliasesCache };

		// Override with names where available (names take priority)
		Object.entries($userNamesCache).forEach(([pubkey, name]) => {
			combined[pubkey] = name;
		});

		return combined;
	}
);

// ================================
// CONTACT LIFECYCLE FUNCTIONS (V5: Holster-Only)
// ================================

/**
 * Initialize contacts loading and subscription
 * Call this when user logs in
 */
export function initializeContacts() {
	initializeHolsterContacts();
}

/**
 * Cleanup contacts subscription
 * Call this when user logs out
 */
export function cleanupContacts() {
	cleanupHolsterContacts();
}

/**
 * Persist contacts to backend
 * Called automatically via CRUD operations
 */
export async function persistContacts(contacts?: ContactsCollectionData) {
	return persistHolsterContacts(contacts);
}

// ================================
// CONTACT MANAGEMENT FUNCTIONS
// ================================

/**
 * Create a new contact
 */
export function createContact(
	contactData: Omit<Contact, 'contact_id' | 'created_at' | 'updated_at'>
): Contact {
	// Only check for duplicates if we have a meaningful public key
	const hasValidPublicKey = contactData.public_key && contactData.public_key.trim() !== '';

	if (hasValidPublicKey && getContactByPublicKey(contactData.public_key!)) {
		throw new Error('Contact already exists with this public key');
	}

	const now = Date.now();
	const contact_id = `contact_${now}_${Math.random().toString(36).substr(2, 9)}`;

	const newContact: Contact = {
		contact_id,
		name: contactData.name,
		public_key: hasValidPublicKey ? contactData.public_key : undefined,
		created_at: now,
		updated_at: now
	};

	// Validate the contact data
	const validatedContact = ContactSchema.parse(newContact);

	// Add to contacts collection
	const updatedContacts = {
		...get(userContacts),
		[contact_id]: validatedContact
	};

	// V5: Update store and persist (Holster-only)
	updateHolsterContactsStore(updatedContacts);

	// Force update the names cache immediately to ensure reactivity
	if (hasValidPublicKey) {
		userNamesCache.update((cache) => ({
			...cache,
			[contactData.public_key!]: contactData.name
		}));
	}

	return validatedContact;
}

/**
 * Update an existing contact
 */
export function updateContact(contact_id: string, updates: Partial<Contact>): void {
	const currentContacts = get(userContacts);
	const existingContact = currentContacts[contact_id];

	if (!existingContact) {
		//console.warn(`Contact with ID ${contact_id} not found`);
		return;
	}

	const updatedContact = {
		...existingContact,
		...updates,
		updated_at: Date.now()
	};

	// Validate the updated contact
	const validatedContact = ContactSchema.parse(updatedContact);

	// Force update the names cache immediately if name changed and has public key
	if (updates.name && validatedContact.public_key) {
		userNamesCache.update((cache) => ({
			...cache,
			[validatedContact.public_key!]: validatedContact.name
		}));
	}

	// Update contacts collection
	const updatedContacts = {
		...currentContacts,
		[contact_id]: validatedContact
	};

	// V5: Update store and persist (Holster-only)
	updateHolsterContactsStore(updatedContacts);
}

/**
 * Delete a contact (V5: Holster-only)
 */
export async function deleteContact(contact_id: string): Promise<void> {
	// Use Holster-specific delete that sets to null
	await deleteHolsterContact(contact_id);
}

/**
 * Get contact by public key
 */
export function getContactByPublicKey(public_key: string): Contact | undefined {
	const contacts = get(userContacts);
	return Object.values(contacts).find((contact) => contact.public_key === public_key);
}

/**
 * Check if a public key is already in use
 */
export function isPublicKeyInUse(public_key: string, excludeContactId?: string): boolean {
	// Only check for duplicates if we have a meaningful public key
	if (!public_key || public_key.trim() === '') {
		return false;
	}

	const contacts = get(userContacts);
	return Object.values(contacts).some(
		(contact) => contact.public_key === public_key && contact.contact_id !== excludeContactId
	);
}

// ================================
// USER IDENTIFICATION FUNCTIONS
// ================================

/**
 * Determine if an identifier is a contactId or pubKey
 */
export function getIdentifierType(identifier: string): 'contactId' | 'pubKey' {
	return identifier.startsWith('contact_') ? 'contactId' : 'pubKey';
}

/**
 * Get the public key for a given contact ID
 */
export function getPublicKeyFromContactId(contactId: string): string | undefined {
	const contacts = get(userContacts);
	const contact = contacts[contactId];

	if (!contact) {
		//console.warn(`[USERS] Contact not found for ID: ${contactId}`);
		return undefined;
	}

	if (!contact.public_key) {
		//console.warn(`[USERS] Contact ${contactId} (${contact.name}) has no public key`);
		return undefined;
	}

	return contact.public_key;
}

/**
 * Get the contact ID for a given public key (reverse lookup)
 */
export function getContactIdFromPublicKey(pubKey: string): string | undefined {
	const contacts = get(userContacts);
	const contact = Object.values(contacts).find((contact) => contact.public_key === pubKey);
	return contact?.contact_id;
}

/**
 * Resolve any identifier (contactId or pubKey) to a pubKey for network operations
 */
export function resolveToPublicKey(identifier: string): string | undefined {
	if (getIdentifierType(identifier) === 'contactId') {
		return getPublicKeyFromContactId(identifier);
	}

	// Already a pubKey, return as-is
	return identifier;
}

/**
 * Resolve multiple identifiers to public keys, filtering out any that can't be resolved
 */
export function resolveToPublicKeys(identifiers: string[]): string[] {
	return identifiers
		.map(resolveToPublicKey)
		.filter((pubKey): pubKey is string => pubKey !== undefined);
}

// ================================
// USER NAME/ALIAS FUNCTIONS
// ================================

/**
 * Get alias for a user by public key (V5: Holster-only)
 * Returns cached alias or fallback to truncated ID
 */
export async function getUserAlias(pubkey: string) {
	// Check the reactive cache
	const cache = get(userAliasesCache);
	if (cache[pubkey]) {
		return cache[pubkey];
	}

	// V5: Holster user data is loaded via holster.svelte.ts subscription
	// No need to fetch directly - data comes through reactive stores
	// If not in cache yet, return fallback
	console.log(`[USER-NAME-V5] Alias not yet cached for ${pubkey}, using fallback`);

	// Fallback to truncated ID
	const fallbackName = pubkey.substring(0, 8) + '...';
	return fallbackName;
}

/**
 * Get display name for a user by either pubKey or contactId
 * Prioritizes contact names over Gun aliases
 * Uses the combined cache for reactive components
 */
export async function getUserName(identifier: string): Promise<string> {
	// First check the combined cache (contacts take priority over aliases)
	const combinedCache = get(userNamesOrAliasesCache);
	if (combinedCache[identifier]) {
		return combinedCache[identifier];
	}

	let displayName: string;
	let shouldCacheInNamesCache = false;

	// Check if this is a contactId
	if (getIdentifierType(identifier) === 'contactId') {
		const contacts = get(userContacts);
		const contact = contacts[identifier];
		if (contact) {
			displayName = contact.name;
			shouldCacheInNamesCache = true; // Contact names go in userNamesCache
		} else {
			// If contactId not found, return the identifier itself as fallback
			displayName = identifier;
			shouldCacheInNamesCache = true; // Cache the fallback too
		}
	} else {
		// For pubKey-based lookup, check if we have a contact with this public key
		const contact = getContactByPublicKey(identifier);
		if (contact) {
			displayName = contact.name;
			shouldCacheInNamesCache = true; // Contact names go in userNamesCache
		} else {
			// If no contact found, fall back to Gun alias
			displayName = await getUserAlias(identifier);
			// Don't cache here - getUserAlias already handles userAliasesCache
		}
	}

	// Cache in the appropriate store based on the type of name
	if (shouldCacheInNamesCache) {
		userNamesCache.update((cache) => ({
			...cache,
			[identifier]: displayName
		}));
	}

	return displayName;
}

// ================================
// TREE CONTACT RESOLUTION
// ================================

/**
 * Resolve contact IDs to public keys in a tree structure
 * This ensures the tree is persisted with the most useful identifier format
 * while preserving contact IDs that don't have public keys
 */
export function resolveContactIdsInTree(
	node: import('$lib/protocol/schemas').Node
): import('$lib/protocol/schemas').Node {
	// Create a deep clone to avoid modifying the original
	const resolvedNode = structuredClone(node);

	// Helper function to resolve contributor arrays
	function resolveContributorArray(contributorIds: string[]): string[] {
		return contributorIds.map((contributorId) => {
			// Try to resolve contact IDs to public keys
			const resolvedPublicKey = resolveToPublicKey(contributorId);
			if (resolvedPublicKey && resolvedPublicKey !== contributorId) {
				console.log(
					`[PERSIST-RESOLVE] Resolved contact ID '${contributorId}' to public key '${resolvedPublicKey.substring(0, 20)}...'`
				);
				return resolvedPublicKey;
			}
			// Keep the original ID if it's already a public key or has no resolution
			return contributorId;
		});
	}

	// V5: Recursive function to process the tree with Contributor[] arrays
	function processNode(currentNode: import('$lib/protocol/schemas').Node): void {
		// Only NonRootNodes have contributor arrays
		if (currentNode.type === 'NonRootNode') {
			const nonRootNode = currentNode as import('$lib/protocol/schemas').NonRootNode;

			// V5: Resolve contributor IDs (extract from Contributor[] objects, resolve, reconstruct)
			if (nonRootNode.contributors && nonRootNode.contributors.length > 0) {
				const originalCount = nonRootNode.contributors.length;
				const contributorIds = nonRootNode.contributors.map(c => c.id);
				const resolvedIds = resolveContributorArray(contributorIds);
				// Reconstruct Contributor[] array with resolved IDs, preserving points
				nonRootNode.contributors = resolvedIds.map((id, index) => ({
					id,
					points: nonRootNode.contributors[index]?.points || 100
				}));
				console.log(
					`[PERSIST-RESOLVE] Processed ${originalCount} → ${nonRootNode.contributors.length} contributor IDs for node '${currentNode.name}' (${currentNode.id})`
				);
			}

			// V5: Resolve anti-contributor IDs
			if (nonRootNode.anti_contributors && nonRootNode.anti_contributors.length > 0) {
				const originalCount = nonRootNode.anti_contributors.length;
				const antiContributorIds = nonRootNode.anti_contributors.map(c => c.id);
				const resolvedIds = resolveContributorArray(antiContributorIds);
				// Reconstruct Contributor[] array with resolved IDs, preserving points
				nonRootNode.anti_contributors = resolvedIds.map((id, index) => ({
					id,
					points: nonRootNode.anti_contributors![index]?.points || 100
				}));
				console.log(
					`[PERSIST-RESOLVE] Processed ${originalCount} → ${nonRootNode.anti_contributors.length} anti-contributor IDs for node '${currentNode.name}' (${currentNode.id})`
				);
			}
		}

		// Recursively process all child nodes
		if (currentNode.children && currentNode.children.length > 0) {
			currentNode.children.forEach(processNode);
		}
	}

	// Start processing from the root
	processNode(resolvedNode);

	return resolvedNode;
}