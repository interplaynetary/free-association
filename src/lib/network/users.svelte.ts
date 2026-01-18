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

// Import organizations module for org_id resolution
import { globalOrganizations, getOrganizationName, cleanupOrganizations } from './organizations.svelte';

// ================================
// USERS LIST SUBSCRIPTION (Holster)
// ================================

console.log('[TRACE] src/lib/network/users.svelte.ts: <module scope>');

import { holster } from '$lib/network/holster.svelte';

let usersListCallback: ((data: any) => void) | null = null;
let isUsersListInitialized = false;

/**
 * Subscribe to freely-associating-players list from Holster
 */
function subscribeToUsersList() {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: subscribeToUsersList');
	if (isUsersListInitialized) {
		console.log('[USERS-LIST] Already subscribed');
		console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: subscribeToUsersList (already subscribed)');
		return;
	}

	usersListCallback = (data: any) => {
		if (!data) return;

		// Filter out metadata fields and deleted entries
		const usersData: Record<string, { alias: string, lastSeen: number }> = {};
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

		// Update stores with NON-DESTRUCTIVE merge
		userPubKeys.update(current => {
			// Merge current and new keys, removing duplicates
			const merged = new Set([...current, ...pubKeys]);
			return [...merged];
		});

		userAliasesCache.update(current => {
			// Merge new aliases into current cache
			// New aliases overwrite old ones if they exist (which is what we want for updates)
			return {
				...current,
				...aliases
			};
		});

		console.log('[USERS-LIST] Updated (Merged):', {
			received: pubKeys.length,
			totalKnown: get(userPubKeys).length
		});
	};

	holster.get('freely-associating-players').on(usersListCallback, true);
	isUsersListInitialized = true;
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: subscribeToUsersList');
}

/**
 * Initialize users list subscription
 */
export function initializeUsersList() {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: initializeUsersList');
	console.log('[USERS-LIST] Initializing...');
	subscribeToUsersList();
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: initializeUsersList');
}

/**
 * Cleanup users list subscription
 * 
 * NOTE: This does NOT clear the users list data (userPubKeys, userAliasesCache)
 * to preserve read-only browsing mode after logout. The subscription remains
 * active for continued network browsing.
 */
export async function cleanupUsersList() {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: cleanupUsersList');
	// NOTE: We do NOT unsubscribe or clear data here anymore!
	// The users list subscription persists for read-only browsing.
	// Only auth-specific data is cleared on logout.

	console.log('[USERS-LIST] Keeping subscription active for read-only browsing');

	// Also cleanup organizations (but keep their subscriptions active too)
	// const orgsModule = require('$lib/network/organizations.svelte');
	cleanupOrganizations();

	// Note: Membership is now handled by the unified entity/attribute system
	// Cleaned up with myAttributeRecognitions store
	console.log('[USERS-LIST] Auth-specific data cleaned up');
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: cleanupUsersList');
}

// ================================
// CORE USER & CONTACT STORES (V5: Holster-Only)
// ================================

// User tracking stores
export const userPubKeys = writable<string[]>([]);

// V5: Contact management stores (Holster-only)
export const userContacts: Writable<ContactsCollectionData> = holsterContacts as Writable<ContactsCollectionData>;
export const isLoadingContacts = isLoadingHolsterContacts;
export const contactSearchQuery = writable('');

// Unified entity system - membership is just an attribute now
import {
	getEntityAttribute,
	setEntityAttribute,
	hasAttribute
} from './entities.svelte';

/**
 * Get organization membership (entity_id with 'membership' attribute)
 */
export function getOrgMembership(org_id: string): string[] | undefined {
	const membership = getEntityAttribute(org_id, 'membership');
	return Array.isArray(membership) ? membership : undefined;
}

/**
 * Check if an organization has a membership list
 */
export function hasOrgMembership(org_id: string): boolean {
	return hasAttribute(org_id, 'membership');
}

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
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: initializeContacts');
	initializeHolsterContacts();
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: initializeContacts');
}

/**
 * Cleanup contacts subscription
 * Call this when user logs out
 */
export function cleanupContacts() {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: cleanupContacts');
	cleanupHolsterContacts();
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: cleanupContacts');
}

/**
 * Persist contacts to backend
 * Called automatically via CRUD operations
 */
export async function persistContacts(contacts?: ContactsCollectionData) {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: persistContacts');
	const result = await persistHolsterContacts(contacts);
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: persistContacts');
	return result;
}

// ================================
// CONTACT MANAGEMENT FUNCTIONS
// ================================

/**
 * Create a new contact
 */
export async function createContact(
	contactData: Omit<Contact, 'contact_id' | 'created_at' | 'updated_at'>
): Promise<Contact> {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: createContact', { name: contactData.name });
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
	const currentContacts = get(userContacts) || {};
	const updatedContacts = {
		...currentContacts,
		[contact_id]: validatedContact
	};

	// V5: Update store and persist (Holster-only) - WAIT for persistence!
	await updateHolsterContactsStore(updatedContacts);

	// Force update the names cache immediately to ensure reactivity
	if (hasValidPublicKey) {
		userNamesCache.update((cache) => ({
			...cache,
			[contactData.public_key!]: contactData.name
		}));
	}

	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: createContact', { contact_id });
	return validatedContact;
}

/**
 * Update an existing contact
 */
export function updateContact(contact_id: string, updates: Partial<Contact>): void {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: updateContact', { contact_id });
	const currentContacts = get(userContacts);
	if (!currentContacts) {
		console.warn(`[USERS] Cannot update contact: contacts not loaded`);
		console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: updateContact (not loaded)');
		return;
	}

	const existingContact = currentContacts[contact_id];

	if (!existingContact) {
		//console.warn(`Contact with ID ${contact_id} not found`);
		console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: updateContact (not found)');
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
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: updateContact');
}

/**
 * Delete a contact (V5: Holster-only)
 */
export async function deleteContact(contact_id: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: deleteContact', { contact_id });
	// Use Holster-specific delete that sets to null
	await deleteHolsterContact(contact_id);
	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: deleteContact');
}

/**
 * Get contact by public key
 */
export function getContactByPublicKey(public_key: string): Contact | undefined {
	const contacts = get(userContacts);
	if (!contacts) {
		// console.log(`[GET-CONTACT-BY-PUBKEY] ❌ Contacts not loaded yet`);
		return undefined;
	}

	const contactsList = Object.values(contacts);
	// console.log(`[GET-CONTACT-BY-PUBKEY] Searching for pubkey ${public_key.slice(0, 20)}... among ${contactsList.length} contacts`);

	const found = contactsList.find((contact) => contact.public_key === public_key);
	if (found) {
		// console.log(`[GET-CONTACT-BY-PUBKEY] ✅ Found contact: ${found.name} (${found.contact_id})`);
	} else {
		// console.log(`[GET-CONTACT-BY-PUBKEY] ❌ No contact found with this pubkey`);
		// console.log(`[GET-CONTACT-BY-PUBKEY] Available pubkeys:`, contactsList.map(c => c.public_key?.slice(0, 20) + '...'));
	}

	return found;
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
	if (!contacts) return false;
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

	// Handle null/undefined contacts (e.g., not loaded yet)
	if (!contacts) {
		return undefined;
	}

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
	if (!contacts) return undefined;
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
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: getUserName');
	// Handle org_ids (works for ANY org in globalOrganizations)
	if (identifier.startsWith('org_')) {
		const orgs = get(globalOrganizations);
		const org = orgs[identifier];
		if (org) {
			console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: getUserName (org found)');
			return getOrganizationName(org, 'en'); // TODO: Use user's preferred language
		}
		console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: getUserName (org fallback)');
		return identifier; // Fallback if org not found
	}

	// First check the combined cache (contacts take priority over aliases)
	const combinedCache = get(userNamesOrAliasesCache);
	if (combinedCache[identifier]) {
		// console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: getUserName (cache hit)');
		return combinedCache[identifier];
	}

	let displayName: string;
	let shouldCacheInNamesCache = false;

	// Check if this is a contactId
	if (getIdentifierType(identifier) === 'contactId') {
		const contacts = get(userContacts);
		if (contacts) {
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
			// Contacts not loaded yet - return identifier as fallback
			displayName = identifier;
		}
	} else {
		// For pubKey-based lookup, check if we have a contact with this public key
		// console.log(`[GET-USER-NAME] Looking up pubkey: ${identifier.slice(0, 20)}...`);
		const contact = getContactByPublicKey(identifier);
		if (contact) {
			// console.log(`[GET-USER-NAME] ✅ Found contact for pubkey: ${contact.name}`);
			displayName = contact.name;
			shouldCacheInNamesCache = true; // Contact names go in userNamesCache
		} else {
			// console.log(`[GET-USER-NAME] ⚠️  No contact found for pubkey, falling back to alias`);
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

	// console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: getUserName');
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
	console.log('[TRACE] [ENTER] src/lib/network/users.svelte.ts: resolveContactIdsInTree');
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

	console.log('[TRACE] [EXIT] src/lib/network/users.svelte.ts: resolveContactIdsInTree');
	return resolvedNode;
}

// ================================
// ORGANIZATION MEMBERSHIP RESOLUTION
// ================================

/**
 * Resolve organization ID to array of member public keys (recursive!)
 * 
 * Handles:
 * - Direct members (pubkeys)
 * - Nested organizations (org_ids) - resolved recursively
 * - Circular references (prevented via visited set)
 * 
 * Resolution order:
 * 1. Check declared membership lists
 * 2. Check cached membership from subscriptions
 * 3. Return empty if not found
 * 
 * @param org_id - Organization identifier
 * @param visited - Set of already visited org_ids (prevents infinite loops)
 * @returns Array of resolved public keys (deduplicated)
 */
export function resolveOrganizationMembers(
	org_id: string,
	visited: Set<string> = new Set()
): string[] {
	// Prevent infinite loops (circular organization references)
	if (visited.has(org_id)) {
		console.warn(`[ORG-RESOLVE] Circular reference detected: ${org_id}`);
		return [];
	}
	visited.add(org_id);

	// Get membership list from attribute system
	const members = getOrgMembership(org_id);
	if (!members) {
		console.log(`[ORG-RESOLVE] No membership list found for ${org_id}`);
		return [];
	}

	const resolved: string[] = [];

	// Process each member
	for (const member of members) {
		if (member.startsWith('org_')) {
			// Nested organization - resolve recursively
			const nestedMembers = resolveOrganizationMembers(member, visited);
			resolved.push(...nestedMembers);
		} else {
			// Direct member (pubkey) - add as-is
			resolved.push(member);
		}
	}

	// Deduplicate
	return [...new Set(resolved)];
}

/**
 * Resolve contributor ID with organization support
 * 
 * Handles three types of identifiers:
 * 1. org_id (starting with "org_") → resolves to array of pubkeys
 * 2. contact_id (starting with "contact_") → resolves to single pubkey
 * 3. pubkey (raw) → returns as-is
 * 
 * @param contributorId - The identifier to resolve
 * @returns Array of public keys (may be empty if unresolvable)
 */
export function resolveContributorWithOrgs(contributorId: string): string[] {
	// Case 1: Organization - expand to all members
	if (contributorId.startsWith('org_')) {
		return resolveOrganizationMembers(contributorId);
	}

	// Case 2: Contact ID - resolve to pubkey
	if (contributorId.startsWith('contact_')) {
		const pubkey = getPublicKeyFromContactId(contributorId);
		return pubkey ? [pubkey] : [];
	}

	// Case 3: Already a pubkey - return as array
	return [contributorId];
}

/**
 * Resolve multiple contributor IDs (with org support)
 * 
 * Expands organizations and resolves contacts to public keys.
 * Returns deduplicated array of all resolved public keys.
 * 
 * @param contributorIds - Array of contributor identifiers
 * @returns Array of resolved public keys (deduplicated)
 */
export function resolveContributorsWithOrgs(contributorIds: string[]): string[] {
	const allResolved: string[] = [];

	for (const id of contributorIds) {
		const resolved = resolveContributorWithOrgs(id);
		allResolved.push(...resolved);
	}

	// Deduplicate
	return [...new Set(allResolved)];
}