import { get, writable, derived } from 'svelte/store';
import type { Writable } from 'svelte/store';
import { gun } from '$lib/state/gun.svelte';
import type { Contact, ContactsCollection } from '$lib/schema';
import { ContactSchema } from '$lib/schema';

// ================================
// CORE USER & CONTACT STORES
// ================================

// User tracking stores
export const userPubKeys = writable<string[]>([]);

// Contact management stores
export const userContacts: Writable<ContactsCollection> = writable({});
export const isLoadingContacts = writable(false);
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

	const now = new Date().toISOString();
	const contact_id = `contact_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

	const newContact: Contact = {
		contact_id,
		name: contactData.name,
		public_key: hasValidPublicKey ? contactData.public_key : undefined,
		wallet_address: contactData.wallet_address,
		created_at: now,
		updated_at: now
	};

	// Validate the contact data
	const validatedContact = ContactSchema.parse(newContact);

	// Add to contacts collection
	userContacts.update((contacts) => ({
		...contacts,
		[contact_id]: validatedContact
	}));

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
	userContacts.update((contacts) => {
		const existingContact = contacts[contact_id];
		if (!existingContact) {
			console.warn(`Contact with ID ${contact_id} not found`);
			return contacts;
		}

		const updatedContact = {
			...existingContact,
			...updates,
			updated_at: new Date().toISOString()
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

		return {
			...contacts,
			[contact_id]: validatedContact
		};
	});
}

/**
 * Delete a contact
 */
export function deleteContact(contact_id: string): void {
	userContacts.update((contacts) => {
		const { [contact_id]: deleted, ...remaining } = contacts;
		return remaining;
	});
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

/**
 * Get contact by wallet address
 */
export function getContactByWalletAddress(wallet_address: string): Contact | undefined {
	const contacts = get(userContacts);
	return Object.values(contacts).find((contact) => contact.wallet_address === wallet_address);
}

/**
 * Check if a wallet address is already in use
 */
export function isWalletAddressInUse(wallet_address: string, excludeContactId?: string): boolean {
	// Only check for duplicates if we have a meaningful wallet address
	if (!wallet_address || wallet_address.trim() === '') {
		return false;
	}

	const contacts = get(userContacts);
	return Object.values(contacts).some(
		(contact) => contact.wallet_address === wallet_address && contact.contact_id !== excludeContactId
	);
}

/**
 * Save user's own wallet address
 */
export async function saveUserWalletAddress(userPub: string, walletAddress: string): Promise<void> {
	return new Promise((resolve, reject) => {
		if (!userPub || !walletAddress) {
			reject(new Error('User public key and wallet address are required'));
			return;
		}

		// Save to Gun under user's space
		gun
			.user(userPub)
			.get('profile')
			.get('wallet_address')
			.put(walletAddress, (ack) => {
				if (ack.err) {
					reject(new Error('Failed to save wallet address'));
				} else {
					resolve();
				}
			});
	});
}

/**
 * Get user's own wallet address
 */
export async function getUserWalletAddress(userPub: string): Promise<string | null> {
	return new Promise((resolve) => {
		if (!userPub) {
			resolve(null);
			return;
		}

		gun
			.user(userPub)
			.get('profile')
			.get('wallet_address')
			.once((data) => {
				resolve(data || null);
			});
	});
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
		console.warn(`[USERS] Contact not found for ID: ${contactId}`);
		return undefined;
	}

	if (!contact.public_key) {
		console.warn(`[USERS] Contact ${contactId} (${contact.name}) has no public key`);
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
 * Get Gun alias for a user by public key
 */
export async function getUserAlias(pubkey: string) {
	// First check the reactive cache
	const cache = get(userAliasesCache);
	if (cache[pubkey]) {
		return cache[pubkey];
	}

	// If not found, try the user's protected space using Gun's user system
	try {
		const alias = await gun.user(pubkey).get('alias');
		if (alias && typeof alias === 'string') {
			// Update cache
			userAliasesCache.update((cache) => ({
				...cache,
				[pubkey]: alias
			}));
			return alias;
		}
	} catch (error) {
		console.log(`[USER-NAME] Could not fetch alias for user ${pubkey}:`, error);
	}

	// Fallback to truncated ID
	const fallbackName = pubkey.substring(0, 8) + '...';
	return fallbackName;
}

/**
 * Get display name for a user by either pubKey or contactId
 * Prioritizes contact names over Gun aliases
 * Always caches results for reactive components
 */
export async function getUserName(identifier: string): Promise<string> {
	// First check if we already have this cached
	const cache = get(userNamesCache);
	if (cache[identifier]) {
		return cache[identifier];
	}

	let displayName: string;

	// Check if this is a contactId
	if (getIdentifierType(identifier) === 'contactId') {
		const contacts = get(userContacts);
		const contact = contacts[identifier];
		if (contact) {
			displayName = contact.name;
		} else {
		// If contactId not found, return the identifier itself as fallback
			displayName = identifier;
	}
	} else {
		// For pubKey-based lookup, check if we have a contact with this public key
	const contact = getContactByPublicKey(identifier);
	if (contact) {
			displayName = contact.name;
		} else {
	// If no contact found, fall back to Gun alias
			displayName = await getUserAlias(identifier);
		}
	}

	// Always cache the result for reactive components
	userNamesCache.update((cache) => ({
		...cache,
		[identifier]: displayName
	}));

	return displayName;
}
