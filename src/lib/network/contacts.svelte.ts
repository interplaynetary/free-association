/**
 * Contacts Module - Holster Implementation (V5 Pattern)
 *
 * Now using createStore() for consistency with protocol stores!
 * - Automatic timestamp tracking and conflict resolution
 * - Built-in persistence with debouncing
 * - Schema validation via Zod
 * - Simple initialize/cleanup lifecycle
 */

import { get, derived } from 'svelte/store';
import { createStore } from '$lib/utils/primitives/store.svelte';
// V5: Import from v5 schemas
import type { ContactsCollection, Contact } from '$lib/protocol/schemas';
import { ContactsCollectionSchema } from '$lib/protocol/schemas';

// ============================================================================
// Store (V5 Pattern - Elegant!)
// ============================================================================

/**
 * Contacts Store - Using createStore() pattern
 * 
 * ✅ Automatic persistence
 * ✅ Conflict resolution
 * ✅ Timestamp tracking
 * ✅ Schema validation
 */
export const contactsStore = createStore({
	holsterPath: 'contacts',
	schema: ContactsCollectionSchema,
	persistDebounce: 100 // Debounce rapid contact updates
});

// Export as holsterContacts for backwards compatibility
export const holsterContacts = contactsStore;

// Loading flag (backwards compatibility) - derived from store state
export const isLoadingHolsterContacts = derived(
	holsterContacts,
	($contacts) => $contacts === null
);

// ============================================================================
// Initialization & Cleanup (V5 Pattern)
// ============================================================================

/**
 * Initialize contacts when user logs in
 * Just calls store.initialize() - that's it!
 */
export function initializeHolsterContacts() {
	console.log('[CONTACTS-V5] Initializing...');
	contactsStore.initialize();
}

/**
 * Cleanup on logout
 * Just calls store.cleanup() - that's it!
 */
export async function cleanupHolsterContacts() {
	console.log('[CONTACTS-V5] Cleaning up...');
	await contactsStore.cleanup();
}

/**
 * Reset initialization (backwards compatibility)
 */
export function resetInitialization() {
	console.log('[CONTACTS-V5] Resetting...');
	cleanupHolsterContacts();
}

// ============================================================================
// Persistence (V5 Pattern - Simplified!)
// ============================================================================

/**
 * Persist contacts - now just a wrapper around store.set()
 * The store handles everything: timestamps, conflict resolution, debouncing!
 */
export async function persistHolsterContacts(
	contacts?: ContactsCollection
): Promise<void> {
	const contactsToSave = contacts || get(holsterContacts);
	
	if (!contactsToSave || Object.keys(contactsToSave).length === 0) {
		console.log('[CONTACTS-V5] No contacts to persist');
		return;
	}
	
	// Just set the data - store handles everything!
	contactsStore.set(contactsToSave);
	
	// Wait for persistence to complete
	await contactsStore.waitForPersistence();
}

// ============================================================================
// CRUD Operations (V5 Pattern - Simplified!)
// ============================================================================

/**
 * Delete a contact
 * Just remove from collection and store.set() - automatic persistence!
 */
export async function deleteHolsterContact(contactId: string): Promise<void> {
	console.log('[CONTACTS-V5] Deleting contact:', contactId);
	
	const currentContacts = get(holsterContacts);
	if (!currentContacts) return;
	
	// Remove contact from collection
	const { [contactId]: deleted, ...remaining } = currentContacts;
	
	// Set the updated collection - store handles persistence!
	contactsStore.set(remaining);
	
	// Wait for persistence
	await contactsStore.waitForPersistence();
}

/**
 * Update the contacts store and persist
 * Now even simpler - just store.set()!
 */
export async function updateHolsterContactsStore(
	updatedContacts: ContactsCollection
): Promise<void> {
	contactsStore.set(updatedContacts);
	await contactsStore.waitForPersistence();
}
