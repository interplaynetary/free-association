/**
 * Contacts Module - Mesh Implementation (V5 Pattern)
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
import type { ContactsCollectionData as ContactsCollection, Contact } from '@playnet/free-association/schemas';
import { ContactsCollectionSchema } from '@playnet/free-association/schemas';

console.log('[TRACE] src/lib/network/contacts.svelte.ts: <module scope>');

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
	meshPath: 'contacts',
	schema: ContactsCollectionSchema,
	persistDebounce: 100 // Debounce rapid contact updates
});

// Export as meshContacts for backwards compatibility
export const meshContacts = contactsStore;

// Loading flag (backwards compatibility) - derived from store state
export const isLoadingMeshContacts = derived(
	meshContacts,
	($contacts) => $contacts === null
);

// ============================================================================
// Initialization & Cleanup (V5 Pattern)
// ============================================================================

/**
 * Initialize contacts when user logs in
 * Just calls store.initialize() - that's it!
 */
/**
 * Initialize contacts when user logs in
 * Just calls store.initialize() - that's it!
 */
export function initializeMeshContacts() {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: initializeMeshContacts');
	console.log('[CONTACTS-V5] Initializing...');
	contactsStore.initialize();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: initializeMeshContacts');
}

/**
 * Cleanup on logout
 * Just calls store.cleanup() - that's it!
 */
export async function cleanupMeshContacts() {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: cleanupMeshContacts');
	console.log('[CONTACTS-V5] Cleaning up...');
	await contactsStore.cleanup();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: cleanupMeshContacts');
}

/**
 * Reset initialization (backwards compatibility)
 */
export function resetInitialization() {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: resetInitialization');
	console.log('[CONTACTS-V5] Resetting...');
	cleanupMeshContacts();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: resetInitialization');
}

// ============================================================================
// Persistence (V5 Pattern - Simplified!)
// ============================================================================

/**
 * Persist contacts - now just a wrapper around store.set()
 * The store handles everything: timestamps, conflict resolution, debouncing!
 */
export async function persistMeshContacts(
	contacts?: ContactsCollection
): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: persistMeshContacts');
	const contactsToSave = contacts || get(meshContacts);

	if (!contactsToSave || Object.keys(contactsToSave).length === 0) {
		console.log('[CONTACTS-V5] No contacts to persist');
		console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: persistMeshContacts (empty)');
		return;
	}

	// Just set the data - store handles everything!
	contactsStore.set(contactsToSave);

	// Wait for persistence to complete
	await contactsStore.waitForPersistence();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: persistMeshContacts');
}

// ============================================================================
// CRUD Operations (V5 Pattern - Simplified!)
// ============================================================================

/**
 * Delete a contact
 * Just remove from collection and store.set() - automatic persistence!
 */
export async function deleteMeshContact(contactId: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: deleteMeshContact', { contactId });
	console.log('[CONTACTS-V5] Deleting contact:', contactId);

	const currentContacts = get(meshContacts);
	if (!currentContacts) {
		console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: deleteMeshContact (no contacts)');
		return;
	}

	// Remove contact from collection
	const { [contactId]: deleted, ...remaining } = currentContacts;

	// Set the updated collection - store handles persistence!
	contactsStore.set(remaining);

	// Wait for persistence
	await contactsStore.waitForPersistence();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: deleteMeshContact');
}

/**
 * Update the contacts store and persist
 * Now even simpler - just store.set()!
 */
export async function updateMeshContactsStore(
	updatedContacts: ContactsCollection
): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/contacts.svelte.ts: updateMeshContactsStore');
	contactsStore.set(updatedContacts);
	await contactsStore.waitForPersistence();
	console.log('[TRACE] [EXIT] src/lib/network/contacts.svelte.ts: updateMeshContactsStore');
}
