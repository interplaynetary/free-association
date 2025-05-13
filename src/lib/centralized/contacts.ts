import { gun, user } from '$lib/gun/gunSetup';

// Define the contact interface
export interface Contact {
	id: string;
	name: string;
	email?: string;
	phone?: string;
	notes?: string;
	createdAt?: number;
	updatedAt?: number;
}

// Cache contacts to avoid repeated Gun operations
let contactsCache = new Map<string, Contact>();
let lastCacheUpdate = 0;
const CACHE_TTL_MS = 30000; // 30-second TTL for contacts cache

// Function to add a contact
export async function addContact(contact: Omit<Contact, 'id' | 'createdAt'>): Promise<Contact> {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to add contacts');
	}
	
	// Generate a unique ID
	const contactId = `contact_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`;
	
	// Prepare contact data
	const newContact: Contact = {
		id: contactId,
		name: contact.name,
		email: contact.email,
		phone: contact.phone,
		notes: contact.notes,
		createdAt: Date.now()
	};
	
	// Save to Gun
	await new Promise<void>((resolve, reject) => {
		const contactRef = gun.user().get('contacts').get(contactId);
		contactRef.put({
			name: newContact.name,
			email: newContact.email,
			phone: newContact.phone,
			notes: newContact.notes,
			createdAt: newContact.createdAt
		}, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				// Add to local cache
				contactsCache.set(contactId, newContact);
				resolve();
			}
		});
	});
	
	return newContact;
}

// Function to update a contact
export async function updateContact(contact: Contact): Promise<Contact> {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to update contacts');
	}
	
	if (!contact.id) {
		throw new Error('Contact ID is required');
	}
	
	// Prepare updated contact data
	const updatedContact: Contact = {
		...contact,
		updatedAt: Date.now()
	};
	
	// Save to Gun
	await new Promise<void>((resolve, reject) => {
		const contactRef = gun.user().get('contacts').get(contact.id);
		contactRef.put({
			name: updatedContact.name,
			email: updatedContact.email,
			phone: updatedContact.phone,
			notes: updatedContact.notes,
			updatedAt: updatedContact.updatedAt
		}, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				// Update local cache
				contactsCache.set(contact.id, updatedContact);
				resolve();
			}
		});
	});
	
	return updatedContact;
}

// Function to delete a contact
export async function deleteContact(contactId: string): Promise<void> {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to delete contacts');
	}
	
	if (!contactId) {
		throw new Error('Contact ID is required');
	}
	
	// Delete from Gun
	await new Promise<void>((resolve, reject) => {
		const contactRef = gun.user().get('contacts').get(contactId);
		contactRef.put(null, (ack: any) => {
			if (ack.err) {
				reject(new Error(ack.err));
			} else {
				// Remove from local cache
				contactsCache.delete(contactId);
				resolve();
			}
		});
	});
}

// Function to get a single contact
export async function getContact(contactId: string): Promise<Contact | null> {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to get contacts');
	}
	
	if (!contactId) {
		throw new Error('Contact ID is required');
	}
	
	// Check cache first
	if (contactsCache.has(contactId)) {
		return contactsCache.get(contactId) || null;
	}
	
	// Get from Gun
	return new Promise<Contact | null>((resolve) => {
		const contactRef = gun.user().get('contacts').get(contactId);
		contactRef.once((data: any) => {
			if (!data) {
				resolve(null);
				return;
			}
			
			// Create contact object
			const contact: Contact = {
				id: contactId,
				name: data.name || 'Unnamed Contact',
				email: data.email,
				phone: data.phone,
				notes: data.notes,
				createdAt: data.createdAt,
				updatedAt: data.updatedAt
			};
			
			// Add to cache
			contactsCache.set(contactId, contact);
			resolve(contact);
		});
	});
}

// Function to get all contacts
export async function getAllContacts(): Promise<Contact[]> {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to get contacts');
	}
	
	// Check if cache is still valid
	const now = Date.now();
	if (contactsCache.size > 0 && (now - lastCacheUpdate < CACHE_TTL_MS)) {
		return Array.from(contactsCache.values());
	}
	
	// Clear cache if it's outdated
	contactsCache.clear();
	
	// Get from Gun
	return new Promise<Contact[]>((resolve) => {
		const contactsList: Contact[] = [];
		const contactsRef = gun.user().get('contacts');
		
		// Set up listener to collect all contacts
		const listener = contactsRef.map().once((data: any, key: string) => {
			if (!data || key === '_') return;
			
			// Create contact object
			const contact: Contact = {
				id: key,
				name: data.name || 'Unnamed Contact',
				email: data.email,
				phone: data.phone,
				notes: data.notes,
				createdAt: data.createdAt,
				updatedAt: data.updatedAt
			};
			
			// Add to array and cache
			contactsList.push(contact);
			contactsCache.set(key, contact);
		});
		
		// Use a timeout to handle completion of data loading
		setTimeout(() => {
			// Clean up listener
			if (listener && listener.off) {
				listener.off();
			}
			
			// Update cache timestamp
			lastCacheUpdate = now;
			
			// Return results
			resolve(contactsList);
		}, 500); // Adjust timeout as needed for your data size
	});
}

// Function to set up real-time contact updates
export function subscribeToContacts(
	onUpdate: (contacts: Contact[]) => void
): () => void {
	if (!user.is?.pub) {
		throw new Error('User must be authenticated to subscribe to contacts');
	}
	
	// Set up contacts array and cache
	const contactsList: Contact[] = [];
	contactsCache.clear();
	
	// Create listener
	const contactsRef = gun.user().get('contacts');
	const listener = contactsRef.map().on((data: any, key: string) => {
		if (key === '_') return;
		
		// If data is null, the contact was deleted
		if (data === null) {
			// Remove from array and cache
			const index = contactsList.findIndex(c => c.id === key);
			if (index !== -1) {
				contactsList.splice(index, 1);
			}
			contactsCache.delete(key);
		} else {
			// Create or update contact
			const contact: Contact = {
				id: key,
				name: data.name || 'Unnamed Contact',
				email: data.email,
				phone: data.phone,
				notes: data.notes,
				createdAt: data.createdAt,
				updatedAt: data.updatedAt
			};
			
			// Check if contact exists in array
			const index = contactsList.findIndex(c => c.id === key);
			if (index === -1) {
				// Add new contact
				contactsList.push(contact);
			} else {
				// Update existing contact
				contactsList[index] = contact;
			}
			
			// Update cache
			contactsCache.set(key, contact);
		}
		
		// Update cache timestamp
		lastCacheUpdate = Date.now();
		
		// Call callback with updated list
		onUpdate([...contactsList]);
	});
	
	// Return cleanup function
	return () => {
		if (listener && listener.off) {
			listener.off();
		}
	};
}

// Export a typed-safe reference to the contacts node
export const contactsRef = {
	path: ['contacts'],
	node: () => user.get('contacts')
}; 