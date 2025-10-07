/**
 * Notes Feature - Holster Implementation Test
 *
 * This is an isolated feature to validate the Holster migration patterns:
 * - CRUD operations with explicit timestamps
 * - Conflict resolution using _updatedAt
 * - Real-time sync across devices
 * - User authentication integration
 *
 * This serves as a reference implementation for migrating other modules.
 */

import { writable, derived, get } from 'svelte/store';
import { z } from 'zod/v4';
import { holster, user, userPub } from './holster.svelte';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';

// ============================================================================
// Schema
// ============================================================================

export const NoteSchema = z.object({
	id: z.string().min(1),
	title: z.string().min(1),
	content: z.string(),
	_updatedAt: z.number().positive().optional()
});

export const NotesCollectionSchema = z.record(z.string(), NoteSchema);

export type Note = z.infer<typeof NoteSchema>;
export type NotesCollection = z.infer<typeof NotesCollectionSchema>;

// ============================================================================
// State
// ============================================================================

// Local notes state
export const notes = writable<NotesCollection>({});

// Loading flag
export const isLoadingNotes = writable(false);

// Track last known network timestamps for each note
const lastNetworkTimestamps = new Map<string, number>();

// ============================================================================
// Subscription Management
// ============================================================================

let notesCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's notes from Holster
 */
function subscribeToNotes() {
	if (!user.is) {
		console.log('[NOTES] Cannot subscribe: no authenticated user');
		return;
	}

	console.log('[NOTES] Subscribing to notes for user:', user.is.username);

	// Create callback for updates
	notesCallback = (data: any) => {
		console.log('[NOTES] Received update:', data);

		// Skip if loading (initial data fetch)
		if (get(isLoadingNotes)) {
			return;
		}

		if (!data) {
			return;
		}

		// Parse and validate
		const parseResult = NotesCollectionSchema.safeParse(data);
		if (!parseResult.success) {
			console.error('[NOTES] Invalid notes data:', parseResult.error);
			return;
		}

		const networkNotes = parseResult.data;

		// Update each note with timestamp checking
		notes.update((currentNotes) => {
			const updatedNotes = { ...currentNotes };

			for (const [noteId, networkNote] of Object.entries(networkNotes)) {
				const networkTimestamp = getTimestamp(networkNote);
				const lastTimestamp = lastNetworkTimestamps.get(noteId);

				// Only update if newer or first time seeing this note
				if (!lastTimestamp || (networkTimestamp && networkTimestamp > lastTimestamp)) {
					updatedNotes[noteId] = networkNote;
					if (networkTimestamp) {
						lastNetworkTimestamps.set(noteId, networkTimestamp);
					}
					console.log('[NOTES] Updated note:', noteId);
				} else {
					console.log('[NOTES] Skipping stale update for note:', noteId);
				}
			}

			return updatedNotes;
		});
	};

	// Subscribe with on()
	user.get('notes', notesCallback);
}

/**
 * Initialize notes subscription when user logs in
 */
export function initializeNotes() {
	if (!user.is) {
		console.log('[NOTES] Cannot initialize: no authenticated user');
		return;
	}

	console.log('[NOTES] Initializing notes...');
	isLoadingNotes.set(true);

	// Load initial data with get()
	user.get('notes', (data: any) => {
		console.log('[NOTES] Initial load:', data);

		if (data) {
			const parseResult = NotesCollectionSchema.safeParse(data);
			if (parseResult.success) {
				notes.set(parseResult.data);

				// Track initial timestamps
				for (const [noteId, note] of Object.entries(parseResult.data)) {
					const timestamp = getTimestamp(note);
					if (timestamp) {
						lastNetworkTimestamps.set(noteId, timestamp);
					}
				}
			} else {
				console.error('[NOTES] Invalid initial data:', parseResult.error);
				notes.set({});
			}
		} else {
			notes.set({});
		}

		isLoadingNotes.set(false);

		// Subscribe to updates
		subscribeToNotes();
	});
}

/**
 * Cleanup subscription
 */
export function cleanupNotes() {
	if (notesCallback) {
		user.get('notes').off(notesCallback);
		notesCallback = null;
	}
	notes.set({});
	lastNetworkTimestamps.clear();
	console.log('[NOTES] Cleaned up');
}

// ============================================================================
// CRUD Operations
// ============================================================================

/**
 * Create a new note
 */
export async function createNote(title: string, content: string): Promise<string> {
	if (!user.is) {
		throw new Error('Not authenticated');
	}

	const noteId = `note_${Date.now()}_${Math.random().toString(36).substring(7)}`;
	const note: Note = addTimestamp({
		id: noteId,
		title,
		content
	});

	console.log('[NOTES] Creating note:', noteId);

	// Optimistically update local state
	notes.update((n) => ({ ...n, [noteId]: note }));

	// Persist to Holster
	await persistNote(note);

	return noteId;
}

/**
 * Update an existing note
 */
export async function updateNote(noteId: string, updates: Partial<Omit<Note, 'id' | '_updatedAt'>>): Promise<void> {
	if (!user.is) {
		throw new Error('Not authenticated');
	}

	const currentNotes = get(notes);
	const existingNote = currentNotes[noteId];

	if (!existingNote) {
		throw new Error(`Note ${noteId} not found`);
	}

	const updatedNote: Note = addTimestamp({
		...existingNote,
		...updates
	});

	console.log('[NOTES] Updating note:', noteId);

	// Optimistically update local state
	notes.update((n) => ({ ...n, [noteId]: updatedNote }));

	// Persist to Holster
	await persistNote(updatedNote);
}

/**
 * Delete a note
 */
export async function deleteNote(noteId: string): Promise<void> {
	if (!user.is) {
		throw new Error('Not authenticated');
	}

	console.log('[NOTES] Deleting note:', noteId);

	// Optimistically update local state
	notes.update((n) => {
		const updated = { ...n };
		delete updated[noteId];
		return updated;
	});

	// Remove from Holster
	return new Promise((resolve, reject) => {
		user.get('notes').next(noteId).put(null, (err: any) => {
			if (err) {
				console.error('[NOTES] Delete error:', err);
				reject(err);
			} else {
				console.log('[NOTES] Deleted from Holster:', noteId);
				lastNetworkTimestamps.delete(noteId);
				resolve();
			}
		});
	});
}

/**
 * Persist a single note to Holster with conflict detection
 */
async function persistNote(note: Note): Promise<void> {
	if (!user.is) {
		throw new Error('Not authenticated');
	}

	const localTimestamp = getTimestamp(note);
	const networkTimestamp = lastNetworkTimestamps.get(note.id) || null;

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, networkTimestamp)) {
		console.warn('[NOTES] Skipping persist - network has newer data:', note.id);
		return;
	}

	return new Promise((resolve, reject) => {
		user.get('notes').next(note.id).put(note, (err: any) => {
			if (err) {
				console.error('[NOTES] Persist error:', err);
				reject(err);
			} else {
				console.log('[NOTES] Persisted to Holster:', note.id);
				if (localTimestamp) {
					lastNetworkTimestamps.set(note.id, localTimestamp);
				}
				resolve();
			}
		});
	});
}

// ============================================================================
// Derived Stores
// ============================================================================

/**
 * Get notes as a sorted array
 */
export const notesArray = derived(notes, ($notes) => {
	return Object.values($notes).sort((a, b) => {
		const aTime = getTimestamp(a) || 0;
		const bTime = getTimestamp(b) || 0;
		return bTime - aTime; // Newest first
	});
});

/**
 * Get a single note by ID
 */
export function getNoteById(noteId: string) {
	return derived(notes, ($notes) => $notes[noteId] || null);
}
