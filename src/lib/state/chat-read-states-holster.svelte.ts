/**
 * Chat Read States Module - Holster Implementation
 *
 * Manages chat read states (tracking which messages have been read in each chat)
 * using Holster with timestamp-based conflict resolution.
 *
 * Pattern: Same as contacts-holster - key-value collection with timestamps
 */

import { writable, get } from 'svelte/store';
import { holsterUser } from './holster.svelte';
import type { ChatReadStates } from '$lib/schema';
import { parseChatReadStates } from '$lib/validation';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/holsterTimestamp';

// ============================================================================
// State
// ============================================================================

// Local state for chat read states
export const holsterChatReadStates = writable<ChatReadStates>({});

// Loading flag
export const isLoadingHolsterChatReadStates = writable(false);

// Track last known network timestamp
let lastNetworkTimestamp: number | null = null;

// Prevent duplicate initialization
let isInitialized: boolean = false;

// ============================================================================
// Subscription Management
// ============================================================================

let chatReadStatesCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's chat read states
 */
function subscribeToChatReadStates() {
	if (!holsterUser.is) {
		console.log('[CHAT-READ-STATES-HOLSTER] Cannot subscribe: no authenticated user');
		return;
	}


	chatReadStatesCallback = (data: any) => {

		// Skip if loading (initial data fetch)
		if (get(isLoadingHolsterChatReadStates)) {
			return;
		}

		if (!data) {
			return;
		}

		// Extract timestamp and filter out metadata fields
		const networkTimestamp = getTimestamp(data);
		const { _updatedAt, ...dataOnly } = data;

		// Parse and validate
		const parsed = parseChatReadStates(dataOnly);
		if (!parsed) {
			console.error('[CHAT-READ-STATES-HOLSTER] Invalid chat read states data');
			return;
		}

		const networkReadStates = parsed;

		// Only update if newer or first time
		if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
			holsterChatReadStates.set(networkReadStates);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
		} else {
		}
	};

	holsterUser.get('chatReadStates').on(chatReadStatesCallback);
}

/**
 * Initialize chat read states subscription
 */
export function initializeHolsterChatReadStates() {
	if (!holsterUser.is) {
		console.log('[CHAT-READ-STATES-HOLSTER] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitialized) {
		console.log('[CHAT-READ-STATES-HOLSTER] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[CHAT-READ-STATES-HOLSTER] Initializing...');
	isInitialized = true;
	isLoadingHolsterChatReadStates.set(true);

	// Load initial data
	holsterUser.get('chatReadStates', (data: any) => {

		if (data) {
			// Extract timestamp and filter out metadata
			const timestamp = getTimestamp(data);
			const { _updatedAt, ...dataOnly } = data;

			// Parse and validate
			const parsed = parseChatReadStates(dataOnly);
			if (parsed) {
				holsterChatReadStates.set(parsed);
				if (timestamp) {
					lastNetworkTimestamp = timestamp;
				}
			} else {
				console.error('[CHAT-READ-STATES-HOLSTER] Invalid initial data');
				holsterChatReadStates.set({});
			}
		} else {
			holsterChatReadStates.set({});
		}

		isLoadingHolsterChatReadStates.set(false);

		// Subscribe to updates
		subscribeToChatReadStates();
	});
}

/**
 * Cleanup chat read states subscription
 */
export function cleanupHolsterChatReadStates() {
	if (chatReadStatesCallback && holsterUser.is) {
		holsterUser.get('chatReadStates').off(chatReadStatesCallback);
		chatReadStatesCallback = null;
	}
	holsterChatReadStates.set({});
	lastNetworkTimestamp = null;
	isInitialized = false;
	console.log('[CHAT-READ-STATES-HOLSTER] Cleaned up');
}

/**
 * Persist chat read states to Holster
 */
export async function persistHolsterChatReadStates(readStates?: ChatReadStates): Promise<void> {
	if (!holsterUser.is) {
		console.log('[CHAT-READ-STATES-HOLSTER] Not authenticated, skipping persistence');
		return;
	}

	const readStatesToSave = readStates || get(holsterChatReadStates);

	if (!readStatesToSave || Object.keys(readStatesToSave).length === 0) {
		console.log('[CHAT-READ-STATES-HOLSTER] No chat read states to persist');
		return;
	}

	// Add timestamp
	const timestampedData = addTimestamp(readStatesToSave);
	const localTimestamp = getTimestamp(timestampedData);

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
		console.warn('[CHAT-READ-STATES-HOLSTER] Skipping persist - network has newer data');
		return;
	}

	console.log('[CHAT-READ-STATES-HOLSTER] Persisting chat read states...');

	return new Promise((resolve, reject) => {
		holsterUser.get('chatReadStates').put(timestampedData, (err: any) => {
			if (err) {
				console.error('[CHAT-READ-STATES-HOLSTER] Persist error:', err);
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

/**
 * Reset initialization state (for logout/re-login in same session)
 */
export function resetInitialization() {
	console.log('[CHAT-READ-STATES-HOLSTER] Resetting initialization state');
	cleanupHolsterChatReadStates();
}
