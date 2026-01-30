/**
 * Chat Read States Module - Mesh Implementation
 *
 * Manages chat read states (tracking which messages have been read in each chat)
 * using Mesh with timestamp-based conflict resolution.
 *
 * Pattern: Same as contacts-mesh - key-value collection with timestamps
 */

import { writable, get } from 'svelte/store';
import { meshUser } from '$lib/network/mesh.svelte';
// V5: Use Zod schemas for validation
import type { ChatReadStates } from '@playnet/free-association/schemas';
import { ChatReadStatesSchema } from '@playnet/free-association/schemas';
import { addTimestamp, getTimestamp, shouldPersist } from '$lib/utils/data/meshTimestamp';

// ============================================================================
// State
// ============================================================================

// Local state for chat read states
export const meshChatReadStates = writable<ChatReadStates>({});

// Loading flag
export const isLoadingMeshChatReadStates = writable(false);

// Track last known network timestamp
let lastNetworkTimestamp: number | null = null;

// Prevent duplicate initialization
let isInitialized: boolean = false;

// Track first real data received
let hasReceivedRealData = false;

// ============================================================================
// Subscription Management
// ============================================================================

let chatReadStatesCallback: ((data: any) => void) | null = null;

/**
 * Subscribe to user's chat read states
 */
function subscribeToChatReadStates() {
	if (!meshUser.is) {
		console.log('[CHAT-READ-STATES-MESH] Cannot subscribe: no authenticated user');
		return;
	}

	chatReadStatesCallback = (data: any) => {
		if (!data) {
			if (!hasReceivedRealData) {
				console.log('[CHAT-READ-STATES-MESH] Subscription returned null, waiting for network data...');
			}
			return;
		}

		if (!hasReceivedRealData) {
			console.log('[CHAT-READ-STATES-MESH] First real data received from network');
			hasReceivedRealData = true;
		}

		// Extract timestamp and filter out metadata fields
		const networkTimestamp = getTimestamp(data);
		const { _updatedAt, ...dataOnly } = data;

		// V5: Parse and validate with Zod
		const parseResult = ChatReadStatesSchema.safeParse(dataOnly);
		if (!parseResult.success) {
			console.error('[CHAT-READ-STATES-MESH] Invalid chat read states data:', parseResult.error);
			return;
		}

		const networkReadStates = parseResult.data;

		// Only update if newer or first time
		if (!lastNetworkTimestamp || (networkTimestamp && networkTimestamp > lastNetworkTimestamp)) {
			meshChatReadStates.set(networkReadStates);
			if (networkTimestamp) {
				lastNetworkTimestamp = networkTimestamp;
			}
			isLoadingMeshChatReadStates.set(false);
		}
	};

	meshUser.get('chatReadStates').on(chatReadStatesCallback, true);
}

/**
 * Initialize chat read states subscription
 */
export function initializeMeshChatReadStates() {
	if (!meshUser.is) {
		console.log('[CHAT-READ-STATES-MESH] Cannot initialize: no authenticated user');
		return;
	}

	if (isInitialized) {
		console.log('[CHAT-READ-STATES-MESH] Already initialized, skipping duplicate call');
		return;
	}

	console.log('[CHAT-READ-STATES-MESH] Initializing...');
	isInitialized = true;
	isLoadingMeshChatReadStates.set(true);

	subscribeToChatReadStates();
}

/**
 * Cleanup chat read states subscription
 */
export function cleanupMeshChatReadStates() {
	if (chatReadStatesCallback && meshUser.is) {
		meshUser.get('chatReadStates').off(chatReadStatesCallback);
		chatReadStatesCallback = null;
	}
	meshChatReadStates.set({});
	lastNetworkTimestamp = null;
	isInitialized = false;
	hasReceivedRealData = false;
	console.log('[CHAT-READ-STATES-MESH] Cleaned up');
}

/**
 * Persist chat read states to Mesh
 */
export async function persistMeshChatReadStates(readStates?: ChatReadStates): Promise<void> {
	if (!meshUser.is) {
		console.log('[CHAT-READ-STATES-MESH] Not authenticated, skipping persistence');
		return;
	}

	const readStatesToSave = readStates || get(meshChatReadStates);

	if (!readStatesToSave || Object.keys(readStatesToSave).length === 0) {
		console.log('[CHAT-READ-STATES-MESH] No chat read states to persist');
		return;
	}

	// Add timestamp
	const timestampedData = addTimestamp(readStatesToSave);
	const localTimestamp = getTimestamp(timestampedData);

	// Check if safe to persist
	if (!shouldPersist(localTimestamp, lastNetworkTimestamp)) {
		console.warn('[CHAT-READ-STATES-MESH] Skipping persist - network has newer data');
		return;
	}

	console.log('[CHAT-READ-STATES-MESH] Persisting chat read states...');

	return new Promise((resolve, reject) => {
		meshUser.get('chatReadStates').put(timestampedData, (err: any) => {
			if (err) {
				console.error('[CHAT-READ-STATES-MESH] Persist error:', err);
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
	console.log('[CHAT-READ-STATES-MESH] Resetting initialization state');
	cleanupMeshChatReadStates();
}
