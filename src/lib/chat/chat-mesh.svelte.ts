import { writable, get, type Writable } from 'svelte/store';
import { mesh, meshUser } from '$lib/network/mesh.svelte';

export type MessageStatus = 'pending' | 'sent' | 'failed';

export interface Message {
	who: string;
	what: string;
	when: number;
	whopub?: string;
	// Optimistic UI fields (only present for pending messages)
	messageId?: string;
	status?: MessageStatus;
}

interface ChatSubscription {
	chatId: string;
	store: Writable<Message[]>;
	unsubscribe?: () => void;
	lastSeenTimestamp: number;
	isInitialLoad: boolean;
}

// Global chat subscriptions
const meshChatSubscriptions = new Map<string, ChatSubscription>();

/**
 * Process messages from Mesh data
 * Mesh returns complete object with all messages at once (no streaming)
 */
function processMessages(chatData: any, lastSeenTimestamp: number): Message[] {
	if (!chatData || typeof chatData !== 'object') {
		return [];
	}

	const messages: Message[] = [];

	// Iterate over all message IDs in the chat
	for (const [messageId, msgData] of Object.entries(chatData)) {
		// Skip metadata fields
		if (messageId.startsWith('_')) continue;

		const msg = msgData as any;

		// Skip if no timestamp (invalid message)
		if (!msg || typeof msg.when !== 'number') {
			console.warn('[CHAT-MESH] Message missing timestamp:', messageId);
			continue;
		}

		// Delta detection: only process messages newer than last seen
		if (msg.when <= lastSeenTimestamp) {
			continue;
		}

		// Plain text messages (no encryption)
		messages.push({
			who: msg.who || 'Unknown',
			whopub: msg.whopub || '',
			what: msg.what || '',
			when: msg.when
		});
	}

	// Sort by timestamp (oldest first)
	return messages.sort((a, b) => a.when - b.when);
}

/**
 * Subscribe to a Mesh chat and start listening for messages
 */
export function subscribeToMeshChat(chatId: string): ChatSubscription {
	// Return existing subscription if it exists
	if (meshChatSubscriptions.has(chatId)) {
		return meshChatSubscriptions.get(chatId)!;
	}

	console.log(`[CHAT-MESH] Creating chat subscription for: ${chatId}`);

	const subscription: ChatSubscription = {
		chatId,
		store: writable<Message[]>([]),
		lastSeenTimestamp: 0,
		isInitialLoad: true,
		unsubscribe: undefined
	};

	// Define callback for subscription
	const onChatUpdate = (chatData: any) => {

		// On initial load, load ALL messages (ignore lastSeenTimestamp)
		// On subsequent updates, only process new messages (delta detection)
		const timestampForFilter = subscription.isInitialLoad ? 0 : subscription.lastSeenTimestamp;
		const newMessages = processMessages(chatData, timestampForFilter);

		console.log(`[CHAT-MESH] ${subscription.isInitialLoad ? 'Initial load' : 'Delta detection'}: ${newMessages.length} messages (lastSeenTimestamp: ${subscription.lastSeenTimestamp})`);

		if (newMessages.length > 0) {
			console.log(`[CHAT-MESH] Processed ${newMessages.length} messages`);

			// Get current messages
			const currentMessages = get(subscription.store);

			// Append new messages (avoid duplicates by timestamp)
			const existingWhenSet = new Set(currentMessages.map(m => m.when));
			const trulyNewMessages = newMessages.filter(m => !existingWhenSet.has(m.when));
			const allMessages = [...currentMessages, ...trulyNewMessages].sort((a, b) => a.when - b.when);

			// Update last seen timestamp
			const latestTimestamp = Math.max(...newMessages.map((m) => m.when));
			subscription.lastSeenTimestamp = Math.max(subscription.lastSeenTimestamp, latestTimestamp);

			// Mark initial load as complete
			subscription.isInitialLoad = false;

			// Update store
			subscription.store.set(allMessages);
		} else if (subscription.isInitialLoad) {
			// Initial load with no messages - still mark as complete
			subscription.isInitialLoad = false;
		}
	};

	// Subscribe to chat updates from Mesh
	// Note: Mesh returns complete object (all messages at once), not streaming
	mesh.get(chatId).on(onChatUpdate, true);

	// Store unsubscribe function
	subscription.unsubscribe = () => mesh.get(chatId).off(onChatUpdate);
	meshChatSubscriptions.set(chatId, subscription);

	console.log('[CHAT-MESH] Subscribed to chat:', chatId);
	return subscription;
}

/**
 * Unsubscribe from a Mesh chat
 */
export function unsubscribeFromMeshChat(chatId: string): void {
	const subscription = meshChatSubscriptions.get(chatId);
	if (subscription) {
		subscription.unsubscribe?.();
		meshChatSubscriptions.delete(chatId);
		console.log(`[CHAT-MESH] Unsubscribed from chat: ${chatId}`);
	}
}

/**
 * Get messages store for a specific chat (subscribes if not already subscribed)
 */
export function getMeshChatMessages(chatId: string): Writable<Message[]> {
	const subscription = meshChatSubscriptions.get(chatId) || subscribeToMeshChat(chatId);
	return subscription.store;
}

/**
 * Send a message to a Mesh chat with optimistic UI updates
 * NOTE: Messages are sent in PLAIN TEXT (no encryption)
 * This is honest about the security model - capacity chats are public by design
 */
export async function sendMeshMessage(chatId: string, messageText: string): Promise<void> {
	if (!messageText.trim()) {
		throw new Error('Message cannot be empty');
	}

	if (!meshUser.is || !meshUser.is.pub) {
		throw new Error('You must be logged in to send messages');
	}

	// Get or create subscription
	const subscription = meshChatSubscriptions.get(chatId) || subscribeToMeshChat(chatId);

	// Create message ID and timestamp
	const messageId = Date.now().toString();
	const timestamp = Date.now();

	const message: Message = {
		who: meshUser.is.username || 'Unknown',
		whopub: meshUser.is.pub,
		what: messageText.trim(),
		when: timestamp,
		messageId: messageId,
		status: 'pending' as MessageStatus
	};

	try {
		console.log(`[CHAT-MESH] Sending message to ${chatId}:`, messageText);

		// OPTIMISTIC UPDATE: Add message to store immediately with 'pending' status
		const currentMessages = get(subscription.store);
		subscription.store.set([...currentMessages, message]);

		// Update lastSeenTimestamp BEFORE sending to prevent network duplicate
		subscription.lastSeenTimestamp = Math.max(
			subscription.lastSeenTimestamp,
			timestamp
		);

		// Store message directly in chat node using Mesh's chaining API
		// The callback handles status updates
		mesh.get(chatId).next(messageId).put(
			{
				who: message.who,
				whopub: message.whopub,
				what: message.what,
				when: message.when
			},
			(err: any) => {
				// Get current messages again (may have changed)
				const messagesNow = get(subscription.store);

				// Find our pending message by messageId (could have any status now)
				const messageIndex = messagesNow.findIndex(
					(m) => m.messageId === messageId
				);

				if (messageIndex === -1) {
					// Message not found - network update may have already replaced it
					// This is OK - the message is displayed correctly
					console.log('[CHAT-MESH] Pending message already replaced by network update:', messageId);
					return;
				}

				if (err) {
					console.error('[CHAT-MESH] Error sending message:', err);

					// Update status to 'failed'
					const updatedMessages = [...messagesNow];
					updatedMessages[messageIndex] = {
						...updatedMessages[messageIndex],
						status: 'failed' as MessageStatus
					};
					subscription.store.set(updatedMessages);
				} else {

					// Remove optimistic fields by creating a clean new object
					// Don't use delete - create new object for proper Svelte reactivity
					const oldMessage = messagesNow[messageIndex];
					const sentMessage: Message = {
						who: oldMessage.who,
						whopub: oldMessage.whopub,
						what: oldMessage.what,
						when: oldMessage.when
						// Explicitly omit messageId and status
					};

					const updatedMessages = [...messagesNow];
					updatedMessages[messageIndex] = sentMessage;
					subscription.store.set(updatedMessages);
				}
			}
		);
	} catch (error) {
		console.error(`[CHAT-MESH] Error sending message to ${chatId}:`, error);

		// On synchronous error, mark message as failed
		const currentMessages = get(subscription.store);
		const messageIndex = currentMessages.findIndex(
			(m) => m.messageId === messageId && m.status === 'pending'
		);

		if (messageIndex !== -1) {
			const updatedMessages = [...currentMessages];
			updatedMessages[messageIndex] = {
				...updatedMessages[messageIndex],
				status: 'failed' as MessageStatus
			};
			subscription.store.set(updatedMessages);
		}

		throw error;
	}
}

/**
 * Clear all Mesh chat subscriptions
 */
export function clearAllMeshChatSubscriptions(): void {
	meshChatSubscriptions.forEach((subscription, chatId) => {
		subscription.unsubscribe?.();
	});
	meshChatSubscriptions.clear();
	console.log('[CHAT-MESH] Cleared all chat subscriptions');
}

/**
 * Alias for clearAllMeshChatSubscriptions (for consistency with other modules)
 */
export function cleanupMeshChat(): void {
	clearAllMeshChatSubscriptions();
}

/**
 * Reset all chat subscriptions (alias for logout/re-login)
 */
export function resetAllChatSubscriptions(): void {
	clearAllMeshChatSubscriptions();
}

/**
 * Get all active Mesh chat subscription IDs
 */
export function getActiveMeshChatSubscriptions(): string[] {
	return Array.from(meshChatSubscriptions.keys());
}
