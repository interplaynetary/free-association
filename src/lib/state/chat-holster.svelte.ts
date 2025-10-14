import { writable, get, type Writable } from 'svelte/store';
import { holster, holsterUser } from './holster.svelte';

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
const holsterChatSubscriptions = new Map<string, ChatSubscription>();

/**
 * Process messages from Holster data
 * Holster returns complete object with all messages at once (no streaming)
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
			console.warn('[CHAT-HOLSTER] Message missing timestamp:', messageId);
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
 * Subscribe to a Holster chat and start listening for messages
 */
export function subscribeToHolsterChat(chatId: string): ChatSubscription {
	// Return existing subscription if it exists
	if (holsterChatSubscriptions.has(chatId)) {
		return holsterChatSubscriptions.get(chatId)!;
	}

	console.log(`[CHAT-HOLSTER] Creating chat subscription for: ${chatId}`);

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

		console.log(`[CHAT-HOLSTER] ${subscription.isInitialLoad ? 'Initial load' : 'Delta detection'}: ${newMessages.length} messages (lastSeenTimestamp: ${subscription.lastSeenTimestamp})`);

		if (newMessages.length > 0) {
			console.log(`[CHAT-HOLSTER] Processed ${newMessages.length} messages`);

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

	// Subscribe to chat updates from Holster
	// Note: Holster returns complete object (all messages at once), not streaming
	holster.get(chatId).on(onChatUpdate, true);

	// Store unsubscribe function
	subscription.unsubscribe = () => holster.get(chatId).off(onChatUpdate);
	holsterChatSubscriptions.set(chatId, subscription);

	console.log('[CHAT-HOLSTER] Subscribed to chat:', chatId);
	return subscription;
}

/**
 * Unsubscribe from a Holster chat
 */
export function unsubscribeFromHolsterChat(chatId: string): void {
	const subscription = holsterChatSubscriptions.get(chatId);
	if (subscription) {
		subscription.unsubscribe?.();
		holsterChatSubscriptions.delete(chatId);
		console.log(`[CHAT-HOLSTER] Unsubscribed from chat: ${chatId}`);
	}
}

/**
 * Get messages store for a specific chat (subscribes if not already subscribed)
 */
export function getHolsterChatMessages(chatId: string): Writable<Message[]> {
	const subscription = holsterChatSubscriptions.get(chatId) || subscribeToHolsterChat(chatId);
	return subscription.store;
}

/**
 * Send a message to a Holster chat with optimistic UI updates
 * NOTE: Messages are sent in PLAIN TEXT (no encryption)
 * This is honest about the security model - capacity chats are public by design
 */
export async function sendHolsterMessage(chatId: string, messageText: string): Promise<void> {
	if (!messageText.trim()) {
		throw new Error('Message cannot be empty');
	}

	if (!holsterUser.is || !holsterUser.is.pub) {
		throw new Error('You must be logged in to send messages');
	}

	// Get or create subscription
	const subscription = holsterChatSubscriptions.get(chatId) || subscribeToHolsterChat(chatId);

	// Create message ID and timestamp
	const messageId = Date.now().toString();
	const timestamp = Date.now();

	const message: Message = {
		who: holsterUser.is.username || 'Unknown',
		whopub: holsterUser.is.pub,
		what: messageText.trim(),
		when: timestamp,
		messageId: messageId,
		status: 'pending' as MessageStatus
	};

	try {
		console.log(`[CHAT-HOLSTER] Sending message to ${chatId}:`, messageText);

		// OPTIMISTIC UPDATE: Add message to store immediately with 'pending' status
		const currentMessages = get(subscription.store);
		subscription.store.set([...currentMessages, message]);

		// Update lastSeenTimestamp BEFORE sending to prevent network duplicate
		subscription.lastSeenTimestamp = Math.max(
			subscription.lastSeenTimestamp,
			timestamp
		);

		// Store message directly in chat node using Holster's chaining API
		// The callback handles status updates
		holster.get(chatId).next(messageId).put(
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
					console.log('[CHAT-HOLSTER] Pending message already replaced by network update:', messageId);
					return;
				}

				if (err) {
					console.error('[CHAT-HOLSTER] Error sending message:', err);

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
		console.error(`[CHAT-HOLSTER] Error sending message to ${chatId}:`, error);

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
 * Clear all Holster chat subscriptions
 */
export function clearAllHolsterChatSubscriptions(): void {
	holsterChatSubscriptions.forEach((subscription, chatId) => {
		subscription.unsubscribe?.();
	});
	holsterChatSubscriptions.clear();
	console.log('[CHAT-HOLSTER] Cleared all chat subscriptions');
}

/**
 * Alias for clearAllHolsterChatSubscriptions (for consistency with other modules)
 */
export function cleanupHolsterChat(): void {
	clearAllHolsterChatSubscriptions();
}

/**
 * Reset all chat subscriptions (alias for logout/re-login)
 */
export function resetAllChatSubscriptions(): void {
	clearAllHolsterChatSubscriptions();
}

/**
 * Get all active Holster chat subscription IDs
 */
export function getActiveHolsterChatSubscriptions(): string[] {
	return Array.from(holsterChatSubscriptions.keys());
}
