import { gun, user, userPub, GUN } from '$lib/state/gun.svelte';
import { browser } from '$app/environment';
import { writable, get, type Writable, derived } from 'svelte/store';
import SEA from 'gun/sea';
import type { ChatReadStatesData } from '$lib/schema';
import { updateStoreWithFreshTimestamp, chatReadStatesTimestamp } from './core.svelte';

interface Message {
	who: string;
	what: string;
	when: number;
	whopub?: string;
}

interface ChatSubscription {
	chatId: string;
	store: Writable<Message[]>;
	unsubscribe?: () => void;
}

// Chat read state interface - tracks last read timestamp for each chat
interface ChatReadState {
	chatId: string;
	lastReadTimestamp: number;
	updatedAt: number;
}

// Map of chatId -> ChatReadState
type ChatReadStates = Record<string, ChatReadState>;

// Global chat state - using Map to track subscriptions, but stores for reactivity
const chatSubscriptions = new Map<string, ChatSubscription>();
let notificationManager: any = null;
const isWindowFocused = writable(true);

// Store for chat read states - persisted across sessions (flat data)
export const chatReadStates = writable<ChatReadStatesData>({});

// Loading state for read states
export const isLoadingChatReadStates = writable(false);

// Reactive unread counts store - updates when messages or read states change
export const unreadCounts = derived([chatReadStates], ([$chatReadStates]) => {
	const counts: Record<string, number> = {};

	// Calculate unread count for each chat that has a subscription
	for (const [chatId, subscription] of chatSubscriptions.entries()) {
		const messages = get(subscription.store);
		const readState = $chatReadStates[chatId];

		if (!readState) {
			// If no read state, all messages are unread
			counts[chatId] = messages.length;
		} else {
			// Count messages newer than last read timestamp
			counts[chatId] = messages.filter((msg) => msg.when > readState.lastReadTimestamp).length;
		}
	}

	// Also check read states for chats that might not have subscriptions yet
	Object.keys($chatReadStates).forEach((chatId) => {
		if (!(chatId in counts)) {
			const subscription = chatSubscriptions.get(chatId);
			if (subscription) {
				const messages = get(subscription.store);
				const readState = $chatReadStates[chatId];
				counts[chatId] = messages.filter((msg) => msg.when > readState.lastReadTimestamp).length;
			}
		}
	});

	return counts;
});

// Initialize notification manager and window focus tracking
if (browser) {
	// Get notification manager from global window if available
	if ((window as any).notificationManager) {
		notificationManager = (window as any).notificationManager;
	}

	// Track window focus for smart notifications
	const handleFocus = () => {
		isWindowFocused.set(true);
		// Clear notifications when window is focused
		if (notificationManager) {
			chatSubscriptions.forEach((_, chatId) => {
				notificationManager.clearNotification(`message-${chatId}`);
			});
		}
	};

	const handleBlur = () => {
		isWindowFocused.set(false);
	};

	window.addEventListener('focus', handleFocus);
	window.addEventListener('blur', handleBlur);
}

/**
 * Subscribe to a chat and start listening for messages in the background
 * NOTE: This is now primarily used by the network module for stream management
 */
export function subscribeToChat(chatId: string) {
	// Don't create duplicate subscriptions
	if (chatSubscriptions.has(chatId)) {
		return chatSubscriptions.get(chatId)!;
	}

	////console.log(`[Chat State] Creating chat subscription for: ${chatId}`);

	const subscription: ChatSubscription = {
		chatId,
		store: writable<Message[]>([]),
		unsubscribe: undefined
	};

	// Note: Gun subscription is now handled by network.svelte.ts
	// This function just creates the store structure

	chatSubscriptions.set(chatId, subscription);

	return subscription;
}

/**
 * Unsubscribe from a chat
 */
export function unsubscribeFromChat(chatId: string) {
	const subscription = chatSubscriptions.get(chatId);
	if (subscription) {
		subscription.unsubscribe?.();
		chatSubscriptions.delete(chatId);
		//console.log(`[Chat State] Removed subscription for chat: ${chatId}`);
	}
}

/**
 * Get messages store for a specific chat (subscribes if not already subscribed)
 */
export function getChatMessages(chatId: string): Writable<Message[]> {
	const subscription = chatSubscriptions.get(chatId) || subscribeToChat(chatId);
	return subscription.store;
}

/**
 * Send a message to a chat
 */
export async function sendMessage(chatId: string, messageText: string): Promise<void> {
	if (!messageText.trim()) {
		throw new Error('Message cannot be empty');
	}

	if (!user.is?.pub) {
		throw new Error('You must be logged in to send messages');
	}

	try {
		const encryptionKey = '#foo';

		//console.log(`[Chat State] Sending message to ${chatId}:`, messageText);

		const secret = await SEA.encrypt(messageText.trim(), encryptionKey);
		if (!secret) {
			throw new Error('Failed to encrypt message');
		}

		// Store message in user space and reference it in chat
		const message = user.get('all').set({ what: secret });
		const index = new Date().toISOString();

		// Put the message reference in the chat
		await gun.get(chatId).get(index).put(message);

		//console.log(`[Chat State] Message sent successfully to ${chatId}`);
	} catch (error) {
		console.error(`[Chat State] Error sending message to ${chatId}:`, error);
		throw error;
	}
}

/**
 * Get all active chat subscriptions
 */
export function getActiveChatSubscriptions(): string[] {
	return Array.from(chatSubscriptions.keys());
}

/**
 * Clear all chat subscriptions (useful for logout)
 */
export function clearAllChatSubscriptions() {
	chatSubscriptions.forEach((subscription, chatId) => {
		subscription.unsubscribe?.();
	});
	chatSubscriptions.clear();
	//console.log('[Chat State] Cleared all chat subscriptions');
}

/**
 * Clear all chat subscriptions and read states (useful for logout)
 */
export function clearAllChatData() {
	clearAllChatSubscriptions();
	clearChatReadStates();
	//console.log('[Chat State] Cleared all chat data');
}

/**
 * Mark a chat as read up to a specific timestamp
 * @param chatId The chat ID to mark as read
 * @param timestamp Optional timestamp to mark as read (defaults to current time)
 */
export function markChatAsRead(chatId: string, timestamp?: number): void {
	const readTimestamp = timestamp || Date.now();

	//console.log(`[Chat State] Marking chat ${chatId} as read up to timestamp: ${readTimestamp}`);

	// Get current states for comparison
	const currentStates = get(chatReadStates);
	const existingState = currentStates[chatId];

	// Only update if the new timestamp is newer than the existing one
	if (!existingState || readTimestamp > existingState.lastReadTimestamp) {
		const updatedStates = {
			...currentStates,
			[chatId]: {
				chatId,
				lastReadTimestamp: readTimestamp,
				updatedAt: Date.now()
			}
		};

		// 🚨 CRITICAL: Use atomic update to ensure data and timestamp sync
		updateStoreWithFreshTimestamp(chatReadStates, chatReadStatesTimestamp, updatedStates);
		//console.log(`[Chat State] Updated read state for ${chatId}:`, updatedStates[chatId]);
	}
}

/**
 * Get the number of unread messages in a chat (reactive version)
 * @param chatId The chat ID to check
 * @returns Number of unread messages
 */
export function getUnreadMessageCount(chatId: string): number {
	// Get subscription (create if doesn't exist for store access)
	const subscription = chatSubscriptions.get(chatId) || subscribeToChat(chatId);

	const messages = get(subscription.store);
	const readStates = get(chatReadStates);
	const readState = readStates[chatId];

	if (!readState) {
		// If no read state, all messages are unread
		return messages.length;
	}

	// Count messages newer than last read timestamp
	const unreadCount = messages.filter((msg) => msg.when > readState.lastReadTimestamp).length;
	//console.log(
	//	`[Chat State] Unread count for ${chatId}: ${unreadCount} (last read: ${readState.lastReadTimestamp})`
	//);

	return unreadCount;
}

/**
 * Get reactive unread count for a specific chat
 * @param chatId The chat ID to check
 * @returns Reactive unread count
 */
export function getReactiveUnreadCount(chatId: string) {
	// Ensure subscription exists
	const subscription = chatSubscriptions.get(chatId) || subscribeToChat(chatId);

	return derived([subscription.store, chatReadStates], ([$messages, $readStates]) => {
		const readState = $readStates[chatId];

		if (!readState) {
			// If no read state, all messages are unread
			return $messages.length;
		}

		// Count messages newer than last read timestamp
		return $messages.filter((msg) => msg.when > readState.lastReadTimestamp).length;
	});
}

/**
 * Get the timestamp of the last message in a chat
 * @param chatId The chat ID to check
 * @returns Timestamp of the last message, or null if no messages
 */
export function getLastMessageTimestamp(chatId: string): number | null {
	const subscription = chatSubscriptions.get(chatId);
	if (!subscription) return null;

	const messages = get(subscription.store);
	if (messages.length === 0) return null;

	// Messages are sorted by timestamp, so last message is the newest
	return messages[messages.length - 1].when;
}

/**
 * Mark a chat as read up to the latest message
 * @param chatId The chat ID to mark as read
 */
export function markChatAsFullyRead(chatId: string): void {
	const lastMessageTimestamp = getLastMessageTimestamp(chatId);
	if (lastMessageTimestamp) {
		markChatAsRead(chatId, lastMessageTimestamp);
	}
}

/**
 * Get all chat IDs that have unread messages
 * @returns Array of chat IDs with unread messages
 */
export function getChatsWithUnreadMessages(): string[] {
	const chatsWithUnread: string[] = [];
	const readStates = get(chatReadStates);

	// Check all chats we have read states for, not just subscribed chats
	for (const chatId of Object.keys(readStates)) {
		if (getUnreadMessageCount(chatId) > 0) {
			chatsWithUnread.push(chatId);
		}
	}

	return chatsWithUnread;
}

/**
 * Get read state for a specific chat
 * @param chatId The chat ID
 * @returns ChatReadState or null if not found
 */
export function getChatReadState(chatId: string): ChatReadState | null {
	const readStates = get(chatReadStates);
	return readStates[chatId] || null;
}

/**
 * Set read states (used for loading from network)
 * @param states The read states to set
 */
export function setChatReadStates(states: ChatReadStatesData): void {
	//console.log('[Chat State] Setting read states from network:', states);
	chatReadStates.set(states);
}

/**
 * Clear all read states (useful for logout)
 */
export function clearChatReadStates(): void {
	//console.log('[Chat State] Clearing all read states');
	chatReadStates.set({});
}

/**
 * Check if a chat has unread messages (using read state timestamps)
 */
export function hasUnreadMessages(chatId: string): boolean {
	return getUnreadMessageCount(chatId) > 0;
}
