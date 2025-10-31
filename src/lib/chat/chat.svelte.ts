import { writable, get, type Writable, derived } from 'svelte/store';
import type { ChatReadStates, ChatReadState } from '$lib/protocol/schemas';
import * as HolsterChat from './chat-holster.svelte';

// Conditionally import browser - gracefully handle test environment
let browser = false;
try {
	// @ts-ignore - dynamic import for test compatibility
	const env = await import('$app/environment');
	browser = env.browser;
} catch {
	// Running in test environment without SvelteKit
	browser = false;
}

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

// Global chat state - using Map to track subscriptions, but stores for reactivity
const chatSubscriptions = new Map<string, ChatSubscription>();
let notificationManager: any = null;
const isWindowFocused = writable(true);

// Store for chat read states - persisted across sessions (unwrapped)
export const chatReadStates = writable<ChatReadStates>({});

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
			// Count messages newer than last read timestamp (use lastReadTimestamp if available, otherwise lastRead)
			const lastRead = readState.lastReadTimestamp || readState.lastRead;
			counts[chatId] = messages.filter((msg) => msg.when > lastRead).length;
		}
	}

	// Also check read states for chats that might not have subscriptions yet
	Object.keys($chatReadStates).forEach((chatId) => {
		if (!(chatId in counts)) {
			const subscription = chatSubscriptions.get(chatId);
			if (subscription) {
				const messages = get(subscription.store);
				const readState = $chatReadStates[chatId];
				const lastRead = readState.lastReadTimestamp || readState.lastRead;
				counts[chatId] = messages.filter((msg) => msg.when > lastRead).length;
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
 * V5: Uses Holster-only backend
 */
export function getChatMessages(chatId: string): Writable<Message[]> {
	return HolsterChat.getHolsterChatMessages(chatId) as Writable<Message[]>;
}

/**
 * Send a message to a chat
 * V5: Uses Holster-only backend
 */
export async function sendMessage(chatId: string, messageText: string): Promise<void> {
	return HolsterChat.sendHolsterMessage(chatId, messageText);
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
 * V5: Holster-only
 */
export function clearAllChatData() {
	clearAllChatSubscriptions();
	clearChatReadStates();
	HolsterChat.clearAllHolsterChatSubscriptions();

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
	if (!existingState || readTimestamp > (existingState.lastReadTimestamp || existingState.lastRead)) {
		const updatedStates = {
			...currentStates,
			[chatId]: {
				lastRead: readTimestamp,  // Required field
				lastReadTimestamp: readTimestamp,  // Alias for compatibility
				updatedAt: Date.now(),
				_updatedAt: Date.now()  // Holster timestamp
			}
		};

		// 🚨 CRITICAL: Use atomic update to ensure data and timestamp sync
		// Update chat read states directly (no timestamp wrapping - Gun tracks timestamps)
		chatReadStates.set(updatedStates);
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

	// Count messages newer than last read timestamp (use lastReadTimestamp if available, otherwise lastRead)
	const lastRead = readState.lastReadTimestamp || readState.lastRead;
	const unreadCount = messages.filter((msg) => msg.when > lastRead).length;
	//console.log(
	//	`[Chat State] Unread count for ${chatId}: ${unreadCount} (last read: ${lastRead})`
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

		// Count messages newer than last read timestamp (use lastReadTimestamp if available, otherwise lastRead)
		const lastRead = readState.lastReadTimestamp || readState.lastRead;
		return $messages.filter((msg) => msg.when > lastRead).length;
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
export function setChatReadStates(states: ChatReadStates): void {
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
