import { gun, user, userpub, GUN } from '$lib/state/gun.svelte';
import { browser } from '$app/environment';
import { writable, get, type Writable } from 'svelte/store';
import SEA from 'gun/sea';

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
 */
export function subscribeToChat(chatId: string) {
	// Don't create duplicate subscriptions
	if (chatSubscriptions.has(chatId)) {
		return chatSubscriptions.get(chatId)!;
	}

	console.log(`[Chat State] Subscribing to chat: ${chatId}`);

	const subscription: ChatSubscription = {
		chatId,
		store: writable<Message[]>([]),
		unsubscribe: undefined
	};

	// Set up Gun subscription
	const gunRef = gun.get(chatId).map();

	const unsubscribe = gunRef.on(async (data: any, key: string) => {
		if (data) {
			try {
				const encryptionKey = '#foo';

				var message = {
					// transform the data
					who: await gun.user(data).get('alias'), // a user might lie who they are! So let the user system detect whose data it is.
					what: (await SEA.decrypt(data.what, encryptionKey)) + '', // Use encryptionKey, not key
					// @ts-ignore - GUN.state.is typing issue
					when: GUN.state.is(data, 'what'), // get the internal timestamp for the what property.
					whopub: await gun.user(data).get('pub')
				};

				if (message.what && message.when) {
					// Get current messages from store
					const currentMessages = get(subscription.store);

					// Check if message already exists to prevent duplicates
					const messageExists = currentMessages.some(
						(m: Message) => m.when === message.when && m.what === message.what
					);

					if (!messageExists) {
						// Get current userpub value from store
						const currentUserPub = get(userpub);
						const isOwnMessage = (message.whopub as unknown as string) === currentUserPub;
						const isNewMessage = currentMessages.length > 0; // Skip notifications for initial load

						// Add message and keep sorted, then update store
						const updatedMessages = [...currentMessages, message as unknown as Message].sort(
							(a, b) => a.when - b.when
						);
						subscription.store.set(updatedMessages);

						// Show notification for new messages from others when window is not focused
						if (!isOwnMessage && isNewMessage && !get(isWindowFocused) && notificationManager) {
							notificationManager.onPeerMessage(
								{
									id: message.when.toString(),
									text: message.what,
									sender: message.who
								},
								message.whopub || 'unknown',
								message.who
							);
						}

						console.log(`[Chat State] New message in ${chatId}:`, message.what);
					}
				}
			} catch (error) {
				console.error(`[Chat State] Error processing message in ${chatId}:`, error);
			}
		}
	});

	subscription.unsubscribe = () => {
		gunRef.off();
		console.log(`[Chat State] Unsubscribed from chat: ${chatId}`);
	};

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
		console.log(`[Chat State] Removed subscription for chat: ${chatId}`);
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

		console.log(`[Chat State] Sending message to ${chatId}:`, messageText);

		const secret = await SEA.encrypt(messageText.trim(), encryptionKey);
		if (!secret) {
			throw new Error('Failed to encrypt message');
		}

		// Store message in user space and reference it in chat
		const message = user.get('all').set({ what: secret });
		const index = new Date().toISOString();

		// Put the message reference in the chat
		await gun.get(chatId).get(index).put(message);

		console.log(`[Chat State] Message sent successfully to ${chatId}`);
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
	console.log('[Chat State] Cleared all chat subscriptions');
}

/**
 * Check if a chat has unread messages (messages received while window was not focused)
 */
export function hasUnreadMessages(chatId: string): boolean {
	const subscription = chatSubscriptions.get(chatId);
	if (!subscription) return false;

	// This is a simple implementation - you might want to track read/unread state more sophisticatedly
	const messages = get(subscription.store);
	return messages.length > 0 && !get(isWindowFocused);
}
