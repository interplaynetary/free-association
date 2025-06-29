# Background Chat Subscription System

## Overview

The chat subscription logic has been extracted from individual Chat components into a global state management system (`src/lib/state/chat.svelte.ts`). This enables:

1. **Background Message Listening**: Messages are received even when the chat component is not mounted
2. **Notification Support**: New messages trigger notifications when the window is not focused
3. **Persistent Subscriptions**: Chat subscriptions persist across component mount/unmount cycles
4. **Centralized State**: All chat state is managed in one place

## Key Benefits

### Before (Component-based subscriptions)

- ❌ Messages only received when Chat component is mounted
- ❌ No notifications for messages received while chat is closed
- ❌ Duplicate subscriptions if multiple chat components exist
- ❌ Lost messages during component re-mounts

### After (Background subscriptions)

- ✅ Messages received continuously in background
- ✅ Smart notifications when window is not focused
- ✅ Single subscription per chat ID
- ✅ Persistent message history

## Usage

### Basic Chat Component

```typescript
import { subscribeToChat, getChatMessages, sendMessage } from '$lib/state/chat.svelte';

// Subscribe to a chat (starts background listening)
subscribeToChat('general');

// Get messages (automatically subscribes if not already subscribed)
const messages = getChatMessages('general');

// Send a message
await sendMessage('general', 'Hello world!');
```

### In Svelte Components

```svelte
<script>
	import { onMount } from 'svelte';
	import { subscribeToChat, getChatMessages } from '$lib/state/chat.svelte';

	let messages = $state([]);
	let messagesStore;

	onMount(() => {
		// Start background subscription
		subscribeToChat('general');

		// Get reactive store and subscribe to changes
		messagesStore = getChatMessages('general');
		const unsubscribe = messagesStore.subscribe((newMessages) => {
			messages = newMessages;
		});

		return () => unsubscribe();
	});
</script>
```

## Notification Integration

The system automatically integrates with the existing notification manager:

- Messages received while window is not focused trigger notifications
- Notifications are cleared when window regains focus
- Only messages from other users trigger notifications (not your own messages)
- Notifications include sender name and message preview

## API Reference

### `subscribeToChat(chatId: string)`

Starts background subscription to a chat. Safe to call multiple times.

### `unsubscribeFromChat(chatId: string)`

Stops listening to a specific chat.

### `getChatMessages(chatId: string): Writable<Message[]>`

Returns a reactive Svelte store for chat messages. Automatically subscribes if needed.

### `sendMessage(chatId: string, messageText: string): Promise<void>`

Sends an encrypted message to a chat.

### `clearAllChatSubscriptions()`

Clears all active subscriptions (called automatically on logout).

### `getActiveChatSubscriptions(): string[]`

Returns list of currently active chat IDs.

### `hasUnreadMessages(chatId: string): boolean`

Simple check for unread messages (when window was not focused).

## Implementation Details

- Uses Gun's `.map().on()` for real-time subscriptions
- Messages stored in reactive Svelte stores for instant UI updates
- No polling - immediate reactivity through store subscriptions
- Messages are encrypted with a shared key ('#foo')
- Timestamps use Gun's internal timing for reliable ordering
- Window focus/blur events control notification behavior
- Integrates with existing notification manager
- Cleanup on logout via global state reset

## Migration from Component-based Chat

1. Remove `onMount` subscription logic from Chat components
2. Import functions from `$lib/state/chat.svelte`
3. Call `subscribeToChat(chatId)` in `onMount`
4. Get store with `getChatMessages(chatId)` and subscribe to it
5. Replace direct Gun operations with `sendMessage()`
6. Remove polling intervals - use store subscriptions instead

The old Chat.svelte has been refactored as an example of this migration.
