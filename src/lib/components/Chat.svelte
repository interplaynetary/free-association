<script lang="ts">
	import ChatMessage from '$lib/components/ChatMessage.svelte';
	import { onMount, onDestroy } from 'svelte';
	import { userAlias, userPub, user } from '$lib/state/gun.svelte';
	import { globalState } from '$lib/global.svelte';
	import {
		subscribeToChat,
		getChatMessages,
		sendMessage as sendChatMessage,
		unsubscribeFromChat,
		markChatAsRead,
		getLastMessageTimestamp,
		getUnreadMessageCount
	} from '$lib/state/chat.svelte';

	interface ChatProps {
		chatId?: string;
		placeholder?: string;
		maxLength?: number;
	}

	let { chatId = 'chat', placeholder = 'Type a message...', maxLength = 100 }: ChatProps = $props();

	let newMessage = $state('');
	let isSending = $state(false);

	let scrollBottom = $state<HTMLDivElement>();
	let lastScrollTop = $state(0);
	let canAutoScroll = $state(true);
	let unreadMessages = $state(false);
	let mainElement: HTMLElement | null = null;
	let isUserActivelyViewing = $state(false);
	let markAsReadTimeout: ReturnType<typeof setTimeout> | null = null;

	// Get reactive store for messages
	let messagesStore: any;
	let messages = $state<any[]>([]);

	function autoScroll() {
		setTimeout(() => {
			if (scrollBottom) {
				// Scroll within the chat container only, not the entire page
				mainElement = scrollBottom.closest('main');
				if (mainElement) {
					mainElement.scrollTop = mainElement.scrollHeight;
				}
			}
		}, 50);
		unreadMessages = false;

		// Mark chat as read when user scrolls to bottom
		markChatAsReadDebounced();
	}

	function watchScroll(e: Event) {
		const target = e.target as HTMLElement;
		const newScrollTop = target.scrollTop;
		canAutoScroll = (newScrollTop || Infinity) > lastScrollTop;
		lastScrollTop = newScrollTop;

		// Check if user is near the bottom of the chat
		const isNearBottom = newScrollTop + target.clientHeight >= target.scrollHeight - 100;

		// If user is actively scrolling and near the bottom, mark as read
		if (isNearBottom && canAutoScroll) {
			isUserActivelyViewing = true;
			markChatAsReadDebounced();
		}
	}

	function markChatAsReadDebounced() {
		// Clear any existing timeout
		if (markAsReadTimeout) {
			clearTimeout(markAsReadTimeout);
		}

		// Set a new timeout to mark as read after a short delay
		markAsReadTimeout = setTimeout(() => {
			if (isUserActivelyViewing || canAutoScroll) {
				const lastMessageTimestamp = getLastMessageTimestamp(chatId);
				if (lastMessageTimestamp) {
					markChatAsRead(chatId, lastMessageTimestamp);
					console.log(`[Chat] Marked ${chatId} as read up to ${lastMessageTimestamp}`);
				}
			}
		}, 1000); // 1 second delay
	}

	function handleVisibilityChange() {
		if (document.visibilityState === 'visible') {
			// User returned to the tab - if they're viewing this chat, mark as read
			if (canAutoScroll) {
				markChatAsReadDebounced();
			}
		}
	}

	onMount(() => {
		// Subscribe to the chat - this will start background listening
		subscribeToChat(chatId);

		// Get the reactive store for this chat
		messagesStore = getChatMessages(chatId);

		// Set up visibility change listener
		document.addEventListener('visibilitychange', handleVisibilityChange);

		// Subscribe to store changes with reactive updates
		const unsubscribe = messagesStore.subscribe((newMessages: any[]) => {
			const previousLength = messages.length;
			messages = newMessages;

			// Auto-scroll for new messages
			if (newMessages.length > previousLength) {
				if (canAutoScroll) {
					autoScroll();
				} else {
					unreadMessages = true;
				}
			}

			// If user is actively viewing and there are new messages, mark as read
			if (newMessages.length > 0 && (canAutoScroll || isUserActivelyViewing)) {
				markChatAsReadDebounced();
			}
		});

		// Mark as read when component mounts if user is at bottom
		setTimeout(() => {
			if (canAutoScroll && messages.length > 0) {
				isUserActivelyViewing = true;
				markChatAsReadDebounced();
			}
		}, 500);

		return () => {
			unsubscribe();
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			if (markAsReadTimeout) {
				clearTimeout(markAsReadTimeout);
			}
		};
	});

	onDestroy(() => {
		// Optionally unsubscribe when component is destroyed
		// Comment this out if you want to keep listening in background
		// unsubscribeFromChat(chatId);

		// Clean up timeout
		if (markAsReadTimeout) {
			clearTimeout(markAsReadTimeout);
		}
	});

	async function sendMessage() {
		if (!newMessage.trim() || isSending) return;

		isSending = true;

		try {
			await sendChatMessage(chatId, newMessage.trim());

			// Clear the input and scroll
			newMessage = '';
			canAutoScroll = true;
			autoScroll();
		} catch (error) {
			console.error('Error sending message:', error);
			const errorMessage = error instanceof Error ? error.message : 'Failed to send message';
			globalState.showToast(errorMessage, 'error');
		} finally {
			isSending = false;
		}
	}

	function handleFormSubmit(e: Event) {
		e.preventDefault();
		e.stopPropagation();
		sendMessage();
	}

	function handleKeyDown(e: KeyboardEvent) {
		if (e.key === 'Enter' && !e.shiftKey) {
			e.preventDefault();
			sendMessage();
		}
	}

	function handleButtonClick(e: Event) {
		e.preventDefault();
		e.stopPropagation();
		sendMessage();
	}
</script>

<div class="container">
	{#if user.is?.pub}
		<main onscroll={watchScroll}>
			{#each messages as message (message.when)}
				<ChatMessage {message} sender={$userPub} />
			{/each}

			<div class="dummy" bind:this={scrollBottom}></div>
		</main>

		<form onsubmit={handleFormSubmit}>
			<input
				type="text"
				{placeholder}
				bind:value={newMessage}
				maxlength={maxLength}
				onkeydown={handleKeyDown}
				disabled={isSending}
			/>

			<button type="submit" disabled={!newMessage.trim() || isSending} onclick={handleButtonClick}>
				{#if isSending}
					⏳
				{:else}
					💥
				{/if}
			</button>
		</form>

		{#if !canAutoScroll}
			<div class="scroll-button">
				<button onclick={autoScroll} class:red={unreadMessages}>
					{#if unreadMessages}
						💬
					{/if}
					👇
				</button>
			</div>
		{/if}
	{:else}
		<div class="login-message">
			<p>Please log in to chat</p>
		</div>
	{/if}
</div>

<style>
	.container {
		display: flex;
		flex-direction: column;
		height: 100%;
		min-height: 300px;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		background: white;
		overflow: hidden; /* Prevent container overflow */
	}

	main {
		flex: 1;
		overflow-y: auto;
		overflow-x: hidden; /* Prevent horizontal overflow */
		padding: 1rem;
		max-height: 400px;
		position: relative;
		width: 100%;
		box-sizing: border-box;
		/* Ensure content stays within bounds */
		word-wrap: break-word;
		overflow-wrap: break-word;
	}

	/* Ensure all children of main respect the container width */
	main > * {
		max-width: 100%;
		box-sizing: border-box;
	}

	form {
		display: flex;
		padding: 0.5rem;
		border-top: 1px solid #e5e7eb;
		background: #f9fafb;
		flex-shrink: 0; /* Prevent form from shrinking */
		box-sizing: border-box;
	}

	input {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		margin-right: 0.5rem;
		min-width: 0; /* Allow input to shrink if needed */
		box-sizing: border-box;
	}

	input:disabled {
		background: #f3f4f6;
		color: #6b7280;
		cursor: not-allowed;
	}

	button {
		padding: 0.5rem 1rem;
		border: none;
		border-radius: 4px;
		background: #3b82f6;
		color: white;
		cursor: pointer;
		flex-shrink: 0; /* Prevent button from shrinking */
	}

	button:disabled {
		background: #9ca3af;
		cursor: not-allowed;
	}

	.scroll-button {
		position: absolute;
		bottom: 80px;
		right: 1rem;
		z-index: 10; /* Ensure it's above other content */
	}

	.scroll-button button {
		border-radius: 50%;
		width: 40px;
		height: 40px;
		background: #6b7280;
	}

	.scroll-button button.red {
		background: #ef4444;
	}

	.dummy {
		height: 1px;
		width: 100%;
		flex-shrink: 0;
	}

	.login-message {
		display: flex;
		align-items: center;
		justify-content: center;
		height: 100%;
		color: #6b7280;
		font-style: italic;
		padding: 1rem;
		box-sizing: border-box;
	}

	.login-message p {
		margin: 0;
		text-align: center;
	}
</style>
