<script lang="ts">
	import ChatMessage from './ChatMessage.svelte';
	import { onMount } from 'svelte';
	import { username, userpub, user, gun, GUN } from '$lib/state/gun.svelte';
	import { browser } from '$app/environment';
	import SEA from 'gun/sea';

	interface ChatProps {
		chatId?: string;
		placeholder?: string;
		maxLength?: number;
	}

	let { chatId = 'chat', placeholder = 'Type a message...', maxLength = 100 }: ChatProps = $props();

	interface Message {
		who: string;
		what: string;
		when: number;
		whopub?: string;
	}

	let newMessage = $state('');
	let messages = $state<Message[]>([]);

	let scrollBottom: HTMLDivElement;
	let lastScrollTop = $state(0);
	let canAutoScroll = $state(true);
	let unreadMessages = $state(false);

	// Notification support
	let notificationManager: any = null;
	let isWindowFocused = $state(true);

	function autoScroll() {
		setTimeout(() => {
			if (scrollBottom) {
				const mainElement = scrollBottom.closest('main');
				if (mainElement) {
					mainElement.scrollTop = mainElement.scrollHeight;
				}
			}
		}, 50);
		unreadMessages = false;
	}

	function watchScroll(e: Event) {
		const target = e.target as HTMLElement;
		canAutoScroll = (target.scrollTop || Infinity) > lastScrollTop;
		lastScrollTop = target.scrollTop;
	}

	// Handle window focus/blur for notifications
	function handleFocus() {
		isWindowFocused = true;
		// Clear notifications when window is focused
		if (notificationManager) {
			notificationManager.clearNotification(`message-${chatId}`);
		}
	}

	function handleBlur() {
		isWindowFocused = false;
	}

	onMount(() => {
		// Get notification manager from global window
		if (browser && (window as any).notificationManager) {
			notificationManager = (window as any).notificationManager;
		}

		// Set up window focus/blur listeners
		if (browser) {
			window.addEventListener('focus', handleFocus);
			window.addEventListener('blur', handleBlur);
		}

		// Get Messages with notification support
		gun
			.get(chatId)
			.map()
			.on(async (data: any, key: string) => {
				if (data) {
					const encryptionKey = '#foo';

					var message = {
						who: await gun.user(data).get('alias'),
						what: (await SEA.decrypt(data.what, encryptionKey)) + '',
						when: GUN.state.is(data, 'what'),
						whopub: await gun.user(data).get('pub')
					};

					if (message.what && message.when) {
						const messageExists = messages.some(
							(m) => m.when === message.when && m.what === message.what
						);

						if (!messageExists) {
							// Check if this is a new message from someone else
							const isOwnMessage = message.whopub === $userpub;
							const isNewMessage = messages.length > 0; // Skip notifications for initial load
							
							messages = [...messages, message].sort((a, b) => a.when - b.when);

							if (canAutoScroll) {
								autoScroll();
							} else {
								unreadMessages = true;
							}

							// Show notification for new messages from others when window is not focused
							if (!isOwnMessage && isNewMessage && !isWindowFocused && notificationManager) {
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
						}
					}
				}
			});

		// Cleanup function
		return () => {
			if (browser) {
				window.removeEventListener('focus', handleFocus);
				window.removeEventListener('blur', handleBlur);
			}
		};
	});

	async function sendMessage() {
		if (!newMessage.trim()) return;

		try {
			const encryptionKey = '#foo';
			const secret = await SEA.encrypt(newMessage.trim(), encryptionKey);

			const message = user.get('all').set({ what: secret });
			const index = new Date().toISOString();

			gun.get(chatId).get(index).put(message);

			newMessage = '';
			canAutoScroll = true;
			autoScroll();
		} catch (error) {
			console.error('Error sending message:', error);
		}
	}

	// Request notification permission when component mounts
	onMount(async () => {
		if (browser && notificationManager) {
			await notificationManager.requestPermission();
		}
	});
</script>

<div class="container">
	{#if user.is?.pub}
		<main onscroll={watchScroll}>
			{#each messages as message (message.when)}
				<ChatMessage {message} sender={$userpub} />
			{/each}

			<div class="dummy" bind:this={scrollBottom}></div>
		</main>

		<form
			onsubmit={(e) => {
				e.preventDefault();
				sendMessage();
			}}
		>
			<input type="text" {placeholder} bind:value={newMessage} maxlength={maxLength} />
			<button type="submit" disabled={!newMessage.trim()}>💥</button>
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

		<!-- Notification status indicator -->
		{#if browser && notificationManager}
			<div class="notification-status">
				{#if Notification.permission === 'granted'}
					🔔 Notifications enabled
				{:else if Notification.permission === 'denied'}
					🔕 Notifications blocked
				{:else}
					<button onclick={() => notificationManager?.requestPermission()}>
						🔔 Enable notifications
					</button>
				{/if}
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
		overflow: hidden;
	}

	main {
		flex: 1;
		overflow-y: auto;
		overflow-x: hidden;
		padding: 1rem;
		max-height: 400px;
		position: relative;
		width: 100%;
		box-sizing: border-box;
		word-wrap: break-word;
		overflow-wrap: break-word;
	}

	main > * {
		max-width: 100%;
		box-sizing: border-box;
	}

	form {
		display: flex;
		padding: 0.5rem;
		border-top: 1px solid #e5e7eb;
		background: #f9fafb;
		flex-shrink: 0;
		box-sizing: border-box;
	}

	input {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #d1d5db;
		border-radius: 0.375rem;
		margin-right: 0.5rem;
		font-size: 14px;
		box-sizing: border-box;
	}

	button {
		padding: 0.5rem 1rem;
		background: #3b82f6;
		color: white;
		border: none;
		border-radius: 0.375rem;
		cursor: pointer;
		font-size: 14px;
		flex-shrink: 0;
	}

	button:disabled {
		background: #9ca3af;
		cursor: not-allowed;
	}

	button:hover:not(:disabled) {
		background: #2563eb;
	}

	.scroll-button {
		position: absolute;
		bottom: 70px;
		right: 20px;
		z-index: 10;
	}

	.scroll-button button {
		border-radius: 50%;
		width: 40px;
		height: 40px;
		display: flex;
		align-items: center;
		justify-content: center;
		box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1);
		background: white;
		color: #374151;
		border: 1px solid #e5e7eb;
	}

	.scroll-button button.red {
		background: #ef4444;
		color: white;
		border-color: #dc2626;
	}

	.notification-status {
		padding: 0.5rem;
		background: #f3f4f6;
		border-top: 1px solid #e5e7eb;
		font-size: 12px;
		color: #6b7280;
		text-align: center;
	}

	.notification-status button {
		background: #10b981;
		font-size: 12px;
		padding: 0.25rem 0.5rem;
	}

	.notification-status button:hover {
		background: #059669;
	}

	.login-message {
		display: flex;
		align-items: center;
		justify-content: center;
		height: 100%;
		color: #6b7280;
	}

	.dummy {
		height: 1px;
	}
</style> 