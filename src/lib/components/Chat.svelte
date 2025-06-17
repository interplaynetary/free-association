<script lang="ts">
	import ChatMessage from './ChatMessage.svelte';
	import { onMount } from 'svelte';
	import { username, userpub, user, gun, GUN } from '$lib/state/gun.svelte';
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
		whopub?: string; // Match ChatMessage interface
	}

	let newMessage = $state('');
	let messages = $state<Message[]>([]);

	let scrollBottom: HTMLDivElement;
	let lastScrollTop = $state(0);
	let canAutoScroll = $state(true);
	let unreadMessages = $state(false);

	function autoScroll() {
		setTimeout(() => {
			if (scrollBottom) {
				// Scroll within the chat container only, not the entire page
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

	onMount(() => {
		// Remove the time filter to get all messages
		// Get Messages (simplified like example)
		gun
			.get(chatId)
			.map()
			.on(async (data: any, key: string) => {
				if (data) {
					// Key for end-to-end encryption
					const encryptionKey = '#foo';

					var message = {
						// transform the data
						who: await gun.user(data).get('alias'), // a user might lie who they are! So let the user system detect whose data it is.
						what: (await SEA.decrypt(data.what, encryptionKey)) + '', // Use encryptionKey, not key
						when: GUN.state.is(data, 'what'), // get the internal timestamp for the what property.
						whopub: await gun.user(data).get('pub')
					};

					if (message.what && message.when) {
						// Check if message already exists to prevent duplicates
						const messageExists = messages.some(
							(m) => m.when === message.when && m.what === message.what
						);

						if (!messageExists) {
							// Add message and keep array sorted, but don't artificially limit to 100
							messages = [...messages, message].sort((a, b) => a.when - b.when);

							if (canAutoScroll) {
								autoScroll();
							} else {
								unreadMessages = true;
							}
						}
					}
				}
			});
	});

	async function sendMessage() {
		if (!newMessage.trim()) return;

		try {
			const encryptionKey = '#foo';
			const secret = await SEA.encrypt(newMessage.trim(), encryptionKey);

			// Debug logging
			console.log('Sending message with userPub:', user.is?.pub);

			// Store message in user space and reference it in chat (like the example)
			const message = user.get('all').set({ what: secret });
			const index = new Date().toISOString();

			console.log('Storing message reference in chat at index:', index);

			// Put the message reference in the chat
			gun.get(chatId).get(index).put(message);

			// Clear the input and scroll
			newMessage = '';
			canAutoScroll = true;
			autoScroll();
		} catch (error) {
			console.error('Error sending message:', error);
		}
	}
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
