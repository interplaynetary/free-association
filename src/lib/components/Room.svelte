<script lang="ts">
	import ChatMessage from './ChatMessage.svelte';
	import { onMount } from 'svelte';
	import { username, user } from '$lib/state/gun.svelte';
	import {
		currentRoom,
		currentRoomChat,
		joinRoom,
		postMessageToRoom,
		leaveRoom,
		type RoomMessage
	} from '$lib/state/room.svelte';

	interface RoomProps {
		placeholder?: string;
		maxLength?: number;
	}

	let { placeholder = 'Type a message...', maxLength = 500 }: RoomProps = $props();

	// Room joining form state
	let hostPubKey = $state('');
	let hostUsername = $state('');
	let roomId = $state('');
	let isJoining = $state(false);

	// Chat state
	let newMessage = $state('');
	let scrollBottom: HTMLDivElement;
	let lastScrollTop = $state(0);
	let canAutoScroll = $state(true);
	let unreadMessages = $state(false);

	// Auto-scroll functionality
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

	// Transform room messages to match ChatMessage interface
	function transformMessage(roomMessage: RoomMessage) {
		return {
			who: hostUsername || roomMessage.authorId.substring(0, 8), // Use provided username or truncated pubkey
			what: roomMessage.content,
			when: roomMessage.timestamp,
			whopub: roomMessage.authorId
		};
	}

	// Join room functionality
	async function handleJoinRoom() {
		if (!hostPubKey.trim() || !roomId.trim()) return;

		try {
			isJoining = true;
			console.log(`[ROOM UI] Joining room ${roomId} hosted by ${hostPubKey}`);

			joinRoom(roomId.trim(), hostPubKey.trim());

			// Clear the form
			hostPubKey = '';
			hostUsername = '';
			roomId = '';
		} catch (error) {
			console.error('[ROOM UI] Error joining room:', error);
		} finally {
			isJoining = false;
		}
	}

	// Send message functionality
	async function sendMessage() {
		if (!newMessage.trim() || !$currentRoom) return;

		try {
			console.log(`[ROOM UI] Sending message to room ${$currentRoom.id}`);
			await postMessageToRoom($currentRoom.id, newMessage.trim());

			newMessage = '';
			canAutoScroll = true;
			autoScroll();
		} catch (error) {
			console.error('[ROOM UI] Error sending message:', error);
		}
	}

	// Leave room functionality
	function handleLeaveRoom() {
		console.log('[ROOM UI] Leaving room');
		leaveRoom();
	}

	// Auto-scroll when new messages arrive
	$effect(() => {
		if ($currentRoomChat.length > 0) {
			if (canAutoScroll) {
				autoScroll();
			} else {
				unreadMessages = true;
			}
		}
	});
</script>

<div class="container">
	{#if $username}
		{#if !$currentRoom}
			<!-- Room joining form -->
			<div class="join-form">
				<h3>Join a Room</h3>
				<form
					onsubmit={(e) => {
						e.preventDefault();
						handleJoinRoom();
					}}
				>
					<div class="form-group">
						<label for="hostPubKey">Host Public Key:</label>
						<input
							id="hostPubKey"
							type="text"
							bind:value={hostPubKey}
							placeholder="Enter the host's public key..."
							disabled={isJoining}
							required
						/>
					</div>

					<div class="form-group">
						<label for="hostUsername">Host Username (optional):</label>
						<input
							id="hostUsername"
							type="text"
							bind:value={hostUsername}
							placeholder="Display name for host..."
							disabled={isJoining}
						/>
					</div>

					<div class="form-group">
						<label for="roomId">Room ID:</label>
						<input
							id="roomId"
							type="text"
							bind:value={roomId}
							placeholder="Enter room ID..."
							disabled={isJoining}
							required
						/>
					</div>

					<button type="submit" disabled={isJoining || !hostPubKey.trim() || !roomId.trim()}>
						{isJoining ? 'Joining...' : 'Join Room'}
					</button>
				</form>

				<div class="instructions">
					<p><strong>How to join a room:</strong></p>
					<ol>
						<li>Get the public key of the room host</li>
						<li>Get the room ID from the host</li>
						<li>Optionally provide a display name for the host</li>
						<li>Click "Join Room" to start participating</li>
					</ol>
				</div>
			</div>
		{:else}
			<!-- Room chat interface -->
			<div class="room-header">
				<div class="room-info">
					<h3>Room: {$currentRoom.id}</h3>
					<p>
						Host: {hostUsername || $currentRoom.hostId.substring(0, 8)}... • {$currentRoom.pubKeys
							.length} participants
					</p>
				</div>
				<button onclick={handleLeaveRoom} class="leave-button">Leave Room</button>
			</div>

			<main onscroll={watchScroll}>
				{#each $currentRoomChat as message (message.id)}
					<ChatMessage message={transformMessage(message)} sender={$username} />
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
		{/if}
	{:else}
		<div class="login-message">
			<p>Please log in to join rooms</p>
		</div>
	{/if}
</div>

<style>
	.container {
		display: flex;
		flex-direction: column;
		height: 100%;
		min-height: 400px;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		background: white;
		overflow: hidden;
	}

	.join-form {
		padding: 2rem;
		max-width: 500px;
		margin: 0 auto;
	}

	.join-form h3 {
		margin: 0 0 1.5rem 0;
		color: #1f2937;
		text-align: center;
	}

	.form-group {
		margin-bottom: 1rem;
	}

	.form-group label {
		display: block;
		margin-bottom: 0.5rem;
		font-weight: 500;
		color: #374151;
	}

	.form-group input {
		width: 100%;
		padding: 0.75rem;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 0.875rem;
		box-sizing: border-box;
	}

	.form-group input:focus {
		outline: none;
		border-color: #3b82f6;
		box-shadow: 0 0 0 1px #3b82f6;
	}

	.join-form button {
		width: 100%;
		padding: 0.75rem;
		border: none;
		border-radius: 4px;
		background: #3b82f6;
		color: white;
		font-weight: 500;
		cursor: pointer;
		margin-bottom: 1.5rem;
	}

	.join-form button:disabled {
		background: #9ca3af;
		cursor: not-allowed;
	}

	.instructions {
		background: #f3f4f6;
		padding: 1rem;
		border-radius: 4px;
		font-size: 0.875rem;
		color: #4b5563;
	}

	.instructions p {
		margin: 0 0 0.5rem 0;
		font-weight: 500;
	}

	.instructions ol {
		margin: 0;
		padding-left: 1.25rem;
	}

	.instructions li {
		margin-bottom: 0.25rem;
	}

	.room-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 1rem;
		border-bottom: 1px solid #e5e7eb;
		background: #f9fafb;
		flex-shrink: 0;
	}

	.room-info h3 {
		margin: 0;
		color: #1f2937;
		font-size: 1.125rem;
	}

	.room-info p {
		margin: 0.25rem 0 0 0;
		color: #6b7280;
		font-size: 0.875rem;
	}

	.leave-button {
		padding: 0.5rem 1rem;
		border: 1px solid #ef4444;
		border-radius: 4px;
		background: white;
		color: #ef4444;
		cursor: pointer;
		font-size: 0.875rem;
	}

	.leave-button:hover {
		background: #ef4444;
		color: white;
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

	form input {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		margin-right: 0.5rem;
		min-width: 0;
		box-sizing: border-box;
	}

	form button {
		padding: 0.5rem 1rem;
		border: none;
		border-radius: 4px;
		background: #3b82f6;
		color: white;
		cursor: pointer;
		flex-shrink: 0;
	}

	form button:disabled {
		background: #9ca3af;
		cursor: not-allowed;
	}

	.scroll-button {
		position: absolute;
		bottom: 80px;
		right: 1rem;
		z-index: 10;
	}

	.scroll-button button {
		border-radius: 50%;
		width: 40px;
		height: 40px;
		background: #6b7280;
		border: none;
		color: white;
		cursor: pointer;
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
