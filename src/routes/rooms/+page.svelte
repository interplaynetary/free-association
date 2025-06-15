<script lang="ts">
	import Room from '$lib/components/Room.svelte';
	import { username } from '$lib/state/gun.svelte';
	import { currentRoom } from '$lib/state/room.svelte';
</script>

<div class="page-container">
	<div class="page-header">
		<h1>Rooms</h1>
		<p class="page-description">
			Join decentralized chat rooms by connecting to room hosts and participating in group
			conversations.
		</p>
		{#if $currentRoom}
			<div class="current-room-indicator">
				<span class="status-dot"></span>
				Connected to room: <strong>{$currentRoom.id}</strong>
			</div>
		{/if}
	</div>

	<div class="room-container">
		<Room />
	</div>
</div>

<style>
	.page-container {
		display: flex;
		flex-direction: column;
		height: 100vh;
		max-height: 100vh;
		padding: 1rem;
		gap: 1rem;
		box-sizing: border-box;
		background: #f8fafc;
		overflow: hidden;
	}

	.page-header {
		flex-shrink: 0;
		text-align: center;
		padding: 1rem 0;
	}

	.page-header h1 {
		margin: 0 0 0.5rem 0;
		color: #1f2937;
		font-size: 2rem;
		font-weight: 700;
	}

	.page-description {
		margin: 0 0 1rem 0;
		color: #6b7280;
		font-size: 1rem;
		max-width: 600px;
		margin-left: auto;
		margin-right: auto;
		line-height: 1.5;
	}

	.current-room-indicator {
		display: inline-flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.5rem 1rem;
		background: #dcfce7;
		color: #166534;
		border-radius: 6px;
		font-size: 0.875rem;
		border: 1px solid #bbf7d0;
	}

	.status-dot {
		width: 8px;
		height: 8px;
		background: #22c55e;
		border-radius: 50%;
		animation: pulse 2s infinite;
	}

	@keyframes pulse {
		0%,
		100% {
			opacity: 1;
		}
		50% {
			opacity: 0.5;
		}
	}

	.room-container {
		flex: 1;
		min-height: 0;
		max-width: 800px;
		margin: 0 auto;
		width: 100%;
		display: flex;
		flex-direction: column;
	}

	/* Mobile responsiveness */
	@media (max-width: 768px) {
		.page-container {
			padding: 0.5rem;
			gap: 0.5rem;
		}

		.page-header h1 {
			font-size: 1.5rem;
		}

		.page-description {
			font-size: 0.875rem;
			padding: 0 0.5rem;
		}

		.current-room-indicator {
			font-size: 0.75rem;
			padding: 0.375rem 0.75rem;
		}
	}

	@media (max-width: 480px) {
		.page-container {
			padding: 0.25rem;
		}

		.page-header {
			padding: 0.5rem 0;
		}

		.page-header h1 {
			font-size: 1.25rem;
		}
	}
</style>
