<script lang="ts">
	/**
	 * @component EmptyState
	 * Displays placeholder when there's no data
	 * 
	 * @prop {string} message - Message to display
	 * @prop {string} emoji - Optional emoji icon
	 * @prop {string} actionLabel - Optional action button label
	 * @prop {Function} onAction - Optional action handler
	 */
	
	let { 
		message, 
		emoji = '📭', 
		actionLabel = undefined,
		onAction = undefined 
	}: {
		message: string;
		emoji?: string;
		actionLabel?: string;
		onAction?: () => void;
	} = $props();
</script>

<div class="empty-state">
	<div class="emoji">{emoji}</div>
	<p class="message">{message}</p>
	{#if actionLabel && onAction}
		<button class="action-btn" onclick={onAction}>
			{actionLabel}
		</button>
	{/if}
</div>

<style>
	.empty-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		padding: 3rem 1.5rem;
		text-align: center;
		color: var(--text-muted, #666);
	}
	
	.emoji {
		font-size: 3rem;
		margin-bottom: 1rem;
		opacity: 0.6;
	}
	
	.message {
		margin: 0 0 1.5rem 0;
		font-size: 1rem;
		line-height: 1.5;
		max-width: 24rem;
	}
	
	.action-btn {
		background: var(--primary-color, #667eea);
		color: white;
		border: none;
		padding: 0.625rem 1.25rem;
		border-radius: 0.5rem;
		font-weight: 600;
		cursor: pointer;
		transition: transform 0.2s, box-shadow 0.2s;
	}
	
	.action-btn:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
</style>

