<script lang="ts">
	/**
	 * @component QuickActions
	 * Floating quick action panel
	 * 
	 * @prop {string} currentPhase - Current phase
	 * @prop {number} pendingCount - Number of pending actions
	 * @prop {Function} onActionClick - Callback when action is clicked
	 */
	
	let { currentPhase, pendingCount = 0, onActionClick }: {
		currentPhase: string;
		pendingCount?: number;
		onActionClick?: () => void;
	} = $props();
	
	const phaseActions = {
		proposing: { label: 'Submit Proposal', icon: '📝', color: '#667eea' },
		challenging: { label: 'Review Proposals', icon: '⚠️', color: '#f44336' },
		commenting: { label: 'Add Feedback', icon: '💬', color: '#2196f3' },
		supporting: { label: 'Express Support', icon: '👍', color: '#4caf50' },
		complete: { label: 'View Results', icon: '🏆', color: '#4caf50' }
	};
	
	const action = $derived(phaseActions[currentPhase as keyof typeof phaseActions] || phaseActions.proposing);
</script>

<div class="quick-actions">
	<button 
		class="action-button" 
		style="--action-color: {action.color}"
		onclick={onActionClick}
	>
		<span class="icon">{action.icon}</span>
		<span class="label">{action.label}</span>
		{#if pendingCount > 0}
			<span class="count">{pendingCount}</span>
		{/if}
	</button>
</div>

<style>
	.quick-actions {
		position: sticky;
		bottom: 1rem;
		display: flex;
		justify-content: center;
		pointer-events: none;
		z-index: 100;
	}
	
	.action-button {
		pointer-events: auto;
		display: flex;
		align-items: center;
		gap: 0.75rem;
		background: var(--action-color, #667eea);
		color: white;
		border: none;
		padding: 1rem 1.75rem;
		border-radius: 2rem;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		box-shadow: 0 4px 20px rgba(0, 0, 0, 0.2);
		transition: all 0.3s;
		position: relative;
	}
	
	.action-button:hover {
		transform: translateY(-4px);
		box-shadow: 0 6px 28px rgba(0, 0, 0, 0.25);
	}
	
	.icon {
		font-size: 1.5rem;
	}
	
	.label {
		white-space: nowrap;
	}
	
	.count {
		background: white;
		color: var(--action-color, #667eea);
		font-size: 0.875rem;
		font-weight: 700;
		padding: 0.25rem 0.625rem;
		border-radius: 1rem;
		min-width: 1.75rem;
		text-align: center;
	}
	
	@media (max-width: 640px) {
		.label {
			display: none;
		}
		
		.action-button {
			padding: 1rem 1.25rem;
		}
	}
</style>

