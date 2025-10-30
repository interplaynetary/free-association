<script lang="ts">
	/**
	 * @component DeciderTabs
	 * Tab navigation for different views
	 * 
	 * @prop {string} activeTab - Currently active tab
	 * @prop {Function} onTabChange - Callback when tab changes
	 * @prop {number} actionCount - Number of pending actions
	 */
	
	interface Props {
		activeTab?: string;
		onTabChange?: (tab: string) => void;
		actionCount?: number;
	}
	
	let { activeTab = $bindable('proposals'), onTabChange, actionCount = 0 }: Props = $props();
	
	const tabs = [
		{ id: 'proposals', label: 'Proposals', icon: '🎯' },
		{ id: 'my-actions', label: 'My Actions', icon: '✓' },
		{ id: 'activity', label: 'Activity', icon: '📊' }
	];
	
	function handleTabClick(tabId: string) {
		activeTab = tabId;
		onTabChange?.(tabId);
	}
</script>

<div class="decider-tabs">
	{#each tabs as tab}
		<button 
			class="tab"
			class:active={activeTab === tab.id}
			onclick={() => handleTabClick(tab.id)}
		>
			<span class="icon">{tab.icon}</span>
			<span class="label">{tab.label}</span>
			{#if tab.id === 'my-actions' && actionCount > 0}
				<span class="badge">{actionCount}</span>
			{/if}
		</button>
	{/each}
</div>

<style>
	.decider-tabs {
		display: flex;
		background: white;
		border-radius: 0.75rem;
		padding: 0.375rem;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
		gap: 0.375rem;
	}
	
	.tab {
		flex: 1;
		display: flex;
		align-items: center;
		justify-content: center;
		gap: 0.5rem;
		padding: 0.75rem 1rem;
		border: none;
		background: transparent;
		border-radius: 0.5rem;
		cursor: pointer;
		transition: all 0.2s;
		font-weight: 600;
		font-size: 0.9375rem;
		color: var(--text-muted, #666);
		position: relative;
	}
	
	.tab:hover {
		background: var(--bg-muted, #f5f5f5);
		color: var(--text-primary, #333);
	}
	
	.tab.active {
		background: var(--primary-color, #667eea);
		color: white;
	}
	
	.icon {
		font-size: 1.125rem;
	}
	
	.label {
		white-space: nowrap;
	}
	
	.badge {
		position: absolute;
		top: 0.25rem;
		right: 0.25rem;
		background: #f44336;
		color: white;
		font-size: 0.6875rem;
		font-weight: 700;
		padding: 0.125rem 0.375rem;
		border-radius: 0.625rem;
		min-width: 1.25rem;
		text-align: center;
	}
	
	@media (max-width: 640px) {
		.label {
			display: none;
		}
		
		.tab {
			padding: 0.75rem;
		}
	}
</style>

