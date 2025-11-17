<script lang="ts">
	/**
	 * @component AgendaNavigation
	 * Navigate between multiple agenda items with status indicators
	 */
	import type { AgendaItem } from '../../decider.svelte';
	import { normalizeAgendaItem } from '../../decider.svelte';
	
	interface Props {
		agenda: AgendaItem[];
		currentIndex: number;
		onNavigate?: (index: number) => void;
		completedIndices?: Set<number>;
	}
	
	let { 
		agenda,
		currentIndex,
		onNavigate,
		completedIndices = new Set()
	}: Props = $props();
	
	function handleSelect(index: number) {
		if (onNavigate && index !== currentIndex) {
			onNavigate(index);
		}
	}
</script>

<nav class="agenda-navigation">
	<div class="agenda-title">📋 Agenda</div>
	
	<div class="agenda-items">
		{#each agenda as item, index}
			{@const normalized = normalizeAgendaItem(item)}
			{@const isCurrent = index === currentIndex}
			{@const isCompleted = completedIndices.has(index)}
			
			<button
				class="agenda-item"
				class:current={isCurrent}
				class:completed={isCompleted}
				onclick={() => handleSelect(index)}
				disabled={isCurrent}
			>
				<div class="item-number">
					{#if isCompleted}
						<span class="check">✓</span>
					{:else}
						{index + 1}
					{/if}
				</div>
				
				<div class="item-content">
					<div class="item-text">{normalized.text}</div>
					{#if normalized.timeWindow}
						<div class="item-meta">
							⏱️ {Math.floor(normalized.timeWindow / 1000)}s
						</div>
					{/if}
				</div>
			</button>
		{/each}
	</div>
</nav>

<style>
	.agenda-navigation {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		padding: 1rem;
		background: var(--bg-light, #f8f9fa);
		border-radius: 0.75rem;
		border: 1px solid var(--border-light, #e5e7eb);
	}
	
	.agenda-title {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-secondary, #6b7280);
		text-transform: uppercase;
		letter-spacing: 0.05em;
	}
	
	.agenda-items {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.agenda-item {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		padding: 0.75rem;
		background: white;
		border: 2px solid transparent;
		border-radius: 0.5rem;
		cursor: pointer;
		transition: all 0.2s;
		text-align: left;
	}
	
	.agenda-item:hover:not(:disabled) {
		border-color: #667eea;
		transform: translateX(4px);
	}
	
	.agenda-item.current {
		border-color: #667eea;
		background: linear-gradient(135deg, rgba(102, 126, 234, 0.1) 0%, rgba(118, 75, 162, 0.1) 100%);
		cursor: default;
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.15);
	}
	
	.agenda-item.completed {
		background: rgba(16, 185, 129, 0.05);
		border-color: #10b981;
	}
	
	.item-number {
		flex-shrink: 0;
		width: 2rem;
		height: 2rem;
		display: flex;
		align-items: center;
		justify-content: center;
		background: #f3f4f6;
		border-radius: 0.375rem;
		font-weight: 600;
		font-size: 0.875rem;
		color: #6b7280;
	}
	
	.agenda-item.current .item-number {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
	}
	
	.agenda-item.completed .item-number {
		background: #10b981;
		color: white;
	}
	
	.check {
		font-size: 1rem;
	}
	
	.item-content {
		flex: 1;
		min-width: 0;
	}
	
	.item-text {
		font-size: 0.875rem;
		font-weight: 500;
		color: var(--text-primary, #1f2937);
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}
	
	.item-meta {
		font-size: 0.75rem;
		color: var(--text-secondary, #6b7280);
		margin-top: 0.25rem;
	}
	
	@media (max-width: 640px) {
		.agenda-navigation {
			padding: 0.75rem;
		}
		
		.agenda-item {
			padding: 0.5rem;
		}
		
		.item-number {
			width: 1.75rem;
			height: 1.75rem;
			font-size: 0.75rem;
		}
	}
</style>

