<script lang="ts">
	/**
	 * @component PhaseIndicator
	 * Visual progress indicator through Decider phases
	 * 
	 * @prop {string} currentPhase - Current phase name
	 * @prop {boolean} compact - Show compact version with dots
	 */
	
	let { currentPhase, compact = false }: {
		currentPhase: string;
		compact?: boolean;
	} = $props();
	
	const phases = [
		{ name: 'proposing', label: 'Propose', emoji: '📝' },
		{ name: 'challenging', label: 'Challenge', emoji: '⚠️' },
		{ name: 'commenting', label: 'Discuss', emoji: '💬' },
		{ name: 'supporting', label: 'Support', emoji: '👍' },
		{ name: 'complete', label: 'Complete', emoji: '🏆' }
	];
	
	const currentIndex = $derived(phases.findIndex(p => p.name === currentPhase));
	
	function getPhaseState(index: number): 'complete' | 'active' | 'upcoming' {
		if (index < currentIndex) return 'complete';
		if (index === currentIndex) return 'active';
		return 'upcoming';
	}
</script>

<div class="phase-indicator" class:compact>
	{#if compact}
		<div class="dots">
			{#each phases as phase, i}
				<div 
					class="dot" 
					class:complete={getPhaseState(i) === 'complete'}
					class:active={getPhaseState(i) === 'active'}
					title={phase.label}
				></div>
			{/each}
		</div>
	{:else}
		<div class="steps">
			{#each phases as phase, i}
				{#if i > 0}
					<div class="arrow">→</div>
				{/if}
				<div 
					class="step" 
					class:complete={getPhaseState(i) === 'complete'}
					class:active={getPhaseState(i) === 'active'}
				>
					<div class="step-icon">{phase.emoji}</div>
					<div class="step-label">{phase.label}</div>
				</div>
			{/each}
		</div>
	{/if}
</div>

<style>
	.phase-indicator {
		display: flex;
		align-items: center;
	}
	
	/* Compact dot version */
	.dots {
		display: flex;
		gap: 0.5rem;
		align-items: center;
	}
	
	.dot {
		width: 0.625rem;
		height: 0.625rem;
		border-radius: 50%;
		background: var(--bg-muted, #ddd);
		transition: all 0.3s;
	}
	
	.dot.active {
		background: var(--primary-color, #667eea);
		transform: scale(1.4);
		box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.2);
	}
	
	.dot.complete {
		background: var(--success-color, #4caf50);
	}
	
	/* Full step version */
	.steps {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.step {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 0.375rem;
		padding: 0.625rem;
		border-radius: 0.75rem;
		background: var(--bg-muted, #f5f5f5);
		min-width: 4.5rem;
		transition: all 0.3s;
	}
	
	.step.active {
		background: var(--primary-color, #667eea);
		color: white;
		transform: scale(1.05);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.step.complete {
		background: var(--success-color, #4caf50);
		color: white;
	}
	
	.step-icon {
		font-size: 1.5rem;
	}
	
	.step-label {
		font-size: 0.8125rem;
		font-weight: 600;
		text-align: center;
	}
	
	.arrow {
		font-size: 1.25rem;
		color: var(--text-muted, #ccc);
		flex-shrink: 0;
	}
	
	@media (max-width: 640px) {
		.steps {
			justify-content: center;
		}
		
		.arrow {
			display: none;
		}
	}
</style>

