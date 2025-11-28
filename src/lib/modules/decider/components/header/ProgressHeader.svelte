<script lang="ts">
	/**
	 * @component ProgressHeader
	 * Elegant sticky progress indicator showing phase progression
	 * and contextual information
	 */
	
	import { onMount, onDestroy } from 'svelte';
	
	interface Props {
		currentPhase: string;
		phaseStartTime?: number;
		phaseDuration?: number;
		agendaItem?: string;
		agendaIndex?: number;
		agendaTotal?: number;
		compact?: boolean;
	}
	
	let {
		currentPhase,
		phaseStartTime,
		phaseDuration,
		agendaItem,
		agendaIndex,
		agendaTotal,
		compact = false
	}: Props = $props();
	
	const phases = ['proposing', 'challenging', 'commenting', 'supporting', 'complete'];
	
	const phaseLabels: Record<string, string> = {
		proposing: 'Propose',
		challenging: 'Challenge',
		commenting: 'Comment',
		supporting: 'Support',
		complete: 'Complete'
	};
	
	const phaseIcons: Record<string, string> = {
		proposing: '📝',
		challenging: '⚠️',
		commenting: '💬',
		supporting: '👍',
		complete: '🏆'
	};
	
	const nextPhase = $derived(() => {
		const currentIndex = phases.indexOf(currentPhase);
		return currentIndex >= 0 && currentIndex < phases.length - 1 
			? phaseLabels[phases[currentIndex + 1]]
			: null;
	});
	
	// Time formatting - uses onMount instead of $effect
	let timeRemaining = $state<string>('');
	let timerInterval: ReturnType<typeof setInterval> | null = null;
	
	function updateTimer() {
		if (!phaseStartTime || !phaseDuration) {
			timeRemaining = '';
			return;
		}
		
		const now = Date.now();
		const elapsed = now - phaseStartTime;
		const remaining = Math.max(0, phaseDuration - elapsed);
		
		if (remaining === 0) {
			timeRemaining = 'Time up';
			return;
		}
		
		const minutes = Math.floor(remaining / 60000);
		const seconds = Math.floor((remaining % 60000) / 1000);
		timeRemaining = `${minutes}:${seconds.toString().padStart(2, '0')}`;
	}
	
	onMount(() => {
		updateTimer();
		timerInterval = setInterval(updateTimer, 1000);
	});
	
	onDestroy(() => {
		if (timerInterval) {
			clearInterval(timerInterval);
		}
	});
	
	function getPhaseState(phase: string): 'complete' | 'active' | 'pending' {
		const currentIndex = phases.indexOf(currentPhase);
		const phaseIndex = phases.indexOf(phase);
		
		if (phaseIndex < currentIndex) return 'complete';
		if (phaseIndex === currentIndex) return 'active';
		return 'pending';
	}
</script>

<header class="progress-header" class:compact>
	<nav class="phase-breadcrumb" role="navigation" aria-label="Progress through phases">
		{#each phases as phase}
			{@const state = getPhaseState(phase)}
			<div class="phase-step" class:complete={state === 'complete'} class:active={state === 'active'} class:pending={state === 'pending'}>
				{#if !compact}
					<span class="icon" role="img" aria-label={phaseLabels[phase]}>
						{phaseIcons[phase]}
					</span>
				{/if}
				<span class="label">{phaseLabels[phase]}</span>
				{#if state === 'complete'}
					<span class="check">✓</span>
				{/if}
			</div>
			{#if phase !== 'complete'}
				<div class="connector" class:complete={state === 'complete'}></div>
			{/if}
		{/each}
	</nav>
	
	{#if !compact}
		<div class="context-info">
			{#if timeRemaining && phaseDuration}
				<div class="time-info">
					<span class="timer">⏱️ {timeRemaining}</span>
					{#if nextPhase()}
						<span class="next-hint">Next: {nextPhase()}</span>
					{/if}
				</div>
			{/if}
			
			{#if agendaItem && agendaTotal && agendaTotal > 1}
				<div class="agenda-info">
					<span class="agenda-label">Item {(agendaIndex ?? 0) + 1} of {agendaTotal}:</span>
					<span class="agenda-text">{agendaItem}</span>
				</div>
			{/if}
		</div>
	{/if}
</header>

<style>
	.progress-header {
		position: sticky;
		top: 0;
		z-index: 100;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		padding: 1rem 1.5rem;
		border-radius: 0 0 1rem 1rem;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.progress-header.compact {
		padding: 0.75rem 1rem;
		gap: 0.5rem;
	}
	
	/* Phase Breadcrumb */
	.phase-breadcrumb {
		display: flex;
		align-items: center;
		justify-content: space-between;
		gap: 0.5rem;
	}
	
	.phase-step {
		display: flex;
		align-items: center;
		gap: 0.375rem;
		padding: 0.5rem 0.875rem;
		border-radius: 1.5rem;
		background: rgba(255, 255, 255, 0.1);
		transition: all 0.3s ease;
		font-size: 0.875rem;
		font-weight: 500;
	}
	
	.phase-step.complete {
		background: rgba(255, 255, 255, 0.25);
	}
	
	.phase-step.active {
		background: white;
		color: #667eea;
		font-weight: 600;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.2);
	}
	
	.phase-step.pending {
		opacity: 0.6;
	}
	
	.icon {
		font-size: 1rem;
	}
	
	.label {
		white-space: nowrap;
	}
	
	.check {
		font-size: 0.75rem;
		margin-left: 0.125rem;
	}
	
	.connector {
		flex: 1;
		height: 2px;
		background: rgba(255, 255, 255, 0.2);
		position: relative;
		transition: background 0.3s ease;
	}
	
	.connector.complete {
		background: rgba(255, 255, 255, 0.5);
	}
	
	/* Context Info */
	.context-info {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
		font-size: 0.875rem;
	}
	
	.time-info {
		display: flex;
		align-items: center;
		gap: 0.75rem;
	}
	
	.timer {
		font-weight: 600;
		font-variant-numeric: tabular-nums;
	}
	
	.next-hint {
		opacity: 0.9;
		font-size: 0.8125rem;
	}
	
	.agenda-info {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		opacity: 0.95;
	}
	
	.agenda-label {
		font-weight: 600;
		font-size: 0.75rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}
	
	.agenda-text {
		font-weight: 500;
	}
	
	/* Responsive */
	@media (max-width: 640px) {
		.phase-step .label {
			display: none;
		}
		
		.phase-step.active .label {
			display: inline;
		}
		
		.context-info {
			flex-direction: column;
			align-items: flex-start;
			gap: 0.5rem;
		}
		
		.next-hint {
			display: none;
		}
	}
</style>

