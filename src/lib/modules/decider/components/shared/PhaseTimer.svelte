<script lang="ts">
	/**
	 * @component PhaseTimer
	 * Displays a countdown timer for the current phase with visual progress
	 */
	import { onMount, onDestroy } from 'svelte';
	import { getTimeState, type TimeState } from '../../utils/time';
	import { INTERVALS } from '../../constants';
	
	interface Props {
		phaseStartTime: number;
		phaseDuration: number;
		currentPhase: string;
		compact?: boolean;
	}
	
	let { 
		phaseStartTime,
		phaseDuration,
		currentPhase,
		compact = false
	}: Props = $props();
	
	let timeState = $state<TimeState>(getTimeState(phaseStartTime, phaseDuration));
	let intervalId: ReturnType<typeof setInterval> | null = null;
	
	function updateTimer() {
		timeState = getTimeState(phaseStartTime, phaseDuration);
		
		if (timeState.isExpired && intervalId) {
			clearInterval(intervalId);
		}
	}
	
	onMount(() => {
		updateTimer();
		intervalId = setInterval(updateTimer, INTERVALS.TIMER);
	});
	
	onDestroy(() => {
		if (intervalId) clearInterval(intervalId);
	});
</script>

<div class="phase-timer" class:compact class:urgent={timeState.isUrgent} class:expired={timeState.isExpired}>
	{#if !compact}
		<div class="timer-label">
			<span class="phase-name">{currentPhase}</span>
			<span class="time-text">{timeState.formatted}</span>
		</div>
	{/if}
	
	<div class="progress-container">
		<div class="progress-bar" style="width: {timeState.progress}%"></div>
	</div>
	
	{#if compact}
		<div class="compact-time">{timeState.formatted}</div>
	{/if}
</div>

<style>
	.phase-timer {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		padding: 0.75rem;
		background: rgba(255, 255, 255, 0.05);
		border-radius: 0.5rem;
		border: 1px solid rgba(255, 255, 255, 0.1);
	}
	
	.phase-timer.compact {
		padding: 0.5rem;
		gap: 0.25rem;
	}
	
	.timer-label {
		display: flex;
		justify-content: space-between;
		align-items: center;
		font-size: 0.875rem;
	}
	
	.phase-name {
		font-weight: 600;
		text-transform: capitalize;
		opacity: 0.9;
	}
	
	.time-text {
		font-variant-numeric: tabular-nums;
		font-weight: 500;
		opacity: 0.8;
	}
	
	.progress-container {
		height: 4px;
		background: rgba(255, 255, 255, 0.1);
		border-radius: 2px;
		overflow: hidden;
	}
	
	.progress-bar {
		height: 100%;
		background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
		transition: width 0.1s linear;
		border-radius: 2px;
	}
	
	.compact-time {
		text-align: center;
		font-size: 0.75rem;
		font-variant-numeric: tabular-nums;
		opacity: 0.8;
	}
	
	/* Urgent state */
	.phase-timer.urgent {
		border-color: #f59e0b;
		animation: pulse 1s ease-in-out infinite;
	}
	
	.phase-timer.urgent .progress-bar {
		background: linear-gradient(90deg, #f59e0b 0%, #ef4444 100%);
	}
	
	/* Expired state */
	.phase-timer.expired {
		border-color: #ef4444;
		opacity: 0.7;
	}
	
	.phase-timer.expired .progress-bar {
		background: #ef4444;
	}
	
	@keyframes pulse {
		0%, 100% {
			box-shadow: 0 0 0 0 rgba(245, 158, 11, 0.4);
		}
		50% {
			box-shadow: 0 0 0 4px rgba(245, 158, 11, 0);
		}
	}
</style>

