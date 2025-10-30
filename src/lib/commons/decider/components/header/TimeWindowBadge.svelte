<script lang="ts">
	/**
	 * @component TimeWindowBadge
	 * Displays time remaining in current phase
	 * 
	 * @prop {number} phaseStartTime - When the phase started (timestamp)
	 * @prop {number} timeWindow - Duration of phase in milliseconds
	 */
	
	import { onMount } from 'svelte';
	
	let { phaseStartTime, timeWindow }: {
		phaseStartTime: number;
		timeWindow: number;
	} = $props();
	
	let now = $state(Date.now());
	
	const timeRemaining = $derived(Math.max(0, (phaseStartTime + timeWindow) - now));
	const totalTime = $derived(timeWindow);
	const percentRemaining = $derived((timeRemaining / totalTime) * 100);
	
	const urgency = $derived.by(() => {
		if (percentRemaining > 50) return 'plenty';
		if (percentRemaining > 20) return 'moderate';
		return 'urgent';
	});
	
	function formatTimeRemaining(ms: number): string {
		if (ms === 0) return 'Ended';
		
		const hours = Math.floor(ms / 3600000);
		const minutes = Math.floor((ms % 3600000) / 60000);
		
		if (hours >= 24) {
			const days = Math.floor(hours / 24);
			return `${days}d ${hours % 24}h`;
		}
		if (hours > 0) return `${hours}h ${minutes}m`;
		return `${minutes}m`;
	}
	
	const displayTime = $derived(formatTimeRemaining(timeRemaining));
	
	onMount(() => {
		const interval = setInterval(() => {
			now = Date.now();
		}, 60000); // Update every minute
		
		return () => clearInterval(interval);
	});
</script>

<div class="time-badge" class:plenty={urgency === 'plenty'} class:moderate={urgency === 'moderate'} class:urgent={urgency === 'urgent'}>
	<span class="icon">⏱️</span>
	<span class="time">{displayTime}</span>
</div>

<style>
	.time-badge {
		display: inline-flex;
		align-items: center;
		gap: 0.375rem;
		padding: 0.375rem 0.875rem;
		border-radius: 1rem;
		font-size: 0.875rem;
		font-weight: 600;
		transition: all 0.3s;
	}
	
	.time-badge.plenty {
		background: #e8f5e9;
		color: #2e7d32;
	}
	
	.time-badge.moderate {
		background: #fff9c4;
		color: #f57f17;
	}
	
	.time-badge.urgent {
		background: #ffebee;
		color: #c62828;
		animation: pulse 2s ease-in-out infinite;
	}
	
	@keyframes pulse {
		0%, 100% { opacity: 1; }
		50% { opacity: 0.7; }
	}
	
	.icon {
		font-size: 1rem;
	}
	
	.time {
		white-space: nowrap;
	}
</style>

