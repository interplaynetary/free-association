<script lang="ts">
	/**
	 * @component DeciderHeader
	 * Compact header showing essential Decider information
	 */
	
	import PhaseIndicator from './PhaseIndicator.svelte';
	import TimeWindowBadge from './TimeWindowBadge.svelte';
	import ParticipantAvatars from './ParticipantAvatars.svelte';
	
	let { 
		agendaItem,
		currentPhase,
		phaseStartTime,
		timeWindow,
		participants,
		currentUserPub,
		compact = false,
		onExpand = undefined
	}: {
		agendaItem: string;
		currentPhase: string;
		phaseStartTime: number;
		timeWindow: number;
		participants: string[];
		currentUserPub: string;
		compact?: boolean;
		onExpand?: () => void;
	} = $props();
	
	const phaseEmojis: Record<string, string> = {
		proposing: '📝',
		challenging: '⚠️',
		commenting: '💬',
		supporting: '👍',
		complete: '🏆'
	};
	
	const phaseEmoji = $derived(phaseEmojis[currentPhase] || '⏳');
</script>

<header class="decider-header" class:compact class:clickable={!!onExpand} onclick={onExpand}>
	<div class="main-info">
		<div class="phase-badge">
			<span class="emoji">{phaseEmoji}</span>
			<span class="phase-name">{currentPhase.toUpperCase()}</span>
		</div>
		<h3 class="agenda-item">{agendaItem}</h3>
	</div>
	
	<div class="meta-info">
		<TimeWindowBadge {phaseStartTime} {timeWindow} />
		<ParticipantAvatars {participants} {currentUserPub} compact={compact} />
	</div>
	
	{#if !compact}
		<div class="progress-section">
			<PhaseIndicator {currentPhase} compact={false} />
		</div>
	{/if}
</header>

<style>
	.decider-header {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		padding: 1.5rem;
		border-radius: 1rem;
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.decider-header.compact {
		padding: 1rem;
	}
	
	.decider-header.clickable {
		cursor: pointer;
		transition: transform 0.2s, box-shadow 0.2s;
	}
	
	.decider-header.clickable:hover {
		transform: translateY(-2px);
		box-shadow: 0 6px 24px rgba(102, 126, 234, 0.3);
	}
	
	.main-info {
		display: flex;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
	}
	
	.phase-badge {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		background: rgba(255, 255, 255, 0.2);
		padding: 0.5rem 1rem;
		border-radius: 0.75rem;
		backdrop-filter: blur(10px);
	}
	
	.emoji {
		font-size: 1.25rem;
	}
	
	.phase-name {
		font-size: 0.875rem;
		font-weight: 700;
		letter-spacing: 0.5px;
	}
	
	.agenda-item {
		margin: 0;
		font-size: 1.125rem;
		font-weight: 600;
		flex: 1;
		min-width: 0;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
	}
	
	.compact .agenda-item {
		font-size: 1rem;
	}
	
	.meta-info {
		display: flex;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
	}
	
	.progress-section {
		padding-top: 0.5rem;
		border-top: 1px solid rgba(255, 255, 255, 0.2);
	}
	
	@media (max-width: 640px) {
		.decider-header {
			padding: 1rem;
		}
		
		.main-info {
			flex-direction: column;
			align-items: flex-start;
		}
		
		.agenda-item {
			white-space: normal;
		}
	}
</style>

