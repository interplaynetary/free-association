<script lang="ts">
	/**
	 * @component ParticipantAvatars
	 * Displays participant list
	 * 
	 * @prop {string[]} participants - Array of participant public keys
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {boolean} compact - Show compact stacked version
	 */
	
	let { participants, currentUserPub, compact = false }: {
		participants: string[];
		currentUserPub: string;
		compact?: boolean;
	} = $props();
	
	const participantList = $derived(participants.map(pub => ({
		pub,
		isCurrentUser: pub === currentUserPub,
		shortName: pub === currentUserPub ? 'You' : pub.slice(0, 8)
	})));
</script>

<div class="participant-avatars" class:compact>
	{#if compact}
		<div class="stacked-avatars">
			{#each participantList.slice(0, 4) as participant}
				<div 
					class="avatar" 
					class:is-you={participant.isCurrentUser}
					title={participant.isCurrentUser ? 'You' : participant.pub}
				>
					{participant.shortName.slice(0, 2).toUpperCase()}
				</div>
			{/each}
			{#if participantList.length > 4}
				<div class="avatar more">+{participantList.length - 4}</div>
			{/if}
		</div>
		<span class="count">{participants.length}</span>
	{:else}
		<div class="participant-list">
			{#each participantList as participant}
				<div class="participant-badge" class:is-you={participant.isCurrentUser}>
					{participant.isCurrentUser ? 'You' : participant.shortName}...
				</div>
			{/each}
		</div>
	{/if}
</div>

<style>
	.participant-avatars {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	/* Compact stacked version */
	.stacked-avatars {
		display: flex;
		align-items: center;
	}
	
	.avatar {
		width: 2rem;
		height: 2rem;
		border-radius: 50%;
		background: var(--avatar-bg, #e3f2fd);
		color: var(--avatar-color, #1976d2);
		display: flex;
		align-items: center;
		justify-content: center;
		font-size: 0.75rem;
		font-weight: 600;
		border: 2px solid white;
		margin-left: -0.5rem;
		transition: transform 0.2s;
	}
	
	.avatar:first-child {
		margin-left: 0;
	}
	
	.avatar:hover {
		transform: translateY(-2px);
		z-index: 1;
	}
	
	.avatar.is-you {
		background: var(--primary-color, #667eea);
		color: white;
	}
	
	.avatar.more {
		background: var(--bg-muted, #f0f0f0);
		color: var(--text-muted, #666);
		font-size: 0.625rem;
	}
	
	.count {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-muted, #666);
	}
	
	/* Full list version */
	.participant-list {
		display: flex;
		flex-wrap: wrap;
		gap: 0.5rem;
	}
	
	.participant-badge {
		background: var(--badge-bg, #e3f2fd);
		color: var(--badge-color, #1976d2);
		padding: 0.375rem 0.875rem;
		border-radius: 1rem;
		font-size: 0.875rem;
		font-weight: 600;
	}
	
	.participant-badge.is-you {
		background: var(--primary-color, #667eea);
		color: white;
	}
</style>

