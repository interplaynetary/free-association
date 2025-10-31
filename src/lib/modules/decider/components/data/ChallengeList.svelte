<script lang="ts">
	/**
	 * @component ChallengeList
	 * Displays list of challenges for a proposal
	 * 
	 * @prop {Array} challenges - Array of challenge objects with content and authorPub
	 * @prop {string} currentUserPub - Current user's public key
	 */
	
	import ContentCard from '../shared/ContentCard.svelte';
	import EmptyState from '../shared/EmptyState.svelte';
	
	let { challenges, currentUserPub }: {
		challenges: Array<{ content: string; authorPub: string; timestamp?: number }>;
		currentUserPub: string;
	} = $props();
</script>

<div class="challenge-list">
	<h4 class="section-title">
		<span class="icon">⚠️</span>
		Challenges ({challenges.length})
	</h4>
	
	{#if challenges.length === 0}
		<EmptyState 
			message="No challenges raised yet. This proposal may pass early if no concerns are raised."
			emoji="✅"
		/>
	{:else}
		<div class="items">
			{#each challenges as challenge}
				<ContentCard
					content={challenge.content}
					authorPub={challenge.authorPub}
					{currentUserPub}
					timestamp={challenge.timestamp}
					type="challenge"
				/>
			{/each}
		</div>
	{/if}
</div>

<style>
	.challenge-list {
		margin-top: 1rem;
		padding: 1rem;
		background: #fff9f8;
		border-radius: 0.75rem;
		border: 1px solid #ffebee;
	}
	
	.section-title {
		margin: 0 0 0.875rem 0;
		font-size: 1rem;
		font-weight: 600;
		color: #f44336;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.125rem;
	}
	
	.items {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
</style>

