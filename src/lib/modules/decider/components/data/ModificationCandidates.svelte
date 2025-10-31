<script lang="ts">
	/**
	 * @component ModificationCandidates
	 * Shows original proposal and all proposed modifications
	 * 
	 * @prop {string} originalContent - Original proposal content
	 * @prop {Array} modifications - Array of modification objects
	 * @prop {string} currentUserPub - Current user's public key
	 */
	
	import ContentCard from '../shared/ContentCard.svelte';
	import EmptyState from '../shared/EmptyState.svelte';
	
	let { originalContent, modifications, currentUserPub }: {
		originalContent: string;
		modifications: Array<{ content: string; authorPub: string; timestamp?: number }>;
		currentUserPub: string;
	} = $props();
</script>

<div class="modification-candidates">
	<h4 class="section-title">
		<span class="icon">✏️</span>
		Versions ({modifications.length + 1})
	</h4>
	
	<div class="items">
		<!-- Original version -->
		<div class="candidate original">
			<div class="candidate-label">
				<span class="label-text">Original</span>
			</div>
			<p class="content">{originalContent}</p>
		</div>
		
		<!-- Modified versions -->
		{#if modifications.length === 0}
			<EmptyState 
				message="No modifications proposed yet. If no one proposes improvements, the original will pass as-is."
				emoji="📋"
			/>
		{:else}
			{#each modifications as mod, i}
				<div class="candidate modified">
					<div class="candidate-label">
						<span class="label-text">Modified Version {i + 1}</span>
					</div>
					<ContentCard
						content={mod.content}
						authorPub={mod.authorPub}
						{currentUserPub}
						timestamp={mod.timestamp}
						type="modification"
					/>
				</div>
			{/each}
		{/if}
	</div>
</div>

<style>
	.modification-candidates {
		margin-top: 1rem;
		padding: 1rem;
		background: #fff8f0;
		border-radius: 0.75rem;
		border: 1px solid #ffecb3;
	}
	
	.section-title {
		margin: 0 0 0.875rem 0;
		font-size: 1rem;
		font-weight: 600;
		color: #ff9800;
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
		gap: 1rem;
	}
	
	.candidate {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.candidate-label {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.label-text {
		font-size: 0.8125rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: var(--text-muted, #666);
	}
	
	.candidate.original .label-text {
		color: #2196f3;
	}
	
	.candidate.modified .label-text {
		color: #ff9800;
	}
	
	.original .content {
		background: white;
		padding: 0.875rem;
		border-radius: 0.625rem;
		border-left: 3px solid #2196f3;
		margin: 0;
		line-height: 1.5;
	}
</style>

