<script lang="ts">
	/**
	 * @component ConsensusResult
	 * Displays final consensus decision
	 * 
	 * @prop {string} originalProposal - Original proposal content
	 * @prop {string} finalDecision - The final decided version
	 * @prop {string} authorPub - Original author's public key
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {number} totalSupport - Total support points received
	 */
	
	import AuthorBadge from '../shared/AuthorBadge.svelte';
	
	let { originalProposal, finalDecision, authorPub, currentUserPub, totalSupport = undefined }: {
		originalProposal: string;
		finalDecision: string;
		authorPub: string;
		currentUserPub: string;
		totalSupport?: number;
	} = $props();
	
	const wasModified = $derived(originalProposal !== finalDecision);
</script>

<div class="consensus-result">
	<div class="header">
		<h3 class="title">
			<span class="icon">🏆</span>
			Final Decision
		</h3>
		<AuthorBadge {authorPub} {currentUserPub} />
	</div>
	
	<div class="decision-box">
		<div class="decision-text">{finalDecision}</div>
		{#if totalSupport !== undefined}
			<div class="support-info">
				<span class="icon">👍</span>
				{totalSupport} total support points
			</div>
		{/if}
	</div>
	
	{#if wasModified}
		<div class="original-section">
			<div class="original-label">Original Proposal:</div>
			<div class="original-text">{originalProposal}</div>
		</div>
	{/if}
</div>

<style>
	.consensus-result {
		background: linear-gradient(135deg, #f1f8f4 0%, #e8f5e9 100%);
		padding: 1.5rem;
		border-radius: 1rem;
		border: 2px solid #4caf50;
	}
	
	.header {
		display: flex;
		align-items: center;
		justify-content: space-between;
		flex-wrap: wrap;
		gap: 0.75rem;
		margin-bottom: 1rem;
	}
	
	.title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #2e7d32;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.5rem;
	}
	
	.decision-box {
		background: white;
		padding: 1.5rem;
		border-radius: 0.75rem;
		box-shadow: 0 2px 8px rgba(76, 175, 80, 0.15);
	}
	
	.decision-text {
		font-size: 1.125rem;
		line-height: 1.6;
		color: var(--text-primary, #333);
		font-weight: 500;
		margin-bottom: 0.75rem;
	}
	
	.support-info {
		display: flex;
		align-items: center;
		gap: 0.375rem;
		font-size: 0.875rem;
		color: #4caf50;
		font-weight: 600;
	}
	
	.support-info .icon {
		font-size: 1rem;
	}
	
	.original-section {
		margin-top: 1rem;
		padding: 0.875rem;
		background: rgba(255, 255, 255, 0.6);
		border-radius: 0.625rem;
		border: 1px dashed #c8e6c9;
	}
	
	.original-label {
		font-size: 0.8125rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: var(--text-muted, #666);
		margin-bottom: 0.5rem;
	}
	
	.original-text {
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-secondary, #555);
		font-style: italic;
	}
</style>

