<script lang="ts">
	/**
	 * @component ProposalCardMini
	 * Collapsed view of a proposal with essential info
	 * 
	 * @prop {object} proposal - Proposal object
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {number} challengeCount - Number of challenges
	 * @prop {number} commentCount - Number of comments
	 * @prop {number} modificationCount - Number of modifications
	 * @prop {string} status - Proposal status
	 * @prop {Function} onExpand - Callback when card is clicked
	 */
	
	import AuthorBadge from '../shared/AuthorBadge.svelte';
	import ProposalStatusBadge from './ProposalStatusBadge.svelte';
	
	let { 
		proposal,
		currentUserPub,
		challengeCount = 0,
		commentCount = 0,
		modificationCount = 0,
		status,
		onExpand
	}: {
		proposal: { content: string; authorPub: string };
		currentUserPub: string;
		challengeCount?: number;
		commentCount?: number;
		modificationCount?: number;
		status: 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete';
		onExpand: () => void;
	} = $props();
	
	const hasActivity = $derived(challengeCount + commentCount + modificationCount > 0);
	const truncatedContent = $derived(
		proposal.content.length > 120 
			? proposal.content.slice(0, 120) + '...' 
			: proposal.content
	);
</script>

<button class="proposal-card-mini" class:has-activity={hasActivity} onclick={onExpand}>
	<div class="header">
		<h4 class="content">{truncatedContent}</h4>
		<ProposalStatusBadge {status} />
	</div>
	
	<div class="meta">
		<AuthorBadge authorPub={proposal.authorPub} {currentUserPub} compact />
		
		{#if hasActivity}
			<div class="stats">
				{#if challengeCount > 0}
					<span class="stat challenges" title="Challenges">
						⚠️ {challengeCount}
					</span>
				{/if}
				{#if commentCount > 0}
					<span class="stat comments" title="Comments">
						💬 {commentCount}
					</span>
				{/if}
				{#if modificationCount > 0}
					<span class="stat modifications" title="Modifications">
						✏️ {modificationCount}
					</span>
				{/if}
			</div>
		{/if}
	</div>
</button>

<style>
	.proposal-card-mini {
		background: white;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.875rem;
		padding: 1rem;
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		cursor: pointer;
		transition: all 0.3s;
		text-align: left;
		width: 100%;
	}
	
	.proposal-card-mini:hover {
		border-color: var(--primary-color, #667eea);
		box-shadow: 0 4px 16px rgba(102, 126, 234, 0.15);
		transform: translateY(-2px);
	}
	
	.proposal-card-mini.has-activity {
		border-color: #ffa726;
	}
	
	.proposal-card-mini.has-activity:hover {
		border-color: #ff9800;
	}
	
	.header {
		display: flex;
		flex-direction: column;
		gap: 0.625rem;
	}
	
	.content {
		margin: 0;
		font-size: 1rem;
		font-weight: 500;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.meta {
		display: flex;
		align-items: center;
		justify-content: space-between;
		gap: 0.75rem;
		flex-wrap: wrap;
	}
	
	.stats {
		display: flex;
		gap: 0.625rem;
		align-items: center;
	}
	
	.stat {
		display: inline-flex;
		align-items: center;
		gap: 0.25rem;
		font-size: 0.8125rem;
		font-weight: 600;
		padding: 0.25rem 0.625rem;
		border-radius: 0.75rem;
	}
	
	.stat.challenges {
		background: #ffebee;
		color: #c62828;
	}
	
	.stat.comments {
		background: #e3f2fd;
		color: #1565c0;
	}
	
	.stat.modifications {
		background: #fff3e0;
		color: #e65100;
	}
</style>

