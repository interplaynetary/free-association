<script lang="ts">
	/**
	 * @component ProposalCardExpanded
	 * Full detailed view of a proposal
	 * 
	 * @prop {object} proposal - Proposal data
	 * @prop {string} currentUserPub - Current user's public key
	 * @prop {string} currentPhase - Current phase
	 * @prop {Array} challenges - Challenges array
	 * @prop {Array} comments - Comments array
	 * @prop {Array} modifications - Modifications array
	 * @prop {Array} supportExpressions - Support expressions array
	 * @prop {string} consensusResult - Consensus result if complete
	 * @prop {Function} onChallenge - Callback for challenging
	 * @prop {Function} onComment - Callback for commenting
	 * @prop {Function} onModify - Callback for modifying
	 * @prop {Function} onSupport - Callback for supporting
	 */
	
	import AuthorBadge from '../shared/AuthorBadge.svelte';
	import ProposalStatusBadge from './ProposalStatusBadge.svelte';
	import ChallengeList from '../data/ChallengeList.svelte';
	import CommentThread from '../data/CommentThread.svelte';
	import ModificationCandidates from '../data/ModificationCandidates.svelte';
	import SupportSummary from '../data/SupportSummary.svelte';
	import ConsensusResult from '../data/ConsensusResult.svelte';
	
	let { 
		proposal,
		currentUserPub,
		currentPhase,
		challenges = [],
		comments = [],
		modifications = [],
		supportExpressions = [],
		consensusResult = undefined,
		onChallenge = undefined,
		onComment = undefined,
		onModify = undefined,
		onSupport = undefined
	}: {
		proposal: { content: string; authorPub: string };
		currentUserPub: string;
		currentPhase: string;
		challenges?: Array<any>;
		comments?: Array<any>;
		modifications?: Array<any>;
		supportExpressions?: Array<any>;
		consensusResult?: string;
		onChallenge?: () => void;
		onComment?: () => void;
		onModify?: () => void;
		onSupport?: () => void;
	} = $props();
	
	const status = $derived.by(() => {
		if (currentPhase === 'complete' && consensusResult) return 'complete';
		if (challenges.length === 0) return 'passed-no-challenges';
		if (challenges.length > 0 && modifications.length === 0 && currentPhase !== 'commenting') return 'passed-as-is';
		if (modifications.length > 0 && currentPhase === 'supporting') return 'awaiting-support';
		return 'in-process';
	});
	
	const candidates = $derived([proposal.content, ...modifications.map(m => m.content)]);
	const isOwnProposal = $derived(proposal.authorPub === currentUserPub);
</script>

<div class="proposal-card-expanded">
	<div class="header">
		<div class="title-row">
			<h3 class="title">Proposal</h3>
			<ProposalStatusBadge {status} />
		</div>
		<div class="proposal-content">
			<p class="content">{proposal.content}</p>
			<AuthorBadge authorPub={proposal.authorPub} {currentUserPub} />
		</div>
	</div>
	
	<!-- Show consensus result if complete -->
	{#if currentPhase === 'complete' && consensusResult}
		<ConsensusResult
			originalProposal={proposal.content}
			finalDecision={consensusResult}
			authorPub={proposal.authorPub}
			{currentUserPub}
		/>
	{/if}
	
	<!-- Challenges -->
	{#if currentPhase !== 'proposing'}
		<ChallengeList {challenges} {currentUserPub} />
	{/if}
	
	<!-- Comments -->
	{#if comments.length > 0}
		<CommentThread {comments} {currentUserPub} />
	{/if}
	
	<!-- Modifications -->
	{#if (currentPhase === 'commenting' || currentPhase === 'supporting' || currentPhase === 'complete') && challenges.length > 0}
		<ModificationCandidates 
			originalContent={proposal.content}
			{modifications}
			{currentUserPub}
		/>
	{/if}
	
	<!-- Support Summary -->
	{#if supportExpressions.length > 0 && modifications.length > 0}
		<SupportSummary 
			{candidates}
			{supportExpressions}
			winningCandidate={consensusResult}
		/>
	{/if}
	
	<!-- Action Buttons -->
	<div class="actions">
		{#if currentPhase === 'challenging' && !isOwnProposal && onChallenge}
			<button class="action-btn challenge" onclick={onChallenge}>
				<span class="icon">⚠️</span>
				Raise Challenge
			</button>
		{/if}
		
		{#if currentPhase === 'commenting' && challenges.length > 0}
			{#if onComment}
				<button class="action-btn comment" onclick={onComment}>
					<span class="icon">💬</span>
					Add Comment
				</button>
			{/if}
			{#if onModify}
				<button class="action-btn modify" onclick={onModify}>
					<span class="icon">✏️</span>
					Propose Modification
				</button>
			{/if}
		{/if}
		
		{#if currentPhase === 'supporting' && modifications.length > 0 && onSupport}
			<button class="action-btn support" onclick={onSupport}>
				<span class="icon">👍</span>
				Express Support
			</button>
		{/if}
	</div>
</div>

<style>
	.proposal-card-expanded {
		background: white;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 1rem;
		padding: 1.5rem;
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.header {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		padding-bottom: 1rem;
		border-bottom: 2px solid var(--bg-muted, #f0f0f0);
	}
	
	.title-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
		flex-wrap: wrap;
	}
	
	.title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: var(--text-primary, #333);
	}
	
	.proposal-content {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	.content {
		margin: 0;
		font-size: 1.125rem;
		line-height: 1.6;
		color: var(--text-primary, #333);
	}
	
	.actions {
		display: flex;
		gap: 0.75rem;
		flex-wrap: wrap;
		padding-top: 1rem;
		border-top: 2px solid var(--bg-muted, #f0f0f0);
	}
	
	.action-btn {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.75rem 1.25rem;
		border: none;
		border-radius: 0.625rem;
		font-size: 0.9375rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.icon {
		font-size: 1.125rem;
	}
	
	.action-btn.challenge {
		background: #f44336;
		color: white;
	}
	
	.action-btn.challenge:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(244, 67, 54, 0.3);
	}
	
	.action-btn.comment {
		background: #2196f3;
		color: white;
	}
	
	.action-btn.comment:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(33, 150, 243, 0.3);
	}
	
	.action-btn.modify {
		background: #ff9800;
		color: white;
	}
	
	.action-btn.modify:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(255, 152, 0, 0.3);
	}
	
	.action-btn.support {
		background: #4caf50;
		color: white;
	}
	
	.action-btn.support:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(76, 175, 80, 0.3);
	}
</style>

