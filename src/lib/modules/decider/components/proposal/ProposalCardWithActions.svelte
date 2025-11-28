<script lang="ts">
	/**
	 * @component ProposalCardWithActions
	 * Proposal card with inline challenge/comment actions
	 * Elegant alternative to modal workflow
	 */
	
	import { slide } from 'svelte/transition';
	import { useDraft } from '../../composables/useDraft.svelte';
	import AuthorBadge from '../shared/AuthorBadge.svelte';
	import ProposalStatusBadge from './ProposalStatusBadge.svelte';
	
	interface Props {
		proposal: { content: string; authorPub: string };
		currentUserPub: string;
		challengeCount?: number;
		commentCount?: number;
		modificationCount?: number;
		status: 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete';
		currentPhase: string;
		gameId: string;
		onChallenge?: (content: string) => Promise<void>;
		onComment?: (content: string) => Promise<void>;
		onModification?: (content: string) => Promise<void>;
		onAgree?: () => Promise<void>;
		onExpand?: () => void;
	}
	
	let {
		proposal,
		currentUserPub,
		challengeCount = 0,
		commentCount = 0,
		modificationCount = 0,
		status,
		currentPhase,
		gameId,
		onChallenge,
		onComment,
		onModification,
		onAgree,
		onExpand
	}: Props = $props();
	
	// State for inline actions
	let showChallengeInput = $state(false);
	let showCommentInput = $state(false);
	let showModificationInput = $state(false);
	let isSubmitting = $state(false);
	
	// Elegant draft management for each action type
	const challengeDraft = useDraft({ 
		key: `${gameId}:challenge:${proposal.authorPub}`,
		autosave: showChallengeInput
	});
	
	const commentDraft = useDraft({ 
		key: `${gameId}:comment:${proposal.authorPub}`,
		autosave: showCommentInput
	});
	
	const modificationDraft = useDraft({ 
		key: `${gameId}:modification:${proposal.authorPub}`,
		autosave: showModificationInput
	});
	
	const hasActivity = $derived(challengeCount + commentCount + modificationCount > 0);
	const isMyProposal = $derived(proposal.authorPub === currentUserPub);
	
	// Quick challenge templates
	const challengeTemplates = [
		{ label: 'Not clear', text: 'This proposal needs more clarity. Could you elaborate on...' },
		{ label: 'Too costly', text: 'The cost seems too high. Have we considered...' },
		{ label: 'Timeline', text: 'The timeline might be too aggressive. What if we...' }
	];
	
	async function handleAgree() {
		if (!onAgree || isSubmitting) return;
		isSubmitting = true;
		try {
			await onAgree();
		} finally {
			isSubmitting = false;
		}
	}
	
	async function handleChallengeSubmit() {
		if (!onChallenge || !challengeDraft.content.trim() || isSubmitting) return;
		isSubmitting = true;
		try {
			await onChallenge(challengeDraft.content.trim());
			challengeDraft.clear();
			showChallengeInput = false;
		} finally {
			isSubmitting = false;
		}
	}
	
	async function handleCommentSubmit() {
		if (!onComment || !commentDraft.content.trim() || isSubmitting) return;
		isSubmitting = true;
		try {
			await onComment(commentDraft.content.trim());
			commentDraft.clear();
			showCommentInput = false;
		} finally {
			isSubmitting = false;
		}
	}
	
	async function handleModificationSubmit() {
		if (!onModification || !modificationDraft.content.trim() || isSubmitting) return;
		isSubmitting = true;
		try {
			await onModification(modificationDraft.content.trim());
			modificationDraft.clear();
			showModificationInput = false;
		} finally {
			isSubmitting = false;
		}
	}
	
	function useTemplate(text: string) {
		challengeDraft.content = text;
	}
	
	function toggleChallenge() {
		showChallengeInput = !showChallengeInput;
		if (showChallengeInput) {
			showCommentInput = false;
			showModificationInput = false;
		}
	}
	
	function toggleComment() {
		showCommentInput = !showCommentInput;
		if (showCommentInput) {
			showChallengeInput = false;
			showModificationInput = false;
		}
	}
	
	function toggleModification() {
		showModificationInput = !showModificationInput;
		if (showModificationInput) {
			showChallengeInput = false;
			showCommentInput = false;
		}
	}
</script>

<div class="proposal-card-actions" class:has-activity={hasActivity} class:expanded={showChallengeInput || showCommentInput || showModificationInput}>
	<!-- Main proposal content -->
	<button class="proposal-header" onclick={onExpand} disabled={!onExpand}>
		<h4 class="content">{proposal.content}</h4>
		<div class="badges">
			<ProposalStatusBadge {status} />
		</div>
	</button>
	
	<!-- Meta info -->
	<div class="meta">
		<AuthorBadge authorPub={proposal.authorPub} {currentUserPub} compact />
		
		{#if hasActivity}
			<div class="stats">
				{#if challengeCount > 0}
					<span class="stat challenges">⚠️ {challengeCount}</span>
				{/if}
				{#if commentCount > 0}
					<span class="stat comments">💬 {commentCount}</span>
				{/if}
				{#if modificationCount > 0}
					<span class="stat modifications">✏️ {modificationCount}</span>
				{/if}
			</div>
		{/if}
	</div>
	
	<!-- Phase-appropriate actions -->
	{#if !isMyProposal}
		<div class="action-buttons">
			{#if currentPhase === 'challenging'}
				<button class="action-btn agree" onclick={handleAgree} disabled={isSubmitting}>
					✓ No Concerns
				</button>
				<button class="action-btn challenge" onclick={toggleChallenge} class:active={showChallengeInput}>
					⚠️ Raise Concern
				</button>
			{:else if currentPhase === 'commenting'}
				<button class="action-btn comment" onclick={toggleComment} class:active={showCommentInput}>
					💬 Comment
				</button>
				<button class="action-btn modification" onclick={toggleModification} class:active={showModificationInput}>
					✏️ Suggest Change
				</button>
			{/if}
		</div>
		
		<!-- Inline challenge input -->
		{#if showChallengeInput}
			<div class="inline-input" transition:slide={{ duration: 200 }}>
				<!-- Quick templates -->
				<div class="templates">
					{#each challengeTemplates as template}
						<button class="template-btn" onclick={() => useTemplate(template.text)}>
							{template.label}
						</button>
					{/each}
				</div>
				
				<textarea
					bind:value={challengeDraft.content}
					placeholder="What's your concern?"
					rows="3"
					disabled={isSubmitting}
				></textarea>
				
				<div class="input-actions">
					<button class="cancel" onclick={() => showChallengeInput = false} disabled={isSubmitting}>
						Cancel
					</button>
					<button 
						class="submit" 
						onclick={handleChallengeSubmit}
						disabled={!challengeDraft.content.trim() || isSubmitting}
					>
						{isSubmitting ? 'Submitting...' : 'Submit & Next'}
					</button>
				</div>
			</div>
		{/if}
		
		<!-- Inline comment input -->
		{#if showCommentInput}
			<div class="inline-input" transition:slide={{ duration: 200 }}>
				<textarea
					bind:value={commentDraft.content}
					placeholder="Share your thoughts..."
					rows="3"
					disabled={isSubmitting}
				></textarea>
				
				<div class="input-actions">
					<button class="cancel" onclick={() => showCommentInput = false} disabled={isSubmitting}>
						Cancel
					</button>
					<button 
						class="submit" 
						onclick={handleCommentSubmit}
						disabled={!commentDraft.content.trim() || isSubmitting}
					>
						{isSubmitting ? 'Submitting...' : 'Submit Comment'}
					</button>
				</div>
			</div>
		{/if}
		
		<!-- Inline modification input -->
		{#if showModificationInput}
			<div class="inline-input" transition:slide={{ duration: 200 }}>
				<div class="hint">Suggest an improved version:</div>
				<textarea
					bind:value={modificationDraft.content}
					placeholder="Your improved version..."
					rows="4"
					disabled={isSubmitting}
				></textarea>
				
				<div class="input-actions">
					<button class="cancel" onclick={() => showModificationInput = false} disabled={isSubmitting}>
						Cancel
					</button>
					<button 
						class="submit" 
						onclick={handleModificationSubmit}
						disabled={!modificationDraft.content.trim() || isSubmitting}
					>
						{isSubmitting ? 'Submitting...' : 'Submit Modification'}
					</button>
				</div>
			</div>
		{/if}
	{/if}
</div>

<style>
	.proposal-card-actions {
		background: white;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.875rem;
		padding: 1rem;
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		transition: all 0.3s ease;
	}
	
	.proposal-card-actions.has-activity {
		border-color: #ffa726;
	}
	
	.proposal-card-actions.expanded {
		border-color: var(--primary-color, #667eea);
		box-shadow: 0 4px 16px rgba(102, 126, 234, 0.15);
	}
	
	.proposal-header {
		display: flex;
		flex-direction: column;
		gap: 0.625rem;
		text-align: left;
		background: none;
		border: none;
		padding: 0;
		cursor: pointer;
		transition: opacity 0.2s;
	}
	
	.proposal-header:hover:not(:disabled) {
		opacity: 0.8;
	}
	
	.proposal-header:disabled {
		cursor: default;
	}
	
	.content {
		margin: 0;
		font-size: 1rem;
		font-weight: 500;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.badges {
		display: flex;
		gap: 0.5rem;
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
	
	/* Action Buttons */
	.action-buttons {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
		padding-top: 0.5rem;
		border-top: 1px solid var(--border-color, #e0e0e0);
	}
	
	.action-btn {
		flex: 1;
		min-width: fit-content;
		padding: 0.625rem 1rem;
		border: 2px solid;
		border-radius: 0.625rem;
		font-size: 0.875rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s ease;
		background: white;
	}
	
	.action-btn.agree {
		border-color: #10b981;
		color: #10b981;
	}
	
	.action-btn.agree:hover {
		background: #10b981;
		color: white;
	}
	
	.action-btn.challenge {
		border-color: #f44336;
		color: #f44336;
	}
	
	.action-btn.challenge:hover,
	.action-btn.challenge.active {
		background: #f44336;
		color: white;
	}
	
	.action-btn.comment {
		border-color: #2196f3;
		color: #2196f3;
	}
	
	.action-btn.comment:hover,
	.action-btn.comment.active {
		background: #2196f3;
		color: white;
	}
	
	.action-btn.modification {
		border-color: #ff9800;
		color: #ff9800;
	}
	
	.action-btn.modification:hover,
	.action-btn.modification.active {
		background: #ff9800;
		color: white;
	}
	
	/* Inline Input */
	.inline-input {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		padding: 1rem;
		background: #f8f9fa;
		border-radius: 0.625rem;
	}
	
	.templates {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.template-btn {
		padding: 0.375rem 0.75rem;
		background: white;
		border: 1px solid var(--border-color, #e0e0e0);
		border-radius: 1rem;
		font-size: 0.8125rem;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.template-btn:hover {
		border-color: #f44336;
		color: #f44336;
	}
	
	.hint {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-secondary, #666);
	}
	
	textarea {
		width: 100%;
		padding: 0.75rem;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.5rem;
		font-size: 0.9375rem;
		font-family: inherit;
		line-height: 1.5;
		resize: vertical;
		transition: border-color 0.2s;
	}
	
	textarea:focus {
		outline: none;
		border-color: var(--primary-color, #667eea);
	}
	
	textarea:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}
	
	.input-actions {
		display: flex;
		justify-content: flex-end;
		gap: 0.5rem;
	}
	
	.input-actions button {
		padding: 0.625rem 1.25rem;
		border: none;
		border-radius: 0.5rem;
		font-size: 0.875rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.cancel {
		background: var(--bg-muted, #e0e0e0);
		color: var(--text-primary, #333);
	}
	
	.cancel:hover:not(:disabled) {
		background: #d0d0d0;
	}
	
	.submit {
		background: var(--primary-color, #667eea);
		color: white;
	}
	
	.submit:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
</style>

