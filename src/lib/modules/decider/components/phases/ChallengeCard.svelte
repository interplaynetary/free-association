<script lang="ts">
	/**
	 * @component ChallengeCard
	 * Interface for raising challenges to proposals
	 * 
	 * @prop {string} proposalContent - The proposal being challenged
	 * @prop {string} proposalAuthor - Author of the proposal
	 * @prop {Function} onSubmit - Callback when challenge is submitted
	 * @prop {Function} onCancel - Callback when cancelled
	 */
	
	let { proposalContent, proposalAuthor, onSubmit, onCancel }: {
		proposalContent: string;
		proposalAuthor: string;
		onSubmit: (content: string) => Promise<void>;
		onCancel: () => void;
	} = $props();
	
	let challengeInput = $state('');
	let isSubmitting = $state(false);
	
	async function handleSubmit() {
		if (!challengeInput.trim() || isSubmitting) return;
		
		isSubmitting = true;
		try {
			await onSubmit(challengeInput.trim());
		} finally {
			isSubmitting = false;
		}
	}
</script>

<div class="challenge-card">
	<div class="header">
		<h3 class="title">
			<span class="icon">⚠️</span>
			Challenge Proposal
		</h3>
	</div>
	
	<div class="proposal-preview">
		<div class="label">Proposal by {proposalAuthor.slice(0, 8)}...</div>
		<div class="content">{proposalContent}</div>
	</div>
	
	<div class="instructions">
		Raise a concern or identify an issue with this proposal. This helps surface important information before the group commits to a decision.
	</div>
	
	<textarea
		bind:value={challengeInput}
		placeholder="Explain your concern..."
		rows="4"
		disabled={isSubmitting}
	></textarea>
	
	<div class="actions">
		<button class="cancel-btn" onclick={onCancel} disabled={isSubmitting}>
			Cancel
		</button>
		<button 
			class="submit-btn" 
			onclick={handleSubmit}
			disabled={!challengeInput.trim() || isSubmitting}
		>
			{isSubmitting ? 'Submitting...' : 'Submit Challenge'}
		</button>
	</div>
</div>

<style>
	.challenge-card {
		background: white;
		padding: 1.5rem;
		border-radius: 1rem;
		box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
		max-width: 40rem;
	}
	
	.header {
		margin-bottom: 1rem;
	}
	
	.title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #f44336;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.5rem;
	}
	
	.proposal-preview {
		background: #f8f9fa;
		padding: 1rem;
		border-radius: 0.625rem;
		border-left: 3px solid #f44336;
		margin-bottom: 1rem;
	}
	
	.label {
		font-size: 0.8125rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: var(--text-muted, #666);
		margin-bottom: 0.5rem;
	}
	
	.content {
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.instructions {
		background: #fff9f8;
		padding: 0.875rem;
		border-radius: 0.625rem;
		font-size: 0.875rem;
		line-height: 1.5;
		color: var(--text-secondary, #555);
		margin-bottom: 1rem;
	}
	
	textarea {
		width: 100%;
		padding: 0.875rem;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.625rem;
		font-size: 1rem;
		font-family: inherit;
		line-height: 1.5;
		resize: vertical;
		margin-bottom: 1rem;
		transition: border-color 0.2s;
	}
	
	textarea:focus {
		outline: none;
		border-color: #f44336;
	}
	
	textarea:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}
	
	.actions {
		display: flex;
		justify-content: flex-end;
		gap: 0.75rem;
	}
	
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 0.625rem;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.cancel-btn {
		background: var(--bg-muted, #e0e0e0);
		color: var(--text-primary, #333);
	}
	
	.cancel-btn:hover:not(:disabled) {
		background: #d0d0d0;
	}
	
	.submit-btn {
		background: #f44336;
		color: white;
	}
	
	.submit-btn:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(244, 67, 54, 0.3);
	}
	
	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
</style>

