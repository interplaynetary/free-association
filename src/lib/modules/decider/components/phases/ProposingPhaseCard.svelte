<script lang="ts">
	/**
	 * @component ProposingPhaseCard
	 * Interface for submitting new proposals
	 * 
	 * @prop {Function} onSubmit - Callback when proposal is submitted
	 * @prop {string[]} participants - All participants
	 * @prop {Set<string>} submittedParticipants - Participants who already submitted
	 */
	
	let { onSubmit, participants, submittedParticipants = new Set() }: {
		onSubmit: (content: string) => Promise<void>;
		participants: string[];
		submittedParticipants?: Set<string>;
	} = $props();
	
	let proposalInput = $state('');
	let isSubmitting = $state(false);
	
	const remainingCount = $derived(participants.length - submittedParticipants.size);
	const progress = $derived((submittedParticipants.size / participants.length) * 100);
	
	async function handleSubmit() {
		if (!proposalInput.trim() || isSubmitting) return;
		
		isSubmitting = true;
		try {
			await onSubmit(proposalInput.trim());
			proposalInput = '';
		} finally {
			isSubmitting = false;
		}
	}
	
	function handleKeyPress(e: KeyboardEvent) {
		if (e.key === 'Enter' && !e.shiftKey) {
			e.preventDefault();
			handleSubmit();
		}
	}
</script>

<div class="proposing-phase-card">
	<div class="header">
		<h3 class="title">
			<span class="icon">📝</span>
			Submit Your Proposal
		</h3>
		<div class="progress-info">
			<span class="count">{submittedParticipants.size}/{participants.length} submitted</span>
			<div class="progress-bar">
				<div class="progress-fill" style="width: {progress}%"></div>
			</div>
		</div>
	</div>
	
	<div class="instructions">
		Share your initial idea. Every participant should propose their own solution to the agenda item.
	</div>
	
	<div class="input-section">
		<textarea
			bind:value={proposalInput}
			onkeypress={handleKeyPress}
			placeholder="Enter your proposal..."
			rows="3"
			disabled={isSubmitting}
		></textarea>
		
		<div class="actions">
			<div class="char-count">
				{proposalInput.length} characters
			</div>
			<button 
				class="submit-btn" 
				onclick={handleSubmit}
				disabled={!proposalInput.trim() || isSubmitting}
			>
				{isSubmitting ? 'Submitting...' : 'Submit Proposal'}
			</button>
		</div>
	</div>
	
	{#if remainingCount > 0}
		<div class="waiting-notice">
			Waiting for {remainingCount} more {remainingCount === 1 ? 'proposal' : 'proposals'}...
		</div>
	{/if}
</div>

<style>
	.proposing-phase-card {
		background: white;
		padding: 1.5rem;
		border-radius: 1rem;
		box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
	}
	
	.header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		gap: 1rem;
		margin-bottom: 1rem;
		flex-wrap: wrap;
	}
	
	.title {
		margin: 0;
		font-size: 1.125rem;
		font-weight: 600;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.25rem;
	}
	
	.progress-info {
		display: flex;
		flex-direction: column;
		gap: 0.375rem;
		min-width: 8rem;
	}
	
	.count {
		font-size: 0.875rem;
		font-weight: 600;
		color: var(--text-muted, #666);
		text-align: right;
	}
	
	.progress-bar {
		height: 0.375rem;
		background: var(--bg-muted, #f0f0f0);
		border-radius: 0.25rem;
		overflow: hidden;
	}
	
	.progress-fill {
		height: 100%;
		background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
		transition: width 0.5s ease;
	}
	
	.instructions {
		background: #f8f9fa;
		padding: 0.875rem;
		border-radius: 0.625rem;
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-secondary, #555);
		margin-bottom: 1rem;
	}
	
	.input-section {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
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
	
	.actions {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
	}
	
	.char-count {
		font-size: 0.8125rem;
		color: var(--text-muted, #666);
	}
	
	.submit-btn {
		background: var(--primary-color, #667eea);
		color: white;
		border: none;
		padding: 0.75rem 1.5rem;
		border-radius: 0.625rem;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.submit-btn:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.submit-btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	.waiting-notice {
		margin-top: 1rem;
		padding: 0.75rem;
		background: #fff9c4;
		border-radius: 0.625rem;
		font-size: 0.875rem;
		color: #f57f17;
		text-align: center;
		font-weight: 500;
	}
</style>

