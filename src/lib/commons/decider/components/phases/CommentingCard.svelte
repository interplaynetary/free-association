<script lang="ts">
	/**
	 * @component CommentingCard
	 * Interface for adding comments or proposing modifications
	 * 
	 * @prop {string} proposalContent - The proposal being discussed
	 * @prop {Array} challenges - Challenges that need addressing
	 * @prop {Function} onSubmitComment - Callback for submitting comment
	 * @prop {Function} onSubmitModification - Callback for submitting modification
	 * @prop {Function} onClose - Callback when closed
	 */
	
	let { 
		proposalContent, 
		challenges,
		onSubmitComment,
		onSubmitModification,
		onClose
	}: {
		proposalContent: string;
		challenges: Array<{ content: string; authorPub: string }>;
		onSubmitComment: (content: string) => Promise<void>;
		onSubmitModification: (content: string) => Promise<void>;
		onClose: () => void;
	} = $props();
	
	let commentInput = $state('');
	let modificationInput = $state('');
	let isSubmitting = $state(false);
	
	async function handleSubmitComment() {
		if (!commentInput.trim() || isSubmitting) return;
		
		isSubmitting = true;
		try {
			await onSubmitComment(commentInput.trim());
			commentInput = '';
		} finally {
			isSubmitting = false;
		}
	}
	
	async function handleSubmitModification() {
		if (!modificationInput.trim() || isSubmitting) return;
		
		isSubmitting = true;
		try {
			await onSubmitModification(modificationInput.trim());
			modificationInput = '';
		} finally {
			isSubmitting = false;
		}
	}
</script>

<div class="commenting-card">
	<div class="header">
		<h3 class="title">
			<span class="icon">💬</span>
			Discuss & Improve
		</h3>
	</div>
	
	<div class="proposal-section">
		<div class="label">Original Proposal</div>
		<div class="content">{proposalContent}</div>
	</div>
	
	{#if challenges.length > 0}
		<div class="challenges-section">
			<div class="label">
				<span class="icon">⚠️</span>
				Challenges Raised ({challenges.length})
			</div>
			<div class="challenges-list">
				{#each challenges as challenge}
					<div class="challenge-item">
						{challenge.content}
						<small>— {challenge.authorPub.slice(0, 8)}...</small>
					</div>
				{/each}
			</div>
		</div>
	{/if}
	
	<div class="action-sections">
		<!-- Comment Section -->
		<div class="section">
			<label for="comment-input">
				<span class="icon">💭</span>
				Add Comment
			</label>
			<textarea
				id="comment-input"
				bind:value={commentInput}
				placeholder="Provide context, ask questions, or discuss the challenges..."
				rows="3"
				disabled={isSubmitting}
			></textarea>
			<button 
				class="submit-btn comment" 
				onclick={handleSubmitComment}
				disabled={!commentInput.trim() || isSubmitting}
			>
				Submit Comment
			</button>
		</div>
		
		<div class="divider">
			<span>OR</span>
		</div>
		
		<!-- Modification Section -->
		<div class="section">
			<label for="modification-input">
				<span class="icon">✏️</span>
				Propose Modified Version
			</label>
			<textarea
				id="modification-input"
				bind:value={modificationInput}
				placeholder="Suggest an improved version that addresses the challenges..."
				rows="3"
				disabled={isSubmitting}
			></textarea>
			<button 
				class="submit-btn modification" 
				onclick={handleSubmitModification}
				disabled={!modificationInput.trim() || isSubmitting}
			>
				Submit Modification
			</button>
		</div>
	</div>
	
	<div class="footer">
		<button class="close-btn" onclick={onClose}>Close</button>
	</div>
</div>

<style>
	.commenting-card {
		background: white;
		padding: 1.5rem;
		border-radius: 1rem;
		box-shadow: 0 4px 20px rgba(0, 0, 0, 0.15);
		max-width: 45rem;
	}
	
	.header {
		margin-bottom: 1rem;
	}
	
	.title {
		margin: 0;
		font-size: 1.25rem;
		font-weight: 600;
		color: #2196f3;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.icon {
		font-size: 1.25rem;
	}
	
	.proposal-section,
	.challenges-section {
		background: #f8f9fa;
		padding: 1rem;
		border-radius: 0.625rem;
		margin-bottom: 1rem;
	}
	
	.proposal-section {
		border-left: 3px solid #2196f3;
	}
	
	.challenges-section {
		border-left: 3px solid #f44336;
	}
	
	.label {
		font-size: 0.8125rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: var(--text-muted, #666);
		margin-bottom: 0.5rem;
		display: flex;
		align-items: center;
		gap: 0.375rem;
	}
	
	.content {
		font-size: 0.9375rem;
		line-height: 1.5;
		color: var(--text-primary, #333);
	}
	
	.challenges-list {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}
	
	.challenge-item {
		background: white;
		padding: 0.625rem;
		border-radius: 0.5rem;
		font-size: 0.875rem;
		line-height: 1.5;
	}
	
	.challenge-item small {
		color: var(--text-muted, #666);
	}
	
	.action-sections {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.section {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}
	
	label {
		font-weight: 600;
		color: var(--text-primary, #333);
		display: flex;
		align-items: center;
		gap: 0.375rem;
	}
	
	textarea {
		width: 100%;
		padding: 0.875rem;
		border: 2px solid var(--border-color, #e0e0e0);
		border-radius: 0.625rem;
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
	
	.divider {
		text-align: center;
		color: var(--text-muted, #999);
		font-weight: 600;
		position: relative;
		margin: 0.5rem 0;
	}
	
	.divider::before,
	.divider::after {
		content: '';
		position: absolute;
		top: 50%;
		width: calc(50% - 2rem);
		height: 1px;
		background: var(--border-color, #e0e0e0);
	}
	
	.divider::before {
		left: 0;
	}
	
	.divider::after {
		right: 0;
	}
	
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 0.625rem;
		font-size: 0.9375rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	.submit-btn {
		align-self: flex-start;
	}
	
	.submit-btn.comment {
		background: #2196f3;
		color: white;
	}
	
	.submit-btn.comment:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(33, 150, 243, 0.3);
	}
	
	.submit-btn.modification {
		background: #ff9800;
		color: white;
	}
	
	.submit-btn.modification:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(255, 152, 0, 0.3);
	}
	
	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	.footer {
		margin-top: 1.5rem;
		padding-top: 1rem;
		border-top: 1px solid var(--border-color, #e0e0e0);
		display: flex;
		justify-content: flex-end;
	}
	
	.close-btn {
		background: var(--bg-muted, #e0e0e0);
		color: var(--text-primary, #333);
	}
	
	.close-btn:hover {
		background: #d0d0d0;
	}
</style>

