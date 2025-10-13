<script lang="ts">
	import { ReactiveP2PDecider } from '../decider/p2p-decider-reactive.svelte';
	import { onMount, onDestroy } from 'svelte';
	
	// Props
	export let user: any;  // Authenticated Holster user
	export let gameId: string;
	
	// Create reactive decider instance
	let decider: ReactiveP2PDecider;
	let isInitialized = false;
	
	// UI state
	let myProposalInput = '';
	let myChallengeInput = '';
	let myCommentInput = '';
	let myModificationInput = '';
	let selectedProposalForAction: string | null = null;
	
	onMount(async () => {
		decider = new ReactiveP2PDecider(user, gameId);
		
		// Try to join existing game or create new one
		try {
			// For demo: try to create game
			await decider.createGame(['What should we have for dinner?']);
			isInitialized = true;
		} catch (e) {
			console.error('Failed to initialize game:', e);
		}
	});
	
	onDestroy(() => {
		if (decider) {
			decider.destroy();
		}
	});
	
	// Actions
	async function submitProposal() {
		if (!myProposalInput.trim()) return;
		await decider.writeMyProposal(myProposalInput);
		myProposalInput = '';
	}
	
	async function submitChallenge() {
		if (!selectedProposalForAction || !myChallengeInput.trim()) return;
		await decider.writeMyChallengeToProposal(selectedProposalForAction, myChallengeInput);
		myChallengeInput = '';
		selectedProposalForAction = null;
	}
	
	async function submitComment() {
		if (!selectedProposalForAction || !myCommentInput.trim()) return;
		await decider.writeMyCommentOnProposal(selectedProposalForAction, myCommentInput);
		myCommentInput = '';
	}
	
	async function submitModification() {
		if (!selectedProposalForAction || !myModificationInput.trim()) return;
		await decider.writeMyModificationToProposal(selectedProposalForAction, myModificationInput);
		myModificationInput = '';
	}
	
	async function submitSupport(proposalAuthorPub: string, candidates: string[]) {
		// For demo: distribute points equally
		const pointsPerCandidate = Math.floor(10 / candidates.length);
		const support: Record<string, number> = {};
		candidates.forEach((candidate, i) => {
			support[candidate] = i === 0 ? 10 - (pointsPerCandidate * (candidates.length - 1)) : pointsPerCandidate;
		});
		await decider.writeMySupportForProposal(proposalAuthorPub, support);
	}
	
	// Helper to get candidate list for a proposal
	function getCandidates(proposal: any, modifications: any[]) {
		return [proposal.content, ...modifications.map(m => m.content)];
	}
</script>

{#if !isInitialized}
	<div class="loading">
		<p>Initializing game...</p>
	</div>
{:else if decider}
	<div class="game-container">
		<!-- Header with game info -->
		<header>
			<h1>P2P Decider Game</h1>
			{#if decider.config}
				<div class="game-info">
					<p><strong>Game ID:</strong> {decider.config.gameId}</p>
					<p><strong>Agenda:</strong> {decider.config.agenda[decider.config.currentAgendaIndex]}</p>
					<p><strong>Phase:</strong> <span class="phase-badge">{decider.currentPhase}</span></p>
					<p><strong>Participants:</strong> {decider.config.participants.length}</p>
				</div>
			{/if}
		</header>

		<!-- Phase: Proposing -->
		{#if decider.currentPhase === 'proposing' || decider.currentPhase === 'challenging'}
			<section class="phase-section">
				<h2>📝 Submit Your Proposal</h2>
				<div class="input-group">
					<input 
						type="text" 
						bind:value={myProposalInput}
						placeholder="Enter your proposal..."
						on:keypress={(e) => e.key === 'Enter' && submitProposal()}
					/>
					<button on:click={submitProposal}>Submit Proposal</button>
				</div>
			</section>
		{/if}

		<!-- All Proposals Display -->
		{#if decider.allProposals && decider.allProposals.length > 0}
			<section class="proposals-section">
				<h2>🎯 All Proposals ({decider.allProposals.length})</h2>
				<div class="proposals-grid">
					{#each decider.allProposals as proposal}
						<div class="proposal-card">
							<div class="proposal-header">
								<h3>{proposal.content}</h3>
								<span class="author-badge">
									{proposal.authorPub === user.is.pub ? '(You)' : `By ${proposal.authorPub.slice(0, 8)}...`}
								</span>
							</div>
							
							<!-- Challenges -->
							{#if decider.allChallenges && decider.allChallenges.get(proposal.authorPub)?.length}
								<div class="challenges">
									<h4>⚠️ Challenges ({decider.allChallenges.get(proposal.authorPub)?.length})</h4>
									{#each decider.allChallenges.get(proposal.authorPub) || [] as challenge}
										<div class="challenge-item">
											<p>{challenge.content}</p>
											<small>by {challenge.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Comments -->
							{#if decider.allComments && decider.allComments.get(proposal.authorPub)?.length}
								<div class="comments">
									<h4>💬 Comments ({decider.allComments.get(proposal.authorPub)?.length})</h4>
									{#each decider.allComments.get(proposal.authorPub) || [] as comment}
										<div class="comment-item">
											<p>{comment.content}</p>
											<small>by {comment.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Modifications -->
							{#if decider.allModifications && decider.allModifications.get(proposal.authorPub)?.length}
								<div class="modifications">
									<h4>✏️ Modifications ({decider.allModifications.get(proposal.authorPub)?.length})</h4>
									{#each decider.allModifications.get(proposal.authorPub) || [] as mod}
										<div class="modification-item">
											<p>{mod.content}</p>
											<small>by {mod.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Support Summary -->
							{#if decider.allSupport && decider.allSupport.get(proposal.authorPub)?.length}
								<div class="support-summary">
									<h4>👍 Support ({decider.allSupport.get(proposal.authorPub)?.length} votes)</h4>
									{#if decider.consensusResults && decider.consensusResults.get(proposal.authorPub)}
										<div class="winner">
											<strong>Winner:</strong> {decider.consensusResults.get(proposal.authorPub)}
										</div>
									{/if}
								</div>
							{/if}
							
							<!-- Actions -->
							<div class="proposal-actions">
								{#if decider.currentPhase === 'challenging' && proposal.authorPub !== user.is.pub}
									<button 
										on:click={() => selectedProposalForAction = proposal.authorPub}
										class="btn-challenge"
									>
										Challenge
									</button>
								{/if}
								
								{#if decider.currentPhase === 'commenting'}
									<button 
										on:click={() => selectedProposalForAction = proposal.authorPub}
										class="btn-comment"
									>
										Comment
									</button>
									<button 
										on:click={() => selectedProposalForAction = proposal.authorPub}
										class="btn-modify"
									>
										Propose Modification
									</button>
								{/if}
								
								{#if decider.currentPhase === 'supporting'}
									<button 
										on:click={() => submitSupport(
											proposal.authorPub, 
											getCandidates(proposal, decider.allModifications?.get(proposal.authorPub) || [])
										)}
										class="btn-support"
									>
										Vote
									</button>
								{/if}
							</div>
						</div>
					{/each}
				</div>
			</section>
		{/if}

		<!-- Action Modals -->
		{#if selectedProposalForAction}
			<div class="modal-overlay" on:click={() => selectedProposalForAction = null} role="button" tabindex="0" on:keydown={(e) => e.key === 'Escape' && (selectedProposalForAction = null)}>
				<div class="modal" on:click|stopPropagation role="dialog" tabindex="-1" on:keydown={(e) => e.key === 'Escape' && (selectedProposalForAction = null)}>
					{#if decider.currentPhase === 'challenging'}
						<h3>Challenge Proposal</h3>
						<input 
							type="text" 
							bind:value={myChallengeInput}
							placeholder="Explain your challenge..."
						/>
						<div class="modal-actions">
							<button on:click={submitChallenge} class="btn-primary">Submit Challenge</button>
							<button on:click={() => selectedProposalForAction = null}>Cancel</button>
						</div>
					{/if}
					
					{#if decider.currentPhase === 'commenting'}
						<h3>Comment or Modify</h3>
						<div class="modal-content">
							<div class="input-group">
								<label for="comment-input">Comment:</label>
								<input 
									id="comment-input"
									type="text" 
									bind:value={myCommentInput}
									placeholder="Your comment..."
								/>
								<button on:click={submitComment}>Submit Comment</button>
							</div>
							<div class="input-group">
								<label for="modification-input">Modification:</label>
								<input 
									id="modification-input"
									type="text" 
									bind:value={myModificationInput}
									placeholder="Your modified version..."
								/>
								<button on:click={submitModification}>Submit Modification</button>
							</div>
						</div>
						<div class="modal-actions">
							<button on:click={() => selectedProposalForAction = null}>Close</button>
						</div>
					{/if}
				</div>
			</div>
		{/if}

		<!-- Final Results -->
		{#if decider.currentPhase === 'complete' && decider.allProposals && decider.consensusResults}
			<section class="results-section">
				<h2>🏆 Final Results</h2>
				<div class="results-grid">
					{#each decider.allProposals as proposal}
						<div class="result-card">
							<h3>Proposal by {proposal.authorPub.slice(0, 8)}...</h3>
							<div class="winner-announcement">
								<p><strong>Winning Version:</strong></p>
								<p class="winner-text">{decider.consensusResults.get(proposal.authorPub)}</p>
							</div>
						</div>
					{/each}
				</div>
			</section>
		{/if}
	</div>
{/if}

<style>
	.game-container {
		max-width: 1200px;
		margin: 0 auto;
		padding: 2rem;
		font-family: system-ui, -apple-system, sans-serif;
	}
	
	header {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		padding: 2rem;
		border-radius: 12px;
		margin-bottom: 2rem;
	}
	
	h1 {
		margin: 0 0 1rem 0;
	}
	
	.game-info {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1rem;
	}
	
	.phase-badge {
		background: rgba(255, 255, 255, 0.3);
		padding: 0.25rem 0.75rem;
		border-radius: 20px;
		font-weight: bold;
		text-transform: uppercase;
		font-size: 0.85rem;
	}
	
	.phase-section {
		background: #f8f9fa;
		padding: 2rem;
		border-radius: 12px;
		margin-bottom: 2rem;
	}
	
	.input-group {
		display: flex;
		gap: 1rem;
		margin-top: 1rem;
	}
	
	.input-group input {
		flex: 1;
		padding: 0.75rem;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 1rem;
	}
	
	.input-group input:focus {
		outline: none;
		border-color: #667eea;
	}
	
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 8px;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	button:hover {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
	}
	
	.proposals-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(350px, 1fr));
		gap: 1.5rem;
	}
	
	.proposal-card {
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 12px;
		padding: 1.5rem;
		transition: all 0.2s;
	}
	
	.proposal-card:hover {
		border-color: #667eea;
		box-shadow: 0 4px 20px rgba(102, 126, 234, 0.1);
	}
	
	.proposal-header {
		border-bottom: 2px solid #f0f0f0;
		padding-bottom: 1rem;
		margin-bottom: 1rem;
	}
	
	.author-badge {
		display: inline-block;
		background: #e3f2fd;
		color: #1976d2;
		padding: 0.25rem 0.75rem;
		border-radius: 12px;
		font-size: 0.85rem;
		margin-top: 0.5rem;
	}
	
	.challenges, .comments, .modifications, .support-summary {
		margin-top: 1rem;
		padding: 1rem;
		background: #f8f9fa;
		border-radius: 8px;
	}
	
	.challenges h4 { color: #f44336; }
	.comments h4 { color: #2196f3; }
	.modifications h4 { color: #ff9800; }
	.support-summary h4 { color: #4caf50; }
	
	.challenge-item, .comment-item, .modification-item {
		background: white;
		padding: 0.75rem;
		margin-top: 0.5rem;
		border-radius: 6px;
		border-left: 3px solid #667eea;
	}
	
	.winner {
		background: #4caf50;
		color: white;
		padding: 0.75rem;
		border-radius: 6px;
		margin-top: 0.5rem;
		font-weight: bold;
	}
	
	.proposal-actions {
		display: flex;
		gap: 0.5rem;
		margin-top: 1rem;
		flex-wrap: wrap;
	}
	
	.btn-challenge { background: #f44336; color: white; }
	.btn-comment { background: #2196f3; color: white; }
	.btn-modify { background: #ff9800; color: white; }
	.btn-support { background: #4caf50; color: white; }
	
	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.5);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
	}
	
	.modal {
		background: white;
		padding: 2rem;
		border-radius: 12px;
		max-width: 500px;
		width: 90%;
		max-height: 80vh;
		overflow-y: auto;
	}
	
	.modal h3 {
		margin-top: 0;
	}
	
	.modal-content {
		margin: 1.5rem 0;
	}
	
	.modal-actions {
		display: flex;
		gap: 1rem;
		justify-content: flex-end;
		margin-top: 1.5rem;
	}
	
	.btn-primary {
		background: #667eea;
		color: white;
	}
	
	.results-section {
		background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%);
		padding: 2rem;
		border-radius: 12px;
		margin-top: 2rem;
	}
	
	.results-grid {
		display: grid;
		gap: 1.5rem;
		margin-top: 1.5rem;
	}
	
	.result-card {
		background: white;
		padding: 1.5rem;
		border-radius: 12px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
	}
	
	.winner-announcement {
		background: #4caf50;
		color: white;
		padding: 1.5rem;
		border-radius: 8px;
		margin-top: 1rem;
	}
	
	.winner-text {
		font-size: 1.25rem;
		font-weight: bold;
		margin-top: 0.5rem;
	}
	
	.loading {
		display: flex;
		align-items: center;
		justify-content: center;
		height: 100vh;
		font-size: 1.5rem;
		color: #667eea;
	}
</style>

