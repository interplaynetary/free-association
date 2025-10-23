<script lang="ts">
	import { ReactiveP2PDecider } from '../decider/decider.svelte';
	import { onMount, onDestroy } from 'svelte';
	
	// Props
	export let user: any;
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
	let supportModalProposal: string | null = null;
	let supportPoints: Record<string, number> = {};
	let totalPoints = 10;
	
	onMount(async () => {
		decider = new ReactiveP2PDecider(user, gameId);
		
		try {
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
	
	function openSupportModal(proposalAuthorPub: string, candidates: string[]) {
		supportModalProposal = proposalAuthorPub;
		supportPoints = {};
		candidates.forEach(candidate => {
			supportPoints[candidate] = 0;
		});
	}
	
	function updateSupport(candidate: string, value: number) {
		supportPoints[candidate] = Math.max(0, Math.min(totalPoints, value));
	}
	
	function getTotalAllocated(): number {
		return Object.values(supportPoints).reduce((sum, val) => sum + val, 0);
	}
	
	async function submitSupport() {
		if (!supportModalProposal) return;
		const total = getTotalAllocated();
		if (total > totalPoints) {
			alert(`You've allocated ${total} points, but only ${totalPoints} are available!`);
			return;
		}
		await decider.writeMySupportForProposal(supportModalProposal, supportPoints);
		supportModalProposal = null;
		supportPoints = {};
	}
	
	// Helper functions
	function getCandidates(proposal: any, modifications: any[]) {
		return [proposal.content, ...modifications.map(m => m.content)];
	}
	
	function getProposalStatus(proposalAuthorPub: string): string {
		const challenges = decider.allChallenges?.get(proposalAuthorPub);
		const modifications = decider.allModifications?.get(proposalAuthorPub);
		
		if (!challenges || challenges.length === 0) {
			return 'passed-no-challenges';
		}
		if (!modifications || modifications.length === 0) {
			return 'passed-no-modifications';
		}
		return 'in-process';
	}
	
	function formatTimeWindow(ms: number): string {
		const hours = Math.floor(ms / 3600000);
		if (hours < 1) return `${Math.floor(ms / 60000)} minutes`;
		if (hours === 24) return '24 hours';
		return `${hours} hours`;
	}
	
	function getPhaseInstructions(phase: string): string {
		switch (phase) {
			case 'proposing':
				return 'Submit your initial proposal for the agenda item. Each participant should share their idea.';
			case 'challenging':
				return 'Review proposals and raise concerns if you see issues. Proposals with no challenges will pass immediately!';
			case 'commenting':
				return 'Discuss challenged proposals and propose modifications to address concerns. Proposals with no modifications will pass as-is.';
			case 'supporting':
				return 'Distribute your support points across the candidate versions. The most supported version will become the final decision.';
			case 'complete':
				return 'All proposals have been decided! Review the final results below.';
			default:
				return 'Waiting to start...';
		}
	}
	
	function getPhaseEmoji(phase: string): string {
		switch (phase) {
			case 'proposing': return '📝';
			case 'challenging': return '⚠️';
			case 'commenting': return '💬';
			case 'supporting': return '👍';
			case 'complete': return '🏆';
			default: return '⏳';
		}
	}
</script>

{#if !isInitialized}
	<div class="loading">
		<div class="spinner"></div>
		<p>Initializing Decider...</p>
	</div>
{:else if decider}
	<div class="game-container">
		<!-- Header with game info -->
		<header class="header">
			<h1>🎯 P2P Decider</h1>
			{#if decider.config}
				<div class="game-info-grid">
					<div class="info-card">
						<div class="info-label">Game ID</div>
						<div class="info-value">{decider.config.gameId}</div>
					</div>
					<div class="info-card">
						<div class="info-label">Current Agenda</div>
						<div class="info-value">{decider.config.agenda[decider.config.currentAgendaIndex]}</div>
					</div>
					<div class="info-card">
						<div class="info-label">Time Window</div>
						<div class="info-value">{formatTimeWindow(decider.config.timeWindow)}</div>
					</div>
					<div class="info-card">
						<div class="info-label">Participants</div>
						<div class="info-value">{decider.config.participants.length}</div>
					</div>
				</div>
			{/if}
		</header>

		<!-- Process Flow Visualization -->
		<section class="process-flow">
			<h2>Decision Process Flow</h2>
			<div class="flow-steps">
				<div class="flow-step" class:active={decider.currentPhase === 'proposing'} class:complete={['challenging', 'commenting', 'supporting', 'complete'].includes(decider.currentPhase)}>
					<div class="step-number">1</div>
					<div class="step-name">Propose</div>
				</div>
				<div class="flow-arrow">→</div>
				<div class="flow-step" class:active={decider.currentPhase === 'challenging'} class:complete={['commenting', 'supporting', 'complete'].includes(decider.currentPhase)}>
					<div class="step-number">2</div>
					<div class="step-name">Challenge</div>
				</div>
				<div class="flow-arrow">→</div>
				<div class="flow-step" class:active={decider.currentPhase === 'commenting'} class:complete={['supporting', 'complete'].includes(decider.currentPhase)}>
					<div class="step-number">3</div>
					<div class="step-name">Discuss</div>
				</div>
				<div class="flow-arrow">→</div>
				<div class="flow-step" class:active={decider.currentPhase === 'supporting'} class:complete={decider.currentPhase === 'complete'}>
					<div class="step-number">4</div>
					<div class="step-name">Support</div>
				</div>
				<div class="flow-arrow">→</div>
				<div class="flow-step" class:complete={decider.currentPhase === 'complete'}>
					<div class="step-number">✓</div>
					<div class="step-name">Complete</div>
				</div>
			</div>
		</section>

		<!-- Current Phase Section -->
		<section class="current-phase">
			<div class="phase-header">
				<h2>{getPhaseEmoji(decider.currentPhase)} {decider.currentPhase.toUpperCase()}</h2>
			</div>
			<p class="phase-instructions">{getPhaseInstructions(decider.currentPhase)}</p>
		</section>

		<!-- Participants List -->
		{#if decider.config}
			<section class="participants-section">
				<h3>👥 Participants</h3>
				<div class="participants-list">
					{#each decider.config.participants as participant}
						<div class="participant-badge" class:is-you={participant === user.is.pub}>
							{participant === user.is.pub ? 'You' : participant.slice(0, 8) + '...'}
						</div>
					{/each}
				</div>
			</section>
		{/if}

		<!-- Proposal Input (Proposing Phase) -->
		{#if decider.currentPhase === 'proposing'}
			<section class="action-section">
				<h3>📝 Submit Your Proposal</h3>
				<div class="input-group">
					<input 
						type="text" 
						bind:value={myProposalInput}
						placeholder="Enter your proposal..."
						onkeypress={(e) => e.key === 'Enter' && submitProposal()}
					/>
					<button onclick={submitProposal} class="btn-primary">Submit Proposal</button>
				</div>
			</section>
		{/if}

		<!-- All Proposals Display -->
		{#if decider.allProposals && decider.allProposals.length > 0}
			<section class="proposals-section">
				<h2>🎯 All Proposals ({decider.allProposals.length})</h2>
				<div class="proposals-grid">
					{#each decider.allProposals as proposal}
						{@const challenges = decider.allChallenges?.get(proposal.authorPub) || []}
						{@const comments = decider.allComments?.get(proposal.authorPub) || []}
						{@const modifications = decider.allModifications?.get(proposal.authorPub) || []}
						{@const support = decider.allSupport?.get(proposal.authorPub) || []}
						{@const status = getProposalStatus(proposal.authorPub)}
						
						<div class="proposal-card" class:has-early-exit={status !== 'in-process'}>
							<div class="proposal-header">
								<h3>{proposal.content}</h3>
								<div class="proposal-meta">
									<span class="author-badge">
										{proposal.authorPub === user.is.pub ? '(You)' : `By ${proposal.authorPub.slice(0, 8)}...`}
									</span>
									{#if status === 'passed-no-challenges'}
										<span class="status-badge status-pass-early">✅ Passed Early (No Challenges)</span>
									{:else if status === 'passed-no-modifications'}
										<span class="status-badge status-pass-as-is">✅ Passed As-Is (No Modifications)</span>
									{/if}
								</div>
							</div>
							
							<!-- Challenges -->
							{#if challenges.length > 0}
								<div class="data-section challenges">
									<h4>⚠️ Challenges ({challenges.length})</h4>
									{#each challenges as challenge}
										<div class="data-item">
											<p>{challenge.content}</p>
											<small>by {challenge.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Comments -->
							{#if comments.length > 0}
								<div class="data-section comments">
									<h4>💬 Comments ({comments.length})</h4>
									{#each comments as comment}
										<div class="data-item">
											<p>{comment.content}</p>
											<small>by {comment.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Modifications -->
							{#if modifications.length > 0}
								<div class="data-section modifications">
									<h4>✏️ Modifications ({modifications.length})</h4>
									{#each modifications as mod}
										<div class="data-item">
											<p>{mod.content}</p>
											<small>by {mod.authorPub.slice(0, 8)}...</small>
										</div>
									{/each}
								</div>
							{/if}
							
							<!-- Support Summary -->
							{#if support.length > 0}
								<div class="data-section support-summary">
									<h4>👍 Support ({support.length} votes)</h4>
									{#if decider.consensusResults && decider.consensusResults.get(proposal.authorPub)}
										<div class="winner-box">
											<strong>Most Supported:</strong>
											<div class="winner-text">{decider.consensusResults.get(proposal.authorPub)}</div>
										</div>
									{/if}
								</div>
							{/if}
							
							<!-- Actions -->
							<div class="proposal-actions">
								{#if decider.currentPhase === 'challenging' && proposal.authorPub !== user.is.pub}
									<button 
										onclick={() => selectedProposalForAction = proposal.authorPub}
										class="btn-action btn-challenge"
									>
										⚠️ Challenge
									</button>
								{/if}
								
								{#if decider.currentPhase === 'commenting' && challenges.length > 0}
									<button 
										onclick={() => selectedProposalForAction = proposal.authorPub}
										class="btn-action btn-comment"
									>
										💬 Comment
									</button>
									<button 
										onclick={() => selectedProposalForAction = proposal.authorPub}
										class="btn-action btn-modify"
									>
										✏️ Propose Modification
									</button>
								{/if}
								
								{#if decider.currentPhase === 'supporting' && modifications.length > 0}
									<button 
										onclick={() => openSupportModal(proposal.authorPub, getCandidates(proposal, modifications))}
										class="btn-action btn-support"
									>
										👍 Express Support
									</button>
								{/if}
							</div>
						</div>
					{/each}
				</div>
			</section>
		{/if}

		<!-- Challenge/Comment Modal -->
		{#if selectedProposalForAction}
			<div class="modal-overlay" onclick={() => selectedProposalForAction = null} onkeydown={(e) => e.key === 'Escape' && (selectedProposalForAction = null)} role="button" tabindex="0">
				<div class="modal" onclick={(e) => e.stopPropagation()} onkeydown={(e) => e.key === 'Escape' && (selectedProposalForAction = null)} role="dialog" tabindex="-1">
					{#if decider.currentPhase === 'challenging'}
						<h3>⚠️ Challenge Proposal</h3>
						<p class="modal-instruction">Raise a concern or identify an issue with this proposal.</p>
						<textarea
							bind:value={myChallengeInput}
							placeholder="Explain your challenge..."
							rows="4"
						></textarea>
						<div class="modal-actions">
							<button onclick={submitChallenge} class="btn-primary">Submit Challenge</button>
							<button onclick={() => selectedProposalForAction = null} class="btn-secondary">Cancel</button>
						</div>
					{/if}
					
					{#if decider.currentPhase === 'commenting'}
						<h3>💬 Comment or Modify</h3>
						<div class="modal-content">
							<div class="modal-section">
								<label for="comment-input">Add Comment:</label>
								<textarea
									id="comment-input"
									bind:value={myCommentInput}
									placeholder="Provide context or discussion..."
									rows="3"
								></textarea>
								<button onclick={submitComment} class="btn-primary">Submit Comment</button>
							</div>
							<div class="modal-divider">OR</div>
							<div class="modal-section">
								<label for="modification-input">Propose Modified Version:</label>
								<textarea
									id="modification-input"
									bind:value={myModificationInput}
									placeholder="Your improved version..."
									rows="3"
								></textarea>
								<button onclick={submitModification} class="btn-primary">Submit Modification</button>
							</div>
						</div>
						<div class="modal-actions">
							<button onclick={() => selectedProposalForAction = null} class="btn-secondary">Close</button>
						</div>
					{/if}
				</div>
			</div>
		{/if}

		<!-- Support Modal -->
		{#if supportModalProposal}
			<div class="modal-overlay" onclick={() => supportModalProposal = null} onkeydown={(e) => e.key === 'Escape' && (supportModalProposal = null)} role="button" tabindex="0">
				<div class="modal modal-large" onclick={(e) => e.stopPropagation()} onkeydown={(e) => e.key === 'Escape' && (supportModalProposal = null)} role="dialog" tabindex="-1">
					<h3>👍 Express Your Support</h3>
					<p class="modal-instruction">
						Distribute {totalPoints} points across the candidates. Give more points to versions you support more strongly.
					</p>
					
					<div class="support-allocator">
						{#each Object.keys(supportPoints) as candidate}
							<div class="candidate-support">
								<div class="candidate-content">{candidate}</div>
								<div class="points-control">
									<button onclick={() => updateSupport(candidate, (supportPoints[candidate] || 0) - 1)} class="btn-points">−</button>
									<input 
										type="number" 
										value={supportPoints[candidate] || 0}
										oninput={(e) => {
											const target = e.target as HTMLInputElement | null;
											if (target) updateSupport(candidate, parseInt(target.value) || 0);
										}}
										min="0"
										max={totalPoints}
										class="points-input"
									/>
									<button onclick={() => updateSupport(candidate, (supportPoints[candidate] || 0) + 1)} class="btn-points">+</button>
								</div>
							</div>
						{/each}
					</div>
					
					<div class="points-summary" class:over-limit={getTotalAllocated() > totalPoints}>
						<strong>Total Allocated:</strong> {getTotalAllocated()} / {totalPoints} points
					</div>
					
					<div class="modal-actions">
						<button onclick={submitSupport} class="btn-primary" disabled={getTotalAllocated() > totalPoints}>
							Submit Support
						</button>
						<button onclick={() => supportModalProposal = null} class="btn-secondary">Cancel</button>
					</div>
				</div>
			</div>
		{/if}

		<!-- Final Results -->
		{#if decider.currentPhase === 'complete' && decider.consensusResults}
			<section class="results-section">
				<h2>🏆 Final Decisions</h2>
				<p class="results-intro">
					The Decider process is complete! Here are the final decisions for each proposal:
				</p>
				<div class="results-grid">
					{#each decider.allProposals as proposal}
						<div class="result-card">
							<div class="result-header">
								<h3>Proposal by {proposal.authorPub === user.is.pub ? 'You' : proposal.authorPub.slice(0, 8) + '...'}</h3>
								<div class="original-proposal">Original: {proposal.content}</div>
							</div>
							<div class="final-decision">
								<div class="decision-label">Final Decision:</div>
								<div class="decision-text">{decider.consensusResults.get(proposal.authorPub)}</div>
							</div>
						</div>
					{/each}
				</div>
			</section>
		{/if}
	</div>
{/if}

<style>
	* {
		box-sizing: border-box;
	}

	.game-container {
		max-width: 1400px;
		margin: 0 auto;
		padding: 2rem;
		font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
	}
	
	.loading {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		min-height: 50vh;
		gap: 1rem;
	}
	
	.spinner {
		width: 50px;
		height: 50px;
		border: 4px solid #f3f3f3;
		border-top: 4px solid #667eea;
		border-radius: 50%;
		animation: spin 1s linear infinite;
	}
	
	@keyframes spin {
		0% { transform: rotate(0deg); }
		100% { transform: rotate(360deg); }
	}
	
	/* Header */
	.header {
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		padding: 2rem;
		border-radius: 16px;
		margin-bottom: 2rem;
		box-shadow: 0 4px 20px rgba(102, 126, 234, 0.2);
	}
	
	.header h1 {
		margin: 0 0 1.5rem 0;
		font-size: 2rem;
	}
	
	.game-info-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1rem;
	}
	
	.info-card {
		background: rgba(255, 255, 255, 0.15);
		padding: 1rem;
		border-radius: 8px;
		backdrop-filter: blur(10px);
	}
	
	.info-label {
		font-size: 0.85rem;
		opacity: 0.9;
		margin-bottom: 0.25rem;
	}
	
	.info-value {
		font-size: 1.1rem;
		font-weight: 600;
	}
	
	/* Process Flow */
	.process-flow {
		background: white;
		padding: 2rem;
		border-radius: 16px;
		margin-bottom: 2rem;
		box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
	}
	
	.process-flow h2 {
		margin: 0 0 1.5rem 0;
		font-size: 1.5rem;
	}
	
	.flow-steps {
		display: flex;
		align-items: center;
		justify-content: space-between;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.flow-step {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 0.5rem;
		padding: 1rem;
		border-radius: 12px;
		background: #f5f5f5;
		min-width: 100px;
		transition: all 0.3s;
	}
	
	.flow-step.active {
		background: #667eea;
		color: white;
		transform: scale(1.1);
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}
	
	.flow-step.complete {
		background: #4caf50;
		color: white;
	}
	
	.step-number {
		width: 40px;
		height: 40px;
		border-radius: 50%;
		background: rgba(0, 0, 0, 0.1);
		display: flex;
		align-items: center;
		justify-content: center;
		font-weight: bold;
		font-size: 1.2rem;
	}
	
	.step-name {
		font-size: 0.9rem;
		font-weight: 600;
		text-align: center;
	}
	
	.flow-arrow {
		font-size: 1.5rem;
		color: #ccc;
		flex-shrink: 0;
	}
	
	/* Current Phase */
	.current-phase {
		background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%);
		padding: 2rem;
		border-radius: 16px;
		margin-bottom: 2rem;
		box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
	}
	
	.phase-header h2 {
		margin: 0 0 1rem 0;
		font-size: 1.75rem;
	}
	
	.phase-instructions {
		margin: 0;
		font-size: 1.1rem;
		line-height: 1.6;
	}
	
	/* Participants */
	.participants-section {
		background: white;
		padding: 1.5rem;
		border-radius: 12px;
		margin-bottom: 2rem;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.06);
	}
	
	.participants-section h3 {
		margin: 0 0 1rem 0;
	}
	
	.participants-list {
		display: flex;
		flex-wrap: wrap;
		gap: 0.75rem;
	}
	
	.participant-badge {
		background: #e3f2fd;
		color: #1976d2;
		padding: 0.5rem 1rem;
		border-radius: 20px;
		font-size: 0.9rem;
		font-weight: 600;
	}
	
	.participant-badge.is-you {
		background: #667eea;
		color: white;
	}
	
	/* Action Section */
	.action-section {
		background: white;
		padding: 2rem;
		border-radius: 16px;
		margin-bottom: 2rem;
		box-shadow: 0 2px 12px rgba(0, 0, 0, 0.08);
	}
	
	.action-section h3 {
		margin: 0 0 1rem 0;
	}
	
	.input-group {
		display: flex;
		gap: 1rem;
	}
	
	.input-group input {
		flex: 1;
		padding: 0.75rem 1rem;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 1rem;
		transition: border-color 0.2s;
	}
	
	.input-group input:focus {
		outline: none;
		border-color: #667eea;
	}
	
	/* Proposals */
	.proposals-section {
		margin-bottom: 2rem;
	}
	
	.proposals-section h2 {
		margin: 0 0 1.5rem 0;
	}
	
	.proposals-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(400px, 1fr));
		gap: 1.5rem;
	}
	
	.proposal-card {
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 16px;
		padding: 1.5rem;
		transition: all 0.3s;
	}
	
	.proposal-card:hover {
		border-color: #667eea;
		box-shadow: 0 4px 20px rgba(102, 126, 234, 0.15);
		transform: translateY(-2px);
	}
	
	.proposal-card.has-early-exit {
		border-color: #4caf50;
		background: linear-gradient(135deg, #ffffff 0%, #f1f8f4 100%);
	}
	
	.proposal-header {
		margin-bottom: 1rem;
		padding-bottom: 1rem;
		border-bottom: 2px solid #f0f0f0;
	}
	
	.proposal-header h3 {
		margin: 0 0 0.75rem 0;
		font-size: 1.25rem;
		color: #333;
	}
	
	.proposal-meta {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}
	
	.author-badge {
		display: inline-block;
		background: #e3f2fd;
		color: #1976d2;
		padding: 0.25rem 0.75rem;
		border-radius: 12px;
		font-size: 0.85rem;
		font-weight: 600;
	}
	
	.status-badge {
		display: inline-block;
		padding: 0.25rem 0.75rem;
		border-radius: 12px;
		font-size: 0.85rem;
		font-weight: 600;
	}
	
	.status-pass-early {
		background: #c8e6c9;
		color: #2e7d32;
	}
	
	.status-pass-as-is {
		background: #fff9c4;
		color: #f57f17;
	}
	
	.data-section {
		margin-top: 1rem;
		padding: 1rem;
		border-radius: 12px;
		background: #f8f9fa;
	}
	
	.data-section h4 {
		margin: 0 0 0.75rem 0;
		font-size: 1rem;
	}
	
	.challenges h4 { color: #f44336; }
	.comments h4 { color: #2196f3; }
	.modifications h4 { color: #ff9800; }
	.support-summary h4 { color: #4caf50; }
	
	.data-item {
		background: white;
		padding: 0.75rem;
		margin-top: 0.5rem;
		border-radius: 8px;
		border-left: 3px solid #667eea;
	}
	
	.data-item p {
		margin: 0 0 0.5rem 0;
	}
	
	.data-item small {
		color: #666;
		font-size: 0.85rem;
	}
	
	.winner-box {
		background: #4caf50;
		color: white;
		padding: 1rem;
		border-radius: 8px;
		margin-top: 0.75rem;
	}
	
	.winner-text {
		font-size: 1.1rem;
		font-weight: 600;
		margin-top: 0.5rem;
	}
	
	.proposal-actions {
		display: flex;
		gap: 0.75rem;
		margin-top: 1rem;
		flex-wrap: wrap;
	}
	
	/* Buttons */
	button {
		padding: 0.75rem 1.5rem;
		border: none;
		border-radius: 8px;
		font-size: 1rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
	}
	
	button:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
	}
	
	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	.btn-primary {
		background: #667eea;
		color: white;
	}
	
	.btn-secondary {
		background: #e0e0e0;
		color: #333;
	}
	
	.btn-action {
		padding: 0.5rem 1rem;
		font-size: 0.9rem;
	}
	
	.btn-challenge { background: #f44336; color: white; }
	.btn-comment { background: #2196f3; color: white; }
	.btn-modify { background: #ff9800; color: white; }
	.btn-support { background: #4caf50; color: white; }
	
	/* Modal */
	.modal-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.6);
		display: flex;
		align-items: center;
		justify-content: center;
		z-index: 1000;
		padding: 1rem;
	}
	
	.modal {
		background: white;
		padding: 2rem;
		border-radius: 16px;
		max-width: 600px;
		width: 100%;
		max-height: 90vh;
		overflow-y: auto;
		box-shadow: 0 8px 32px rgba(0, 0, 0, 0.2);
	}
	
	.modal-large {
		max-width: 800px;
	}
	
	.modal h3 {
		margin: 0 0 1rem 0;
	}
	
	.modal-instruction {
		margin: 0 0 1.5rem 0;
		color: #666;
		line-height: 1.6;
	}
	
	.modal textarea {
		width: 100%;
		padding: 0.75rem;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 1rem;
		font-family: inherit;
		resize: vertical;
		margin-bottom: 1rem;
	}
	
	.modal textarea:focus {
		outline: none;
		border-color: #667eea;
	}
	
	.modal-content {
		margin: 1.5rem 0;
	}
	
	.modal-section {
		margin-bottom: 1.5rem;
	}
	
	.modal-section label {
		display: block;
		font-weight: 600;
		margin-bottom: 0.5rem;
	}
	
	.modal-divider {
		text-align: center;
		color: #999;
		font-weight: 600;
		margin: 1.5rem 0;
	}
	
	.modal-actions {
		display: flex;
		gap: 1rem;
		justify-content: flex-end;
		margin-top: 2rem;
	}
	
	/* Support Allocator */
	.support-allocator {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		margin: 1.5rem 0;
	}
	
	.candidate-support {
		background: #f8f9fa;
		padding: 1rem;
		border-radius: 12px;
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
	}
	
	.candidate-content {
		flex: 1;
		font-weight: 500;
	}
	
	.points-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}
	
	.btn-points {
		width: 36px;
		height: 36px;
		padding: 0;
		background: #667eea;
		color: white;
		font-size: 1.25rem;
	}
	
	.points-input {
		width: 60px;
		text-align: center;
		padding: 0.5rem;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		font-size: 1rem;
		font-weight: 600;
	}
	
	.points-summary {
		background: #e3f2fd;
		padding: 1rem;
		border-radius: 8px;
		text-align: center;
		font-size: 1.1rem;
	}
	
	.points-summary.over-limit {
		background: #ffebee;
		color: #c62828;
	}
	
	/* Results */
	.results-section {
		background: linear-gradient(135deg, #ffecd2 0%, #fcb69f 100%);
		padding: 2rem;
		border-radius: 16px;
		margin-top: 2rem;
	}
	
	.results-section h2 {
		margin: 0 0 1rem 0;
	}
	
	.results-intro {
		margin: 0 0 1.5rem 0;
		font-size: 1.1rem;
	}
	
	.results-grid {
		display: grid;
		gap: 1.5rem;
	}
	
	.result-card {
		background: white;
		padding: 2rem;
		border-radius: 12px;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
	}
	
	.result-header h3 {
		margin: 0 0 0.5rem 0;
	}
	
	.original-proposal {
		color: #666;
		font-size: 0.95rem;
		font-style: italic;
	}
	
	.final-decision {
		background: #4caf50;
		color: white;
		padding: 1.5rem;
		border-radius: 12px;
		margin-top: 1rem;
	}
	
	.decision-label {
		font-size: 0.9rem;
		opacity: 0.9;
		margin-bottom: 0.5rem;
	}
	
	.decision-text {
		font-size: 1.5rem;
		font-weight: bold;
	}
	
	@media (max-width: 768px) {
		.game-container {
			padding: 1rem;
		}
		
		.proposals-grid {
			grid-template-columns: 1fr;
		}
		
		.flow-steps {
			flex-direction: column;
		}
		
		.flow-arrow {
			transform: rotate(90deg);
		}
	}
</style>
