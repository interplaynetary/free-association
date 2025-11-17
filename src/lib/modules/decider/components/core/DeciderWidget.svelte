<script lang="ts">
	/**
	 * @component DeciderWidget
	 * Main entry point for Decider - embeddable decision-making widget
	 */
	
	import { onMount, onDestroy } from 'svelte';
	import { ReactiveP2PDecider, getEffectivePhaseTime, type ProposedConfigChanges, type AgendaItem, type ProposalData, type ChallengeData, type CommentData, type ModificationProposalData, type SupportExpression } from '../../decider.svelte';
	import { isTimedPhase } from '../../utils/type-guards';
	
	// Import components
	import LoadingState from '../shared/LoadingState.svelte';
	import PhaseTimer from '../shared/PhaseTimer.svelte';
	import DeciderHeader from '../header/DeciderHeader.svelte';
	import DeciderTabs from '../navigation/DeciderTabs.svelte';
	import AgendaNavigation from '../navigation/AgendaNavigation.svelte';
	import ProposalCarousel from '../navigation/ProposalCarousel.svelte';
	import QuickActions from '../navigation/QuickActions.svelte';
	import ProposalCardMini from '../proposal/ProposalCardMini.svelte';
	import ProposalCardExpanded from '../proposal/ProposalCardExpanded.svelte';
	import ProposingPhaseCard from '../phases/ProposingPhaseCard.svelte';
	import ChallengeCard from '../phases/ChallengeCard.svelte';
	import CommentingCard from '../phases/CommentingCard.svelte';
	import SupportCard from '../phases/SupportCard.svelte';
	import ConfigProposalForm from '../forms/ConfigProposalForm.svelte';
	import ActionModal from '../modals/ActionModal.svelte';
	import ExpandedProposalOverlay from '../modals/ExpandedProposalOverlay.svelte';
	
	interface Props {
		user: any;
		gameId: string;
		variant?: 'compact' | 'inline' | 'full';
		agenda?: (string | AgendaItem)[];
		timeWindow?: number;
	}
	
	let { 
		user, 
		gameId, 
		variant = 'inline',
		agenda = ['What should we decide?'],
		timeWindow = 86400000
	}: Props = $props();
	
	// Core state
	let decider = $state<ReactiveP2PDecider | null>(null);
	let isInitialized = $state(false);
	let initError = $state<string | null>(null);
	
	// UI state
	let activeTab = $state('proposals');
	let selectedProposalIndex = $state(0);
	let expandedProposalPub = $state<string | null>(null);
	let expandedOverlayOpen = $state(false);
	let actionModalOpen = $state(false);
	let currentAction = $state<{ type: string; proposalPub?: string } | null>(null);
	let configProposalModalOpen = $state(false);
	
	// Loading and feedback state
	let isSubmitting = $state(false);
	let feedbackMessage = $state<{ type: 'success' | 'error'; text: string } | null>(null);
	let feedbackTimeout: ReturnType<typeof setTimeout> | null = null;
	
	// Store references - safely access stores
	const currentPhaseStore = $derived(decider?.currentPhase);
	const allProposalsStore = $derived(decider?.allProposals);
	const allChallengesStore = $derived(decider?.allChallenges);
	const allCommentsStore = $derived(decider?.allComments);
	const allModificationsStore = $derived(decider?.allModifications);
	const allSupportStore = $derived(decider?.allSupport);
	const consensusResultsStore = $derived(decider?.consensusResults);
	
	// Derived state with proper null safety
	const currentUserPub = $derived(user?.is?.pub || '');
	const config = $derived(decider?.config);
	const configTimeWindow = $derived(config?.timeWindow ?? 86400000);
	
	// Derived values from stores (safe access with fallbacks)
	const currentPhase = $derived.by((): string => {
		return currentPhaseStore && $currentPhaseStore ? $currentPhaseStore : 'proposing';
	});
	const allProposals = $derived.by((): ProposalData[] => {
		return allProposalsStore && $allProposalsStore ? $allProposalsStore : [];
	});
	const allChallenges = $derived.by((): Map<string, ChallengeData[]> => {
		return allChallengesStore && $allChallengesStore ? $allChallengesStore : new Map();
	});
	const allComments = $derived.by((): Map<string, CommentData[]> => {
		return allCommentsStore && $allCommentsStore ? $allCommentsStore : new Map();
	});
	const allModifications = $derived.by((): Map<string, ModificationProposalData[]> => {
		return allModificationsStore && $allModificationsStore ? $allModificationsStore : new Map();
	});
	const allSupport = $derived.by((): Map<string, SupportExpression[]> => {
		return allSupportStore && $allSupportStore ? $allSupportStore : new Map();
	});
	const consensusResults = $derived.by((): Map<string, any> => {
		return consensusResultsStore && $consensusResultsStore ? $consensusResultsStore : new Map();
	});
	
	// Optimized derived state (cached computations)
	const selectedProposal = $derived(allProposals[selectedProposalIndex]);
	
	const selectedChallenges = $derived<ChallengeData[]>(
		selectedProposal ? (allChallenges.get(selectedProposal.authorPub) ?? []) : []
	);
	const selectedComments = $derived<CommentData[]>(
		selectedProposal ? (allComments.get(selectedProposal.authorPub) ?? []) : []
	);
	const selectedModifications = $derived<ModificationProposalData[]>(
		selectedProposal ? (allModifications.get(selectedProposal.authorPub) ?? []) : []
	);
	const selectedSupport = $derived<SupportExpression[]>(
		selectedProposal ? (allSupport.get(selectedProposal.authorPub) ?? []) : []
	);
	const selectedConsensus = $derived(
		selectedProposal ? consensusResults.get(selectedProposal.authorPub) : undefined
	);
	
	// Phase timing (type-safe)
	const phaseStartTime = $derived(config?.phaseStartTime ?? Date.now());
	const phaseDuration = $derived(
		config && currentPhase && isTimedPhase(currentPhase as any)
			? getEffectivePhaseTime(config, currentPhase as any)
			: 0
	);
	
	// Computed state for UI
	const submittedParticipants = $derived(new Set(allProposals.map(p => p.authorPub)));
	
	const pendingActionCount = $derived.by(() => {
		if (!decider || currentPhase === 'complete') return 0;
		
		let count = 0;
		const myPub = currentUserPub;
		
		// Check if user hasn't acted on proposals
		for (const proposal of allProposals) {
			const proposalPub = proposal.authorPub;
			
			if (currentPhase === 'challenging') {
				const challenges = allChallenges.get(proposalPub) ?? [];
				if (!challenges.some(c => c.authorPub === myPub)) count++;
			} else if (currentPhase === 'commenting') {
				const comments = allComments.get(proposalPub) ?? [];
				if (!comments.some(c => c.authorPub === myPub)) count++;
			} else if (currentPhase === 'supporting') {
				const support = allSupport.get(proposalPub) ?? [];
				if (!support.some(s => Object.keys(s).includes(myPub))) count++;
			}
		}
		
		return count;
	});
	
	// Feedback helpers
	function showFeedback(type: 'success' | 'error', text: string) {
		if (feedbackTimeout) {
			clearTimeout(feedbackTimeout);
		}
		feedbackMessage = { type, text };
		feedbackTimeout = setTimeout(() => {
			feedbackMessage = null;
		}, 3000);
	}
	
	// Input validation
	function validateContent(content: string, maxLength = 5000): boolean {
		if (!content || content.trim().length === 0) {
			showFeedback('error', 'Content cannot be empty');
			return false;
		}
		if (content.length > maxLength) {
			showFeedback('error', `Content exceeds maximum length of ${maxLength} characters`);
			return false;
		}
		return true;
	}
	
	// Initialize
	onMount(async () => {
		try {
			const newDecider = new ReactiveP2PDecider(user, gameId);
			await newDecider.createGame(agenda, [], timeWindow);
			decider = newDecider;
			isInitialized = true;
		} catch (e) {
			console.error('Failed to initialize Decider:', e);
			initError = e instanceof Error ? e.message : 'Failed to initialize';
		}
	});
	
	onDestroy(() => {
		if (feedbackTimeout) {
			clearTimeout(feedbackTimeout);
		}
		if (decider) {
			decider.destroy();
		}
	});
	
	// Action handlers with error handling and feedback
	async function handleSubmitProposal(content: string) {
		if (!decider) return;
		if (!validateContent(content)) return;
		
		isSubmitting = true;
		try {
			await decider.writeMyProposal(content);
			showFeedback('success', 'Proposal submitted successfully');
		} catch (e) {
			console.error('Failed to submit proposal:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit proposal');
		} finally {
			isSubmitting = false;
		}
	}
	
	function handleChallengeProposal(proposalPub: string) {
		currentAction = { type: 'challenge', proposalPub };
		actionModalOpen = true;
	}
	
	async function handleSubmitChallenge(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		if (!validateContent(content)) return;
		
		isSubmitting = true;
		try {
			await decider.writeMyChallengeToProposal(currentAction.proposalPub, content);
			showFeedback('success', 'Challenge submitted successfully');
			actionModalOpen = false;
			currentAction = null;
		} catch (e) {
			console.error('Failed to submit challenge:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit challenge');
		} finally {
			isSubmitting = false;
		}
	}
	
	function handleCommentProposal(proposalPub: string) {
		currentAction = { type: 'comment', proposalPub };
		actionModalOpen = true;
	}
	
	async function handleSubmitComment(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		if (!validateContent(content)) return;
		
		isSubmitting = true;
		try {
			await decider.writeMyCommentOnProposal(currentAction.proposalPub, content);
			showFeedback('success', 'Comment submitted successfully');
			actionModalOpen = false;
			currentAction = null;
		} catch (e) {
			console.error('Failed to submit comment:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit comment');
		} finally {
			isSubmitting = false;
		}
	}
	
	async function handleSubmitModification(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		if (!validateContent(content)) return;
		
		isSubmitting = true;
		try {
			await decider.writeMyModificationToProposal(currentAction.proposalPub, content);
			showFeedback('success', 'Modification submitted successfully');
			actionModalOpen = false;
			currentAction = null;
		} catch (e) {
			console.error('Failed to submit modification:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit modification');
		} finally {
			isSubmitting = false;
		}
	}
	
	function handleSupportProposal(proposalPub: string) {
		currentAction = { 
			type: 'support', 
			proposalPub,
		};
		actionModalOpen = true;
	}
	
	async function handleSubmitSupport(allocation: Record<string, number>) {
		if (!decider || !currentAction?.proposalPub) return;
		
		isSubmitting = true;
		try {
			await decider.writeMySupportForProposal(currentAction.proposalPub, allocation);
			showFeedback('success', 'Support submitted successfully');
			actionModalOpen = false;
			currentAction = null;
		} catch (e) {
			console.error('Failed to submit support:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit support');
		} finally {
			isSubmitting = false;
		}
	}
	
	function getProposalStatus(proposalPub: string): 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete' {
		if (!decider) return 'in-process';
		
		const challenges = allChallenges.get(proposalPub) || [];
		const modifications = allModifications.get(proposalPub) || [];
		
		if (currentPhase === 'complete') return 'complete';
		if (challenges.length === 0) return 'passed-no-challenges';
		if (challenges.length > 0 && modifications.length === 0 && currentPhase !== 'commenting') {
			return 'passed-as-is';
		}
		if (modifications.length > 0 && currentPhase === 'supporting') return 'awaiting-support';
		return 'in-process';
	}
	
	function handleExpandProposal(proposalPub: string) {
		expandedProposalPub = proposalPub;
		expandedOverlayOpen = true;
	}
	
	function handleCloseExpanded() {
		expandedOverlayOpen = false;
		expandedProposalPub = null;
	}
	
	function handleQuickAction() {
		if (!decider) return;
		
		if (currentPhase === 'proposing') {
			activeTab = 'proposals';
		} else if (allProposals.length > 0) {
			const firstProposal = allProposals[0];
			if (currentPhase === 'challenging') {
				handleChallengeProposal(firstProposal.authorPub);
			} else if (currentPhase === 'commenting') {
				handleCommentProposal(firstProposal.authorPub);
			} else if (currentPhase === 'supporting') {
				handleSupportProposal(firstProposal.authorPub);
			}
		}
	}
	
	function getChallengeCount(proposalPub: string): number {
		return allChallenges.get(proposalPub)?.length ?? 0;
	}
	
	function getCommentCount(proposalPub: string): number {
		return allComments.get(proposalPub)?.length ?? 0;
	}
	
	function getModificationCount(proposalPub: string): number {
		return allModifications.get(proposalPub)?.length ?? 0;
	}
	
	// Config proposal handlers
	function openConfigProposal() {
		configProposalModalOpen = true;
	}
	
	async function handleSubmitConfigProposal(description: string, changes: ProposedConfigChanges) {
		if (!decider) return;
		if (!validateContent(description, 1000)) return;
		
		isSubmitting = true;
		try {
			await decider.writeMyConfigProposal(description, changes);
			showFeedback('success', 'Configuration proposal submitted successfully');
			configProposalModalOpen = false;
		} catch (e) {
			console.error('Failed to submit config proposal:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to submit config proposal');
		} finally {
			isSubmitting = false;
		}
	}
	
	function handleCancelConfigProposal() {
		configProposalModalOpen = false;
	}
	
	// Agenda navigation implementation
	async function handleAgendaNavigate(index: number) {
		if (!decider || !config) return;
		if (index < 0 || index >= config.agenda.length) {
			showFeedback('error', 'Invalid agenda index');
			return;
		}
		
		isSubmitting = true;
		try {
			// Update config to change current agenda index
			await decider.writeMyConfigProposal(
				`Navigate to agenda item ${index}`,
				{ targetAgendaIndex: index }
			);
			showFeedback('success', `Navigated to agenda item ${index + 1}`);
		} catch (e) {
			console.error('Failed to navigate agenda:', e);
			showFeedback('error', e instanceof Error ? e.message : 'Failed to navigate agenda');
		} finally {
			isSubmitting = false;
		}
	}
	
	// Proposal lookup with caching
	function getProposalByPub(proposalPub: string): ProposalData | undefined {
		return allProposals.find(p => p.authorPub === proposalPub);
	}
</script>

<div class="decider-widget" class:compact={variant === 'compact'} class:inline={variant === 'inline'} class:full={variant === 'full'}>
	<!-- Feedback Toast -->
	{#if feedbackMessage}
		<div class="feedback-toast" class:success={feedbackMessage.type === 'success'} class:error={feedbackMessage.type === 'error'}>
			{feedbackMessage.text}
		</div>
	{/if}
	
	<!-- Loading Overlay -->
	{#if isSubmitting}
		<div class="loading-overlay">
			<div class="loading-spinner"></div>
			<p>Submitting...</p>
		</div>
	{/if}
	
	{#if !isInitialized || !decider}
		<LoadingState message={initError || 'Initializing Decider...'} />
	{:else if config}
		<!-- Header -->
		{@const agendaItem = config.agenda[config.currentAgendaIndex]}
		<DeciderHeader
			agendaItem={typeof agendaItem === 'string' ? agendaItem : agendaItem.text}
			currentPhase={currentPhase}
			phaseStartTime={phaseStartTime}
			timeWindow={configTimeWindow}
			participants={config.participants}
			currentUserPub={currentUserPub}
			compact={variant === 'compact'}
		/>
		
		<!-- Phase Timer -->
		{#if phaseStartTime && phaseDuration > 0}
			<PhaseTimer 
				{phaseStartTime}
				{phaseDuration}
				currentPhase={currentPhase}
				compact={variant === 'compact'}
			/>
		{/if}
		
		<!-- Agenda Navigation -->
		{#if variant !== 'compact' && config.agenda.length > 1}
			<AgendaNavigation
				agenda={config.agenda}
				currentIndex={config.currentAgendaIndex}
				onNavigate={handleAgendaNavigate}
			/>
		{/if}
		
		<!-- Meta-Governance Button -->
		{#if variant !== 'compact'}
			<button 
				class="meta-proposal-btn" 
				onclick={openConfigProposal}
				disabled={isSubmitting}
				aria-label="Propose Configuration Change"
			>
				⚙️ Propose Configuration Change
			</button>
		{/if}
		
		<!-- Tabs (not shown in compact mode) -->
		{#if variant !== 'compact'}
			<DeciderTabs bind:activeTab actionCount={pendingActionCount} />
		{/if}
		
		<!-- Main Content Area -->
		<div class="content-area">
			{#if activeTab === 'proposals'}
				<!-- Proposing Phase: Show input card -->
				{#if currentPhase === 'proposing'}
					<ProposingPhaseCard
						onSubmit={handleSubmitProposal}
						participants={config.participants}
						submittedParticipants={submittedParticipants}
					/>
				{/if}
				
				<!-- Proposal Grid or Carousel -->
				{#if allProposals.length > 0}
					{#if variant === 'compact'}
						<!-- Compact: Show single proposal with carousel -->
						<div class="compact-view">
							{#if selectedProposal?.content}
								<ProposalCardMini
									proposal={{content: selectedProposal.content, authorPub: selectedProposal.authorPub}}
									{currentUserPub}
									challengeCount={getChallengeCount(selectedProposal.authorPub)}
									commentCount={getCommentCount(selectedProposal.authorPub)}
									modificationCount={getModificationCount(selectedProposal.authorPub)}
									status={getProposalStatus(selectedProposal.authorPub)}
									onExpand={() => handleExpandProposal(selectedProposal.authorPub)}
								/>
							{/if}
							<ProposalCarousel
								proposals={allProposals}
								bind:selectedIndex={selectedProposalIndex}
							/>
						</div>
					{:else}
						<!-- Inline/Full: Show grid of mini cards -->
						<div class="proposals-grid">
							{#each allProposals as proposal (proposal.authorPub)}
								{#if proposal.content}
									<ProposalCardMini
										proposal={{content: proposal.content, authorPub: proposal.authorPub}}
										{currentUserPub}
										challengeCount={getChallengeCount(proposal.authorPub)}
										commentCount={getCommentCount(proposal.authorPub)}
										modificationCount={getModificationCount(proposal.authorPub)}
										status={getProposalStatus(proposal.authorPub)}
										onExpand={() => handleExpandProposal(proposal.authorPub)}
									/>
								{/if}
							{/each}
						</div>
					{/if}
				{/if}
			{/if}
		</div>
		
		<!-- Quick Actions (floating button) -->
		<QuickActions
			{currentPhase}
			pendingCount={pendingActionCount}
			onActionClick={handleQuickAction}
		/>
		
		<!-- Action Modal -->
		<ActionModal bind:isOpen={actionModalOpen}>
			{#if currentAction?.type === 'challenge' && currentAction.proposalPub}
				{@const proposal = getProposalByPub(currentAction.proposalPub)}
				{#if proposal?.content}
					<ChallengeCard
						proposalContent={proposal.content}
						proposalAuthor={proposal.authorPub}
						onSubmit={handleSubmitChallenge}
						onCancel={() => { actionModalOpen = false; currentAction = null; }}
					/>
				{/if}
			{:else if currentAction?.type === 'comment' && currentAction.proposalPub}
				{@const proposal = getProposalByPub(currentAction.proposalPub)}
				{@const challenges = allChallenges.get(currentAction.proposalPub) ?? []}
				{#if proposal?.content}
					<CommentingCard
						proposalContent={proposal.content}
						{challenges}
						onSubmitComment={handleSubmitComment}
						onSubmitModification={handleSubmitModification}
						onClose={() => { actionModalOpen = false; currentAction = null; }}
					/>
				{/if}
			{:else if currentAction?.type === 'support' && currentAction.proposalPub}
				{@const proposal = getProposalByPub(currentAction.proposalPub)}
				{@const modifications = allModifications.get(currentAction.proposalPub) ?? []}
				{#if proposal?.content}
					{@const candidates = [proposal.content, ...modifications.map(m => m.content)]}
					<SupportCard
						{candidates}
						totalPoints={10}
						onSubmit={handleSubmitSupport}
						onCancel={() => { actionModalOpen = false; currentAction = null; }}
					/>
				{/if}
			{/if}
		</ActionModal>
		
		<!-- Expanded Proposal Overlay -->
		<ExpandedProposalOverlay bind:isOpen={expandedOverlayOpen} onClose={handleCloseExpanded}>
			{#if expandedProposalPub}
				{@const proposal = getProposalByPub(expandedProposalPub)}
				{#if proposal?.content}
					{@const challenges = allChallenges.get(expandedProposalPub) ?? []}
					{@const comments = allComments.get(expandedProposalPub) ?? []}
					{@const modifications = allModifications.get(expandedProposalPub) ?? []}
					{@const support = allSupport.get(expandedProposalPub) ?? []}
					{@const consensus = consensusResults.get(expandedProposalPub)}
					
					<ProposalCardExpanded
						proposal={{content: proposal.content, authorPub: proposal.authorPub}}
						{currentUserPub}
						{currentPhase}
						{challenges}
						{comments}
						{modifications}
						supportExpressions={support}
						consensusResult={consensus}
						onChallenge={() => { expandedOverlayOpen = false; handleChallengeProposal(expandedProposalPub!); }}
						onComment={() => { expandedOverlayOpen = false; handleCommentProposal(expandedProposalPub!); }}
						onModify={() => { expandedOverlayOpen = false; handleCommentProposal(expandedProposalPub!); }}
						onSupport={() => { expandedOverlayOpen = false; handleSupportProposal(expandedProposalPub!); }}
					/>
				{/if}
			{/if}
		</ExpandedProposalOverlay>
		
		<!-- Config Proposal Modal -->
		<ActionModal bind:isOpen={configProposalModalOpen}>
			<ConfigProposalForm
				agenda={config.agenda}
				onSubmit={handleSubmitConfigProposal}
				onCancel={handleCancelConfigProposal}
			/>
		</ActionModal>
	{/if}
</div>

<style>
	.decider-widget {
		position: relative;
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
		padding: 1.5rem;
		background: var(--bg-light, #f8f9fa);
		border-radius: 1rem;
		font-family: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
		max-width: 100%;
	}
	
	.decider-widget.compact {
		max-width: 25rem;
		padding: 1rem;
		gap: 1rem;
	}
	
	.decider-widget.inline {
		max-width: 60rem;
		margin: 0 auto;
	}
	
	.decider-widget.full {
		max-width: 80rem;
		margin: 0 auto;
		min-height: 100vh;
	}
	
	/* Feedback Toast */
	.feedback-toast {
		position: fixed;
		top: 2rem;
		right: 2rem;
		padding: 1rem 1.5rem;
		border-radius: 0.5rem;
		font-weight: 500;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		z-index: 1000;
		animation: slideIn 0.3s ease-out;
	}
	
	.feedback-toast.success {
		background: #10b981;
		color: white;
	}
	
	.feedback-toast.error {
		background: #ef4444;
		color: white;
	}
	
	@keyframes slideIn {
		from {
			transform: translateX(100%);
			opacity: 0;
		}
		to {
			transform: translateX(0);
			opacity: 1;
		}
	}
	
	/* Loading Overlay */
	.loading-overlay {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		background: rgba(0, 0, 0, 0.5);
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		gap: 1rem;
		z-index: 999;
	}
	
	.loading-overlay p {
		color: white;
		font-weight: 600;
		font-size: 1.125rem;
	}
	
	.loading-spinner {
		width: 3rem;
		height: 3rem;
		border: 4px solid rgba(255, 255, 255, 0.3);
		border-top-color: white;
		border-radius: 50%;
		animation: spin 0.8s linear infinite;
	}
	
	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}
	
	.content-area {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}
	
	.compact-view {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}
	
	.proposals-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(20rem, 1fr));
		gap: 1rem;
	}
	
	.meta-proposal-btn {
		padding: 0.75rem 1.5rem;
		background: linear-gradient(135deg, #f59e0b 0%, #ef4444 100%);
		color: white;
		border: none;
		border-radius: 0.5rem;
		font-weight: 600;
		font-size: 0.875rem;
		cursor: pointer;
		transition: all 0.2s;
		box-shadow: 0 2px 8px rgba(245, 158, 11, 0.2);
	}
	
	.meta-proposal-btn:hover:not(:disabled) {
		transform: translateY(-2px);
		box-shadow: 0 4px 12px rgba(245, 158, 11, 0.3);
	}
	
	.meta-proposal-btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}
	
	@media (max-width: 640px) {
		.decider-widget {
			padding: 1rem;
			gap: 1rem;
		}
		
		.proposals-grid {
			grid-template-columns: 1fr;
		}
		
		.feedback-toast {
			top: 1rem;
			right: 1rem;
			left: 1rem;
		}
	}
</style>
