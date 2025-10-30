<script lang="ts">
	/**
	 * @component DeciderWidget
	 * Main entry point for Decider - embeddable decision-making widget
	 */
	
	import { onMount, onDestroy } from 'svelte';
	import { ReactiveP2PDecider } from '../../decider.svelte';
	
	// Import components
	import LoadingState from '../shared/LoadingState.svelte';
	import DeciderHeader from '../header/DeciderHeader.svelte';
	import DeciderTabs from '../navigation/DeciderTabs.svelte';
	import ProposalCarousel from '../navigation/ProposalCarousel.svelte';
	import QuickActions from '../navigation/QuickActions.svelte';
	import ProposalCardMini from '../proposal/ProposalCardMini.svelte';
	import ProposalCardExpanded from '../proposal/ProposalCardExpanded.svelte';
	import ProposingPhaseCard from '../phases/ProposingPhaseCard.svelte';
	import ChallengeCard from '../phases/ChallengeCard.svelte';
	import CommentingCard from '../phases/CommentingCard.svelte';
	import SupportCard from '../phases/SupportCard.svelte';
	import ActionModal from '../modals/ActionModal.svelte';
	import ExpandedProposalOverlay from '../modals/ExpandedProposalOverlay.svelte';
	
	interface Props {
		user: any;
		gameId: string;
		variant?: 'compact' | 'inline' | 'full';
		agenda?: string[];
	}
	
	let { 
		user, 
		gameId, 
		variant = 'inline',
		agenda = ['What should we decide?']
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
	
	// Derived state
	const currentUserPub = $derived(user?.is?.pub || '');
	const currentPhase = $derived((decider as ReactiveP2PDecider | null)?.currentPhase || 'proposing');
	const allProposals = $derived((decider as ReactiveP2PDecider | null)?.allProposals || []);
	const config = $derived((decider as ReactiveP2PDecider | null)?.config);
	const timeWindow = $derived(config?.timeWindow || 86400000);
	
	const selectedProposal = $derived(allProposals[selectedProposalIndex]);
	
	// Get data for selected proposal
	const selectedChallenges = $derived(
		selectedProposal ? (decider?.allChallenges?.get(selectedProposal.authorPub) || []) : []
	);
	const selectedComments = $derived(
		selectedProposal ? (decider?.allComments?.get(selectedProposal.authorPub) || []) : []
	);
	const selectedModifications = $derived(
		selectedProposal ? (decider?.allModifications?.get(selectedProposal.authorPub) || []) : []
	);
	const selectedSupport = $derived(
		selectedProposal ? (decider?.allSupport?.get(selectedProposal.authorPub) || []) : []
	);
	const selectedConsensus = $derived(
		selectedProposal ? decider?.consensusResults?.get(selectedProposal.authorPub) : undefined
	);
	
	// Initialize
	onMount(async () => {
		try {
			const newDecider = new ReactiveP2PDecider(user, gameId);
			await newDecider.createGame(agenda);
			decider = newDecider;
			isInitialized = true;
		} catch (e) {
			console.error('Failed to initialize Decider:', e);
			initError = e instanceof Error ? e.message : 'Failed to initialize';
		}
	});
	
	onDestroy(() => {
		if (decider) {
			decider.destroy();
		}
	});
	
	// Action handlers
	async function handleSubmitProposal(content: string) {
		if (!decider) return;
		await decider.writeMyProposal(content);
	}
	
	function handleChallengeProposal(proposalPub: string) {
		currentAction = { type: 'challenge', proposalPub };
		actionModalOpen = true;
	}
	
	async function handleSubmitChallenge(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		await decider.writeMyChallengeToProposal(currentAction.proposalPub, content);
		actionModalOpen = false;
		currentAction = null;
	}
	
	function handleCommentProposal(proposalPub: string) {
		currentAction = { type: 'comment', proposalPub };
		actionModalOpen = true;
	}
	
	async function handleSubmitComment(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		await decider.writeMyCommentOnProposal(currentAction.proposalPub, content);
	}
	
	async function handleSubmitModification(content: string) {
		if (!decider || !currentAction?.proposalPub) return;
		await decider.writeMyModificationToProposal(currentAction.proposalPub, content);
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
		await decider.writeMySupportForProposal(currentAction.proposalPub, allocation);
		actionModalOpen = false;
		currentAction = null;
	}
	
	function getProposalStatus(proposalPub: string): 'passed-no-challenges' | 'passed-as-is' | 'in-process' | 'awaiting-support' | 'complete' {
		if (!decider) return 'in-process';
		
		const challenges = decider.allChallenges?.get(proposalPub) || [];
		const modifications = decider.allModifications?.get(proposalPub) || [];
		
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
		return decider?.allChallenges?.get(proposalPub)?.length || 0;
	}
	
	function getCommentCount(proposalPub: string): number {
		return decider?.allComments?.get(proposalPub)?.length || 0;
	}
	
	function getModificationCount(proposalPub: string): number {
		return decider?.allModifications?.get(proposalPub)?.length || 0;
	}
</script>

<div class="decider-widget" class:compact={variant === 'compact'} class:inline={variant === 'inline'} class:full={variant === 'full'}>
	{#if !isInitialized || !decider}
		<LoadingState message={initError || 'Initializing Decider...'} />
	{:else if config}
		<!-- Header -->
		<DeciderHeader
			agendaItem={config.agenda[config.currentAgendaIndex] as string}
			currentPhase={currentPhase as string}
			phaseStartTime={Date.now() as number}
			timeWindow={timeWindow as number}
			participants={config.participants as string[]}
			currentUserPub={currentUserPub as string}
			compact={variant === 'compact'}
		/>
		
		<!-- Tabs (not shown in compact mode) -->
		{#if variant !== 'compact'}
			<DeciderTabs bind:activeTab actionCount={0} />
		{/if}
		
		<!-- Main Content Area -->
		<div class="content-area">
			{#if activeTab === 'proposals'}
				<!-- Proposing Phase: Show input card -->
				{#if currentPhase === 'proposing'}
					<ProposingPhaseCard
						onSubmit={handleSubmitProposal}
						participants={config.participants}
						submittedParticipants={new Set(allProposals.map((p: any) => p.authorPub))}
					/>
				{/if}
				
				<!-- Proposal Grid or Carousel -->
				{#if allProposals.length > 0}
					{#if variant === 'compact'}
						<!-- Compact: Show single proposal with carousel -->
						<div class="compact-view">
							{#if selectedProposal && selectedProposal.content}
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
							{#each allProposals as proposal}
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
		{#if variant !== 'compact'}
			<QuickActions
				{currentPhase}
				pendingCount={0}
				onActionClick={handleQuickAction}
			/>
		{/if}
		
		<!-- Action Modal -->
		<ActionModal bind:isOpen={actionModalOpen}>
			{#if currentAction?.type === 'challenge' && currentAction.proposalPub}
				{@const proposal = allProposals.find((p: any) => p.authorPub === currentAction?.proposalPub)}
				{#if proposal && proposal.content}
					<ChallengeCard
						proposalContent={proposal.content}
						proposalAuthor={proposal.authorPub}
						onSubmit={handleSubmitChallenge}
						onCancel={() => { actionModalOpen = false; currentAction = null; }}
					/>
				{/if}
			{:else if currentAction?.type === 'comment' && currentAction.proposalPub}
				{@const proposal = allProposals.find((p: any) => p.authorPub === currentAction?.proposalPub)}
				{@const challenges = decider?.allChallenges?.get(currentAction?.proposalPub || '') || []}
				{#if proposal && proposal.content}
					<CommentingCard
						proposalContent={proposal.content}
						{challenges}
						onSubmitComment={handleSubmitComment}
						onSubmitModification={handleSubmitModification}
						onClose={() => { actionModalOpen = false; currentAction = null; }}
					/>
				{/if}
			{:else if currentAction?.type === 'support' && currentAction.proposalPub}
				{@const proposal = allProposals.find((p: any) => p.authorPub === currentAction?.proposalPub)}
				{@const modifications = decider?.allModifications?.get(currentAction?.proposalPub || '') || []}
				{#if proposal && proposal.content}
					{@const candidates = [proposal.content, ...modifications.map((m: any) => m.content)]}
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
				{@const proposal = allProposals.find((p: any) => p.authorPub === expandedProposalPub)}
				{#if proposal && proposal.content}
					{@const challenges = decider?.allChallenges?.get(expandedProposalPub) || []}
					{@const comments = decider?.allComments?.get(expandedProposalPub) || []}
					{@const modifications = decider?.allModifications?.get(expandedProposalPub) || []}
					{@const support = decider?.allSupport?.get(expandedProposalPub) || []}
					{@const consensus = decider?.consensusResults?.get(expandedProposalPub)}
					
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
	{/if}
</div>

<style>
	.decider-widget {
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
	
	@media (max-width: 640px) {
		.decider-widget {
			padding: 1rem;
			gap: 1rem;
		}
		
		.proposals-grid {
			grid-template-columns: 1fr;
		}
	}
</style>
