/**
 * Decider Component Library
 * 
 * A comprehensive set of Svelte 5 components for building
 * collaborative decision-making interfaces.
 */

// Core Components
export { default as DeciderWidget } from './core/DeciderWidget.svelte';

// Header Components
export { default as DeciderHeader } from './header/DeciderHeader.svelte';
export { default as PhaseIndicator } from './header/PhaseIndicator.svelte';
export { default as TimeWindowBadge } from './header/TimeWindowBadge.svelte';
export { default as ParticipantAvatars } from './header/ParticipantAvatars.svelte';

// Proposal Components
export { default as ProposalCardMini } from './proposal/ProposalCardMini.svelte';
export { default as ProposalCardExpanded } from './proposal/ProposalCardExpanded.svelte';
export { default as ProposalStatusBadge } from './proposal/ProposalStatusBadge.svelte';

// Phase-Specific Action Components
export { default as ProposingPhaseCard } from './phases/ProposingPhaseCard.svelte';
export { default as ChallengeCard } from './phases/ChallengeCard.svelte';
export { default as CommentingCard } from './phases/CommentingCard.svelte';
export { default as SupportCard } from './phases/SupportCard.svelte';

// Data Display Components
export { default as ChallengeList } from './data/ChallengeList.svelte';
export { default as CommentThread } from './data/CommentThread.svelte';
export { default as ModificationCandidates } from './data/ModificationCandidates.svelte';
export { default as SupportSummary } from './data/SupportSummary.svelte';
export { default as ConsensusResult } from './data/ConsensusResult.svelte';

// Navigation Components
export { default as ProposalCarousel } from './navigation/ProposalCarousel.svelte';
export { default as DeciderTabs } from './navigation/DeciderTabs.svelte';
export { default as QuickActions } from './navigation/QuickActions.svelte';

// Modal Components
export { default as ActionModal } from './modals/ActionModal.svelte';
export { default as ExpandedProposalOverlay } from './modals/ExpandedProposalOverlay.svelte';

// Shared/Utility Components
export { default as AuthorBadge } from './shared/AuthorBadge.svelte';
export { default as ContentCard } from './shared/ContentCard.svelte';
export { default as PointAllocator } from './shared/PointAllocator.svelte';
export { default as EmptyState } from './shared/EmptyState.svelte';
export { default as LoadingState } from './shared/LoadingState.svelte';

