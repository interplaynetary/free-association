/**
 * @module useDeciderState
 * Composable hooks for Decider state management
 */

import { derived, type Readable } from 'svelte/store';
import { get } from 'svelte/store';
import type { ReactiveP2PDecider, ProposalData, GamePhase } from '../decider.svelte';

/**
 * Extract all reactive state from a Decider instance
 * This creates a clean separation between the Decider logic and UI state
 */
export function useDeciderState(decider: ReactiveP2PDecider | null) {
	// Null-safe store access
	const stores = {
		currentPhase: decider?.currentPhase,
		allProposals: decider?.allProposals,
		allChallenges: decider?.allChallenges,
		allComments: decider?.allComments,
		allModifications: decider?.allModifications,
		allSupport: decider?.allSupport,
		consensusResults: decider?.consensusResults,
	};
	
	// Derived values with safe defaults
	return {
		currentPhase: $derived(stores.currentPhase ? get(stores.currentPhase) : 'proposing' as GamePhase),
		allProposals: $derived(stores.allProposals ? get(stores.allProposals) : []),
		allChallenges: $derived(stores.allChallenges ? get(stores.allChallenges) : new Map()),
		allComments: $derived(stores.allComments ? get(stores.allComments) : new Map()),
		allModifications: $derived(stores.allModifications ? get(stores.allModifications) : new Map()),
		allSupport: $derived(stores.allSupport ? get(stores.allSupport) : new Map()),
		consensusResults: $derived(stores.consensusResults ? get(stores.consensusResults) : new Map()),
	};
}

/**
 * Get proposal-specific data for a selected proposal
 */
export function useProposalData(
	proposal: ProposalData | undefined,
	allChallenges: Map<string, any[]>,
	allComments: Map<string, any[]>,
	allModifications: Map<string, any[]>,
	allSupport: Map<string, any[]>
) {
	return {
		challenges: $derived(proposal ? (allChallenges.get(proposal.authorPub) || []) : []),
		comments: $derived(proposal ? (allComments.get(proposal.authorPub) || []) : []),
		modifications: $derived(proposal ? (allModifications.get(proposal.authorPub) || []) : []),
		support: $derived(proposal ? (allSupport.get(proposal.authorPub) || []) : []),
	};
}

/**
 * Get proposal metadata (counts, status, etc.)
 */
export function useProposalMetadata(
	proposalPub: string,
	currentPhase: GamePhase,
	allChallenges: Map<string, any[]>,
	allModifications: Map<string, any[]>
) {
	const challenges = allChallenges.get(proposalPub) || [];
	const modifications = allModifications.get(proposalPub) || [];
	
	const status = $derived(() => {
		if (currentPhase === 'complete') return 'complete';
		if (challenges.length === 0) return 'passed-no-challenges';
		if (challenges.length > 0 && modifications.length === 0 && currentPhase !== 'commenting') {
			return 'passed-as-is';
		}
		if (modifications.length > 0 && currentPhase === 'supporting') return 'awaiting-support';
		return 'in-process';
	});
	
	return {
		challengeCount: $derived(challenges.length),
		modificationCount: $derived(modifications.length),
		status: $derived(status()),
	};
}

