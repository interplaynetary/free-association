/**
 * @module useDeciderState
 * Composable for managing decider state logic
 * Extracts complex derived state computation from DeciderWidget
 */

import { derived, type Readable } from 'svelte/store';
import type { 
	ReactiveP2PDecider,
	ProposalData,
	ChallengeData,
	CommentData,
	ModificationProposalData,
	SupportExpression,
	GameConfig
} from '../decider.svelte';

export interface DeciderState {
	currentPhase: string;
	allProposals: ProposalData[];
	allChallenges: Map<string, ChallengeData[]>;
	allComments: Map<string, CommentData[]>;
	allModifications: Map<string, ModificationProposalData[]>;
	allSupport: Map<string, SupportExpression[]>;
	consensusResults: Map<string, any>;
	config: GameConfig | null;
}

export function useDeciderState(decider: ReactiveP2PDecider | null) {
	if (!decider) {
		return createEmptyState();
	}
	
	// Create safe derived state
	const currentPhase = $derived.by(() => 
		decider.currentPhase && $state.snapshot(decider.currentPhase) || 'proposing'
	);
	
	const allProposals = $derived.by(() => 
		decider.allProposals && $state.snapshot(decider.allProposals) || []
	);
	
	const allChallenges = $derived.by(() => 
		decider.allChallenges && $state.snapshot(decider.allChallenges) || new Map()
	);
	
	const allComments = $derived.by(() => 
		decider.allComments && $state.snapshot(decider.allComments) || new Map()
	);
	
	const allModifications = $derived.by(() => 
		decider.allModifications && $state.snapshot(decider.allModifications) || new Map()
	);
	
	const allSupport = $derived.by(() => 
		decider.allSupport && $state.snapshot(decider.allSupport) || new Map()
	);
	
	const consensusResults = $derived.by(() => 
		decider.consensusResults && $state.snapshot(decider.consensusResults) || new Map()
	);
	
	const config = $derived(decider.config);
	
	return {
		get currentPhase() { return currentPhase; },
		get allProposals() { return allProposals; },
		get allChallenges() { return allChallenges; },
		get allComments() { return allComments; },
		get allModifications() { return allModifications; },
		get allSupport() { return allSupport; },
		get consensusResults() { return consensusResults; },
		get config() { return config; }
	};
}

function createEmptyState(): DeciderState {
	return {
		currentPhase: 'proposing',
		allProposals: [],
		allChallenges: new Map(),
		allComments: new Map(),
		allModifications: new Map(),
		allSupport: new Map(),
		consensusResults: new Map(),
		config: null
	};
}




