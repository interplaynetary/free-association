/**
 * Council Systems - Unified Export
 * 
 * Provides both traditional proposal-based councils and recognition-based councils.
 */

// Traditional proposal-based councils
export {
	createCouncil,
	type CouncilProxy,
	type Delegate,
	type Member,
	type Proposal,
	type Action,
	type VoteDecision,
	type VoteResult,
	type ProposalStatus,
	type ResponseStatus
} from './councils';

// Recognition-based councils
export {
	RecognitionCouncil,
	createRecognitionCouncil,
	createAndRegisterCouncil,
	createRecognitionData,
	printMembershipStatus,
	printAllocationSummary,
	type RecognitionCouncilConfig,
	type CouncilMembershipStatus,
	type CouncilAllocationSummary
} from './recognition-council';

// Hybrid councils (proposals + recognition)
export {
	HybridCouncil,
	createHybridCouncil,
	printProposalStatus,
	printProposalsSummary
} from './hybrid-council';

// Re-export types from dependencies
export type {
	RecognitionData,
	MembershipOutput
} from '$lib/protocol/collective/schemas';

export type {
	ComplianceFilter
} from '$lib/protocol/utils/filters';

export type {
	Organization,
	AvailabilitySlot,
	NeedSlot,
	Node,
	RootNode
} from '$lib/protocol/schemas';

