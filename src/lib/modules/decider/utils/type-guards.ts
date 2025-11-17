/**
 * @module type-guards
 * Type guards for elegant, type-safe code
 */

import type { GamePhase, AgendaItem, ProposalData } from '../decider.svelte';

/**
 * Type guard for timed phases
 */
export function isTimedPhase(phase: GamePhase): phase is 'proposing' | 'challenging' | 'commenting' | 'supporting' {
	return phase === 'proposing' || phase === 'challenging' || phase === 'commenting' || phase === 'supporting';
}

/**
 * Type guard for active phases (not complete or not_started)
 */
export function isActivePhase(phase: GamePhase): boolean {
	return phase !== 'not_started' && phase !== 'complete';
}

/**
 * Type guard for object-format agenda items
 */
export function isAgendaItemObject(item: string | AgendaItem): item is AgendaItem {
	return typeof item === 'object' && 'text' in item;
}

/**
 * Type guard for proposals with content
 */
export function hasContent(proposal: ProposalData): proposal is ProposalData & { content: string } {
	return proposal.content !== null && proposal.content.trim().length > 0;
}

/**
 * Type guard for config proposals
 */
export function isConfigProposal(proposal: ProposalData): boolean {
	return proposal.proposalType === 'config' || proposal.proposalType === 'hybrid';
}

/**
 * Type guard for content proposals
 */
export function isContentProposal(proposal: ProposalData): boolean {
	return proposal.proposalType === 'content' || proposal.proposalType === 'hybrid';
}

/**
 * Check if a value is defined (not null or undefined)
 */
export function isDefined<T>(value: T | null | undefined): value is T {
	return value !== null && value !== undefined;
}

/**
 * Check if array is non-empty
 */
export function isNonEmpty<T>(arr: T[]): arr is [T, ...T[]] {
	return arr.length > 0;
}

