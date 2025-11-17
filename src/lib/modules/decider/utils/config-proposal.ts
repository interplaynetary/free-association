/**
 * @module config-proposal
 * Elegant utilities for creating configuration proposals
 */

import type { ProposedConfigChanges, PhaseTimeConfig } from '../decider.svelte';

/**
 * Form state for configuration proposals
 */
export interface ConfigProposalFormState {
	description: string;
	scope: 'global' | 'agenda';
	targetAgendaIndex: number;
	
	// Global settings
	globalTimeWindow?: number;
	globalPhaseConfig?: Partial<PhaseTimeConfig>;
	
	// Agenda-specific settings
	agendaTimeWindow?: number;
	agendaPhaseConfig?: Partial<PhaseTimeConfig>;
}

/**
 * Convert form state to ProposedConfigChanges
 */
export function formStateToChanges(state: ConfigProposalFormState): ProposedConfigChanges {
	const changes: ProposedConfigChanges = {};
	
	if (state.scope === 'global') {
		if (state.globalTimeWindow) {
			changes.timeWindow = state.globalTimeWindow * 1000; // Convert to ms
		}
		
		if (state.globalPhaseConfig && Object.keys(state.globalPhaseConfig).length > 0) {
			changes.phaseTimeConfig = convertPhaseConfigToMs(state.globalPhaseConfig);
		}
	} else {
		changes.targetAgendaIndex = state.targetAgendaIndex;
		
		if (state.agendaTimeWindow) {
			changes.agendaItemTimeWindow = state.agendaTimeWindow * 1000;
		}
		
		if (state.agendaPhaseConfig && Object.keys(state.agendaPhaseConfig).length > 0) {
			changes.agendaItemPhaseConfig = convertPhaseConfigToMs(state.agendaPhaseConfig);
		}
	}
	
	return changes;
}

/**
 * Convert phase config from seconds to milliseconds
 */
function convertPhaseConfigToMs(config: Partial<PhaseTimeConfig>): PhaseTimeConfig {
	return {
		proposing: config.proposing ? config.proposing * 1000 : undefined,
		challenging: config.challenging ? config.challenging * 1000 : undefined,
		commenting: config.commenting ? config.commenting * 1000 : undefined,
		supporting: config.supporting ? config.supporting * 1000 : undefined,
	};
}

/**
 * Validate form state
 */
export function validateFormState(state: ConfigProposalFormState): boolean {
	if (!state.description.trim()) return false;
	
	if (state.scope === 'global') {
		return !!(state.globalTimeWindow || state.globalPhaseConfig);
	} else {
		return !!(state.agendaTimeWindow || state.agendaPhaseConfig);
	}
}

/**
 * Create empty form state
 */
export function createEmptyFormState(): ConfigProposalFormState {
	return {
		description: '',
		scope: 'global',
		targetAgendaIndex: 0,
	};
}

/**
 * Common time presets for quick selection
 */
export const TIME_PRESETS = {
	quick: { label: 'Quick (30s)', value: 30 },
	normal: { label: 'Normal (2m)', value: 120 },
	thorough: { label: 'Thorough (5m)', value: 300 },
	extended: { label: 'Extended (15m)', value: 900 },
	longForm: { label: 'Long Form (1h)', value: 3600 },
} as const;

/**
 * Common phase distribution patterns
 */
export const PHASE_PRESETS = {
	balanced: {
		label: 'Balanced (equal time)',
		config: (total: number) => ({
			proposing: total / 4,
			challenging: total / 4,
			commenting: total / 4,
			supporting: total / 4,
		})
	},
	proposalFocus: {
		label: 'Proposal Focus (50% proposing)',
		config: (total: number) => ({
			proposing: total / 2,
			challenging: total / 6,
			commenting: total / 6,
			supporting: total / 6,
		})
	},
	deliberationFocus: {
		label: 'Deliberation Focus (emphasis on discussion)',
		config: (total: number) => ({
			proposing: total / 6,
			challenging: total / 3,
			commenting: total / 3,
			supporting: total / 6,
		})
	},
	rapidConsensus: {
		label: 'Rapid Consensus (quick decisions)',
		config: (total: number) => ({
			proposing: total / 3,
			challenging: total / 6,
			commenting: total / 6,
			supporting: total / 3,
		})
	},
} as const;

