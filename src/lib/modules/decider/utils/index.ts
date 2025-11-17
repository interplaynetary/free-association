/**
 * @module utils
 * Elegant utility functions for the Decider system
 * 
 * Organized exports for clean imports:
 * @example
 * import { formatTime, isTimedPhase, TIME_PRESETS } from '$lib/modules/decider/utils';
 */

// Time utilities
export {
	formatTime,
	calculateProgress,
	calculateRemaining,
	isUrgent,
	isExpired,
	parseTime,
	getTimeState,
	type TimeState,
} from './time';

// Type guards
export {
	isTimedPhase,
	isActivePhase,
	isAgendaItemObject,
	hasContent,
	isConfigProposal,
	isContentProposal,
	isDefined,
	isNonEmpty,
} from './type-guards';

// Config proposal utilities
export {
	formStateToChanges,
	validateFormState,
	createEmptyFormState,
	TIME_PRESETS,
	PHASE_PRESETS,
	type ConfigProposalFormState,
} from './config-proposal';

