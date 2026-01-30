import { z } from 'zod';
import { derived, writable, type Readable, type Writable, get } from 'svelte/store';
import { writeAtPath, readAtPath, listenAtPath } from '$lib/utils/data/meshData';
import { createStore } from '$lib/utils/primitives/store.svelte';
import { createVersionedStore, type VersionedStore } from '$lib/utils/primitives/v-store.svelte';
import { jsonEquals } from '$lib/utils/primitives/v-store-equality-checkers';
import { seed as itcSeed, event as itcEvent, join as itcJoin, type Stamp as ITCStamp } from '$lib/utils/primitives/itc';

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Remove undefined fields from an object for Gun compatibility
 * Gun/Mesh cannot handle undefined values, only null or defined values
 */
function removeUndefinedFields<T extends Record<string, any>>(obj: T): Partial<T> {
	const cleaned: any = {};
	for (const key in obj) {
		if (obj[key] !== undefined) {
			cleaned[key] = obj[key];
		}
	}
	return cleaned;
}

// ============================================================================
// ZOD SCHEMAS (with versioned store metadata)
// ============================================================================

const PlayerSchema = z.string().min(1);

// Base schemas with timestamp and ITC for versioned store compatibility
const ChallengeDataSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
	timestamp: z.number(),
	itcStamp: z.any().optional(), // ITC stamp for causality
});

const CommentDataSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
	timestamp: z.number(),
	itcStamp: z.any().optional(),
});

const ModificationProposalDataSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
	timestamp: z.number(),
	itcStamp: z.any().optional(),
});

const SupportExpressionSchema = z.record(z.string(), z.number().int().min(0));

// Phase time configuration schema (must be defined before use)
const PhaseTimeConfigSchema = z.object({
	proposing: z.number().int().positive().optional(),
	challenging: z.number().int().positive().optional(),
	commenting: z.number().int().positive().optional(),
	supporting: z.number().int().positive().optional(),
}).optional();

// Proposed configuration changes that can be part of a proposal
const ProposedConfigChangesSchema = z.object({
	timeWindow: z.number().int().positive().optional(), // Propose new global time window
	phaseTimeConfig: PhaseTimeConfigSchema, // Propose new phase time config
	agendaItemTimeWindow: z.number().int().positive().optional(), // Propose time for specific agenda item
	agendaItemPhaseConfig: PhaseTimeConfigSchema, // Propose phase config for specific agenda item
	targetAgendaIndex: z.number().int().min(0).optional(), // Which agenda item to apply changes to
}).optional();

const ProposalDataSchema = z.object({
	content: z.string().nullable(),
	authorPub: z.string(),
	proposalType: z.enum(['content', 'config', 'hybrid']), // Type of proposal (required, no default)
	proposedConfigChanges: ProposedConfigChangesSchema, // Meta-proposals: propose config changes
	challenges: z.array(ChallengeDataSchema).optional(),
	comments: z.array(CommentDataSchema).optional(),
	modificationProposals: z.array(ModificationProposalDataSchema).optional(),
	supportExpressions: z.array(SupportExpressionSchema).optional(),
	timestamp: z.number(),
	itcStamp: z.any().optional(),
});

// Agenda item schema with optional time overrides
const AgendaItemSchema = z.object({
	text: z.string(),
	timeWindow: z.number().int().positive().optional(), // Override global time window
	phaseTimeConfig: PhaseTimeConfigSchema, // Override global phase times
}).or(z.string()); // Backwards compatible with simple strings

const GameConfigSchema = z.object({
	gameId: z.string(),
	participants: z.array(z.string()),
	agenda: z.array(AgendaItemSchema),
	currentAgendaIndex: z.number().int().min(0).default(0),
	timeWindow: z.number().int().positive().default(86400000), // Global default
	phaseTimeConfig: PhaseTimeConfigSchema, // Global phase time config
	phaseStartTime: z.number().optional(), // When current phase started
	currentPhaseOverride: z.enum(['proposing', 'challenging', 'commenting', 'supporting', 'complete']).optional(),
	createdAt: z.number(),
	createdBy: z.string(),
});

// ============================================================================
// TYPE INFERENCE
// ============================================================================

type Player = z.infer<typeof PlayerSchema>;
type ChallengeData = z.infer<typeof ChallengeDataSchema>;
type CommentData = z.infer<typeof CommentDataSchema>;
type ModificationProposalData = z.infer<typeof ModificationProposalDataSchema>;
type SupportExpression = z.infer<typeof SupportExpressionSchema>;
type ProposedConfigChanges = z.infer<typeof ProposedConfigChangesSchema>;
type ProposalData = z.infer<typeof ProposalDataSchema>;
type PhaseTimeConfig = z.infer<typeof PhaseTimeConfigSchema>;
type AgendaItem = z.infer<typeof AgendaItemSchema>;
type GameConfig = z.infer<typeof GameConfigSchema>;

// Backwards compatibility aliases
type Challenge = ChallengeData;
type Comment = CommentData;
type ModificationProposal = ModificationProposalData;

type GamePhase =
	| 'not_started'
	| 'proposing'
	| 'challenging'
	| 'commenting'
	| 'supporting'
	| 'complete';

// Helper to normalize agenda items (string or object)
function normalizeAgendaItem(item: AgendaItem): { text: string; timeWindow?: number; phaseTimeConfig?: PhaseTimeConfig } {
	if (typeof item === 'string') {
		return { text: item };
	}
	return item;
}

// Get effective time window for a specific phase in a specific agenda item
function getEffectivePhaseTime(
	config: GameConfig,
	phase: GamePhase,
	agendaIndex?: number
): number {
	if (phase === 'not_started' || phase === 'complete') return 0;

	const idx = agendaIndex ?? config.currentAgendaIndex;
	const agendaItem = config.agenda[idx];
	const normalized = agendaItem ? normalizeAgendaItem(agendaItem) : null;

	// 1. Check agenda-specific phase config
	if (normalized?.phaseTimeConfig?.[phase]) {
		return normalized.phaseTimeConfig[phase]!;
	}

	// 2. Check global phase config
	if (config.phaseTimeConfig?.[phase]) {
		return config.phaseTimeConfig[phase]!;
	}

	// 3. Check agenda-specific time window (divide by 4 phases)
	if (normalized?.timeWindow) {
		return Math.floor(normalized.timeWindow / 4);
	}

	// 4. Use global time window (divide by 4 phases)
	return Math.floor(config.timeWindow / 4);
}

// Get effective total time window for an agenda item
function getEffectiveTimeWindow(config: GameConfig, agendaIndex?: number): number {
	const idx = agendaIndex ?? config.currentAgendaIndex;
	const agendaItem = config.agenda[idx];
	const normalized = agendaItem ? normalizeAgendaItem(agendaItem) : null;

	return normalized?.timeWindow ?? config.timeWindow;
}

// Apply proposed config changes to a game config (used when a meta-proposal wins)
function applyConfigChanges(
	config: GameConfig,
	proposedChanges: ProposedConfigChanges
): GameConfig {
	const newConfig = { ...config };

	if (!proposedChanges) return newConfig;

	// Apply global changes
	if (proposedChanges.timeWindow !== undefined) {
		newConfig.timeWindow = proposedChanges.timeWindow;
	}

	if (proposedChanges.phaseTimeConfig) {
		newConfig.phaseTimeConfig = {
			...newConfig.phaseTimeConfig,
			...proposedChanges.phaseTimeConfig
		};
	}

	// Apply agenda-specific changes
	if (proposedChanges.targetAgendaIndex !== undefined) {
		const targetIdx = proposedChanges.targetAgendaIndex;
		const agendaItem = newConfig.agenda[targetIdx];

		if (agendaItem) {
			const normalized = normalizeAgendaItem(agendaItem);

			// Build updated agenda item
			const updatedItem: { text: string; timeWindow?: number; phaseTimeConfig?: PhaseTimeConfig } = {
				text: normalized.text,
				timeWindow: proposedChanges.agendaItemTimeWindow ?? normalized.timeWindow,
				phaseTimeConfig: proposedChanges.agendaItemPhaseConfig ?? normalized.phaseTimeConfig
			};

			// Update agenda array
			newConfig.agenda = [...newConfig.agenda];
			newConfig.agenda[targetIdx] = updatedItem as AgendaItem;
		}
	}

	return newConfig;
}

// ============================================================================
// GLOBAL STORES (Module-Level)
// ============================================================================

// Game configurations (persistent, keyed by gameId)
const gameConfigsStore = createStore({
	meshPath: 'decider/games/configs',
	schema: z.record(z.string(), GameConfigSchema),
	persistDebounce: 200
});

// Network proposals (versioned, keyed by gameId:participantPub)
const networkProposals: VersionedStore<ProposalData, string> = createVersionedStore({
	fields: {
		content: (p) => p.content,
		challenges: (p) => p.challenges,
		comments: (p) => p.comments,
		modifications: (p) => p.modificationProposals,
		support: (p) => p.supportExpressions
	},
	fieldEqualityCheckers: {
		challenges: jsonEquals,
		comments: jsonEquals,
		modifications: jsonEquals,
		support: jsonEquals
	},
	schema: ProposalDataSchema,
	itcExtractor: (p) => p.itcStamp,
	timestampExtractor: (p) => p.timestamp,
	enableLogging: true
});

// Network challenges (versioned, keyed by gameId:participantPub:proposalAuthorPub)
const networkChallenges: VersionedStore<ChallengeData, string> = createVersionedStore({
	fields: { content: (c) => c.content },
	schema: ChallengeDataSchema,
	itcExtractor: (c) => c.itcStamp,
	timestampExtractor: (c) => c.timestamp,
	enableLogging: false
});

// Network comments (versioned, keyed by gameId:participantPub:proposalAuthorPub)
const networkComments: VersionedStore<CommentData, string> = createVersionedStore({
	fields: { content: (c) => c.content },
	schema: CommentDataSchema,
	itcExtractor: (c) => c.itcStamp,
	timestampExtractor: (c) => c.timestamp,
	enableLogging: false
});

// Network modifications (versioned, keyed by gameId:participantPub:proposalAuthorPub)
const networkModifications: VersionedStore<ModificationProposalData, string> = createVersionedStore({
	fields: { content: (m) => m.content },
	schema: ModificationProposalDataSchema,
	itcExtractor: (m) => m.itcStamp,
	timestampExtractor: (m) => m.timestamp,
	enableLogging: false
});

// Network support (writable store, keyed by gameId:participantPub:proposalAuthorPub)
const networkSupportState: Writable<Map<string, SupportExpression>> = writable(new Map());

// ============================================================================
// DERIVED STORES (Aggregated Views)
// ============================================================================

/**
 * Get all proposals for a specific game
 */
function getGameProposals(gameId: string): Readable<ProposalData[]> {
	return derived([networkProposals.store], ([$proposals]) => {
		const filtered: ProposalData[] = [];
		for (const [key, versionedEntity] of $proposals.entries()) {
			if (key.startsWith(`${gameId}:`)) {
				filtered.push(versionedEntity.data);
			}
		}
		// Sort by timestamp
		return filtered.sort((a, b) => a.timestamp - b.timestamp);
	});
}

/**
 * Get all challenges for a specific game, grouped by proposal author
 */
function getGameChallenges(gameId: string): Readable<Map<string, ChallengeData[]>> {
	return derived([networkChallenges.store], ([$challenges]) => {
		const result = new Map<string, ChallengeData[]>();

		for (const [key, versionedEntity] of $challenges.entries()) {
			if (key.startsWith(`${gameId}:`)) {
				// Key format: gameId:participantPub:proposalAuthorPub
				const parts = key.split(':');
				if (parts.length >= 3) {
					const proposalAuthorPub = parts[2];
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
					}
					result.get(proposalAuthorPub)!.push(versionedEntity.data);
				}
			}
		}

		// Sort each array by timestamp
		for (const [key, challenges] of result) {
			result.set(key, challenges.sort((a, b) => a.timestamp - b.timestamp));
		}

		return result;
	});
}

/**
 * Get all comments for a specific game, grouped by proposal author
 */
function getGameComments(gameId: string): Readable<Map<string, CommentData[]>> {
	return derived([networkComments.store], ([$comments]) => {
		const result = new Map<string, CommentData[]>();

		for (const [key, versionedEntity] of $comments.entries()) {
			if (key.startsWith(`${gameId}:`)) {
				const parts = key.split(':');
				if (parts.length >= 3) {
					const proposalAuthorPub = parts[2];
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
					}
					result.get(proposalAuthorPub)!.push(versionedEntity.data);
				}
			}
		}

		for (const [key, comments] of result) {
			result.set(key, comments.sort((a, b) => a.timestamp - b.timestamp));
		}

		return result;
	});
}

/**
 * Get all modifications for a specific game, grouped by proposal author
 */
function getGameModifications(gameId: string): Readable<Map<string, ModificationProposalData[]>> {
	return derived([networkModifications.store], ([$modifications]) => {
		const result = new Map<string, ModificationProposalData[]>();

		for (const [key, versionedEntity] of $modifications.entries()) {
			if (key.startsWith(`${gameId}:`)) {
				const parts = key.split(':');
				if (parts.length >= 3) {
					const proposalAuthorPub = parts[2];
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
					}
					result.get(proposalAuthorPub)!.push(versionedEntity.data);
				}
			}
		}

		for (const [key, modifications] of result) {
			result.set(key, modifications.sort((a, b) => a.timestamp - b.timestamp));
		}

		return result;
	});
}

/**
 * Get all support expressions for a specific game, grouped by proposal author
 */
function getGameSupport(gameId: string): Readable<Map<string, SupportExpression[]>> {
	return derived([networkSupportState], ([$supportState]) => {
		const result = new Map<string, SupportExpression[]>();

		for (const [key, supportExpr] of $supportState.entries()) {
			if (key.startsWith(`${gameId}:`)) {
				const parts = key.split(':');
				if (parts.length >= 3) {
					const proposalAuthorPub = parts[2];
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
					}
					result.get(proposalAuthorPub)!.push(supportExpr);
				}
			}
		}

		return result;
	});
}

/**
 * Get current phase for a game (derived from data counts)
 */
/**
 * Get current phase based on time (with fallback to heuristics if no phase start time)
 */
function getGamePhase(gameId: string): Readable<GamePhase> {
	const proposals = getGameProposals(gameId);

	return derived(
		[proposals, gameConfigsStore],
		([$proposals, $configs]) => {
			const config = $configs?.[gameId];
			if (!config) return 'not_started';

			// Check for manual phase override
			if (config.currentPhaseOverride) return config.currentPhaseOverride;

			// If no proposals yet, we're in proposing phase
			if ($proposals.length === 0) return 'proposing';

			// If we have a phase start time, use time-based transitions
			if (config.phaseStartTime) {
				const now = Date.now();
				const elapsed = now - config.phaseStartTime;

				const proposingTime = getEffectivePhaseTime(config, 'proposing');
				const challengingTime = getEffectivePhaseTime(config, 'challenging');
				const commentingTime = getEffectivePhaseTime(config, 'commenting');
				const supportingTime = getEffectivePhaseTime(config, 'supporting');

				if (elapsed < proposingTime) return 'proposing';
				if (elapsed < proposingTime + challengingTime) return 'challenging';
				if (elapsed < proposingTime + challengingTime + commentingTime) return 'commenting';
				if (elapsed < proposingTime + challengingTime + commentingTime + supportingTime) return 'supporting';

				return 'complete';
			}

			// Fallback to heuristic-based phase detection
			// (for games created before phase timing was implemented)
			return 'proposing'; // Simplified fallback - just stay in proposing until phase start time is set
		}
	);
}

/**
 * Get consensus results for a game
 */
function getConsensusResults(gameId: string): Readable<Map<string, string>> {
	const proposals = getGameProposals(gameId);
	const modifications = getGameModifications(gameId);
	const support = getGameSupport(gameId);

	return derived(
		[proposals, modifications, support],
		([$proposals, $modifications, $support]) => {
			const results = new Map<string, string>();

			for (const proposal of $proposals) {
				const proposalAuthorPub = proposal.authorPub;
				const supportExpressions = $support.get(proposalAuthorPub) || [];
				const proposalModifications = $modifications.get(proposalAuthorPub) || [];

				// If no support yet, use original content
				if (supportExpressions.length === 0) {
					results.set(proposalAuthorPub, proposal.content || '');
					continue;
				}

				// Calculate support for each candidate
				const candidates = [
					proposal.content!,
					...proposalModifications.map(m => m.content)
				];

				const supportCounts = new Map<string, number>();
				for (const candidate of candidates) {
					supportCounts.set(candidate, 0);
				}

				for (const supportExpr of supportExpressions) {
					for (const [candidate, points] of Object.entries(supportExpr)) {
						if (supportCounts.has(candidate)) {
							supportCounts.set(candidate, supportCounts.get(candidate)! + points);
						}
					}
				}

				// Find winner
				let winner = proposal.content!;
				let maxSupport = 0;
				for (const [candidate, supportCount] of supportCounts) {
					if (supportCount > maxSupport) {
						winner = candidate;
						maxSupport = supportCount;
					}
				}

				results.set(proposalAuthorPub, winner);
			}

			return results;
		}
	);
}

// ============================================================================
// HELPER FUNCTIONS (Like stores.svelte.ts)
// ============================================================================

/**
 * Merge all network ITC stamps for a game
 */
function getMergedGameITC(gameId: string, localITC?: ITCStamp): ITCStamp {
	let merged = localITC || itcSeed();

	const proposalsMap = networkProposals.get();
	for (const [key, versionedEntity] of proposalsMap.entries()) {
		if (key.startsWith(`${gameId}:`) && versionedEntity.metadata.itcStamp) {
			merged = itcJoin(merged, versionedEntity.metadata.itcStamp);
		}
	}

	return itcEvent(merged);
}

/**
 * Compose my proposal with ITC merge
 */
function composeMyProposal(
	gameId: string,
	myPub: string,
	content: string,
	existingProposal?: ProposalData,
	proposedConfigChanges?: ProposedConfigChanges
): ProposalData {
	const mergedITC = getMergedGameITC(gameId, existingProposal?.itcStamp);

	// Determine proposal type
	let proposalType: 'content' | 'config' | 'hybrid' = 'content';
	if (proposedConfigChanges) {
		proposalType = content ? 'hybrid' : 'config';
	}

	return {
		content,
		authorPub: myPub,
		proposalType,
		proposedConfigChanges,
		challenges: existingProposal?.challenges,
		comments: existingProposal?.comments,
		modificationProposals: existingProposal?.modificationProposals,
		supportExpressions: existingProposal?.supportExpressions,
		timestamp: Date.now(),
		itcStamp: mergedITC
	};
}

/**
 * Compose challenge with ITC merge
 */
function composeMyChallenge(
	gameId: string,
	myPub: string,
	content: string
): ChallengeData {
	const mergedITC = getMergedGameITC(gameId);

	return {
		content,
		authorPub: myPub,
		timestamp: Date.now(),
		itcStamp: mergedITC
	};
}

/**
 * Compose comment with ITC merge
 */
function composeMyComment(
	gameId: string,
	myPub: string,
	content: string
): CommentData {
	const mergedITC = getMergedGameITC(gameId);

	return {
		content,
		authorPub: myPub,
		timestamp: Date.now(),
		itcStamp: mergedITC
	};
}

/**
 * Compose modification with ITC merge
 */
function composeMyModification(
	gameId: string,
	myPub: string,
	content: string
): ModificationProposalData {
	const mergedITC = getMergedGameITC(gameId);

	return {
		content,
		authorPub: myPub,
		timestamp: Date.now(),
		itcStamp: mergedITC
	};
}

/**
 * Subscribe to a participant's data for a specific game
 */
function subscribeToGameParticipant(
	user: any,
	gameId: string,
	participantPub: string,
	agendaIndex: number
): () => void {
	const unsubscribers: Array<() => void> = [];

	// Subscribe to their proposal
	const unsubProposal = listenAtPath(
		user,
		[participantPub, 'games', gameId, 'proposals', agendaIndex.toString()],
		(data) => {
			if (data) {
				const key = `${gameId}:${participantPub}`;
				const proposal = data as ProposalData;
				networkProposals.update(key, proposal);
			}
		},
		true
	);
	unsubscribers.push(unsubProposal);

	// Get config to know all participants
	const configs = get(gameConfigsStore);
	const config = configs?.[gameId];
	const allParticipants = config?.participants || [];

	// Subscribe to their challenges, comments, modifications, support for each proposal
	for (const proposalAuthorPub of allParticipants) {
		// Challenges
		const unsubChallenge = listenAtPath(
			user,
			[participantPub, 'games', gameId, 'challenges', proposalAuthorPub],
			(data) => {
				if (data) {
					const key = `${gameId}:${participantPub}:${proposalAuthorPub}`;
					const challenge = data as ChallengeData;
					networkChallenges.update(key, challenge);
				}
			},
			true
		);
		unsubscribers.push(unsubChallenge);

		// Comments
		const unsubComment = listenAtPath(
			user,
			[participantPub, 'games', gameId, 'comments', proposalAuthorPub],
			(data) => {
				if (data) {
					const key = `${gameId}:${participantPub}:${proposalAuthorPub}`;
					const comment = data as CommentData;
					networkComments.update(key, comment);
				}
			},
			true
		);
		unsubscribers.push(unsubComment);

		// Modifications
		const unsubModification = listenAtPath(
			user,
			[participantPub, 'games', gameId, 'modifications', proposalAuthorPub],
			(data) => {
				if (data) {
					const key = `${gameId}:${participantPub}:${proposalAuthorPub}`;
					const modification = data as ModificationProposalData;
					networkModifications.update(key, modification);
				}
			},
			true
		);
		unsubscribers.push(unsubModification);

		// Support
		const unsubSupport = listenAtPath(
			user,
			[participantPub, 'games', gameId, 'support', proposalAuthorPub],
			(data) => {
				if (data) {
					const key = `${gameId}:${participantPub}:${proposalAuthorPub}`;
					networkSupportState.update($map => {
						const newMap = new Map($map);
						newMap.set(key, data as SupportExpression);
						return newMap;
					});
				}
			},
			true
		);
		unsubscribers.push(unsubSupport);
	}

	// Return cleanup function
	return () => {
		unsubscribers.forEach(unsub => unsub());
	};
}

// ============================================================================
// REACTIVE P2P DECIDER CLASS (Thin Wrapper)
// ============================================================================

/**
 * Reactive P2P Decider using global versioned stores
 * 
 * This is now a thin wrapper around global stores that provides:
 * - Game-scoped derived views
 * - Backwards-compatible API
 * - Automatic ITC causality tracking
 * - Fine-grained reactivity
 */
class ReactiveP2PDecider {
	private user: any;
	private gameId: string;
	private myPublicKey: string;

	// Derived stores (scoped to this game)
	allProposals: Readable<ProposalData[]>;
	allChallenges: Readable<Map<string, ChallengeData[]>>;
	allComments: Readable<Map<string, CommentData[]>>;
	allModifications: Readable<Map<string, ModificationProposalData[]>>;
	allSupport: Readable<Map<string, SupportExpression[]>>;
	currentPhase: Readable<GamePhase>;
	consensusResults: Readable<Map<string, string>>;
	isReady: Readable<boolean>;

	// Config and participants (for backwards compatibility)
	config = $state<GameConfig | null>(null);
	participants = $state<string[]>([]);

	// Track subscriptions for cleanup
	private unsubscribers: Array<() => void> = [];

	constructor(user: any, gameId: string) {
		if (!user || !user.is || !user.is.pub) {
			throw new Error('User must be authenticated before creating ReactiveP2PDecider');
		}
		this.user = user;
		this.gameId = gameId;
		this.myPublicKey = user.is.pub;

		// Create game-scoped derived stores
		this.allProposals = getGameProposals(gameId);
		this.allChallenges = getGameChallenges(gameId);
		this.allComments = getGameComments(gameId);
		this.allModifications = getGameModifications(gameId);
		this.allSupport = getGameSupport(gameId);
		this.currentPhase = getGamePhase(gameId);
		this.consensusResults = getConsensusResults(gameId);
		this.isReady = derived([gameConfigsStore], ([$configs]) =>
			$configs?.[gameId] !== undefined
		);

		// Subscribe to config changes to update local state
		const unsubConfig = gameConfigsStore.subscribe(($configs) => {
			const gameConfig = $configs?.[gameId];
			if (gameConfig) {
				this.config = gameConfig;
				this.participants = gameConfig.participants;
			}
		});
		this.unsubscribers.push(unsubConfig);
	}

	// ========================================================================
	// GAME INITIALIZATION
	// ========================================================================

	async createGame(
		agenda: (string | AgendaItem)[],
		otherParticipantPubKeys: string[] = [],
		timeWindow: number = 86400000,
		phaseTimeConfig?: PhaseTimeConfig
	): Promise<void> {
		console.log(`[DECIDER] Creating new game with ID: ${this.gameId}`);

		const config: GameConfig = {
			gameId: this.gameId,
			participants: [this.myPublicKey, ...otherParticipantPubKeys],
			agenda,
			currentAgendaIndex: 0,
			timeWindow,
			phaseTimeConfig,
			phaseStartTime: Date.now(), // Start timing immediately
			createdAt: Date.now(),
			createdBy: this.myPublicKey,
		};

		// Update global store (persists to Mesh automatically via createStore)
		const currentConfigs = get(gameConfigsStore) || {};
		gameConfigsStore.set({
			...currentConfigs,
			[this.gameId]: config
		});

		// Setup listeners for all participants
		this.setupAllListeners(config.participants);

		console.log('[DECIDER] Game created successfully');
	}

	async joinGame(creatorPubKey: string): Promise<void> {
		console.log(`[DECIDER] Joining game ${this.gameId} created by ${creatorPubKey}`);

		// For now, wait for config to appear in gameConfigsStore via network sync
		// TODO: Implement proper peer discovery and config fetching
		const checkInterval = setInterval(() => {
			const configs = get(gameConfigsStore);
			if (configs?.[this.gameId]) {
				clearInterval(checkInterval);

				const config = configs[this.gameId];

				// Add myself as participant if not already included
				if (!config.participants.includes(this.myPublicKey)) {
					config.participants.push(this.myPublicKey);

					// Update store (persists automatically)
					const currentConfigs = get(gameConfigsStore) || {};
					gameConfigsStore.set({
						...currentConfigs,
						[this.gameId]: config
					});
				}

				// Setup listeners
				this.setupAllListeners(config.participants);

				console.log('[DECIDER] Successfully joined game');
			}
		}, 100);

		// Timeout after 10 seconds
		setTimeout(() => {
			clearInterval(checkInterval);
			console.error('[DECIDER] Timeout waiting for game config');
		}, 10000);
	}

	// ========================================================================
	// SUBSCRIPTION SETUP
	// ========================================================================

	private setupAllListeners(participants: string[]): void {
		const agendaIndex = this.config?.currentAgendaIndex || 0;

		console.log(`[DECIDER] Setting up listeners for ${participants.length} participants`);

		for (const participantPub of participants) {
			const unsub = subscribeToGameParticipant(
				this.user,
				this.gameId,
				participantPub,
				agendaIndex
			);
			this.unsubscribers.push(unsub);
		}

		console.log(`[DECIDER] Set up subscriptions for game ${this.gameId}`);
	}

	// ========================================================================
	// WRITE OPERATIONS (to my own user space)
	// ========================================================================

	async writeMyProposal(content: string, proposedConfigChanges?: ProposedConfigChanges): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		const key = `${this.gameId}:${this.myPublicKey}`;
		const existing = networkProposals.getData(key);

		const proposal = composeMyProposal(
			this.gameId,
			this.myPublicKey,
			content,
			existing,
			proposedConfigChanges
		);

		const agendaIndex = this.config.currentAgendaIndex;
		console.log(`[DECIDER] Writing my proposal for agenda item ${agendaIndex}:`, content);

		// Clean undefined fields for Gun compatibility
		const cleanedProposal = removeUndefinedFields(proposal);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'proposals', agendaIndex.toString()],
				cleanedProposal,
				(err) => {
					if (err) reject(err);
					else {
						console.log('[DECIDER] Proposal written successfully');
						resolve();
					}
				}
			);
		});
	}

	/**
	 * Create a meta-proposal to change game configuration
	 * @param content - Description of the proposed changes
	 * @param configChanges - The configuration changes to propose
	 */
	async writeMyConfigProposal(content: string, configChanges: ProposedConfigChanges): Promise<void> {
		return this.writeMyProposal(content, configChanges);
	}

	async writeMyChallengeToProposal(proposalAuthorPub: string, challengeContent: string): Promise<void> {
		const challenge = composeMyChallenge(
			this.gameId,
			this.myPublicKey,
			challengeContent
		);

		console.log(`[DECIDER] Writing challenge to proposal by ${proposalAuthorPub}`);

		// Clean undefined fields for Gun compatibility
		const cleanedChallenge = removeUndefinedFields(challenge);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'challenges', proposalAuthorPub],
				cleanedChallenge,
				(err) => {
					if (err) reject(err);
					else {
						console.log('[DECIDER] Challenge written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMyCommentOnProposal(proposalAuthorPub: string, commentContent: string): Promise<void> {
		const comment = composeMyComment(
			this.gameId,
			this.myPublicKey,
			commentContent
		);

		console.log(`[DECIDER] Writing comment on proposal by ${proposalAuthorPub}`);

		// Clean undefined fields for Gun compatibility
		const cleanedComment = removeUndefinedFields(comment);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'comments', proposalAuthorPub],
				cleanedComment,
				(err) => {
					if (err) reject(err);
					else {
						console.log('[DECIDER] Comment written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMyModificationToProposal(proposalAuthorPub: string, modificationContent: string): Promise<void> {
		const modification = composeMyModification(
			this.gameId,
			this.myPublicKey,
			modificationContent
		);

		console.log(`[DECIDER] Writing modification to proposal by ${proposalAuthorPub}`);

		// Clean undefined fields for Gun compatibility
		const cleanedModification = removeUndefinedFields(modification);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'modifications', proposalAuthorPub],
				cleanedModification,
				(err) => {
					if (err) reject(err);
					else {
						console.log('[DECIDER] Modification written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMySupportForProposal(proposalAuthorPub: string, support: SupportExpression): Promise<void> {
		console.log(`[DECIDER] Writing support for proposal by ${proposalAuthorPub}`);

		// Clean undefined fields for Gun compatibility
		const cleanedSupport = removeUndefinedFields(support);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'support', proposalAuthorPub],
				cleanedSupport,
				(err) => {
					if (err) reject(err);
					else {
						console.log('[DECIDER] Support written successfully');
						resolve();
					}
				}
			);
		});
	}

	// ========================================================================
	// CLEANUP
	// ========================================================================

	destroy(): void {
		console.log(`[DECIDER] Cleaning up ${this.unsubscribers.length} subscriptions`);
		this.unsubscribers.forEach(unsub => unsub());
		this.unsubscribers = [];
	}
}

// ============================================================================
// EXPORTS
// ============================================================================

export {
	ReactiveP2PDecider,
	// Global stores (for advanced usage)
	gameConfigsStore,
	networkProposals,
	networkChallenges,
	networkComments,
	networkModifications,
	networkSupportState,
	// Helper functions
	subscribeToGameParticipant,
	getGameProposals,
	getGameChallenges,
	getGameComments,
	getGameModifications,
	getGameSupport,
	getGamePhase,
	getConsensusResults,
	composeMyProposal,
	composeMyChallenge,
	composeMyComment,
	composeMyModification,
	normalizeAgendaItem,
	getEffectivePhaseTime,
	getEffectiveTimeWindow,
	applyConfigChanges,
	// Types
	type GameConfig,
	type ProposalData,
	type ChallengeData,
	type CommentData,
	type ModificationProposalData,
	type PhaseTimeConfig,
	type AgendaItem,
	type ProposedConfigChanges,
	type Challenge, // Backwards compatibility alias
	type Comment, // Backwards compatibility alias
	type ModificationProposal, // Backwards compatibility alias
	type SupportExpression,
	type GamePhase,
};
