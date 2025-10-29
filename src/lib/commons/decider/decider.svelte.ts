import { z } from 'zod';
import { writeAtPath, readAtPath, listenAtPath } from '../utils/holsterData';
import { 
  setNodeId, 
  toVersioned, 
  mergeVersioned, 
  updateFields, 
  fromVersioned,
  type VersionedData 
} from '../utils/crdt';

// ============================================================================
// BASE ZOD SCHEMAS (without CRDT metadata)
// CRDT versioning will be applied per-field automatically
// ============================================================================

const PlayerSchema = z.string().min(1);

// Base schemas for content - CRDT metadata is added separately
const ChallengeBaseSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
});

const CommentBaseSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
});

const ModificationProposalBaseSchema = z.object({
	content: z.string(),
	authorPub: z.string(),
});

const SupportExpressionSchema = z.record(z.string(), z.number().int().min(0));

const ProposalBaseSchema = z.object({
	content: z.string().nullable(),
	authorPub: z.string(),
	challenges: z.array(ChallengeBaseSchema).optional(),
	comments: z.array(CommentBaseSchema).optional(),
	modificationProposals: z.array(ModificationProposalBaseSchema).optional(),
	supportExpressions: z.array(SupportExpressionSchema).optional(),
});

const GameConfigSchema = z.object({
	gameId: z.string(),
	participants: z.array(z.string()),
	agenda: z.array(z.string()),
	currentAgendaIndex: z.number().int().min(0).default(0),
	timeWindow: z.number().int().positive().default(86400000),
	createdAt: z.number(),
	createdBy: z.string(),
});

// ============================================================================
// TYPE INFERENCE
// ============================================================================

type Player = z.infer<typeof PlayerSchema>;
type Challenge = z.infer<typeof ChallengeBaseSchema>;
type Comment = z.infer<typeof CommentBaseSchema>;
type ModificationProposal = z.infer<typeof ModificationProposalBaseSchema>;
type SupportExpression = z.infer<typeof SupportExpressionSchema>;
type ProposalData = z.infer<typeof ProposalBaseSchema>;
type GameConfig = z.infer<typeof GameConfigSchema>;

type GamePhase = 
	| 'not_started'
	| 'proposing'
	| 'challenging'
	| 'commenting'
	| 'supporting'
	| 'complete';

// ============================================================================
// PARTICIPANT DATA STATE
// ============================================================================

// ============================================================================
// REACTIVE P2P DECIDER CLASS
// ============================================================================

/**
 * Reactive P2P Decider using Svelte 5 runes.
 * 
 * All game state is stored in reactive state ($state) that automatically updates
 * as new data arrives from the network through Holster listeners.
 */
class ReactiveP2PDecider {
	private user: any;
	private gameId: string;
	private myPublicKey: string;

	// Core state (using $state rune)
	config = $state<GameConfig | null>(null);
	participants = $state<string[]>([]);
	
	// Raw data state (one per participant) - stored as versioned CRDT data
	private proposalsState = $state(new Map<string, VersionedData | null>());
	private challengesState = $state(new Map<string, Map<string, VersionedData | null>>());
	private commentsState = $state(new Map<string, Map<string, VersionedData | null>>());
	private modificationsState = $state(new Map<string, Map<string, VersionedData | null>>());
	private supportState = $state(new Map<string, Map<string, SupportExpression | null>>());
	
	// Derived state - automatically calculated from raw data (using $derived rune)
	allProposals = $derived(this.getAllProposals());
	allChallenges = $derived(this.getAllChallenges());
	allComments = $derived(this.getAllComments());
	allModifications = $derived(this.getAllModifications());
	allSupport = $derived(this.getAllSupport());
	
	// High-level derived state
	currentPhase = $derived(this.getCurrentPhase());
	consensusResults = $derived(this.getConsensusResults());
	isReady = $derived(this.config !== null);
	
	// Listener cleanup functions
	private unsubscribers: Array<() => void> = [];

	constructor(user: any, gameId: string) {
		if (!user || !user.is || !user.is.pub) {
			throw new Error('User must be authenticated before creating ReactiveP2PDecider');
		}
		this.user = user;
		this.gameId = gameId;
		this.myPublicKey = user.is.pub;
		
		// Set CRDT node ID to our public key for deterministic tie-breaking
		setNodeId(this.myPublicKey);
		
		// Runes are initialized inline above, no setup needed here
	}

	// ========================================================================
	// DERIVED COMPUTATION METHODS (for $derived runes)
	// ========================================================================

	private getAllProposals(): ProposalData[] {
		const result: ProposalData[] = [];
		for (const [_pub, versionedProposal] of this.proposalsState) {
			if (versionedProposal) {
				// Extract plain values from versioned CRDT data
				const proposal = fromVersioned(versionedProposal) as ProposalData;
				result.push(proposal);
			}
		}
		// Sort by the highest timestamp in any field of the proposal
		return result.sort((a, b) => {
			const tsA = this.getMaxTimestamp(this.proposalsState.get(a.authorPub));
			const tsB = this.getMaxTimestamp(this.proposalsState.get(b.authorPub));
			return tsA - tsB;
		});
	}
	
	// Helper to get the max timestamp from versioned data for sorting
	private getMaxTimestamp(versionedData: VersionedData | null | undefined): number {
		if (!versionedData) return 0;
		let max = 0;
		for (const field of Object.values(versionedData)) {
			if (field?.timestamp && field.timestamp > max) {
				max = field.timestamp;
			}
		}
		return max;
	}

	private getAllChallenges(): Map<string, Challenge[]> {
		const result = new Map<string, Challenge[]>();
		const versionedMap = new Map<string, Map<string, VersionedData>>();
		
		// Aggregate: participantPub -> proposalAuthorPub -> challenge
		// Into: proposalAuthorPub -> challenge[]
		for (const [_participantPub, proposalMap] of this.challengesState) {
			for (const [proposalAuthorPub, versionedChallenge] of proposalMap) {
				if (versionedChallenge) {
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
						versionedMap.set(proposalAuthorPub, new Map());
					}
					const challenge = fromVersioned(versionedChallenge) as Challenge;
					result.get(proposalAuthorPub)!.push(challenge);
					versionedMap.get(proposalAuthorPub)!.set(challenge.authorPub, versionedChallenge);
				}
			}
		}
		
		// Sort by timestamp
		for (const [key, challenges] of result) {
			const versionedChallenges = versionedMap.get(key)!;
			result.set(key, challenges.sort((a, b) => {
				const tsA = this.getMaxTimestamp(versionedChallenges.get(a.authorPub));
				const tsB = this.getMaxTimestamp(versionedChallenges.get(b.authorPub));
				return tsA - tsB;
			}));
		}
		
		return result;
	}

	private getAllComments(): Map<string, Comment[]> {
		const result = new Map<string, Comment[]>();
		const versionedMap = new Map<string, Map<string, VersionedData>>();
		
		for (const [_participantPub, proposalMap] of this.commentsState) {
			for (const [proposalAuthorPub, versionedComment] of proposalMap) {
				if (versionedComment) {
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
						versionedMap.set(proposalAuthorPub, new Map());
					}
					const comment = fromVersioned(versionedComment) as Comment;
					result.get(proposalAuthorPub)!.push(comment);
					versionedMap.get(proposalAuthorPub)!.set(comment.authorPub, versionedComment);
				}
			}
		}
		
		for (const [key, comments] of result) {
			const versionedComments = versionedMap.get(key)!;
			result.set(key, comments.sort((a, b) => {
				const tsA = this.getMaxTimestamp(versionedComments.get(a.authorPub));
				const tsB = this.getMaxTimestamp(versionedComments.get(b.authorPub));
				return tsA - tsB;
			}));
		}
		
		return result;
	}

	private getAllModifications(): Map<string, ModificationProposal[]> {
		const result = new Map<string, ModificationProposal[]>();
		const versionedMap = new Map<string, Map<string, VersionedData>>();
		
		for (const [_participantPub, proposalMap] of this.modificationsState) {
			for (const [proposalAuthorPub, versionedModification] of proposalMap) {
				if (versionedModification) {
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
						versionedMap.set(proposalAuthorPub, new Map());
					}
					const modification = fromVersioned(versionedModification) as ModificationProposal;
					result.get(proposalAuthorPub)!.push(modification);
					versionedMap.get(proposalAuthorPub)!.set(modification.authorPub, versionedModification);
				}
			}
		}
		
		for (const [key, modifications] of result) {
			const versionedModifications = versionedMap.get(key)!;
			result.set(key, modifications.sort((a, b) => {
				const tsA = this.getMaxTimestamp(versionedModifications.get(a.authorPub));
				const tsB = this.getMaxTimestamp(versionedModifications.get(b.authorPub));
				return tsA - tsB;
			}));
		}
		
		return result;
	}

	private getAllSupport(): Map<string, SupportExpression[]> {
		const result = new Map<string, SupportExpression[]>();
		
		for (const [_participantPub, proposalMap] of this.supportState) {
			for (const [proposalAuthorPub, supportExpr] of proposalMap) {
				if (supportExpr) {
					if (!result.has(proposalAuthorPub)) {
						result.set(proposalAuthorPub, []);
					}
					result.get(proposalAuthorPub)!.push(supportExpr);
				}
			}
		}
		
		return result;
	}

	private getCurrentPhase(): GamePhase {
		if (this.allProposals.length === 0) return 'proposing';
		
		const proposalCount = this.allProposals.length;
		const participantCount = this.participants.length;
		
		// Check if we have challenges for all proposals
		let totalChallenges = 0;
		for (const challenges of this.allChallenges.values()) {
			totalChallenges += challenges.length;
		}
		
		// If we have proposals but few/no challenges yet, we're in challenging phase
		if (totalChallenges < participantCount) return 'challenging';
		
		// Check if we have comments/modifications
		let totalComments = 0;
		let totalModifications = 0;
		for (const comments of this.allComments.values()) {
			totalComments += comments.length;
		}
		for (const modifications of this.allModifications.values()) {
			totalModifications += modifications.length;
		}
		
		// If we have challenges but few comments, we're in commenting phase
		if (totalComments < participantCount * proposalCount) return 'commenting';
		
		// Check if we have support
		let totalSupport = 0;
		for (const support of this.allSupport.values()) {
			totalSupport += support.length;
		}
		
		// If we have comments but little support, we're in supporting phase
		if (totalSupport < participantCount * proposalCount) return 'supporting';
		
		// All data collected
		return 'complete';
	}

	private getConsensusResults(): Map<string, string> {
		const results = new Map<string, string>();
		
		for (const proposal of this.allProposals) {
			const proposalAuthorPub = proposal.authorPub;
			const supportExpressions = this.allSupport.get(proposalAuthorPub) || [];
			const modifications = this.allModifications.get(proposalAuthorPub) || [];
			
			// If no support yet, use original content
			if (supportExpressions.length === 0) {
				results.set(proposalAuthorPub, proposal.content || '');
				continue;
			}
			
			// Calculate support for each candidate
			const candidates = [
				proposal.content!,
				...modifications.map(m => m.content)
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
			for (const [candidate, support] of supportCounts) {
				if (support > maxSupport) {
					winner = candidate;
					maxSupport = support;
				}
			}
			
			results.set(proposalAuthorPub, winner);
		}
		
		return results;
	}

	// ========================================================================
	// HELPER FUNCTIONS FOR GUN COMPATIBILITY
	// ========================================================================

	/**
	 * Convert array to object with numeric keys for Gun storage
	 */
	private arrayToObject(arr: any[]): Record<string, any> {
		const obj: Record<string, any> = {};
		arr.forEach((item, index) => {
			obj[index.toString()] = item;
		});
		return obj;
	}

	/**
	 * Convert object with numeric keys back to array
	 */
	private objectToArray(obj: Record<string, any>): any[] {
		if (!obj) return [];
		const keys = Object.keys(obj).filter(k => !isNaN(Number(k))).sort((a, b) => Number(a) - Number(b));
		return keys.map(k => obj[k]);
	}

	// ========================================================================
	// GAME INITIALIZATION
	// ========================================================================

	async createGame(agenda: string[], otherParticipantPubKeys: string[] = []): Promise<void> {
		console.log(`Creating new game with ID: ${this.gameId}`);
		
		const config: GameConfig = {
			gameId: this.gameId,
			participants: [this.myPublicKey, ...otherParticipantPubKeys],
			agenda,
			currentAgendaIndex: 0,
			timeWindow: 86400000,
			createdAt: Date.now(),
			createdBy: this.myPublicKey,
		};

		this.config = config;
		this.participants = config.participants;

		// Convert arrays to objects for Gun storage
		const gunCompatibleConfig = {
			gameId: config.gameId,
			participants: this.arrayToObject(config.participants),
			agenda: this.arrayToObject(config.agenda),
			currentAgendaIndex: config.currentAgendaIndex,
			timeWindow: config.timeWindow,
			createdAt: config.createdAt,
			createdBy: config.createdBy,
		};

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'config'],
				gunCompatibleConfig,
				(err) => {
					if (err) {
						console.error('Failed to create game:', err);
						reject(err);
					} else {
						console.log('Game created successfully');
						// Start listening for all participants
						this.setupAllListeners();
						resolve();
					}
				}
			);
		});
	}

	async joinGame(creatorPubKey: string): Promise<void> {
		console.log(`Joining game ${this.gameId} created by ${creatorPubKey}`);

		return new Promise((resolve, reject) => {
			readAtPath(
				this.user,
				[creatorPubKey, 'games', this.gameId, 'config'],
				(data) => {
					if (!data) {
						const error = 'Game config not found';
						console.error(error);
						reject(error);
						return;
					}

					// Convert Gun-stored objects back to arrays
					const gunData = data as any;
					const normalizedData = {
						...gunData,
						participants: this.objectToArray(gunData.participants),
						agenda: this.objectToArray(gunData.agenda),
					};

					const config = GameConfigSchema.parse(normalizedData);
					
					if (!config.participants.includes(this.myPublicKey)) {
						config.participants.push(this.myPublicKey);
					}

					this.config = config;
					this.participants = config.participants;

					// Convert back to Gun-compatible format for writing
					const gunCompatibleConfig = {
						gameId: config.gameId,
						participants: this.arrayToObject(config.participants),
						agenda: this.arrayToObject(config.agenda),
						currentAgendaIndex: config.currentAgendaIndex,
						timeWindow: config.timeWindow,
						createdAt: config.createdAt,
						createdBy: config.createdBy,
					};

					writeAtPath(
						this.user,
						['games', this.gameId, 'config'],
						gunCompatibleConfig,
						(err) => {
							if (err) {
								console.error('Failed to write config:', err);
								reject(err);
							} else {
								console.log('Successfully joined game');
								// Start listening for all participants
								this.setupAllListeners();
								resolve();
							}
						}
					);
				}
			);
		});
	}

	// ========================================================================
	// REACTIVE LISTENERS SETUP
	// ========================================================================

	/**
	 * Sets up listeners for all data types from all participants.
	 * Updates stores automatically as new data arrives.
	 */
	private setupAllListeners(): void {
		const participantList = this.participants;
		const agendaIndex = this.config?.currentAgendaIndex || 0;

		console.log(`Setting up listeners for ${participantList.length} participants`);

		for (const participantPub of participantList) {
			// Listen for proposals
			const unsubProposal = listenAtPath(
				this.user,
				[participantPub, 'games', this.gameId, 'proposals', agendaIndex.toString()],
				(data) => {
					if (data) {
						console.log(`📥 Received proposal from ${participantPub}`);
						// Data is already in versioned format from network
						const newVersionedProposal = data as VersionedData;
						const existingVersionedProposal = this.proposalsState.get(participantPub);
						// Use CRDT merge for per-field conflict resolution
						const merged = existingVersionedProposal 
							? mergeVersioned(existingVersionedProposal, newVersionedProposal)
							: newVersionedProposal;
						this.proposalsState.set(participantPub, merged);
					}
				},
				true  // Get initial data
			);
			this.unsubscribers.push(unsubProposal);

			// Listen for challenges/comments/modifications/support for each proposal
			for (const proposalAuthorPub of participantList) {
				// Challenges
				const unsubChallenge = listenAtPath(
					this.user,
					[participantPub, 'games', this.gameId, 'challenges', proposalAuthorPub],
					(data) => {
						if (data) {
							console.log(`📥 Received challenge from ${participantPub} to ${proposalAuthorPub}`);
							if (!this.challengesState.has(participantPub)) {
								this.challengesState.set(participantPub, new Map());
							}
							const newVersionedChallenge = data as VersionedData;
							const existingVersionedChallenge = this.challengesState.get(participantPub)!.get(proposalAuthorPub);
							const merged = existingVersionedChallenge 
								? mergeVersioned(existingVersionedChallenge, newVersionedChallenge)
								: newVersionedChallenge;
							this.challengesState.get(participantPub)!.set(proposalAuthorPub, merged);
						}
					},
					true
				);
				this.unsubscribers.push(unsubChallenge);

				// Comments
				const unsubComment = listenAtPath(
					this.user,
					[participantPub, 'games', this.gameId, 'comments', proposalAuthorPub],
					(data) => {
						if (data) {
							console.log(`📥 Received comment from ${participantPub} on ${proposalAuthorPub}`);
							if (!this.commentsState.has(participantPub)) {
								this.commentsState.set(participantPub, new Map());
							}
							const newVersionedComment = data as VersionedData;
							const existingVersionedComment = this.commentsState.get(participantPub)!.get(proposalAuthorPub);
							const merged = existingVersionedComment 
								? mergeVersioned(existingVersionedComment, newVersionedComment)
								: newVersionedComment;
							this.commentsState.get(participantPub)!.set(proposalAuthorPub, merged);
						}
					},
					true
				);
				this.unsubscribers.push(unsubComment);

				// Modifications
				const unsubModification = listenAtPath(
					this.user,
					[participantPub, 'games', this.gameId, 'modifications', proposalAuthorPub],
					(data) => {
						if (data) {
							console.log(`📥 Received modification from ${participantPub} to ${proposalAuthorPub}`);
							if (!this.modificationsState.has(participantPub)) {
								this.modificationsState.set(participantPub, new Map());
							}
							const newVersionedModification = data as VersionedData;
							const existingVersionedModification = this.modificationsState.get(participantPub)!.get(proposalAuthorPub);
							const merged = existingVersionedModification 
								? mergeVersioned(existingVersionedModification, newVersionedModification)
								: newVersionedModification;
							this.modificationsState.get(participantPub)!.set(proposalAuthorPub, merged);
						}
					},
					true
				);
				this.unsubscribers.push(unsubModification);

				// Support
				const unsubSupport = listenAtPath(
					this.user,
					[participantPub, 'games', this.gameId, 'support', proposalAuthorPub],
					(data) => {
						if (data) {
							console.log(`📥 Received support from ${participantPub} for ${proposalAuthorPub}`);
							if (!this.supportState.has(participantPub)) {
								this.supportState.set(participantPub, new Map());
							}
							this.supportState.get(participantPub)!.set(proposalAuthorPub, data as SupportExpression);
						}
					},
					true
				);
				this.unsubscribers.push(unsubSupport);
			}
		}

		console.log(`✅ Set up ${this.unsubscribers.length} listeners`);
	}

	/**
	 * Clean up all listeners when done.
	 */
	destroy(): void {
		console.log(`Cleaning up ${this.unsubscribers.length} listeners`);
		this.unsubscribers.forEach(unsub => unsub());
		this.unsubscribers = [];
	}

	// ========================================================================
	// WRITE OPERATIONS (to my own user space)
	// ========================================================================

	async writeMyProposal(content: string): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');
		const config = this.config;

		const agendaIndex = config.currentAgendaIndex;
		// Convert to versioned CRDT format with per-field timestamps
		const versionedProposal = toVersioned({
			content,
			authorPub: this.myPublicKey,
		}, ProposalBaseSchema);

		console.log(`✍️ Writing my proposal for agenda item ${agendaIndex}:`, content);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'proposals', agendaIndex.toString()],
				versionedProposal,
				(err) => {
					if (err) reject(err);
					else {
						console.log('✅ Proposal written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMyChallengeToProposal(proposalAuthorPub: string, challengeContent: string): Promise<void> {
		const versionedChallenge = toVersioned({
			content: challengeContent,
			authorPub: this.myPublicKey,
		}, ChallengeBaseSchema);

		console.log(`✍️ Writing challenge to proposal by ${proposalAuthorPub}`);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'challenges', proposalAuthorPub],
				versionedChallenge,
				(err) => {
					if (err) reject(err);
					else {
						console.log('✅ Challenge written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMyCommentOnProposal(proposalAuthorPub: string, commentContent: string): Promise<void> {
		const versionedComment = toVersioned({
			content: commentContent,
			authorPub: this.myPublicKey,
		}, CommentBaseSchema);

		console.log(`✍️ Writing comment on proposal by ${proposalAuthorPub}`);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'comments', proposalAuthorPub],
				versionedComment,
				(err) => {
					if (err) reject(err);
					else {
						console.log('✅ Comment written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMyModificationToProposal(proposalAuthorPub: string, modificationContent: string): Promise<void> {
		const versionedModification = toVersioned({
			content: modificationContent,
			authorPub: this.myPublicKey,
		}, ModificationProposalBaseSchema);

		console.log(`✍️ Writing modification to proposal by ${proposalAuthorPub}`);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'modifications', proposalAuthorPub],
				versionedModification,
				(err) => {
					if (err) reject(err);
					else {
						console.log('✅ Modification written successfully');
						resolve();
					}
				}
			);
		});
	}

	async writeMySupportForProposal(proposalAuthorPub: string, support: SupportExpression): Promise<void> {
		console.log(`✍️ Writing support for proposal by ${proposalAuthorPub}`);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'support', proposalAuthorPub],
				support,
				(err) => {
					if (err) reject(err);
					else {
						console.log('✅ Support written successfully');
						resolve();
					}
				}
			);
		});
	}
}

// ============================================================================
// EXPORTS
// ============================================================================

export { 
	ReactiveP2PDecider,
	type GameConfig,
	type ProposalData,
	type Challenge,
	type Comment,
	type ModificationProposal,
	type SupportExpression,
	type GamePhase,
};

