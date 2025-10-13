import { z } from 'zod';
import { writeAtPath, readAtPath, listenAtPath } from '../utils/holsterData';

// ============================================================================
// ZOD SCHEMAS
// ============================================================================

const PlayerSchema = z.string().min(1);

const ChallengeSchema = z.object({
	content: z.string(),
	timestamp: z.number(),
	authorPub: z.string(),
});

const CommentSchema = z.object({
	content: z.string(),
	timestamp: z.number(),
	authorPub: z.string(),
});

const ModificationProposalSchema = z.object({
	content: z.string(),
	timestamp: z.number(),
	authorPub: z.string(),
});

const SupportExpressionSchema = z.record(z.string(), z.number().int().min(0));

const ProposalSchema = z.object({
	content: z.string().nullable(),
	timestamp: z.number(),
	authorPub: z.string(),
	challenges: z.array(ChallengeSchema),
	comments: z.array(CommentSchema),
	modificationProposals: z.array(ModificationProposalSchema),
	supportExpressions: z.array(SupportExpressionSchema),
});

const GameConfigSchema = z.object({
	gameId: z.string(),
	participants: z.array(z.string()), // Array of public keys
	agenda: z.array(z.string()),
	currentAgendaIndex: z.number().int().min(0).default(0),
	timeWindow: z.number().int().positive().default(86400000), // 1 day in milliseconds
	createdAt: z.number(),
	createdBy: z.string(),
});

// ============================================================================
// TYPE INFERENCE
// ============================================================================

type Player = z.infer<typeof PlayerSchema>;
type Challenge = z.infer<typeof ChallengeSchema>;
type Comment = z.infer<typeof CommentSchema>;
type ModificationProposal = z.infer<typeof ModificationProposalSchema>;
type SupportExpression = z.infer<typeof SupportExpressionSchema>;
type ProposalData = z.infer<typeof ProposalSchema>;
type GameConfig = z.infer<typeof GameConfigSchema>;

// ============================================================================
// PROPOSAL CLASS
// ============================================================================

class Proposal {
	content: string | null;
	timestamp: number;
	authorPub: string;
	challenges: Challenge[];
	comments: Comment[];
	modificationProposals: ModificationProposal[];
	supportExpressions: SupportExpression[];

	constructor(authorPub: string, content: string | null = null) {
		this.content = content;
		this.timestamp = Date.now();
		this.authorPub = authorPub;
		this.challenges = [];
		this.comments = [];
		this.modificationProposals = [];
		this.supportExpressions = [];
	}

	mostSupportedVersion(): string {
		console.log('Calculating the most supported proposal version...');

		if (!this.supportExpressions.length) {
			console.log('No support expressions found. Returning the active proposal as the most supported version.');
			return this.content!;
		}

		const proposalCandidates: string[] = [
			this.content!,
			...this.modificationProposals.map(m => m.content)
		];
		const supportCounts = new Map<string, number>(
			proposalCandidates.map(candidate => [candidate, 0])
		);

		for (const supportExpression of this.supportExpressions) {
			for (const [candidate, points] of Object.entries(supportExpression)) {
				if (supportCounts.has(candidate)) {
					const currentSupport = supportCounts.get(candidate)!;
					supportCounts.set(candidate, currentSupport + points);
					console.log(`Updated support for candidate "${candidate}": ${currentSupport} -> ${currentSupport + points} points.`);
				}
			}
		}

		let mostSupportedCandidate = this.content!;
		let maxSupport = 0;

		for (const [candidate, support] of supportCounts.entries()) {
			console.log(`Candidate "${candidate}" has ${support} points.`);
			if (support > maxSupport) {
				mostSupportedCandidate = candidate;
				maxSupport = support;
			}
		}

		console.log(`Most supported candidate determined: "${mostSupportedCandidate}" with ${maxSupport} points.`);
		return mostSupportedCandidate;
	}
}

// ============================================================================
// P2P DECIDER CLASS
// ============================================================================

/**
 * P2P Decider using Holster for distributed consensus.
 * 
 * Path Structure in Holster User Space:
 * 
 * user[myPublicKey]/
 *   games/
 *     [gameId]/
 *       config: { participants, agenda, currentAgendaIndex, ... }
 *       proposals/
 *         [agendaItemIndex]: { content, timestamp, authorPub }
 *       challenges/
 *         [proposalAuthorPub]: { content, timestamp, authorPub }
 *       comments/
 *         [proposalAuthorPub]: { content, timestamp, authorPub }
 *       modifications/
 *         [proposalAuthorPub]: { content, timestamp, authorPub }
 *       support/
 *         [proposalAuthorPub]: { [candidateContent]: points }
 */
class P2PDecider {
	private user: any; // Holster user instance (must be authenticated)
	private gameId: string;
	private config: GameConfig | null = null;
	private myPublicKey: string;

	constructor(user: any, gameId: string) {
		if (!user || !user.is || !user.is.pub) {
			throw new Error('User must be authenticated before creating P2PDecider');
		}
		this.user = user;
		this.gameId = gameId;
		this.myPublicKey = user.is.pub;
	}

	// ========================================================================
	// GAME INITIALIZATION
	// ========================================================================

	/**
	 * Creates a new game session. The creator becomes the first participant.
	 */
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

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'config'],
				config,
				(err) => {
					if (err) {
						console.error('Failed to create game:', err);
						reject(err);
					} else {
						console.log('Game created successfully');
						resolve();
					}
				}
			);
		});
	}

	/**
	 * Joins an existing game by reading the config from the creator's public space.
	 */
	async joinGame(creatorPubKey: string): Promise<void> {
		console.log(`Joining game ${this.gameId} created by ${creatorPubKey}`);

		return new Promise((resolve, reject) => {
			// Read config from creator's public space
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

					this.config = GameConfigSchema.parse(data);
					console.log('Successfully joined game:', this.config);

					// Add myself to participants if not already there
					if (!this.config.participants.includes(this.myPublicKey)) {
						this.config.participants.push(this.myPublicKey);
					}

					// Write my own copy of the config with updated participants
					writeAtPath(
						this.user,
						['games', this.gameId, 'config'],
						this.config,
						(err) => {
							if (err) {
								console.error('Failed to write config:', err);
								reject(err);
							} else {
								resolve();
							}
						}
					);
				}
			);
		});
	}

	/**
	 * Discovers all participants by reading configs from known participants.
	 */
	async discoverParticipants(): Promise<string[]> {
		if (!this.config) {
			throw new Error('Must join or create game first');
		}

		console.log('Discovering all participants...');
		const allParticipants = new Set<string>(this.config.participants);

		// Read each participant's config to discover more participants
		const discoveryPromises = Array.from(allParticipants).map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'config'],
					(data) => {
						if (data && data.participants) {
							data.participants.forEach((p: string) => allParticipants.add(p));
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(discoveryPromises);

		// Update my config with all discovered participants
		this.config.participants = Array.from(allParticipants);
		console.log(`Discovered ${allParticipants.size} participants:`, this.config.participants);

		return this.config.participants;
	}

	// ========================================================================
	// WRITE OPERATIONS (to my own user space)
	// ========================================================================

	/**
	 * Writes my proposal for the current agenda item.
	 */
	async writeMyProposal(content: string): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		const agendaIndex = this.config.currentAgendaIndex;
		const proposal = {
			content,
			timestamp: Date.now(),
			authorPub: this.myPublicKey,
		};

		console.log(`Writing my proposal for agenda item ${agendaIndex}:`, content);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'proposals', agendaIndex.toString()],
				proposal,
				(err) => {
					if (err) reject(err);
					else resolve();
				}
			);
		});
	}

	/**
	 * Writes my challenge to a specific proposal.
	 */
	async writeMyChallengeToProposal(proposalAuthorPub: string, challengeContent: string): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		const challenge: Challenge = {
			content: challengeContent,
			timestamp: Date.now(),
			authorPub: this.myPublicKey,
		};

		console.log(`Writing challenge to proposal by ${proposalAuthorPub}:`, challengeContent);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'challenges', proposalAuthorPub],
				challenge,
				(err) => {
					if (err) reject(err);
					else resolve();
				}
			);
		});
	}

	/**
	 * Writes my comment on a specific proposal.
	 */
	async writeMyCommentOnProposal(proposalAuthorPub: string, commentContent: string): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		const comment: Comment = {
			content: commentContent,
			timestamp: Date.now(),
			authorPub: this.myPublicKey,
		};

		console.log(`Writing comment on proposal by ${proposalAuthorPub}:`, commentContent);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'comments', proposalAuthorPub],
				comment,
				(err) => {
					if (err) reject(err);
					else resolve();
				}
			);
		});
	}

	/**
	 * Writes my modification proposal for a specific proposal.
	 */
	async writeMyModificationToProposal(proposalAuthorPub: string, modificationContent: string): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		const modification: ModificationProposal = {
			content: modificationContent,
			timestamp: Date.now(),
			authorPub: this.myPublicKey,
		};

		console.log(`Writing modification to proposal by ${proposalAuthorPub}:`, modificationContent);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'modifications', proposalAuthorPub],
				modification,
				(err) => {
					if (err) reject(err);
					else resolve();
				}
			);
		});
	}

	/**
	 * Writes my support distribution for a specific proposal.
	 */
	async writeMySupportForProposal(proposalAuthorPub: string, support: SupportExpression): Promise<void> {
		if (!this.config) throw new Error('Must join or create game first');

		console.log(`Writing support for proposal by ${proposalAuthorPub}:`, support);

		return new Promise((resolve, reject) => {
			writeAtPath(
				this.user,
				['games', this.gameId, 'support', proposalAuthorPub],
				support,
				(err) => {
					if (err) reject(err);
					else resolve();
				}
			);
		});
	}

	// ========================================================================
	// READ OPERATIONS (from all participants' spaces)
	// ========================================================================

	/**
	 * Reads all proposals from all participants for the current agenda item.
	 */
	async readAllProposals(): Promise<Proposal[]> {
		if (!this.config) throw new Error('Must join or create game first');

		const agendaIndex = this.config.currentAgendaIndex;
		console.log(`Reading all proposals for agenda item ${agendaIndex}...`);

		const proposals: Proposal[] = [];
		const readPromises = this.config.participants.map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'proposals', agendaIndex.toString()],
					(data) => {
						if (data && data.content) {
							const proposal = new Proposal(data.authorPub, data.content);
							proposal.timestamp = data.timestamp;
							proposals.push(proposal);
							console.log(`Read proposal from ${pubKey}:`, data.content);
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(readPromises);
		console.log(`Found ${proposals.length} proposals`);
		return proposals;
	}

	/**
	 * Reads all challenges to a specific proposal from all participants.
	 */
	async readAllChallengesForProposal(proposalAuthorPub: string): Promise<Challenge[]> {
		if (!this.config) throw new Error('Must join or create game first');

		console.log(`Reading all challenges for proposal by ${proposalAuthorPub}...`);

		const challenges: Challenge[] = [];
		const readPromises = this.config.participants.map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'challenges', proposalAuthorPub],
					(data) => {
						if (data && data.content) {
							challenges.push(ChallengeSchema.parse(data));
							console.log(`Read challenge from ${pubKey}:`, data.content);
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(readPromises);
		console.log(`Found ${challenges.length} challenges`);
		return challenges;
	}

	/**
	 * Reads all comments on a specific proposal from all participants.
	 */
	async readAllCommentsForProposal(proposalAuthorPub: string): Promise<Comment[]> {
		if (!this.config) throw new Error('Must join or create game first');

		console.log(`Reading all comments for proposal by ${proposalAuthorPub}...`);

		const comments: Comment[] = [];
		const readPromises = this.config.participants.map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'comments', proposalAuthorPub],
					(data) => {
						if (data && data.content) {
							comments.push(CommentSchema.parse(data));
							console.log(`Read comment from ${pubKey}:`, data.content);
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(readPromises);
		console.log(`Found ${comments.length} comments`);
		return comments;
	}

	/**
	 * Reads all modifications to a specific proposal from all participants.
	 */
	async readAllModificationsForProposal(proposalAuthorPub: string): Promise<ModificationProposal[]> {
		if (!this.config) throw new Error('Must join or create game first');

		console.log(`Reading all modifications for proposal by ${proposalAuthorPub}...`);

		const modifications: ModificationProposal[] = [];
		const readPromises = this.config.participants.map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'modifications', proposalAuthorPub],
					(data) => {
						if (data && data.content) {
							modifications.push(ModificationProposalSchema.parse(data));
							console.log(`Read modification from ${pubKey}:`, data.content);
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(readPromises);
		console.log(`Found ${modifications.length} modifications`);
		return modifications;
	}

	/**
	 * Reads all support expressions for a specific proposal from all participants.
	 */
	async readAllSupportForProposal(proposalAuthorPub: string): Promise<SupportExpression[]> {
		if (!this.config) throw new Error('Must join or create game first');

		console.log(`Reading all support for proposal by ${proposalAuthorPub}...`);

		const supportExpressions: SupportExpression[] = [];
		const readPromises = this.config.participants.map(pubKey =>
			new Promise<void>((resolve) => {
				readAtPath(
					this.user,
					[pubKey, 'games', this.gameId, 'support', proposalAuthorPub],
					(data) => {
						if (data && typeof data === 'object') {
							supportExpressions.push(data);
							console.log(`Read support from ${pubKey}:`, data);
						}
						resolve();
					}
				);
			})
		);

		await Promise.all(readPromises);
		console.log(`Found ${supportExpressions.length} support expressions`);
		return supportExpressions;
	}

	// ========================================================================
	// DECISION FLOW (P2P ORCHESTRATION)
	// ========================================================================

	/**
	 * Main decision flow: proposals -> challenges -> comments/modifications -> support -> decision
	 */
	async runDecisionFlow(): Promise<Proposal[]> {
		if (!this.config) throw new Error('Must join or create game first');

		const currentPrompt = this.config.agenda[this.config.currentAgendaIndex];
		console.log(`\n========================================`);
		console.log(`Running decision flow for: "${currentPrompt}"`);
		console.log(`========================================\n`);

		// Step 1: Express proposals
		await this.expressProposals(currentPrompt);

		// Wait for all participants to submit proposals
		await this.waitForProposals();

		// Step 2: Read all proposals
		const proposals = await this.readAllProposals();

		if (proposals.length === 0) {
			console.log('No proposals found. Ending decision flow.');
			return [];
		}

		// Step 3: Process each proposal through the decision flow
		const passedProposals: Proposal[] = [];

		for (const proposal of proposals) {
			const passed = await this.processProposal(proposal);
			if (passed) {
				passedProposals.push(proposal);
			}
		}

		console.log(`\n========================================`);
		console.log(`Decision flow complete. ${passedProposals.length} proposals passed.`);
		console.log(`========================================\n`);

		return passedProposals;
	}

	/**
	 * Processes a single proposal through: challenges -> comments/mods -> support
	 */
	private async processProposal(proposal: Proposal): Promise<boolean> {
		console.log(`\n--- Processing proposal by ${proposal.authorPub} ---`);

		// Step 1: Express challenges
		await this.expressChallenges(proposal);
		await this.waitForChallenges();

		// Step 2: Read all challenges
		proposal.challenges = await this.readAllChallengesForProposal(proposal.authorPub);

		if (proposal.challenges.length === 0) {
			console.log('No challenges. Proposal passes automatically.');
			return true;
		}

		// Step 3: Express comments and modifications
		await this.expressCommentsAndModifications(proposal);
		await this.waitForCommentsAndModifications();

		// Step 4: Read all comments and modifications
		proposal.comments = await this.readAllCommentsForProposal(proposal.authorPub);
		proposal.modificationProposals = await this.readAllModificationsForProposal(proposal.authorPub);

		if (proposal.modificationProposals.length === 0) {
			console.log('No modifications proposed. Proposal passes with original content.');
			return true;
		}

		// Step 5: Express support
		await this.expressSupportOfContentCandidates(proposal);
		await this.waitForSupport();

		// Step 6: Read all support and determine winner
		proposal.supportExpressions = await this.readAllSupportForProposal(proposal.authorPub);
		proposal.content = proposal.mostSupportedVersion();

		console.log(`Final proposal content: "${proposal.content}"`);
		return true;
	}

	// ========================================================================
	// PLAYER ACTIONS (to be implemented/overridden by UI or AI)
	// ========================================================================

	async expressProposals(prompt: string): Promise<void> {
		console.log(`I need to express a proposal for: "${prompt}"`);
		// This should be implemented by UI or AI
		// For now, using mock data
		const mockProposals = [
			`Implement a new training program to increase team collaboration.`,
			`Redesign the current workflow to reduce bottlenecks.`,
			`Launch a pilot project to test remote working arrangements.`,
		];
		const content = mockProposals[Math.floor(Math.random() * mockProposals.length)];
		await this.writeMyProposal(content);
	}

	async expressChallenges(proposal: Proposal): Promise<void> {
		console.log(`Should I challenge proposal by ${proposal.authorPub}?`);
		// This should be implemented by UI or AI
		// For now, randomly challenge
		const shouldChallenge = Math.random() > 0.7;
		if (shouldChallenge && proposal.authorPub !== this.myPublicKey) {
			const mockChallenges = [`Improve Vibes`, `Make more resonant`];
			const challenge = mockChallenges[Math.floor(Math.random() * mockChallenges.length)];
			await this.writeMyChallengeToProposal(proposal.authorPub, challenge);
		}
	}

	async expressCommentsAndModifications(proposal: Proposal): Promise<void> {
		console.log(`Commenting and proposing modifications for proposal by ${proposal.authorPub}`);
		// This should be implemented by UI or AI
		const mockComments = [`Love it`, `Hate it`, `Needs work`];
		const mockModifications = [`Make Caps Lock`, `All Lower Case`];

		const comment = mockComments[Math.floor(Math.random() * mockComments.length)];
		await this.writeMyCommentOnProposal(proposal.authorPub, comment);

		const shouldModify = Math.random() > 0.5;
		if (shouldModify) {
			const modification = mockModifications[Math.floor(Math.random() * mockModifications.length)];
			await this.writeMyModificationToProposal(proposal.authorPub, modification);
		}
	}

	async expressSupportOfContentCandidates(proposal: Proposal): Promise<void> {
		console.log(`Expressing support for proposal candidates...`);
		// This should be implemented by UI or AI
		const candidates = [
			proposal.content!,
			...proposal.modificationProposals.map(m => m.content)
		];

		const support: SupportExpression = {};
		candidates.forEach(candidate => {
			support[candidate] = Math.floor(Math.random() * 10);
		});

		await this.writeMySupportForProposal(proposal.authorPub, support);
	}

	// ========================================================================
	// SYNCHRONIZATION HELPERS
	// ========================================================================

	private async waitForProposals(): Promise<void> {
		console.log('Waiting for all participants to submit proposals...');
		// In a real implementation, this would use listeners or polling with timeouts
		await new Promise(resolve => setTimeout(resolve, 1000));
	}

	private async waitForChallenges(): Promise<void> {
		console.log('Waiting for all participants to submit challenges...');
		await new Promise(resolve => setTimeout(resolve, 500));
	}

	private async waitForCommentsAndModifications(): Promise<void> {
		console.log('Waiting for all participants to submit comments and modifications...');
		await new Promise(resolve => setTimeout(resolve, 500));
	}

	private async waitForSupport(): Promise<void> {
		console.log('Waiting for all participants to submit support...');
		await new Promise(resolve => setTimeout(resolve, 500));
	}

	// ========================================================================
	// LISTENERS (for real-time updates)
	// ========================================================================

	/**
	 * Sets up listeners for a specific proposal to get real-time updates.
	 */
	setupProposalListeners(proposalAuthorPub: string, onUpdate: (proposal: Proposal) => void): () => void {
		const unsubscribers: Array<() => void> = [];

		// Listen for challenges
		this.config!.participants.forEach(pubKey => {
			const unsub = listenAtPath(
				this.user,
				[pubKey, 'games', this.gameId, 'challenges', proposalAuthorPub],
				() => {
					console.log(`New challenge detected from ${pubKey}`);
					// Trigger update
					this.aggregateProposalData(proposalAuthorPub).then(onUpdate);
				},
				false
			);
			unsubscribers.push(unsub);
		});

		// Return combined unsubscriber
		return () => {
			unsubscribers.forEach(unsub => unsub());
		};
	}

	/**
	 * Aggregates all data for a proposal from all participants.
	 */
	private async aggregateProposalData(proposalAuthorPub: string): Promise<Proposal> {
		const proposal = new Proposal(proposalAuthorPub);
		proposal.challenges = await this.readAllChallengesForProposal(proposalAuthorPub);
		proposal.comments = await this.readAllCommentsForProposal(proposalAuthorPub);
		proposal.modificationProposals = await this.readAllModificationsForProposal(proposalAuthorPub);
		proposal.supportExpressions = await this.readAllSupportForProposal(proposalAuthorPub);
		return proposal;
	}
}

// ============================================================================
// EXPORTS
// ============================================================================

export { P2PDecider, Proposal, type GameConfig, type Challenge, type Comment, type ModificationProposal, type SupportExpression };
