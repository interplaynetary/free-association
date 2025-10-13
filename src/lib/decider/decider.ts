import { z } from 'zod';

// ============================================================================
// ZOD SCHEMAS
// ============================================================================

const PlayerSchema = z.string().min(1);

const ChallengeSchema = z.string();

const CommentSchema = z.string();

const ModificationProposalSchema = z.string();

const SupportExpressionSchema = z.record(z.string(), z.number().int().min(0));

const ProposalSchema = z.object({
	content: z.string().nullable(),
	challenges: z.array(ChallengeSchema),
	comments: z.array(CommentSchema),
	modificationProposals: z.array(ModificationProposalSchema),
	supportExpressions: z.array(SupportExpressionSchema),
});

const ProcessorConfigSchema = z.object({
	players: z.array(PlayerSchema),
	agenda: z.array(z.string()),
	timeWindow: z.number().int().positive().default(86400000), // 1 day in milliseconds
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
type ProcessorConfig = z.infer<typeof ProcessorConfigSchema>;

// ============================================================================
// PROPOSAL CLASS
// ============================================================================

class Proposal {
	content: string | null;
	challenges: Challenge[];
	comments: Comment[];
	modificationProposals: ModificationProposal[];
	supportExpressions: SupportExpression[];

	constructor() {
		this.content = null;
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

		const proposalCandidates: string[] = [this.content!, ...this.modificationProposals];
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
// DECIDER CLASS
// ============================================================================

class Decider {
	players: Player[];
	agenda: string[];
	passedProposals: Proposal[];
	timeWindow: number;

	constructor(players: Player[] = [], prompts: string[] = []) {
		this.players = [...players];
		this.agenda = [...prompts];
		this.passedProposals = [];
		this.timeWindow = 86400000; // 1 day in milliseconds
	}

	async expressProposals(prompt: string): Promise<Proposal[]> {
		console.log(`Expressing proposals in response to prompt: "${prompt}"`);
		const proposals: Proposal[] = [];

		const proposalPromises = this.players.map(player =>
			this.promptPlayer(player, `Express a Proposal in response to: ${prompt}`)
				.then(proposal => {
					console.log(`Player "${player}" proposed: "${proposal.content}"`);
					proposals.push(proposal);
				})
		);

		await Promise.all(proposalPromises);

		for (const proposal of proposals) {
			await this.expressChallenges(proposal);
		}
		return proposals;
	}

	async expressChallenges(proposal: Proposal): Promise<Challenge[]> {
		console.log(`Expressing challenges for proposal: "${proposal.content}"`);
		const challenges: Challenge[] = [];

		for (const player of this.players) {
			const response = await this.multipleChoiceResponse(
				player,
				'Challenge proposal?',
				['Challenge', 'No Challenge']
			);
			console.log(`Player "${player}" chose to: "${response}"`);

			if (response === 'Challenge') {
				const challenge = await this.promptPlayerForChallenge(
					player,
					'Explain Challenge to proposal?'
				);
				challenges.push(challenge);
				console.log(`Player "${player}" challenged the proposal with: "${challenge}"`);
			}
		}

		proposal.challenges = challenges;

		if (challenges.length === 0) {
			this.passedProposals.push(proposal);
			console.log(`No challenges found. Proposal "${proposal.content}" passed as the active proposal.`);
		} else {
			await this.expressCommentsAndModifications(proposal);
		}
		return challenges;
	}

	async expressCommentsAndModifications(proposal: Proposal): Promise<void> {
		console.log(`Gathering comments and modifications for proposal: "${proposal.content}"`);
		const comments: Comment[] = [];
		const modificationProposals: ModificationProposal[] = [];

		for (const player of this.players) {
			const comment = await this.promptPlayerForComment(
				player,
				'Comment on this proposal and proposal challenges!'
			);
			comments.push(comment);
			console.log(`Player "${player}" commented: "${comment}"`);

			const modification = await this.promptPlayerForModification(
				player,
				'Propose a modification to the proposal!'
			);
			modificationProposals.push(modification);
			console.log(`Player "${player}" proposed a modification: "${modification}"`);
		}

		proposal.comments = comments;
		proposal.modificationProposals = modificationProposals;

		if (modificationProposals.length === 0) {
			this.passedProposals.push(proposal);
			console.log(`No modifications proposed. Proposal "${proposal.content}" passed as the active proposal.`);
		} else {
			await this.expressSupportOfContentCandidates(proposal);
		}
	}

	async expressSupportOfContentCandidates(proposal: Proposal): Promise<void> {
		console.log(`Gathering support for active proposal candidates...`);
		const choices: string[] = [proposal.content!, ...proposal.modificationProposals];
		const supportExpressions: SupportExpression[] = [];

		for (const player of this.players) {
			const support = await this.poll(player, 'Support an active-proposal-candidate!', choices);
			supportExpressions.push(support);
			console.log(`Player "${player}" expressed support: ${JSON.stringify(support)}`);
		}

		proposal.supportExpressions = supportExpressions;
		proposal.content = proposal.mostSupportedVersion();
		this.passedProposals.push(proposal);
		console.log(`Final active proposal is now: "${proposal.content}"`);
	}

	async promptPlayer(player: Player, prompt: string): Promise<Proposal> {
		return new Promise<Proposal>(resolve => {
			console.log(`Prompting player "${player}": ${prompt}`);

			const possibleProposals = [
				`Implement a new training program to increase team collaboration.`,
				`Redesign the current workflow to reduce bottlenecks.`,
				`Launch a pilot project to test remote working arrangements.`,
				`Invest in new technology to automate repetitive tasks.`,
				`Introduce a feedback loop for faster decision-making.`,
			];

			const newProposal = new Proposal();
			const selectedProposal = possibleProposals[Math.floor(Math.random() * possibleProposals.length)];
			newProposal.content = selectedProposal;
			console.log(`Created new proposal:`, newProposal.content);
			resolve(newProposal);
		});
	}

	async multipleChoiceResponse(player: Player, question: string, options: string[]): Promise<string> {
		return new Promise<string>(resolve => {
			const selectedOption = options[Math.floor(Math.random() * options.length)];
			console.log(`Player "${player}" responded to "${question}" with: "${selectedOption}"`);
			resolve(selectedOption);
		});
	}

	async poll(player: Player, question: string, choices: string[]): Promise<SupportExpression> {
		return new Promise<SupportExpression>(resolve => {
			const result: SupportExpression = {};
			choices.forEach(choice => {
				const supportPoints = Math.floor(Math.random() * 10);
				result[choice] = supportPoints;

				if (supportPoints > 0) {
					console.log(`Player "${player}" supports "${choice}" with ${supportPoints} points because they find it promising.`);
				} else {
					console.log(`Player "${player}" does not support "${choice}" because they feel it lacks detail.`);
				}
			});
			console.log(`Polling player "${player}" for "${question}". Results: ${JSON.stringify(result)}`);
			resolve(result);
		});
	}

	async promptPlayerForComment(player: Player, prompt: string): Promise<Comment> {
		return new Promise<Comment>(resolve => {
			console.log(`Prompting player "${player}": ${prompt}`);

			const possibleComments = [
				`Love it`,
				`Hate it`
			];

			const selectedComment = possibleComments[Math.floor(Math.random() * possibleComments.length)];
			console.log(`Created new comment:`, selectedComment);
			resolve(selectedComment);
		});
	}

	async promptPlayerForModification(player: Player, prompt: string): Promise<ModificationProposal> {
		return new Promise<ModificationProposal>(resolve => {
			console.log(`Prompting player "${player}": ${prompt}`);

			const possibleModifications = [
				`Make Caps Lock`,
				`All Lower Case`
			];

			const selectedModification = possibleModifications[Math.floor(Math.random() * possibleModifications.length)];
			console.log(`Created new modification:`, selectedModification);
			resolve(selectedModification);
		});
	}

	async promptPlayerForChallenge(player: Player, prompt: string): Promise<Challenge> {
		return new Promise<Challenge>(resolve => {
			console.log(`Prompting player "${player}": ${prompt}`);

			const possibleChallenges = [
				`Improve Vibes`,
				`Make more resonant`
			];

			const selectedChallenge = possibleChallenges[Math.floor(Math.random() * possibleChallenges.length)];
			console.log(`Created new challenge:`, selectedChallenge);
			resolve(selectedChallenge);
		});
	}
}

// ============================================================================
// EXAMPLE USAGE
// ============================================================================

(async () => {
	const processor = new Decider(['ruz', 'pekko']);
	const result = await processor.expressProposals('What should we do?');
	console.log('Final result of proposals:', result);
})();