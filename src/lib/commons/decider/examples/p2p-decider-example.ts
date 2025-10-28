/**
 * Example: P2P Decider with Holster
 * 
 * This example demonstrates how multiple players use the P2P Decider
 * to reach consensus in a distributed manner.
 */

import { P2PDecider } from '../src/lib/p2p-decider.svelte';
// import Holster from './path/to/holster';

// ============================================================================
// SETUP
// ============================================================================

/**
 * This example assumes you have Holster set up and running.
 * In a real application, you would import Holster and create user instances.
 */

async function setupPlayers() {
	// const holster = Holster();
	
	// // Create and authenticate users
	// const user1 = holster.user();
	// await user1.create('alice', 'password123');
	// await user1.auth('alice', 'password123');
	
	// const user2 = holster.user();
	// await user2.create('bob', 'password456');
	// await user2.auth('bob', 'password456');
	
	// const user3 = holster.user();
	// await user3.create('charlie', 'password789');
	// await user3.auth('charlie', 'password789');
	
	// return { user1, user2, user3 };
}

// ============================================================================
// EXAMPLE 1: Simple 2-Player Game
// ============================================================================

async function example1_SimpleTwoPlayerGame() {
	console.log('\n========================================');
	console.log('EXAMPLE 1: Simple 2-Player Game');
	console.log('========================================\n');

	// const { user1, user2 } = await setupPlayers();
	// const gameId = 'game-' + Date.now();

	// // Alice creates the game
	// console.log('--- Alice creates game ---');
	// const aliceDecider = new P2PDecider(user1, gameId);
	// await aliceDecider.createGame(
	// 	['What should we have for dinner?'],
	// 	[user2.is.pub]  // Include Bob
	// );

	// // Bob joins the game
	// console.log('\n--- Bob joins game ---');
	// const bobDecider = new P2PDecider(user2, gameId);
	// await bobDecider.joinGame(user1.is.pub);

	// // Both players run the decision flow
	// console.log('\n--- Both players run decision flow ---');
	// const [aliceResults, bobResults] = await Promise.all([
	// 	aliceDecider.runDecisionFlow(),
	// 	bobDecider.runDecisionFlow()
	// ]);

	// console.log('\nAlice\'s view of results:', aliceResults);
	// console.log('Bob\'s view of results:', bobResults);

	console.log('See p2p-architecture.md for full implementation details');
}

// ============================================================================
// EXAMPLE 2: Custom Player Actions
// ============================================================================

/**
 * This example shows how to override the default player actions
 * to implement custom UI or AI behavior.
 */
class CustomP2PDecider extends P2PDecider {
	private playerName: string;

	constructor(user: any, gameId: string, playerName: string) {
		super(user, gameId);
		this.playerName = playerName;
	}

	// Override to provide custom proposal logic
	async expressProposals(prompt: string): Promise<void> {
		console.log(`${this.playerName} is thinking about: "${prompt}"`);
		
		// Custom logic here - could be:
		// - UI prompt for user input
		// - AI-generated proposal
		// - Predefined strategy
		
		const myProposal = await this.getProposalFromUI(prompt);
		await this.writeMyProposal(myProposal);
	}

	// Override to provide custom challenge logic
	async expressChallenges(proposal: any): Promise<void> {
		console.log(`${this.playerName} is evaluating proposal by ${proposal.authorPub}`);
		
		// Don't challenge my own proposal
		if (proposal.authorPub === this.myPublicKey) {
			return;
		}

		const shouldChallenge = await this.evaluateProposal(proposal);
		if (shouldChallenge) {
			const challenge = await this.getChallengeFromUI(proposal);
			await this.writeMyChallengeToProposal(proposal.authorPub, challenge);
		}
	}

	// Override to provide custom comment/modification logic
	async expressCommentsAndModifications(proposal: any): Promise<void> {
		console.log(`${this.playerName} is commenting on proposal by ${proposal.authorPub}`);
		
		const comment = await this.getCommentFromUI(proposal);
		await this.writeMyCommentOnProposal(proposal.authorPub, comment);

		const shouldModify = await this.askUserIfModify(proposal);
		if (shouldModify) {
			const modification = await this.getModificationFromUI(proposal);
			await this.writeMyModificationToProposal(proposal.authorPub, modification);
		}
	}

	// Override to provide custom support logic
	async expressSupportOfContentCandidates(proposal: any): Promise<void> {
		console.log(`${this.playerName} is allocating support points`);
		
		const candidates = [
			proposal.content,
			...proposal.modificationProposals.map((m: any) => m.content)
		];

		const support = await this.allocateSupportPoints(candidates);
		await this.writeMySupportForProposal(proposal.authorPub, support);
	}

	// Helper methods (to be implemented with actual UI/AI)
	private async getProposalFromUI(prompt: string): Promise<string> {
		// Mock implementation
		return `${this.playerName}'s proposal for: ${prompt}`;
	}

	private async evaluateProposal(proposal: any): Promise<boolean> {
		// Mock implementation
		return Math.random() > 0.7;
	}

	private async getChallengeFromUI(proposal: any): Promise<string> {
		// Mock implementation
		return `Challenge from ${this.playerName}`;
	}

	private async getCommentFromUI(proposal: any): Promise<string> {
		// Mock implementation
		return `Comment from ${this.playerName}`;
	}

	private async askUserIfModify(proposal: any): Promise<boolean> {
		// Mock implementation
		return Math.random() > 0.5;
	}

	private async getModificationFromUI(proposal: any): Promise<string> {
		// Mock implementation
		return `Modified by ${this.playerName}`;
	}

	private async allocateSupportPoints(candidates: string[]): Promise<Record<string, number>> {
		// Mock implementation
		const support: Record<string, number> = {};
		const totalPoints = 10;
		const remainingPoints = totalPoints;
		
		candidates.forEach((candidate, i) => {
			if (i === candidates.length - 1) {
				support[candidate] = remainingPoints;
			} else {
				const points = Math.floor(Math.random() * (remainingPoints + 1));
				support[candidate] = points;
			}
		});
		
		return support;
	}
}

// ============================================================================
// EXAMPLE 3: Path Structure Visualization
// ============================================================================

/**
 * This example visualizes the data paths that get created during a game.
 */
function example3_PathVisualization() {
	console.log('\n========================================');
	console.log('EXAMPLE 3: Path Structure');
	console.log('========================================\n');

	const alicePub = 'alice-public-key-abc123';
	const bobPub = 'bob-public-key-def456';
	const gameId = 'game-xyz789';

	console.log('After Alice creates a game and Bob joins:');
	console.log('');
	console.log(`user[${alicePub}]/`);
	console.log(`  games/`);
	console.log(`    ${gameId}/`);
	console.log(`      config: { participants: ['${alicePub}', '${bobPub}'], agenda: ['What for dinner?'], ... }`);
	console.log('');
	console.log(`user[${bobPub}]/`);
	console.log(`  games/`);
	console.log(`    ${gameId}/`);
	console.log(`      config: { participants: ['${alicePub}', '${bobPub}'], agenda: ['What for dinner?'], ... }`);
	console.log('');

	console.log('After both express proposals for agenda item 0:');
	console.log('');
	console.log(`user[${alicePub}]/games/${gameId}/`);
	console.log(`  proposals/`);
	console.log(`    0: { content: 'Pizza', timestamp: 1234567890, authorPub: '${alicePub}' }`);
	console.log('');
	console.log(`user[${bobPub}]/games/${gameId}/`);
	console.log(`  proposals/`);
	console.log(`    0: { content: 'Sushi', timestamp: 1234567891, authorPub: '${bobPub}' }`);
	console.log('');

	console.log('After Bob challenges Alice\'s proposal:');
	console.log('');
	console.log(`user[${bobPub}]/games/${gameId}/`);
	console.log(`  challenges/`);
	console.log(`    ${alicePub}: { content: 'Too greasy', timestamp: 1234567892, authorPub: '${bobPub}' }`);
	console.log('');

	console.log('After both comment and Alice proposes modification:');
	console.log('');
	console.log(`user[${alicePub}]/games/${gameId}/`);
	console.log(`  comments/`);
	console.log(`    ${bobPub}: { content: 'Sushi sounds good', timestamp: 1234567893, authorPub: '${alicePub}' }`);
	console.log(`  modifications/`);
	console.log(`    ${bobPub}: { content: 'Sushi with miso soup', timestamp: 1234567894, authorPub: '${alicePub}' }`);
	console.log('');
	console.log(`user[${bobPub}]/games/${gameId}/`);
	console.log(`  comments/`);
	console.log(`    ${alicePub}: { content: 'Pizza needs veggies', timestamp: 1234567895, authorPub: '${bobPub}' }`);
	console.log(`  modifications/`);
	console.log(`    ${alicePub}: { content: 'Vegetarian pizza', timestamp: 1234567896, authorPub: '${bobPub}' }`);
	console.log('');

	console.log('After both express support for Bob\'s proposal candidates:');
	console.log('');
	console.log(`user[${alicePub}]/games/${gameId}/`);
	console.log(`  support/`);
	console.log(`    ${bobPub}: { 'Sushi': 3, 'Sushi with miso soup': 7 }`);
	console.log('');
	console.log(`user[${bobPub}]/games/${gameId}/`);
	console.log(`  support/`);
	console.log(`    ${bobPub}: { 'Sushi': 4, 'Sushi with miso soup': 6 }`);
	console.log('');

	console.log('Both players read all support and independently calculate:');
	console.log('Winner: "Sushi with miso soup" (13 total points)');
}

// ============================================================================
// EXAMPLE 4: Real-time Listeners
// ============================================================================

async function example4_RealtimeListeners() {
	console.log('\n========================================');
	console.log('EXAMPLE 4: Real-time Listeners');
	console.log('========================================\n');

	// const { user1, user2 } = await setupPlayers();
	// const gameId = 'game-' + Date.now();

	// const aliceDecider = new P2PDecider(user1, gameId);
	// await aliceDecider.createGame(['What to build?'], [user2.is.pub]);

	// // Alice sets up listeners for Bob's proposal
	// console.log('Alice sets up listeners for Bob\'s proposal');
	// const unsubscribe = aliceDecider.setupProposalListeners(
	// 	user2.is.pub,
	// 	(updatedProposal) => {
	// 		console.log('🔔 Alice received update for Bob\'s proposal:');
	// 		console.log('  Challenges:', updatedProposal.challenges.length);
	// 		console.log('  Comments:', updatedProposal.comments.length);
	// 		console.log('  Modifications:', updatedProposal.modificationProposals.length);
	// 		console.log('  Support:', updatedProposal.supportExpressions.length);
	// 	}
	// );

	// // Later, when Alice wants to stop listening
	// setTimeout(() => {
	// 	console.log('Alice unsubscribes from listeners');
	// 	unsubscribe();
	// }, 60000);

	console.log('See P2PDecider.setupProposalListeners() for implementation');
}

// ============================================================================
// EXAMPLE 5: Multi-Agenda Game
// ============================================================================

async function example5_MultiAgendaGame() {
	console.log('\n========================================');
	console.log('EXAMPLE 5: Multi-Agenda Game');
	console.log('========================================\n');

	// const { user1, user2, user3 } = await setupPlayers();
	// const gameId = 'game-' + Date.now();

	// // Create game with multiple agenda items
	// const decider1 = new P2PDecider(user1, gameId);
	// await decider1.createGame(
	// 	[
	// 		'What product should we build?',
	// 		'What features should it have?',
	// 		'What should the pricing be?'
	// 	],
	// 	[user2.is.pub, user3.is.pub]
	// );

	// // Process first agenda item
	// console.log('\n--- Processing agenda item 0 ---');
	// await decider1.runDecisionFlow();

	// // Move to next agenda item (would need to implement this)
	// // decider1.config.currentAgendaIndex++;
	// // await decider1.writeConfig();

	// // Process second agenda item
	// console.log('\n--- Processing agenda item 1 ---');
	// await decider1.runDecisionFlow();

	console.log('Multi-agenda support could be added by managing currentAgendaIndex');
}

// ============================================================================
// RUN EXAMPLES
// ============================================================================

async function main() {
	console.log('P2P Decider Examples');
	console.log('====================\n');
	console.log('These examples demonstrate the P2P architecture.');
	console.log('To run with real Holster instances, uncomment the code.\n');

	// Run examples
	await example1_SimpleTwoPlayerGame();
	example3_PathVisualization();
	await example4_RealtimeListeners();
	await example5_MultiAgendaGame();

	console.log('\n========================================');
	console.log('Examples complete!');
	console.log('========================================\n');
	console.log('For full documentation, see:');
	console.log('  docs/p2p-architecture.md');
}

// Uncomment to run:
// main().catch(console.error);

export { example1_SimpleTwoPlayerGame, example3_PathVisualization, example4_RealtimeListeners, example5_MultiAgendaGame };

