/**
 * Hybrid Council - Combines Proposals with Recognition-Based Allocation
 * 
 * This extends RecognitionCouncil to add traditional proposal-based voting
 * for governance decisions while keeping recognition-based resource allocation.
 * 
 * Use Cases:
 * - Governance decisions: Proposal-based voting (explicit consent)
 * - Resource allocation: Recognition-based (automatic/continuous)
 * - Threshold changes: Proposal-based (affects membership)
 * - Capacity management: Proposal-based (collective decision)
 * - Member needs: Recognition-based (self-declared)
 * 
 * Key Features:
 * - Vote weight = MRD score (deeper recognition = more influence)
 * - Quorum based on total member MRD
 * - Proposals can execute methods on council
 * - Recognition still determines resource allocation
 */

import { RecognitionCouncil } from './recognition-council';
import type { RecognitionCouncilConfig } from './recognition-council';
import type { AvailabilitySlot, NeedSlot } from '../../../../packages/protocol/src/schemas';
import type { ComplianceFilter } from '../../../../packages/protocol/src/utils/filters';

// ═══════════════════════════════════════════════════════════════════
// PROPOSAL SYSTEM (from councils.ts)
// ═══════════════════════════════════════════════════════════════════

type VoteDecision = 'yes' | 'no' | 'abstain';

interface Action {
	description: string;
	methodName: string | null;
	methodArgs: any[];
}

interface VoteResult {
	yes: number;
	no: number;
}

export interface ProposalStatus {
	proposal: Proposal;
	description: string;
	votes: VoteResult;
	totalVotingPower: number;
	quorum: number;
	isApproved: boolean;
	execution_result?: any;
}

class Proposal {
	public readonly description: string;
	public readonly votes: Map<string, VoteDecision> = new Map();
	public readonly actions: Map<any, Action> = new Map();
	public readonly created_at: number;
	public executed: boolean = false;
	
	constructor(description: string) {
		this.description = description;
		this.created_at = Date.now();
	}
	
	addAction(
		target: any,
		description: string,
		methodName: string | null = null,
		methodArgs: any[] = []
	): void {
		this.actions.set(target, {
			description,
			methodName,
			methodArgs
		});
	}
	
	castVote(voterId: string, decision: VoteDecision): void {
		this.votes.set(voterId, decision);
	}
	
	getCurrentVotes(): VoteResult {
		let yes = 0;
		let no = 0;
		
		for (const [_, vote] of this.votes.entries()) {
			if (vote === 'yes') yes++;
			else if (vote === 'no') no++;
		}
		
		return { yes, no };
	}
	
	get supporters(): string[] {
		return Array.from(this.votes.entries())
			.filter(([_, vote]) => vote === 'yes')
			.map(([voterId, _]) => voterId);
	}
}

// ═══════════════════════════════════════════════════════════════════
// HYBRID COUNCIL
// ═══════════════════════════════════════════════════════════════════

/**
 * Hybrid Council - Recognition + Proposals
 * 
 * Governance via proposals, resources via recognition.
 * 
 * Pattern:
 * - Membership: Emerges from recognition (MRD)
 * - Voting power: Weighted by MRD score
 * - Governance: Proposal-based voting
 * - Resources: Recognition-based allocation
 */
export class HybridCouncil extends RecognitionCouncil {
	private proposals: Proposal[] = [];
	private quorum_percentage: number;
	
	constructor(name: string, config: RecognitionCouncilConfig & {
		quorum_percentage?: number;
	} = {}) {
		super(name, config);
		this.quorum_percentage = config.quorum_percentage ?? 0.5; // 50% default
	}
	
	// ═══════════════════════════════════════════════════════════════
	// PROPOSAL CREATION
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Create a new proposal
	 * 
	 * @param description - What the proposal does
	 * @param actions - Actions to execute if approved
	 * @returns Proposal instance
	 */
	createProposal(description: string, actions?: Map<any, Action>): Proposal {
		const proposal = new Proposal(description);
		
		if (actions) {
			for (const [target, action] of actions.entries()) {
				proposal.addAction(target, action.description, action.methodName, action.methodArgs);
			}
		}
		
		this.proposals.push(proposal);
		
		console.log(`[HYBRID-COUNCIL ${this.councilName}] Proposal created: ${description}`);
		
		return proposal;
	}
	
	/**
	 * Propose to add collective capacity
	 */
	proposeAddCapacity(slot: AvailabilitySlot): Proposal {
		return this.createProposal(
			`Add collective capacity: ${slot.name} (${slot.quantity} ${slot.unit || 'units'})`,
			new Map([[this, {
				description: 'Add capacity slot',
				methodName: 'addCollectiveCapacity',
				methodArgs: [slot]
			}]])
		);
	}
	
	/**
	 * Propose to remove collective capacity
	 */
	proposeRemoveCapacity(slotId: string): Proposal {
		return this.createProposal(
			`Remove collective capacity: ${slotId}`,
			new Map([[this, {
				description: 'Remove capacity slot',
				methodName: 'removeCollectiveCapacity',
				methodArgs: [slotId]
			}]])
		);
	}
	
	/**
	 * Propose to change MRD threshold
	 */
	proposeThresholdChange(newThreshold: number): Proposal {
		return this.createProposal(
			`Change MRD threshold to ${newThreshold}`,
			new Map([[this, {
				description: 'Update membership threshold',
				methodName: 'setThreshold',
				methodArgs: [newThreshold]
			}]])
		);
	}
	
	/**
	 * Propose to set member filter
	 */
	proposeSetFilter(memberId: string, filter: ComplianceFilter): Proposal {
		const filterDesc = filter.type === 'blocked' ? 'BLOCK' 
			: filter.type === 'capped' ? `CAP at ${filter.value}`
			: 'UNLIMITED';
		
		return this.createProposal(
			`Set filter for ${memberId}: ${filterDesc}`,
			new Map([[this, {
				description: 'Set compliance filter',
				methodName: 'setMemberFilter',
				methodArgs: [memberId, filter]
			}]])
		);
	}
	
	/**
	 * Propose to remove member filter
	 */
	proposeRemoveFilter(memberId: string): Proposal {
		return this.createProposal(
			`Remove filter for ${memberId}`,
			new Map([[this, {
				description: 'Remove compliance filter',
				methodName: 'removeMemberFilter',
				methodArgs: [memberId]
			}]])
		);
	}
	
	// ═══════════════════════════════════════════════════════════════
	// VOTING
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Cast vote on a proposal
	 * 
	 * Vote weight = voter's MRD score
	 * Only members can vote
	 * 
	 * @param voterId - Member identifier (pubkey)
	 * @param proposal - Proposal to vote on
	 * @param decision - Vote decision
	 */
	castVote(voterId: string, proposal: Proposal, decision: VoteDecision): void {
		if (!this.isMember(voterId)) {
			console.warn(`[HYBRID-COUNCIL] ${voterId} is not a member, cannot vote`);
			return;
		}
		
		const weight = this.getMRD(voterId);
		proposal.castVote(voterId, decision);
		
		console.log(`[HYBRID-COUNCIL ${this.councilName}] ${voterId} votes ${decision} ` +
			`(weight: ${weight.toFixed(2)}) on: ${proposal.description}`);
	}
	
	/**
	 * Get weighted vote counts for a proposal
	 */
	getWeightedVotes(proposal: Proposal): VoteResult {
		let yes = 0;
		let no = 0;
		
		for (const [voterId, decision] of proposal.votes.entries()) {
			const weight = this.getMRD(voterId);
			
			if (decision === 'yes') {
				yes += weight;
			} else if (decision === 'no') {
				no += weight;
			}
			// abstain doesn't count
		}
		
		return { yes, no };
	}
	
	/**
	 * Calculate total voting power (sum of all member MRDs)
	 */
	getTotalVotingPower(): number {
		return Array.from(this.current_members)
			.reduce((sum, memberId) => sum + this.getMRD(memberId), 0);
	}
	
	/**
	 * Calculate quorum (threshold for proposal approval)
	 */
	getQuorum(): number {
		return this.getTotalVotingPower() * this.quorum_percentage;
	}
	
	/**
	 * Check if proposal is approved
	 */
	isProposalApproved(proposal: Proposal): boolean {
		const weightedVotes = this.getWeightedVotes(proposal);
		const quorum = this.getQuorum();
		return weightedVotes.yes >= quorum;
	}
	
	// ═══════════════════════════════════════════════════════════════
	// PROPOSAL PROCESSING
	// ═══════════════════════════════════════════════════════════════
	
	/**
	 * Execute a proposal's actions
	 */
	private executeProposal(proposal: Proposal): any {
		if (proposal.executed) {
			console.warn(`[HYBRID-COUNCIL] Proposal already executed: ${proposal.description}`);
			return;
		}
		
		const results: any[] = [];
		
		for (const [target, action] of proposal.actions.entries()) {
			if (!action.methodName) continue;
			
			try {
				const method = (target as any)[action.methodName];
				if (typeof method === 'function') {
					const result = method.apply(target, action.methodArgs);
					results.push(result);
					console.log(`[HYBRID-COUNCIL] Executed: ${action.methodName}(${action.methodArgs.join(', ')})`);
				}
			} catch (error) {
				console.error(`[HYBRID-COUNCIL] Error executing ${action.methodName}:`, error);
				results.push({ error: String(error) });
			}
		}
		
		proposal.executed = true;
		return results;
	}
	
	/**
	 * Process all proposals and execute approved ones
	 * 
	 * @returns Async generator of proposal statuses
	 */
	async *processProposals(): AsyncGenerator<ProposalStatus, void, unknown> {
		for (const proposal of this.proposals) {
			if (proposal.executed) continue; // Skip already executed
			
			const weightedVotes = this.getWeightedVotes(proposal);
			const totalVotingPower = this.getTotalVotingPower();
			const quorum = this.getQuorum();
			const isApproved = weightedVotes.yes >= quorum;
			
			const status: ProposalStatus = {
				proposal,
				description: proposal.description,
				votes: weightedVotes,
				totalVotingPower,
				quorum,
				isApproved
			};
			
			if (isApproved) {
				console.log(`[HYBRID-COUNCIL ${this.councilName}] Proposal APPROVED: ${proposal.description}`);
				status.execution_result = this.executeProposal(proposal);
			}
			
			yield status;
		}
	}
	
	/**
	 * Process proposals synchronously (simpler API)
	 */
	async processProposalsSync(): Promise<ProposalStatus[]> {
		const statuses: ProposalStatus[] = [];
		for await (const status of this.processProposals()) {
			statuses.push(status);
		}
		return statuses;
	}
	
	// ═══════════════════════════════════════════════════════════════
	// GETTERS
	// ═══════════════════════════════════════════════════════════════
	
	get allProposals(): Proposal[] {
		return [...this.proposals];
	}
	
	get pendingProposals(): Proposal[] {
		return this.proposals.filter(p => !p.executed);
	}
	
	get executedProposals(): Proposal[] {
		return this.proposals.filter(p => p.executed);
	}
	
	/**
	 * Get proposals that have enough votes to pass
	 */
	get approvableProposals(): Proposal[] {
		return this.pendingProposals.filter(p => this.isProposalApproved(p));
	}
}

// ═══════════════════════════════════════════════════════════════════
// HELPER FUNCTIONS
// ═══════════════════════════════════════════════════════════════════

/**
 * Create hybrid council
 */
export function createHybridCouncil(
	name: string,
	config: RecognitionCouncilConfig & { quorum_percentage?: number } = {}
): HybridCouncil {
	return new HybridCouncil(name, config);
}

/**
 * Print proposal status
 */
export function printProposalStatus(council: HybridCouncil, proposal: Proposal): void {
	const weightedVotes = council.getWeightedVotes(proposal);
	const quorum = council.getQuorum();
	const totalPower = council.getTotalVotingPower();
	const isApproved = council.isProposalApproved(proposal);
	
	console.log(`\n=== Proposal: ${proposal.description} ===`);
	console.log(`Votes (weighted):`);
	console.log(`  Yes: ${weightedVotes.yes.toFixed(2)}`);
	console.log(`  No: ${weightedVotes.no.toFixed(2)}`);
	console.log(`Quorum: ${quorum.toFixed(2)} / ${totalPower.toFixed(2)}`);
	console.log(`Status: ${isApproved ? '✅ APPROVED' : '❌ NOT APPROVED'}`);
	console.log(`Executed: ${proposal.executed ? 'Yes' : 'No'}`);
	
	// Show individual votes
	console.log(`\nIndividual votes:`);
	for (const [voterId, decision] of proposal.votes.entries()) {
		const weight = council.getMRD(voterId);
		console.log(`  ${voterId}: ${decision} (weight: ${weight.toFixed(2)})`);
	}
}

/**
 * Print all proposals summary
 */
export function printProposalsSummary(council: HybridCouncil): void {
	console.log(`\n=== ${council.councilName} - Proposals Summary ===`);
	console.log(`Total: ${council.allProposals.length}`);
	console.log(`Pending: ${council.pendingProposals.length}`);
	console.log(`Executed: ${council.executedProposals.length}`);
	console.log(`Approvable: ${council.approvableProposals.length}`);
	
	if (council.pendingProposals.length > 0) {
		console.log(`\nPending proposals:`);
		for (const proposal of council.pendingProposals) {
			const isApproved = council.isProposalApproved(proposal);
			const status = isApproved ? '✅' : '⏳';
			console.log(`  ${status} ${proposal.description}`);
		}
	}
}

export { Proposal, type VoteDecision, type Action };

