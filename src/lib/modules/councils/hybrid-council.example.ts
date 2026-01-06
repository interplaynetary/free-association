/**
 * Hybrid Council - Usage Examples
 * 
 * Demonstrates combining proposal-based governance with recognition-based allocation.
 */

import {
	HybridCouncil,
	createHybridCouncil,
	printProposalStatus,
	printProposalsSummary
} from './hybrid-council';
import {
	createRecognitionData,
	printMembershipStatus,
	printAllocationSummary
} from './recognition-council';
import type { AvailabilitySlot } from '@playnet/free-association/schemas';
import type { BaseNeed } from '$lib/protocol/collective/schemas';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Hybrid Council
// ═══════════════════════════════════════════════════════════════════

export function example1_BasicHybrid() {
	console.log('\n=== EXAMPLE 1: Basic Hybrid Council ===\n');

	// Create hybrid council
	const council = createHybridCouncil('Community Resource Council', {
		mrd_threshold: 0.5,
		quorum_percentage: 0.5,  // 50% of voting power needed
		seed_members: ['alice', 'bob', 'charlie']
	});

	// Set up recognition
	const recognition = createRecognitionData({
		alice: { bob: 40, charlie: 35, diana: 25 },
		bob: { alice: 45, charlie: 30, diana: 25 },
		charlie: { alice: 35, bob: 35, diana: 30 },
		diana: { alice: 33, bob: 33, charlie: 34 }
	});

	council.updateRecognition(recognition);

	console.log('Council created with hybrid governance:');
	console.log('  - Membership: Recognition-based (MRD)');
	console.log('  - Voting: MRD-weighted proposals');
	console.log('  - Resources: Recognition-based allocation');

	printMembershipStatus(council);

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Governance via Proposals, Resources via Recognition
// ═══════════════════════════════════════════════════════════════════

export function example2_GovernanceVsResources() {
	console.log('\n=== EXAMPLE 2: Governance vs Resources ===\n');

	const council = example1_BasicHybrid();

	// GOVERNANCE: Add capacity via proposal (requires vote)
	console.log('\n--- Governance Decision: Add Collective Capacity ---');

	const capacitySlot: AvailabilitySlot = {
		id: 'grant_2024',
		quantity: 100000,  // $100K
		type_id: 'funding',
		name: 'Community Grant 2024',
		emoji: '💰',
		unit: 'USD'
	};

	const addCapacityProposal = council.proposeAddCapacity(capacitySlot);

	// Members vote (weight = MRD score)
	council.castVote('alice', addCapacityProposal, 'yes');
	council.castVote('bob', addCapacityProposal, 'yes');
	council.castVote('charlie', addCapacityProposal, 'yes');
	council.castVote('diana', addCapacityProposal, 'yes');

	printProposalStatus(council, addCapacityProposal);

	// Process proposals → capacity added if approved
	console.log('\n--- Processing Proposals ---');
	council.processProposalsSync();

	// RESOURCES: Allocation happens automatically via recognition
	console.log('\n--- Resource Allocation: Automatic via Recognition ---');

	// Members declare needs (no vote needed!)
	council.declareMemberNeeds('alice', [{
		id: 'alice_slot',
		quantity: 30000,
		type_id: 'funding',
		name: 'Community Garden',
		unit: 'USD'
	}]);

	council.declareMemberNeeds('bob', [{
		id: 'bob_slot',
		quantity: 40000,
		type_id: 'funding',
		name: 'Youth Programs',
		unit: 'USD'
	}]);

	council.declareMemberNeeds('charlie', [{
		id: 'charlie_slot',
		quantity: 25000,
		type_id: 'funding',
		name: 'Art Installation',
		unit: 'USD'
	}]);

	// Allocation computed automatically based on recognition
	printAllocationSummary(council);

	console.log('\n✅ Governance via proposals, resources via recognition!');

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: MRD-Weighted Voting
// ═══════════════════════════════════════════════════════════════════

export function example3_WeightedVoting() {
	console.log('\n=== EXAMPLE 3: MRD-Weighted Voting ===\n');

	const council = example1_BasicHybrid();

	// Alice has highest MRD → most voting power
	console.log('Voting power (MRD scores):');
	for (const member of council.members) {
		const mrd = council.getMRD(member);
		console.log(`  ${member}: ${mrd.toFixed(2)}`);
	}

	// Create proposal to change threshold
	console.log('\n--- Proposal: Lower MRD Threshold ---');
	const thresholdProposal = council.proposeThresholdChange(0.3);

	// Vote with different weights
	council.castVote('alice', thresholdProposal, 'yes');  // High weight
	council.castVote('bob', thresholdProposal, 'no');     // Medium weight
	council.castVote('charlie', thresholdProposal, 'no'); // Medium weight
	council.castVote('diana', thresholdProposal, 'yes');  // Lower weight

	printProposalStatus(council, thresholdProposal);

	// Alice's high MRD might swing it even if outnumbered!
	const weightedVotes = council.getWeightedVotes(thresholdProposal);
	console.log(`\nWeighted result:`);
	console.log(`  Alice + Diana (yes): ${weightedVotes.yes.toFixed(2)}`);
	console.log(`  Bob + Charlie (no): ${weightedVotes.no.toFixed(2)}`);

	if (council.isProposalApproved(thresholdProposal)) {
		console.log(`\n✅ Proposal APPROVED (deeper recognition = more influence)`);
	} else {
		console.log(`\n❌ Proposal REJECTED`);
	}

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Compliance Filters via Proposal
// ═══════════════════════════════════════════════════════════════════

export function example4_FilterProposals() {
	console.log('\n=== EXAMPLE 4: Compliance Filters via Proposal ===\n');

	const council = example2_GovernanceVsResources();

	// Council votes to cap Bob's allocation (e.g., compliance requirement)
	console.log('--- Proposal: Cap Bob at $20K ---');
	const filterProposal = council.proposeSetFilter('bob', {
		type: 'capped',
		value: 20000
	});

	// Everyone votes yes (compliance requirement)
	council.castVote('alice', filterProposal, 'yes');
	council.castVote('bob', filterProposal, 'yes');  // Bob agrees to cap
	council.castVote('charlie', filterProposal, 'yes');
	council.castVote('diana', filterProposal, 'yes');

	printProposalStatus(council, filterProposal);

	// Execute
	console.log('\n--- Processing Proposals ---');
	council.processProposalsSync();

	// Allocations now respect the cap
	console.log('\n--- Allocations with Filter Applied ---');
	printAllocationSummary(council);

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Multiple Proposals Workflow
// ═══════════════════════════════════════════════════════════════════

export function example5_MultipleProposals() {
	console.log('\n=== EXAMPLE 5: Multiple Proposals Workflow ===\n');

	const council = example1_BasicHybrid();

	// Add capacity via proposal
	const capacity1 = council.proposeAddCapacity({
		id: 'funding_slot',
		quantity: 50000,
		type_id: 'funding',
		name: 'General Fund',
		unit: 'USD'
	});

	const capacity2 = council.proposeAddCapacity({
		id: 'space_slot',
		quantity: 100,
		type_id: 'workspace',
		name: 'Community Center',
		unit: 'hours',
		city: 'Berlin'
	});

	// Change threshold
	const thresholdChange = council.proposeThresholdChange(0.4);

	// Add filter
	const filterChange = council.proposeSetFilter('alice', {
		type: 'capped',
		value: 30000
	});

	console.log('--- 4 Proposals Created ---');
	printProposalsSummary(council);

	// Vote on all proposals
	console.log('\n--- Voting Round ---');
	for (const proposal of council.allProposals) {
		council.castVote('alice', proposal, 'yes');
		council.castVote('bob', proposal, 'yes');
		council.castVote('charlie', proposal, 'no');  // Charlie votes no on everything
	}

	// Process all
	console.log('\n--- Processing All Proposals ---');
	const results = await council.processProposalsSync();

	console.log(`\nResults: ${results.filter(r => r.isApproved).length}/${results.length} approved`);
	printProposalsSummary(council);

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Dynamic Membership Affects Voting Power
// ═══════════════════════════════════════════════════════════════════

export function example6_DynamicVotingPower() {
	console.log('\n=== EXAMPLE 6: Dynamic Membership Affects Voting Power ===\n');

	const council = createHybridCouncil('Evolving Council', {
		mrd_threshold: 0.5,
		seed_members: ['alice', 'bob']
	});

	// Initial recognition (only Alice and Bob)
	let recognition = createRecognitionData({
		alice: { bob: 50, charlie: 25, diana: 25 },
		bob: { alice: 50, charlie: 25, diana: 25 },
		charlie: { alice: 40, bob: 40, diana: 20 },
		diana: { alice: 35, bob: 35, charlie: 30 }
	});

	council.updateRecognition(recognition);

	console.log('Initial state:');
	printMembershipStatus(council);

	console.log(`\nTotal voting power: ${council.getTotalVotingPower().toFixed(2)}`);
	console.log(`Quorum: ${council.getQuorum().toFixed(2)}`);

	// Create proposal
	const proposal = council.proposeAddCapacity({
		id: 'test_slot',
		quantity: 1000,
		type_id: 'funding',
		name: 'Test',
		unit: 'USD'
	});

	// Alice and Bob vote yes
	council.castVote('alice', proposal, 'yes');
	council.castVote('bob', proposal, 'yes');

	console.log('\n--- Before Recognition Update ---');
	printProposalStatus(council, proposal);

	// Recognition increases for Charlie and Diana → they become members
	recognition = createRecognitionData({
		alice: { bob: 40, charlie: 30, diana: 30 },
		bob: { alice: 40, charlie: 30, diana: 30 },
		charlie: { alice: 35, bob: 35, diana: 30 },
		diana: { alice: 33, bob: 33, charlie: 34 }
	});

	council.updateRecognition(recognition);

	console.log('\n--- After Recognition Update ---');
	printMembershipStatus(council);

	console.log(`\nNew total voting power: ${council.getTotalVotingPower().toFixed(2)}`);
	console.log(`New quorum: ${council.getQuorum().toFixed(2)}`);

	// Proposal status changed! (quorum increased, might not pass anymore)
	printProposalStatus(council, proposal);

	console.log('\n⚠️  Recognition changes affect voting power and quorum!');

	return council;
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

export async function runAllHybridExamples() {
	example1_BasicHybrid();
	example2_GovernanceVsResources();
	example3_WeightedVoting();
	example4_FilterProposals();
	await example5_MultipleProposals();
	example6_DynamicVotingPower();

	console.log('\n=== All hybrid examples completed ===\n');
}

// Uncomment to run
// runAllHybridExamples();

