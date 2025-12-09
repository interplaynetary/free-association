/**
 * Recognition-Based Council - Usage Examples
 * 
 * Demonstrates how to use the recognition council system
 * for various use cases.
 */

import {
	RecognitionCouncil,
	createRecognitionCouncil,
	createRecognitionData,
	printMembershipStatus,
	printAllocationSummary
} from './recognition-council';
import type { AvailabilitySlot, NeedSlot } from '../../../../packages/protocol/src/schemas';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 1: Basic Climate Action Council
// ═══════════════════════════════════════════════════════════════════

export function example1_BasicCouncil() {
	console.log('\n=== EXAMPLE 1: Basic Climate Action Council ===\n');
	
	// Create council with seed members
	const council = createRecognitionCouncil('Climate Action Council', {
		mrd_threshold: 0.5,
		seed_members: ['alice', 'bob', 'charlie']
	});
	
	// Create recognition data (who recognizes whom)
	const recognition = createRecognitionData({
		alice: { bob: 40, charlie: 30, diana: 30 },
		bob: { alice: 50, charlie: 30, diana: 20 },
		charlie: { alice: 35, bob: 35, diana: 30 },
		diana: { alice: 33, bob: 33, charlie: 34 }
	});
	
	// Update recognition → membership computed automatically
	const membership = council.updateRecognition(recognition);
	
	console.log('Initial membership:', membership.members);
	console.log('MRD scores:', membership.mrdScores);
	
	// Print status
	printMembershipStatus(council);
	
	// Diana has high mutual recognition → becomes member!
	console.log(`\nDiana became member: ${council.isMember('diana')}`);
	console.log(`Diana's MRD: ${council.getMRD('diana').toFixed(2)}`);
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 2: Resource Allocation
// ═══════════════════════════════════════════════════════════════════

export function example2_ResourceAllocation() {
	console.log('\n=== EXAMPLE 2: Resource Allocation ===\n');
	
	const council = example1_BasicCouncil();
	
	// Council declares collective capacity (grant to allocate)
	const grantSlot: AvailabilitySlot = {
		id: 'grant_2024',
		quantity: 100000,  // $100K
		need_type_id: 'funding',
		name: 'Climate Action Grant 2024',
		emoji: '💰',
		unit: 'USD',
		description: 'Annual grant for climate projects'
	};
	
	council.addCollectiveCapacity(grantSlot);
	
	// Members declare their needs
	const aliceNeedSlots: NeedSlot[] = [{
		id: 'alice_slot_1',
		quantity: 30000,
		need_type_id: 'funding',
		name: 'Solar Panel Installation',
		emoji: '☀️',
		unit: 'USD'
	}];
	
	const bobNeedSlots: NeedSlot[] = [{
		id: 'bob_slot_1',
		quantity: 40000,
		need_type_id: 'funding',
		name: 'Urban Reforestation',
		emoji: '🌳',
		unit: 'USD'
	}];
	
	const charlieNeedSlots: NeedSlot[] = [{
		id: 'charlie_slot_1',
		quantity: 25000,
		need_type_id: 'funding',
		name: 'Community Education',
		emoji: '📚',
		unit: 'USD'
	}];
	
	council.declareMemberNeeds('alice', aliceNeedSlots);
	council.declareMemberNeeds('bob', bobNeedSlots);
	council.declareMemberNeeds('charlie', charlieNeedSlots);
	
	// Compute allocations based on recognition!
	const summary = council.getAllocationSummary();
	
	console.log('\nAllocations computed:');
	console.log('Total Capacity:', summary.total_capacity);
	console.log('Total Allocated:', summary.total_allocated);
	console.log('Unused:', summary.unused_capacity);
	
	printAllocationSummary(council);
	
	// Each member gets allocated proportionally to their recognition share
	console.log('\nRecognition shares:');
	for (const [member, share] of Object.entries(summary.member_shares)) {
		console.log(`  ${member}: ${(share * 100).toFixed(1)}%`);
	}
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 3: Evolving Recognition & Membership
// ═══════════════════════════════════════════════════════════════════

export function example3_EvolvingMembership() {
	console.log('\n=== EXAMPLE 3: Evolving Recognition & Membership ===\n');
	
	const council = example2_ResourceAllocation();
	
	console.log('Initial members:', council.members);
	
	// Eve joins the network and starts getting recognized
	const updatedRecognition = createRecognitionData({
		alice: { bob: 35, charlie: 25, diana: 25, eve: 15 },
		bob: { alice: 45, charlie: 25, diana: 15, eve: 15 },
		charlie: { alice: 30, bob: 30, diana: 25, eve: 15 },
		diana: { alice: 30, bob: 30, charlie: 25, eve: 15 },
		eve: { alice: 25, bob: 25, charlie: 25, diana: 25 }
	});
	
	const membership = council.updateRecognition(updatedRecognition);
	
	console.log('\nAfter recognition update:');
	console.log('Members:', membership.members);
	console.log('Added:', membership.added);
	console.log('Removed:', membership.removed);
	
	printMembershipStatus(council);
	
	// Check if Eve became a member
	if (council.isMember('eve')) {
		console.log(`\nEve joined! MRD: ${council.getMRD('eve').toFixed(2)}`);
		
		// Eve can now declare needs and receive allocations
		const eveNeedSlots: NeedSlot[] = [{
			id: 'eve_slot_1',
			quantity: 15000,
			need_type_id: 'funding',
			name: 'Climate Data Platform',
			emoji: '📊',
			unit: 'USD'
		}];
		
		council.declareMemberNeeds('eve', eveNeedSlots);
		
		// Recompute allocations with Eve included
		printAllocationSummary(council);
	}
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 4: Compliance Filters
// ═══════════════════════════════════════════════════════════════════

export function example4_ComplianceFilters() {
	console.log('\n=== EXAMPLE 4: Compliance Filters ===\n');
	
	const council = example2_ResourceAllocation();
	
	// Apply filters to members
	// Alice: Blocked (e.g., due to compliance issue)
	council.setMemberFilter('alice', { type: 'blocked' });
	
	// Bob: Capped at $20K (e.g., jurisdiction limit)
	council.setMemberFilter('bob', { type: 'capped', value: 20000 });
	
	// Charlie: Unlimited (default)
	council.setMemberFilter('charlie', { type: 'unlimited' });
	
	console.log('Filters applied:');
	console.log('  Alice: BLOCKED ($0)');
	console.log('  Bob: CAPPED ($20K max)');
	console.log('  Charlie: UNLIMITED');
	
	// Compute allocations with filters
	const allocations = council.computeAllocations();
	
	console.log('\nAllocations with filters:');
	printAllocationSummary(council);
	
	console.log('\nFilter effects:');
	console.log(`  Alice ideal: $${allocations.ideal_allocations['alice']?.toFixed(0) || 0}`);
	console.log(`  Alice final: $${allocations.final_allocations['alice']?.toFixed(0) || 0}`);
	console.log(`  Bob ideal: $${allocations.ideal_allocations['bob']?.toFixed(0) || 0}`);
	console.log(`  Bob final: $${allocations.final_allocations['bob']?.toFixed(0) || 0}`);
	
	// Unused capacity redistributed to unlimited members
	console.log(`\nUnused capacity: $${allocations.unused_capacity.toFixed(0)}`);
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 5: Threshold Adjustment
// ═══════════════════════════════════════════════════════════════════

export function example5_ThresholdAdjustment() {
	console.log('\n=== EXAMPLE 5: Threshold Adjustment ===\n');
	
	const council = createRecognitionCouncil('Flexible Council', {
		mrd_threshold: 0.8,  // High threshold (exclusive)
		seed_members: ['alice', 'bob']
	});
	
	const recognition = createRecognitionData({
		alice: { bob: 50, charlie: 30, diana: 20 },
		bob: { alice: 50, charlie: 30, diana: 20 },
		charlie: { alice: 40, bob: 40, diana: 20 },
		diana: { alice: 35, bob: 35, charlie: 30 }
	});
	
	council.updateRecognition(recognition);
	
	console.log('High threshold (0.8):');
	printMembershipStatus(council);
	
	// Lower threshold to be more inclusive
	console.log('\n--- Lowering threshold to 0.5 ---\n');
	council.setThreshold(0.5);
	
	console.log('Lower threshold (0.5):');
	printMembershipStatus(council);
	
	console.log('\nMembership expanded with lower threshold!');
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE 6: Multi-Type Resources
// ═══════════════════════════════════════════════════════════════════

export function example6_MultiTypeResources() {
	console.log('\n=== EXAMPLE 6: Multi-Type Resources ===\n');
	
	const council = example1_BasicCouncil();
	
	// Council has multiple resource types
	council.addCollectiveCapacity({
		id: 'funding_slot',
		quantity: 50000,
		need_type_id: 'funding',
		name: 'Grant Funding',
		emoji: '💰',
		unit: 'USD'
	});
	
	council.addCollectiveCapacity({
		id: 'space_slot',
		quantity: 100,  // hours
		need_type_id: 'workspace',
		name: 'Community Center',
		emoji: '🏢',
		unit: 'hours',
		location_type: 'in-person',
		city: 'Berlin'
	});
	
	council.addCollectiveCapacity({
		id: 'expertise_slot',
		quantity: 40,  // hours
		need_type_id: 'consulting',
		name: 'Climate Expert Consulting',
		emoji: '🧑‍🏫',
		unit: 'hours'
	});
	
	// Members declare needs across types
	council.declareMemberNeeds('alice', [
		{
			id: 'alice_funding',
			quantity: 15000,
			need_type_id: 'funding',
			name: 'Project Funding',
			unit: 'USD'
		},
		{
			id: 'alice_space',
			quantity: 30,
			need_type_id: 'workspace',
			name: 'Meeting Space',
			unit: 'hours',
			location_type: 'in-person',
			city: 'Berlin'
		}
	]);
	
	council.declareMemberNeeds('bob', [
		{
			id: 'bob_funding',
			quantity: 20000,
			need_type_id: 'funding',
			name: 'Campaign Funding',
			unit: 'USD'
		},
		{
			id: 'bob_consulting',
			quantity: 15,
			need_type_id: 'consulting',
			name: 'Expert Advice',
			unit: 'hours'
		}
	]);
	
	console.log('Council resources:');
	console.log('  - $50K funding');
	console.log('  - 100 hours workspace');
	console.log('  - 40 hours consulting');
	
	printAllocationSummary(council);
	
	return council;
}

// ═══════════════════════════════════════════════════════════════════
// RUN ALL EXAMPLES
// ═══════════════════════════════════════════════════════════════════

export function runAllExamples() {
	example1_BasicCouncil();
	example2_ResourceAllocation();
	example3_EvolvingMembership();
	example4_ComplianceFilters();
	example5_ThresholdAdjustment();
	example6_MultiTypeResources();
	
	console.log('\n=== All examples completed ===\n');
}

// Uncomment to run examples
// runAllExamples();

