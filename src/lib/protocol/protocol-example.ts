/**
 * Comprehensive example of Free Association Protocol
 * This file demonstrates the usage of the protocol functions
 */

import {
	createRootNode,
	createNonRootNode,
	addChild,
	addContributors,
	updateManualFulfillment,
	weight,
	desire,
	fulfilled,
	findNodeById,
	getPathToNode,
	getDescendants,
	getParentNode,
	totalChildPoints,
	shareOfParent,
	mutualFulfillment,
	providerShares,
	isContribution,
	FilterRules,
	applyCapacityFilter,
	createCapacityShare,
	receiverShareFrom,
	addCapacity,
	addCapacityShare
} from './protocol';

import type { Capacity, CapacityShare, Node, RootNode, NonRootNode, ShareMap } from './protocol';

// Function to run the comprehensive example
function runProtocolExample() {
	console.log('Running Free Association Protocol Example');
	console.log('----------------------------------------');

	// 1. Create a small network of nodes
	console.log('\n1. Creating network of nodes');
	// Create root nodes for different users
	const alice = createRootNode('alice-root', 'Alice', 'alice-user-id');
	const bob = createRootNode('bob-root', 'Bob', 'bob-user-id');
	const charlie = createRootNode('charlie-root', 'Charlie', 'charlie-user-id');

	// Build Alice's tree
	addChild(alice, 'alice-1', 'Health', 30);
	addChild(alice, 'alice-2', 'Work', 50);
	addChild(alice, 'alice-3', 'Relationships', 20);

	const aliceHealth = findNodeById(alice, 'alice-1') as NonRootNode;
	const aliceWork = findNodeById(alice, 'alice-2') as NonRootNode;
	const aliceRelationships = findNodeById(alice, 'alice-3') as NonRootNode;

	// Add children to Alice's nodes
	addChild(aliceHealth, 'alice-1-1', 'Exercise', 60);
	addChild(aliceHealth, 'alice-1-2', 'Nutrition', 40);

	addChild(aliceWork, 'alice-2-1', 'Project A', 70);
	addChild(aliceWork, 'alice-2-2', 'Project B', 30);

	addChild(aliceRelationships, 'alice-3-1', 'Family', 50);
	addChild(aliceRelationships, 'alice-3-2', 'Friends', 50);

	// Build Bob's tree
	addChild(bob, 'bob-1', 'Fitness', 40);
	addChild(bob, 'bob-2', 'Career', 60);

	const bobFitness = findNodeById(bob, 'bob-1') as NonRootNode;
	const bobCareer = findNodeById(bob, 'bob-2') as NonRootNode;

	addChild(bobFitness, 'bob-1-1', 'Running', 50);
	addChild(bobFitness, 'bob-1-2', 'Gym', 50);

	addChild(bobCareer, 'bob-2-1', 'Programming', 30);
	addChild(bobCareer, 'bob-2-2', 'Design', 20);
	addChild(bobCareer, 'bob-2-3', 'Communication', 50);

	// Build Charlie's tree
	addChild(charlie, 'charlie-1', 'Art', 50);
	addChild(charlie, 'charlie-2', 'Business', 50);

	// Create a nodes map for looking up nodes by ID
	const nodesMap: Record<string, Node> = {
		'alice-root': alice,
		'bob-root': bob,
		'charlie-root': charlie
	};

	// Add all nodes to the nodesMap for lookup
	function addNodesToMap(node: Node) {
		nodesMap[node.id] = node;
		for (const child of node.children) {
			addNodesToMap(child);
		}
	}

	addNodesToMap(alice);
	addNodesToMap(bob);
	addNodesToMap(charlie);

	// Print the created trees
	console.log('Alice Tree before contributors:', JSON.stringify(alice, null, 2));

	// 2. Demonstrate tree navigation functions
	console.log('\n2. Tree Navigation');

	// Find node by ID
	const foundNode = findNodeById(alice, 'alice-2-1');
	console.log('Found Project A node:', foundNode?.name);

	// Get path to node
	const pathToNode = getPathToNode(alice, 'alice-2-1');
	console.log('Path to Project A:', pathToNode);

	// Get all descendants
	const aliceDescendants = getDescendants(alice);
	console.log('Alice has', aliceDescendants.length, 'descendants');

	// Get parent node
	const parentNode = getParentNode(alice, 'alice-2-1');
	console.log('Parent of Project A is:', parentNode?.name);

	// 3. Core calculations before adding contributors
	console.log('\n3. Core Calculations Before Contributors');

	// Total child points
	console.log('Total Alice Work points:', totalChildPoints(aliceWork));

	// Calculate weights
	console.log('Alice Health weight:', weight(aliceHealth, alice));
	console.log('Alice Work weight:', weight(aliceWork, alice));
	console.log('Alice Relationships weight:', weight(aliceRelationships, alice));

	// Set manual fulfillment for a node
	updateManualFulfillment(aliceHealth, 0.7);
	console.log('Set Alice Health manual_fulfillment to:', aliceHealth.manual_fulfillment);

	// Calculate fulfillment
	console.log(
		'Alice Health fulfillment (should use manual 0.7 if non-leaf nodes respect it):',
		fulfilled(aliceHealth, alice)
	);
	console.log('Alice Work fulfillment (no contributors yet):', fulfilled(aliceWork, alice));
	console.log('Alice overall fulfillment (before contributors):', fulfilled(alice, alice));

	// 4. Setup contributors for mutual fulfillment
	console.log('\n4. Setting up contributors');

	// Let's get the exercise node after it's already in the tree
	const aliceExercise = findNodeById(alice, 'alice-1-1') as NonRootNode;
	console.log('Exercise node before adding contributor:', JSON.stringify(aliceExercise, null, 2));
	console.log('Exercise node is contribution node?', isContribution(aliceExercise));

	// Add Bob as a contributor to Alice's Exercise node
	addContributors(aliceExercise, ['bob-root']);
	console.log("Added Bob as contributor to Alice's Exercise node");
	console.log('Exercise node after adding contributor:', JSON.stringify(aliceExercise, null, 2));
	console.log(
		'Exercise node is contribution node after adding Bob?',
		isContribution(aliceExercise)
	);

	// Check if fulfillment is 1.0 for the exercise node since it's a leaf contribution node
	console.log('Exercise node fulfillment (should be 1.0):', fulfilled(aliceExercise, alice));

	// Add Charlie as a contributor to Alice's Project A node
	const aliceProjectA = findNodeById(alice, 'alice-2-1') as NonRootNode;
	addContributors(aliceProjectA, ['charlie-root']);
	console.log("Added Charlie as contributor to Alice's Project A node");

	// Add Alice as a contributor to Bob's Programming node
	const bobProgramming = findNodeById(bob, 'bob-2-1') as NonRootNode;
	addContributors(bobProgramming, ['alice-root']);
	console.log("Added Alice as contributor to Bob's Programming node");

	// Re-check Alice's Health fulfillment after adding a contributor to one of its children
	console.log(
		'Alice Health fulfillment after adding contributor to Exercise:',
		fulfilled(aliceHealth, alice)
	);

	// Print Alice's tree after adding contributors
	console.log('Alice Tree after contributors:', JSON.stringify(alice, null, 2));

	// 5. Mutual fulfillment calculations
	console.log('\n5. Mutual Fulfillment');

	// Calculate mutual fulfillment
	const aliceBobMutual = mutualFulfillment(alice, bob, nodesMap);
	console.log('Mutual fulfillment between Alice and Bob:', aliceBobMutual);

	const aliceCharlieMutual = mutualFulfillment(alice, charlie, nodesMap);
	console.log('Mutual fulfillment between Alice and Charlie:', aliceCharlieMutual);

	// Provider shares
	const aliceShares = providerShares(alice, 1, nodesMap);
	console.log("Alice's provider shares (depth 1):", aliceShares);

	const aliceSharesDepth2 = providerShares(alice, 2, nodesMap);
	console.log("Alice's provider shares (depth 2):", aliceSharesDepth2);

	// Verify contributor IDs
	console.log('Alice Exercise contributor IDs:', aliceExercise.contributor_ids);
	console.log('Bob Programming contributor IDs:', bobProgramming.contributor_ids);

	// 6. Capacity management
	console.log('\n6. Capacity Management');

	// Create a capacity for Alice
	const aliceCapacity: Capacity = {
		id: 'alice-capacity-1',
		name: 'Programming Time',
		quantity: 10, // 10 hours
		unit: 'hours',
		share_depth: 2,
		expanded: true,
		location_type: 'remote',
		all_day: false,
		time_zone: 'UTC',
		max_natural_div: 1, // Minimum 1 hour chunks
		max_percentage_div: 0.5, // Maximum 50% to any one person
		hidden_until_request_accepted: false,
		owner_id: 'alice-user-id',
		shares: [],
		start_date: '2023-01-01',
		start_time: '09:00',
		end_date: '2023-01-01',
		end_time: '19:00',
		// Add a filter rule to only share with specific nodes
		filter_rule: FilterRules.and(
			FilterRules.excludeNodes(['charlie-root']), // Exclude Charlie
			FilterRules.aboveThreshold(0.1) // Only share with nodes that have at least 10% share
		)
	};

	// Add the capacity to Alice
	addCapacity(alice, aliceCapacity);
	console.log('Added Programming Time capacity to Alice');

	// Calculate shares based on provider shares and filter
	const rawAliceShares = providerShares(alice, aliceCapacity.share_depth, nodesMap);
	console.log('Raw provider shares:', rawAliceShares);

	const filteredShares = applyCapacityFilter(aliceCapacity, rawAliceShares, nodesMap);
	console.log('Filtered shares:', filteredShares);

	// Create capacity shares
	for (const [recipientId, sharePercentage] of Object.entries(filteredShares)) {
		if (sharePercentage > 0) {
			const capacityShare = createCapacityShare(
				aliceCapacity.id,
				recipientId,
				sharePercentage,
				aliceCapacity
			);
			addCapacityShare(alice, capacityShare);
			console.log(
				`Created capacity share for ${recipientId}: ${sharePercentage.toFixed(2)} (${capacityShare.computed_quantity} hours)`
			);
		}
	}

	// Check receiver's share
	const bobShareFromAlice = receiverShareFrom(
		bob,
		alice,
		aliceCapacity,
		aliceCapacity.share_depth,
		nodesMap
	);
	console.log("Bob's share from Alice's capacity:", bobShareFromAlice);

	// 7. Final state
	console.log('\n7. Final Network State');
	console.log(
		"Alice's capacities:",
		alice.capacities.map((c) => ({
			name: c.name,
			quantity: c.quantity,
			shares: c.shares.map((s) => ({
				recipient: s.recipient_id,
				percentage: s.share_percentage,
				quantity: s.computed_quantity
			}))
		}))
	);

	console.log('\nEnd of Protocol Example');
}

// Run the example
runProtocolExample();

export default runProtocolExample;
