import { Peerbit } from 'peerbit';
import { ForestProgram, FreeAssociationNetwork } from './peerbit.js';
import { SearchRequest, StringMatch, StringMatchMethod } from '@peerbit/document';

/**
 * Simple test function to demonstrate the Peerbit implementation
 */
async function testPeerbitImplementation() {
	console.log('Starting Peerbit Free Association test...');

	try {
		// Create Peerbit instances for Alice and Bob
		console.log('Creating Peerbit instances...');
		const alicePeerbit = await Peerbit.create();
		const bobPeerbit = await Peerbit.create();

		console.log('Alice Peer ID:', alicePeerbit.identity.publicKey.toString());
		console.log('Bob Peer ID:', bobPeerbit.identity.publicKey.toString());

		// Create forests for each peer
		console.log('\nInitializing forests...');
		const aliceForest = new ForestProgram();
		const bobForest = new ForestProgram();

		// Open the forests
		await aliceForest.open();
		await bobForest.open();

		console.log('Alice Forest Address:', aliceForest.address.toString());
		console.log('Bob Forest Address:', bobForest.address.toString());

		// Initialize root nodes
		console.log('\nCreating root nodes...');
		const aliceRoot = await aliceForest.initializeRoot('Alice', 100);
		const bobRoot = await bobForest.initializeRoot('Bob', 100);

		console.log('Alice Root ID:', aliceRoot.id);
		console.log('Bob Root ID:', bobRoot.id);

		// Add a capacity for Alice
		console.log('\nAdding capacity to Alice...');
		const roomCapacity = await aliceForest.addCapacity(
			'Spare Room',
			10,
			'room',
			2,
			1, // Natural divisibility
			0.1 // Percentage divisibility
		);

		console.log('Capacity ID:', roomCapacity.id);
		console.log('Capacity Details:', {
			name: roomCapacity.name,
			quantity: roomCapacity.quantity,
			unit: roomCapacity.unit
		});

		// Add children with mutual contributions
		console.log('\nAdding children with mutual contributions...');
		const aliceChild = await aliceForest.addChild(
			aliceRoot.id,
			'alice_child',
			30,
			[bobRoot.id] // Bob is a contributor
		);

		const bobChild = await bobForest.addChild(
			bobRoot.id,
			'bob_child',
			40,
			[aliceRoot.id] // Alice is a contributor
		);

		console.log('Alice Child:', {
			id: aliceChild.id,
			name: aliceChild.name,
			contributors: aliceChild.contributors
		});

		console.log('Bob Child:', {
			id: bobChild.id,
			name: bobChild.name,
			contributors: bobChild.contributors
		});

		// Create network interfaces
		console.log('\nCreating network interfaces...');
		const aliceNetwork = new FreeAssociationNetwork(alicePeerbit, aliceForest);
		const bobNetwork = new FreeAssociationNetwork(bobPeerbit, bobForest);

		// Connect to each other's forests
		console.log('\nConnecting peers...');
		const aliceAddress = aliceForest.address.toString();
		const bobAddress = bobForest.address.toString();

		await aliceNetwork.joinForest(bobAddress);
		await bobNetwork.joinForest(aliceAddress);

		console.log('Connection established between peers');

		// Calculate mutual fulfillment
		console.log('\nCalculating mutual fulfillment...');
		const mutualFulfillment = await aliceNetwork.calculateMutualFulfillment(
			aliceRoot.id,
			bobAddress,
			bobRoot.id
		);

		console.log(`Mutual fulfillment between Alice and Bob: ${mutualFulfillment * 100}%`);

		// Create capacity shares based on mutual fulfillment
		console.log('\nCreating capacity shares...');
		const bobsRoomShare = await bobNetwork.createCapacityShare(
			aliceAddress,
			aliceRoot.id,
			roomCapacity.id
		);

		if (bobsRoomShare) {
			console.log(`Bob's share of Alice's room: ${bobsRoomShare.sharePercentage * 100}%`);
			console.log(`Computed quantity: ${bobsRoomShare.computedQuantity} ${roomCapacity.unit}`);
		} else {
			console.log('Failed to create capacity share');
		}

		// Verify that Bob has the capacity share
		console.log('\nVerifying capacity shares...');
		const bobRootNode = await bobForest.getNode(bobRoot.id);
		if (bobRootNode) {
			console.log(`Bob's capacity share IDs: ${bobRootNode.capacityShareIds.join(', ')}`);

			// If we have a capacity share ID, try to retrieve the share details
			if (bobRootNode.capacityShareIds.length > 0) {
				const shareId = bobRootNode.capacityShareIds[0];
				// Use index.search instead of query
				const shares = await bobForest.capacityShares.index.search(
					new SearchRequest({
						query: [
							new StringMatch({
								key: 'id',
								value: shareId,
								method: StringMatchMethod.exact
							})
						]
					})
				);
				const share = shares.length > 0 ? shares[0] : null;

				if (share) {
					console.log('Retrieved capacity share:', {
						id: share.id,
						targetCapacityId: share.targetCapacityId,
						sharePercentage: share.sharePercentage,
						computedQuantity: share.computedQuantity
					});
				}
			}
		}

		console.log('\nTest completed successfully!');

		// Close Peerbit instances
		await alicePeerbit.stop();
		await bobPeerbit.stop();
	} catch (error) {
		console.error('Error during test:', error);
	}
}

// Run the test
testPeerbitImplementation().catch(console.error);

// Use proper ES module exports
export { testPeerbitImplementation };
