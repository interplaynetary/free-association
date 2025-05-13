import type { Capacity, MaxDivisibility, SpaceTimeCoordinates, Forest, TreeZipper } from '../types';
import { LocationType } from '../types';
import { addChild, createRootNode } from '../tree';
import { addCapacity, createCapacityShare, addCapacityShare } from '../capacity';
import { addToForest } from './forest';

// Create an example capacity
export function createExampleCapacity(): Capacity {
	const coordinates: SpaceTimeCoordinates = {
		locationType: LocationType.Specific,
		allDay: true,
		recurrence: null,
		customRecurrence: null,
		startDate: new Date(),
		startTime: new Date(),
		endDate: new Date(),
		endTime: new Date(),
		timeZone: 'UTC'
	};

	const maxDivisibility: MaxDivisibility = {
		naturalDiv: 1,
		percentageDiv: 0.1 // Maximum 10% share
	};

	return {
		capacityId: 'room1',
		capacityName: 'Spare Room',
		quantity: 10,
		unit: 'room',
		shareDepth: 2,
		expanded: true,
		coordinates,
		maxDivisibility,
		hiddenUntilRequestAccepted: false
	};
}

// Create an example pie capacity
export function createExamplePieCapacity(): Capacity {
	const coordinates: SpaceTimeCoordinates = {
		locationType: LocationType.Specific,
		allDay: true,
		recurrence: null,
		customRecurrence: null,
		startDate: new Date(),
		startTime: new Date(),
		endDate: new Date(),
		endTime: new Date(),
		timeZone: 'UTC'
	};

	const maxDivisibility: MaxDivisibility = {
		naturalDiv: 1, // Can't split a slice
		percentageDiv: 0.125 // Minimum share is one slice (1/8)
	};

	return {
		capacityId: 'pie1',
		capacityName: 'Apple Pie',
		quantity: 8,
		unit: 'slices',
		shareDepth: 3,
		expanded: true,
		coordinates,
		maxDivisibility,
		hiddenUntilRequestAccepted: false
	};
}

// Create a forest with capacities
export function createCapacityForest(): { forest: Forest; ci: Forest } {
	// Create the base forest
	let forest: Forest = new Map();

	// Create root nodes with mutual contributors
	const aliceNode = createRootNode('alice', 'Alice', 100, ['bob', 'charlie'], null);
	const bobNode = createRootNode('bob', 'Bob', 100, ['alice', 'charlie'], null);
	const charlieNode = createRootNode('charlie', 'Charlie', 100, ['alice', 'bob'], null);

	// Create TreeZippers from the nodes
	const aliceRoot: TreeZipper = { zipperCurrent: aliceNode, zipperContext: null };
	const bobRoot: TreeZipper = { zipperCurrent: bobNode, zipperContext: null };
	const charlieRoot: TreeZipper = { zipperCurrent: charlieNode, zipperContext: null };

	// Create capacities
	const roomCapacity = createExampleCapacity();
	const pieCapacity = createExamplePieCapacity();

	// Add capacities to roots
	const aliceWithCapacity = addCapacity(roomCapacity, aliceRoot);
	const bobWithCapacity = addCapacity(pieCapacity, bobRoot);

	// Create capacity shares
	const aliceRoomShare = createCapacityShare(roomCapacity, 0.5); // 50% share of room
	const bobPieShare = createCapacityShare(pieCapacity, 0.25); // 25% share of pie (2 slices)

	// Add children and capacity shares
	const aliceWithChild = addChild('alice_child', 30, ['bob', 'charlie'], null, aliceWithCapacity);
	const bobWithChild = addChild('bob_child', 40, ['alice', 'charlie'], null, bobWithCapacity);
	const bobWithShare = aliceWithChild
		? addCapacityShare('alice_room', aliceRoomShare, bobWithChild!)
		: null;
	const charlieWithChild = addChild('charlie_child', 50, ['alice', 'bob'], null, charlieRoot);
	const charlieWithShare = bobWithShare
		? addCapacityShare('bob_pie', bobPieShare, charlieWithChild!)
		: null;

	// Build the forest
	if (aliceWithChild) forest = addToForest(forest, aliceWithChild);
	if (bobWithShare) forest = addToForest(forest, bobWithShare);
	if (charlieWithShare) forest = addToForest(forest, charlieWithShare);

	// Create a separate contributor index (in this case, identical to the forest)
	const ci: Forest = new Map(forest);

	return { forest, ci };
}
