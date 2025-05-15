import { Peerbit } from 'peerbit';
import { Program } from '@peerbit/program';
import { Documents, SearchRequest, StringMatch, StringMatchMethod, Or } from '@peerbit/document';
import { field, variant, vec } from '@dao-xyz/borsh';
import { v4 as uuid } from 'uuid';

/**
 * Node data type - unified representation of our node structure
 */
@variant(0)
class Node {
	@field({ type: 'string' })
	id!: string;

	@field({ type: 'string' })
	name!: string;

	@field({ type: 'u32' })
	points!: number;

	@field({ type: 'f32' })
	manualFulfillment?: number;

	@field({ type: vec('string') })
	childrenIds!: string[];

	@field({ type: 'bool' })
	isRoot!: boolean;

	@field({ type: vec('string') })
	contributors!: string[];

	@field({ type: vec('string') })
	capacityIds!: string[];

	@field({ type: vec('string') })
	capacityShareIds!: string[];

	constructor(props: {
		id?: string;
		name: string;
		points: number;
		manualFulfillment?: number;
		childrenIds?: string[];
		isRoot: boolean;
		contributors?: string[];
		capacityIds?: string[];
		capacityShareIds?: string[];
	}) {
		this.id = props.id || uuid();
		this.name = props.name;
		this.points = props.points;
		this.manualFulfillment = props.manualFulfillment;
		this.childrenIds = props.childrenIds || [];
		this.isRoot = props.isRoot;
		this.contributors = props.contributors || [];
		this.capacityIds = props.capacityIds || [];
		this.capacityShareIds = props.capacityShareIds || [];
	}
}

/**
 * Capacity representation
 */
@variant(0)
class Capacity {
	@field({ type: 'string' })
	id!: string;

	@field({ type: 'string' })
	name!: string;

	@field({ type: 'u32' })
	quantity!: number;

	@field({ type: 'string' })
	unit!: string;

	@field({ type: 'u8' })
	shareDepth!: number;

	@field({ type: 'u32' })
	naturalDivisibility!: number;

	@field({ type: 'f32' })
	percentageDivisibility!: number;

	constructor(props: {
		id?: string;
		name: string;
		quantity: number;
		unit: string;
		shareDepth: number;
		naturalDivisibility: number;
		percentageDivisibility: number;
	}) {
		this.id = props.id || uuid();
		this.name = props.name;
		this.quantity = props.quantity;
		this.unit = props.unit;
		this.shareDepth = props.shareDepth;
		this.naturalDivisibility = props.naturalDivisibility;
		this.percentageDivisibility = props.percentageDivisibility;
	}
}

/**
 * Capacity share representation
 */
@variant(0)
class CapacityShare {
	@field({ type: 'string' })
	id!: string;

	@field({ type: 'string' })
	targetCapacityId!: string;

	@field({ type: 'f32' })
	sharePercentage!: number;

	@field({ type: 'u32' })
	computedQuantity!: number;

	constructor(props: {
		id?: string;
		targetCapacityId: string;
		sharePercentage: number;
		computedQuantity: number;
	}) {
		this.id = props.id || uuid();
		this.targetCapacityId = props.targetCapacityId;
		this.sharePercentage = props.sharePercentage;
		this.computedQuantity = props.computedQuantity;
	}
}

/**
 * MutualFulfillment relationship between nodes
 */
@variant(0)
class MutualFulfillment {
	@field({ type: 'string' })
	id!: string;

	@field({ type: 'string' })
	sourceNodeId!: string;

	@field({ type: 'string' })
	targetNodeId!: string;

	@field({ type: 'f32' })
	fulfillmentScore!: number;

	@field({ type: 'u64' })
	timestamp!: number;

	constructor(props: {
		id?: string;
		sourceNodeId: string;
		targetNodeId: string;
		fulfillmentScore: number;
		timestamp?: number;
	}) {
		this.id = props.id || uuid();
		this.sourceNodeId = props.sourceNodeId;
		this.targetNodeId = props.targetNodeId;
		this.fulfillmentScore = props.fulfillmentScore;
		this.timestamp = props.timestamp || Date.now();
	}
}

/**
 * Forest Program - represents a peer's node tree
 */
@variant('forest')
class ForestProgram extends Program {
	@field({ type: Documents })
	nodes!: Documents<Node>;

	@field({ type: Documents })
	capacities!: Documents<Capacity>;

	@field({ type: Documents })
	capacityShares!: Documents<CapacityShare>;

	@field({ type: Documents })
	mutualFulfillments!: Documents<MutualFulfillment>;

	// Owner's root node ID
	@field({ type: 'string' })
	rootNodeId!: string;

	constructor() {
		super();
		this.nodes = new Documents();
		this.capacities = new Documents();
		this.capacityShares = new Documents();
		this.mutualFulfillments = new Documents();
		this.rootNodeId = '';
	}

	async open(options?: any): Promise<void> {
		// Initialize document stores with updated Peerbit API
		await this.nodes.open({
			type: Node,
			canPerform: () => true,
			replicate: true
		});

		await this.capacities.open({
			type: Capacity,
			canPerform: () => true,
			replicate: true
		});

		await this.capacityShares.open({
			type: CapacityShare,
			canPerform: () => true,
			replicate: true
		});

		await this.mutualFulfillments.open({
			type: MutualFulfillment,
			canPerform: () => true,
			replicate: true
		});
	}

	/**
	 * Initialize a forest with a root node
	 */
	async initializeRoot(name: string, points: number): Promise<Node> {
		if (this.rootNodeId) {
			throw new Error('Forest already has a root node');
		}

		const rootNode = new Node({
			name,
			points,
			isRoot: true
		});

		await this.nodes.put(rootNode);
		this.rootNodeId = rootNode.id;
		return rootNode;
	}

	/**
	 * Add a child node to a parent node
	 */
	async addChild(
		parentId: string,
		name: string,
		points: number,
		contributors: string[] = []
	): Promise<Node> {
		// Find the parent node by ID
		const parentResults = await this.nodes.index.search(
			new SearchRequest({
				query: [
					new StringMatch({
						key: 'id',
						value: parentId,
						method: StringMatchMethod.exact
					})
				]
			})
		);

		const parent = parentResults.length > 0 ? parentResults[0] : null;

		if (!parent) {
			throw new Error(`Parent node ${parentId} not found`);
		}

		// Don't allow adding children to nodes with contributors
		if (parent.contributors.length > 0) {
			throw new Error(`Cannot add children to node with contributors`);
		}

		const child = new Node({
			name,
			points,
			isRoot: false,
			contributors
		});

		await this.nodes.put(child);

		// Update parent node's children list
		parent.childrenIds.push(child.id);
		await this.nodes.put(parent);

		return child;
	}

	/**
	 * Add a capacity to the root node
	 */
	async addCapacity(
		name: string,
		quantity: number,
		unit: string,
		shareDepth: number,
		naturalDivisibility: number,
		percentageDivisibility: number
	): Promise<Capacity> {
		// Find the root node
		const rootResults = await this.nodes.index.search(
			new SearchRequest({
				query: [
					new StringMatch({
						key: 'id',
						value: this.rootNodeId,
						method: StringMatchMethod.exact
					})
				]
			})
		);

		const rootNode = rootResults.length > 0 ? rootResults[0] : null;

		if (!rootNode) {
			throw new Error('Root node not found');
		}

		const capacity = new Capacity({
			name,
			quantity,
			unit,
			shareDepth,
			naturalDivisibility,
			percentageDivisibility
		});

		await this.capacities.put(capacity);

		// Add capacity to root node
		rootNode.capacityIds.push(capacity.id);
		await this.nodes.put(rootNode);

		return capacity;
	}

	/**
	 * Get a node by ID
	 */
	async getNode(nodeId: string): Promise<Node | null> {
		const results = await this.nodes.index.search(
			new SearchRequest({
				query: [
					new StringMatch({
						key: 'id',
						value: nodeId,
						method: StringMatchMethod.exact
					})
				]
			})
		);

		return results.length > 0 ? results[0] : null;
	}

	/**
	 * Get all children of a node
	 */
	async getChildren(nodeId: string): Promise<Node[]> {
		// Find the node
		const node = await this.getNode(nodeId);

		if (!node) {
			return [];
		}

		const children: Node[] = [];
		for (const childId of node.childrenIds) {
			const child = await this.getNode(childId);
			if (child) {
				children.push(child);
			}
		}
		return children;
	}

	/**
	 * Simple implementation of the weight calculation
	 * from the Haskell code
	 */
	async calculateNodeWeight(nodeId: string): Promise<number> {
		const node = await this.getNode(nodeId);

		if (!node) {
			return 0;
		}

		// Root node always has weight 1.0
		if (node.isRoot) {
			return 1.0;
		}

		// Find the parent
		const parentNode = await this.findParentNode(nodeId);
		if (!parentNode) {
			return 1.0; // Default to full weight if parent isn't found
		}

		// Calculate total points of all siblings
		const siblings = await this.getChildren(parentNode.id);
		const totalPoints = siblings.reduce((sum, sibling) => sum + sibling.points, 0);

		// Calculate this node's contribution to parent
		const nodeContribution = totalPoints === 0 ? 0 : node.points / totalPoints;

		// Weight is recursive - multiply by parent's weight
		const parentWeight = await this.calculateNodeWeight(parentNode.id);
		return nodeContribution * parentWeight;
	}

	/**
	 * Find the parent node of a given node
	 */
	async findParentNode(nodeId: string): Promise<Node | null> {
		// Get all nodes from the index
		const query = new SearchRequest({});
		const allNodes = await this.nodes.index.search(query);

		for (const node of allNodes) {
			if (node.childrenIds.includes(nodeId)) {
				return node;
			}
		}
		return null;
	}

	/**
	 * Find all descendants of a node (recursive)
	 */
	async getAllDescendants(nodeId: string): Promise<Node[]> {
		const node = await this.getNode(nodeId);

		if (!node) {
			return [];
		}

		let descendants: Node[] = [];
		for (const childId of node.childrenIds) {
			const child = await this.getNode(childId);
			if (child) {
				descendants.push(child);
				const childDescendants = await this.getAllDescendants(childId);
				descendants = descendants.concat(childDescendants);
			}
		}
		return descendants;
	}
}

/**
 * A simplified network class for working with multiple forests
 */
class FreeAssociationNetwork {
	private peerbit: Peerbit;
	private localForest: ForestProgram;
	private remotePeers: Map<string, ForestProgram> = new Map();

	constructor(peerbit: Peerbit, localForest: ForestProgram) {
		this.peerbit = peerbit;
		this.localForest = localForest;
	}

	/**
	 * Join a remote peer's forest
	 */
	async joinForest(address: string): Promise<void> {
		const remoteForest = await this.peerbit.open<ForestProgram>(address);
		this.remotePeers.set(address, remoteForest);
	}

	/**
	 * Calculate mutual fulfillment between local and remote peers
	 */
	async calculateMutualFulfillment(
		localNodeId: string,
		remoteAddress: string,
		remoteNodeId: string
	): Promise<number> {
		const remoteForest = this.remotePeers.get(remoteAddress);
		if (!remoteForest) {
			throw new Error(`Remote peer ${remoteAddress} not found`);
		}

		// Get the nodes
		const localNode = await this.localForest.getNode(localNodeId);
		const remoteNode = await remoteForest.getNode(remoteNodeId);

		if (!localNode || !remoteNode) {
			return 0;
		}

		// Check for existing mutual fulfillment records using Or query
		const sourceTargetQuery = new StringMatch({
			key: 'sourceNodeId',
			value: localNodeId,
			method: StringMatchMethod.exact
		});

		const targetSourceQuery = new StringMatch({
			key: 'sourceNodeId',
			value: remoteNodeId,
			method: StringMatchMethod.exact
		});

		const orQuery = new Or([sourceTargetQuery, targetSourceQuery]);

		const mutualQuery = new SearchRequest({
			query: [orQuery]
		});

		const existingFulfillments =
			await this.localForest.mutualFulfillments.index.search(mutualQuery);

		if (existingFulfillments.length > 0) {
			// Sort by timestamp descending and take the latest
			const latestFulfillment = existingFulfillments.sort(
				(a: MutualFulfillment, b: MutualFulfillment) => b.timestamp - a.timestamp
			)[0];
			return latestFulfillment.fulfillmentScore;
		}

		// Implement a simplified version of the mutual fulfillment calculation
		// Looking at contributions in both directions

		// Check if the local node is a contributor to the remote node
		const isLocalContributingToRemote = remoteNode.contributors.includes(localNodeId);

		// Check if the remote node is a contributor to the local node
		const isRemoteContributingToLocal = localNode.contributors.includes(remoteNodeId);

		// Simple mutual fulfillment calculation
		if (isLocalContributingToRemote && isRemoteContributingToLocal) {
			return 0.75; // Strong mutual contribution
		} else if (isLocalContributingToRemote || isRemoteContributingToLocal) {
			return 0.5; // One-way contribution
		}

		// Check for transitive contributions through children
		const localChildren = await this.localForest.getAllDescendants(localNodeId);
		const remoteChildren = await remoteForest.getAllDescendants(remoteNodeId);

		// Look for any mutual contributions among children
		for (const localChild of localChildren) {
			for (const remoteChild of remoteChildren) {
				if (
					localChild.contributors.includes(remoteNodeId) ||
					remoteChild.contributors.includes(localNodeId)
				) {
					return 0.25; // Weaker transitive contribution
				}
			}
		}

		return 0; // No contribution relationship found
	}

	/**
	 * Create a capacity share based on mutual fulfillment
	 */
	async createCapacityShare(
		remoteAddress: string,
		remoteNodeId: string,
		capacityId: string
	): Promise<CapacityShare | null> {
		const remoteForest = this.remotePeers.get(remoteAddress);
		if (!remoteForest) {
			throw new Error(`Remote peer ${remoteAddress} not found`);
		}

		// Get the capacity
		const capacityResults = await remoteForest.capacities.index.search(
			new SearchRequest({
				query: [
					new StringMatch({
						key: 'id',
						value: capacityId,
						method: StringMatchMethod.exact
					})
				]
			})
		);

		const capacity = capacityResults.length > 0 ? capacityResults[0] : null;

		if (!capacity) {
			return null;
		}

		// Calculate mutual fulfillment
		const fulfillment = await this.calculateMutualFulfillment(
			this.localForest.rootNodeId,
			remoteAddress,
			remoteNodeId
		);

		// Apply the mutual fulfillment as share percentage, respecting divisibility
		const sharePercentage = Math.min(fulfillment, capacity.percentageDivisibility);

		// Calculate the computed quantity
		const rawQuantity = Math.round(capacity.quantity * sharePercentage);
		const computedQuantity =
			Math.floor(rawQuantity / capacity.naturalDivisibility) * capacity.naturalDivisibility;

		// Create and store the capacity share
		const share = new CapacityShare({
			targetCapacityId: capacityId,
			sharePercentage,
			computedQuantity
		});

		await this.localForest.capacityShares.put(share);

		// Add the capacity share to the root node
		const rootNode = await this.localForest.getNode(this.localForest.rootNodeId);

		if (rootNode) {
			rootNode.capacityShareIds.push(share.id);
			await this.localForest.nodes.put(rootNode);
		}

		return share;
	}
}

/**
 * Example usage demonstrating a simplified peer-to-peer setup
 */
async function main() {
	// Create peerbit instances for Alice and Bob
	const alicePeerbit = await Peerbit.create();
	const bobPeerbit = await Peerbit.create();

	// Create forests for each peer
	const aliceForest = new ForestProgram();
	const bobForest = new ForestProgram();

	// Open the forests
	await aliceForest.open();
	await bobForest.open();

	// Initialize root nodes
	const aliceRoot = await aliceForest.initializeRoot('Alice', 100);
	const bobRoot = await bobForest.initializeRoot('Bob', 100);

	// Add a capacity for Alice
	const roomCapacity = await aliceForest.addCapacity(
		'Spare Room',
		10,
		'room',
		2,
		1, // Natural divisibility
		0.1 // Percentage divisibility
	);

	// Add children with mutual contributions
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

	// Create network interfaces
	const aliceNetwork = new FreeAssociationNetwork(alicePeerbit, aliceForest);
	const bobNetwork = new FreeAssociationNetwork(bobPeerbit, bobForest);

	// Connect to each other's forests
	const aliceAddress = aliceForest.address.toString();
	const bobAddress = bobForest.address.toString();

	await aliceNetwork.joinForest(bobAddress);
	await bobNetwork.joinForest(aliceAddress);

	// Calculate mutual fulfillment
	const mutualFulfillment = await aliceNetwork.calculateMutualFulfillment(
		aliceRoot.id,
		bobAddress,
		bobRoot.id
	);

	console.log(`Mutual fulfillment between Alice and Bob: ${mutualFulfillment * 100}%`);

	// Create capacity shares based on mutual fulfillment
	const bobsRoomShare = await bobNetwork.createCapacityShare(
		aliceAddress,
		aliceRoot.id,
		roomCapacity.id
	);

	if (bobsRoomShare) {
		console.log(`Bob's share of Alice's room: ${bobsRoomShare.sharePercentage * 100}%`);
		console.log(`Computed quantity: ${bobsRoomShare.computedQuantity} ${roomCapacity.unit}`);
	}
}

export {
	ForestProgram,
	FreeAssociationNetwork,
	Node,
	Capacity,
	CapacityShare,
	MutualFulfillment,
	main
};
