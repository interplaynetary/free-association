import { Peerbit } from "peerbit";
import { Program } from "@peerbit/program";
import {
    Documents,
    SearchRequest,
    StringMatch,
    StringMatchMethod,
    IntegerCompare,
    PutOperation,
    DeleteOperation,
    Sort,
    Compare,
    Query,
    Or,
} from "@peerbit/document";
import { field, variant, vec } from "@dao-xyz/borsh";
import { v4 as uuid } from "uuid";
import { PublicSignKey, sha256Sync, randomBytes } from "@peerbit/crypto";
import { webSockets } from "@libp2p/websockets";
import { circuitRelayTransport } from "@libp2p/circuit-relay-v2";
import * as filters from "@libp2p/websockets/filters";
import { concat } from "uint8arrays";

// are we properly using the rootId? perhaps it should simply be the public key?

// right now we are treating sogf and provider-shares as caches, but we should be persisting these! So they can be queried by others!

/**
 * Node data type - unified representation of our node structure
 */
@variant("node")
class Node {
    @field({ type: "string" })
    id!: string;

    @field({ type: "string" })
    name!: string;

    @field({ type: "u32" })
    points!: number;

    @field({ type: "f32" })
    manualFulfillment?: number;

    @field({ type: vec("string") })
    childrenIds!: string[];

    @field({ type: "bool" })
    isRoot!: boolean;

    @field({ type: vec("string") })
    contributors!: string[];

    @field({ type: vec("string") })
    capacityIds!: string[];

    constructor(props: {
        id?: string;
        name: string;
        points: number;
        manualFulfillment?: number;
        childrenIds?: string[];
        isRoot: boolean;
        contributors?: string[];
        capacityIds?: string[];
    }) {
        this.id = props.id || uuid();
        this.name = props.name;
        this.points = props.points;
        this.manualFulfillment = props.manualFulfillment;
        this.childrenIds = props.childrenIds || [];
        this.isRoot = props.isRoot;
        this.contributors = props.contributors || [];
        this.capacityIds = props.capacityIds || [];
    }
}

/**
 * Capacity representation
 */
@variant("capacity")
class Capacity {
    @field({ type: "string" })
    capacityId!: string;

    @field({ type: "string" })
    capacityName!: string;

    @field({ type: "u32" })
    quantity!: number;

    @field({ type: "string" })
    unit!: string;

    @field({ type: "u8" })
    shareDepth!: number;

    @field({ type: "u32" })
    naturalDiv!: number;

    @field({ type: "f32" })
    percentageDiv!: number;

    constructor(props: {
        capacityId?: string;
        capacityName: string;
        quantity: number;
        unit: string;
        shareDepth: number;
        naturalDiv: number;
        percentageDiv: number;
    }) {
        this.capacityId = props.capacityId || uuid();
        this.capacityName = props.capacityName;
        this.quantity = props.quantity;
        this.unit = props.unit;
        this.shareDepth = props.shareDepth;
        this.naturalDiv = props.naturalDiv;
        this.percentageDiv = props.percentageDiv;
    }
}

/**
 * Tree Program - represents a peer's node tree
 */
@variant("tree")
class Tree extends Program {
    @field({ type: Documents })
    nodes!: Documents<Node>;

    @field({ type: Documents })
    capacities!: Documents<Capacity>;

    // Owner's root node ID
    @field({ type: "string" })
    rootNodeId!: string;

    // Cache for shares of general fulfillment map
    private nodeSOGFMap: Map<string, number> | undefined;

    // Cache for provider shares map
    private nodeProviderSharesMap: Map<number, Map<string, number>> = new Map();

    constructor() {
        super();
        // Initialize nodes store with unique ID
        this.nodes = new Documents<Node>({
            id: sha256Sync(
                concat([
                    new TextEncoder().encode("tree-nodes"),
                    randomBytes(32), // Add randomness to ensure unique ID
                ])
            ),
        });

        // Initialize capacities store with unique ID
        this.capacities = new Documents<Capacity>({
            id: sha256Sync(
                concat([
                    new TextEncoder().encode("tree-capacities"),
                    randomBytes(32), // Add randomness to ensure unique ID
                ])
            ),
        });

        this.rootNodeId = "";
    }

    async open(options?: any): Promise<void> {
        // Initialize document stores with updated Peerbit API
        await this.nodes.open({
            type: Node,
            canPerform: () => true,
            replicate: true,
            index: {
                type: Node,
                idProperty: "id",
            },
        });

        await this.capacities.open({
            type: Capacity,
            canPerform: () => true,
            replicate: true,
            index: {
                type: Capacity,
                idProperty: "capacityId",
            },
        });
    }

    /**
     * Initialize a tree with a root node
     */
    async initializeRoot(name: string, points: number): Promise<Node> {
        if (this.rootNodeId) {
            throw new Error("Tree already has a root node");
        }

        const rootNode = new Node({
            name: name,
            points: points,
            isRoot: true,
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
        const parent = await this.getNode(parentId);
        if (!parent) {
            throw new Error(`Parent node ${parentId} not found`);
        }

        // Don't allow adding children to nodes with contributors
        if (parent.contributors.length > 0) {
            throw new Error(`Cannot add children to node with contributors`);
        }

        const child = new Node({
            name: name,
            points: points,
            isRoot: false,
            contributors: contributors,
        });

        await this.nodes.put(child);

        // Update parent node's children list
        parent.childrenIds.push(child.id);
        await this.nodes.put(parent);

        // Clear caches when structure changes
        this.nodeSOGFMap = undefined;
        this.nodeProviderSharesMap.clear();

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
        naturalDiv: number,
        percentageDiv: number
    ): Promise<Capacity> {
        // Find the root node
        const rootNode = await this.getNode(this.rootNodeId);
        if (!rootNode) {
            throw new Error("Root node not found");
        }

        const capacity = new Capacity({
            capacityName: name,
            quantity,
            unit,
            shareDepth,
            naturalDiv,
            percentageDiv,
        });

        await this.capacities.put(capacity);

        // Add capacity to root node
        rootNode.capacityIds.push(capacity.capacityId);
        await this.nodes.put(rootNode);

        return capacity;
    }

    /**
     * Get a node by ID
     */
    async getNode(id: string): Promise<Node | null> {
        try {
            return await this.nodes.index.get(id);
        } catch (error) {
            console.error(`Error fetching node ${id}:`, error);
            return null;
        }
    }

    /**
     * Get all children of a node
     */
    async getChildren(id: string): Promise<Node[]> {
        // Find the node
        const node = await this.getNode(id);
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
    async calculateNodeWeight(id: string): Promise<number> {
        const node = await this.getNode(id);
        if (!node) {
            return 0;
        }

        // Root node always has weight 1.0
        if (node.isRoot) {
            return 1.0;
        }

        // Find the parent
        const parentNode = await this.findParentNode(id);
        if (!parentNode) {
            return 1.0; // Default to full weight if parent isn't found
        }

        // Calculate total points of all siblings
        const siblings = await this.getChildren(parentNode.id);
        const totalPoints = siblings.reduce(
            (sum, sibling) => sum + sibling.points,
            0
        );

        // Calculate this node's contribution to parent
        const nodeContribution =
            totalPoints === 0 ? 0 : node.points / totalPoints;

        // Weight is recursive - multiply by parent's weight
        const parentWeight = await this.calculateNodeWeight(parentNode.id);
        return nodeContribution * parentWeight;
    }

    /**
     * Find the parent node of a given node
     */
    async findParentNode(id: string): Promise<Node | null> {
        try {
            // Search directly for nodes that have this id as a child
            const results = await this.nodes.index.search(
                new SearchRequest({
                    query: [
                        new StringMatch({
                            key: "childrenIds",
                            value: id,
                            method: StringMatchMethod.exact,
                        }),
                    ],
                })
            );
            return results.length > 0 ? results[0] : null;
        } catch (error) {
            console.error(`Error finding parent node for ${id}:`, error);
            return null;
        }
    }

    /**
     * Find all descendants of a node (recursive)
     */
    async getAllDescendants(id: string): Promise<Node[]> {
        const node = await this.getNode(id);
        if (!node) {
            return [];
        }

        // Get all descendants in one query using recursive search
        const results = await this.nodes.index.search(
            new SearchRequest({
                query: [
                    new StringMatch({
                        key: "parentId",
                        value: id,
                        method: StringMatchMethod.exact,
                    }),
                ],
                fetch: 0xffffffff, // Fetch all results
            })
        );

        return results;
    }

    /**
     * Calculate total points from all children
     */
    async totalChildPoints(id: string): Promise<number> {
        const children = await this.getChildren(id);
        return children.reduce((sum, child) => sum + child.points, 0);
    }

    /**
     * Calculate a node's share of its parent
     */
    async shareOfParent(id: string): Promise<number> {
        const node = await this.getNode(id);
        if (!node || node.isRoot) {
            return 1.0; // Root node has 100% share
        }

        const parentNode = await this.findParentNode(id);
        if (!parentNode) {
            return 1.0; // If parent not found, return full share
        }

        const parentTotal = await this.totalChildPoints(parentNode.id);
        return parentTotal === 0 ? 0 : node.points / parentTotal;
    }

    /**
     * Check if a node is a contribution node (has contributors and is not root)
     */
    async isContribution(id: string): Promise<boolean> {
        const node = await this.getNode(id);
        if (!node || node.isRoot) {
            return false; // Root nodes are never contribution nodes
        }

        return node.contributors.length > 0;
    }

    /**
     * Check if a node has direct contribution children
     */
    async hasDirectContributionChild(id: string): Promise<boolean> {
        const children = await this.getChildren(id);

        for (const child of children) {
            if (await this.isContribution(child.id)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Check if a node has non-contribution children
     */
    async hasNonContributionChild(id: string): Promise<boolean> {
        const children = await this.getChildren(id);

        for (const child of children) {
            if (!(await this.isContribution(child.id))) {
                return true;
            }
        }

        return children.length === 0 ? true : false;
    }

    /**
     * Calculate the proportion of total child points from contribution children
     */
    async contributionChildrenWeight(id: string): Promise<number> {
        const children = await this.getChildren(id);

        // Identify contribution and non-contribution children
        const contributionChildren: Node[] = [];

        for (const child of children) {
            if (await this.isContribution(child.id)) {
                contributionChildren.push(child);
            }
        }

        // Calculate weights for each category
        const contributionWeights: number[] = [];
        const totalWeights: number[] = [];

        for (const child of contributionChildren) {
            contributionWeights.push(await this.calculateNodeWeight(child.id));
        }

        for (const child of children) {
            totalWeights.push(await this.calculateNodeWeight(child.id));
        }

        // Sum the weights
        const contributionWeightSum = contributionWeights.reduce(
            (sum, w) => sum + w,
            0
        );
        const totalWeightSum = totalWeights.reduce((sum, w) => sum + w, 0);

        // Handle division by zero
        return totalWeightSum === 0
            ? 0
            : contributionWeightSum / totalWeightSum;
    }

    /**
     * Sum fulfillment from children matching a predicate
     */
    async childrenFulfillment(
        id: string,
        predicate: (childId: string) => Promise<boolean>
    ): Promise<number> {
        const children = await this.getChildren(id);
        let sum = 0;

        for (const child of children) {
            if (await predicate(child.id)) {
                const childFulfillment = await this.fulfilled(child.id);
                const childShare = await this.shareOfParent(child.id);
                sum += childFulfillment * childShare;
            }
        }

        return sum;
    }

    /**
     * Calculate the fulfillment from contribution children
     */
    async contributionChildrenFulfillment(id: string): Promise<number> {
        return this.childrenFulfillment(id, (childId) =>
            this.isContribution(childId)
        );
    }

    /**
     * Calculate the fulfillment from non-contribution children
     */
    async nonContributionChildrenFulfillment(id: string): Promise<number> {
        const children = await this.getChildren(id);
        const nonContribChildren: Node[] = [];

        for (const child of children) {
            if (!(await this.isContribution(child.id))) {
                nonContribChildren.push(child);
            }
        }

        if (nonContribChildren.length === 0) {
            return 0;
        }

        // Get weight and fulfillment for each child
        const childWeights: number[] = [];
        const childFulfillments: number[] = [];

        for (const child of nonContribChildren) {
            childWeights.push(await this.calculateNodeWeight(child.id));
            childFulfillments.push(await this.fulfilled(child.id));
        }

        // Calculate weighted sum and total weight
        let weightedSum = 0;
        for (let i = 0; i < childWeights.length; i++) {
            weightedSum += childWeights[i] * childFulfillments[i];
        }

        const totalWeight = childWeights.reduce((sum, w) => sum + w, 0);

        // Return weighted average
        return totalWeight === 0 ? 0 : weightedSum / totalWeight;
    }

    /**
     * Calculate fulfillment with caching
     */
    async fulfilled(id: string): Promise<number> {
        const node = await this.getNode(id);
        if (!node) {
            return 0;
        }

        // Helper predicates and values
        const hasManualFulfillment = node.manualFulfillment !== undefined;
        const isLeafNode = node.childrenIds.length === 0;
        const isContribNode = await this.isContribution(id);
        const hasContribChildren = await this.hasDirectContributionChild(id);
        const hasNonContribChildren = await this.hasNonContributionChild(id);

        // Safely extract manual fulfillment value with a default
        const getManualValue = node.manualFulfillment ?? 0.0;

        // Leaf contribution node
        if (isLeafNode && isContribNode) {
            return 1.0;
        }

        // Leaf non-contribution node
        if (isLeafNode) {
            return 0.0;
        }

        // Non-leaf node with manual fulfillment for contribution children only
        if (
            hasManualFulfillment &&
            hasContribChildren &&
            !hasNonContribChildren
        ) {
            return getManualValue;
        }

        // Non-leaf node with mixed children types and manual fulfillment
        if (hasManualFulfillment && hasContribChildren) {
            const contribWeight = await this.contributionChildrenWeight(id);
            const nonContribFulfillment =
                await this.nonContributionChildrenFulfillment(id);
            return (
                getManualValue * contribWeight +
                nonContribFulfillment * (1.0 - contribWeight)
            );
        }

        // Other nodes (aggregate from children)
        const children = await this.getChildren(id);
        let sum = 0;

        for (const child of children) {
            const childFulfillment = await this.fulfilled(child.id);
            const childShare = await this.shareOfParent(child.id);
            sum += childFulfillment * childShare;
        }

        return sum;
    }

    /**
     * Calculate the desire (unfulfilled need) of a node
     */
    async desire(id: string): Promise<number> {
        return 1.0 - (await this.fulfilled(id));
    }

    /**
     * Calculate a node's share of general fulfillment to another node
     */
    async shareOfGeneralFulfillment(
        contributorId: string,
        targetId: string,
        contributorIndexMap: Map<string, string>
    ): Promise<number> {
        // Get contributor root node
        const contributor = contributorIndexMap.get(contributorId);
        if (!contributor) return 0;

        // Get the target node and all its descendants
        const target = await this.getNode(targetId);
        if (!target) return 0;

        // Get all descendants (plus the target itself)
        const targetDescendants = [
            target,
            ...(await this.getAllDescendants(targetId)),
        ];

        // Find only nodes where this contributor is listed
        const contributingNodes: Node[] = [];

        for (const node of targetDescendants) {
            if (
                !node.isRoot &&
                node.contributors.includes(contributorId) &&
                (await this.isContribution(node.id))
            ) {
                contributingNodes.push(node);
            }
        }

        // Calculate weighted contribution for each node
        let weightedContributions = 0;

        for (const node of contributingNodes) {
            const nodeWeight = await this.calculateNodeWeight(node.id);
            const nodeFulfillment = await this.fulfilled(node.id);
            const contributorCount = node.contributors.length;

            weightedContributions +=
                (nodeWeight * nodeFulfillment) / contributorCount;
        }

        return weightedContributions;
    }

    /**
     * Get normalized shares of general fulfillment map
     */
    async sharesOfGeneralFulfillmentMap(
        contributorIndexMap: Map<string, string>
    ): Promise<Map<string, number>> {
        // Use cached share map if available
        if (this.nodeSOGFMap) {
            return this.nodeSOGFMap;
        }

        // Calculate shares for all contributors
        const contributorShares: [string, number][] = [];

        for (const [id, _] of contributorIndexMap.entries()) {
            const share = await this.shareOfGeneralFulfillment(
                id,
                this.rootNodeId,
                contributorIndexMap
            );

            if (share > 0) {
                contributorShares.push([id, share]);
            }
        }

        // Create and normalize the raw share map
        const rawShareMap = new Map(contributorShares);
        const normalizedMap = this.normalizeShareMap(rawShareMap);

        // Cache the result
        this.nodeSOGFMap = normalizedMap;

        return normalizedMap;
    }

    /**
     * Calculate and update provider-centric shares
     */
    async providerShares(
        contributorIndexMap: Map<string, string>,
        depth: number
    ): Promise<Map<string, number>> {
        // Use stored map if available
        const cachedMap = this.nodeProviderSharesMap.get(depth);
        if (cachedMap) {
            return cachedMap;
        }

        // Calculate fresh map
        let freshMap: Map<string, number>;

        if (depth === 1) {
            // Direct contributor shares based on SOGF
            freshMap = await this.sharesOfGeneralFulfillmentMap(
                contributorIndexMap
            );
        } else {
            // Transitive shares for depth > 1
            freshMap = await this.transitiveShares(contributorIndexMap, depth);
        }

        // Normalize and cache the result
        const normalizedMap = this.normalizeShareMap(freshMap);
        this.nodeProviderSharesMap.set(depth, normalizedMap);

        return normalizedMap;
    }

    /**
     * Calculate transitive shares for a given depth
     */
    private async transitiveShares(
        contributorIndexMap: Map<string, string>,
        depth: number
    ): Promise<Map<string, number>> {
        let currentShares = await this.providerShares(contributorIndexMap, 1);

        // Iteratively combine transitive shares
        for (let i = 2; i <= depth; i++) {
            currentShares = await this.combineTransitiveShares(
                currentShares,
                contributorIndexMap
            );
        }

        return currentShares;
    }

    /**
     * Combine shares transitively across the network
     */
    private async combineTransitiveShares(
        currentShares: Map<string, number>,
        contributorIndexMap: Map<string, string>
    ): Promise<Map<string, number>> {
        const resultMap = new Map<string, number>();

        for (const [recipientId, share] of currentShares.entries()) {
            // Get the tree program for this recipient
            const recipientTreeId = contributorIndexMap.get(recipientId);
            if (!recipientTreeId) continue;

            // Get recipient's direct shares
            const recipientTree = await this.getNode(recipientTreeId);
            if (!recipientTree) continue;

            // Get recipient's direct shares using the SOGF map
            const recipientDirectShares =
                await this.sharesOfGeneralFulfillmentMap(contributorIndexMap);

            for (const [
                subRecipientId,
                subShare,
            ] of recipientDirectShares.entries()) {
                const weightedShare = share * subShare;
                const currentShare = resultMap.get(subRecipientId) ?? 0;
                resultMap.set(subRecipientId, currentShare + weightedShare);
            }
        }

        return resultMap;
    }

    /**
     * Get direct share between provider and recipient
     */
    async directShare(
        providerId: string,
        recipientId: string,
        contributorIndexMap: Map<string, string>
    ): Promise<number> {
        const shares = await this.providerShares(contributorIndexMap, 1);
        return shares.get(recipientId) ?? 0;
    }

    /**
     * Get a receiver's share from a specific capacity provider
     */
    async receiverShareFrom(
        receiverId: string,
        providerId: string,
        capacityId: string,
        maxDepth: number,
        contributorIndexMap: Map<string, string>
    ): Promise<number> {
        const providerShares = await this.providerShares(
            contributorIndexMap,
            maxDepth
        );
        return providerShares.get(receiverId) ?? 0;
    }

    /**
     * Calculate personal capacity share
     */
    async getPersonalCapacityShare(
        personId: string,
        capacityId: string,
        contributorIndexMap: Map<string, string>
    ): Promise<number> {
        // Find all owners of this capacity
        const capacityOwners: string[] = [];

        for (const [id, treeId] of contributorIndexMap.entries()) {
            const node = await this.getNode(treeId);
            if (!node || !node.isRoot) continue;

            if (node.capacityIds.includes(capacityId)) {
                capacityOwners.push(id);
            }
        }

        // Calculate direct shares from each owner
        const directShares: number[] = [];

        for (const ownerId of capacityOwners) {
            const share = await this.receiverShareFrom(
                personId,
                ownerId,
                capacityId,
                2,
                contributorIndexMap
            );
            directShares.push(share);
        }

        // Return the maximum share (or 0 if no shares)
        return directShares.length === 0 ? 0 : Math.max(...directShares);
    }

    /**
     * Generic function to normalize a map so that its values sum to 1
     */
    normalizeMap<K>(map: Map<K, number>): Map<K, number> {
        const total = Array.from(map.values()).reduce((sum, v) => sum + v, 0);

        if (total === 0) return map;

        const normalizedMap = new Map<K, number>();
        for (const [key, value] of map.entries()) {
            normalizedMap.set(key, value / total);
        }

        return normalizedMap;
    }

    /**
     * Type-specific version of normalizeMap for ShareMap
     */
    normalizeShareMap(map: Map<string, number>): Map<string, number> {
        return this.normalizeMap(map);
    }

    /**
     * Compute quantity share based on capacity and percentage
     */
    computeQuantityShare(capacity: Capacity, percentage: number): number {
        const rawQuantity = Math.round(capacity.quantity * percentage);

        // Apply percentage divisibility constraint
        const percentConstrained =
            percentage > capacity.percentageDiv
                ? Math.round(capacity.quantity * capacity.percentageDiv)
                : rawQuantity;

        // Apply natural number divisibility constraint
        const naturalConstrained =
            Math.floor(percentConstrained / capacity.naturalDiv) *
            capacity.naturalDiv;

        return naturalConstrained;
    }
}

export const createClient = async (localNetwork = false) => {
    const client = await Peerbit.create({
        //  directory:   "./test", for persistance
        libp2p: {
            addresses: {
                // TMP disable because flaky behaviour with libp2p 1.8.1
                // re-enable when https://github.com/dao-xyz/peerbit/issues/302 closed
                listen: [
                    /* "/webrtc" */
                ],
            },
            connectionGater: localNetwork
                ? {
                      denyDialMultiaddr: () => {
                          // by default libp2p refuse to dial local addresses from the browser since they
                          // are usually sent by remote peers broadcasting undialable multiaddrs but
                          // here we are explicitly connecting to a local node so do not deny dialing
                          // any discovered address
                          return false;
                      },
                  }
                : undefined,
            transports: [
                webSockets({
                    filter: filters.all,
                }),
                circuitRelayTransport({
                    discoverRelays: 1,
                }),
                // TMP disable because flaky behaviour with libp2p 1.8.1
                // re-enable when https://github.com/dao-xyz/peerbit/issues/302 closed
                /*    webRTC(), */
            ],
        },
    });

    // for online apps
    if (localNetwork) {
        // will dial a local server
        // see https://peerbit.org/#/modules/deploy/server/?id=testing-locally
        // to get more info how to launch one
        await client.dial(
            "/ip4/127.0.0.1/tcp/8002/ws/p2p/" +
                (await (await fetch("http://localhost:8082/peer/id")).text())
        );
    } else {
        // will dial public relay servers
        await client.bootstrap();
    }

    return client;
};

export { Tree, Node, Capacity };
