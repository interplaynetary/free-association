import { GunNode } from '../../../gun/GunNode';
import { GunSubscription } from '../../../gun/GunSubscription';
import {
	createGunStore,
	createCollectionStore,
	deriveFromGun,
	filterCollectionStore,
	aggregateCollection,
	mapStore,
	combineStores,
	asyncDerivedStore
} from '../../../utils/svelte/reactiveStores';
import { derived, writable, get, type Readable } from 'svelte/store';
import { type Proportion, DistributionMap, asProportion } from '../../utils/proportions';

// Types
export interface RecognitionData {
	name?: string;
	points?: number;
	parent?: any; // Gun reference
	manualFulfillment?: number;
}

export interface ContributorData extends RecognitionData {
	// Additional contributor-specific properties
}

// Base interface for all reactive recognition stores
export interface BaseRecognitionStores<T> {
	// Base data stores
	dataStore: Readable<T>;
	nameStore: Readable<string>;
	pointsStore: Readable<number>;

	// Methods to update data
	updateName: (name: string) => Promise<void>;
	updatePoints: (points: number) => Promise<void>;

	// Access to underlying models
	node: GunNode<T>;
	path: string[];
}

// Interface for recognition stores
export interface RecognitionStores extends BaseRecognitionStores<RecognitionData> {
	// Collection stores
	contributorsStore: Readable<[string, any][]>;
	childrenStore: Readable<[string, any][]>;

	// Derived data stores
	parentStore: Readable<any | null>;
	isContributionStore: Readable<boolean>;
	isContributorStore: Readable<boolean>;
	totalChildPointsStore: Readable<number>;
	shareOfParentStore: Readable<number>;
	shareOfRootStore: Readable<number>;
	hasDirectContributionChildStore: Readable<boolean>;
	hasNonContributionChildStore: Readable<boolean>;
	contributionChildrenWeightStore: Readable<number>;
	contributionChildrenFulfillmentStore: Readable<number>;
	nonContributionChildrenFulfillmentStore: Readable<number>;
	fulfilledStore: Readable<number>;
	desireStore: Readable<number>;

	// Methods to modify related nodes
	addChild: (
		name: string,
		points?: number,
		contributors?: string[],
		manualFulfillment?: number
	) => Promise<string>;
	removeChild: (childId: string) => Promise<void>;
	addContributor: (contributorId: string) => Promise<void>;
	removeContributor: (contributorId: string) => Promise<void>;
}

// Interface for contributor stores
export interface ContributorStores extends RecognitionStores {
	// Additional contributor-specific stores
	sharesOfGeneralFulfillmentStore: Readable<Map<string, number>>;
	socialDistributionsStore: Readable<Map<number, DistributionMap<string>>>;

	// Methods for contributors
	updateSharesOfGeneralFulfillment: () => Promise<Map<string, number>>;
	calculateSocialDistribution: (depth?: number) => Promise<DistributionMap<string>>;
	mutualRecognition: (otherContributorId: string) => Promise<Proportion>;
}

/**
 * Helper function to get child node property as a GunStore
 * This replaces direct GunSubscription usage with GunStore for reactivity
 */
function getChildPropertyStore<T>(
	parentPath: string[],
	childId: string,
	property: string,
	defaultValue?: T
): Readable<T> {
	const childPath = [...parentPath, 'children', childId, property];
	return createGunStore<T>(childPath, defaultValue as any);
}

/**
 * Create reactive stores for a recognition node
 * @param path Path to the recognition node
 * @returns Object containing reactive stores and methods
 */
export function createRecognitionStores(path: string[]): RecognitionStores {
	// Create the base node
	const node = new GunNode<RecognitionData>(path);

	// Basic data stores
	const dataStore = createGunStore<RecognitionData>(path);
	const nameStore = createGunStore<string>([...path, 'name'], '');
	const pointsStore = createGunStore<number>([...path, 'points'], 0);
	const parentStore = createGunStore<any | null>([...path, 'parent'], null);
	const manualFulfillmentStore = createGunStore<number>([...path, 'manualFulfillment'], 0);

	// Extract values from Gun objects
	const nameValueStore = derived(nameStore, ($name) => {
		if ($name && typeof $name === 'object' && 'value' in ($name as any)) {
			return ($name as any).value || '';
		}
		return typeof $name === 'string' ? $name : '';
	});

	const pointsValueStore = derived(pointsStore, ($points) => {
		if ($points && typeof $points === 'object' && 'value' in ($points as any)) {
			return Number(($points as any).value) || 0;
		}
		return typeof $points === 'number' ? $points : 0;
	});

	// Collection stores
	const contributorsStore = createCollectionStore<any>([...path, 'contributors']);
	const childrenStore = createCollectionStore<any>([...path, 'children']);

	// Derived stores
	const isContributorStore = derived(parentStore, ($parent) => $parent === null);

	const isContributionStore = derived(
		[parentStore, contributorsStore],
		([$parent, $contributors]) => Boolean($parent && $contributors.length > 0)
	);

	const totalChildPointsStore = aggregateCollection(childrenStore, (children) =>
		children.reduce((sum, [_, child]) => sum + (Number(child.points) || 0), 0)
	);

	const shareOfParentStore = deriveFromGun(
		[pointsStore, totalChildPointsStore, parentStore],
		([points, totalChildPoints, parent]) => {
			if (!parent) return 1; // This is root
			return totalChildPoints === 0 ? 0 : points / totalChildPoints;
		}
	);

	// Create a shareOfRoot store that updates when relevant values change
	const shareOfRootStore = asyncDerivedStore(
		[shareOfParentStore, parentStore],
		async (values) => {
			const shareOfParent = values[0] as number;
			const parent = values[1] as any;

			if (!parent) return 1;

			const parentPath = Array.isArray(parent) ? parent : [];
			if (parentPath.length === 0) return shareOfParent;

			try {
				// Use createGunStore instead of direct GunSubscription
				const parentShareStore = createGunStore<number>(parentPath, 1);
				const parentShareOfRoot = get(parentShareStore);

				return shareOfParent * parentShareOfRoot;
			} catch (err) {
				console.error('Error getting parent share of root:', err);
				return shareOfParent;
			}
		},
		1
	);

	// Async derived stores using the improved pattern
	const hasDirectContributionChildStore = asyncDerivedStore(
		[childrenStore],
		async (values) => {
			const children = values[0] as [string, any][];
			for (const [childId, _] of children) {
				// Create a store for this child's isContribution property
				const isContributionStore = getChildPropertyStore<boolean>(
					path,
					childId,
					'isContribution',
					false
				);
				const isContribution = get(isContributionStore);

				if (isContribution) {
					return true;
				}
			}
			return false;
		},
		false
	);

	const hasNonContributionChildStore = asyncDerivedStore(
		[childrenStore],
		async (values) => {
			const children = values[0] as [string, any][];
			for (const [childId, _] of children) {
				// Create a store for this child's isContribution property
				const isContributionStore = getChildPropertyStore<boolean>(
					path,
					childId,
					'isContribution',
					false
				);
				const isContribution = get(isContributionStore);

				if (!isContribution) {
					return true;
				}
			}
			return false;
		},
		false
	);

	const contributionChildrenWeightStore = asyncDerivedStore(
		[childrenStore, totalChildPointsStore],
		async (values) => {
			const children = values[0] as [string, any][];
			const totalChildPoints = values[1] as number;

			let contributionPoints = 0;

			for (const [childId, child] of children) {
				const points = Number(child.points) || 0;

				// Create a store for this child's isContribution property
				const isContributionStore = getChildPropertyStore<boolean>(
					path,
					childId,
					'isContribution',
					false
				);
				const isContribution = get(isContributionStore);

				if (isContribution) {
					contributionPoints += points;
				}
			}

			return totalChildPoints === 0 ? 0 : contributionPoints / totalChildPoints;
		},
		0
	);

	const contributionChildrenFulfillmentStore = asyncDerivedStore(
		[childrenStore, totalChildPointsStore],
		async (values) => {
			const children = values[0] as [string, any][];
			const totalChildPoints = values[1] as number;

			let fulfillmentSum = 0;

			for (const [childId, child] of children) {
				// Create stores for required properties
				const isContributionStore = getChildPropertyStore<boolean>(
					path,
					childId,
					'isContribution',
					false
				);
				const isContribution = get(isContributionStore);

				if (isContribution) {
					// Get this child's fulfilled value
					const fulfilledStore = getChildPropertyStore<number>(path, childId, 'fulfilled', 0);
					const fulfilled = get(fulfilledStore);

					// Calculate share of parent
					const points = Number(child.points) || 0;
					const shareOfParent = totalChildPoints === 0 ? 0 : points / totalChildPoints;

					fulfillmentSum += fulfilled * shareOfParent;
				}
			}

			return fulfillmentSum;
		},
		0
	);

	const nonContributionChildrenFulfillmentStore = asyncDerivedStore(
		[childrenStore, totalChildPointsStore],
		async (values) => {
			const children = values[0] as [string, any][];
			const totalChildPoints = values[1] as number;

			let fulfillmentSum = 0;

			for (const [childId, child] of children) {
				// Create stores for required properties
				const isContributionStore = getChildPropertyStore<boolean>(
					path,
					childId,
					'isContribution',
					false
				);
				const isContribution = get(isContributionStore);

				if (!isContribution) {
					// Get this child's fulfilled value
					const fulfilledStore = getChildPropertyStore<number>(path, childId, 'fulfilled', 0);
					const fulfilled = get(fulfilledStore);

					// Calculate share of parent
					const points = Number(child.points) || 0;
					const shareOfParent = totalChildPoints === 0 ? 0 : points / totalChildPoints;

					fulfillmentSum += fulfilled * shareOfParent;
				}
			}

			return fulfillmentSum;
		},
		0
	);

	// The core fulfillment calculation - refactored to use proper async reactivity
	const fulfilledStore = asyncDerivedStore(
		[
			isContributionStore,
			childrenStore,
			manualFulfillmentStore,
			hasDirectContributionChildStore,
			hasNonContributionChildStore,
			contributionChildrenWeightStore,
			nonContributionChildrenFulfillmentStore,
			totalChildPointsStore
		],
		async (values) => {
			const isContribution = values[0] as boolean;
			const children = values[1] as [string, any][];
			const manualFulfillment = values[2] as number;
			const hasDirectContributionChild = values[3] as boolean;
			const hasNonContributionChild = values[4] as boolean;
			const contributionChildrenWeight = values[5] as number;
			const nonContributionChildrenFulfillment = values[6] as number;
			const totalChildPoints = values[7] as number;

			// Case 1: Is a contribution node (always 100% fulfilled)
			if (isContribution) {
				return 1.0;
			}

			// Case 2: No children, so zero fulfillment
			if (children.length === 0) {
				return 0.0;
			}

			// Case 3: Has manual fulfillment set AND has direct contribution children
			if (typeof manualFulfillment === 'number' && hasDirectContributionChild) {
				// Case 3a: Only has contribution children
				if (!hasNonContributionChild) {
					return manualFulfillment;
				}

				// Case 3b: Hybrid case - combine manual fulfillment for contribution children
				// with calculated fulfillment for non-contribution children
				return (
					manualFulfillment * contributionChildrenWeight +
					nonContributionChildrenFulfillment * (1 - contributionChildrenWeight)
				);
			}

			// Case 4: Default case - calculate from all children
			let childrenFulfillment = 0;

			for (const [childId, child] of children) {
				// Get this child's fulfilled value
				const fulfilledStore = getChildPropertyStore<number>(path, childId, 'fulfilled', 0);
				const fulfilled = get(fulfilledStore);

				// Calculate share of parent
				const points = Number(child.points) || 0;
				const shareOfParent = totalChildPoints === 0 ? 0 : points / totalChildPoints;

				childrenFulfillment += fulfilled * shareOfParent;
			}

			return childrenFulfillment;
		},
		0
	);

	// Desire is inverse of fulfillment
	const desireStore = derived(fulfilledStore, ($fulfilled) =>
		typeof $fulfilled === 'number' ? 1 - $fulfilled : 1
	);

	// Methods to modify data
	const updateName = async (name: string): Promise<void> => {
		await node.get('name').put(name as unknown as Partial<any>);
	};

	const updatePoints = async (points: number): Promise<void> => {
		await node.get('points').put(points as unknown as Partial<any>);
	};

	const addChild = async (
		name: string,
		points: number = 0,
		contributors: string[] = [],
		manualFulfillment?: number
	): Promise<string> => {
		// Generate a unique ID for the child node
		const childId = Date.now().toString();

		// Create the child node
		const childNode = node.get('children').get(childId);

		// Set up the basic properties
		await childNode.put({
			name,
			points,
			parent: node.getChain(),
			...(manualFulfillment !== undefined ? { manualFulfillment } : {})
		});

		// Add contributors if provided
		for (const contributorId of contributors) {
			await childNode
				.get('contributors')
				.get(contributorId)
				.put(true as unknown as Partial<any>);
		}

		return childId;
	};

	const removeChild = async (childId: string): Promise<void> => {
		await node
			.get('children')
			.get(childId)
			.put(null as any);
	};

	const addContributor = async (contributorId: string): Promise<void> => {
		await node.get('contributors').get(contributorId).put({ value: true });
	};

	const removeContributor = async (contributorId: string): Promise<void> => {
		await node
			.get('contributors')
			.get(contributorId)
			.put(null as any);
	};

	return {
		// Basic data stores
		dataStore,
		nameStore: nameValueStore,
		pointsStore: pointsValueStore,
		parentStore,

		// Collection stores
		contributorsStore,
		childrenStore,

		// Derived stores
		isContributorStore,
		isContributionStore,
		totalChildPointsStore,
		shareOfParentStore,
		shareOfRootStore,
		hasDirectContributionChildStore,
		hasNonContributionChildStore,
		contributionChildrenWeightStore,
		contributionChildrenFulfillmentStore,
		nonContributionChildrenFulfillmentStore,
		fulfilledStore,
		desireStore,

		// Methods
		updateName,
		updatePoints,
		addChild,
		removeChild,
		addContributor,
		removeContributor,

		// Access to underlying models
		node,
		path
	};
}

/**
 * Create reactive stores for a contributor node
 * @param path Path to the contributor node
 * @returns Object containing contributor-specific reactive stores and methods
 */
export function createContributorStores(path: string[]): ContributorStores {
	// Start with base recognition stores
	const recognitionStores = createRecognitionStores(path);

	// Add contributor-specific stores
	const node = recognitionStores.node;

	// Store for shares of general fulfillment
	const sharesOfGeneralFulfillmentStore = createGunStore<Record<string, number>>(
		[...path, 'sharesOfGeneralFulfillment'],
		{}
	);

	// Convert record to Map for easier consumption
	const sharesMapStore = derived(sharesOfGeneralFulfillmentStore, ($shares) => {
		const sharesMap = new Map<string, number>();

		if ($shares) {
			Object.entries($shares).forEach(([key, value]) => {
				if (key !== '_') {
					// Skip Gun metadata
					sharesMap.set(key, Number(value) || 0);
				}
			});
		}

		return sharesMap;
	});

	// Store for social distributions
	const socialDistributionsRawStore = createGunStore<Record<string, Record<string, number>>>(
		[...path, 'socialDistributions'],
		{}
	);

	// Convert record to Map<number, DistributionMap> for easier consumption
	const socialDistributionsStore = derived(socialDistributionsRawStore, ($distributions) => {
		const distribMap = new Map<number, DistributionMap<string>>();

		if ($distributions) {
			Object.entries($distributions).forEach(([depthKey, depthValue]) => {
				if (depthKey !== '_') {
					// Skip Gun metadata
					const depth = Number(depthKey);
					const distMap = new DistributionMap<string>();

					Object.entries(depthValue).forEach(([contribKey, weight]) => {
						if (contribKey !== '_') {
							// Skip Gun metadata
							distMap.set(contribKey, Number(weight) || 0);
						}
					});

					distribMap.set(depth, distMap);
				}
			});
		}

		return distribMap;
	});

	// Method to update shares of general fulfillment
	const updateSharesOfGeneralFulfillment = async (): Promise<Map<string, number>> => {
		const updatedShares = new Map<string, number>();

		// In a real implementation, this would calculate shares based on recognitions
		// This is a simplified version

		// Create a store for the contributor index
		const contributorIndexStore = createGunStore<Record<string, any>>(
			[...path, 'contributorIndex'],
			{}
		);

		// Get the current value
		const contributorIndex = get(contributorIndexStore);

		if (contributorIndex) {
			for (const key in contributorIndex) {
				if (key !== '_') {
					// Skip Gun metadata
					// Calculate share for this contributor
					// This would normally involve complex calculations
					const share = Math.random(); // Placeholder
					updatedShares.set(key, share);
				}
			}

			// Save the updated shares
			const sharesObj: Record<string, number> = {};
			for (const [key, value] of updatedShares.entries()) {
				sharesObj[key] = value;
			}

			await node.get('sharesOfGeneralFulfillment').put(sharesObj);
		}

		return updatedShares;
	};

	// Method to calculate mutual recognition
	const mutualRecognition = async (otherContributorId: string): Promise<Proportion> => {
		// Get this contributor's shares
		const myShares = get(sharesMapStore);

		// Create a store for the other contributor's shares
		const otherContributorPath = [...path.slice(0, -1), otherContributorId];
		const otherSharesStore = createGunStore<Record<string, number>>(
			[...otherContributorPath, 'sharesOfGeneralFulfillment'],
			{}
		);

		// Convert to a map
		const otherSharesMapStore = derived(otherSharesStore, ($shares) => {
			const sharesMap = new Map<string, number>();

			if ($shares) {
				Object.entries($shares).forEach(([key, value]) => {
					if (key !== '_') {
						// Skip Gun metadata
						sharesMap.set(key, Number(value) || 0);
					}
				});
			}

			return sharesMap;
		});

		// Get the current value
		const otherShares = get(otherSharesMapStore);

		// Find this contributor in their shares
		const myId = path[path.length - 1];
		const myShareInTheirs = otherShares.get(myId) || 0;

		// Find them in my shares
		const theirShareInMine = myShares.get(otherContributorId) || 0;

		// Mutual recognition is the minimum of the two values
		const mutualRec = Math.min(myShareInTheirs, theirShareInMine);
		return asProportion(mutualRec);
	};

	// Method to calculate social distribution
	const calculateSocialDistribution = async (
		depth: number = 1
	): Promise<DistributionMap<string>> => {
		if (depth <= 0) {
			throw new Error('Depth must be a positive number');
		}

		const distribution = new DistributionMap<string>();

		// Get current social distributions
		const socialDistributions = get(socialDistributionsStore);

		if (depth === 1) {
			// For depth 1, use direct mutual recognition
			const contributorIndex = await node.get('contributorIndex').once();

			if (contributorIndex) {
				for (const key in contributorIndex) {
					if (key !== '_') {
						// Skip Gun metadata
						const mutualRec = await mutualRecognition(key);
						distribution.set(key, mutualRec);
					}
				}
			}
		} else {
			// For deeper levels, use the cumulative distribution approach
			const previousDistribution = socialDistributions.get(depth - 1);

			if (!previousDistribution) {
				// Calculate it if it doesn't exist
				await calculateSocialDistribution(depth - 1);
				return calculateSocialDistribution(depth); // Retry
			}

			// For each contributor in the previous distribution
			for (const [contributorId, weight] of previousDistribution.entries()) {
				// Get their mutual recognition distribution
				const contributorPath = [...path.slice(0, -1), contributorId];

				// Create a store for this contributor's distributions
				const contributorDistStore = createGunStore<Record<string, Record<string, number>>>(
					[...contributorPath, 'socialDistributions'],
					{}
				);

				// Get the current value
				const contributorDistributionsRaw = get(contributorDistStore);
				if (!contributorDistributionsRaw || !contributorDistributionsRaw['1']) continue;

				const contributorDistribution = new DistributionMap<string>();

				// Parse the raw distribution
				for (const [subKey, subValue] of Object.entries(contributorDistributionsRaw['1'])) {
					if (subKey !== '_') {
						// Skip Gun metadata
						contributorDistribution.set(subKey, Number(subValue) || 0);
					}
				}

				// Add weighted distribution
				for (const [subContributor, subWeight] of contributorDistribution.entries()) {
					const currentWeight = distribution.get(subContributor) || 0;
					distribution.set(subContributor, currentWeight + weight * subWeight);
				}
			}
		}

		// Save the distribution
		const newDistributions = new Map(socialDistributions);
		newDistributions.set(depth, distribution);

		// Convert to Gun-friendly object
		const distribObj: Record<string, Record<string, number>> = {};

		for (const [depthKey, distMap] of newDistributions.entries()) {
			distribObj[depthKey.toString()] = {};

			for (const [contribKey, weight] of distMap.entries()) {
				distribObj[depthKey.toString()][contribKey] = weight;
			}
		}

		// Save to Gun
		await node.get('socialDistributions').put(distribObj);

		return distribution;
	};

	return {
		...recognitionStores,

		// Additional contributor-specific stores
		sharesOfGeneralFulfillmentStore: sharesMapStore,
		socialDistributionsStore,

		// Additional methods
		updateSharesOfGeneralFulfillment,
		calculateSocialDistribution,
		mutualRecognition
	};
}

/**
 * Helper class for accessing stores within a specific app scope
 */
export class StoreAccess {
	private static instance: StoreAccess;
	private appScope: string;

	private constructor(appScope: string = 'free-association') {
		this.appScope = appScope;
	}

	/**
	 * Get the singleton instance
	 * @param appScope Optional app scope (namespace)
	 * @returns The StoreAccess instance
	 */
	public static getInstance(appScope?: string): StoreAccess {
		if (!StoreAccess.instance) {
			StoreAccess.instance = new StoreAccess(appScope);
		}
		return StoreAccess.instance;
	}

	/**
	 * Get recognition stores for a specific path
	 * @param path Path segments after the app scope
	 * @returns Object containing recognition stores
	 */
	public getRecognitionStores(...path: string[]): RecognitionStores {
		return createRecognitionStores([this.appScope, ...path]);
	}

	/**
	 * Get contributor stores for a specific path
	 * @param path Path segments after the app scope
	 * @returns Object containing contributor stores
	 */
	public getContributorStores(...path: string[]): ContributorStores {
		return createContributorStores([this.appScope, ...path]);
	}
}
