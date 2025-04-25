import { GunNode } from '../gun/GunNode';
import {
	createGunStore,
	createCollectionStore,
	type GunStore,
	type CollectionItem
} from '../svelte/reactiveStores';
import { derived, get, type Readable } from 'svelte/store';

/**
 * Generic interface for a node in a reactive graph
 */
export interface GraphNode {
	[key: string]: any;
}

/**
 * Options for traversal operations to prevent cycles
 */
export interface TraversalOptions {
	/** Maximum depth to traverse */
	maxDepth?: number;
	/** Whether to detect and prevent cycles */
	preventCycles?: boolean;
	/** Custom equality function for determining if two nodes are the same */
	nodeEquality?: (a: any, b: any) => boolean;
	/** Callback function when a cycle is detected */
	onCycleDetected?: (path: string[], cycleNode: any) => void;
}

/**
 * Default traversal options
 */
export const DEFAULT_TRAVERSAL_OPTIONS: TraversalOptions = {
	maxDepth: 10,
	preventCycles: true,
	nodeEquality: (a, b) => a === b,
	onCycleDetected: (path, cycleNode) => {
		console.warn(`Cycle detected at path ${path.join('/')}`, cycleNode);
	}
};

/**
 * Class for tracking visited nodes during traversal
 */
export class VisitTracker {
	private visited = new Set<string>();
	private nodeToPathMap = new Map<any, string[]>();
	private options: TraversalOptions;

	constructor(options: TraversalOptions = DEFAULT_TRAVERSAL_OPTIONS) {
		this.options = { ...DEFAULT_TRAVERSAL_OPTIONS, ...options };
	}

	/**
	 * Check if a node has been visited
	 */
	public hasVisited(path: string[], node: any): boolean {
		if (!this.options.preventCycles) return false;

		const pathKey = path.join('/');

		// Simple path-based check
		if (this.visited.has(pathKey)) return true;

		// Node identity check if node equality is defined
		if (node && this.options.nodeEquality) {
			for (const [visitedNode, visitedPath] of this.nodeToPathMap.entries()) {
				if (this.options.nodeEquality(node, visitedNode)) {
					if (this.options.onCycleDetected) {
						this.options.onCycleDetected(path, node);
					}
					return true;
				}
			}
		}

		return false;
	}

	/**
	 * Mark a node as visited
	 */
	public markVisited(path: string[], node: any): void {
		if (!this.options.preventCycles) return;

		const pathKey = path.join('/');
		this.visited.add(pathKey);

		if (node) {
			this.nodeToPathMap.set(node, [...path]);
		}
	}

	/**
	 * Clear the visit tracker
	 */
	public clear(): void {
		this.visited.clear();
		this.nodeToPathMap.clear();
	}
}

/**
 * Generic registry for caching stores to prevent duplicate creation
 */
export class StoreRegistry {
	private static instance: StoreRegistry;
	private stores = new Map<string, any>();
	private collectionStores = new Map<string, any>();
	private derivedStores = new Map<string, Readable<any>>();
	private derivationGraph = new Map<string, Set<string>>();

	private constructor() {}

	public static getInstance(): StoreRegistry {
		if (!StoreRegistry.instance) {
			StoreRegistry.instance = new StoreRegistry();
		}
		return StoreRegistry.instance;
	}

	public getStore<T>(path: string[], defaultValue?: T, transient: boolean = false): GunStore<T> {
		const key = path.join('/');
		if (!this.stores.has(key)) {
			this.stores.set(key, createGunStore<T>(path, defaultValue, transient));
		}
		return this.stores.get(key) as GunStore<T>;
	}

	public getCollectionStore<T extends CollectionItem>(
		path: string[],
		sortFn?: (a: [string, T], b: [string, T]) => number,
		transient: boolean = false
	): Readable<[string, T][]> {
		const key = path.join('/');
		if (!this.collectionStores.has(key)) {
			const store = createCollectionStore<T>(path, sortFn as any, transient);
			this.collectionStores.set(key, store);
		}
		return this.collectionStores.get(key) as Readable<[string, T][]>;
	}

	/**
	 * Check if adding a dependency would create a cycle in derived stores
	 * @returns true if cycle would be created
	 */
	private wouldCreateDerivationCycle(targetKey: string, dependencies: string[]): boolean {
		// For each dependency, check if target is already in its dependency chain
		const visited = new Set<string>();
		const stack: string[] = [...dependencies];

		while (stack.length > 0) {
			const current = stack.pop()!;

			if (current === targetKey) {
				return true; // Would create a cycle
			}

			if (!visited.has(current)) {
				visited.add(current);
				const deps = this.derivationGraph.get(current);
				if (deps) {
					stack.push(...deps);
				}
			}
		}

		return false;
	}

	/**
	 * Register a derived store with cycle detection
	 */
	public registerDerivedStore<T>(
		key: string,
		store: Readable<T>,
		dependencies: string[] = [],
		transient: boolean = false
	): Readable<T> {
		// Check if this would create a cycle
		if (this.wouldCreateDerivationCycle(key, dependencies)) {
			console.error(`Derivation cycle detected for ${key}. Using non-derived value.`);
			// Return a non-derived store instead of creating a cycle
			return {
				subscribe: (fn) => {
					fn(null as any);
					return () => {};
				}
			};
		}

		// Update the derivation graph
		this.derivationGraph.set(key, new Set(dependencies));

		// Register the store
		this.derivedStores.set(key, store);
		return store;
	}

	public getDerivedStore<T>(key: string): Readable<T> | undefined {
		return this.derivedStores.get(key) as Readable<T> | undefined;
	}

	public clear(): void {
		this.stores.clear();
		this.collectionStores.clear();
		this.derivedStores.clear();
		this.derivationGraph.clear();
	}
}

/**
 * Error handling utility for async operations
 */
export async function withErrorHandling<T>(
	operation: () => Promise<T>,
	fallback: T,
	errorMessage: string
): Promise<T> {
	try {
		return await operation();
	} catch (err) {
		console.error(errorMessage, err);
		return fallback;
	}
}

/**
 * Utility for safely getting store values
 */
export function safeGet<T>(store: Readable<T>, defaultValue: T): T {
	try {
		return get(store);
	} catch (err) {
		console.error('Error getting store value:', err);
		return defaultValue;
	}
}

/**
 * Type for a function that limits traversal depth and prevents cycles
 */
export type TraversalGuard = (currentDepth: number, path: string[], node: any) => boolean;

/**
 * Generic reactive graph for working with hierarchical data
 * This is a more generic version of the graph implementation used in the recognition system.
 * It includes built-in cycle detection and prevention.
 */
export class ReactiveGraph {
	private registry: StoreRegistry;
	private defaultTraversalOptions: TraversalOptions;
	private traversalCache = new Map<string, Promise<string[]>>();
	private debugMode = true;
	private isTransient: boolean;

	constructor(options: TraversalOptions = DEFAULT_TRAVERSAL_OPTIONS, transient: boolean = false) {
		this.registry = StoreRegistry.getInstance();
		this.defaultTraversalOptions = { ...DEFAULT_TRAVERSAL_OPTIONS, ...options };
		this.isTransient = transient;
	}

	/**
	 * Enable or disable debug mode
	 * @param enabled Whether debug mode should be enabled
	 * @returns This instance for chaining
	 */
	public setDebugMode(enabled: boolean): this {
		this.debugMode = enabled;
		return this;
	}

	/**
	 * Set whether this graph should use transient storage
	 * @param transient Whether to use transient storage
	 * @returns This instance for chaining
	 */
	public setTransient(transient: boolean): this {
		this.isTransient = transient;
		return this;
	}

	/**
	 * Check if this graph is using transient storage
	 */
	public isUsingTransient(): boolean {
		return this.isTransient;
	}

	/**
	 * Log debug information if debug mode is enabled
	 * @param message Debug message
	 * @param data Optional data to log
	 */
	private logDebug(message: string, data?: any): void {
		if (this.debugMode) {
			console.log(`[ReactiveGraph] ${message}`, data);
		}
	}

	/**
	 * Get a reactive store for a node at the specified path
	 */
	public getNodeStore<T>(path: string[], defaultValue?: T): GunStore<T> {
		this.logDebug(`Getting node store for path: ${path.join('/')}`, {
			defaultValue,
			transient: this.isTransient
		});
		return this.registry.getStore<T>(path, defaultValue, this.isTransient);
	}

	/**
	 * Get a reactive store for a collection at the specified path
	 */
	public getCollectionStore<T extends CollectionItem>(
		path: string[],
		sortFn?: (a: [string, T], b: [string, T]) => number
	): Readable<[string, T][]> {
		this.logDebug(`Getting collection store for path: ${path.join('/')}`, {
			hasSortFn: !!sortFn,
			transient: this.isTransient
		});
		return this.registry.getCollectionStore<T>(path, sortFn, this.isTransient);
	}

	/**
	 * Create a traversal guard function based on options
	 */
	private createTraversalGuard(
		visitTracker: VisitTracker,
		options: TraversalOptions
	): TraversalGuard {
		const { maxDepth = DEFAULT_TRAVERSAL_OPTIONS.maxDepth } = options;

		return (currentDepth: number, path: string[], node: any): boolean => {
			// Check depth limit
			if (maxDepth !== undefined && currentDepth >= maxDepth) {
				this.logDebug(`Depth limit reached at ${currentDepth}`, { path });
				return false;
			}

			// Check for cycles
			if (visitTracker.hasVisited(path, node)) {
				this.logDebug(`Cycle detected at path`, { path });
				return false;
			}

			// Mark as visited
			visitTracker.markVisited(path, node);

			return true;
		};
	}

	/**
	 * Traverse a path in the graph, following each segment with cycle detection.
	 * Uses memoization to prevent redundant traversals.
	 */
	public async traversePath<T = any>(
		basePath: string[],
		pathToTraverse: string[],
		accessor: (data: T, segment: string) => any = (data, seg) => {
			if (data && typeof data === 'object') {
				return (data as Record<string, any>)[seg];
			}
			return undefined;
		},
		options: TraversalOptions = this.defaultTraversalOptions
	): Promise<string[]> {
		// Generate a cache key based on the paths and options
		const cacheKey = `${basePath.join('/')}:${pathToTraverse.join('/')}:${JSON.stringify(options)}`;

		// Check if we have a cached result
		if (this.traversalCache.has(cacheKey)) {
			this.logDebug(`Using cached traversal for ${cacheKey}`);
			return this.traversalCache.get(cacheKey)!;
		}

		// Perform the traversal
		this.logDebug(`Traversing path`, { basePath, pathToTraverse, options });
		const resultPromise = this._traversePath<T>(basePath, pathToTraverse, accessor, options);

		// Cache the result
		this.traversalCache.set(cacheKey, resultPromise);

		// Clean up expired cache entries after 5 minutes
		setTimeout(
			() => {
				this.traversalCache.delete(cacheKey);
			},
			5 * 60 * 1000
		);

		return resultPromise;
	}

	/**
	 * Internal implementation of path traversal
	 */
	private async _traversePath<T = any>(
		basePath: string[],
		pathToTraverse: string[],
		accessor: (data: T, segment: string) => any = (data, seg) => {
			if (data && typeof data === 'object') {
				return (data as Record<string, any>)[seg];
			}
			return undefined;
		},
		options: TraversalOptions = this.defaultTraversalOptions
	): Promise<string[]> {
		const visitTracker = new VisitTracker(options);
		let currentPath = [...basePath];
		let currentDepth = 0;

		// Create a guard function that combines depth limiting and cycle detection
		const guard = this.createTraversalGuard(visitTracker, options);

		for (const segment of pathToTraverse) {
			const store = this.getNodeStore<T>(currentPath);
			const data = safeGet(store, null as any);

			// Apply the guard before continuing
			if (!guard(currentDepth, currentPath, data)) {
				throw new Error(
					`Traversal stopped due to depth limit or cycle at ${currentPath.join('/')}`
				);
			}

			if (!data || accessor(data, segment) === undefined) {
				throw new Error(`Cannot traverse path: ${segment} not found in ${currentPath.join('/')}`);
			}

			currentPath = [...currentPath, segment];
			currentDepth++;
		}

		return currentPath;
	}

	/**
	 * Create a reactive store that updates when the path to traverse changes
	 */
	public traversePathReactive<T = any>(
		basePath: string[],
		pathToTraverseStore: Readable<string[]>,
		accessor?: (data: T, segment: string) => any,
		options: TraversalOptions = this.defaultTraversalOptions
	): Readable<string[]> {
		this.logDebug(`Creating reactive path traversal`, { basePath, options });

		return derived(
			pathToTraverseStore,
			($pathToTraverse, set) => {
				this.traversePath<T>(basePath, $pathToTraverse, accessor, options)
					.then(set)
					.catch((err) => {
						console.error(`Error in reactive path traversal:`, err);
						set(basePath); // Fall back to the base path on error
					});
			},
			basePath
		);
	}

	/**
	 * Get nodes from a collection that match a filter function
	 */
	public async getFilteredNodes<T extends CollectionItem>(
		path: string[],
		collectionKey: string,
		filterFn: (node: T) => boolean
	): Promise<[string, T][]> {
		const collectionPath = [...path, collectionKey];
		const collectionStore = this.getCollectionStore<T>(collectionPath);

		return withErrorHandling(
			async () => {
				const nodes = safeGet(collectionStore, [] as [string, T][]);
				return nodes.filter(([_, node]) => filterFn(node));
			},
			[] as [string, T][],
			`Error filtering nodes from ${collectionPath.join('/')}`
		);
	}

	/**
	 * Get a reactive store that filters nodes based on a predicate that can change over time
	 */
	public filterNodesReactive<T extends CollectionItem>(
		path: string[],
		collectionKey: string,
		predicateStore: Readable<(node: T) => boolean>
	): Readable<[string, T][]> {
		const collectionPath = [...path, collectionKey];
		const collectionStore = this.getCollectionStore<T>(collectionPath);

		this.logDebug(`Creating reactive filtered nodes`, { path: collectionPath });

		return derived([collectionStore, predicateStore], ([$collection, $predicate]) => {
			return $collection.filter(([_, node]) => $predicate(node));
		});
	}

	/**
	 * Get a node by its ID in a collection
	 */
	public async getNodeById<T extends CollectionItem>(
		path: string[],
		collectionKey: string,
		nodeId: string
	): Promise<T | null> {
		const nodePath = [...path, collectionKey, nodeId];
		const nodeStore = this.getNodeStore<T>(nodePath);

		return withErrorHandling(
			async () => {
				const node = safeGet(nodeStore, null as any);
				return node;
			},
			null as any,
			`Error getting node by ID: ${nodePath.join('/')}`
		);
	}

	/**
	 * Add a node to a collection
	 * @returns This instance for chaining
	 */

	public async addNode<T extends CollectionItem>(
		path: string[],
		collectionKey: string,
		nodeData: T,
		nodeId?: string
	): Promise<string> {
		const nodeRef = new GunNode([...path, collectionKey], undefined, this.isTransient);
		const id = nodeId || Date.now().toString();

		return withErrorHandling(
			async () => {
				this.logDebug(`Adding node to collection`, {
					path: [...path, collectionKey],
					id,
					nodeData,
					transient: this.isTransient
				});
				await nodeRef.get(id).put(nodeData as any);
				return id;
			},
			'',
			`Error adding node to ${[...path, collectionKey].join('/')}`
		);
	}

	/**
	 * Remove a node from a collection
	 * @returns This instance for chaining
	 */
	public async removeNode(path: string[], collectionKey: string, nodeId: string): Promise<this> {
		const nodeRef = new GunNode([...path, collectionKey], undefined, this.isTransient);

		await withErrorHandling(
			async () => {
				this.logDebug(`Removing node from collection`, {
					path: [...path, collectionKey],
					nodeId,
					transient: this.isTransient
				});
				await nodeRef.get(nodeId).put(null as any);
			},
			undefined,
			`Error removing node ${nodeId} from ${[...path, collectionKey].join('/')}`
		);

		return this;
	}

	/**
	 * Update a node property
	 * @returns This instance for chaining
	 */
	public async updateNodeProperty<T>(path: string[], property: string, value: T): Promise<this> {
		const nodeRef = new GunNode(path, undefined, this.isTransient);

		await withErrorHandling(
			async () => {
				this.logDebug(`Updating node property`, {
					path,
					property,
					value,
					transient: this.isTransient
				});
				await nodeRef.get(property).put(value as any);
			},
			undefined,
			`Error updating property ${property} for ${path.join('/')}`
		);

		return this;
	}

	/**
	 * Create a derived store with cycle detection
	 */
	public createDerivedStore<T>(
		storeKeys: string[],
		stores: Readable<any>[],
		deriveFn: (values: any[]) => T,
		key?: string
	): Readable<T> {
		// Create a unique key if not provided
		const storeKey = key || `derived:${storeKeys.join('+')}`;

		// Check if we already have this derived store
		const existing = this.registry.getDerivedStore<T>(storeKey);
		if (existing) {
			this.logDebug(`Using existing derived store`, { storeKey });
			return existing;
		}

		this.logDebug(`Creating new derived store`, {
			storeKey,
			dependencies: storeKeys
		});

		// Create a new derived store
		const derivedStore = derived(stores, (values) => deriveFn(values), undefined as unknown as T);

		// Register with dependency tracking
		return this.registry.registerDerivedStore(storeKey, derivedStore, storeKeys, this.isTransient);
	}

	/**
	 * Get properties of a node as separate stores
	 */
	public getNodeProperties<T>(
		nodePath: string[],
		properties: Array<keyof T>
	): { [K in keyof T]?: Readable<T[K] | undefined> } {
		this.logDebug(`Getting node properties`, { nodePath, properties });

		// Create the result object with the proper type
		const result: { [K in keyof T]?: Readable<T[K] | undefined> } = {};
		const nodeStore = this.getNodeStore<T>(nodePath);

		for (const prop of properties) {
			// Since we know prop is a keyof T, we can safely use type assertion
			result[prop as keyof T] = derived(nodeStore, ($data) =>
				$data && typeof $data === 'object' && prop in ($data as object)
					? ($data as any)[prop]
					: undefined
			);
		}

		return result;
	}

	/**
	 * Get a reactive store for a specific property of a node
	 */
	public getNodePropertyStore<T>(
		nodePath: string[],
		property: string,
		defaultValue: T
	): Readable<T> {
		this.logDebug(`Getting node property store`, {
			nodePath,
			property,
			defaultValue
		});

		const nodeStore = this.getNodeStore<Record<string, any>>(nodePath);
		return derived(nodeStore, ($data) =>
			$data && typeof $data === 'object' && property in $data
				? ($data[property] as T)
				: defaultValue
		);
	}

	/**
	 * Register a derived store with a specific key
	 */
	public registerDerivedStore<T>(
		key: string,
		store: Readable<T>,
		dependencies: string[] = [],
		transient: boolean = false
	): Readable<T> {
		return this.registry.registerDerivedStore(key, store, dependencies, transient);
	}

	/**
	 * Compose multiple traversal operations into a single function
	 */
	public compose(
		...traversals: Array<(path: string[]) => Promise<string[]>>
	): (startPath: string[]) => Promise<string[]> {
		return async (startPath: string[]): Promise<string[]> => {
			this.logDebug(`Executing composed traversal`, {
				startPath,
				numTraversals: traversals.length
			});

			let currentPath = startPath;
			for (const traversal of traversals) {
				currentPath = await traversal(currentPath);
			}
			return currentPath;
		};
	}

	/**
	 * Clear the traversal cache
	 */
	public clearTraversalCache(): this {
		this.traversalCache.clear();
		this.logDebug(`Traversal cache cleared`);
		return this;
	}

	/**
	 * Traverse a graph using breadth-first search with cycle detection
	 */
	public async bfs<T = any>(
		startPath: string[],
		getNeighbors: (node: T, path: string[]) => Promise<Array<[string, T]>>,
		options: TraversalOptions = this.defaultTraversalOptions
	): Promise<Array<[string[], T]>> {
		const result: Array<[string[], T]> = [];
		const visitTracker = new VisitTracker(options);
		const guard = this.createTraversalGuard(visitTracker, options);

		// Queue of [path, node] pairs to visit
		const queue: Array<[string[], T]> = [];

		// Get the start node
		const startNodeStore = this.getNodeStore<T>(startPath);
		const startNode = safeGet(startNodeStore, null as any);

		if (!startNode) {
			return result;
		}

		// Start BFS
		queue.push([startPath, startNode]);

		while (queue.length > 0) {
			const [currentPath, currentNode] = queue.shift()!;

			// Apply guard
			if (!guard(result.length, currentPath, currentNode)) {
				continue;
			}

			// Add to result
			result.push([currentPath, currentNode]);

			// Get neighbors
			const neighbors = await getNeighbors(currentNode, currentPath);

			// Add neighbors to queue
			for (const [segment, neighborNode] of neighbors) {
				const neighborPath = [...currentPath, segment];
				queue.push([neighborPath, neighborNode]);
			}
		}

		return result;
	}

	/**
	 * Traverse a graph using depth-first search with cycle detection
	 */
	public async dfs<T = any>(
		startPath: string[],
		getNeighbors: (node: T, path: string[]) => Promise<Array<[string, T]>>,
		options: TraversalOptions = this.defaultTraversalOptions
	): Promise<Array<[string[], T]>> {
		const result: Array<[string[], T]> = [];
		const visitTracker = new VisitTracker(options);
		const guard = this.createTraversalGuard(visitTracker, options);

		// Get the start node
		const startNodeStore = this.getNodeStore<T>(startPath);
		const startNode = safeGet(startNodeStore, null as any);

		if (!startNode) {
			return result;
		}

		// Recursive DFS function
		const dfsRecursive = async (path: string[], node: T, depth: number): Promise<void> => {
			// Apply guard
			if (!guard(depth, path, node)) {
				return;
			}

			// Add to result
			result.push([path, node]);

			// Get neighbors
			const neighbors = await getNeighbors(node, path);

			// Visit neighbors
			for (const [segment, neighborNode] of neighbors) {
				const neighborPath = [...path, segment];
				await dfsRecursive(neighborPath, neighborNode, depth + 1);
			}
		};

		// Start DFS
		await dfsRecursive(startPath, startNode, 0);

		return result;
	}
}

/**
 * Create a new reactive graph instance with default options
 */
export function createReactiveGraph(
	options?: TraversalOptions,
	transient: boolean = false
): ReactiveGraph {
	return new ReactiveGraph(options, transient);
}
