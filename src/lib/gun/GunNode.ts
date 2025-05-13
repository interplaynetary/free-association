import { gun, getNodeRef } from './gunSetup';
import type { GunData } from './gunSetup';
import { GunSubscription } from './GunSubscription';
import type { SubscriptionCleanup, SubscriptionHandler } from './gunSetup';

/**
 * Base class for Gun node wrappers that provides a consistent API
 * Following the guide's recommendations for wrapping Gun functionality
 */
export class GunNode<T = any> {
	protected chain: any;
	protected path: string[];
	private certificate?: string;

	/**
	 * Create a new Gun node wrapper
	 * @param path Array of path segments to the node
	 * @param certificate Optional certificate for write access to protected nodes
	 */
	constructor(path: string[], certificate?: string) {
		// console.log('[DEBUG GunNode] Creating GunNode', { path, hasCertificate: !!certificate });
		this.path = [...path]; // Store a copy of the path
		this.chain = getNodeRef(path);
		this.certificate = certificate;
	}

	/**
	 * Get a child node at the given path
	 * @param key The key of the child node
	 * @param NodeClass The class to use for the child node (defaults to GunNode)
	 * @returns A new instance of the provided NodeClass
	 */
	public get<R = any, N extends GunNode<R> = GunNode<R>>(
		key: string,
		NodeClass: new (path: string[], certificate?: string) => N = GunNode as any
	): N {
		// console.log('[DEBUG GunNode] Getting child node', {
		//   parentPath: this.path.join('/'),
		//   childKey: key
		// });
		// Create a new path with the child key
		const childPath = [...this.path, key];
		return new NodeClass(childPath, this.certificate);
	}

	/**
	 * Put a value to the Gun node
	 * @param value The value to put
	 * @param options Optional Gun options (e.g., for certificates)
	 * @returns This node instance for chaining
	 */
	public put(value: Partial<T> | null, options?: any): this {
		// console.log('[DEBUG GunNode] Putting value to node', {
		//   path: this.path.join('/'),
		//   value,
		//   hasCertificate: !!this.certificate
		// });

		if (this.certificate) {
			// Use certificate if available
			options = options || {};
			options.opt = options.opt || {};
			options.opt.cert = this.certificate;
		}

		// Create a clone of options to avoid mutation
		const opts = options ? structuredClone(options) : null;
		this.chain.put(value, null, opts);
		return this;
	}

	/**
	 * Subscribe to updates from this node
	 * @param handler Function to call with updates
	 * @returns A function to unsubscribe
	 */
	public on(handler: SubscriptionHandler<T>): SubscriptionCleanup {
		// console.log('[DEBUG GunNode] Setting up direct .on subscription', {
		//   path: this.path.join('/')
		// });
		const subscription = new GunSubscription<T>(this.path);
		return subscription.on(handler);
	}

	/**
	 * Get a single value from the node and resolve
	 * @returns Promise that resolves with the node value
	 */
	public once(): Promise<T> {
		// console.log('[DEBUG GunNode] Getting value once', {
		//   path: this.path.join('/')
		// });

		return new Promise<T>((resolve, reject) => {
			// Create a timeout to avoid hanging forever
			const timeout = setTimeout(() => {
				console.warn(`Gun once() timed out after 10 seconds for path: ${this.path.join('/')}`);
				resolve(null as any); // Resolve with null instead of rejecting
			}, 10000);

			try {
				this.chain.once((data: T) => {
					clearTimeout(timeout);
					// console.log('[DEBUG GunNode] Received value from once()', {
					//   path: this.path.join('/'),
					//   data
					// });
					resolve(data);
				});
			} catch (error) {
				clearTimeout(timeout);
				console.error(`Error in Gun once() for path: ${this.path.join('/')}`, error);
				resolve(null as any); // Resolve with null instead of rejecting
			}
		});
	}

	/**
	 * Get a readable stream of values from this node
	 * @returns A subscription that can be used as a stream
	 */
	public stream(): GunSubscription<T> {
		// console.log('[DEBUG GunNode] Creating stream subscription', {
		//   path: this.path.join('/')
		// });
		return new GunSubscription<T>(this.path);
	}

	/**
	 * Subscribe to all values in this node (for collections)
	 * @param handler Function to call with each item
	 * @returns A function to unsubscribe
	 */
	public each(handler: SubscriptionHandler<any>): SubscriptionCleanup {
		// console.log('[DEBUG GunNode] Setting up .each() subscription for collection', {
		//   path: this.path.join('/')
		// });
		const subscription = new GunSubscription(this.path);
		const eachSub = subscription.each();
		// console.log('[DEBUG GunNode] Created .each() subscription, now setting up handler');
		return eachSub.on(handler);
	}

	/**
	 * Subscribe to filtered values in this node that match the predicate
	 * @param predicate Function that tests each value, returns true to include the value
	 * @param handler Function to call with each matching item
	 * @returns A function to unsubscribe
	 */
	public filter(
		predicate: (value: any) => boolean,
		handler: SubscriptionHandler<any>
	): SubscriptionCleanup {
		const subscription = new GunSubscription(this.path);
		const filteredSub = subscription.filter(predicate);
		return filteredSub.on(handler);
	}

	/**
	 * Set a certificate for this node and all child nodes accessed through it
	 * @param certificate The certificate string
	 * @returns This node instance for chaining
	 */
	public withCertificate(certificate: string): this {
		this.certificate = certificate;
		return this;
	}

	/**
	 * Get an object representation of this node
	 * @param initialValue Optional initial value to use before data arrives
	 * @returns A subscription that emits the full object
	 */
	public asObject(initialValue: Record<string, any> = {}): GunSubscription<Record<string, any>> {
		const subscription = new GunSubscription(this.path);
		return subscription.asObject(initialValue);
	}

	/**
	 * Get the Svelte-compatible store directly from Gun
	 * Returns the raw Gun chain which has the .subscribe method from gunSetup
	 *
	 * @returns A Svelte-compatible store for the current node
	 */
	public toStore() {
		// Return the raw Gun chain which has subscribe method from gunSetup
		return this.chain;
	}

	/**
	 * Get the full path to this node as an array
	 * @returns Array of path segments
	 */
	public getPath(): string[] {
		return [...this.path]; // Return a copy
	}

	/**
	 * Get the Soul (unique identifier) of this node if it has one
	 * @returns Promise that resolves with the Soul string or undefined
	 */
	public async getSoul(): Promise<string | undefined> {
		try {
			const data = (await this.once()) as GunData;
			if (data && data._ && data._['#']) {
				return data._['#'];
			}
			return undefined;
		} catch (err) {
			console.error('Error getting Soul:', err);
			return undefined;
		}
	}

	/**
	 * Check if this node exists in the database
	 * @returns Promise that resolves to true if the node exists with data
	 */
	public async exists(): Promise<boolean> {
		try {
			const data = await this.once();
			return Boolean(data && Object.keys(data).filter((k) => k !== '_').length > 0);
		} catch (err) {
			return false;
		}
	}

	/**
	 * Get the raw Gun chain
	 * @returns The raw Gun chain
	 */
	public getChain(): any {
		return this.chain;
	}

	// ===== DEEP REFERENCE TRAVERSAL METHODS =====

	/**
	 * Check if a value is a soul reference
	 * @param value The value to check
	 * @returns True if the value is a soul reference
	 */
	private isSoulReference(value: any): boolean {
		return (
			value &&
			typeof value === 'object' &&
			Object.keys(value).length === 1 &&
			value['#'] !== undefined
		);
	}

	/**
	 * Resolve a value if it's a soul reference
	 * @param value The value that might be a soul reference
	 * @returns Promise resolving to either the resolved reference or the original value
	 */
	private async resolveReference(value: any): Promise<any> {
		if (!this.isSoulReference(value)) {
			return value;
		}

		// It's a soul reference, look it up
		const soul = value['#'];
		return new Promise<any>((resolve, reject) => {
			const timeout = setTimeout(() => {
				reject(new Error(`Reference resolution timed out for soul: ${soul}`));
			}, 10000);

			gun.get(soul).once((refData: any) => {
				clearTimeout(timeout);
				resolve(refData);
			});
		});
	}

	/**
	 * Deep get that automatically resolves references to a specified depth
	 * @param maxDepth Maximum depth to resolve references (default: 1)
	 * @returns Promise resolving to the deeply resolved data
	 */
	public async deepGet(maxDepth: number = 1): Promise<any> {
		if (maxDepth <= 0) {
			return this.once();
		}

		try {
			// Get the initial data
			const data = await this.once();

			// If it's null or not an object, just return it
			if (!data || typeof data !== 'object') {
				return data;
			}

			// Start processing the object, resolving references recursively
			return this.resolveReferencesDeep(data, maxDepth);
		} catch (err) {
			console.error('Error in deepGet:', err);
			throw err;
		}
	}

	/**
	 * Recursively resolve all references in an object
	 * @param obj The object to process
	 * @param maxDepth Maximum depth to resolve references
	 * @param currentDepth Current recursion depth
	 * @returns The processed object with resolved references
	 */
	private async resolveReferencesDeep(
		obj: any,
		maxDepth: number,
		currentDepth: number = 0
	): Promise<any> {
		// Base case: we've reached max depth or it's not an object
		if (currentDepth >= maxDepth || !obj || typeof obj !== 'object') {
			return obj;
		}

		// Special handling for Gun metadata
		if (obj._ && typeof obj._ === 'object') {
			// Keep the underscore metadata intact
			const result: Record<string, any> = { _: obj._ };

			// Process all other properties
			for (const key of Object.keys(obj)) {
				if (key === '_') continue;

				const value = obj[key];

				if (this.isSoulReference(value)) {
					// It's a reference, resolve it
					try {
						const resolved = await this.resolveReference(value);
						// Continue deeper with resolved value
						result[key] = await this.resolveReferencesDeep(resolved, maxDepth, currentDepth + 1);
					} catch (err) {
						console.warn(`Failed to resolve reference for key ${key}:`, err);
						// Keep the reference if resolution fails
						result[key] = value;
					}
				} else if (Array.isArray(value)) {
					// Handle arrays
					result[key] = await Promise.all(
						value.map((item) => this.resolveReferencesDeep(item, maxDepth, currentDepth + 1))
					);
				} else if (value && typeof value === 'object') {
					// Recursive case for nested objects
					result[key] = await this.resolveReferencesDeep(value, maxDepth, currentDepth + 1);
				} else {
					// Base case for primitive values
					result[key] = value;
				}
			}

			return result;
		} else if (this.isSoulReference(obj)) {
			// It's a direct reference
			try {
				const resolved = await this.resolveReference(obj);
				return this.resolveReferencesDeep(resolved, maxDepth, currentDepth + 1);
			} catch (err) {
				console.warn('Failed to resolve direct reference:', err);
				return obj;
			}
		} else {
			// Regular object case
			const result: Record<string, any> = Array.isArray(obj) ? ([] as any) : {};

			for (const key of Object.keys(obj)) {
				const value = obj[key];

				if (this.isSoulReference(value)) {
					// It's a reference, resolve it
					try {
						const resolved = await this.resolveReference(value);
						// Continue deeper with resolved value
						result[key] = await this.resolveReferencesDeep(resolved, maxDepth, currentDepth + 1);
					} catch (err) {
						console.warn(`Failed to resolve reference for key ${key}:`, err);
						// Keep the reference if resolution fails
						result[key] = value;
					}
				} else if (Array.isArray(value)) {
					// Handle arrays
					result[key] = await Promise.all(
						value.map((item) => this.resolveReferencesDeep(item, maxDepth, currentDepth + 1))
					);
				} else if (value && typeof value === 'object') {
					// Recursive case for nested objects
					result[key] = await this.resolveReferencesDeep(value, maxDepth, currentDepth + 1);
				} else {
					// Base case for primitive values
					result[key] = value;
				}
			}

			return result;
		}
	}

	/**
	 * Create a subscription that automatically resolves references
	 * @param maxDepth Maximum depth to resolve references (default: 1)
	 * @returns A subscription that emits deeply resolved data
	 */
	public deepStream(maxDepth: number = 1): GunSubscription<any> {
		// Create a regular subscription
		const baseSubscription = this.stream();

		// Map values through the deep resolver
		return baseSubscription.map(async (value) => {
			// Skip null/undefined values
			if (value === null || value === undefined) {
				return value;
			}

			try {
				return await this.resolveReferencesDeep(value, maxDepth);
			} catch (err) {
				console.error('Error in deepStream:', err);
				return value; // Return original value on error
			}
		});
	}

	/**
	 * Subscribe to deeply resolved updates
	 * @param handler Function to call with updates
	 * @param maxDepth Maximum depth to resolve references (default: 1)
	 * @returns A function to unsubscribe
	 */
	public deepOn(handler: SubscriptionHandler<any>, maxDepth: number = 1): SubscriptionCleanup {
		return this.deepStream(maxDepth).on(handler);
	}
}
