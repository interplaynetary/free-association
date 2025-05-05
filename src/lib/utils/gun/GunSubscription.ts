import { gun, transientGun, user, transientUser } from './gunSetup';
import type { SubscriptionHandler, SubscriptionCleanup } from './gunSetup';

/**
 * A wrapper around Gun subscriptions that provides:
 * 1. Proper cleanup of subscriptions
 * 2. Better error handling
 * 3. Consistent stream-based API
 * 4. Protection against receiving data after unsubscribe
 */
export class GunSubscription<T = any> {
	private gunRef: any;
	private active: boolean = false;
	private handlers: Set<SubscriptionHandler<T>> = new Set();
	private reader: ReadableStreamDefaultReader<T> | null = null;
	private stream: ReadableStream<T> | null = null;
	// Track last value for late subscribers
	private lastValue: T | undefined = undefined;
	private hasValue: boolean = false;
	// Add timeout for operations
	private timeoutMs: number = 10000;

	/**
	 * Create a subscription to a Gun node path
	 * @param path Array of path segments to the Gun node
	 * @param timeoutMs Optional timeout in milliseconds for operations
	 * @param transient If true, use transientGun which doesn't persist to localStorage
	 */
	constructor(path: string[], timeoutMs?: number, transient: boolean = false) {
		// Navigate the Gun path
		let ref = transient ? transientGun : (gun as any);
		for (const segment of path) {
			ref = ref.get(segment);
		}
		this.gunRef = ref;

		if (timeoutMs !== undefined) {
			this.timeoutMs = timeoutMs;
		}
	}

	/**
	 * Create a subscription to a Gun path with a more natural syntax
	 * @param path Array of path segments to the Gun node
	 * @param transient If true, use transientGun which doesn't persist to localStorage
	 * @returns A new GunSubscription instance
	 */
	public static from<T>(path: string[], transient: boolean = false): GunSubscription<T> {
		return new GunSubscription<T>(path, undefined, transient);
	}

	/**
	 * Create a subscription with a static value that never changes
	 * @param value The static value
	 * @returns A GunSubscription that emits only the provided value
	 */
	public static of<T>(value: T): GunSubscription<T> {
		const sub = new GunSubscription<T>([]);
		sub.lastValue = value;
		sub.hasValue = true;

		// Override subscribe to just emit the value once
		const originalSubscribe = sub.subscribe;
		sub.subscribe = function (): ReadableStream<T> {
			if (this.stream) {
				return this.stream;
			}

			this.active = true;
			this.stream = new ReadableStream<T>({
				start: (controller) => {
					controller.enqueue(value);
				},
				cancel: () => {
					this.unsubscribe();
				}
			});

			return this.stream;
		};

		return sub;
	}

	/**
	 * Start the subscription and get a stream that can be read
	 * @returns A ReadableStream of data from the Gun node
	 */
	public subscribe(): ReadableStream<T> {
		if (this.stream) {
			return this.stream;
		}

		this.active = true;

		// Create a stream that will emit values from Gun
		this.stream = new ReadableStream<T>({
			start: (controller) => {
				// Emit the last value immediately if we have one
				if (this.hasValue && this.lastValue !== undefined) {
					controller.enqueue(this.lastValue);
				}

				// Set up Gun subscription
				this.gunRef.on((data: T, key: string) => {
					if (!this.active) return; // Skip if we've been unsubscribed
					if (key === '_') return; // Skip Gun metadata

					// Store the last value
					this.lastValue = data;
					this.hasValue = true;

					// Emit the value to all handlers
					this.handlers.forEach((handler) => {
						try {
							handler(data);
						} catch (err) {
							console.error('Error in Gun subscription handler:', err);
						}
					});

					// Send the value to the stream consumer
					controller.enqueue(data);
				});
			},
			cancel: () => {
				// Clean up when the stream is cancelled
				this.unsubscribe();
			}
		});

		return this.stream;
	}

	/**
	 * Subscribe with a callback instead of using the stream directly
	 * @param handler Function to call when new data arrives
	 * @returns A function to unsubscribe
	 */
	public on(handler: SubscriptionHandler<T>): SubscriptionCleanup {
		// console.log('[DEBUG GunSubscription] Setting up .on subscription', {
		//   path: this.gunRef?._.get || 'unknown',
		//   active: this.active,
		//   hasExistingValue: this.hasValue
		// });

		// Start the subscription if it's not active
		if (!this.active) {
			// console.log('[DEBUG GunSubscription] Activating subscription via .subscribe()');
			this.subscribe();
		}

		// Add the handler
		this.handlers.add(handler);

		// Immediately emit the last value if we have one
		if (this.hasValue && this.lastValue !== undefined) {
			// console.log('[DEBUG GunSubscription] Emitting cached value immediately', {
			//   path: this.gunRef?._.get || 'unknown',
			//   value: this.lastValue
			// });
			try {
				handler(this.lastValue);
			} catch (err) {
				console.error('Error in Gun subscription handler (immediate):', err);
			}
		}

		// Return a function to remove this specific handler
		return () => {
			// console.log('[DEBUG GunSubscription] Removing handler from subscription', {
			//   path: this.gunRef?._.get || 'unknown',
			//   handlersLeft: this.handlers.size - 1
			// });
			this.handlers.delete(handler);
			// If no more handlers, unsubscribe completely
			if (this.handlers.size === 0) {
				// console.log('[DEBUG GunSubscription] No handlers left, unsubscribing completely', {
				//   path: this.gunRef?._.get || 'unknown'
				// });
				this.unsubscribe();
			}
		};
	}

	/**
	 * Get a single value from Gun and then unsubscribe
	 * Following the guide's recommendation for handling timeouts
	 * @returns Promise that resolves with the value
	 */
	public once(): Promise<T> {
		return new Promise<T>((resolve, reject) => {
			// Immediately return last value if available
			if (this.hasValue && this.lastValue !== undefined) {
				resolve(this.lastValue);
				return;
			}

			// Create a timeout to avoid hanging forever
			const timeout = setTimeout(() => {
				if (cleanupFn) cleanupFn();
				reject(new Error(`Gun once() timed out after ${this.timeoutMs}ms`));
			}, this.timeoutMs);

			// Declare cleanup variable first before using it
			let cleanupFn: (() => void) | null = null;

			// Set up a one-time handler
			cleanupFn = this.on((data) => {
				clearTimeout(timeout);
				if (cleanupFn) cleanupFn(); // Call cleanup using the now-defined variable
				resolve(data);
			});
		});
	}

	/**
	 * Stop the subscription and clean up resources
	 */
	public unsubscribe(): void {
		if (!this.active) return;

		// Mark as inactive
		this.active = false;

		// Clean up Gun subscription
		if (this.gunRef) {
			this.gunRef.off();
		}

		// Clear handlers
		this.handlers.clear();

		// Clean up stream
		if (this.reader) {
			this.reader.cancel();
			this.reader = null;
		}

		this.stream = null;
	}

	/**
	 * Create a Svelte-compatible store from this subscription
	 * @deprecated Use gun.chain.subscribe or customStore from gunSetup instead
	 * @returns An object with subscribe method compatible with Svelte stores
	 */
	public toStore() {
		console.warn(
			'GunSubscription.toStore() is deprecated. Use gun.chain.subscribe or customStore from gunSetup instead'
		);
		return {
			subscribe: (run: SubscriptionHandler<T>) => {
				const cleanup = this.on(run);
				return cleanup;
			}
		};
	}

	/**
	 * Map values from a Gun node using the provided mapper function
	 * @param mapFn Function to transform each item
	 * @returns A new GunSubscription with mapped values
	 */
	public map<R>(mapFn: (value: T) => R): GunSubscription<R> {
		const source = this.subscribe();
		const mappedSub = new GunSubscription<R>([]);

		// Create a transformed stream
		mappedSub.stream = new ReadableStream<R>({
			start: (controller) => {
				// Get a reader for the source stream
				this.reader = source.getReader();

				// Read values and map them
				const processValues = async () => {
					try {
						while (true) {
							// Add null check before accessing the reader
							if (!this.reader) {
								console.error('Reader is null, cannot process values');
								break;
							}

							const { value, done } = await this.reader.read();
							if (done) break;

							// Map the value and send to stream
							const mappedValue = mapFn(value);
							mappedSub.lastValue = mappedValue;
							mappedSub.hasValue = true;
							controller.enqueue(mappedValue);

							// Send to handlers
							mappedSub.handlers.forEach((handler) => {
								try {
									handler(mappedValue);
								} catch (err) {
									console.error('Error in mapped subscription handler:', err);
								}
							});
						}
					} catch (err) {
						console.error('Error reading from Gun subscription:', err);
						controller.error(err);
					}
				};

				processValues();
			},
			cancel: () => {
				// Clean up when the stream is cancelled
				mappedSub.unsubscribe();
				this.unsubscribe();
			}
		});

		return mappedSub;
	}

	/**
	 * Combine values from this stream with another stream
	 * Emits values whenever either stream emits, using the latest value from each
	 * @param other The other stream to combine with
	 * @param combineFn Function that combines values from both streams
	 * @returns A new GunSubscription with combined values
	 */
	public combine<U, R>(
		other: GunSubscription<U>,
		combineFn: (a: T, b: U) => R
	): GunSubscription<R> {
		const combinedSub = new GunSubscription<R>([]);
		combinedSub.active = true;

		let lastValueA: T | undefined = this.lastValue;
		let lastValueB: U | undefined = other.lastValue;
		let hasValueA = this.hasValue;
		let hasValueB = other.hasValue;

		// Set up subscription to this stream
		const subA = this.on((valueA) => {
			lastValueA = valueA;
			hasValueA = true;

			// Only emit if we have values from both streams
			if (hasValueB && lastValueB !== undefined) {
				const combined = combineFn(valueA, lastValueB);
				combinedSub.lastValue = combined;
				combinedSub.hasValue = true;

				// Notify all handlers
				combinedSub.handlers.forEach((handler) => {
					try {
						handler(combined);
					} catch (err) {
						console.error('Error in combined subscription handler:', err);
					}
				});
			}
		});

		// Set up subscription to the other stream
		const subB = other.on((valueB) => {
			lastValueB = valueB;
			hasValueB = true;

			// Only emit if we have values from both streams
			if (hasValueA && lastValueA !== undefined) {
				const combined = combineFn(lastValueA, valueB);
				combinedSub.lastValue = combined;
				combinedSub.hasValue = true;

				// Notify all handlers
				combinedSub.handlers.forEach((handler) => {
					try {
						handler(combined);
					} catch (err) {
						console.error('Error in combined subscription handler:', err);
					}
				});
			}
		});

		// Override the on method to handle proper cleanup
		const originalOn = combinedSub.on.bind(combinedSub);
		combinedSub.on = (handler: SubscriptionHandler<R>) => {
			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up source subscriptions
				if (combinedSub.handlers.size === 0) {
					subA();
					subB();
					combinedSub.active = false;
				}
			};
		};

		return combinedSub;
	}

	/**
	 * Combine with another stream, but only emit when this stream emits
	 * using the latest value from the other stream
	 * @param other The other stream to get latest values from
	 * @param combineFn Function that combines values from both streams
	 * @returns A new GunSubscription with combined values
	 */
	public withLatestFrom<U, R>(
		other: GunSubscription<U>,
		combineFn: (a: T, b: U) => R
	): GunSubscription<R> {
		const resultSub = new GunSubscription<R>([]);
		resultSub.active = true;

		let latestB: U | undefined = other.lastValue;
		let hasLatestB = other.hasValue;

		// Listen for values from the other stream
		const subB = other.on((valueB) => {
			latestB = valueB;
			hasLatestB = true;
		});

		// Listen for values from this stream and combine with latest from other
		const subA = this.on((valueA) => {
			if (hasLatestB && latestB !== undefined) {
				const result = combineFn(valueA, latestB);
				resultSub.lastValue = result;
				resultSub.hasValue = true;

				// Notify all handlers
				resultSub.handlers.forEach((handler) => {
					try {
						handler(result);
					} catch (err) {
						console.error('Error in withLatestFrom subscription handler:', err);
					}
				});
			}
		});

		// Override the on method to handle proper cleanup
		const originalOn = resultSub.on.bind(resultSub);
		resultSub.on = (handler: SubscriptionHandler<R>) => {
			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up source subscriptions
				if (resultSub.handlers.size === 0) {
					subA();
					subB();
					resultSub.active = false;
				}
			};
		};

		return resultSub;
	}

	/**
	 * Merge multiple streams into one, emitting values from all of them
	 * @param others Other streams to merge with
	 * @returns A new GunSubscription that emits values from all input streams
	 */
	public merge(...others: GunSubscription<T>[]): GunSubscription<T> {
		const mergedSub = new GunSubscription<T>([]);
		mergedSub.active = true;

		// Set up all the subscriptions including this one
		const allStreams = [this, ...others];
		const cleanups: SubscriptionCleanup[] = [];

		allStreams.forEach((stream) => {
			const cleanup = stream.on((value) => {
				mergedSub.lastValue = value;
				mergedSub.hasValue = true;

				// Notify all handlers
				mergedSub.handlers.forEach((handler) => {
					try {
						handler(value);
					} catch (err) {
						console.error('Error in merged subscription handler:', err);
					}
				});
			});

			cleanups.push(cleanup);
		});

		// Override the on method to handle proper cleanup
		const originalOn = mergedSub.on.bind(mergedSub);
		mergedSub.on = (handler: SubscriptionHandler<T>) => {
			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up all source subscriptions
				if (mergedSub.handlers.size === 0) {
					cleanups.forEach((fn) => fn());
					mergedSub.active = false;
				}
			};
		};

		return mergedSub;
	}

	/**
	 * Debounce emissions from this stream by a specified time
	 * @param ms Time in milliseconds to debounce
	 * @returns A new GunSubscription that emits debounced values
	 */
	public debounce(ms: number): GunSubscription<T> {
		const debouncedSub = new GunSubscription<T>([]);
		debouncedSub.active = true;

		let timeout: any = null;

		// Set up subscription to this stream
		const sub = this.on((value) => {
			// Clear any existing timeout
			if (timeout !== null) {
				clearTimeout(timeout);
			}

			// Create a new timeout
			timeout = setTimeout(() => {
				debouncedSub.lastValue = value;
				debouncedSub.hasValue = true;

				// Notify all handlers
				debouncedSub.handlers.forEach((handler) => {
					try {
						handler(value);
					} catch (err) {
						console.error('Error in debounced subscription handler:', err);
					}
				});
			}, ms);
		});

		// Override the on method to handle proper cleanup
		const originalOn = debouncedSub.on.bind(debouncedSub);
		debouncedSub.on = (handler: SubscriptionHandler<T>) => {
			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up source subscription and timeout
				if (debouncedSub.handlers.size === 0) {
					sub();
					if (timeout !== null) {
						clearTimeout(timeout);
					}
					debouncedSub.active = false;
				}
			};
		};

		return debouncedSub;
	}

	/**
	 * Start this subscription with a specified value
	 * @param value The initial value to emit
	 * @returns A new GunSubscription that first emits the specified value
	 */
	public startWith(value: T): GunSubscription<T> {
		const startWithSub = new GunSubscription<T>([]);
		startWithSub.active = true;
		startWithSub.lastValue = value;
		startWithSub.hasValue = true;

		// Set up subscription to this stream
		const sub = this.on((nextValue) => {
			startWithSub.lastValue = nextValue;

			// Notify all handlers
			startWithSub.handlers.forEach((handler) => {
				try {
					handler(nextValue);
				} catch (err) {
					console.error('Error in startWith subscription handler:', err);
				}
			});
		});

		// Override the on method to handle proper cleanup and initial value
		const originalOn = startWithSub.on.bind(startWithSub);
		startWithSub.on = (handler: SubscriptionHandler<T>) => {
			// Immediately emit the initial value
			try {
				handler(value);
			} catch (err) {
				console.error('Error in startWith initial value handler:', err);
			}

			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up source subscription
				if (startWithSub.handlers.size === 0) {
					sub();
					startWithSub.active = false;
				}
			};
		};

		return startWithSub;
	}

	/**
	 * Map each value to a new stream and flatten the results
	 * Cancels previous inner subscriptions when a new value arrives
	 * @param project Function that maps each value to a new GunSubscription
	 * @returns A GunSubscription that emits values from the inner streams
	 */
	public switchMap<R>(project: (value: T) => GunSubscription<R>): GunSubscription<R> {
		const resultSub = new GunSubscription<R>([]);
		resultSub.active = true;

		let currentInnerSub: SubscriptionCleanup | null = null;

		// Set up subscription to this stream
		const mainSub = this.on((outerValue) => {
			// Cancel previous inner subscription
			if (currentInnerSub !== null) {
				currentInnerSub();
				currentInnerSub = null;
			}

			// Create new inner subscription
			const innerStream = project(outerValue);
			currentInnerSub = innerStream.on((innerValue) => {
				resultSub.lastValue = innerValue;
				resultSub.hasValue = true;

				// Notify all handlers
				resultSub.handlers.forEach((handler) => {
					try {
						handler(innerValue);
					} catch (err) {
						console.error('Error in switchMap subscription handler:', err);
					}
				});
			});
		});

		// Override the on method to handle proper cleanup
		const originalOn = resultSub.on.bind(resultSub);
		resultSub.on = (handler: SubscriptionHandler<R>) => {
			const cleanup = originalOn(handler);

			return () => {
				cleanup();

				// If no more handlers, clean up all subscriptions
				if (resultSub.handlers.size === 0) {
					mainSub();
					if (currentInnerSub !== null) {
						currentInnerSub();
						currentInnerSub = null;
					}
					resultSub.active = false;
				}
			};
		};

		return resultSub;
	}

	/**
	 * Create a mapped subscription to all values in a Gun node
	 * For iterating over collections of values
	 * @returns A subscription to all values in the node
	 */
	public each(): GunSubscription {
		// console.log('[DEBUG GunSubscription] Creating .each() subscription', {
		//   path: this.gunRef?._.get || 'unknown'
		// });

		// Create a new subscription that will use .map() internally
		const mapSub = new GunSubscription([]);
		mapSub.gunRef = this.gunRef.map();
		mapSub.active = true;

		// Track seen keys to help with cleanup
		const seenKeys = new Set<string>();
		// NEW: Track removed keys to avoid duplicate removal events
		const removedKeys = new Set<string>();

		// Create the stream with proper Gun map handling
		mapSub.stream = new ReadableStream({
			start: (controller) => {
				// console.log('[DEBUG GunSubscription.each] Starting stream controller');

				// Force immediate emit of existing data
				this.gunRef.map().once((data: any, key: string) => {
					if (key === '_') return; // Skip Gun metadata
					if (!data) return; // Skip null/undefined data

					// console.log('[DEBUG GunSubscription.each] Initial data from .once()', { key, data });

					// Add the key to the data for context
					const valueWithKey = { ...data, _key: key };
					seenKeys.add(key);

					// Emit initial data
					controller.enqueue(valueWithKey);

					// Store as last value for late subscribers
					mapSub.lastValue = valueWithKey;
					mapSub.hasValue = true;

					// Emit to handlers
					mapSub.handlers.forEach((handler) => {
						try {
							handler(valueWithKey);
						} catch (err) {
							console.error('Error in initial map handler:', err);
						}
					});
				});

				// Set up ongoing subscription
				mapSub.gunRef.on((data: any, key: string) => {
					// console.log('[DEBUG GunSubscription.each] Live update from .on()', {
					//   key,
					//   data,
					//   active: mapSub.active
					// });

					if (!mapSub.active) return;
					if (key === '_') return; // Skip Gun metadata

					if (!data) {
						// Item was removed, we need to handle this
						// NEW: Skip if we've already processed this key's removal
						if (removedKeys.has(key)) {
							// console.log('[DEBUG GunSubscription.each] Skipping duplicate removal event for', { key });
							return;
						}

						// console.log('[DEBUG GunSubscription.each] Item removed', { key });
						seenKeys.delete(key);
						// NEW: Add to removed keys set
						removedKeys.add(key);

						// Emit a removal event
						const removalEvent = { _key: key, _removed: true };
						controller.enqueue(removalEvent);

						// Notify handlers of removal
						mapSub.handlers.forEach((handler) => {
							try {
								handler(removalEvent);
							} catch (err) {
								console.error('Error in removal handler:', err);
							}
						});

						return;
					}

					// Add the key to the data for context
					const valueWithKey = { ...data, _key: key };
					// console.log('[DEBUG GunSubscription.each] Processing value', { key, valueWithKey });
					seenKeys.add(key);
					// NEW: If this was previously removed, remove it from the removed keys set
					removedKeys.delete(key);

					// Store as last value
					mapSub.lastValue = valueWithKey;
					mapSub.hasValue = true;

					// Emit to handlers
					mapSub.handlers.forEach((handler) => {
						// console.log('[DEBUG GunSubscription.each] Emitting to handler', {
						//   key,
						//   handlersCount: mapSub.handlers.size
						// });
						try {
							handler(valueWithKey);
						} catch (err) {
							console.error('Error in map handler:', err);
						}
					});

					// Send to stream
					controller.enqueue(valueWithKey);
				});
			},
			cancel: () => {
				// console.log('[DEBUG GunSubscription.each] Stream cancelled, unsubscribing');
				mapSub.unsubscribe();
			}
		});

		return mapSub;
	}

	/**
	 * Transform this subscription to a JSON-compatible object
	 * @param initialValue Optional initial value to use before data arrives
	 * @returns A subscription that emits the full object
	 */
	public asObject(initialValue: Record<string, any> = {}): GunSubscription<Record<string, any>> {
		const objSub = new GunSubscription<Record<string, any>>([]);
		objSub.hasValue = true;
		objSub.lastValue = initialValue;

		// Track the current state
		let currentState = { ...initialValue };

		// Create the stream
		objSub.stream = new ReadableStream<Record<string, any>>({
			start: (controller) => {
				// Start by emitting the initial value
				controller.enqueue(currentState);

				// Set up subscription to each property
				this.gunRef.map().on((data: any, key: string) => {
					if (!objSub.active) return;
					if (key === '_') return; // Skip Gun metadata

					// Update the state with this property
					if (data === null || data === undefined) {
						// Property was removed
						delete currentState[key];
					} else {
						currentState[key] = data;
					}

					// Clone to avoid reference issues
					const newState = { ...currentState };
					objSub.lastValue = newState;

					// Emit to handlers
					objSub.handlers.forEach((handler) => {
						try {
							handler(newState);
						} catch (err) {
							console.error('Error in object subscription handler:', err);
						}
					});

					// Send to stream
					controller.enqueue(newState);
				});
			},
			cancel: () => {
				objSub.unsubscribe();
			}
		});

		return objSub;
	}

	/**
	 * Create a filtered subscription to values in a Gun node that match the predicate
	 * Similar to .each() but only emits values that pass the filter function
	 * @param predicate Function that tests each value, returns true to include the value
	 * @returns A subscription that only emits values passing the predicate
	 */
	public filter(predicate: (value: any) => boolean): GunSubscription {
		const filterSub = new GunSubscription([]);
		filterSub.active = true;

		// Track seen keys to help with cleanup
		const seenKeys = new Set<string>();
		// Track removed keys to avoid duplicate removal events
		const removedKeys = new Set<string>();

		// Create the stream with filtering
		filterSub.stream = new ReadableStream({
			start: (controller) => {
				// Force immediate emit of existing data that passes the filter
				this.gunRef.map().once((data: any, key: string) => {
					if (key === '_') return; // Skip Gun metadata
					if (!data) return; // Skip null/undefined data

					// Add the key to the data for context
					const valueWithKey = { ...data, _key: key };

					// Only process if it passes the filter
					if (predicate(valueWithKey)) {
						seenKeys.add(key);

						// Emit initial data
						controller.enqueue(valueWithKey);

						// Store as last value for late subscribers
						filterSub.lastValue = valueWithKey;
						filterSub.hasValue = true;

						// Emit to handlers
						filterSub.handlers.forEach((handler) => {
							try {
								handler(valueWithKey);
							} catch (err) {
								console.error('Error in initial filter handler:', err);
							}
						});
					}
				});

				// Set up ongoing subscription
				this.gunRef.map().on((data: any, key: string) => {
					if (!filterSub.active) return;
					if (key === '_') return; // Skip Gun metadata

					if (!data) {
						// Item was removed, handle if we were tracking it
						if (removedKeys.has(key)) {
							return; // Skip duplicate removal event
						}

						if (seenKeys.has(key)) {
							seenKeys.delete(key);
							removedKeys.add(key);

							// Emit a removal event
							const removalEvent = { _key: key, _removed: true };
							controller.enqueue(removalEvent);

							// Notify handlers of removal
							filterSub.handlers.forEach((handler) => {
								try {
									handler(removalEvent);
								} catch (err) {
									console.error('Error in removal handler:', err);
								}
							});
						}

						return;
					}

					// Add the key to the data for context
					const valueWithKey = { ...data, _key: key };

					// Only process if it passes the filter
					if (predicate(valueWithKey)) {
						seenKeys.add(key);
						removedKeys.delete(key);

						// Store as last value
						filterSub.lastValue = valueWithKey;
						filterSub.hasValue = true;

						// Emit to handlers
						filterSub.handlers.forEach((handler) => {
							try {
								handler(valueWithKey);
							} catch (err) {
								console.error('Error in filter handler:', err);
							}
						});

						// Send to stream
						controller.enqueue(valueWithKey);
					} else if (seenKeys.has(key)) {
						// The item was previously included but no longer passes the filter
						// Handle as if it was removed
						seenKeys.delete(key);
						removedKeys.add(key);

						// Emit a removal event
						const removalEvent = { _key: key, _removed: true };
						controller.enqueue(removalEvent);

						// Notify handlers of removal
						filterSub.handlers.forEach((handler) => {
							try {
								handler(removalEvent);
							} catch (err) {
								console.error('Error in filter handler for removal:', err);
							}
						});
					}
				});
			},
			cancel: () => {
				filterSub.unsubscribe();
			}
		});

		return filterSub;
	}
}
