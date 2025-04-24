import { GunNode } from '../gun/GunNode';
import { GunSubscription } from '../gun/GunSubscription';
import type { SubscriptionCleanup } from '../gun/gunSetup';
import { writable, derived, get, type Readable } from 'svelte/store';

/**
 * Type for items in a collection
 */
export interface CollectionItem {
  _key?: string;
  _removed?: boolean;
  [key: string]: any;
}

/**
 * Type for a Gun node store that provides reactive access to a Gun node
 */
export interface GunStore<T> extends Readable<T> {
  /** The underlying Gun node */
  node: () => GunNode;
  /** The underlying subscription */
  subscription: () => GunSubscription<T>;
}

/**
 * Type for a collection store that provides reactive access to a Gun collection
 */
export interface CollectionStore<T extends CollectionItem> extends Readable<[string, T][]> {
  /** Add an item to the collection */
  add: (value: Omit<T, '_key' | '_removed'>) => string;
  /** Update an item in the collection */
  update: (id: string, changes: Partial<T>) => void;
  /** Remove an item from the collection */
  remove: (id: string) => void;
  /** Get a specific item from the collection as a store */
  item: (id: string) => GunStore<T>;
  /** The underlying Gun node */
  node: () => GunNode;
  /** The underlying subscription */
  subscription: () => GunSubscription<[string, T][]>;
}

/**
 * Creates a reactive Gun store for a single node
 * 
 * @param path Path to the Gun node
 * @param initialValue Optional initial value before data loads
 * @param transient If true, use transientGun which doesn't persist to localStorage
 * @returns A store that reactively updates when the Gun node changes
 */
export function createGunStore<T>(
  path: string | string[],
  initialValue?: T,
  transient: boolean = false
): GunStore<T> {
  // Create the Gun node
  const nodePath = typeof path === 'string' ? [path] : path;
  const node = new GunNode<T>(nodePath, undefined, transient);
  
  // // // console.log(`[DEBUG] Creating Gun store for path: ${nodePath.join('/')}`, { node });
  
  // Create a subscription from the node
  let subscription = node.stream();
  
  // Apply initial value if provided
  if (initialValue !== undefined) {
    subscription = subscription.startWith(initialValue as T);
  }
  
  // Create a Svelte store for tracking active subscribers
  const { subscribe } = writable<T>(initialValue as T, (set) => {
    // console.log(`[DEBUG] Setting up Gun subscription for path: ${nodePath.join('/')}`);
    
    // Set up the subscription and pass values to the store
    const cleanup = subscription.on(value => {
      // console.log(`[DEBUG] Received update in Gun store for path: ${nodePath.join('/')}`, value);
      set(value as T);
    });
    
    // Return the unsubscribe function
    return () => {
      // console.log(`[DEBUG] Cleaning up Gun subscription for path: ${nodePath.join('/')}`);
      cleanup();
    };
  });
  
  // Return the store with additional methods
  return {
    subscribe,
    node: () => node,
    subscription: () => subscription
  };
}

/**
 * Creates a reactive store for a Gun collection
 * Handles collection maintenance (add, remove, update)
 * 
 * @param path Path to the Gun collection
 * @param sortFn Optional function to sort the collection
 * @param transient If true, use transientGun which doesn't persist to localStorage
 * @returns A store that provides access to the collection as an array of [id, item] pairs
 */
export function createCollectionStore<T extends CollectionItem>(
  path: string | string[],
  sortFn?: (a: [string, T], b: [string, T]) => number,
  transient: boolean = false
): CollectionStore<T> {
  // Create the Gun node
  const nodePath = typeof path === 'string' ? [path] : path;
  const node = new GunNode(nodePath, undefined, transient);
  
  // console.log(`[DEBUG] Creating collection store for path: ${nodePath.join('/')}`, { node });
  
  // Start with an empty array, will be populated by items from Gun
  let items: [string, T][] = [];
  
  // Create a writable store with an empty array as initial value
  const { subscribe, set } = writable<[string, T][]>([]);
  
  // Track subscription state
  let activeSubscribers = 0;
  let gunSubscription: SubscriptionCleanup | undefined;
  
  // Set up the subscription function for this collection
  const setupSubscription = () => {
    // console.log(`[DEBUG] Setting up collection subscription for path: ${nodePath.join('/')}`);
    
    // Track already removed items to avoid processing them multiple times
    const removedItems = new Set<string>();
    
    // Define filter predicate to limit processed data
    const filterPredicate = (item: any) => {
      if (!item) return false;
      
      const id = item._key as string;
      
      // Skip if we've already processed this remove event
      if (item._removed && removedItems.has(id)) {
        return false;
      }
      
      return true;
    };
    
    // Use the Gun node's filter method instead of each
    return node.filter(filterPredicate, (item: any) => {
      // console.log(`[DEBUG] Collection received filtered item update:`, { path: nodePath.join('/'), itemKey: item._key, item });
      
      // Each item will have a _key property
      const id = item._key as string;
      
      if (item._removed) {
        // Skip if we've already removed this item from our array
        if (removedItems.has(id)) {
          // console.log(`[DEBUG] Skipping already removed item: ${id}`);
          return;
        }
        
        // console.log(`[DEBUG] Item removed: ${id}`);
        // Remove items marked for deletion
        const wasRemoved = items.some(([itemId]) => itemId === id);
        items = items.filter(([itemId]) => itemId !== id);
        
        // Only track if the item was actually in our array
        if (wasRemoved) {
          removedItems.add(id);
          // Schedule cleanup of removed items after some time
          setTimeout(() => removedItems.delete(id), 10000);
        }
      } else {
        // If we get an update for an item, it's no longer removed
        removedItems.delete(id);
        
        // Update or add items
        const index = items.findIndex(([itemId]) => itemId === id);
        if (index >= 0) {
          // console.log(`[DEBUG] Updating existing item: ${id}`);
          items[index] = [id, item as T];
        } else {
          // console.log(`[DEBUG] Adding new item: ${id}`);
          items.push([id, item as T]);
        }
      }
      
      // Sort items if a sort function was provided
      if (sortFn) {
        items.sort(sortFn);
      }
      
      // console.log(`[DEBUG] Updated collection state:`, { path: nodePath.join('/'), itemCount: items.length });
      
      // Update the store with a fresh copy of the array
      set([...items]);
    });
  };
  
  // Return the store with additional methods
  return {
    subscribe: (callback) => {
      // Increment active subscribers count
      activeSubscribers++;
      // console.log(`[DEBUG] New subscriber to collection ${nodePath.join('/')}. Total subscribers: ${activeSubscribers}`);
      
      // Initialize Gun subscription if it doesn't exist
      if (!gunSubscription) {
        // console.log(`[DEBUG] No existing Gun subscription, creating one for: ${nodePath.join('/')}`);
        gunSubscription = setupSubscription();
      } else {
        // console.log(`[DEBUG] Using existing Gun subscription for: ${nodePath.join('/')}`);
      }
      
      // Subscribe to the writable store
      const unsubscribe = subscribe(callback);
      
      // Return function that handles cleanup when this subscriber unsubscribes
      return () => {
        // Run the standard unsubscribe function
        unsubscribe();
        
        // Decrement active subscribers count
        activeSubscribers--;
        // console.log(`[DEBUG] Subscriber left collection ${nodePath.join('/')}. Remaining subscribers: ${activeSubscribers}`);
        
        // If no more subscribers, clean up the Gun subscription
        if (activeSubscribers === 0 && gunSubscription) {
          // console.log(`[DEBUG] No more subscribers, cleaning up Gun subscription for: ${nodePath.join('/')}`);
          gunSubscription();
          gunSubscription = undefined;
        }
      };
    },
    
    add: (value: Omit<T, '_key' | '_removed'>) => {
      const id = Date.now().toString();
      // console.log(`[DEBUG] Adding item to collection ${nodePath.join('/')}:`, { id, value });
      node.get(id).put(value as any);
      return id;
    },
    
    update: (id: string, changes: Partial<T>) => {
      // console.log(`[DEBUG] Updating item in collection ${nodePath.join('/')}:`, { id, changes });
      node.get(id).put(changes as any);
    },
    
    remove: (id: string) => {
      // console.log(`[DEBUG] Removing item from collection ${nodePath.join('/')}:`, { id });
      node.get(id).put(null as any);
    },
    
    item: (id: string) => {
      // console.log(`[DEBUG] Creating item store for ${nodePath.join('/')}/${id}`);
      return createGunStore<T>([...nodePath, id]);
    },
    
    node: () => node,
    
    subscription: () => {
      // console.log(`[DEBUG] Creating a raw subscription for collection ${nodePath.join('/')}`);
      // Create a subscription for the collection
      const subscription = new GunSubscription<[string, T][]>(nodePath);
      
      // Create a transformer that turns the collection into an array
      return subscription.map(() => {
        // console.log(`[DEBUG] Mapping raw subscription data for ${nodePath.join('/')}`);
        // Return a copy of the current items array
        return [...items];
      });
    }
  };
}

/**
 * Creates a derived store from one or more Gun stores
 * 
 * @param stores The stores to derive from
 * @param fn Function that computes the derived value
 * @returns A readable derived store
 */
export function deriveFromGun<S extends Readable<any>[], T>(
  stores: S,
  fn: (values: Array<S[number] extends Readable<infer U> ? U : never>) => T
): Readable<T> {
  return derived(stores, fn);
}

/**
 * Creates a store that combines multiple Gun stores using a combining function
 * 
 * @param storeA First store
 * @param storeB Second store
 * @param combineFn Function to combine values from both stores
 * @returns A combined store
 */
export function combineStores<A, B, R>(
  storeA: GunStore<A>,
  storeB: GunStore<B>,
  combineFn: (a: A, b: B) => R
): GunStore<R> {
  // Create a combined subscription
  const combinedSubscription = storeA.subscription().combine(
    storeB.subscription(),
    combineFn
  );
  
  // Create a Svelte store
  const { subscribe } = writable<R>(undefined as unknown as R, (set) => {
    const cleanup = combinedSubscription.on(combined => {
      set(combined);
    });
    
    return cleanup;
  });
  
  // Return an enhanced store that includes node and subscription methods
  return { 
    subscribe,
    node: () => storeA.node(), // Use first store's node
    subscription: () => combinedSubscription
  };
}

/**
 * Creates a store that maps values from a Gun store
 * 
 * @param store The source store
 * @param mapFn Function to transform values
 * @returns A mapped store
 */
export function mapStore<T, R>(
  store: GunStore<T>,
  mapFn: (value: T) => R
): GunStore<R> {
  // Create a mapped subscription
  const mappedSubscription = store.subscription().map(mapFn);
  
  // Create a Svelte store
  const { subscribe } = writable<R>(undefined as unknown as R, (set) => {
    const cleanup = mappedSubscription.on(mapped => {
      set(mapped);
    });
    
    return cleanup;
  });
  
  return {
    subscribe,
    node: () => store.node(),
    subscription: () => mappedSubscription
  };
}

/**
 * Creates a store that filters values from a collection store
 * 
 * @param store The collection store
 * @param filterFn Function to filter items
 * @returns A filtered collection store
 */
export function filterCollectionStore<T extends CollectionItem>(
  store: CollectionStore<T>,
  filterFn: (item: T) => boolean
): GunStore<[string, T][]> {
  // Use derived for filtering
  const filtered = derived(store, ($items) => {
    return $items.filter(([_, item]) => filterFn(item));
  });
  
  // Return with GunStore interface
  return {
    subscribe: filtered.subscribe,
    node: () => store.node(),
    subscription: () => store.subscription().map(items => items.filter(([_, item]) => filterFn(item)))
  };
}

/**
 * Creates a store that aggregates values from a collection store
 * 
 * @param store The collection store
 * @param aggregateFn Function to compute the aggregate value
 * @returns A store with the aggregated value
 */
export function aggregateCollection<T extends CollectionItem, R>(
  store: CollectionStore<T>,
  aggregateFn: (items: [string, T][]) => R
): Readable<R> {
  return derived(store, aggregateFn);
}

/**
 * Creates a debounced store that only updates after a specified delay
 * 
 * @param store The source store
 * @param delayMs Delay in milliseconds
 * @returns A debounced store
 */
export function debounceStore<T>(
  store: GunStore<T>,
  delayMs: number
): GunStore<T> {
  // Create a debounced subscription
  const debouncedSubscription = store.subscription().debounce(delayMs);
  
  // Create a Svelte store
  const { subscribe } = writable<T>(undefined as unknown as T, (set) => {
    const cleanup = debouncedSubscription.on(value => {
      set(value);
    });
    
    return cleanup;
  });
  
  return {
    subscribe,
    node: () => store.node(),
    subscription: () => debouncedSubscription
  };
}

/**
 * Creates a store that uses switchMap for dynamic source switching
 * 
 * @param store The source store
 * @param projectFn Function that returns a new store based on the source value
 * @returns A dynamically switching store
 */
export function switchMapStore<T, R>(
  store: GunStore<T>,
  projectFn: (value: T) => GunStore<R>
): GunStore<R> {
  // Create a switchMap subscription
  const switchedSubscription = store.subscription().switchMap(value => 
    projectFn(value).subscription()
  );
  
  // Create a Svelte store
  const { subscribe } = writable<R>(undefined as unknown as R, (set) => {
    const cleanup = switchedSubscription.on(value => {
      set(value);
    });
    
    return cleanup;
  });
  
  return {
    subscribe,
    node: () => store.node(),
    subscription: () => switchedSubscription
  };
}

/**
 * Creates a store that combines the latest values from two stores when the first one emits
 * 
 * @param store The primary store
 * @param otherStore The secondary store to get latest values from
 * @param combineFn Function to combine values
 * @returns A combined store
 */
export function withLatestFromStore<A, B, R>(
  store: GunStore<A>,
  otherStore: GunStore<B>,
  combineFn: (a: A, b: B) => R
): GunStore<R> {
  // Create a withLatestFrom subscription
  const combinedSubscription = store.subscription().withLatestFrom(
    otherStore.subscription(),
    combineFn
  );
  
  // Create a Svelte store
  const { subscribe } = writable<R>(undefined as unknown as R, (set) => {
    const cleanup = combinedSubscription.on(combined => {
      set(combined);
    });
    
    return cleanup;
  });
  
  return {
    subscribe,
    node: () => store.node(),
    subscription: () => combinedSubscription
  };
}

/**
 * Creates a store for a nested Gun node structure
 * Automatically resolves references to a specified depth
 * 
 * @param path Path to the root node
 * @param depth How many levels of references to resolve
 * @returns A store with deeply resolved data
 */
export function createDeepStore<T>(
  path: string | string[],
  depth: number = 1
): GunStore<T> {
  // Create the Gun node
  const nodePath = typeof path === 'string' ? [path] : path;
  const node = new GunNode(nodePath);
  
  // Create a deep stream
  const deepSubscription = node.deepStream(depth);
  
  // Create a Svelte store
  const { subscribe } = writable<T>(undefined as unknown as T, (set) => {
    const cleanup = deepSubscription.on(value => {
      set(value as T);
    });
    
    return cleanup;
  });
  
  return {
    subscribe,
    node: () => node,
    subscription: () => deepSubscription
  };
}

/**
 * Creates an async derived store that handles dependencies reactively
 * and properly manages race conditions between async operations.
 * 
 * @param stores Array of stores this derived store depends on
 * @param fn Async function that calculates the derived value from the input stores
 * @param initialValue Initial value to use before first calculation completes
 * @returns A readable store that updates when any input store changes
 */
export function asyncDerivedStore<S extends Readable<any>[], T>(
  stores: S,
  fn: (values: { [K in keyof S]: S[K] extends Readable<infer U> ? U : never }) => Promise<T>,
  initialValue: T
): Readable<T> {
  // Create a store with the initial value
  const { subscribe, set } = writable<T>(initialValue);
  
  // Version tracking to handle race conditions between async computations
  let version = 0;
  
  // Function to update the store when dependencies change
  function update() {
    const currentVersion = ++version;
    
    // Get current values from all stores
    const storeValues = stores.map(store => get(store)) as { [K in keyof S]: S[K] extends Readable<infer U> ? U : never };
    
    // Process values through the provided function
    fn(storeValues)
      .then(result => {
        // Only update if this is still the latest calculation
        if (currentVersion === version) {
          set(result);
        }
      })
      .catch(err => {
        console.error('Error in async derived store:', err);
      });
  }
  
  // Set up subscriptions to all input stores
  let unsubscribers: Array<() => void> = [];
  let subscriberCount = 0;
  
  return {
    subscribe: (run, invalidate) => {
      // If this is the first subscriber, set up input store subscriptions
      if (subscriberCount === 0) {
        unsubscribers = stores.map(store => 
          store.subscribe(() => {
            update();
          })
        );
        
        // Run initial update
        update();
      }
      
      subscriberCount++;
      
      // Set up subscription to the internal store
      const unsubscribe = subscribe(run, invalidate);
      
      // Return cleanup function
      return () => {
        unsubscribe();
        subscriberCount--;
        
        // If no more subscribers, clean up input subscriptions
        if (subscriberCount === 0) {
          unsubscribers.forEach(unsub => unsub());
          unsubscribers = [];
        }
      };
    }
  };
} 