import Gun from 'gun/gun';

// ===== SVELTE COMPATIBILITY =====

// Extend Gun's type definitions for Svelte integration
declare module 'gun/gun' {
	interface IGunChain<T, TKey = any, TAck = any, TNode = any> {
		subscribe: (callback: (data: any) => void) => () => void;
	}

	interface _GunRoot {
		back?: _GunRoot;
		each?: any;
	}
}

// Add subscribe method to Gun for Svelte compatibility
// This is the "magic" approach from svelte-gun.md
// @ts-ignore - We're extending the Gun prototype
Gun.chain.subscribe = function (publish: (data: any) => void) {
	const gun = this;
	const at = gun._;
	// @ts-ignore - We've extended the _GunRoot type above
	const isMap = !!at && !!at.back && !!at.back.each;

	if (isMap) {
		const store = new Map();
		publish(Array.from(store));
		gun.on((data: any, _key: string, as: any) => {
			const key = _key || ((data || {})._ || {})['#'] || as.via.soul;
			if (data === null) {
				store.delete(key);
			} else {
				store.set(key, data);
			}
			publish(Array.from(store));
		});
	} else {
		gun.on((data: any) => publish(data));
	}

	return gun.off;
};

/**
 * STANDARD USAGE EXAMPLES
 *
 * In Svelte components, use the subscribe method directly on Gun chains:
 *
 * ```
 * <script>
 *   import { gun } from './gunSetup';
 *
 *   // Simple data binding
 *   let userData;
 *   const unsubscribe = gun.get('users').get('alice').subscribe(data => {
 *     userData = data;
 *   });
 *
 *   // Using with Svelte's $: syntax
 *   const userStore = gun.get('users').get('alice');
 *   $: console.log($userStore); // Reactive access
 *
 *   // Cleanup on component destroy
 *   onDestroy(unsubscribe);
 * </script>
 * ```
 *
 * For more complex data transformations, use customStore:
 *
 * ```
 * const filteredUsers = customStore(gun.get('users'), {
 *   filter: (criteria) => { ... } // Custom methods
 * });
 * ```
 */

/**
 * Helper function to initialize Gun in Svelte components
 * Automatically calls onDestroy to clean up
 */
export function initGunOnMount() {
	// Using custom Gun binding
	try {
		// @ts-ignore - Svelte component lifecycle methods
		const { onMount, onDestroy } = require('svelte');
		let cleanup: any = null;

		// Use onMount to initialize
		onMount(() => {
			// Your Gun initialization logic here
		});

		// Use onDestroy to clean up
		onDestroy(() => {
			if (cleanup) cleanup();
		});
	} catch (err) {
		console.error('Error initializing Gun:', err);
	}
}

// Custom store function for reusability (alternative approach)
export function customStore(ref: any, methods = {}) {
	let store: any = {};
	const subscribers: Array<(data: any) => void> = [];

	// Add a listener to GUN data
	ref.on(function (data: any, key: string) {
		if (ref._.get === key) {
			store = data;
		} else if (!data) {
			delete store[key];
			store = { ...store }; // Create a new reference to trigger Svelte reactivity
		} else {
			store[key] = data;
		}
		// Tell each subscriber that data has been updated
		for (let i = 0; i < subscribers.length; i += 1) {
			subscribers[i](store);
		}
	});

	function subscribe(subscriber: (data: any) => void) {
		subscribers.push(subscriber);

		// Publish initial value
		subscriber(store);

		// Return cleanup function to be called on component dismount
		return () => {
			const index = subscribers.indexOf(subscriber);
			if (index !== -1) {
				subscribers.splice(index, 1);
			}
			if (!subscribers.length) {
				ref.off();
			}
		};
	}

	return { ...methods, subscribe };
}
