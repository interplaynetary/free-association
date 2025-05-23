/*
Adding a Svelte store for GUN data 
Luckily, we can add our own subscriber system in regular .js files by writing our own custom store. This approach is easy to work with, since you can easily add your own custom methods to write GUN data or even adding more gun.get() calls for nested data.

The store below is a simplified version of the official Svelte writable store. You can also check it out in the interactive demo
*/
// stores.js
import { gun } from './state.svelte';

// Define types for Gun references and data
type GunRef = {
	_: { get: string };
	on: (callback: (data: any, key: string) => void) => void;
	off: () => void;
	get: (key: string) => GunRef;
	set: (data: any) => any;
	put: (data: any) => any;
	map: () => GunRef;
};

type Subscriber<T> = (value: T) => void;
type Unsubscriber = () => void;

interface StoreWithMethods<T, M> {
	subscribe: (subscriber: Subscriber<T>) => Unsubscriber;
	[key: string]: any;
}

function customStore<T extends Record<string, any>, M>(
	ref: GunRef,
	methods: M = {} as M
): StoreWithMethods<T, M> {
	let store: T = {} as T;
	const subscribers: Array<Subscriber<T>> = [];

	// Add a listener to GUN data
	ref.on(function (data: any, key: string) {
		/* If the ref._get matches the data key it means we are getting
		 * data from a call to gun.get(), and so we don't need the store
		 * to be an object with nested data. Otherwise we are getting data
		 * from a call to map() and should nest the data in an object
		 */
		if (ref._.get === key) {
			store = data as T;
		} else if (!data) {
			/* This clause will not work as intended on null values / false / 0
			 * if you use such data consider subscribing to a parent node instead
			 * eg. gun.get("temperature") instead of gun.get("temperature").get("value").
			 * Or you can pass a validate() function (TODO: add example)
			 */
			delete store[key];
		} else {
			store[key] = data;
		}
		// Tell each subscriber that data has been updated
		for (let i = 0; i < subscribers.length; i += 1) {
			subscribers[i](store);
		}
	});

	function subscribe(subscriber: Subscriber<T>): Unsubscriber {
		subscribers.push(subscriber);

		// Publish initial value
		subscriber(store);

		// return cleanup function to be called on component dismount
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

interface Message {
	text: string;
	sender: string;
	icon: string;
}

interface MessageStore extends Record<string, Message> {}

interface MessageMethods {
	add: (text: string) => any;
	delete: (key: string) => any;
}

const ref = gun.get('messages');
export const messages = customStore<MessageStore, MessageMethods>(ref.map(), {
	add: (text: string) => ref.set({ text, sender: 'moi', icon: '😺' }),
	delete: (key: string) => ref.get(key).put(null)
});
/*
This pattern makes for very clean markup in the component itself

<script>
import { messages } from "./stores.js"
</script>

<input placeholder="write message" on:change={e => messages.add(e.target.value) && (e.target.value = "")} />

{#each Object.entries($messages) as [key, {sender, text, icon}] (key)}
<div style="padding: .4rem">
  {icon} {sender}: {text}
  <a href="#" on:click|preventDefault={() => messages.delete(key)}>delete</a>
</div>
{/each}
*/
