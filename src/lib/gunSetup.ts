import Gun from 'gun';
import 'gun/sea';
import 'gun/axe';
import { writable } from 'svelte/store';
import { userTree, healTree } from './state.svelte';
import { createRootNode } from './protocol/protocol';
import { manifest } from './state.svelte';

// Database
export const gun = Gun();

export const usersList = gun.get('users');

export let user = gun.user().recall({ sessionStorage: true });

// Current User's username
export const username = writable('');
export const userpub = writable('');

gun.on('auth', async () => {
	const alias = await user.get('alias'); // username string
	username.set(alias);
	userpub.set(user.is?.pub);
	usersList.get(user.is?.pub).put({
		name: alias,
		lastSeen: Date.now()
	});

	console.log(`signed in as ${alias}`);

	// Load existing user data
	manifest();

	// Check if user has a tree, and if not, initialize one
	user.get('tree').once((treeData: any) => {
		// Use the healTree function from state.ts
		const healedTree = healTree(treeData);

		if (!healedTree && user.is?.pub) {
			// No tree exists yet and healing couldn't help, create a new one
			console.log('No tree found, creating initial tree for user');
			const newTree = createRootNode(user.is.pub, alias, user.is.pub);
			const jsonTree = JSON.stringify(newTree);
			console.log('Storing new tree as JSON string:', jsonTree);
			user.get('tree').put(jsonTree);
			userTree.set(newTree);
		}
	});
});

export function login(username: string, password: string) {
	user.auth(username, password, ({ err }: { err: any }) => err && alert(err));
}

export function signup(username: string, password: string) {
	user.create(username, password, ({ err }: { err: any }) => {
		if (err) {
			alert(err);
		} else {
			login(username, password);
		}
	});
}

export function signout() {
	user.leave();
	username.set('');
	userpub.set('');
}