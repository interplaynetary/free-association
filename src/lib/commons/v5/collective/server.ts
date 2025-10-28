import Holster from '@mblaney/holster/src/holster.js';
import { writable, get } from 'svelte/store';
import { config } from './config';

// Initialize Holster - now uses config from environment variables
export const holster = Holster({
	peers: config.holster.peers,
	indexedDB: config.holster.indexedDB,
	file: config.holster.file
});

// Authentication state store
export const isHolsterAuthenticating = writable(true);

// Initialize Holster user immediately to avoid reference errors
// Named holsterUser to distinguish from Gun's user export
export const holsterUser = holster.user();

// Current User's alias and pub for Holster
export const holsterUserAlias = writable('');
export const holsterUserPub = writable('');
