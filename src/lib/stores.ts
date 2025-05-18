import { writable } from 'svelte/store';

// User stores
export const username = writable<string>('');
export const userpub = writable<string>('');
