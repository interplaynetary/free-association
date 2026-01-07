import { writable, get } from 'svelte/store';
import * as holsterCore from '$lib/network/holster';
import type { AuthState, AuthCallbacks } from '$lib/network/holster';

// Re-export core Holster instances for backwards compatibility
export const holster = holsterCore.holster;
export const holsterUser = holsterCore.holsterUser;
export const holsterUsersList = holsterCore.holsterUsersList;

// ═══════════════════════════════════════════════════════════════════
// SVELTE REACTIVE STORES
// ═══════════════════════════════════════════════════════════════════

export const isHolsterAuthenticating = writable(true);
export const holsterUserAlias = writable('');
export const holsterUserPub = writable('');

// ═══════════════════════════════════════════════════════════════════
// STORE UPDATE HELPERS
// ═══════════════════════════════════════════════════════════════════

function updateStoresFromAuthState(authState: AuthState): void {
	holsterUserAlias.set(authState.alias);
	holsterUserPub.set(authState.pub);
}

function clearStores(): void {
	holsterUserAlias.set('');
	holsterUserPub.set('');
}

// ═══════════════════════════════════════════════════════════════════
// BROWSER INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

if (typeof window !== 'undefined' && !import.meta.env.VITEST) {
	console.log('[TRACE] src/lib/network/holster.svelte.ts: <module scope>');
	const checkAuth = async () => {
		console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: checkAuth');
		try {
			isHolsterAuthenticating.set(true);

			const authState = await holsterCore.recall({
				onSuccess: (state) => {
					console.log('[TRACE] [CALLBACK] src/lib/network/holster.svelte.ts: recall onSuccess');
					updateStoresFromAuthState(state)
				},
				onError: (error) => console.error('[HOLSTER RECALL] Error:', error)
			});

			if (authState.isAuthenticated) {
				updateStoresFromAuthState(authState);
			} else {
				clearStores();
			}
		} catch (error) {
			console.error('[HOLSTER RECALL] Error during authentication check:', error);
			clearStores();
		} finally {
			isHolsterAuthenticating.set(false);
		}
		console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: checkAuth');
	};

	checkAuth();
} else if (import.meta.env.VITEST) {
	isHolsterAuthenticating.set(false);
}

// ═══════════════════════════════════════════════════════════════════
// WRAPPED AUTH FUNCTIONS (with store updates)
// ═══════════════════════════════════════════════════════════════════

export async function login(alias: string, password: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: login', { alias });
	const authState = await holsterCore.login(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[HOLSTER LOGIN] Error:', error)
	});

	updateStoresFromAuthState(authState);
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: login');
}

export async function signup(alias: string, password: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: signup', { alias });
	const authState = await holsterCore.signup(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[HOLSTER SIGNUP] Error:', error)
	});

	updateStoresFromAuthState(authState);
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: signup');
}

export async function signout(): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: signout');
	await holsterCore.signout();
	clearStores();
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: signout');
}

export async function changePassword(currentPassword: string, newPassword: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: changePassword');
	await holsterCore.changePassword(currentPassword, newPassword);
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: changePassword');
}

// ═══════════════════════════════════════════════════════════════════
// TEST UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function mockAuth(pub: string, alias: string = 'test_user'): void {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: mockAuth');
	holsterCore.mockAuth(pub, alias);
	holsterUserPub.set(pub);
	holsterUserAlias.set(alias);
	isHolsterAuthenticating.set(false);
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: mockAuth');
}

export function clearAuth(): void {
	console.log('[TRACE] [ENTER] src/lib/network/holster.svelte.ts: clearAuth');
	holsterCore.clearAuth();
	holsterUserPub.set('');
	holsterUserAlias.set('');
	isHolsterAuthenticating.set(false);
	console.log('[TRACE] [EXIT] src/lib/network/holster.svelte.ts: clearAuth');
}

export function isAuthenticated(): boolean {
	return holsterCore.isAuthenticated();
}

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORTS (for backwards compatibility)
// ═══════════════════════════════════════════════════════════════════

export { NetworkError, AuthError } from './holster';