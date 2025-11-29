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
	const checkAuth = async () => {
		try {
			isHolsterAuthenticating.set(true);

			const authState = await holsterCore.recall({
				onSuccess: (state) => updateStoresFromAuthState(state),
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
	};

	checkAuth();
} else if (import.meta.env.VITEST) {
	isHolsterAuthenticating.set(false);
}

// ═══════════════════════════════════════════════════════════════════
// WRAPPED AUTH FUNCTIONS (with store updates)
// ═══════════════════════════════════════════════════════════════════

export async function login(alias: string, password: string): Promise<void> {
	const authState = await holsterCore.login(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[HOLSTER LOGIN] Error:', error)
	});

	updateStoresFromAuthState(authState);
}

export async function signup(alias: string, password: string): Promise<void> {
	const authState = await holsterCore.signup(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[HOLSTER SIGNUP] Error:', error)
	});

	updateStoresFromAuthState(authState);
}

export async function signout(): Promise<void> {
	await holsterCore.signout();
	clearStores();
}

export async function changePassword(currentPassword: string, newPassword: string): Promise<void> {
	await holsterCore.changePassword(currentPassword, newPassword);
}

// ═══════════════════════════════════════════════════════════════════
// TEST UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function mockAuth(pub: string, alias: string = 'test_user'): void {
	holsterCore.mockAuth(pub, alias);
	holsterUserPub.set(pub);
	holsterUserAlias.set(alias);
	isHolsterAuthenticating.set(false);
}

export function clearAuth(): void {
	holsterCore.clearAuth();
	holsterUserPub.set('');
	holsterUserAlias.set('');
	isHolsterAuthenticating.set(false);
}

export function isAuthenticated(): boolean {
	return holsterCore.isAuthenticated();
}

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORTS (for backwards compatibility)
// ═══════════════════════════════════════════════════════════════════

export { NetworkError, AuthError } from './holster';