import { writable } from 'svelte/store';
import * as meshCore from '$lib/network/mesh';
import type { AuthState } from '$lib/network/mesh';

// Re-export core Mesh instances for backwards compatibility
export const mesh = meshCore.mesh;
export const meshUser = meshCore.meshUser;
export const meshUsersList = meshCore.meshUsersList;

// ═══════════════════════════════════════════════════════════════════
// SVELTE REACTIVE STORES
// ═══════════════════════════════════════════════════════════════════

export const isMeshAuthenticating = writable(true);
export const meshUserAlias = writable('');
export const meshUserPub = writable('');

// ═══════════════════════════════════════════════════════════════════
// STORE UPDATE HELPERS
// ═══════════════════════════════════════════════════════════════════

function updateStoresFromAuthState(authState: AuthState): void {
	meshUserAlias.set(authState.alias);
	meshUserPub.set(authState.pub);
}

function clearStores(): void {
	meshUserAlias.set('');
	meshUserPub.set('');
}

// ═══════════════════════════════════════════════════════════════════
// BROWSER INITIALIZATION
// ═══════════════════════════════════════════════════════════════════

/**
 * Initialize Mesh Authentication
 * 
 * Starts the authentication check process. This should be called once at startup.
 * Returns a cleanup function (though traditionally auth listeners persist).
 */
/**
 * Initialize Mesh Authentication
 * 
 * Starts the authentication check process. This should be called once at startup.
 * Returns a cleanup function (though traditionally auth listeners persist).
 */
export async function initializeAuth(): Promise<() => void> {
	if (typeof window === 'undefined' || import.meta.env.VITEST) {
		isMeshAuthenticating.set(false);
		return () => { };
	}

	console.log('[TRACE] src/lib/network/mesh.svelte.ts: initializeAuth');

	const checkAuth = async () => {
		console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: checkAuth');
		try {
			isMeshAuthenticating.set(true);

			// Ensure Mesh is initialized before using it
			await meshCore.initMesh();

			const authState = await meshCore.recall({
				onSuccess: (state) => {
					console.log('[TRACE] [CALLBACK] src/lib/network/mesh.svelte.ts: recall onSuccess');
					updateStoresFromAuthState(state)
				},
				onError: (error) => console.error('[MESH RECALL] Error:', error)
			});

			if (authState.isAuthenticated) {
				updateStoresFromAuthState(authState);
			} else {
				clearStores();
			}
		} catch (error) {
			console.error('[MESH RECALL] Error during authentication check:', error);
			clearStores();
		} finally {
			isMeshAuthenticating.set(false);
		}
		console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: checkAuth');
	};

	await checkAuth();

	// In the future, if meshCore has a listener, we would subscribe here.
	// For now, recall() is a one-time check, so we essentially just run it.
	return () => {
		// Cleanup logic if needed (e.g. stop listeners)
	};
}

// ═══════════════════════════════════════════════════════════════════
// WRAPPED AUTH FUNCTIONS (with store updates)
// ═══════════════════════════════════════════════════════════════════

export async function login(alias: string, password: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: login', { alias });
	const authState = await meshCore.login(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[MESH LOGIN] Error:', error)
	});

	updateStoresFromAuthState(authState);
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: login');
}

export async function signup(alias: string, password: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: signup', { alias });
	const authState = await meshCore.signup(alias, password, {
		onSuccess: (state) => updateStoresFromAuthState(state),
		onError: (error) => console.error('[MESH SIGNUP] Error:', error)
	});

	updateStoresFromAuthState(authState);
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: signup');
}

export async function signout(): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: signout');
	await meshCore.signout();
	clearStores();
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: signout');
}

export async function changePassword(currentPassword: string, newPassword: string): Promise<void> {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: changePassword');
	await meshCore.changePassword(currentPassword, newPassword);
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: changePassword');
}

// ═══════════════════════════════════════════════════════════════════
// TEST UTILITIES
// ═══════════════════════════════════════════════════════════════════

export function mockAuth(pub: string, alias: string = 'test_user'): void {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: mockAuth');
	meshCore.mockAuth(pub, alias);
	meshUserPub.set(pub);
	meshUserAlias.set(alias);
	isMeshAuthenticating.set(false);
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: mockAuth');
}

export function clearAuth(): void {
	console.log('[TRACE] [ENTER] src/lib/network/mesh.svelte.ts: clearAuth');
	meshCore.clearAuth();
	meshUserPub.set('');
	meshUserAlias.set('');
	isMeshAuthenticating.set(false);
	console.log('[TRACE] [EXIT] src/lib/network/mesh.svelte.ts: clearAuth');
}

export function isAuthenticated(): boolean {
	return meshCore.isAuthenticated();
}

// ═══════════════════════════════════════════════════════════════════
// RE-EXPORTS (for backwards compatibility)
// ═══════════════════════════════════════════════════════════════════

export { NetworkError, AuthError } from './mesh';