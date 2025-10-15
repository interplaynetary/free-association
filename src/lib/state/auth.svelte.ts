/**
 * Unified Authentication Module
 *
 * This module provides a single interface for authentication that works
 * with either Gun or Holster based on the USE_HOLSTER_AUTH flag.
 *
 * UI components should import from this module instead of directly from
 * gun.svelte.ts or holster.svelte.ts to ensure they work with both systems.
 */

import { USE_HOLSTER_AUTH } from '$lib/config';
import { derived, type Readable } from 'svelte/store';

// Import from both systems
import {
	userAlias as gunUserAlias,
	userPub as gunUserPub,
	isAuthenticating as gunIsAuthenticating,
	usersList as gunUsersList,
	login as gunLogin,
	signup as gunSignup,
	signout as gunSignout
} from './gun.svelte';

import {
	holsterUserAlias,
	holsterUserPub,
	isHolsterAuthenticating,
	holsterUsersList,
	login as holsterLogin,
	signup as holsterSignup,
	signout as holsterSignout
} from './holster.svelte';

// ============================================================================
// Unified Stores
// ============================================================================

/**
 * Current user's alias (username)
 * Returns the appropriate store based on USE_HOLSTER_AUTH flag
 */
export const userAlias: Readable<string> = derived(
	USE_HOLSTER_AUTH ? holsterUserAlias : gunUserAlias,
	($alias) => $alias
);

/**
 * Current user's public key
 * Returns the appropriate store based on USE_HOLSTER_AUTH flag
 */
export const userPub: Readable<string> = derived(
	USE_HOLSTER_AUTH ? holsterUserPub : gunUserPub,
	($pub) => $pub
);

/**
 * Authentication loading state
 * Returns the appropriate store based on USE_HOLSTER_AUTH flag
 */
export const isAuthenticating: Readable<boolean> = derived(
	USE_HOLSTER_AUTH ? isHolsterAuthenticating : gunIsAuthenticating,
	($auth) => $auth
);

// ============================================================================
// Unified Functions
// ============================================================================

/**
 * Login with username and password
 * Routes to the appropriate implementation based on USE_HOLSTER_AUTH flag
 */
export async function login(alias: string, password: string): Promise<void> {
	console.log(`[AUTH] Using ${USE_HOLSTER_AUTH ? 'Holster' : 'Gun'} for login`);

	if (USE_HOLSTER_AUTH) {
		return holsterLogin(alias, password);
	} else {
		return gunLogin(alias, password);
	}
}

/**
 * Signup (create new account) with username and password
 * Routes to the appropriate implementation based on USE_HOLSTER_AUTH flag
 */
export async function signup(alias: string, password: string): Promise<void> {
	console.log(`[AUTH] Using ${USE_HOLSTER_AUTH ? 'Holster' : 'Gun'} for signup`);

	if (USE_HOLSTER_AUTH) {
		return holsterSignup(alias, password);
	} else {
		return gunSignup(alias, password);
	}
}

/**
 * Sign out the current user
 * Routes to the appropriate implementation based on USE_HOLSTER_AUTH flag
 */
export async function signout(): Promise<void> {
	console.log(`[AUTH] Using ${USE_HOLSTER_AUTH ? 'Holster' : 'Gun'} for signout`);

	if (USE_HOLSTER_AUTH) {
		return holsterSignout();
	} else {
		return gunSignout();
	}
}

// ============================================================================
// Debugging
// ============================================================================

if (import.meta.env.DEV && typeof window !== 'undefined') {
	(window as any).authDebug = {
		system: USE_HOLSTER_AUTH ? 'Holster' : 'Gun',
		checkAuth: () => {
			console.log('[AUTH-DEBUG] Current auth system:', USE_HOLSTER_AUTH ? 'Holster' : 'Gun');
			console.log('[AUTH-DEBUG] userAlias store:', userAlias);
			console.log('[AUTH-DEBUG] userPub store:', userPub);
			console.log('[AUTH-DEBUG] isAuthenticating store:', isAuthenticating);

			// Subscribe to get current values
			const unsubAlias = userAlias.subscribe(val => {
				console.log('[AUTH-DEBUG] Current userAlias value:', val);
			});
			const unsubPub = userPub.subscribe(val => {
				console.log('[AUTH-DEBUG] Current userPub value:', val);
			});
			const unsubAuth = isAuthenticating.subscribe(val => {
				console.log('[AUTH-DEBUG] Current isAuthenticating value:', val);
			});

			// Clean up subscriptions
			setTimeout(() => {
				unsubAlias();
				unsubPub();
				unsubAuth();
			}, 100);
		}
	};

	console.log('[AUTH] Unified auth module loaded');
	console.log(`[AUTH] Using ${USE_HOLSTER_AUTH ? 'Holster' : 'Gun'} for authentication`);
	console.log('[AUTH] Debug utilities available: window.authDebug');
}
