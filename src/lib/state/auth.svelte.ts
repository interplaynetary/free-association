/**
 * Authentication Module - V5 (Holster-Only)
 *
 * This module provides authentication using Holster.
 * Migrated from Gun/Holster dual support to Holster-only.
 *
 * UI components should import from this module for authentication.
 */

import type { Readable } from 'svelte/store';

// V5: Import from Holster (in v5 commons)
import {
	holsterUserAlias,
	holsterUserPub,
	isHolsterAuthenticating,
	holsterUsersList,
	login as holsterLogin,
	signup as holsterSignup,
	signout as holsterSignout
} from '$lib/commons/v5/holster.svelte';

// ============================================================================
// Exported Stores (V5: Holster-Only)
// ============================================================================

/**
 * Current user's alias (username)
 */
export const userAlias: Readable<string> = holsterUserAlias;

/**
 * Current user's public key
 */
export const userPub: Readable<string> = holsterUserPub;

/**
 * Authentication loading state
 */
export const isAuthenticating: Readable<boolean> = isHolsterAuthenticating;

/**
 * List of all users
 */
export const usersList = holsterUsersList;

// ============================================================================
// Exported Functions (V5: Holster-Only)
// ============================================================================

/**
 * Login with username and password
 */
export async function login(alias: string, password: string): Promise<void> {
	console.log('[AUTH-V5] Logging in with Holster');
	return holsterLogin(alias, password);
}

/**
 * Signup (create new account) with username and password
 */
export async function signup(alias: string, password: string): Promise<void> {
	console.log('[AUTH-V5] Signing up with Holster');
	return holsterSignup(alias, password);
}

/**
 * Sign out the current user
 */
export async function signout(): Promise<void> {
	console.log('[AUTH-V5] Signing out from Holster');
	return holsterSignout();
}

// ============================================================================
// Debugging
// ============================================================================

if (import.meta.env.DEV && typeof window !== 'undefined') {
	(window as any).authDebug = {
		system: 'Holster',
		checkAuth: () => {
			console.log('[AUTH-DEBUG] Current auth system: Holster (v5)');
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

	console.log('[AUTH-V5] Holster-only auth module loaded');
	console.log('[AUTH-V5] Debug utilities available: window.authDebug');
}
