/**
 * Authentication API Layer
 * 
 * Provides a clean, implementation-agnostic API for UI components.
 * Currently backed by Holster (via holster.svelte.ts).
 * 
 * This is a thin re-export layer with cleaner naming for convenience.
 */

// Re-export stores with cleaner names
export {
	holsterUserAlias as userAlias,
	holsterUserPub as userPub,
	isHolsterAuthenticating as isAuthenticating,
	holsterUsersList as usersList,
	holster,
	holsterUser,
	isAuthenticated
} from '$lib/network/holster.svelte';

// Re-export functions (no wrapper logic needed)
export { login, signup, signout, changePassword } from '$lib/network/holster.svelte';

// Re-export types and errors
export type { AuthState, AuthCallbacks } from '$lib/network/holster';
export { NetworkError, AuthError } from '$lib/network/holster.svelte';
