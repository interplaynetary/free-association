// Core reactive state and derived stores
export * from './core.svelte';

// Gun database and authentication
export * from './gun.svelte';

// Holster database (migration in progress)
// Export selectively to avoid conflicts with Gun exports
export {
	holster,
	holsterUser,
	holsterUserAlias,
	holsterUserPub,
	holsterUsersList,
	isHolsterAuthenticating,
	login as holsterLogin,
	signup as holsterSignup,
	signout as holsterSignout,
	changePassword as holsterChangePassword
} from './holster.svelte';

// Calculation and recalculation logic
export * from './calculations.svelte';

// Network subscriptions and mutual recognition
export * from './network.svelte';

// UI data providers
export * from '$lib/utils/ui-providers.svelte';

// Import subscriptions to activate them
import './subscriptions.svelte';
import './network.svelte'; // This activates the network subscriptions
import './holster.svelte'; // This initializes Holster (migration Phase 1)
