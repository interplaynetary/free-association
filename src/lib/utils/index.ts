/**
 * Core Gun Utilities
 * This file re-exports clean Gun utilities without application-specific code
 */

// Re-export core utilities from the Gun setup
export { gun } from '../gun/gunSetup';
export type { SubscriptionHandler, SubscriptionCleanup } from '../gun/gunSetup';

// Re-export node and subscription classes
export { GunNode } from '../gun/GunNode';
export { GunSubscription } from '../gun/GunSubscription';

// Re-export reactive stores
export {
	createGunStore,
	createCollectionStore,
	deriveFromGun,
	combineStores,
	mapStore,
	filterCollectionStore,
	aggregateCollection,
	debounceStore,
	switchMapStore,
	withLatestFromStore,
	createDeepStore
} from './svelte/reactiveStores';

// Re-export user authentication utilities
export { user, recallUser, authenticate, logout } from '../gun/gunSetup';

// Re-export connection management utilities
export {
	connectionStatus,
	reconnectToPeers,
	initConnectionManager,
	checkConnectionStatus
} from '../gun/old/connectionManager';
