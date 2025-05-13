// Export centralized module with Gun persistence

// Export from tree.ts
export { GunUserTree, enterChild, exitToParent, updateNodePersistentCache } from './tree';

// Export from types.ts
export type {
	Node,
	TreeZipper,
	Ctx,
	NavigationPath,
	PersistentCache,
	ShareMap,
	Forest,
	Capacity,
	CapacityInventory,
	CapacityShare
} from './types';

// Export from cache.ts
export { emptyCache, cacheLookup, cacheInsert, withCacheM } from './cache';

// Export from calculations.ts
export { sharesOfGeneralFulfillmentMap, providerShares } from './calculations';

// Export from utils.ts
export { normalizeShareMap } from './utils';
