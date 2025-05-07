import { Observable } from 'rxjs';

// Tree reference types
export interface TreeRef {
	playerId: string;
	treeId: string;
}

export function createTreeRef(playerId: string, treeId: string): TreeRef {
	return { playerId, treeId };
}

// Cache types
export type CacheValue =
	| { type: 'float'; value: number }
	| { type: 'int'; value: number }
	| { type: 'stringList'; value: string[] }
	| { type: 'shareMap'; value: ShareMap };

export interface Cache<K, V> {
	map: Map<K, V>;
	misses: number;
	hits: number;
}

// Share types
export type ShareMap = Map<string, number>;
export type CapacityShareMap = Map<string, CapacityShare>;

// Node and Tree types
export interface Node {
	id: string;
	name: string;
	points: number;
	children: Map<string, Node>;
	contributors: Set<TreeRef>;
	manualFulfillment?: number;
	capacities: Map<string, Capacity>;
	capacityShares: Map<string, CapacityShare>;
	localCache: Cache<string, CacheValue>;
}

export interface TreeZipper {
	current: Node;
	context?: Context;
}

export interface Context {
	parent: Node;
	siblings: Map<string, Node>;
	ancestors: Context[];
}

// Player level types
export interface ShareCache {
	providerShares: Map<string, ShareMap>; // key: ${providerId}:${depth}
	capacityShares: Map<string, CapacityShareMap>; // key: ${providerId}:${capacityId}
}

export interface Player {
	playerId: string;
	forest: Map<string, TreeZipper>;
	computedShares: ShareCache;
	mutualFulfillments: Cache<string, number>;
}

// Network API
export interface NetworkAPI {
	getProviderShares(providerId: string, depth: number): Promise<ShareMap>;
	getMutualFulfillment(ref: TreeRef): Promise<number>;
	getCapacityShares(capacityId: string): Promise<CapacityShareMap>;

	onSharesUpdated(providerId: string): Observable<ShareMap>;
	onFulfillmentUpdated(ref: TreeRef): Observable<number>;
}

// Capacity types
export interface Capacity {
	id: string;
	name: string;
	quantity: number;
	unit: string;
	shareDepth: number;
	expanded: boolean;
	hiddenUntilRequestAccepted: boolean;
}

export interface CapacityShare {
	targetCapacity: Capacity;
	sharePercentage: number;
	computedQuantity: number;
}

// Cache key generators
export const cacheKeys = {
	weight: (nodeId: string) => `${nodeId}_weight`,
	fulfillment: (nodeId: string) => `${nodeId}_fulfillment`,
	mutual: (ref1: TreeRef, ref2: TreeRef) => {
		const key1 = `${ref1.playerId}:${ref1.treeId}`;
		const key2 = `${ref2.playerId}:${ref2.treeId}`;
		return key1 < key2 ? `${key1}_mutual_${key2}` : `${key2}_mutual_${key1}`;
	},
	descendants: (nodeId: string) => `${nodeId}_descendants`,
	totalPoints: (nodeId: string) => `${nodeId}_total_points`,
	providerShares: (providerId: string, depth: number) => `${providerId}:${depth}`,
	capacityShares: (providerId: string, capacityId: string) => `${providerId}:${capacityId}`
};
