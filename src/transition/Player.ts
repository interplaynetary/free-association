import { BehaviorSubject, Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Player, TreeZipper, ShareMap, CapacityShareMap, NetworkAPI, ShareCache } from './core';
import { createCache, cacheLookup, cacheInsert } from './Cache';
import { cacheKeys } from './core';

export class PlayerImpl implements Player, NetworkAPI {
	private shareUpdates = new BehaviorSubject<ShareCache>({
		providerShares: new Map(),
		capacityShares: new Map()
	});

	constructor(
		public readonly playerId: string,
		public trees: Map<string, TreeZipper> = new Map(),
		public computedShares: ShareCache = {
			providerShares: new Map(),
			capacityShares: new Map()
		},
		public mutualFulfillments = createCache<string, number>()
	) {}

	// Tree management
	addTree(zipper: TreeZipper): void {
		this.trees.set(zipper.current.id, zipper);
	}

	getTree(treeId: string): TreeZipper | undefined {
		return this.trees.get(treeId);
	}

	// Share computation and caching
	private computeAndCacheProviderShares(providerId: string, depth: number): ShareMap {
		const tree = this.trees.get(providerId);
		if (!tree) {
			return new Map();
		}

		// Here we would implement the provider shares calculation
		// This is a placeholder that would use the actual calculation logic
		const shares = new Map<string, number>();

		const key = cacheKeys.providerShares(providerId, depth);
		this.computedShares.providerShares.set(key, shares);
		this.shareUpdates.next(this.computedShares);

		return shares;
	}

	private computeAndCacheMutualFulfillment(nodeId1: string, nodeId2: string): number {
		const key = cacheKeys.mutual(nodeId1, nodeId2);
		const cached = cacheLookup(key, this.mutualFulfillments);

		if (cached) {
			return cached.value;
		}

		// Here we would implement the mutual fulfillment calculation
		// This is a placeholder that would use the actual calculation logic
		const fulfillment = 0;

		this.mutualFulfillments = cacheInsert(key, fulfillment, this.mutualFulfillments);
		return fulfillment;
	}

	// NetworkAPI implementation
	async getProviderShares(providerId: string, depth: number): Promise<ShareMap> {
		const key = cacheKeys.providerShares(providerId, depth);
		const cached = this.computedShares.providerShares.get(key);

		if (cached) {
			return cached;
		}

		return this.computeAndCacheProviderShares(providerId, depth);
	}

	async getMutualFulfillment(nodeId: string): Promise<number> {
		return this.computeAndCacheMutualFulfillment(this.playerId, nodeId);
	}

	async getCapacityShares(capacityId: string): Promise<CapacityShareMap> {
		const key = cacheKeys.capacityShares(this.playerId, capacityId);
		return this.computedShares.capacityShares.get(key) || new Map();
	}

	onSharesUpdated(providerId: string): Observable<ShareMap> {
		return this.shareUpdates.pipe(
			map((cache) => {
				const shares = Array.from(cache.providerShares.entries())
					.filter(([key]) => key.startsWith(`${providerId}:`))
					.map(([, value]) => value);

				return shares.length > 0 ? shares[0] : new Map();
			})
		);
	}

	onFulfillmentUpdated(nodeId: string): Observable<number> {
		// This would be implemented to notify of fulfillment changes
		// For now, we return a default value
		return new BehaviorSubject(0);
	}
}
