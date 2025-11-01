import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster/core"
import {createGETHandler} from "$lib/server/middleware/request-handler"

/**
 * Get aggregated statistics across all relay types
 *
 * GET /api/relay/stats
 */
export const GET = createGETHandler(
  async () => {
    const registry = getRegistry(user)
    const allStats = registry.getStats()
    const types = registry.getTypes()

    // Calculate totals
    const totals = {
      totalRequests: 0,
      totalProcessed: 0,
      totalDuplicates: 0,
      totalUnchanged: 0,
      totalAgeFiltered: 0,
      totalPending: 0,
      totalCachedHashes: 0,
    }

    for (const stats of Object.values(allStats)) {
      totals.totalRequests += stats.requests.totalRequests
      totals.totalProcessed += stats.processing.totalProcessed
      totals.totalDuplicates += stats.requests.duplicates || 0
      totals.totalUnchanged += stats.requests.unchanged || 0
      totals.totalAgeFiltered += stats.requests.ageFiltered || 0
      totals.totalPending += stats.caches.pending || 0
      totals.totalCachedHashes += stats.caches.contentHash || 0
    }

    return {
      types,
      totals,
      byType: allStats,
      timestamp: Date.now(),
    }
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false}
  }
)

