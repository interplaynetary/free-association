import {
  contentHashCache,
  pendingRequests,
  inviteCodes,
  removeDays,
  cleanupQueue,
  processingCleanup,
  processingStats,
  requestStats,
  dbStats,
  resetStatsIfNeeded,
  HASH_CACHE_TTL,
} from "./holster"

/**
 * Start periodic cache cleanup
 */
export function startCacheCleanup() {
  setInterval(() => {
    const now = Date.now()
    // Clean up expired content hash cache entries
    const hashExpiredKeys: string[] = []
    for (const [key, value] of contentHashCache.entries()) {
      if (now - value.timestamp > HASH_CACHE_TTL) {
        hashExpiredKeys.push(key)
      }
    }
    hashExpiredKeys.forEach(key => contentHashCache.delete(key))

    // Clean up stale pending requests (over 10 minutes old)
    const staleKeys: string[] = []
    for (const [key, value] of pendingRequests.entries()) {
      if (now - value.startTime > 600000) {
        staleKeys.push(key)
      }
    }
    staleKeys.forEach(key => pendingRequests.delete(key))

    if (hashExpiredKeys.length + staleKeys.length > 0) {
      console.log(
        `Cleaned up ${hashExpiredKeys.length} content hash entries, ${staleKeys.length} stale requests`,
      )
    }
  }, 60000) // Every minute
}

/**
 * Start memory and performance monitoring
 */
export function startPerformanceMonitoring() {
  setInterval(() => {
    const used = process.memoryUsage()
    const uptime = process.uptime()

    console.log(
      `[MONITOR] Memory: ${Math.round(used.rss / 1024 / 1024)}MB RSS, ${Math.round(used.heapUsed / 1024 / 1024)}MB Heap`,
    )
    console.log(
      `[MONITOR] Requests: ${requestStats.totalRequests} total, ${requestStats.slowRequests} slow (>100ms), avg: ${Math.round(requestStats.averageTime)}ms`,
    )
    console.log(
      `[MONITOR] Database: ${dbStats.totalOps} ops, ${dbStats.slowOps} slow (>1s), avg: ${Math.round(dbStats.averageDbTime)}ms, errors: ${dbStats.errorCount}`,
    )
    console.log(
      `[MONITOR] Processing: processed=${processingStats.totalProcessed}, avg=${Math.round(processingStats.averageProcessingTime)}ms, delay=${processingStats.currentDelay}ms, pending=${pendingRequests.size}`,
    )
    console.log(
      `[MONITOR] Caches: invites=${inviteCodes.size}, removeDays=${removeDays.size}, cleanup=${cleanupQueue.size}, processing=${processingCleanup.size}, contentHash=${contentHashCache.size}`,
    )
    console.log(`[MONITOR] Uptime: ${Math.round(uptime)}s`)

    // Reset stats every hour
    resetStatsIfNeeded()
  }, 60000) // Every minute
}

/**
 * Initialize all monitoring tasks
 */
export function initializeMonitoring() {
  startCacheCleanup()
  startPerformanceMonitoring()
}

