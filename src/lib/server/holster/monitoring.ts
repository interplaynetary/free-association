import {
  inviteCodes,
  requestStats,
  dbStats,
  resetStatsIfNeeded,
} from "./core"

// Cache cleanup is now handled by data-relay engines

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
      `[MONITOR] Caches: invites=${inviteCodes.size}`,
    )
    console.log(`[MONITOR] Uptime: ${Math.round(uptime)}s`)

    // Reset stats every hour
    resetStatsIfNeeded()
  }, 60000) // Every minute
}

/**
 * Initialize monitoring
 */
export function initializeMonitoring() {
  startPerformanceMonitoring()
}

