import {json} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {
  requestStats,
  dbStats,
  processingStats,
  inviteCodes,
  removeDays,
  cleanupQueue,
  contentHashCache,
  pendingRequests,
} from "$lib/server/rsstream/holster"
import {checkAuth} from "$lib/server/rsstream/auth"

export const GET: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const uptime = process.uptime()
  const memUsage = process.memoryUsage()

  const stats = {
    timestamp: new Date().toISOString(),
    uptime: Math.round(uptime),
    memory: {
      rss: Math.round(memUsage.rss / 1024 / 1024),
      heapUsed: Math.round(memUsage.heapUsed / 1024 / 1024),
      heapTotal: Math.round(memUsage.heapTotal / 1024 / 1024),
      external: Math.round(memUsage.external / 1024 / 1024),
    },
    requests: {
      ...requestStats,
      slowRequestPercentage:
        requestStats.totalRequests > 0
          ? Math.round(
              (requestStats.slowRequests / requestStats.totalRequests) * 100,
            )
          : 0,
    },
    database: {
      ...dbStats,
      slowDbPercentage:
        dbStats.totalOps > 0
          ? Math.round((dbStats.slowOps / dbStats.totalOps) * 100)
          : 0,
      errorPercentage:
        dbStats.totalOps > 0
          ? Math.round((dbStats.errorCount / dbStats.totalOps) * 100)
          : 0,
    },
    processing: {
      ...processingStats,
      pendingRequests: pendingRequests.size,
    },
    caches: {
      inviteCodes: inviteCodes.size,
      removeDays: removeDays.size,
      cleanupQueue: cleanupQueue.size,
      contentHash: contentHashCache.size,
    },
    process: {
      pid: process.pid,
      version: process.version,
      platform: process.platform,
    },
  }

  return json(stats)
}

