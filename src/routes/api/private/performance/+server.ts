import {
  requestStats,
  dbStats,
  inviteCodes,
} from "$lib/server/holster/core"
import {createGETHandler} from "$lib/server/middleware/request-handler"

/**
 * GET /api/private/performance - Detailed performance metrics
 * 
 * @deprecated Consider using GET /api/health?services=holster,system for similar data
 */
export const GET = createGETHandler(
  async () => {
    const uptime = process.uptime()
    const memUsage = process.memoryUsage()

    return {
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
      caches: {
        inviteCodes: inviteCodes.size,
      },
      process: {
        pid: process.pid,
        version: process.version,
        platform: process.platform,
      },
    }
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false}
  }
)

