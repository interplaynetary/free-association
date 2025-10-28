import {json} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {requestStats} from "$lib/server/rsstream/holster"

export const GET: RequestHandler = async () => {
  const uptime = process.uptime()
  const memUsage = process.memoryUsage()

  return json({
    status: "ok",
    uptime: Math.round(uptime),
    memory: Math.round(memUsage.rss / 1024 / 1024),
    requests: requestStats.totalRequests,
    timestamp: Date.now(),
  })
}

