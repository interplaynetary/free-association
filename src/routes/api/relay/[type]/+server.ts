import {json, error} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster/core"
import {checkAuth} from "$lib/server/holster/auth"

/**
 * Generic Data Relay Endpoint
 *
 * POST /api/relay/{type}
 *
 * Accepts data for any registered relay type and processes it through
 * the appropriate engine.
 *
 * Examples:
 * - POST /api/relay/rss-feed
 * - POST /api/relay/twitter
 * - POST /api/relay/webhook
 * - POST /api/relay/iot-sensor
 */
export const POST: RequestHandler = async (event) => {
  // Check for authentication (customize based on your needs)
  const authError = checkAuth(event)
  if (authError) return authError

  const {type} = event.params
  const data = await event.request.json()

  try {
    const registry = getRegistry(user)
    const result = await registry.process(type, data)

    if (!result.success) {
      // Map status to HTTP status codes
      const statusCode = {
        invalid: 400,
        duplicate: 202,
        age_filtered: 400,
        error: 500,
      }[result.status] || 500

      return json(
        {
          success: false,
          error: result.error,
          status: result.status,
        },
        {status: statusCode},
      )
    }

    return json({
      success: true,
      status: result.status, // "stored" | "unchanged"
    })
  } catch (err) {
    console.error(`[Relay:${type}] Unexpected error:`, err)
    error(500, "Internal server error")
  }
}

/**
 * Get statistics for a specific relay type
 *
 * GET /api/relay/{type}
 */
export const GET: RequestHandler = async (event) => {
  // Check for authentication
  const authError = checkAuth(event)
  if (authError) return authError

  const {type} = event.params

  const registry = getRegistry(user)
  const stats = registry.getEngineStats(type)

  if (!stats) {
    error(404, `Unknown relay type: ${type}`)
  }

  return json(stats)
}

