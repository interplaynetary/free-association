import {json, error} from "@sveltejs/kit"
import type {RequestHandler} from "@sveltejs/kit"
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster/core"
import {createGETHandler} from "$lib/server/middleware/request-handler"

/**
 * Get subscription information for a relay type
 * 
 * GET /api/relay/{type}/subscriptions
 * 
 * Returns:
 * - Whether subscriptions are supported
 * - Subscription stats
 * - Schema information
 */
export const GET = createGETHandler(
  async ({event}) => {
    const {type} = event.params

    const registry = getRegistry(user)
    const engine = registry.get(type)

    if (!engine) {
      error(404, `Unknown relay type: ${type}`)
    }

    const subscriptionManager = engine.getSubscriptionManager()
    
    if (!subscriptionManager) {
      return {
        supported: false,
        type,
      }
    }

    const stats = await subscriptionManager.getStats()

    return {
      supported: true,
      type,
      stats,
    }
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false}
  }
)

