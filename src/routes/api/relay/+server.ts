import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster/core"
import {createGETHandler} from "$lib/server/middleware/request-handler"

/**
 * Get statistics and information for all registered data relays
 *
 * GET /api/relay
 */
export const GET = createGETHandler(
  async () => {
    const registry = getRegistry(user)

    return {
      types: registry.getTypes(),
      stats: registry.getStats(),
    }
  },
  {
    requireAuth: true,
    authOptions: {allowBasic: true, allowJwt: false, allowApiKey: false}
  }
)

