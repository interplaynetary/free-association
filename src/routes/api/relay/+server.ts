import {json} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/rsstream/holster"
import {checkAuth} from "$lib/server/rsstream/auth"

/**
 * Get statistics and information for all registered data relays
 *
 * GET /api/relay
 */
export const GET: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const registry = getRegistry(user)

  return json({
    types: registry.getTypes(),
    stats: registry.getStats(),
  })
}

