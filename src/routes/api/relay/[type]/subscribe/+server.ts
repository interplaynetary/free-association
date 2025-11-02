import {json, error} from "@sveltejs/kit"
import type {RequestHandler} from "@sveltejs/kit"
import {getRegistry} from "$lib/server/data-relay"
import {user} from "$lib/server/holster/core"
import {holsterNext} from "$lib/server/holster/db"
import {requireAuthEvent} from "$lib/server/middleware/unified-auth"

/**
 * Subscribe to a resource for a specific relay type
 * 
 * POST /api/relay/{type}/subscribe
 * 
 * Body depends on relay type's subscription schema
 * Typically: { code: string, url: string, ... }
 */
export const POST: RequestHandler = async (event) => {
  // Require authentication
  const auth = requireAuthEvent(event, {allowBasic: true, allowJwt: false, allowApiKey: false})
  
  const {type} = event.params
  const data = await event.request.json()

  // Validate account code
  if (!data.code) {
    return json({error: "Account code required"}, {status: 400})
  }

  const accountCode = data.code

  // Verify account exists and user has access
  if (!user.is) {
    error(500, "Host error")
  }

  const account = await holsterNext("accounts", accountCode)
  if (!account) {
    return json({error: "Account not found"}, {status: 404})
  }

  try {
    const registry = getRegistry(user)
    
    // Check if type supports subscriptions
    if (!registry.supportsSubscriptions(type)) {
      return json(
        {
          error: `Relay type "${type}" does not support subscriptions`,
        },
        {status: 400},
      )
    }

    // Process subscription
    const result = await registry.subscribe(type, data, accountCode)

    if (!result.success) {
      // Determine appropriate status code
      const statusCode = result.error?.includes("limit") ? 403 : 400
      
      return json(
        {
          success: false,
          error: result.error,
        },
        {status: statusCode},
      )
    }

    return json({
      success: true,
      data: result.data,
    })
  } catch (err) {
    console.error(`[Relay:${type}] Subscription error:`, err)
    error(500, "Subscription failed")
  }
}

