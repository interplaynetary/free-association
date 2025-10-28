import {json, error} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {user} from "$lib/server/holster/core"
import {checkAuth} from "$lib/server/holster/auth"
import {z} from "zod"

const querySchema = z.object({
  collection: z.string().optional(),
  resourceId: z.string().optional(),
  timeKey: z.number().optional(),
  limit: z.number().int().positive().max(100).optional().default(20),
  offset: z.number().int().nonnegative().optional().default(0),
})

/**
 * Query data from a specific relay type
 *
 * GET /api/relay/{type}/query?resourceId=...&limit=20
 *
 * This allows browsing stored data generically
 */
export const GET: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const {type} = event.params
  const params = Object.fromEntries(event.url.searchParams.entries())
  
  // Parse query parameters
  const parsedParams = querySchema.safeParse({
    ...params,
    limit: params.limit ? parseInt(params.limit) : undefined,
    offset: params.offset ? parseInt(params.offset) : undefined,
    timeKey: params.timeKey ? parseInt(params.timeKey) : undefined,
  })

  if (!parsedParams.success) {
    error(400, parsedParams.error.message)
  }

  const {resourceId, timeKey, limit, offset} = parsedParams.data

  try {
    // Determine collection name based on relay type
    const collections: Record<string, string> = {
      "rss-feed": "feedItems",
      twitter: "tweets",
      mastodon: "mastodonPosts",
      reddit: "redditPosts",
      webhook: "webhookEvents",
      "iot-sensor": "sensorData",
      email: "emails",
      logs: "logs",
      "json-document": "documents",
    }

    const collection = collections[type]
    if (!collection) {
      error(404, `Unknown relay type: ${type}`)
    }

    // Build query path
    let chain = user.get(collection)

    if (resourceId) {
      chain = chain.next(resourceId)
    }

    if (timeKey) {
      chain = chain.next(timeKey)
    }

    // Fetch data
    const data = await new Promise(resolve => {
      chain.once((data: any) => {
        if (!data) {
          resolve([])
          return
        }

        // Convert Gun object to array
        const items: any[] = []
        for (const [key, value] of Object.entries(data)) {
          if (key !== "_" && value && typeof value === "object") {
            items.push({...value, _key: key})
          }
        }

        resolve(items)
      })
    })

    // Apply pagination
    const items = (data as any[])
      .slice(offset, offset + limit)

    return json({
      type,
      collection,
      resourceId,
      timeKey,
      items,
      count: items.length,
      offset,
      limit,
      hasMore: (data as any[]).length > offset + limit,
    })
  } catch (err) {
    console.error(`Error querying ${type}:`, err)
    error(500, "Query failed")
  }
}

