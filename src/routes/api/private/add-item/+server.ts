import {error, json} from "@sveltejs/kit"
import type {RequestHandler} from "./$types"
import {addItemSchema} from "$lib/server/schemas/rsstream"
import {
  user,
  contentHashCache,
  pendingRequests,
  processingStats,
  HASH_CACHE_TTL,
} from "$lib/server/rsstream/holster"
import {createContentHash, mapEnclosure, mapCategory} from "$lib/server/rsstream/utils"
import {processItem} from "$lib/server/rsstream/feed-items"
import {day, updateRequestStats} from "$lib/server/rsstream/holster"
import {checkAuth} from "$lib/server/rsstream/auth"

export const POST: RequestHandler = async (event) => {
  const authError = checkAuth(event)
  if (authError) return authError

  const startTime = Date.now()

  const body = await event.request.json()
  const result = addItemSchema.safeParse(body)

  if (!result.success) {
    const firstError = result.error.errors[0]
    error(400, firstError.message)
  }

  const {url, guid, title, content, author, permalink, timestamp, enclosure, category} =
    result.data

  const now = Date.now()

  // Request deduplication - if same request is already processing
  const itemKey = `${url}_${guid}`
  if (pendingRequests.has(itemKey)) {
    return json({status: "processing", duplicate: true}, {status: 202})
  }

  // Mark as processing
  pendingRequests.set(itemKey, {startTime: now, res: null})

  if (!user.is) {
    pendingRequests.delete(itemKey)
    error(500, "Host error")
  }

  const twoWeeksAgo = Date.now() - 1209600000
  if (timestamp < twoWeeksAgo) {
    // Ignore items that are older than 2 weeks.
    pendingRequests.delete(itemKey)
    return json({status: "ignored", reason: "too old"})
  }

  const data = {
    title: title ?? "",
    content: content ?? "",
    author: author ?? "",
    permalink: permalink ?? "",
    guid,
    timestamp,
    url,
  }

  const mappedEnclosure = mapEnclosure(enclosure)
  const mappedCategory = mapCategory(category ?? [])
  if (mappedEnclosure) (data as any).enclosure = mappedEnclosure
  if (mappedCategory) (data as any).category = mappedCategory

  const dayKey = day(timestamp)

  // Content hash cache check - avoid processing if data hasn't changed
  const contentHash = createContentHash(data)
  const hashCacheKey = `${url}_${guid}`
  const cachedHash = contentHashCache.get(hashCacheKey)

  if (
    cachedHash &&
    cachedHash.hash === contentHash &&
    now - cachedHash.timestamp < HASH_CACHE_TTL
  ) {
    // Content hasn't changed, skip processing
    pendingRequests.delete(itemKey)
    return json({status: "unchanged", cached: true})
  }

  // Aggressive throttling to reduce system load
  const pendingDelay = pendingRequests.size * 200
  const systemDelay =
    processingStats.currentDelay > 0
      ? Math.min(processingStats.currentDelay, 10000)
      : 0
  const totalThrottle = Math.min(pendingDelay + systemDelay, 60000)

  if (totalThrottle > 0) {
    await new Promise(resolve => setTimeout(resolve, totalThrottle))
  }

  // Log when content hash has changed and requires processing
  if (cachedHash) {
    console.log(
      `[CONTENT-CHANGE] ${url} guid:${guid} - hash changed, queuing for processing`,
    )
  } else {
    console.log(`[CONTENT-NEW] ${url} guid:${guid} - new item, queuing for processing`)
  }

  // Update content hash cache for future change detection
  contentHashCache.set(hashCacheKey, {
    hash: contentHash,
    timestamp: now,
  })

  // Process item directly (no queue)
  const processResult = await processItem(data as any, dayKey, itemKey)

  // Clean up pending request
  pendingRequests.delete(itemKey)

  const processingTime = Date.now() - startTime

  // Update performance statistics
  updateRequestStats(processingTime)

  if (processingTime > 100) {
    console.log(`[SLOW] add-item request: ${processingTime}ms for ${url}`)
  }

  if (!processResult.success) {
    error(500, processResult.error || "Error processing item")
  }

  return json({status: "success"})
}

