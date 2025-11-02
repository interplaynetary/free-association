import {z} from "zod"
import type {DataRelayConfig} from "../config"
import {buildFlatPath, DEFAULT_RETENTION, DEFAULT_THROTTLING} from "../config"
import {buildSimpleSubscriptionConfig} from "../subscription-helpers"
import {holsterVerify} from "$lib/server/holster/db"
import {config} from "$lib/server/config"

// Type for Gun/Holster user instance
type User = any

// ============================================================================
// RSS Feed Schema
// ============================================================================

const enclosureSchema = z.object({
  photo: z
    .array(
      z.object({
        link: z.string().optional(),
        alt: z.string().optional(),
      }),
    )
    .optional(),
  audio: z.array(z.string()).optional(),
  video: z.array(z.string()).optional(),
})

const rssFeedItemInputSchema = z.object({
  url: z.string().min(1, "Feed URL required"),
  guid: z.string().min(1, "GUID required"),
  timestamp: z.number().int().positive(),
  title: z.string().optional(),
  content: z.string().optional(),
  author: z.string().optional(),
  permalink: z.string().optional(),
  enclosure: enclosureSchema.optional(),
  category: z.array(z.string()).optional(),
})

type RSSFeedItemInput = z.infer<typeof rssFeedItemInputSchema>

interface RSSFeedItemStored {
  url: string
  guid: string
  timestamp: number
  title: string
  content: string
  author: string
  permalink: string
  enclosure?: {
    photo?: Record<string, string>
    audio?: Record<string, boolean>
    video?: Record<string, boolean>
  }
  category?: Record<string, boolean>
}

// ============================================================================
// Data Mappers
// ============================================================================

function mapEnclosure(e: z.infer<typeof enclosureSchema> | undefined): RSSFeedItemStored["enclosure"] | undefined {
  if (!e) return undefined

  let found = false
  const enclosure: RSSFeedItemStored["enclosure"] = {}

  if (e.photo?.length) {
    enclosure.photo = {}
    for (const p of e.photo) {
      if (p?.link) {
        found = true
        enclosure.photo[p.link] = p.alt || ""
      }
    }
  }

  if (e.audio?.length) {
    enclosure.audio = {}
    for (const a of e.audio) {
      if (a) {
        found = true
        enclosure.audio[a] = true
      }
    }
  }

  if (e.video?.length) {
    enclosure.video = {}
    for (const v of e.video) {
      if (v) {
        found = true
        enclosure.video[v] = true
      }
    }
  }

  return found ? enclosure : undefined
}

function mapCategory(c: string[]): Record<string, boolean> | undefined {
  if (!c?.length) return undefined

  let found = false
  const category: Record<string, boolean> = {}
  for (const value of c) {
    if (value) {
      found = true
      category[value] = true
    }
  }
  return found ? category : undefined
}

// ============================================================================
// RSS Feed Configuration
// ============================================================================

export const rssFeedConfig: DataRelayConfig<RSSFeedItemInput, RSSFeedItemStored> = {
  type: "rss-feed",
  displayName: "RSS Feed",

  inputSchema: rssFeedItemInputSchema,

  storage: {
    collection: "feedItems",

    getResourceId: data => data.url,
    getItemId: data => data.guid,
    getTimestamp: data => data.timestamp,

    timeGrouping: "day",

    buildPath: (user: User, resourceId: string, timeKey: number | null, itemId: string) => {
      return buildFlatPath(user, "feedItems", resourceId, timeKey, itemId)
    },
  },

  deduplication: {
    buildKey: data => `${data.url}_${data.guid}`,
    hashFields: ["title", "content", "author", "permalink"],
    cacheTTL: 1209600000, // 2 weeks
  },

  retention: {
    ...DEFAULT_RETENTION,
    maxAge: 1209600000, // 2 weeks
  },

  transform: {
    toStorage: (data: RSSFeedItemInput): RSSFeedItemStored => {
      const stored: RSSFeedItemStored = {
        url: data.url,
        guid: data.guid,
        timestamp: data.timestamp,
        title: data.title ?? "",
        content: data.content ?? "",
        author: data.author ?? "",
        permalink: data.permalink ?? "",
      }

      const enclosure = mapEnclosure(data.enclosure)
      if (enclosure) stored.enclosure = enclosure

      const category = mapCategory(data.category ?? [])
      if (category) stored.category = category

      return stored
    },
  },

  metadata: {
    collection: "feeds",
    getKey: data => data.url,
    onItemAdd: async (user: User, feedUrl: string, currentMetadata: any) => {
      // Metadata is now managed through subscription system
      return null
    },
  },

  subscription: buildSimpleSubscriptionConfig("rss-feed", "feeds", {
    required: true,
    countField: "subscribed",
    limitField: "feeds",
    verifyResourceId: async (resourceId: string, account: any) => {
      // Verify signed URL
      return await holsterVerify(resourceId, account)
    },
    fetchMetadata: async (feedUrl: string) => {
      // Fetch feed metadata from external RSS service
      const addFeedUrl = config.addFeedUrl
      const addFeedID = config.addFeedId
      const addFeedApiKey = config.addFeedApiKey

      if (!addFeedUrl || !addFeedID || !addFeedApiKey) {
        console.error("RSS feed service not configured")
        return null
      }

      try {
        const response = await fetch(addFeedUrl, {
          method: "POST",
          headers: {
            "Content-Type": "application/x-www-form-urlencoded",
          },
          body: `id=${addFeedID}&key=${addFeedApiKey}&action=add-feed&xmlUrl=${encodeURIComponent(feedUrl)}`,
        })

        if (!response.ok) {
          console.error("Failed to fetch feed metadata:", response.statusText)
          return null
        }

        const result = await response.json()
        if (result.error || !result.add) {
          console.error("Invalid feed metadata response:", result.error)
          return null
        }

        return {
          title: result.add.title,
          description: result.add.description ?? "",
          html_url: result.add.html_url ?? "",
          language: result.add.language ?? "",
          image: result.add.image ?? "",
        }
      } catch (error) {
        console.error("Error fetching feed metadata:", error)
        return null
      }
    },
    externalService: {
      subscribe: async (feedUrl: string) => {
        const addFeedUrl = config.addFeedUrl
        const addFeedID = config.addFeedId
        const addFeedApiKey = config.addFeedApiKey

        if (!addFeedUrl || !addFeedID || !addFeedApiKey) {
          return {
            success: false,
            error: "RSS feed service not configured",
          }
        }

        try {
          const response = await fetch(addFeedUrl, {
            method: "POST",
            headers: {
              "Content-Type": "application/x-www-form-urlencoded",
            },
            body: `id=${addFeedID}&key=${addFeedApiKey}&action=add-feed&xmlUrl=${encodeURIComponent(feedUrl)}`,
          })

          if (!response.ok) {
            return {
              success: false,
              error: "External RSS service error",
            }
          }

          return {success: true}
        } catch (error) {
          return {
            success: false,
            error: "Failed to contact RSS service",
          }
        }
      },
      unsubscribe: async (feedUrl: string) => {
        const addFeedUrl = config.addFeedUrl
        const addFeedID = config.addFeedId
        const addFeedApiKey = config.addFeedApiKey

        if (!addFeedUrl || !addFeedID || !addFeedApiKey) {
          return {
            success: false,
            error: "RSS feed service not configured",
          }
        }

        try {
          const response = await fetch(addFeedUrl, {
            method: "POST",
            headers: {
              "Content-Type": "application/x-www-form-urlencoded",
            },
            body: `id=${addFeedID}&key=${addFeedApiKey}&action=remove-feed&xmlUrl=${encodeURIComponent(feedUrl)}`,
          })

          if (!response.ok) {
            return {
              success: false,
              error: "External RSS service error",
            }
          }

          return {success: true}
        } catch (error) {
          return {
            success: false,
            error: "Failed to contact RSS service",
          }
        }
      },
    },
  }),

  throttling: DEFAULT_THROTTLING,

  ageFilter: {
    maxItemAge: 1209600000, // 2 weeks
    rejectionMessage: "Item too old (>2 weeks)",
  },
}

