import {z} from "zod"
import type {DataRelayConfig} from "../config"
import {buildFlatPath, DEFAULT_RETENTION, DEFAULT_THROTTLING} from "../config"
import type {User} from "@mblaney/holster/src/holster.js"

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

function mapEnclosure(e: z.infer<typeof enclosureSchema>): RSSFeedItemStored["enclosure"] | undefined {
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
      // Feed metadata is managed separately via add-feed endpoint
      // This could be used to track item counts if needed
      return null
    },
  },

  throttling: DEFAULT_THROTTLING,

  ageFilter: {
    maxItemAge: 1209600000, // 2 weeks
    rejectionMessage: "Item too old (>2 weeks)",
  },
}

