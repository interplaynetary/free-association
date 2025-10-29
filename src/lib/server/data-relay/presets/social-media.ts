import {z} from "zod"
import type {DataRelayConfig} from "../config"
import {buildUserScopedPath, DEFAULT_RETENTION, DEFAULT_THROTTLING} from "../config"
import type {User} from "@mblaney/holster/src/holster.js"

// ============================================================================
// Twitter/X Post Configuration
// ============================================================================

const twitterPostInputSchema = z.object({
  tweetId: z.string().min(1),
  userId: z.string().min(1),
  username: z.string().min(1),
  text: z.string(),
  createdAt: z.number(),
  replyCount: z.number().optional(),
  retweetCount: z.number().optional(),
  likeCount: z.number().optional(),
  media: z
    .array(
      z.object({
        type: z.enum(["photo", "video", "gif"]),
        url: z.string(),
        altText: z.string().optional(),
      }),
    )
    .optional(),
  isRetweet: z.boolean().optional(),
  quotedTweetId: z.string().optional(),
  hashtags: z.array(z.string()).optional(),
  mentions: z.array(z.string()).optional(),
})

export const twitterConfig: DataRelayConfig = {
  type: "twitter",
  displayName: "Twitter/X",

  inputSchema: twitterPostInputSchema,

  storage: {
    collection: "tweets",
    getResourceId: data => data.username,
    getItemId: data => data.tweetId,
    getTimestamp: data => data.createdAt,
    timeGrouping: "day",
    buildPath: (user: User, username: string, timeKey: number | null, tweetId: string) => {
      return buildUserScopedPath(user, "tweets", username, timeKey, tweetId)
    },
  },

  deduplication: {
    buildKey: data => `${data.username}_${data.tweetId}`,
    hashFields: ["text", "replyCount", "retweetCount", "likeCount"],
    cacheTTL: 3600000, // 1 hour (tweets update frequently)
  },

  retention: {
    maxAge: 2592000000, // 30 days
    enableCleanup: true,
    cleanupBatchSize: 100,
  },

  transform: {
    toStorage: data => ({
      ...data,
      hashtags: data.hashtags?.reduce((acc, tag) => ({...acc, [tag]: true}), {}) || {},
      mentions: data.mentions?.reduce((acc, user) => ({...acc, [user]: true}), {}) || {},
    }),
  },

  metadata: {
    collection: "twitterUsers",
    getKey: data => data.username,
    onItemAdd: async (user: User, username: string, current: any) => {
      return {
        username,
        tweetCount: (current?.tweetCount || 0) + 1,
        lastUpdated: Date.now(),
      }
    },
  },

  throttling: {
    ...DEFAULT_THROTTLING,
    delayPerRequest: 100, // Twitter is rate-limited, so be gentle
  },

  ageFilter: {
    maxItemAge: 7776000000, // 90 days
    rejectionMessage: "Tweet too old (>90 days)",
  },
}

// ============================================================================
// Mastodon Post Configuration
// ============================================================================

const mastodonPostInputSchema = z.object({
  id: z.string(),
  account: z.object({
    id: z.string(),
    username: z.string(),
    acct: z.string(), // full handle with instance
    displayName: z.string(),
  }),
  content: z.string(),
  createdAt: z.string().datetime(),
  repliesCount: z.number().optional(),
  reblogsCount: z.number().optional(),
  favouritesCount: z.number().optional(),
  mediaAttachments: z.array(z.any()).optional(),
  mentions: z.array(z.any()).optional(),
  tags: z.array(z.any()).optional(),
  visibility: z.enum(["public", "unlisted", "private", "direct"]).optional(),
})

export const mastodonConfig: DataRelayConfig = {
  type: "mastodon",
  displayName: "Mastodon",

  inputSchema: mastodonPostInputSchema,

  storage: {
    collection: "mastodonPosts",
    getResourceId: data => data.account.acct,
    getItemId: data => data.id,
    getTimestamp: data => new Date(data.createdAt).getTime(),
    timeGrouping: "day",
    buildPath: (user: User, acct: string, timeKey: number | null, postId: string) => {
      return buildUserScopedPath(user, "mastodonPosts", acct, timeKey, postId)
    },
  },

  deduplication: {
    buildKey: data => `${data.account.acct}_${data.id}`,
    hashFields: ["content", "repliesCount", "reblogsCount", "favouritesCount"],
    cacheTTL: 3600000, // 1 hour
  },

  retention: {
    maxAge: 2592000000, // 30 days
    enableCleanup: true,
    cleanupBatchSize: 100,
  },

  transform: {
    toStorage: data => ({
      id: data.id,
      account: data.account,
      content: data.content,
      createdAt: new Date(data.createdAt).getTime(),
      stats: {
        replies: data.repliesCount || 0,
        reblogs: data.reblogsCount || 0,
        favourites: data.favouritesCount || 0,
      },
      media: data.mediaAttachments || [],
      visibility: data.visibility || "public",
    }),
  },

  throttling: DEFAULT_THROTTLING,
}

// ============================================================================
// Reddit Post Configuration
// ============================================================================

const redditPostInputSchema = z.object({
  id: z.string(),
  subreddit: z.string(),
  author: z.string(),
  title: z.string(),
  selftext: z.string().optional(),
  url: z.string(),
  createdUtc: z.number(),
  score: z.number(),
  numComments: z.number(),
  permalink: z.string(),
  isVideo: z.boolean().optional(),
  thumbnail: z.string().optional(),
})

export const redditConfig: DataRelayConfig = {
  type: "reddit",
  displayName: "Reddit",

  inputSchema: redditPostInputSchema,

  storage: {
    collection: "redditPosts",
    getResourceId: data => data.subreddit,
    getItemId: data => data.id,
    getTimestamp: data => data.createdUtc * 1000,
    timeGrouping: "day",
    buildPath: (user: User, subreddit: string, timeKey: number | null, postId: string) => {
      let chain = user.get("redditPosts").next(subreddit)
      if (timeKey !== null) chain = chain.next(timeKey)
      return chain.next(postId)
    },
  },

  deduplication: {
    buildKey: data => `${data.subreddit}_${data.id}`,
    hashFields: ["title", "score", "numComments"],
    cacheTTL: 1800000, // 30 minutes (Reddit scores update frequently)
  },

  retention: {
    maxAge: 604800000, // 7 days
    enableCleanup: true,
    cleanupBatchSize: 50,
  },

  transform: {
    toStorage: data => ({
      id: data.id,
      subreddit: data.subreddit,
      author: data.author,
      title: data.title,
      content: data.selftext || "",
      url: data.url,
      createdAt: data.createdUtc * 1000,
      score: data.score,
      numComments: data.numComments,
      permalink: `https://reddit.com${data.permalink}`,
      isVideo: data.isVideo || false,
      thumbnail: data.thumbnail,
    }),
  },

  metadata: {
    collection: "subreddits",
    getKey: data => data.subreddit,
    onItemAdd: async (user: User, subreddit: string, current: any) => {
      return {
        name: subreddit,
        postCount: (current?.postCount || 0) + 1,
        lastUpdated: Date.now(),
      }
    },
  },

  throttling: DEFAULT_THROTTLING,

  ageFilter: {
    maxItemAge: 2592000000, // 30 days
    rejectionMessage: "Post too old (>30 days)",
  },
}

