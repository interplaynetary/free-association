import { error } from '@sveltejs/kit';

/**
 * Rate limiter configuration
 */
interface RateLimitConfig {
  windowMs: number;
  max: number;
  message: string;
  keyGenerator: (request: Request, userId?: string) => string;
}

/**
 * Rate limit entry
 */
interface RateLimitEntry {
  count: number;
  resetTime: number;
}

// In-memory store for rate limits
const rateLimitStore = new Map<string, Map<string, RateLimitEntry>>();

/**
 * Get client IP from request
 */
function getClientIp(request: Request): string {
  const forwarded = request.headers.get('x-forwarded-for');
  if (forwarded) {
    return forwarded.split(',')[0].trim();
  }
  return 'unknown';
}

/**
 * Check rate limit
 */
export function checkRateLimit(
  request: Request,
  config: RateLimitConfig,
  userId?: string
): void {
  const key = config.keyGenerator(request, userId);
  const now = Date.now();
  
  // Get or create store for this limiter
  let limiterStore = rateLimitStore.get(config.message);
  if (!limiterStore) {
    limiterStore = new Map();
    rateLimitStore.set(config.message, limiterStore);
  }
  
  // Get or create entry for this key
  let entry = limiterStore.get(key);
  if (!entry || now > entry.resetTime) {
    entry = {
      count: 0,
      resetTime: now + config.windowMs
    };
    limiterStore.set(key, entry);
  }
  
  // Check limit
  if (entry.count >= config.max) {
    const retryAfterSec = Math.ceil((entry.resetTime - now) / 1000);
    throw error(429, `${config.message} (Retry after ${retryAfterSec} seconds)`);
  }
  
  // Increment count
  entry.count++;
}

/**
 * General API rate limiter (100 requests per 15 min)
 */
export function checkGeneralRateLimit(request: Request, userId?: string): void {
  checkRateLimit(request, {
    windowMs: 15 * 60 * 1000,
    max: 100,
    message: 'Too many requests. Limit: 100 requests per 15 minutes.',
    keyGenerator: (req, uid) => uid || getClientIp(req)
  }, userId);
}

/**
 * AI endpoint rate limiter (20 requests per 15 min)
 */
export function checkAiRateLimit(request: Request, userId?: string): void {
  checkRateLimit(request, {
    windowMs: 15 * 60 * 1000,
    max: 20,
    message: 'Too many AI requests. Limit: 20 AI requests per 15 minutes.',
    keyGenerator: (req, uid) => uid || getClientIp(req)
  }, userId);
}

/**
 * Auth endpoint rate limiter (5 attempts per 15 min)
 */
export function checkAuthRateLimit(request: Request): void {
  checkRateLimit(request, {
    windowMs: 15 * 60 * 1000,
    max: 5,
    message: 'Too many authentication attempts. Please try again later.',
    keyGenerator: (req) => getClientIp(req)
  });
}

/**
 * Token-based rate limiter for AI endpoints
 */
interface TokenBucket {
  used: number;
  windowStart: number;
}

const tokenBuckets = new Map<string, TokenBucket>();

export function checkTokenRateLimit(
  request: Request,
  requestedTokens: number,
  userId?: string
): void {
  const TOKEN_LIMIT = 10000;
  const TOKEN_WINDOW_MS = 15 * 60 * 1000;
  
  const source = userId || getClientIp(request);
  const now = Date.now();
  
  let bucket = tokenBuckets.get(source);
  if (!bucket || now - bucket.windowStart >= TOKEN_WINDOW_MS) {
    bucket = { used: 0, windowStart: now };
    tokenBuckets.set(source, bucket);
  }
  
  if (bucket.used + requestedTokens > TOKEN_LIMIT) {
    const retryAfterSec = Math.ceil((bucket.windowStart + TOKEN_WINDOW_MS - now) / 1000);
    throw error(429, `Token rate limit exceeded. Limit: ${TOKEN_LIMIT} tokens per 15 minutes. Retry after ${retryAfterSec} seconds. (Used: ${bucket.used}, Requested: ${requestedTokens})`);
  }
  
  bucket.used += requestedTokens;
}

/**
 * Cleanup old rate limit entries (run periodically)
 */
export function cleanupRateLimits(): void {
  const now = Date.now();
  
  for (const [limiterName, limiterStore] of rateLimitStore.entries()) {
    for (const [key, entry] of limiterStore.entries()) {
      if (now > entry.resetTime) {
        limiterStore.delete(key);
      }
    }
  }
  
  for (const [source, bucket] of tokenBuckets.entries()) {
    if (now - bucket.windowStart >= 15 * 60 * 1000) {
      tokenBuckets.delete(source);
    }
  }
}

// Run cleanup every 5 minutes
if (typeof setInterval !== 'undefined') {
  setInterval(cleanupRateLimits, 5 * 60 * 1000);
}

