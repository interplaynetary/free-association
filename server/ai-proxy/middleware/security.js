import rateLimit from 'express-rate-limit';
import helmet from 'helmet';

/**
 * Rate limiter for general API endpoints
 * Uses userId from JWT if available, otherwise falls back to IP
 */
export const generalLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // Limit each user/IP to 100 requests per windowMs
  keyGenerator: (req) => {
    // Prefer userId from JWT token (set by auth middleware)
    return req.user?.userId || req.ip;
  },
  message: {
    error: 'Too many requests',
    message: 'You have exceeded the 100 requests in 15 minutes limit.',
    retryAfter: '15 minutes'
  },
  standardHeaders: true,
  legacyHeaders: false,
});

/**
 * Stricter rate limiter for AI endpoints (more expensive)
 * Uses userId from JWT if available, otherwise falls back to IP
 */
export const aiLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 20, // Limit each user/IP to 20 AI requests per windowMs
  keyGenerator: (req) => {
    return req.user?.userId || req.ip;
  },
  message: {
    error: 'Too many AI requests',
    message: 'You have exceeded the 20 AI requests in 15 minutes limit.',
    retryAfter: '15 minutes'
  },
  standardHeaders: true,
  legacyHeaders: false,
});

/**
 * Rate limiter for authentication endpoints
 * Always uses IP address for auth attempts (no user context yet)
 */
export const authLimiter = rateLimit({
  windowMs: 15 * 60 * 1000,
  max: 5, // Limit auth attempts per IP
  message: {
    error: 'Too many authentication attempts',
    message: 'Too many attempts. Please try again later.',
    retryAfter: '15 minutes'
  },
  standardHeaders: true,
  legacyHeaders: false,
});

/**
 * Configure Helmet for security headers
 */
export function configureHelmet() {
  return helmet({
    contentSecurityPolicy: {
      directives: {
        defaultSrc: ["'self'"],
        styleSrc: ["'self'", "'unsafe-inline'"],
        scriptSrc: ["'self'"],
        imgSrc: ["'self'", 'data:', 'https:'],
        connectSrc: ["'self'"],
        fontSrc: ["'self'"],
        objectSrc: ["'none'"],
        mediaSrc: ["'self'"],
        frameSrc: ["'none'"],
      },
    },
    dnsPrefetchControl: { allow: false },
    frameguard: { action: 'deny' },
    hidePoweredBy: true,
    hsts: {
      maxAge: 31536000,
      includeSubDomains: true,
      preload: true
    },
    ieNoOpen: true,
    noSniff: true,
    referrerPolicy: { policy: 'no-referrer' },
    xssFilter: true,
  });
}

/**
 * CORS configuration
 */
export function configureCors() {
  const isProduction = process.env.NODE_ENV === 'production';

  // In production, ALLOWED_ORIGINS must be explicitly set
  if (isProduction && !process.env.ALLOWED_ORIGINS) {
    console.error('🚨 FATAL: ALLOWED_ORIGINS must be set in production!');
    console.error('   Set comma-separated origins in .env file.');
    process.exit(1);
  }

  return {
    origin: process.env.ALLOWED_ORIGINS
      ? process.env.ALLOWED_ORIGINS.split(',').map(o => o.trim())
      : ['http://localhost:5173', 'http://localhost:5174', 'http://localhost:4173'],
    methods: ['GET', 'POST', 'PUT', 'DELETE', 'OPTIONS'],
    allowedHeaders: ['Content-Type', 'Authorization', 'X-API-Key'],
    exposedHeaders: ['X-RateLimit-Limit', 'X-RateLimit-Remaining', 'X-RateLimit-Reset'],
    credentials: true,
    maxAge: 86400 // 24 hours
  };
}

/**
 * Request logger middleware
 */
export function requestLogger(req, res, next) {
  const start = Date.now();

  res.on('finish', () => {
    const duration = Date.now() - start;
    const logEntry = {
      timestamp: new Date().toISOString(),
      method: req.method,
      path: req.path,
      status: res.statusCode,
      duration: `${duration}ms`,
      ip: req.ip,
      userAgent: req.get('user-agent')
    };

    // Only log errors and important requests
    if (res.statusCode >= 400 || process.env.DEBUG === 'true') {
      console.log(JSON.stringify(logEntry));
    }
  });

  next();
}

/**
 * Error handler middleware
 */
export function errorHandler(err, req, res, next) {
  console.error('Error:', {
    timestamp: new Date().toISOString(),
    error: err.message,
    stack: err.stack,
    path: req.path,
    method: req.method
  });

  // Don't leak error details in production
  const isProduction = process.env.NODE_ENV === 'production';

  res.status(err.statusCode || 500).json({
    error: isProduction ? 'Internal server error' : err.message,
    ...(isProduction ? {} : { stack: err.stack }),
    timestamp: Date.now()
  });
}

/**
 * 404 handler
 */
export function notFoundHandler(req, res) {
  res.status(404).json({
    error: 'Not Found',
    message: `Route ${req.method} ${req.path} not found`,
    timestamp: Date.now()
  });
}

const TOKEN_LIMIT = parseInt(process.env.AI_TOKEN_LIMIT || '10000', 10);
const TOKEN_WINDOW_MS = 15 * 60 * 1000; // 15 minutes
const tokenBuckets = new Map();

/**
 * AI Token limiter: blocks requests that would exceed the 10,000 token window per source.
 * Order of keying: JWT userId, then X-API-Key, then IP.
 */
export function aiTokenLimiter(req, res, next) {
  const now = Date.now();
  const userId = req.user?.userId;
  const apiKey = req.header('X-API-Key');
  const ip = req.ip;
  const source = userId || apiKey || ip;
  // Defensive: ensure source is always something
  if (!source) {
    return res.status(400).json({ error: 'Unable to identify client for rate limiting.' });
  }

  const bucket = tokenBuckets.get(source) || { used: 0, windowStart: now };

  // Reset window if expired
  if (now - bucket.windowStart >= TOKEN_WINDOW_MS) {
    bucket.used = 0;
    bucket.windowStart = now;
  }

  // Determine tokens/req: use max_tokens from json body, fallback to default
  let requestedTokens = 1024;
  if (req.body && typeof req.body.max_tokens === 'number' && req.body.max_tokens > 0) {
    requestedTokens = req.body.max_tokens;
  }

  if (bucket.used + requestedTokens > TOKEN_LIMIT) {
    const retryAfterSec = Math.ceil((bucket.windowStart + TOKEN_WINDOW_MS - now)/1000);
    return res.status(429).json({
      error: 'Token rate limit exceeded',
      message: `Limit is ${TOKEN_LIMIT} tokens per 15 minutes per user or source.`,
      retryAfter: `${retryAfterSec} seconds`,
      tokensUsed: bucket.used,
      tokensRequested: requestedTokens
    });
  }

  // Approve and update
  bucket.used += requestedTokens;
  tokenBuckets.set(source, bucket);
  next();
}
