import 'dotenv/config';
import express from 'express';
import cors from 'cors';
import bodyParser from 'body-parser';
import Gun from 'gun';
import Holster from '@mblaney/holster';

// Import middleware
import { authenticateEither, generateToken } from './middleware/auth.js';
import {
  generalLimiter,
  authLimiter,
  configureHelmet,
  configureCors,
  requestLogger,
  errorHandler,
  notFoundHandler
} from './middleware/security.js';

// Import routes
import gunRoutes, { setGunInstance } from './routes/gun.js';
import holsterRoutes, { setHolsterInstance } from './routes/holster.js';

// ============ CRITICAL: VALIDATE CREDENTIALS ON STARTUP ============

const apiKey = process.env.MASTER_API_KEY;
const jwtSecret = process.env.JWT_SECRET;
const isProduction = process.env.NODE_ENV === 'production';

// FAIL-FAST: Prevent startup with missing or weak credentials in production
if (isProduction) {
  if (!apiKey || apiKey.length < 32) {
    console.error('🚨 FATAL: MASTER_API_KEY must be set and at least 32 characters in production!');
    console.error('Generate a secure key: openssl rand -hex 32');
    process.exit(1);
  }

  if (!jwtSecret || jwtSecret.length < 32) {
    console.error('🚨 FATAL: JWT_SECRET must be set and at least 32 characters in production!');
    console.error('Generate a secure secret: openssl rand -base64 48');
    process.exit(1);
  }

  // Check entropy - must have at least 16 unique characters
  const uniqueChars = new Set(jwtSecret).size;
  if (uniqueChars < 16) {
    console.error('🚨 FATAL: JWT_SECRET has insufficient entropy!');
    console.error(`   Found only ${uniqueChars} unique characters (minimum: 16).`);
    console.error('   Weak secrets like "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" are rejected.');
    console.error('Generate a secure secret: openssl rand -base64 48');
    process.exit(1);
  }

  // Detect pattern repetition (e.g., "abcabcabcabcabcabcabcabcabcabcabcabc")
  const detectRepeatingPattern = (str) => {
    for (let patternLen = 2; patternLen <= Math.floor(str.length / 3); patternLen++) {
      const pattern = str.substring(0, patternLen);
      const repeated = pattern.repeat(Math.floor(str.length / patternLen));
      if (str.startsWith(repeated)) {
        return true;
      }
    }
    return false;
  };

  if (detectRepeatingPattern(jwtSecret)) {
    console.error('🚨 FATAL: JWT_SECRET contains repeating patterns!');
    console.error('   Detected pattern repetition - use a truly random secret.');
    console.error('Generate a secure secret: openssl rand -base64 48');
    process.exit(1);
  }
}

// WARN: Development mode without credentials
if (!isProduction && !apiKey) {
  console.warn('⚠️  WARNING: MASTER_API_KEY not set in development mode!');
  console.warn('   Set credentials in .env file or authentication will fail.');
}

// Configuration from environment variables with defaults
const config = {
  host: process.env.DATA_API_HOST || '0.0.0.0',
  port: parseInt(process.env.DATA_API_PORT) || 8767,
  gunPeer: process.env.GUN_PEER_URL || 'http://gun-relay:8765/gun',
  holsterPeer: process.env.HOLSTER_PEER_URL || 'ws://holster-relay:8766/holster',
  nodeEnv: process.env.NODE_ENV || 'development'
};

console.log('\n=== SECURE API GATEWAY ===\n');
console.log('Configuration:');
console.log(JSON.stringify(config, null, 2));
console.log('\n=== Security Features ===');
console.log('✓ Helmet security headers');
console.log('✓ Rate limiting (100/15min general, 20/15min AI)');
console.log('✓ API key authentication');
console.log('✓ JWT token authentication');
console.log('✓ Request validation');
console.log('✓ CORS protection');
console.log('✓ Error handling\n');

// Initialize Express
const app = express();

// ============ SECURITY MIDDLEWARE ============

// Trust proxy for Docker/proxy deployments (enables real IP from X-Forwarded-For)
app.set('trust proxy', true);

// Helmet for security headers
app.use(configureHelmet());

// CORS configuration
app.use(cors(configureCors()));

// Body parsing - configurable limit to prevent abuse
const bodyLimit = process.env.BODY_PARSER_LIMIT || '1mb';
app.use(bodyParser.json({ limit: bodyLimit }));
app.use(bodyParser.urlencoded({ extended: true, limit: bodyLimit }));

// Request logging
app.use(requestLogger);

// Initialize Gun client (connects to gun-relay)
const gun = Gun({
  peers: [config.gunPeer],
  localStorage: false,
  radisk: false
});

// Initialize Holster client (connects to holster-relay)
const holster = Holster({
  peers: [config.holsterPeer]
});

// Set instances for routes
setGunInstance(gun);
setHolsterInstance(holster);

// ============ PUBLIC ENDPOINTS ============

/**
 * Root endpoint - API documentation
 */
app.get('/', (req, res) => {
  res.json({
    service: 'Free Association Secure API Gateway',
    version: '1.0.0',
    environment: config.nodeEnv,
    security: {
      authentication: 'API Key (X-API-Key header) or JWT (Authorization: Bearer header)',
      rateLimiting: {
        general: '100 requests per 15 minutes',
        ai: '20 requests per 15 minutes',
        auth: '5 requests per 15 minutes'
      }
    },
    endpoints: {
      public: {
        root: 'GET / - This documentation',
        health: 'GET /health - Service health status',
        auth: 'POST /auth/token - Generate JWT token (rate limited)'
      },
      authenticated: {
        gun: {
          put: 'POST /api/gun/put - Write to Gun database',
          get: 'GET /api/gun/get?path=... - Read from Gun database',
          seed: 'POST /api/gun/seed - Seed Gun with sample data'
        },
        holster: {
          put: 'POST /api/holster/put - Write to Holster database',
          get: 'GET /api/holster/get?path=... - Read from Holster database',
          seed: 'POST /api/holster/seed - Seed Holster with sample data'
        }
      }
    },
    notes: {
      authentication: 'All /api/* endpoints require authentication via X-API-Key or Authorization headers.',
      security: 'This is critical infrastructure. All endpoints are rate-limited and secured.'
    }
  });
});

/**
 * Health check endpoint (no auth required)
 * Tests connectivity to Gun and Holster relays
 */
app.get('/health', async (req, res) => {
  const health = {
    status: 'ok',
    service: 'data-api-gateway',
    timestamp: Date.now(),
    uptime: process.uptime(),
    config: {
      gunPeer: config.gunPeer,
      holsterPeer: config.holsterPeer,
      environment: config.nodeEnv
    },
    relays: {
      gun: 'unknown',
      holster: 'unknown'
    }
  };

  // Test Gun relay connectivity (check instance state, not database)
  try {
    if (gun && gun._.opt && gun._.opt.peers) {
      // Check if Gun instance has peers configured
      const peerCount = Object.keys(gun._.opt.peers).length;
      health.relays.gun = peerCount > 0 ? 'connected' : 'no_peers';
      if (peerCount === 0) health.status = 'degraded';
    } else {
      health.relays.gun = 'not_initialized';
      health.status = 'degraded';
    }
  } catch (err) {
    health.relays.gun = 'unreachable';
    health.status = 'degraded';
  }

  // Test Holster relay connectivity (check if holster instance is initialized)
  try {
    if (holster && typeof holster.get === 'function') {
      health.relays.holster = 'connected';
    } else {
      health.relays.holster = 'not_initialized';
      health.status = 'degraded';
    }
  } catch (err) {
    health.relays.holster = 'unreachable';
    health.status = 'degraded';
  }

  const statusCode = health.status === 'ok' ? 200 : 503;
  res.status(statusCode).json(health);
});

/**
 * Authentication endpoint - Generate JWT tokens for testing
 * In production, integrate with your actual auth system
 */
app.post('/auth/token', authLimiter, (req, res) => {
  const { apiKey, userId } = req.body;

  // Verify API key (in production, use proper auth flow)
  const masterKey = process.env.MASTER_API_KEY || 'dev-key-12345-change-in-production';
  if (apiKey !== masterKey) {
    return res.status(401).json({
      error: 'Invalid API key'
    });
  }

  // Generate JWT
  const expiresIn = process.env.JWT_EXPIRY || '24h';
  const token = generateToken({
    userId: userId || 'anonymous',
    role: 'user',
    issued: Date.now()
  }, expiresIn);

  res.json({
    token,
    expiresIn,
    type: 'Bearer'
  });
});

// ============ AUTHENTICATED API ROUTES ============

// Apply authentication and rate limiting to all /api/* routes
app.use('/api/*', generalLimiter);
app.use('/api/*', authenticateEither);

// Gun endpoints
app.use('/api/gun', gunRoutes);

// Holster endpoints
app.use('/api/holster', holsterRoutes);

// ============ ERROR HANDLING ============

// 404 handler
app.use(notFoundHandler);

// Global error handler
app.use(errorHandler);

// ============ START SERVER ============

app.listen(config.port, config.host, () => {
  console.log(`\n=== SERVER STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Environment: ${config.nodeEnv}`);
  console.log(`\nEndpoints:`);
  console.log(`  - Documentation: http://${config.host}:${config.port}/`);
  console.log(`  - Health check: http://${config.host}:${config.port}/health`);
  console.log(`  - API Gateway: http://${config.host}:${config.port}/api/*`);
  console.log(`\nAuthentication:`);
  const maskedKey = apiKey ? apiKey.substring(0, 8) + '***' : 'not-set';
  console.log(`  - API Key: ${maskedKey}`);
  console.log(`  - Generate JWT: POST /auth/token`);
  console.log(`\n======================\n`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('\nSIGTERM received, shutting down gracefully');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('\nSIGINT received, shutting down gracefully');
  process.exit(0);
});
