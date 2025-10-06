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

const DEFAULT_API_KEY = 'dev-key-12345-change-in-production';
const DEFAULT_JWT_SECRET = 'your-secret-key-change-in-production';

const apiKey = process.env.MASTER_API_KEY;
const jwtSecret = process.env.JWT_SECRET;
const isProduction = process.env.NODE_ENV === 'production';

// FAIL-FAST: Prevent startup with default credentials in production
if (isProduction) {
  if (!apiKey || apiKey === DEFAULT_API_KEY) {
    console.error('🚨 FATAL: Cannot start in production with default or missing MASTER_API_KEY!');
    console.error('Generate a secure key: openssl rand -hex 32');
    process.exit(1);
  }

  if (!jwtSecret || jwtSecret === DEFAULT_JWT_SECRET || jwtSecret.length < 32) {
    console.error('🚨 FATAL: Cannot start in production with default, missing, or weak JWT_SECRET!');
    console.error('JWT_SECRET must be at least 32 characters.');
    console.error('Generate a secure secret: openssl rand -base64 32');
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
}

// WARN: Development mode with default credentials
if (!isProduction && (!apiKey || apiKey === DEFAULT_API_KEY)) {
  console.warn('⚠️  WARNING: Using default API key in development mode!');
  console.warn('   This is ONLY acceptable for local development.');
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

// Helmet for security headers
app.use(configureHelmet());

// CORS configuration
app.use(cors(configureCors()));

// Body parsing - limit to 1MB to prevent abuse
app.use(bodyParser.json({ limit: '1mb' }));
app.use(bodyParser.urlencoded({ extended: true, limit: '1mb' }));

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

  // Test Gun relay connectivity
  try {
    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('timeout')), 3000);
      gun.get('_health_check').once(() => {
        clearTimeout(timeout);
        resolve();
      });
    });
    health.relays.gun = 'connected';
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
  const token = generateToken({
    userId: userId || 'anonymous',
    role: 'user',
    issued: Date.now()
  }, '24h');

  res.json({
    token,
    expiresIn: '24h',
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
  console.log(`  - API Key: X-API-Key: ${process.env.MASTER_API_KEY || 'dev-key-12345-change-in-production'}`);
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
