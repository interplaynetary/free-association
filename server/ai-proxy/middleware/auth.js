import jwt from 'jsonwebtoken';

// In-memory API key store
//
// CURRENT LIMITATION: Single master key, no rotation, no revocation
// This is suitable for:
//   - Development environments
//   - Small single-tenant deployments
//   - Proof-of-concept systems
//
// MIGRATION PATH FOR PRODUCTION:
//
// Phase 1 - Basic Multi-Key (no code changes needed):
//   - Generate multiple API keys
//   - Use addApiKey() function via admin endpoint
//   - Still no persistence, lost on restart
//
// Phase 2 - Persistent Storage (minor refactor):
//   - Replace Set with database (PostgreSQL/Redis)
//   - Add key metadata (user_id, created_at, last_used)
//   - Implement key rotation with expiry dates
//   - Hash keys with bcrypt/argon2 before storage
//   - Example: https://github.com/auth0/node-jsonwebtoken/wiki/API-Key-Best-Practices
//
// Phase 3 - Full Key Management (new service):
//   - Per-user API keys with scoped permissions
//   - Rate limiting per key (not per IP)
//   - Key revocation API
//   - Usage analytics per key
//   - Consider: https://www.npmjs.com/package/@apidevtools/swagger-express-middleware
//
// For immediate production use with current setup:
//   1. Generate strong MASTER_API_KEY (openssl rand -hex 32)
//   2. Rotate key monthly via docker-compose restart
//   3. Monitor for unusual usage patterns
//   4. Plan migration to Phase 2 within 3-6 months
//
const validApiKeys = new Set();

// Only add API key if provided (no hardcoded defaults)
if (process.env.MASTER_API_KEY) {
  validApiKeys.add(process.env.MASTER_API_KEY);
}

/**
 * Middleware to authenticate API key from header
 */
export function authenticateApiKey(req, res, next) {
  const apiKey = req.header('X-API-Key');

  if (!apiKey) {
    return res.status(401).json({
      error: 'Unauthorized',
      message: 'API key is required. Provide via X-API-Key header.'
    });
  }

  if (!validApiKeys.has(apiKey)) {
    return res.status(403).json({
      error: 'Forbidden',
      message: 'Invalid API key'
    });
  }

  next();
}

/**
 * Middleware to authenticate JWT token from header
 */
export function authenticateJWT(req, res, next) {
  const authHeader = req.header('Authorization');
  const token = authHeader && authHeader.split(' ')[1]; // Bearer TOKEN

  if (!token) {
    return res.status(401).json({
      error: 'Unauthorized',
      message: 'JWT token is required. Provide via Authorization: Bearer <token> header.'
    });
  }

  try {
    const secret = process.env.JWT_SECRET;
    if (!secret) {
      throw new Error('JWT_SECRET not configured');
    }
    const decoded = jwt.verify(token, secret);
    req.user = decoded;
    next();
  } catch (error) {
    return res.status(403).json({
      error: 'Forbidden',
      message: error.message === 'JWT_SECRET not configured'
        ? 'Server configuration error'
        : 'Invalid or expired token'
    });
  }
}

/**
 * Middleware that accepts either API key OR JWT
 */
export function authenticateEither(req, res, next) {
  const apiKey = req.header('X-API-Key');
  const authHeader = req.header('Authorization');

  // Try API key first
  if (apiKey && validApiKeys.has(apiKey)) {
    return next();
  }

  // Try JWT
  if (authHeader) {
    const token = authHeader.split(' ')[1];
    if (token) {
      try {
        const secret = process.env.JWT_SECRET;
        if (secret) {
          const decoded = jwt.verify(token, secret);
          req.user = decoded;
          return next();
        }
      } catch (error) {
        // JWT invalid, fall through to error
      }
    }
  }

  return res.status(401).json({
    error: 'Unauthorized',
    message: 'Valid API key (X-API-Key header) or JWT token (Authorization: Bearer header) is required.'
  });
}

/**
 * Generate a JWT token (for testing/admin purposes)
 */
export function generateToken(payload, expiresIn = process.env.JWT_EXPIRY || '24h') {
  const secret = process.env.JWT_SECRET;
  if (!secret) {
    throw new Error('JWT_SECRET not configured - cannot generate tokens');
  }
  return jwt.sign(payload, secret, { expiresIn });
}

/**
 * Add a new API key (for admin purposes)
 */
export function addApiKey(key) {
  validApiKeys.add(key);
}

/**
 * Remove an API key (for admin purposes)
 */
export function removeApiKey(key) {
  validApiKeys.delete(key);
}
