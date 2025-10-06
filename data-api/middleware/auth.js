import jwt from 'jsonwebtoken';

// In-memory API key store
// NOTE: This is suitable for development and small deployments with a single master key.
// For production with multiple users, implement:
//   - Database-backed key storage (PostgreSQL, Redis)
//   - Key rotation mechanism with expiry dates
//   - Per-user API keys with scoped permissions
//   - API key hashing (bcrypt/argon2) for secure storage
const validApiKeys = new Set([
  process.env.MASTER_API_KEY || 'dev-key-12345-change-in-production'
]);

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
    const secret = process.env.JWT_SECRET || 'your-secret-key-change-in-production';
    const decoded = jwt.verify(token, secret);
    req.user = decoded;
    next();
  } catch (error) {
    return res.status(403).json({
      error: 'Forbidden',
      message: 'Invalid or expired token'
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
        const secret = process.env.JWT_SECRET || 'your-secret-key-change-in-production';
        const decoded = jwt.verify(token, secret);
        req.user = decoded;
        return next();
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
  const secret = process.env.JWT_SECRET || 'your-secret-key-change-in-production';
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
