import { error } from '@sveltejs/kit';
import jwt from 'jsonwebtoken';
import { JWT_SECRET, JWT_EXPIRY, MASTER_API_KEY } from '$env/static/private';

// In-memory API key store for multi-key support
const validApiKeys = new Set<string>();

// Only add API key if provided
if (MASTER_API_KEY) {
  validApiKeys.add(MASTER_API_KEY);
}

export interface AuthResult {
  authenticated: boolean;
  user?: {
    userId: string;
    role: string;
    [key: string]: any;
  };
}

/**
 * Authenticate API key from header
 */
export function authenticateApiKey(apiKey: string | null): boolean {
  if (!apiKey) {
    return false;
  }
  return validApiKeys.has(apiKey);
}

/**
 * Authenticate JWT token from header
 */
export function authenticateJWT(token: string | null): AuthResult {
  if (!token) {
    return { authenticated: false };
  }

  try {
    if (!JWT_SECRET) {
      throw new Error('JWT_SECRET not configured');
    }
    const decoded = jwt.verify(token, JWT_SECRET) as any;
    return {
      authenticated: true,
      user: decoded
    };
  } catch (err) {
    return { authenticated: false };
  }
}

/**
 * Authenticate either API key OR JWT
 */
export function authenticateEither(request: Request): AuthResult {
  // Try API key first
  const apiKey = request.headers.get('X-API-Key');
  if (apiKey && authenticateApiKey(apiKey)) {
    return { authenticated: true };
  }

  // Try JWT
  const authHeader = request.headers.get('Authorization');
  if (authHeader) {
    const token = authHeader.split(' ')[1];
    if (token) {
      const jwtResult = authenticateJWT(token);
      if (jwtResult.authenticated) {
        return jwtResult;
      }
    }
  }

  return { authenticated: false };
}

/**
 * Require authentication (throws error if not authenticated)
 */
export function requireAuth(request: Request): AuthResult {
  const result = authenticateEither(request);
  if (!result.authenticated) {
    throw error(401, 'Valid API key (X-API-Key header) or JWT token (Authorization: Bearer header) is required.');
  }
  return result;
}

/**
 * Generate a JWT token
 */
export function generateToken(payload: any, expiresIn?: string): string {
  if (!JWT_SECRET) {
    throw new Error('JWT_SECRET not configured - cannot generate tokens');
  }
  return jwt.sign(payload, JWT_SECRET, { expiresIn: expiresIn || JWT_EXPIRY || '24h' });
}

/**
 * Add a new API key (for admin purposes)
 */
export function addApiKey(key: string): void {
  validApiKeys.add(key);
}

/**
 * Remove an API key (for admin purposes)
 */
export function removeApiKey(key: string): void {
  validApiKeys.delete(key);
}

