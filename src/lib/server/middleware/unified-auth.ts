/**
 * Unified Authentication Middleware
 * 
 * Consolidates multiple authentication systems (JWT, API Key, Basic Auth)
 * into a single, flexible middleware system.
 */

import { error } from '@sveltejs/kit';
import type { RequestEvent } from '@sveltejs/kit';
import jwt from 'jsonwebtoken';
import { config } from '../config';

// ============================================================================
// Types
// ============================================================================

export interface AuthResult {
  authenticated: boolean;
  method?: 'jwt' | 'api-key' | 'basic';
  user?: {
    userId: string;
    role: string;
    [key: string]: any;
  };
}

export interface AuthOptions {
  allowJwt?: boolean;
  allowApiKey?: boolean;
  allowBasic?: boolean;
  required?: boolean;
}

// ============================================================================
// API Key Management
// ============================================================================

const validApiKeys = new Set<string>();

// Initialize with master key if available
if (config.masterApiKey) {
  validApiKeys.add(config.masterApiKey);
}

export function addApiKey(key: string): void {
  validApiKeys.add(key);
}

export function removeApiKey(key: string): void {
  validApiKeys.delete(key);
}

// ============================================================================
// Authentication Methods
// ============================================================================

/**
 * Authenticate using API key
 */
export function authenticateApiKey(apiKey: string | null): AuthResult {
  if (!apiKey) {
    return { authenticated: false };
  }
  
  if (validApiKeys.has(apiKey)) {
    return {
      authenticated: true,
      method: 'api-key'
    };
  }
  
  return { authenticated: false };
}

/**
 * Authenticate using JWT token
 */
export function authenticateJWT(token: string | null): AuthResult {
  if (!token) {
    return { authenticated: false };
  }
  
  try {
    if (!config.jwtSecret) {
      throw new Error('JWT_SECRET not configured');
    }
    
    const decoded = jwt.verify(token, config.jwtSecret) as any;
    return {
      authenticated: true,
      method: 'jwt',
      user: decoded
    };
  } catch (err) {
    return { authenticated: false };
  }
}

/**
 * Authenticate using Basic Auth
 */
export function authenticateBasic(authHeader: string | null): AuthResult {
  if (!authHeader) {
    return { authenticated: false };
  }
  
  const [scheme, credentials] = authHeader.split(' ');
  if (scheme !== 'Basic' || !credentials) {
    return { authenticated: false };
  }
  
  try {
    const [username, password] = Buffer.from(credentials, 'base64')
      .toString()
      .split(':');
    
    // Check against configured Holster credentials
    if (username === config.holsterUsername && password === config.holsterPassword) {
      return {
        authenticated: true,
        method: 'basic',
        user: {
          userId: username,
          role: 'admin'
        }
      };
    }
  } catch (err) {
    console.error('[Basic Auth] Parse error:', err);
  }
  
  return { authenticated: false };
}

// ============================================================================
// Main Authentication Functions
// ============================================================================

/**
 * Try all enabled authentication methods
 */
export function authenticate(
  request: Request,
  options: AuthOptions = {}
): AuthResult {
  const opts = {
    allowJwt: true,
    allowApiKey: true,
    allowBasic: true,
    ...options
  };
  
  // Try API key first (X-API-Key header)
  if (opts.allowApiKey) {
    const apiKey = request.headers.get('X-API-Key');
    if (apiKey) {
      const result = authenticateApiKey(apiKey);
      if (result.authenticated) {
        return result;
      }
    }
  }
  
  // Try JWT token (Authorization: Bearer header)
  if (opts.allowJwt) {
    const authHeader = request.headers.get('Authorization');
    if (authHeader?.startsWith('Bearer ')) {
      const token = authHeader.substring(7);
      const result = authenticateJWT(token);
      if (result.authenticated) {
        return result;
      }
    }
  }
  
  // Try Basic Auth (Authorization: Basic header)
  if (opts.allowBasic) {
    const authHeader = request.headers.get('Authorization');
    if (authHeader?.startsWith('Basic ')) {
      const result = authenticateBasic(authHeader);
      if (result.authenticated) {
        return result;
      }
    }
  }
  
  return { authenticated: false };
}

/**
 * Require authentication (throws error if not authenticated)
 */
export function requireAuth(
  request: Request,
  options: AuthOptions = {}
): AuthResult {
  const result = authenticate(request, options);
  
  if (!result.authenticated) {
    const methods: string[] = [];
    if (options.allowJwt !== false) methods.push('JWT (Authorization: Bearer)');
    if (options.allowApiKey !== false) methods.push('API Key (X-API-Key)');
    if (options.allowBasic !== false) methods.push('Basic Auth (Authorization: Basic)');
    
    throw error(401, `Authentication required. Supported methods: ${methods.join(', ')}`);
  }
  
  return result;
}

/**
 * SvelteKit RequestEvent version of authenticate
 */
export function authenticateEvent(
  event: RequestEvent,
  options: AuthOptions = {}
): AuthResult {
  return authenticate(event.request, options);
}

/**
 * SvelteKit RequestEvent version of requireAuth
 */
export function requireAuthEvent(
  event: RequestEvent,
  options: AuthOptions = {}
): AuthResult {
  return requireAuth(event.request, options);
}

/**
 * Check auth and return error response (for backward compatibility)
 */
export function checkAuth(
  event: RequestEvent,
  options: AuthOptions = { allowBasic: true, allowJwt: false, allowApiKey: false }
): Response | null {
  const result = authenticate(event.request, options);
  
  if (!result.authenticated) {
    return new Response('Unauthorized', { status: 401 });
  }
  
  return null;
}

// ============================================================================
// Token Generation
// ============================================================================

/**
 * Generate a JWT token
 */
export function generateToken(payload: any, expiresIn?: string): string {
  if (!config.jwtSecret) {
    throw new Error('JWT_SECRET not configured - cannot generate tokens');
  }
  
  const expiry = expiresIn || config.jwtExpiry || '24h';
  return jwt.sign(payload, config.jwtSecret, { expiresIn: expiry } as any);
}

/**
 * Verify and decode a JWT token
 */
export function verifyToken(token: string): any {
  if (!config.jwtSecret) {
    throw new Error('JWT_SECRET not configured');
  }
  
  return jwt.verify(token, config.jwtSecret);
}

