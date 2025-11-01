/**
 * Centralized Environment Configuration
 * 
 * Handles all environment variable loading with consistent fallbacks
 * for static builds and development environments.
 */

import { building } from '$app/environment';

/**
 * Safely load environment variable with fallback
 */
function loadEnv(name: string, defaultValue: string = ''): string {
  if (building) return defaultValue;
  
  try {
    return process.env[name] ?? defaultValue;
  } catch {
    return defaultValue;
  }
}

/**
 * Load static private env vars (for SvelteKit static imports)
 */
async function loadStaticEnv<T extends Record<string, string>>(
  keys: string[]
): Promise<Partial<T>> {
  try {
    const env = await import('$env/static/private');
    const result: any = {};
    for (const key of keys) {
      result[key] = (env as any)[key];
    }
    return result;
  } catch (e) {
    return {};
  }
}

// ============================================================================
// Application Configuration
// ============================================================================

export const config = {
  // Environment
  nodeEnv: loadEnv('NODE_ENV', 'development'),
  isDev: loadEnv('NODE_ENV') !== 'production',
  
  // Application
  appUrl: loadEnv('APP_URL', 'http://localhost:3000'),
  appHost: loadEnv('APP_HOST', 'http://localhost:3000'),
  
  // JWT Authentication
  jwtSecret: loadEnv('JWT_SECRET'),
  jwtExpiry: loadEnv('JWT_EXPIRY', '24h'),
  masterApiKey: loadEnv('MASTER_API_KEY'),
  
  // Holster Authentication
  holsterUsername: loadEnv('HOLSTER_USER_NAME', 'host'),
  holsterPassword: loadEnv('HOLSTER_USER_PASSWORD', 'password'),
  
  // OpenRouter
  openrouterBaseUrl: loadEnv('OPENROUTER_BASE_URL', 'https://openrouter.ai/api/v1'),
  
  // RSS Feed Integration
  addFeedUrl: loadEnv('ADD_FEED_URL'),
  addFeedId: loadEnv('ADD_FEED_ID'),
  addFeedApiKey: loadEnv('ADD_FEED_API_KEY'),
} as const;

/**
 * Initialize static environment variables (for routes that need them)
 */
export async function initStaticConfig() {
  const staticEnv = await loadStaticEnv([
    'NODE_ENV',
    'JWT_SECRET',
    'JWT_EXPIRY',
    'MASTER_API_KEY',
    'OPENROUTER_BASE_URL',
    'APP_URL'
  ]);
  
  return {
    ...config,
    ...staticEnv
  };
}

/**
 * Validate required configuration at startup
 */
export function validateConfig(): { valid: boolean; missing: string[] } {
  const missing: string[] = [];
  
  // Optional: Add validation for required vars
  if (!config.jwtSecret && !building) {
    console.warn('⚠️  JWT_SECRET not set - JWT authentication will not work');
  }
  
  if (!config.masterApiKey && !building) {
    console.warn('⚠️  MASTER_API_KEY not set - API key authentication will not work');
  }
  
  return {
    valid: missing.length === 0,
    missing
  };
}

