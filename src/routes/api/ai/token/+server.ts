import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { TokenRequestSchema } from '$lib/server/schemas';
import { generateToken } from '$lib/server/middleware/auth';
import { checkAuthRateLimit } from '$lib/server/middleware/rate-limit';

// Import env vars with fallbacks for static builds
let MASTER_API_KEY: string | undefined;
let JWT_EXPIRY: string | undefined;

try {
  const env = await import('$env/static/private');
  MASTER_API_KEY = env.MASTER_API_KEY;
  JWT_EXPIRY = env.JWT_EXPIRY;
} catch (e) {
  MASTER_API_KEY = undefined;
  JWT_EXPIRY = undefined;
}

/**
 * POST /api/ai/token - Token generation endpoint
 */
export const POST: RequestHandler = async ({ request }) => {
  try {
    // Rate limiting for auth attempts
    checkAuthRateLimit(request);
    
    const body = await request.json();
    const parsed = TokenRequestSchema.safeParse(body);
    
    if (!parsed.success) {
      throw error(400, 'Invalid request: ' + JSON.stringify(parsed.error.format()));
    }
    
    const { apiKey, userId } = parsed.data;
    const masterKey = MASTER_API_KEY || 'dev-key-12345-change-me';
    
    if (apiKey !== masterKey) {
      throw error(401, 'Invalid API key');
    }
    
    const expiresIn = JWT_EXPIRY || '24h';
    const token = generateToken(
      { userId: userId || 'anonymous', role: 'user', issued: Date.now() },
      expiresIn
    );
    
    return json({ 
      token, 
      expiresIn, 
      type: 'Bearer' 
    });
  } catch (err: any) {
    console.error('Token generation error:', err);
    
    if (err.status) {
      throw err;
    }
    
    throw error(500, 'Token generation failed: ' + err.message);
  }
};

