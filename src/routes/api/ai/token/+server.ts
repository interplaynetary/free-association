import { error } from '@sveltejs/kit';
import { TokenRequestSchema } from '$lib/server/schemas';
import { generateToken } from '$lib/server/middleware/unified-auth';
import { checkAuthRateLimit } from '$lib/server/middleware/rate-limit';
import { createPOSTHandler } from '$lib/server/middleware/request-handler';
import { config } from '$lib/server/config';

/**
 * POST /api/ai/token - Token generation endpoint
 */
export const POST = createPOSTHandler(
  TokenRequestSchema,
  async ({ data, event }) => {
    // Rate limiting for auth attempts
    checkAuthRateLimit(event.request);
    
    const { apiKey, userId } = data;
    const masterKey = config.masterApiKey || 'dev-key-12345-change-me';
    
    if (apiKey !== masterKey) {
      throw error(401, 'Invalid API key');
    }
    
    const expiresIn = config.jwtExpiry;
    const token = generateToken(
      { userId: userId || 'anonymous', role: 'user', issued: Date.now() },
      expiresIn
    );
    
    return { 
      token, 
      expiresIn, 
      type: 'Bearer' 
    };
  }
);

