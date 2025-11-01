import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { config } from '$lib/server/config';

/**
 * GET /api/ai/health - Health check
 * 
 * @deprecated Use GET /api/health?services=ai-proxy instead
 */
export const GET: RequestHandler = async () => {
  return json({
    status: 'ok',
    service: 'ai-proxy-ts',
    timestamp: Date.now(),
    environment: config.nodeEnv
  });
};

