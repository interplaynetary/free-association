import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { NODE_ENV } from '$env/static/private';

/**
 * GET /api/ai/health - Health check
 */
export const GET: RequestHandler = async () => {
  return json({
    status: 'ok',
    service: 'ai-proxy-ts',
    timestamp: Date.now(),
    environment: NODE_ENV || 'development'
  });
};

