import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';

// Import env vars with fallbacks for static builds
let NODE_ENV: string | undefined;

try {
  const env = await import('$env/static/private');
  NODE_ENV = env.NODE_ENV;
} catch (e) {
  NODE_ENV = undefined;
}

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

