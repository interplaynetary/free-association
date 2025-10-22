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
 * GET /api/ai - API documentation
 */
export const GET: RequestHandler = async () => {
  return json({
    service: 'AI Proxy Gateway',
    version: '2.0.0',
    language: 'TypeScript',
    environment: NODE_ENV || 'development',
    endpoints: {
      root: 'GET /api/ai',
      health: 'GET /api/ai/health',
      auth: 'POST /api/ai/token',
      aiCompletion: 'POST /api/ai/completion',
    },
    features: {
      typed: true,
      zodValidation: true,
      flowRouting: true
    }
  });
};

