import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { config } from '$lib/server/config';

/**
 * GET /api/ai - API documentation
 */
export const GET: RequestHandler = async () => {
  return json({
    service: 'AI Proxy Gateway',
    version: '2.0.0',
    language: 'TypeScript',
    environment: config.nodeEnv,
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

