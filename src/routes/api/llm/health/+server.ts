import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { getAvailableFlows } from '$lib/server/llm/router';

/**
 * GET /api/llm/health - Health check
 * 
 * @deprecated Use GET /api/health?services=llm-router instead
 */
export const GET: RequestHandler = async () => {
  return json({
    status: 'ok',
    service: 'llm-router-typed',
    timestamp: Date.now(),
    flowsAvailable: getAvailableFlows().length
  });
};

