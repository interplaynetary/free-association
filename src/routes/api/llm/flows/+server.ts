import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { getAvailableFlows } from '$lib/server/llm/router';

/**
 * GET /api/llm/flows - List all available flows
 */
export const GET: RequestHandler = async () => {
  const flows = getAvailableFlows();
  
  return json({
    flows,
    count: flows.length
  });
};

