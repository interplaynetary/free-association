import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { routeRequest } from '$lib/server/llm/router';

/**
 * POST /api/llm/route - Main routing endpoint with typed flows
 */
export const POST: RequestHandler = async ({ request }) => {
  try {
    const body = await request.json();
    const routing = await routeRequest(body);
    
    console.log('Routing decision:', {
      flow: routing.flow?.name,
      model: routing.model,
      provider: routing.provider
    });
    
    return json(routing);
    
  } catch (err: any) {
    console.error('Routing error:', err);
    throw error(err.message.includes('No providers') ? 503 : 400, 'Routing failed: ' + err.message);
  }
};

