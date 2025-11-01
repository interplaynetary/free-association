/**
 * Unified Health Check Endpoint
 * 
 * GET /api/health - Check health of all services
 * GET /api/health?services=ai,llm - Check specific services
 */

import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { checkHealth, getAvailableServices } from '$lib/server/health';

export const GET: RequestHandler = async ({ url }) => {
  const servicesParam = url.searchParams.get('services');
  const services = servicesParam
    ? servicesParam.split(',').map(s => s.trim())
    : undefined;
  
  // Validate requested services
  if (services) {
    const available = getAvailableServices();
    const invalid = services.filter(s => !available.includes(s));
    
    if (invalid.length > 0) {
      return json({
        error: `Unknown services: ${invalid.join(', ')}`,
        available
      }, { status: 400 });
    }
  }
  
  const health = await checkHealth(services);
  
  const statusCode = health.status === 'ok' ? 200 : 503;
  
  return json(health, { status: statusCode });
};
