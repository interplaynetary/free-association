import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { getPoolStatus } from '$lib/server/key-pool/manager';

/**
 * GET /api/keys/health - Service health check
 * 
 * @deprecated Use GET /api/health?services=key-pool instead
 */
export const GET: RequestHandler = async () => {
  const poolStatus = getPoolStatus();
  const healthyKeys = poolStatus.health.healthy;
  const healthPercentage = poolStatus.totalKeys > 0
    ? ((healthyKeys / poolStatus.totalKeys) * 100).toFixed(2) + '%'
    : '0%';
  
  return json({
    status: 'ok',
    service: 'key-pool-openrouter',
    timestamp: Date.now(),
    summary: {
      totalKeys: poolStatus.totalKeys,
      healthyKeys,
      healthPercentage
    }
  });
};

