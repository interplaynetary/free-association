import { json } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { getPoolStatus } from '$lib/server/key-pool/manager';

/**
 * GET /api/keys/status - Get pool status
 */
export const GET: RequestHandler = async () => {
  const poolStatus = getPoolStatus();
  
  return json({
    status: 'ok',
    timestamp: Date.now(),
    pool: {
      name: 'openrouter',
      ...poolStatus
    }
  });
};

