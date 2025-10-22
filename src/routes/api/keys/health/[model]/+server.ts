import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { updateKeyHealth } from '$lib/server/key-pool/manager';
import { HealthReportSchema } from '$lib/server/schemas/routing';

/**
 * POST /api/keys/health/:model - Report key health
 */
export const POST: RequestHandler = async ({ params, request }) => {
  const { model } = params;
  
  // Validate request
  const body = await request.json();
  const parsed = HealthReportSchema.safeParse(body);
  
  if (!parsed.success) {
    throw error(400, 'Invalid request: ' + JSON.stringify(parsed.error.format()));
  }
  
  const { key, status, error: errorMsg, cost } = parsed.data;
  
  const updated = updateKeyHealth(key, status, errorMsg || null, cost || null);
  
  if (!updated) {
    throw error(404, 'Key not found');
  }
  
  return json({
    success: true,
    model: 'openrouter',
    key: key.substring(0, 15) + '...',
    status
  });
};

