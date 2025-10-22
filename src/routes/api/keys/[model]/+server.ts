import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { getHealthyKey, addKey, removeKey } from '$lib/server/key-pool/manager';

/**
 * GET /api/keys/:model - Get a healthy OpenRouter key
 */
export const GET: RequestHandler = async ({ params }) => {
  const { model } = params;
  
  // Only support 'openrouter' pool
  if (model !== 'openrouter') {
    throw error(400, 'Only "openrouter" pool is supported. Use OpenRouter model names (e.g., anthropic/claude-3-opus)');
  }
  
  const keyInfo = getHealthyKey();
  
  if (!keyInfo) {
    throw error(503, 'No healthy OpenRouter keys available');
  }
  
  return json({
    success: true,
    model: 'openrouter',
    key: keyInfo.key,
    pool: {
      totalKeys: 0, // Will be filled by manager
      healthyKeys: 0
    }
  });
};

/**
 * POST /api/keys/:model - Add a new OpenRouter key
 */
export const POST: RequestHandler = async ({ params, request }) => {
  const { model } = params;
  
  if (model !== 'openrouter') {
    throw error(400, 'Only openrouter pool is supported');
  }
  
  const body = await request.json();
  const { key, donorId, donorName } = body;
  
  if (!key || typeof key !== 'string') {
    throw error(400, 'key is required');
  }
  
  const added = addKey(key, donorId, donorName);
  
  if (!added) {
    throw error(409, 'Key already exists');
  }
  
  return json({
    success: true,
    model: 'openrouter',
    message: 'Key added successfully'
  });
};

/**
 * DELETE /api/keys/:model - Remove a key
 */
export const DELETE: RequestHandler = async ({ params, request }) => {
  const { model } = params;
  
  if (model !== 'openrouter') {
    throw error(400, 'Only openrouter pool is supported');
  }
  
  const body = await request.json();
  const { key } = body;
  
  if (!key) {
    throw error(400, 'key is required');
  }
  
  const removed = removeKey(key);
  
  if (!removed) {
    throw error(404, 'Key not found');
  }
  
  return json({
    success: true,
    model: 'openrouter',
    message: 'Key removed successfully'
  });
};

