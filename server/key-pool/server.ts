import express, { Request, Response } from 'express';
import { z } from 'zod';

// Import schemas
import {
  KeyPoolSchema,
  HealthReportRequestSchema,
  AddKeyRequestSchema,
  RemoveKeyRequestSchema,
  ModelHealthSummarySchema,
  type KeyPool,
  type KeyHealthStatus,
  type UsageStats,
  type HealthReportRequest,
  type AddKeyRequest
} from './schemas/keyPool';

interface Config {
  host: string;
  port: number;
  healthCheckInterval: number;
}

const config: Config = {
  host: process.env.KEY_POOL_HOST || '0.0.0.0',
  port: parseInt(process.env.KEY_POOL_PORT || '8769'),
  healthCheckInterval: parseInt(process.env.HEALTH_CHECK_INTERVAL || '30000')
};

console.log('\n=== KEY POOL MANAGER (TypeScript) ===\n');
console.log('Configuration:', JSON.stringify(config, null, 2));

const app = express();
app.use(express.json());

// In-memory key pool storage (typed)
// In production, this should be backed by Redis or a database
const keyPools: Record<string, KeyPool> = {};

/**
 * Initialize key pool with environment variables
 * Format: MODEL_NAME_KEYS=key1,key2,key3
 */
function initializeKeyPools(): void {
  const models = [
    'openrouter',  // Single pool for all OpenRouter keys
    'gpt-4', 'gpt-4-turbo', 'gpt-3.5-turbo',  // Direct OpenAI keys
    'claude-3-opus', 'claude-3-sonnet', 'claude-3-haiku',  // Direct Anthropic keys
    'mistral-large', 'mistral-medium', 'mistral-small'  // Direct Mistral keys
  ];
  
  for (const model of models) {
    const envKey = `${model.toUpperCase().replace(/-/g, '_')}_KEYS`;
    const keysStr = process.env[envKey];
    
    if (keysStr) {
      const keys = keysStr.split(',').map(k => k.trim()).filter(k => k);
      
      if (keys.length > 0) {
        const healthStatus: Record<string, KeyHealthStatus> = {};
        const lastUsed: Record<string, number> = {};
        const usageStats: Record<string, UsageStats> = {};
        
        // Initialize health status for each key
        for (const key of keys) {
          healthStatus[key] = 'healthy';
          lastUsed[key] = 0;
          usageStats[key] = {
            requestsToday: 0,
            costToday: 0,
            successCount: 0,
            failureCount: 0,
            creditsRemaining: null
          };
        }
        
        keyPools[model] = {
          keys,
          currentIndex: 0,
          healthStatus,
          rateLimits: {},
          lastUsed,
          usageStats
        };
        
        console.log(`Initialized ${model} with ${keys.length} key(s)`);
      }
    }
  }
  
  const totalModels = Object.keys(keyPools).length;
  const totalKeys = Object.values(keyPools).reduce((sum, pool) => sum + pool.keys.length, 0);
  console.log(`\nTotal: ${totalModels} models, ${totalKeys} keys\n`);
}

/**
 * Get next healthy key for a model (round-robin with health checks)
 */
function getKeyForModel(modelName: string): string | null {
  const pool = keyPools[modelName];
  
  if (!pool || pool.keys.length === 0) {
    return null;
  }
  
  // Try to find a healthy key, rotating through the pool
  const attempts = pool.keys.length;
  
  for (let i = 0; i < attempts; i++) {
    const key = pool.keys[pool.currentIndex];
    const health = pool.healthStatus[key];
    
    // Check if key is healthy (not depleted, failed, or rate-limited)
    if (health === 'healthy') {
      // Update rotation index for next request
      pool.currentIndex = (pool.currentIndex + 1) % pool.keys.length;
      
      // Update last used timestamp
      pool.lastUsed[key] = Date.now();
      
      return key;
    }
    
    // Skip depleted keys permanently (they need manual re-funding)
    if (health === 'depleted') {
      console.log(`[${modelName}] Skipping depleted key ${key.substring(0, 10)}...`);
    }
    
    // Move to next key
    pool.currentIndex = (pool.currentIndex + 1) % pool.keys.length;
  }
  
  // No healthy keys available
  return null;
}

/**
 * Update key health status
 */
function updateKeyHealth(
  modelName: string,
  key: string,
  status: KeyHealthStatus,
  error: string | null = null,
  cost: number | null = null
): boolean {
  const pool = keyPools[modelName];
  
  if (!pool || !pool.healthStatus.hasOwnProperty(key)) {
    return false;
  }
  
  const oldStatus = pool.healthStatus[key];
  pool.healthStatus[key] = status;
  
  // Update usage stats
  if (status === 'healthy' || status === 'degraded') {
    pool.usageStats[key].successCount++;
  } else if (status === 'failed' || status === 'rate_limited') {
    pool.usageStats[key].failureCount++;
  }
  
  // Track cost if provided
  if (cost !== null && typeof cost === 'number') {
    pool.usageStats[key].costToday += cost;
  }
  
  // Check for depleted credits (OpenRouter specific)
  if (error && error.includes('insufficient credits')) {
    pool.healthStatus[key] = 'depleted';
    console.log(`[${modelName}] Key ${key.substring(0, 10)}... DEPLETED - no credits remaining`);
  }
  
  if (oldStatus !== status) {
    const errorMsg = error ? ` (${error})` : '';
    const costMsg = cost ? ` ($${cost.toFixed(4)})` : '';
    console.log(`[${modelName}] Key ${key.substring(0, 10)}... status: ${oldStatus} → ${status}${errorMsg}${costMsg}`);
  }
  
  return true;
}

/**
 * Background health check process
 */
function startHealthCheckMonitor(): void {
  setInterval(() => {
    for (const [modelName, pool] of Object.entries(keyPools)) {
      for (const key of pool.keys) {
        const status = pool.healthStatus[key];
        const lastUsed = pool.lastUsed[key];
        const timeSinceLastUse = Date.now() - lastUsed;
        
        // Auto-recover degraded keys after 5 minutes of no use
        if (status === 'degraded' && timeSinceLastUse > 5 * 60 * 1000) {
          updateKeyHealth(modelName, key, 'healthy', 'Auto-recovered after cooldown');
        }
        
        // Auto-recover rate-limited keys after 15 minutes
        if (status === 'rate_limited' && timeSinceLastUse > 15 * 60 * 1000) {
          updateKeyHealth(modelName, key, 'healthy', 'Rate limit window expired');
        }
        
        // Note: 'depleted' keys are NOT auto-recovered - they need manual re-funding
      }
    }
  }, config.healthCheckInterval);
  
  console.log(`Health check monitor started (interval: ${config.healthCheckInterval}ms)`);
}

/**
 * GET /keys/:model - Get a key for a specific model
 */
app.get('/keys/:model', (req: Request, res: Response) => {
  const modelName = req.params.model;
  const key = getKeyForModel(modelName);
  
  if (!key) {
    return res.status(503).json({
      error: 'No keys available',
      message: `No healthy keys available for model: ${modelName}`,
      model: modelName
    });
  }
  
  const pool = keyPools[modelName];
  const healthyKeys = Object.values(pool.healthStatus).filter(s => s === 'healthy').length;
  
  res.json({
    success: true,
    model: modelName,
    key,
    pool: {
      totalKeys: pool.keys.length,
      healthyKeys
    }
  });
});

/**
 * POST /health/:model - Report key health status
 */
app.post('/health/:model', (req: Request, res: Response) => {
  const modelName = req.params.model;
  
  // Validate request with Zod
  const parsed = HealthReportRequestSchema.safeParse(req.body);
  
  if (!parsed.success) {
    return res.status(400).json({
      error: 'Invalid request',
      details: parsed.error.format()
    });
  }
  
  const { key, status, error, cost } = parsed.data;
  
  const updated = updateKeyHealth(modelName, key, status, error || null, cost || null);
  
  if (!updated) {
    return res.status(404).json({ error: 'Model or key not found' });
  }
  
  res.json({
    success: true,
    model: modelName,
    key: key.substring(0, 10) + '...',
    status
  });
});

/**
 * GET /status - Get status of all key pools
 */
app.get('/status', (req: Request, res: Response) => {
  const status: Record<string, any> = {};
  
  for (const [modelName, pool] of Object.entries(keyPools)) {
    const healthCounts = {
      healthy: 0,
      degraded: 0,
      failed: 0,
      rate_limited: 0,
      depleted: 0
    };
    
    for (const health of Object.values(pool.healthStatus)) {
      healthCounts[health]++;
    }
    
    const totalRequests = Object.values(pool.usageStats).reduce(
      (sum, stats) => sum + stats.successCount + stats.failureCount,
      0
    );
    const totalSuccess = Object.values(pool.usageStats).reduce(
      (sum, stats) => sum + stats.successCount,
      0
    );
    const totalCost = Object.values(pool.usageStats).reduce(
      (sum, stats) => sum + stats.costToday,
      0
    );
    const successRate = totalRequests > 0 ? ((totalSuccess / totalRequests) * 100).toFixed(2) : '0';
    
    status[modelName] = {
      totalKeys: pool.keys.length,
      health: healthCounts,
      currentIndex: pool.currentIndex,
      stats: {
        totalRequests,
        successRate: `${successRate}%`,
        totalCostToday: `$${totalCost.toFixed(4)}`
      }
    };
  }
  
  res.json({
    status: 'ok',
    timestamp: Date.now(),
    models: status
  });
});

/**
 * GET /health - Service health check
 */
app.get('/health', (req: Request, res: Response) => {
  const totalModels = Object.keys(keyPools).length;
  const totalKeys = Object.values(keyPools).reduce((sum, pool) => sum + pool.keys.length, 0);
  const healthyKeys = Object.values(keyPools).reduce((sum, pool) => {
    return sum + Object.values(pool.healthStatus).filter(s => s === 'healthy').length;
  }, 0);
  
  const healthPercentage = totalKeys > 0
    ? ((healthyKeys / totalKeys) * 100).toFixed(2) + '%'
    : '0%';
  
  res.json({
    status: 'ok',
    service: 'key-pool-ts',
    timestamp: Date.now(),
    summary: {
      totalModels,
      totalKeys,
      healthyKeys,
      healthPercentage
    }
  });
});

/**
 * POST /keys/:model - Add a new key to a model pool
 */
app.post('/keys/:model', (req: Request, res: Response) => {
  const modelName = req.params.model;
  
  // Validate request
  const parsed = AddKeyRequestSchema.safeParse(req.body);
  
  if (!parsed.success) {
    return res.status(400).json({
      error: 'Invalid request',
      details: parsed.error.format()
    });
  }
  
  const { key, metadata } = parsed.data;
  
  if (!keyPools[modelName]) {
    // Create new pool for this model
    keyPools[modelName] = {
      keys: [],
      currentIndex: 0,
      healthStatus: {},
      rateLimits: {},
      lastUsed: {},
      usageStats: {}
    };
  }
  
  const pool = keyPools[modelName];
  
  // Check if key already exists
  if (pool.keys.includes(key)) {
    return res.status(409).json({ error: 'Key already exists in pool' });
  }
  
  // Add key to pool
  pool.keys.push(key);
  pool.healthStatus[key] = 'healthy';
  pool.lastUsed[key] = 0;
  pool.usageStats[key] = {
    requestsToday: 0,
    costToday: 0,
    successCount: 0,
    failureCount: 0,
    creditsRemaining: null
  };
  
  console.log(`Added new key to ${modelName} pool`, metadata || '');
  
  res.json({
    success: true,
    model: modelName,
    totalKeys: pool.keys.length,
    message: 'Key added successfully',
    metadata
  });
});

/**
 * DELETE /keys/:model - Remove a key from a model pool
 */
app.delete('/keys/:model', (req: Request, res: Response) => {
  const modelName = req.params.model;
  
  // Validate request
  const parsed = RemoveKeyRequestSchema.safeParse(req.body);
  
  if (!parsed.success) {
    return res.status(400).json({
      error: 'Invalid request',
      details: parsed.error.format()
    });
  }
  
  const { key } = parsed.data;
  const pool = keyPools[modelName];
  
  if (!pool) {
    return res.status(404).json({ error: 'Model pool not found' });
  }
  
  const index = pool.keys.indexOf(key);
  
  if (index === -1) {
    return res.status(404).json({ error: 'Key not found in pool' });
  }
  
  // Remove key from pool
  pool.keys.splice(index, 1);
  delete pool.healthStatus[key];
  delete pool.lastUsed[key];
  delete pool.usageStats[key];
  
  // Adjust current index if needed
  if (pool.currentIndex >= pool.keys.length) {
    pool.currentIndex = 0;
  }
  
  console.log(`Removed key from ${modelName} pool`);
  
  res.json({
    success: true,
    model: modelName,
    totalKeys: pool.keys.length,
    message: 'Key removed successfully'
  });
});

// Initialize key pools from environment
initializeKeyPools();

// Start health check monitor
startHealthCheckMonitor();

// Start server
app.listen(config.port, config.host, () => {
  console.log(`\n=== KEY POOL MANAGER STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Health check interval: ${config.healthCheckInterval}ms`);
  console.log(`================================\n`);
});

process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('SIGINT received, shutting down');
  process.exit(0);
});

