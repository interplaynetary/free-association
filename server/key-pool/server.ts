import express, { Request, Response } from 'express';
import { z } from 'zod';
import {
  HealthReportSchema,
  KeyHealthStatus
} from '../shared-schemas/routing';

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

console.log('\n=== KEY POOL MANAGER (OpenRouter-Only) ===\n');
console.log('Configuration:', JSON.stringify(config, null, 2));

const app = express();
app.use(express.json());

/**
 * OpenRouter key pool structure
 */
interface KeyInfo {
  key: string;
  health: KeyHealthStatus;
  lastUsed: number;
  successCount: number;
  failureCount: number;
  costToday: number;
  creditsRemaining: number | null;
  donorId?: string;
  donorName?: string;
}

// Single pool for all OpenRouter keys
let openRouterKeys: KeyInfo[] = [];
let currentIndex = 0;

/**
 * Initialize OpenRouter key pool from environment
 */
function initializeKeyPool(): void {
  const keysStr = process.env.OPENROUTER_KEYS;
  
  if (!keysStr) {
    console.warn('⚠️  No OPENROUTER_KEYS configured!');
    console.warn('   Set OPENROUTER_KEYS=sk-or-v1-key1,sk-or-v1-key2,...');
    return;
  }
  
  const keys = keysStr.split(',').map(k => k.trim()).filter(k => k);
  
  openRouterKeys = keys.map(key => ({
    key,
    health: 'healthy' as KeyHealthStatus,
    lastUsed: 0,
    successCount: 0,
    failureCount: 0,
    costToday: 0,
    creditsRemaining: null
  }));
  
  console.log(`✅ Initialized OpenRouter pool with ${openRouterKeys.length} key(s)\n`);
}

/**
 * Get next healthy OpenRouter key (round-robin)
 */
function getHealthyKey(): KeyInfo | null {
  if (openRouterKeys.length === 0) {
    return null;
  }
  
  // Try each key once
  const attempts = openRouterKeys.length;
  
  for (let i = 0; i < attempts; i++) {
    const keyInfo = openRouterKeys[currentIndex];
    
    // Check if key is healthy (not depleted, failed, or rate-limited)
    if (keyInfo.health === 'healthy') {
      // Update rotation index
      currentIndex = (currentIndex + 1) % openRouterKeys.length;
      
      // Update last used
      keyInfo.lastUsed = Date.now();
      
      return keyInfo;
    }
    
    // Try next key
    currentIndex = (currentIndex + 1) % openRouterKeys.length;
  }
  
  return null;
}

/**
 * Update key health status
 */
function updateKeyHealth(
  key: string,
  status: KeyHealthStatus,
  error: string | null = null,
  cost: number | null = null
): boolean {
  const keyInfo = openRouterKeys.find(k => k.key === key);
  
  if (!keyInfo) {
    return false;
  }
  
  const oldStatus = keyInfo.health;
  keyInfo.health = status;
  
  // Update stats
  if (status === 'healthy' || status === 'degraded') {
    keyInfo.successCount++;
  } else if (status === 'failed' || status === 'rate_limited') {
    keyInfo.failureCount++;
  }
  
  // Track cost
  if (cost !== null && cost > 0) {
    keyInfo.costToday += cost;
  }
  
  // Check for depleted credits
  if (error && error.toLowerCase().includes('insufficient credits')) {
    keyInfo.health = 'depleted';
    console.log(`❌ Key ${key.substring(0, 15)}... DEPLETED - no credits remaining`);
  }
  
  if (oldStatus !== status) {
    const errorMsg = error ? ` (${error})` : '';
    const costMsg = cost ? ` ($${cost.toFixed(4)})` : '';
    console.log(`🔄 Key ${key.substring(0, 15)}... ${oldStatus} → ${status}${errorMsg}${costMsg}`);
  }
  
  return true;
}

/**
 * Auto-recovery background process
 */
function startHealthMonitor(): void {
  setInterval(() => {
    const now = Date.now();
    
    for (const keyInfo of openRouterKeys) {
      const timeSinceLastUse = now - keyInfo.lastUsed;
      
      // Auto-recover degraded keys after 5 minutes
      if (keyInfo.health === 'degraded' && timeSinceLastUse > 5 * 60 * 1000) {
        keyInfo.health = 'healthy';
        console.log(`✅ Auto-recovered key ${keyInfo.key.substring(0, 15)}... from degraded`);
      }
      
      // Auto-recover rate-limited keys after 15 minutes
      if (keyInfo.health === 'rate_limited' && timeSinceLastUse > 15 * 60 * 1000) {
        keyInfo.health = 'healthy';
        console.log(`✅ Auto-recovered key ${keyInfo.key.substring(0, 15)}... from rate limit`);
      }
      
      // Depleted keys are NOT auto-recovered
    }
  }, config.healthCheckInterval);
  
  console.log(`🔍 Health monitor started (interval: ${config.healthCheckInterval}ms)`);
}

/**
 * GET /keys/openrouter - Get a healthy OpenRouter key
 */
app.get('/keys/:model', (req: Request, res: Response) => {
  const { model } = req.params;
  
  // Only support 'openrouter' pool
  if (model !== 'openrouter') {
    return res.status(400).json({
      error: 'Invalid model',
      message: 'Only "openrouter" pool is supported. Use OpenRouter model names (e.g., anthropic/claude-3-opus)',
      model
    });
  }
  
  const keyInfo = getHealthyKey();
  
  if (!keyInfo) {
    return res.status(503).json({
      error: 'No keys available',
      message: 'No healthy OpenRouter keys available',
      totalKeys: openRouterKeys.length,
      healthyKeys: openRouterKeys.filter(k => k.health === 'healthy').length
    });
  }
  
  res.json({
    success: true,
    model: 'openrouter',
    key: keyInfo.key,
    pool: {
      totalKeys: openRouterKeys.length,
      healthyKeys: openRouterKeys.filter(k => k.health === 'healthy').length
    }
  });
});

/**
 * POST /health/:model - Report key health
 */
app.post('/health/:model', (req: Request, res: Response) => {
  const { model } = req.params;
  
  // Validate request
  const parsed = HealthReportSchema.safeParse(req.body);
  
  if (!parsed.success) {
    return res.status(400).json({
      error: 'Invalid request',
      details: parsed.error.format()
    });
  }
  
  const { key, status, error, cost } = parsed.data;
  
  const updated = updateKeyHealth(key, status, error || null, cost || null);
  
  if (!updated) {
    return res.status(404).json({ error: 'Key not found' });
  }
  
  res.json({
    success: true,
    model: 'openrouter',
    key: key.substring(0, 15) + '...',
    status
  });
});

/**
 * GET /status - Get pool status
 */
app.get('/status', (req: Request, res: Response) => {
  const healthCounts = {
    healthy: 0,
    degraded: 0,
    failed: 0,
    rate_limited: 0,
    depleted: 0
  };
  
  for (const keyInfo of openRouterKeys) {
    healthCounts[keyInfo.health]++;
  }
  
  const totalRequests = openRouterKeys.reduce((sum, k) => sum + k.successCount + k.failureCount, 0);
  const totalSuccess = openRouterKeys.reduce((sum, k) => sum + k.successCount, 0);
  const totalCost = openRouterKeys.reduce((sum, k) => sum + k.costToday, 0);
  const successRate = totalRequests > 0 ? ((totalSuccess / totalRequests) * 100).toFixed(2) : '0';
  
  res.json({
    status: 'ok',
    timestamp: Date.now(),
    pool: {
      name: 'openrouter',
      totalKeys: openRouterKeys.length,
      health: healthCounts,
      currentIndex,
      stats: {
        totalRequests,
        successRate: `${successRate}%`,
        totalCostToday: `$${totalCost.toFixed(4)}`
      }
    }
  });
});

/**
 * GET /health - Service health check
 */
app.get('/health', (req: Request, res: Response) => {
  const healthyKeys = openRouterKeys.filter(k => k.health === 'healthy').length;
  const healthPercentage = openRouterKeys.length > 0
    ? ((healthyKeys / openRouterKeys.length) * 100).toFixed(2) + '%'
    : '0%';
  
  res.json({
    status: 'ok',
    service: 'key-pool-openrouter',
    timestamp: Date.now(),
    summary: {
      totalKeys: openRouterKeys.length,
      healthyKeys,
      healthPercentage
    }
  });
});

/**
 * POST /keys/openrouter - Add a new OpenRouter key
 */
app.post('/keys/:model', (req: Request, res: Response) => {
  const { model } = req.params;
  
  if (model !== 'openrouter') {
    return res.status(400).json({ error: 'Only openrouter pool is supported' });
  }
  
  const { key, donorId, donorName } = req.body;
  
  if (!key || typeof key !== 'string') {
    return res.status(400).json({ error: 'key is required' });
  }
  
  // Check if key already exists
  if (openRouterKeys.some(k => k.key === key)) {
    return res.status(409).json({ error: 'Key already exists' });
  }
  
  // Add new key
  openRouterKeys.push({
    key,
    health: 'healthy',
    lastUsed: 0,
    successCount: 0,
    failureCount: 0,
    costToday: 0,
    creditsRemaining: null,
    donorId,
    donorName
  });
  
  console.log(`➕ Added new key to OpenRouter pool${donorName ? ` (donor: ${donorName})` : ''}`);
  
  res.json({
    success: true,
    model: 'openrouter',
    totalKeys: openRouterKeys.length,
    message: 'Key added successfully'
  });
});

/**
 * DELETE /keys/openrouter - Remove a key
 */
app.delete('/keys/:model', (req: Request, res: Response) => {
  const { model } = req.params;
  
  if (model !== 'openrouter') {
    return res.status(400).json({ error: 'Only openrouter pool is supported' });
  }
  
  const { key } = req.body;
  
  if (!key) {
    return res.status(400).json({ error: 'key is required' });
  }
  
  const index = openRouterKeys.findIndex(k => k.key === key);
  
  if (index === -1) {
    return res.status(404).json({ error: 'Key not found' });
  }
  
  openRouterKeys.splice(index, 1);
  
  // Adjust current index if needed
  if (currentIndex >= openRouterKeys.length) {
    currentIndex = 0;
  }
  
  console.log(`➖ Removed key from OpenRouter pool`);
  
  res.json({
    success: true,
    model: 'openrouter',
    totalKeys: openRouterKeys.length,
    message: 'Key removed successfully'
  });
});

// Initialize and start
initializeKeyPool();
startHealthMonitor();

app.listen(config.port, config.host, () => {
  console.log(`\n=== KEY POOL STARTED ===`);
  console.log(`Listening on: ${config.host}:${config.port}`);
  console.log(`Health monitor: ${config.healthCheckInterval}ms`);
  console.log(`========================\n`);
});

process.on('SIGTERM', () => {
  console.log('SIGTERM received, shutting down');
  process.exit(0);
});

process.on('SIGINT', () => {
  console.log('SIGINT received, shutting down');
  process.exit(0);
});
