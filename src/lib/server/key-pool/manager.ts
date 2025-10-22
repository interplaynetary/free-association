import type { KeyHealthStatus } from '../schemas/routing';

// Import env vars with fallbacks for static builds
let OPENROUTER_KEYS: string | undefined;

try {
  const env = await import('$env/static/private');
  OPENROUTER_KEYS = env.OPENROUTER_KEYS;
} catch (e) {
  OPENROUTER_KEYS = undefined;
}

/**
 * OpenRouter key pool structure
 */
export interface KeyInfo {
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
 * Initialize OpenRouter key pool
 */
export function initializeKeyPool(): void {
  if (!OPENROUTER_KEYS) {
    console.warn('⚠️  No OPENROUTER_KEYS configured!');
    console.warn('   Set OPENROUTER_KEYS in .env file');
    return;
  }
  
  const keys = OPENROUTER_KEYS.split(',').map(k => k.trim()).filter(k => k);
  
  openRouterKeys = keys.map(key => ({
    key,
    health: 'healthy' as KeyHealthStatus,
    lastUsed: 0,
    successCount: 0,
    failureCount: 0,
    costToday: 0,
    creditsRemaining: null
  }));
  
  console.log(`✅ Initialized OpenRouter pool with ${openRouterKeys.length} key(s)`);
}

/**
 * Get next healthy OpenRouter key (round-robin)
 */
export function getHealthyKey(): KeyInfo | null {
  if (openRouterKeys.length === 0) {
    return null;
  }
  
  const attempts = openRouterKeys.length;
  
  for (let i = 0; i < attempts; i++) {
    const keyInfo = openRouterKeys[currentIndex];
    
    if (keyInfo.health === 'healthy') {
      currentIndex = (currentIndex + 1) % openRouterKeys.length;
      keyInfo.lastUsed = Date.now();
      return keyInfo;
    }
    
    currentIndex = (currentIndex + 1) % openRouterKeys.length;
  }
  
  return null;
}

/**
 * Update key health status
 */
export function updateKeyHealth(
  key: string,
  status: KeyHealthStatus,
  errorMessage: string | null = null,
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
  if (errorMessage && errorMessage.toLowerCase().includes('insufficient credits')) {
    keyInfo.health = 'depleted';
    console.log(`❌ Key ${key.substring(0, 15)}... DEPLETED - no credits remaining`);
  }
  
  if (oldStatus !== status) {
    const errMsg = errorMessage ? ` (${errorMessage})` : '';
    const costMsg = cost ? ` ($${cost.toFixed(4)})` : '';
    console.log(`🔄 Key ${key.substring(0, 15)}... ${oldStatus} → ${status}${errMsg}${costMsg}`);
  }
  
  return true;
}

/**
 * Add a new key to the pool
 */
export function addKey(key: string, donorId?: string, donorName?: string): boolean {
  if (openRouterKeys.some(k => k.key === key)) {
    return false;
  }
  
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
  return true;
}

/**
 * Remove a key from the pool
 */
export function removeKey(key: string): boolean {
  const index = openRouterKeys.findIndex(k => k.key === key);
  
  if (index === -1) {
    return false;
  }
  
  openRouterKeys.splice(index, 1);
  
  if (currentIndex >= openRouterKeys.length) {
    currentIndex = 0;
  }
  
  console.log(`➖ Removed key from OpenRouter pool`);
  return true;
}

/**
 * Get pool status
 */
export function getPoolStatus() {
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
  
  return {
    totalKeys: openRouterKeys.length,
    health: healthCounts,
    currentIndex,
    stats: {
      totalRequests,
      successRate: `${successRate}%`,
      totalCostToday: `$${totalCost.toFixed(4)}`
    }
  };
}

/**
 * Auto-recovery background process
 */
export function startHealthMonitor(intervalMs = 30000): void {
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
    }
  }, intervalMs);
  
  console.log(`🔍 Health monitor started (interval: ${intervalMs}ms)`);
}

// Initialize on module load
initializeKeyPool();

// Start health monitor
if (typeof setInterval !== 'undefined') {
  startHealthMonitor();
}

