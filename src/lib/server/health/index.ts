/**
 * Consolidated Health Check System
 * 
 * Aggregates health status from all services into a unified endpoint.
 * Individual services can register their health check functions.
 */

import { config } from '../config';

// ============================================================================
// Types
// ============================================================================

export interface HealthStatus {
  status: 'ok' | 'degraded' | 'down';
  timestamp: number;
  uptime?: number;
  [key: string]: any;
}

export interface ServiceHealth {
  status: 'ok' | 'degraded' | 'down';
  message?: string;
  details?: any;
}

export type HealthCheckFn = () => Promise<ServiceHealth> | ServiceHealth;

// ============================================================================
// Health Check Registry
// ============================================================================

const healthChecks = new Map<string, HealthCheckFn>();

/**
 * Register a health check function for a service
 */
export function registerHealthCheck(serviceName: string, checkFn: HealthCheckFn): void {
  healthChecks.set(serviceName, checkFn);
}

/**
 * Unregister a health check
 */
export function unregisterHealthCheck(serviceName: string): void {
  healthChecks.delete(serviceName);
}

// ============================================================================
// Built-in Health Checks
// ============================================================================

/**
 * System health (memory, uptime)
 */
export function getSystemHealth(): ServiceHealth {
  try {
    const uptime = process.uptime();
    const memUsage = process.memoryUsage();
    
    // Consider degraded if memory usage > 90%
    const heapUsagePercent = (memUsage.heapUsed / memUsage.heapTotal) * 100;
    const status = heapUsagePercent > 90 ? 'degraded' : 'ok';
    
    return {
      status,
      details: {
        uptime: Math.round(uptime),
        memory: {
          rss: Math.round(memUsage.rss / 1024 / 1024),
          heapUsed: Math.round(memUsage.heapUsed / 1024 / 1024),
          heapTotal: Math.round(memUsage.heapTotal / 1024 / 1024),
          external: Math.round(memUsage.external / 1024 / 1024),
          heapUsagePercent: Math.round(heapUsagePercent)
        },
        process: {
          pid: process.pid,
          version: process.version,
          platform: process.platform
        }
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'Failed to get system health'
    };
  }
}

/**
 * Holster database health
 */
export function getHolsterHealth(): ServiceHealth {
  try {
    // Import stats dynamically to avoid circular dependencies
    const { requestStats, dbStats } = require('../holster/core');
    
    const slowRequestPercentage = requestStats.totalRequests > 0
      ? Math.round((requestStats.slowRequests / requestStats.totalRequests) * 100)
      : 0;
    
    const slowDbPercentage = dbStats.totalOps > 0
      ? Math.round((dbStats.slowOps / dbStats.totalOps) * 100)
      : 0;
    
    const errorPercentage = dbStats.totalOps > 0
      ? Math.round((dbStats.errorCount / dbStats.totalOps) * 100)
      : 0;
    
    // Consider degraded if slow requests > 10% or errors > 5%
    let status: 'ok' | 'degraded' | 'down' = 'ok';
    if (slowRequestPercentage > 10 || errorPercentage > 5) {
      status = 'degraded';
    }
    if (errorPercentage > 20) {
      status = 'down';
    }
    
    return {
      status,
      details: {
        requests: {
          total: requestStats.totalRequests,
          slow: requestStats.slowRequests,
          slowPercentage: slowRequestPercentage,
          averageTime: Math.round(requestStats.averageTime)
        },
        database: {
          totalOps: dbStats.totalOps,
          slowOps: dbStats.slowOps,
          slowPercentage: slowDbPercentage,
          errorCount: dbStats.errorCount,
          errorPercentage,
          averageTime: Math.round(dbStats.averageDbTime)
        }
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'Holster service unavailable'
    };
  }
}

/**
 * Key pool health
 */
export function getKeyPoolHealth(): ServiceHealth {
  try {
    const { getPoolStatus } = require('../key-pool/manager');
    const poolStatus = getPoolStatus();
    
    const healthyKeys = poolStatus.health.healthy;
    const totalKeys = poolStatus.totalKeys;
    
    const healthPercentage = totalKeys > 0
      ? (healthyKeys / totalKeys) * 100
      : 0;
    
    // Consider degraded if < 50% healthy, down if no healthy keys
    let status: 'ok' | 'degraded' | 'down' = 'ok';
    if (healthPercentage < 50) {
      status = 'degraded';
    }
    if (healthyKeys === 0) {
      status = 'down';
    }
    
    return {
      status,
      details: {
        totalKeys,
        healthyKeys,
        healthPercentage: healthPercentage.toFixed(2) + '%',
        health: poolStatus.health,
        stats: poolStatus.stats
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'Key pool service unavailable'
    };
  }
}

/**
 * LLM router health
 */
export function getLLMRouterHealth(): ServiceHealth {
  try {
    const { getAvailableFlows } = require('../llm/router');
    const flows = getAvailableFlows();
    
    return {
      status: flows.length > 0 ? 'ok' : 'degraded',
      details: {
        flowsAvailable: flows.length,
        flows: flows.map((f: any) => f.name)
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'LLM router unavailable'
    };
  }
}

/**
 * AI proxy health
 */
export function getAIProxyHealth(): ServiceHealth {
  try {
    return {
      status: 'ok',
      details: {
        environment: config.nodeEnv,
        openrouterBaseUrl: config.openrouterBaseUrl
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'AI proxy unavailable'
    };
  }
}

/**
 * Data relay health
 */
export function getDataRelayHealth(): ServiceHealth {
  try {
    const { user } = require('../holster/core');
    const { getRegistry } = require('../data-relay');
    
    const registry = getRegistry(user);
    const types = registry.getTypes();
    const stats = registry.getStats();
    
    return {
      status: 'ok',
      details: {
        registeredTypes: types,
        stats
      }
    };
  } catch (err) {
    return {
      status: 'down',
      message: 'Data relay unavailable'
    };
  }
}

// ============================================================================
// Register Built-in Health Checks
// ============================================================================

registerHealthCheck('system', getSystemHealth);
registerHealthCheck('holster', getHolsterHealth);
registerHealthCheck('key-pool', getKeyPoolHealth);
registerHealthCheck('llm-router', getLLMRouterHealth);
registerHealthCheck('ai-proxy', getAIProxyHealth);
registerHealthCheck('data-relay', getDataRelayHealth);

// ============================================================================
// Aggregate Health Check
// ============================================================================

export interface AggregateHealth {
  status: 'ok' | 'degraded' | 'down';
  timestamp: number;
  services: Record<string, ServiceHealth>;
  summary: {
    total: number;
    ok: number;
    degraded: number;
    down: number;
  };
}

/**
 * Check health of specific services or all if none specified
 */
export async function checkHealth(services?: string[]): Promise<AggregateHealth> {
  const timestamp = Date.now();
  const servicesToCheck = services || Array.from(healthChecks.keys());
  
  const results: Record<string, ServiceHealth> = {};
  let okCount = 0;
  let degradedCount = 0;
  let downCount = 0;
  
  // Run all health checks in parallel
  await Promise.all(
    servicesToCheck.map(async (serviceName) => {
      const checkFn = healthChecks.get(serviceName);
      if (!checkFn) {
        results[serviceName] = {
          status: 'down',
          message: 'Health check not registered'
        };
        downCount++;
        return;
      }
      
      try {
        const result = await checkFn();
        results[serviceName] = result;
        
        if (result.status === 'ok') okCount++;
        else if (result.status === 'degraded') degradedCount++;
        else downCount++;
      } catch (err: any) {
        results[serviceName] = {
          status: 'down',
          message: err.message || 'Health check failed'
        };
        downCount++;
      }
    })
  );
  
  // Determine overall status
  let overallStatus: 'ok' | 'degraded' | 'down' = 'ok';
  if (downCount > 0) {
    overallStatus = 'down';
  } else if (degradedCount > 0) {
    overallStatus = 'degraded';
  }
  
  return {
    status: overallStatus,
    timestamp,
    services: results,
    summary: {
      total: servicesToCheck.length,
      ok: okCount,
      degraded: degradedCount,
      down: downCount
    }
  };
}

/**
 * Quick health check (just returns ok/not ok)
 */
export async function isHealthy(services?: string[]): Promise<boolean> {
  const health = await checkHealth(services);
  return health.status === 'ok';
}

/**
 * Get list of available services
 */
export function getAvailableServices(): string[] {
  return Array.from(healthChecks.keys());
}

