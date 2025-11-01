/**
 * Server Utilities Index
 * 
 * Central export point for all server-side utilities.
 * Import from here for cleaner code:
 * 
 * @example
 * import { config, requireAuth, createPOSTHandler, holsterNext } from '$lib/server';
 */

// Configuration
export { config, validateConfig } from './config';

// Middleware - Authentication
export {
  authenticate,
  authenticateEvent,
  authenticateApiKey,
  authenticateJWT,
  authenticateBasic,
  requireAuth,
  requireAuthEvent,
  checkAuth,
  generateToken,
  verifyToken,
  addApiKey,
  removeApiKey,
  type AuthResult,
  type AuthOptions
} from './middleware/unified-auth';

// Middleware - Request Handlers
export {
  createPOSTHandler,
  createGETHandler,
  createDELETEHandler,
  createHandler,
  validateBody,
  parseBody,
  successResponse,
  errorResponse,
  validateRequest,
  checkAuthOrError,
  type HandlerContext,
  type HandlerOptions
} from './middleware/request-handler';

// Middleware - Rate Limiting
export {
  checkRateLimit,
  checkGeneralRateLimit,
  checkAiRateLimit,
  checkAuthRateLimit,
  checkTokenRateLimit,
  cleanupRateLimits
} from './middleware/rate-limit';

// Holster - Database Helpers
export {
  holsterGet,
  holsterNext,
  holsterPut,
  holsterNextPut,
  getAccountByCode,
  getAccountByCodeOrFail,
  updateAccount,
  holsterGetArray,
  holsterDecrypt,
  holsterEncrypt,
  holsterVerify,
  ensureAuthenticated,
  holsterDelete,
  holsterSubscribe
} from './holster/db';

// Holster - Core
export {
  holster,
  user,
  username,
  password,
  host,
  inviteCodes,
  requestStats,
  dbStats,
  timeDbOperation,
  mapInviteCodes,
  initializeHolster,
  getAccount,
  updateRequestStats,
  resetStatsIfNeeded
} from './holster/core';

// Health Monitoring
export {
  checkHealth,
  isHealthy,
  registerHealthCheck,
  unregisterHealthCheck,
  getAvailableServices,
  getSystemHealth,
  getHolsterHealth,
  getKeyPoolHealth,
  getLLMRouterHealth,
  getAIProxyHealth,
  getDataRelayHealth,
  type HealthStatus,
  type ServiceHealth,
  type HealthCheckFn,
  type AggregateHealth
} from './health';

// LLM
export * from './llm/router';
export * from './llm/flows';

// Key Pool
export * from './key-pool/manager';

// Data Relay
export * from './data-relay';

// Schemas
export * from './schemas';

