/**
 * Server Utilities Index
 * 
 * Central export point for all server-side utilities.
 * Import from here for cleaner code:
 * 
 * @example
 * import { config, requireAuth, createPOSTHandler, meshNext } from '$lib/server';
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

// Mesh - Database Helpers
export {
  meshGet,
  meshNext,
  meshPut,
  meshNextPut,
  getAccountByCode,
  getAccountByCodeOrFail,
  updateAccount,
  meshGetArray,
  meshDecrypt,
  meshEncrypt,
  meshVerify,
  ensureAuthenticated,
  meshDelete,
  meshSubscribe
} from './mesh/db';

// Mesh - Core
export {
  mesh,
  user,
  username,
  password,
  host,
  inviteCodes,
  requestStats,
  dbStats,
  timeDbOperation,
  mapInviteCodes,
  initializeMesh,
  getAccount,
  updateRequestStats,
  resetStatsIfNeeded
} from './mesh/core';

// Health Monitoring
export {
  checkHealth,
  isHealthy,
  registerHealthCheck,
  unregisterHealthCheck,
  getAvailableServices,
  getSystemHealth,
  getMeshHealth,
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

