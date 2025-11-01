/**
 * Middleware Index
 * 
 * Central export point for all middleware functionality
 */

// Authentication
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
} from './unified-auth';

// Request Handlers
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
} from './request-handler';

// Rate Limiting
export {
  checkRateLimit,
  checkGeneralRateLimit,
  checkAiRateLimit,
  checkAuthRateLimit,
  checkTokenRateLimit,
  cleanupRateLimits
} from './rate-limit';

