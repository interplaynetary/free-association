/**
 * Backward compatibility layer for old auth middleware
 * 
 * Re-exports unified auth functions with the same signatures
 * to avoid breaking existing code.
 */

export {
  authenticateApiKey,
  authenticateJWT,
  authenticate as authenticateEither,
  requireAuth,
  generateToken,
  addApiKey,
  removeApiKey,
  type AuthResult
} from './unified-auth';

