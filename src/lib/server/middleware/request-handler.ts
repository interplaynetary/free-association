/**
 * Request Handler Wrapper
 * 
 * Provides standardized request handling with automatic:
 * - JSON parsing and validation (Zod)
 * - Authentication checking
 * - Error handling and formatting
 * - Consistent response formats
 */

import { json, text, error } from '@sveltejs/kit';
import type { RequestEvent, RequestHandler } from '@sveltejs/kit';
import type { z } from 'zod';
import { requireAuthEvent, authenticateEvent, type AuthOptions, type AuthResult } from './unified-auth';

// ============================================================================
// Types
// ============================================================================

export interface HandlerContext<TData = any> {
  event: RequestEvent;
  data: TData;
  auth?: AuthResult;
}

export interface HandlerOptions<TSchema extends z.ZodTypeAny = any> {
  // Schema validation
  schema?: TSchema;
  
  // Authentication
  requireAuth?: boolean;
  authOptions?: AuthOptions;
  
  // Response options
  emptyResponse?: boolean;  // Return text("") instead of JSON
  
  // Error handling
  onError?: (err: any) => Response;
}

// ============================================================================
// Handler Wrapper Functions
// ============================================================================

/**
 * Create a POST request handler with validation and auth
 */
export function createPOSTHandler<TSchema extends z.ZodTypeAny>(
  schema: TSchema,
  handler: (ctx: HandlerContext<z.infer<TSchema>>) => Promise<Response | any>,
  options: Omit<HandlerOptions<TSchema>, 'schema'> = {}
): RequestHandler {
  return async (event: RequestEvent) => {
    try {
      // Handle authentication
      let auth: AuthResult | undefined;
      if (options.requireAuth) {
        auth = requireAuthEvent(event, options.authOptions);
      } else if (options.authOptions) {
        auth = authenticateEvent(event, options.authOptions);
      }
      
      // Parse and validate body
      const body = await event.request.json();
      const result = schema.safeParse(body);
      
      if (!result.success) {
        const firstError = result.error.errors[0];
        throw error(400, firstError.message);
      }
      
      // Call handler
      const response = await handler({
        event,
        data: result.data,
        auth
      });
      
      // Handle response
      if (response instanceof Response) {
        return response;
      }
      
      if (options.emptyResponse) {
        return text('');
      }
      
      return json(response);
      
    } catch (err: any) {
      if (options.onError) {
        return options.onError(err);
      }
      
      // Re-throw SvelteKit errors
      if (err.status) {
        throw err;
      }
      
      console.error('[Request Handler Error]', err);
      throw error(500, err.message || 'Internal server error');
    }
  };
}

/**
 * Create a GET request handler with auth
 */
export function createGETHandler(
  handler: (ctx: Omit<HandlerContext, 'data'>) => Promise<Response | any>,
  options: Omit<HandlerOptions, 'schema'> = {}
): RequestHandler {
  return async (event: RequestEvent) => {
    try {
      // Handle authentication
      let auth: AuthResult | undefined;
      if (options.requireAuth) {
        auth = requireAuthEvent(event, options.authOptions);
      } else if (options.authOptions) {
        auth = authenticateEvent(event, options.authOptions);
      }
      
      // Call handler
      const response = await handler({
        event,
        auth
      });
      
      // Handle response
      if (response instanceof Response) {
        return response;
      }
      
      if (options.emptyResponse) {
        return text('');
      }
      
      return json(response);
      
    } catch (err: any) {
      if (options.onError) {
        return options.onError(err);
      }
      
      // Re-throw SvelteKit errors
      if (err.status) {
        throw err;
      }
      
      console.error('[Request Handler Error]', err);
      throw error(500, err.message || 'Internal server error');
    }
  };
}

/**
 * Create a DELETE request handler
 */
export function createDELETEHandler<TSchema extends z.ZodTypeAny>(
  schema: TSchema,
  handler: (ctx: HandlerContext<z.infer<TSchema>>) => Promise<Response | any>,
  options: Omit<HandlerOptions<TSchema>, 'schema'> = {}
): RequestHandler {
  return createPOSTHandler(schema, handler, options);
}

/**
 * Create a generic request handler with full control
 */
export function createHandler<TSchema extends z.ZodTypeAny = any>(
  options: HandlerOptions<TSchema> & {
    method?: 'GET' | 'POST' | 'PUT' | 'DELETE' | 'PATCH';
  } = {}
) {
  return {
    /**
     * POST handler
     */
    POST: (handler: (ctx: HandlerContext<z.infer<TSchema>>) => Promise<Response | any>) => {
      if (!options.schema) {
        throw new Error('Schema required for POST handler');
      }
      return createPOSTHandler(options.schema, handler, options);
    },
    
    /**
     * GET handler
     */
    GET: (handler: (ctx: Omit<HandlerContext, 'data'>) => Promise<Response | any>) => {
      return createGETHandler(handler, options);
    },
    
    /**
     * DELETE handler
     */
    DELETE: (handler: (ctx: HandlerContext<z.infer<TSchema>>) => Promise<Response | any>) => {
      if (!options.schema) {
        throw new Error('Schema required for DELETE handler');
      }
      return createDELETEHandler(options.schema, handler, options);
    }
  };
}

// ============================================================================
// Convenience Functions
// ============================================================================

/**
 * Validate request body against schema
 */
export function validateBody<TSchema extends z.ZodTypeAny>(
  body: any,
  schema: TSchema
): z.infer<TSchema> {
  const result = schema.safeParse(body);
  
  if (!result.success) {
    const firstError = result.error.errors[0];
    throw error(400, firstError.message);
  }
  
  return result.data;
}

/**
 * Parse and validate JSON body from request
 */
export async function parseBody<TSchema extends z.ZodTypeAny>(
  request: Request,
  schema: TSchema
): Promise<z.infer<TSchema>> {
  const body = await request.json();
  return validateBody(body, schema);
}

/**
 * Create standard success response
 */
export function successResponse(data?: any, message?: string) {
  return json({
    success: true,
    message,
    ...data
  });
}

/**
 * Create standard error response
 */
export function errorResponse(message: string, status: number = 400) {
  return json({
    success: false,
    error: message
  }, { status });
}

// ============================================================================
// Backward Compatibility Helpers
// ============================================================================

/**
 * Simple validation wrapper (for gradual migration)
 */
export async function validateRequest<TSchema extends z.ZodTypeAny>(
  event: RequestEvent,
  schema: TSchema
): Promise<z.infer<TSchema>> {
  const body = await event.request.json();
  return validateBody(body, schema);
}

/**
 * Simple auth check wrapper (for gradual migration)
 */
export function checkAuthOrError(
  event: RequestEvent,
  options?: AuthOptions
): AuthResult {
  return requireAuthEvent(event, options);
}

