/**
 * Unified RPC Dispatcher
 * 
 * DRY principle: Single place for RPC method dispatching logic.
 * Used by all server implementations (Workers, Node, Bun).
 */

import { RpcTarget } from '../rpc-target';
import { RpcJSON } from '../json-rpc';

export interface RpcRequest {
  id?: string | number;
  method: string;
  params?: any[];
}

export interface RpcResponse {
  id?: string | number;
  result?: any;
  error?: string;
}

export interface RpcBatchRequest {
  method: string;
  args: string; // JSON-serialized args
}

export interface RpcBatchResponse {
  result?: string; // JSON-serialized result
  error?: string;
}

/**
 * Dispatch a single RPC call to a target object
 */
export async function dispatchRpcCall(
  target: RpcTarget | any,
  request: RpcRequest
): Promise<RpcResponse> {
  try {
    const { id, method, params = [] } = request;

    // Check if method exists on target
    if (typeof (target as any)[method] !== 'function') {
      return {
        id,
        error: `Method not found: ${method}`
      };
    }

    // Call the method
    const result = await (target as any)[method](...params);

    return {
      id,
      result
    };
  } catch (error: any) {
    return {
      id: request.id,
      error: error.message || 'Internal error'
    };
  }
}

/**
 * Dispatch a batch of RPC calls (HTTP batch mode)
 */
export async function dispatchRpcBatch(
  target: RpcTarget | any,
  batchRequests: RpcBatchRequest[]
): Promise<RpcBatchResponse[]> {
  const results: RpcBatchResponse[] = [];

  for (const req of batchRequests) {
    try {
      // Check if method exists
      if (typeof (target as any)[req.method] !== 'function') {
        results.push({ error: `Method not found: ${req.method}` });
        continue;
      }

      // Parse args
      const args = RpcJSON.parse(req.args);

      // Call method
      const result = await (target as any)[req.method](...args);

      // Serialize result
      results.push({ result: RpcJSON.stringify(result) });
    } catch (error: any) {
      results.push({ error: error.message || 'Internal error' });
    }
  }

  return results;
}

/**
 * Create a standardized error response
 */
export function createErrorResponse(id: string | number | undefined, error: string): RpcResponse {
  return { id, error };
}

/**
 * Create a standardized success response
 */
export function createSuccessResponse(id: string | number | undefined, result: any): RpcResponse {
  return { id, result };
}

