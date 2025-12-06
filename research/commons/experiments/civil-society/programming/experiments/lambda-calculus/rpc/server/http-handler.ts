/**
 * Unified HTTP RPC Handler
 * 
 * DRY principle: Single place for HTTP RPC request handling.
 * Works with any HTTP implementation (fetch API, Node http, Bun).
 */

import { RpcJSON } from '../json-rpc';
import { RelayServer } from '../relay-server';
import { dispatchRpcBatch, RpcBatchRequest, RpcBatchResponse } from './rpc-dispatcher';

export interface HttpRequest {
  method: string;
  body: string | Promise<string>;
  headers: Record<string, string>;
}

export interface HttpResponse {
  status: number;
  headers: Record<string, string>;
  body: string;
}

/**
 * Handle HTTP RPC batch request for a RelayServer
 */
export async function handleHttpRpcBatch(
  request: HttpRequest,
  relay: RelayServer
): Promise<HttpResponse> {
  // Only accept POST
  if (request.method !== 'POST') {
    return {
      status: 405,
      headers: { 'Content-Type': 'text/plain' },
      body: 'Method Not Allowed'
    };
  }

  try {
    // Get request body
    const body = typeof request.body === 'string' ? request.body : await request.body;

    // Parse batch requests
    const batchRequests: RpcBatchRequest[] = RpcJSON.parse(body);

    if (!Array.isArray(batchRequests)) {
      return {
        status: 400,
        headers: { 'Content-Type': 'application/json' },
        body: RpcJSON.stringify({ error: 'Expected array of RPC requests' })
      };
    }

    // Dispatch batch
    const results = await dispatchRpcBatch(relay, batchRequests);

    return {
      status: 200,
      headers: { 'Content-Type': 'application/json' },
      body: RpcJSON.stringify(results)
    };
  } catch (error: any) {
    return {
      status: 500,
      headers: { 'Content-Type': 'application/json' },
      body: RpcJSON.stringify({ error: error.message || 'Internal error' })
    };
  }
}

/**
 * Create stats response
 */
export function handleStatsRequest(relay: RelayServer): HttpResponse {
  return {
    status: 200,
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(relay.getStats(), null, 2)
  };
}

