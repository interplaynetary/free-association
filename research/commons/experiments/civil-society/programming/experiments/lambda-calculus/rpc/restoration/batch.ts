/**
 * RestorationBatch - HTTP Batch Mode for Initial Load
 * 
 * Uses HTTP batch mode to minimize round trips during login.
 * After initial load, upgrades to WebSocket for live updates.
 */

import type { EntityId } from '../types';
import type { ReplicaNode, StateFragment } from './discovery';
import { RpcJSON } from '../json-rpc';

/**
 * Configuration for RestorationBatch
 */
export interface BatchConfig {
  url: string;
  timeout?: number;
}

/**
 * Batch operation to be executed
 */
interface BatchOperation {
  id: string;
  method: string;
  args: unknown[];
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
}

/**
 * RestorationBatch - HTTP batch mode for efficient initial restoration
 * 
 * Queues multiple operations and executes them in ONE HTTP REQUEST.
 * Uses Cap'n Web-style promise pipelining internally.
 */
export class RestorationBatch {
  private url: string;
  private timeout: number;
  private operations: BatchOperation[] = [];
  private executed: boolean = false;

  constructor(config: BatchConfig) {
    this.url = config.url;
    this.timeout = config.timeout || 30000; // 30 second default
  }

  /**
   * Queue a findReplicas operation
   * 
   * Returns a promise that will be resolved when execute() is called.
   */
  findReplicas(publicKey: string): Promise<ReplicaNode[]> {
    if (this.executed) {
      throw new Error('Batch already executed');
    }

    return new Promise((resolve, reject) => {
      this.operations.push({
        id: `findReplicas-${this.operations.length}`,
        method: 'findReplicas',
        args: [publicKey],
        resolve: resolve as (value: unknown) => void,
        reject
      });
    });
  }

  /**
   * Queue a getFragments operation
   * 
   * Can reference a previous operation (promise pipelining).
   */
  getFragments(publicKey: string): Promise<StateFragment[]> {
    if (this.executed) {
      throw new Error('Batch already executed');
    }

    return new Promise((resolve, reject) => {
      this.operations.push({
        id: `getFragments-${this.operations.length}`,
        method: 'getFragments',
        args: [publicKey],
        resolve: resolve as (value: unknown) => void,
        reject
      });
    });
  }

  /**
   * Queue a getMerkleRoots operation
   */
  getMerkleRoots(publicKey: string): Promise<Map<string, string>> {
    if (this.executed) {
      throw new Error('Batch already executed');
    }

    return new Promise((resolve, reject) => {
      this.operations.push({
        id: `getMerkleRoots-${this.operations.length}`,
        method: 'getMerkleRoots',
        args: [publicKey],
        resolve: resolve as (value: unknown) => void,
        reject
      });
    });
  }

  /**
   * Execute the batch - send ONE HTTP REQUEST
   * 
   * All queued operations are sent together, and their promises
   * are resolved with the results.
   */
  async execute(): Promise<void> {
    if (this.executed) {
      throw new Error('Batch already executed');
    }

    if (this.operations.length === 0) {
      return; // Nothing to execute
    }

    this.executed = true;

    try {
      // Prepare batch request
      const batchRequest = this.operations.map(op => ({
        id: op.id,
        method: op.method,
        args: op.args
      }));

      // Send batch request
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), this.timeout);

      const response = await fetch(this.url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: RpcJSON.stringify(batchRequest),
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      // Parse batch response
      const batchResponse = RpcJSON.parse(await response.text());

      if (!Array.isArray(batchResponse)) {
        throw new Error('Invalid batch response format');
      }

      // Resolve/reject promises
      for (let i = 0; i < this.operations.length; i++) {
        const operation = this.operations[i];
        const result = batchResponse[i];

        if (result && typeof result === 'object' && 'error' in result) {
          operation.reject(new Error((result as any).error));
        } else {
          operation.resolve(result);
        }
      }
    } catch (error) {
      // Reject all pending operations
      for (const operation of this.operations) {
        operation.reject(error as Error);
      }
      throw error;
    }
  }

  /**
   * Upgrade to WebSocket for live updates
   * 
   * After the initial batch load, switch to WebSocket for real-time communication.
   */
  upgradeToWebSocket(): WebSocket {
    const wsUrl = this.url.replace(/^http/, 'ws');
    const ws = new WebSocket(wsUrl);
    return ws;
  }

  /**
   * Get number of queued operations
   */
  getOperationCount(): number {
    return this.operations.length;
  }

  /**
   * Check if batch has been executed
   */
  isExecuted(): boolean {
    return this.executed;
  }
}

/**
 * Create a restoration batch with default configuration
 */
export function createRestorationBatch(
  url: string,
  options?: Partial<BatchConfig>
): RestorationBatch {
  return new RestorationBatch({
    url,
    ...options
  });
}

/**
 * Helper to execute a batch and automatically upgrade to WebSocket
 */
export async function executeBatchAndUpgrade(
  batch: RestorationBatch
): Promise<{ ws: WebSocket; executed: boolean }> {
  await batch.execute();
  const ws = batch.upgradeToWebSocket();
  
  return {
    ws,
    executed: true
  };
}

