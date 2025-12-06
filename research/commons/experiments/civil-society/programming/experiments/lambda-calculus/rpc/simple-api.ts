/**
 * Simple API - Cap'n Web Inspired
 * 
 * One-line setup for RPC sessions:
 * - newWebSocketSession() - persistent WebSocket connection
 * - newHttpBatchSession() - lightweight HTTP batch mode
 * - newPostMessageSession() - iframe/Worker communication
 * 
 * Now even simpler with EntitySession auto-initialization!
 * 
 * ```typescript
 * let api = newWebSocketSession('alice', 'wss://peer.example.com');
 * await api.initialize(); // One-time init
 * let mr = await api.getMutualRecognition('bob');
 * ```
 */

import { EntitySession } from './entity-session';
import type { EntityAPI, EntityFullAPI } from './api';
import type { RpcStub } from './rpc-target';
import { BrowserStorage } from './browser-storage';
import { RecognitionCache } from './cache';
import { newHttpBatchSession as createHttpBatch } from './transports/http-batch';

/**
 * Simple session is just EntitySession with type safety!
 */
export type SimpleSession = EntitySession;

/**
 * Create WebSocket session (one-line setup!)
 * 
 * Returns typed EntityAPI for full type safety.
 * 
 * @example
 * ```typescript
 * let api: EntityAPI = newWebSocketSession('alice', 'wss://relay.example.com');
 * await api.initialize();
 * let mr = await api.getMutualRecognition('bob');
 * ```
 */
export function newWebSocketSession(
  entityId: string,
  url: string,
  options?: {
    storage?: BrowserStorage;
    cache?: RecognitionCache;
  }
): EntitySession {
  const session = new EntitySession(
    entityId,
    options?.storage,
    options?.cache
  );

  // TODO: Connect WebSocket transport
  // For now, return session with local operations

  return session;
}

/**
 * Create HTTP batch session (lightweight!)
 * 
 * @example
 * ```typescript
 * let batch = newHttpBatchSession('https://api.example.com');
 * let p1 = batch.getMRS(['alice']);
 * let p2 = batch.getMRD(['bob']);
 * let [mrs, mrd] = await Promise.all([p1, p2]);
 * ```
 */
export { newHttpBatchSession as createHttpBatchSession } from './transports/http-batch';

/**
 * Create postMessage session (for iframe/Worker)
 * 
 * @example
 * ```typescript
 * let worker = new Worker('worker.js');
 * let api = newPostMessageSession('alice', worker);
 * await api.initialize();
 * let result = await api.getMutualRecognition('bob');
 * ```
 */
export function newPostMessageSession(
  entityId: string,
  target: Window | Worker,
  options?: {
    storage?: BrowserStorage;
    cache?: RecognitionCache;
  }
): EntitySession {
  const session = new EntitySession(
    entityId,
    options?.storage,
    options?.cache
  );

  // TODO: Connect postMessage transport
  // For now, return session with local operations

  return session;
}

