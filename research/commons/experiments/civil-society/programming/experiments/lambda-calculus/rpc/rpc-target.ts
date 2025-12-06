/**
 * RpcTarget - Cap'n Web Style Marker Class
 * 
 * Classes that extend RpcTarget are automatically RPC-able.
 * Their methods can be called remotely, and instances are passed by reference.
 * 
 * Based on Cap'n Web's RpcTarget pattern:
 * https://blog.cloudflare.com/capnweb-javascript-rpc-library/
 * 
 * Usage:
 * ```typescript
 * class MyApi extends RpcTarget {
 *   async hello(name: string): Promise<string> {
 *     return `Hello, ${name}!`;
 *   }
 * }
 * 
 * // Instance is now RPC-able!
 * const api = new MyApi();
 * ```
 */

/**
 * Marker base class for RPC targets
 * 
 * Any class extending this can be called over RPC.
 * No implementation needed - it's just a marker!
 */
export class RpcTarget {
  // Just a marker - no implementation needed
  // The RPC system will handle method calls automatically
}

/**
 * Check if an object is an RpcTarget
 */
export function isRpcTarget(obj: unknown): obj is RpcTarget {
  return obj instanceof RpcTarget;
}

/**
 * Type helper for RPC stubs (client-side proxies)
 * 
 * Converts a class type to its RPC stub type.
 * All methods remain the same but are called over RPC.
 * 
 * Usage:
 * ```typescript
 * interface MyApi {
 *   hello(name: string): Promise<string>;
 * }
 * 
 * let stub: RpcStub<MyApi> = newWebSocketSession('wss://...');
 * ```
 */
export type RpcStub<T> = {
  [K in keyof T]: T[K] extends (...args: infer A) => infer R
    ? (...args: A) => R
    : T[K];
};

