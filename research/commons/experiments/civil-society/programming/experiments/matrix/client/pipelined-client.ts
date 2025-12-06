/**
 * Pipelined Client Wrapper
 * 
 * Enables Cap'n Web's promise pipelining while maintaining memoization benefits.
 * 
 * Key Features:
 * - Chain RPC calls without awaiting (single round trip)
 * - Memoize based on full pipeline path
 * - Transparent caching of pipelined results
 * 
 * Example:
 * ```typescript
 * // 3 calls in 1 round trip!
 * const session = api.authenticate(id, creds);
 * const network = session.getNetworkState();
 * const mr = await network.computeMutualRecognition("alice", "bob");
 * ```
 */

import type { RpcStub } from 'capnweb';
import { LRUCache, createCacheKey } from './memoization';

/**
 * Pipeline step for cache key generation
 */
interface PipelineStep {
  method: string;
  args: any[];
}

/**
 * Pipelined RPC Wrapper
 * 
 * Wraps RPC stubs to enable memoization of pipelined calls.
 * Tracks the full call chain for cache key generation.
 */
export class PipelinedRpcWrapper<T = any> {
  private readonly cache: LRUCache<string, any>;
  private readonly pipelinePath: PipelineStep[];
  
  constructor(
    private readonly stub: any, // RpcStub<T> | Promise<RpcStub<T>>
    cache: LRUCache<string, any>,
    pipelinePath: PipelineStep[] = []
  ) {
    this.cache = cache;
    this.pipelinePath = pipelinePath;
  }
  
  /**
   * Wrap a method call to enable pipelining + memoization
   */
  private wrapMethod<TResult>(
    method: string,
    ...args: any[]
  ): PipelinedRpcWrapper<TResult> | Promise<TResult> {
    // Build cache key from full pipeline path
    const newPipeline = [...this.pipelinePath, { method, args }];
    const cacheKey = this.buildCacheKey(newPipeline);
    
    // Check cache first
    if (this.cache.has(cacheKey)) {
      const cached = this.cache.get(cacheKey);
      console.log(`[PIPELINE-CACHE-HIT] ${cacheKey}`);
      
      // Return cached value wrapped in resolved promise
      return Promise.resolve(cached);
    }
    
    console.log(`[PIPELINE-CACHE-MISS] ${cacheKey}`);
    
    // Make the pipelined call (no await!)
    const promise = (async () => {
      const resolvedStub = await this.stub;
      const result = await (resolvedStub as any)[method](...args);
      
      // Cache the final result
      this.cache.set(cacheKey, result);
      
      return result;
    })();
    
    // Return wrapped promise for further chaining
    return new PipelinedRpcWrapper<TResult>(
      promise as any,
      this.cache,
      newPipeline
    ) as any;
  }
  
  /**
   * Build cache key from pipeline path
   */
  private buildCacheKey(pipeline: PipelineStep[]): string {
    const parts = pipeline.map(step => 
      `${step.method}(${step.args.map(arg => 
        typeof arg === 'object' ? JSON.stringify(arg) : String(arg)
      ).join(',')})`
    );
    return `pipeline:${parts.join('->')}`;
  }
  
  /**
   * Get the underlying promise (for awaiting)
   */
  async unwrap(): Promise<T> {
    return this.stub as any;
  }
  
  /**
   * Create proxy to intercept method calls
   */
  static create<T = any>(
    stub: any, // RpcStub<T> | Promise<RpcStub<T>>
    cache: LRUCache<string, any>
  ): any {
    const wrapper = new PipelinedRpcWrapper<T>(stub, cache);
    
    return new Proxy(wrapper as any, {
      get(target, prop, receiver) {
        // Pass through special properties
        if (prop === 'then' || prop === 'catch' || prop === 'finally') {
          return (target.stub as any)[prop].bind(target.stub);
        }
        
        if (prop === 'unwrap') {
          return target.unwrap.bind(target);
        }
        
        // Wrap method calls
        if (typeof prop === 'string') {
          return (...args: any[]) => target.wrapMethod(prop, ...args);
        }
        
        return Reflect.get(target, prop, receiver);
      }
    });
  }
}

/**
 * Create a pipelined + memoized RPC client
 * 
 * @param stub - The RPC stub to wrap
 * @param cacheSize - Maximum cache entries (default: 1000)
 * @param cacheTTL - Cache TTL in ms (default: 60000 = 1 minute)
 * @returns Wrapped RPC stub with pipelining + memoization
 */
export function createPipelinedClient<T = any>(
  stub: any, // RpcStub<T>
  cacheSize: number = 1000,
  cacheTTL: number = 60000
): any {
  const cache = new LRUCache<string, any>(cacheSize, cacheTTL);
  return PipelinedRpcWrapper.create(stub, cache);
}

/**
 * Pipeline helper for fluent API
 * 
 * Example:
 * ```typescript
 * const result = await pipeline(api)
 *   .call('authenticate', id, creds)
 *   .call('getNetworkState')
 *   .call('computeMutualRecognition', 'alice', 'bob')
 *   .execute();
 * ```
 */
export class PipelineBuilder<T = any> {
  private steps: Array<{ method: string; args: any[] }> = [];
  
  constructor(
    private readonly stub: any, // RpcStub<T>
    private readonly cache?: LRUCache<string, any>
  ) {}
  
  /**
   * Add a method call to the pipeline
   */
  call(method: string, ...args: any[]): this {
    this.steps.push({ method, args });
    return this;
  }
  
  /**
   * Execute the pipeline and return the result
   */
  async execute<TResult = any>(): Promise<TResult> {
    // Build cache key if cache available
    const cacheKey = this.cache ? 
      `pipeline:${this.steps.map(s => `${s.method}(${s.args.join(',')})`).join('->')}` : 
      null;
    
    // Check cache
    if (cacheKey && this.cache?.has(cacheKey)) {
      console.log(`[PIPELINE-CACHE-HIT] ${cacheKey}`);
      return this.cache.get(cacheKey)!;
    }
    
    if (cacheKey) {
      console.log(`[PIPELINE-CACHE-MISS] ${cacheKey}`);
    }
    
    // Execute pipeline
    let current: any = this.stub;
    for (const step of this.steps) {
      current = (current as any)[step.method](...step.args);
    }
    
    // Await final result
    const result = await current;
    
    // Cache result
    if (cacheKey && this.cache) {
      this.cache.set(cacheKey, result);
    }
    
    return result;
  }
}

/**
 * Create a pipeline builder
 */
export function pipeline<T = any>(
  stub: any, // RpcStub<T>
  cache?: LRUCache<string, any>
): PipelineBuilder<T> {
  return new PipelineBuilder(stub, cache);
}
