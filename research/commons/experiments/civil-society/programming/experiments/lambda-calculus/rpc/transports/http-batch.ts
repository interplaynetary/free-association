/**
 * HTTP Batch Mode Transport
 * 
 * Inspired by Cap'n Web's HTTP batch mode:
 * - Lightweight alternative to WebSocket
 * - Multiple calls in single HTTP request/response
 * - Good for one-time operations without persistent connection
 * 
 * Usage:
 * ```typescript
 * let batch = newHttpBatchSession('https://api.example.com');
 * let p1 = batch.getMRS(['alice']);
 * let p2 = batch.getMRD(['bob']);
 * let [mrs, mrd] = await Promise.all([p1, p2]);
 * // → Single HTTP POST with both calls
 * ```
 */

interface BatchCall {
  method: string;
  args: unknown[];
  resolve: (value: unknown) => void;
  reject: (error: Error) => void;
}

/**
 * HTTP Batch Session
 * Queues calls and sends them all in a single HTTP request
 */
export class HttpBatchSession {
  private url: string;
  private calls: BatchCall[] = [];
  private batched = false;
  private executing = false;

  constructor(url: string) {
    this.url = url;
  }

  /**
   * Make an RPC call (queued until batch executes)
   */
  call(method: string, ...args: unknown[]): Promise<unknown> {
    if (this.batched) {
      throw new Error('Batch already sent. Create a new batch for more calls.');
    }

    return new Promise((resolve, reject) => {
      this.calls.push({ method, args, resolve, reject });

      // Execute batch on next microtask
      if (!this.executing) {
        this.executing = true;
        queueMicrotask(() => this.executeBatch());
      }
    });
  }

  /**
   * Execute the batch (sends HTTP request)
   */
  private async executeBatch(): Promise<void> {
    if (this.batched || this.calls.length === 0) return;

    this.batched = true;
    const calls = [...this.calls];

    try {
      // Send all calls in single HTTP POST
      const response = await fetch(this.url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          type: 'batch',
          calls: calls.map(c => ({
            method: c.method,
            args: c.args
          }))
        })
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }

      const results = await response.json();

      // Resolve each call with its result
      for (let i = 0; i < calls.length; i++) {
        const call = calls[i];
        const result = results[i];

        if (result.error) {
          call.reject(new Error(result.error));
        } else {
          call.resolve(result.value);
        }
      }
    } catch (error) {
      // Reject all pending calls
      for (const call of calls) {
        call.reject(error instanceof Error ? error : new Error(String(error)));
      }
    }
  }
}

/**
 * Create a new HTTP batch session (Cap'n Web style!)
 * 
 * @example
 * ```typescript
 * let batch = newHttpBatchSession('https://api.example.com');
 * let result = await batch.hello('World');
 * ```
 */
export function newHttpBatchSession(url: string): any {
  const session = new HttpBatchSession(url);

  // Return proxy that forwards method calls to session.call()
  return new Proxy({}, {
    get(_, method: string) {
      return (...args: unknown[]) => session.call(method, ...args);
    }
  });
}

