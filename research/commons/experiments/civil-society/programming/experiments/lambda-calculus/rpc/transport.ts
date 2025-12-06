/**
 * Unified Transport Interface (Cap'n Web Style)
 * 
 * Simple, clean interface for all transports.
 * WebSocket, HTTP, postMessage all implement the same interface!
 * 
 * Much simpler than our previous transport types.
 */

import { RpcJSON } from './json-rpc';

// ============================================================================
// Core Transport Interface
// ============================================================================

/**
 * Unified transport interface
 * 
 * All transports (WebSocket, HTTP, postMessage, WebRTC) implement this.
 */
export interface Transport {
  /**
   * Send a message
   */
  send(message: unknown): void;

  /**
   * Register message handler
   */
  onMessage(handler: (message: unknown) => void): void;

  /**
   * Close the transport
   */
  close(): void;

  /**
   * Check if transport is open
   */
  isOpen(): boolean;

  /**
   * Transport name (for debugging)
   */
  readonly name: string;
}

// ============================================================================
// WebSocket Transport (Minimal Implementation)
// ============================================================================

/**
 * Create WebSocket transport with automatic serialization
 * 
 * User never sees RpcJSON - it just works!
 */
export function createWebSocketTransport(url: string): Transport {
  const ws = new WebSocket(url);
  let handler: ((message: unknown) => void) | null = null;

  ws.onmessage = (event) => {
    if (handler) {
      try {
        // Automatic deserialization!
        const message = RpcJSON.parse(event.data);
        handler(message);
      } catch (error) {
        console.error('Failed to deserialize message:', error);
      }
    }
  };

  return {
    name: 'websocket',

    send(message: unknown) {
      if (ws.readyState === WebSocket.OPEN) {
        try {
          // Automatic serialization!
          ws.send(RpcJSON.stringify(message));
        } catch (error) {
          console.error('Failed to serialize message:', error);
        }
      }
    },

    onMessage(h) {
      handler = h;
    },

    close() {
      ws.close();
    },

    isOpen() {
      return ws.readyState === WebSocket.OPEN;
    }
  };
}

// ============================================================================
// postMessage Transport (Minimal Implementation)
// ============================================================================

/**
 * Create postMessage transport with automatic serialization
 */
export function createPostMessageTransport(target: Window | Worker): Transport {
  let handler: ((message: unknown) => void) | null = null;

  const messageHandler = (event: MessageEvent) => {
    if (handler && event.data?._rpc) {
      try {
        // Automatic deserialization!
        const message = RpcJSON.fromObject(event.data.message);
        handler(message);
      } catch (error) {
        console.error('Failed to deserialize postMessage:', error);
      }
    }
  };

  if (typeof window !== 'undefined') {
    window.addEventListener('message', messageHandler);
  }

  return {
    name: 'postmessage',

    send(message: unknown) {
      try {
        // Automatic serialization!
        target.postMessage({
          _rpc: true,
          message: RpcJSON.toObject(message)
        }, '*');
      } catch (error) {
        console.error('Failed to serialize postMessage:', error);
      }
    },

    onMessage(h) {
      handler = h;
    },

    close() {
      if (typeof window !== 'undefined') {
        window.removeEventListener('message', messageHandler);
      }
    },

    isOpen() {
      return true; // Always "open" for postMessage
    }
  };
}

// ============================================================================
// HTTP Transport (For Batch Mode)
// ============================================================================

/**
 * Create HTTP transport with automatic serialization (batch mode)
 */
export function createHttpTransport(url: string): Transport {
  const queue: unknown[] = [];
  let handler: ((message: unknown) => void) | null = null;

  return {
    name: 'http',

    send(message: unknown) {
      queue.push(message);
    },

    onMessage(h) {
      handler = h;
    },

    async close() {
      // Flush queue
      if (queue.length > 0 && handler) {
        try {
          // Automatic serialization!
          const response = await fetch(url, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: RpcJSON.stringify({ batch: queue })
          });

          if (!response.ok) {
            throw new Error(`HTTP ${response.status}: ${response.statusText}`);
          }

          // Automatic deserialization!
          const results = RpcJSON.parse(await response.text());
          
          if (Array.isArray(results)) {
            for (const result of results) {
              handler(result);
            }
          }

          queue.length = 0;
        } catch (error) {
          console.error('Failed to flush HTTP batch:', error);
        }
      }
    },

    isOpen() {
      return true;
    }
  };
}

// ============================================================================
// Local Transport (For Testing)
// ============================================================================

export function createLocalTransport(): { client: Transport; server: Transport } {
  let clientHandler: ((message: unknown) => void) | null = null;
  let serverHandler: ((message: unknown) => void) | null = null;

  const client: Transport = {
    name: 'local-client',
    send(message) {
      if (serverHandler) {
        setTimeout(() => serverHandler!(message), 0);
      }
    },
    onMessage(h) {
      clientHandler = h;
    },
    close() {},
    isOpen: () => true
  };

  const server: Transport = {
    name: 'local-server',
    send(message) {
      if (clientHandler) {
        setTimeout(() => clientHandler!(message), 0);
      }
    },
    onMessage(h) {
      serverHandler = h;
    },
    close() {},
    isOpen: () => true
  };

  return { client, server };
}

