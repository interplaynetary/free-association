/**
 * WebSocket Transport Adapter
 * 
 * Provides WebSocket-based transport for Cap'n Web RPC.
 * Supports:
 * - Auto-reconnection
 * - Heartbeat/keep-alive
 * - Message queueing during reconnect
 */

import type { Transport, TransportMessage, TransportOptions } from './types';

export class WebSocketTransport implements Transport {
  private ws: WebSocket | null = null;
  private url: string;
  private options: TransportOptions;
  private messageHandler: ((message: TransportMessage) => void) | null = null;
  private messageQueue: TransportMessage[] = [];
  private reconnectTimer?: ReturnType<typeof setTimeout>;
  private heartbeatTimer?: ReturnType<typeof setInterval>;

  constructor(url: string, options: TransportOptions = {}) {
    this.url = url;
    this.options = {
      reconnect: true,
      reconnectDelay: 1000,
      heartbeatInterval: 30000,
      timeout: 5000,
      ...options
    };
  }

  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          console.log('[WebSocket] Connected to', this.url);
          
          // Flush message queue
          while (this.messageQueue.length > 0) {
            const msg = this.messageQueue.shift();
            if (msg) this.send(msg);
          }

          // Start heartbeat
          if (this.options.heartbeatInterval) {
            this.startHeartbeat();
          }

          resolve();
        };

        this.ws.onmessage = (event) => {
          try {
            const message = JSON.parse(event.data) as TransportMessage;
            this.messageHandler?.(message);
          } catch (error) {
            console.error('[WebSocket] Failed to parse message:', error);
          }
        };

        this.ws.onerror = (error) => {
          console.error('[WebSocket] Error:', error);
          reject(error);
        };

        this.ws.onclose = () => {
          console.log('[WebSocket] Disconnected');
          this.stopHeartbeat();
          
          // Auto-reconnect if enabled
          if (this.options.reconnect) {
            this.scheduleReconnect();
          }
        };
      } catch (error) {
        reject(error);
      }
    });
  }

  async disconnect(): Promise<void> {
    this.options.reconnect = false; // Disable auto-reconnect
    this.stopHeartbeat();
    
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
    }

    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  async send(message: TransportMessage): Promise<void> {
    if (!this.isConnected()) {
      // Queue message for later
      this.messageQueue.push(message);
      return;
    }

    try {
      this.ws!.send(JSON.stringify(message));
    } catch (error) {
      console.error('[WebSocket] Failed to send message:', error);
      // Re-queue on failure
      this.messageQueue.push(message);
      throw error;
    }
  }

  onMessage(handler: (message: TransportMessage) => void): void {
    this.messageHandler = handler;
  }

  isConnected(): boolean {
    return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
  }

  private scheduleReconnect(): void {
    if (this.reconnectTimer) return;

    console.log(`[WebSocket] Reconnecting in ${this.options.reconnectDelay}ms...`);
    
    this.reconnectTimer = setTimeout(() => {
      this.reconnectTimer = undefined;
      this.connect().catch((error) => {
        console.error('[WebSocket] Reconnect failed:', error);
        // Will schedule another reconnect via onclose
      });
    }, this.options.reconnectDelay);
  }

  private startHeartbeat(): void {
    this.heartbeatTimer = setInterval(() => {
      if (this.isConnected()) {
        this.send({ type: 'call', data: 'ping' }).catch(() => {
          // Heartbeat failed - connection likely dead
          console.warn('[WebSocket] Heartbeat failed');
        });
      }
    }, this.options.heartbeatInterval);
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = undefined;
    }
  }
}

/**
 * Create WebSocket transport
 * Convenience factory function
 */
export function createWebSocketTransport(
  url: string,
  options?: TransportOptions
): WebSocketTransport {
  return new WebSocketTransport(url, options);
}

