/**
 * PostMessage Transport Adapter
 * 
 * Provides postMessage-based transport for Cap'n Web RPC.
 * Use cases:
 * - iframe communication
 * - Web Worker communication
 * - Same-origin scenarios
 */

import type { Transport, TransportMessage, TransportOptions } from './types';

export class PostMessageTransport implements Transport {
  private target: Window | Worker | MessagePort;
  private origin: string;
  private messageHandler: ((message: TransportMessage) => void) | null = null;
  private connected: boolean = false;

  constructor(
    target: Window | Worker | MessagePort,
    origin: string = '*'
  ) {
    this.target = target;
    this.origin = origin;
  }

  async connect(): Promise<void> {
    // Setup message listener
    if ('addEventListener' in this.target) {
      this.target.addEventListener('message', this.handleMessage);
    }

    this.connected = true;
    console.log('[PostMessage] Connected');
  }

  async disconnect(): Promise<void> {
    if ('removeEventListener' in this.target) {
      this.target.removeEventListener('message', this.handleMessage);
    }

    this.connected = false;
    console.log('[PostMessage] Disconnected');
  }

  async send(message: TransportMessage): Promise<void> {
    if (!this.connected) {
      throw new Error('Not connected');
    }

    try {
      if ('postMessage' in this.target) {
        if (this.target instanceof Window) {
          this.target.postMessage(message, this.origin);
        } else {
          this.target.postMessage(message);
        }
      }
    } catch (error) {
      console.error('[PostMessage] Failed to send message:', error);
      throw error;
    }
  }

  onMessage(handler: (message: TransportMessage) => void): void {
    this.messageHandler = handler;
  }

  isConnected(): boolean {
    return this.connected;
  }

  private handleMessage = (event: MessageEvent): void => {
    // Verify origin if specified
    if (this.origin !== '*' && event.origin !== this.origin) {
      console.warn('[PostMessage] Rejected message from wrong origin:', event.origin);
      return;
    }

    try {
      const message = event.data as TransportMessage;
      this.messageHandler?.(message);
    } catch (error) {
      console.error('[PostMessage] Failed to handle message:', error);
    }
  };
}

/**
 * Create postMessage transport for iframe
 */
export function createIframeTransport(
  iframe: HTMLIFrameElement,
  origin: string = '*'
): PostMessageTransport {
  if (!iframe.contentWindow) {
    throw new Error('Iframe has no contentWindow');
  }
  return new PostMessageTransport(iframe.contentWindow, origin);
}

/**
 * Create postMessage transport for Worker
 */
export function createWorkerTransport(worker: Worker): PostMessageTransport {
  return new PostMessageTransport(worker);
}

