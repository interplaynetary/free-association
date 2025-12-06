/**
 * Common transport interface types
 */

export interface TransportMessage {
  type: 'call' | 'result' | 'error' | 'capability';
  id?: number;
  data: unknown;
}

export interface TransportOptions {
  reconnect?: boolean;
  reconnectDelay?: number;
  heartbeatInterval?: number;
  timeout?: number;
}

export interface Transport {
  send(message: TransportMessage): Promise<void>;
  onMessage(handler: (message: TransportMessage) => void): void;
  connect(): Promise<void>;
  disconnect(): Promise<void>;
  isConnected(): boolean;
}

