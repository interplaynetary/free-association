/**
 * Unified WebSocket Message Handler
 * 
 * DRY principle: Single place for WebSocket message handling logic.
 * Works with any WebSocket implementation (native, ws, uWebSockets).
 */

import { RpcJSON } from '../json-rpc';
import { RelayServer } from '../relay-server';
import { dispatchRpcCall, RpcRequest, RpcResponse } from './rpc-dispatcher';

export interface WebSocketAdapter {
  send(data: string): void;
  close(): void;
}

export type MessageType = 'register' | 'connect' | 'disconnect' | 'rpc' | 'subscribe';

export interface RelayMessage {
  type: MessageType;
  entityId?: string;
  fromId?: string;
  toId?: string;
  method?: string;
  params?: any[];
  id?: string | number;
}

/**
 * Handle a WebSocket message for a RelayServer
 */
export async function handleRelayMessage(
  ws: WebSocketAdapter,
  messageData: string | Buffer,
  relay: RelayServer
): Promise<void> {
  try {
    const message: RelayMessage = RpcJSON.parse(messageData.toString());

    switch (message.type) {
      case 'register':
        if (!message.entityId) {
          ws.send(RpcJSON.stringify({ type: 'error', error: 'Missing entityId' }));
          return;
        }
        await relay.register(message.entityId);
        ws.send(RpcJSON.stringify({
          type: 'registered',
          entityId: message.entityId
        }));
        break;

      case 'connect':
        if (!message.fromId || !message.toId) {
          ws.send(RpcJSON.stringify({ type: 'error', error: 'Missing fromId or toId' }));
          return;
        }
        await relay.connect(message.fromId, message.toId);
        ws.send(RpcJSON.stringify({
          type: 'connected',
          fromId: message.fromId,
          toId: message.toId
        }));
        break;

      case 'disconnect':
        if (!message.fromId || !message.toId) {
          ws.send(RpcJSON.stringify({ type: 'error', error: 'Missing fromId or toId' }));
          return;
        }
        await relay.disconnect(message.fromId, message.toId);
        ws.send(RpcJSON.stringify({
          type: 'disconnected',
          fromId: message.fromId,
          toId: message.toId
        }));
        break;

      case 'rpc':
        // Handle RPC call to relay or to a session
        const rpcRequest: RpcRequest = {
          id: message.id,
          method: message.method!,
          params: message.params
        };

        let target: any = relay;
        
        // If entityId is specified, get that session
        if (message.entityId) {
          target = relay.getSession(message.entityId);
          if (!target) {
            ws.send(RpcJSON.stringify({
              type: 'error',
              id: message.id,
              error: `Session not found: ${message.entityId}`
            }));
            return;
          }
        }

        const response = await dispatchRpcCall(target, rpcRequest);
        ws.send(RpcJSON.stringify({
          type: 'rpc-response',
          ...response
        }));
        break;

      default:
        ws.send(RpcJSON.stringify({
          type: 'error',
          error: `Unknown message type: ${(message as any).type}`
        }));
    }
  } catch (error: any) {
    ws.send(RpcJSON.stringify({
      type: 'error',
      error: error.message || 'Internal error'
    }));
  }
}

