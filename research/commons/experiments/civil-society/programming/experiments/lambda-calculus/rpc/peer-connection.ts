/**
 * Peer Connection - Symmetric RPC Connection
 * 
 * Manages symmetric connection between two EntitySession instances.
 * Both sides export their EntitySession at ID 0 and can call each other.
 * 
 * Features:
 * - Symmetric authentication
 * - Capability exchange
 * - Auto-sync support
 * - Reconnection handling
 * - Connection state management
 */

import { EntitySession, type EntitySessionConfig } from './entity-session';
import { CapabilityManager } from './capability-manager';
import { BrowserStorage } from './browser-storage';
import { RecognitionCache } from './cache';
import type {
  TransportType,
  TransportConfig,
  ConnectionInfo,
  ConnectionState,
  Credential,
  SyncUpdate
} from './types';
import { AuthenticationError } from './types';

/**
 * Peer Connection Configuration
 */
export interface PeerConnectionConfig {
  localEntityId: string;
  storage?: BrowserStorage;
  cache?: RecognitionCache;
  transport: TransportConfig;
  autoSync?: boolean;
  reconnect?: boolean;
}

/**
 * Remote session stub
 * Represents the remote EntitySession's API
 */
export interface RemoteEntitySession {
  verifyIdentity(proof: Credential): Promise<boolean>;
  allocateRecognition(targetId: string, amount: number): Promise<void>;
  revokeRecognition(targetId: string): Promise<void>;
  getMutualRecognition(otherId: string): Promise<number>;
  getMRS(universeIds: string[]): Promise<any>;
  getTMR(universeIds: string[]): Promise<number>;
  getMRD(collectiveMembers: string[]): Promise<number>;
  getMyAllocations(): Promise<any>;
  subscribeSyncUpdates(callback: (update: SyncUpdate) => void): Promise<void>;
}

/**
 * Peer Connection class
 * Symmetric protocol - both sides use this to connect
 */
export class PeerConnection {
  private localSession: EntitySession;
  private remoteSession: RemoteEntitySession | null = null;
  private capabilityManager: CapabilityManager;
  private storage: BrowserStorage;
  private connectionInfo: ConnectionInfo;
  private messageHandler: ((message: any) => void) | null = null;

  // For demo purposes - in real implementation would use actual Cap'n Web RPC
  private mockTransport: {
    send: (message: any) => void;
    onMessage: (handler: (message: any) => void) => void;
  } | null = null;

  private constructor(
    localSession: EntitySession,
    storage: BrowserStorage,
    capabilityManager: CapabilityManager,
    connectionInfo: ConnectionInfo
  ) {
    this.localSession = localSession;
    this.storage = storage;
    this.capabilityManager = capabilityManager;
    this.connectionInfo = connectionInfo;
  }

  /**
   * Create and connect to peer
   * This is called by both sides symmetrically
   */
  static async connect(config: PeerConnectionConfig): Promise<PeerConnection> {
    // Initialize storage if not provided
    const storage = config.storage || new BrowserStorage(config.localEntityId);
    if (!config.storage) {
      await storage.initialize();
    }

    // Initialize cache
    const cache = config.cache || new RecognitionCache();

    // Create local session
    const localSession = new EntitySession({
      entityId: config.localEntityId,
      storage,
      cache,
      autoSync: config.autoSync
    });

    // Create capability manager
    const capabilityManager = new CapabilityManager();

    // Export local session at ID 0
    capabilityManager.exportMain(localSession);

    // Create connection info
    const connectionInfo: ConnectionInfo = {
      localEntityId: config.localEntityId,
      state: 'connecting',
      transport: config.transport.type,
      connectedAt: undefined,
      authenticatedAt: undefined,
      lastActivity: Date.now()
    };

    const connection = new PeerConnection(
      localSession,
      storage,
      capabilityManager,
      connectionInfo
    );

    // Setup transport
    await connection.setupTransport(config.transport);

    return connection;
  }

  /**
   * Setup transport layer
   * In a real implementation, this would use actual Cap'n Web RPC transports
   */
  private async setupTransport(config: TransportConfig): Promise<void> {
    // For demo purposes, create a mock transport
    // In real implementation, would use:
    // - newWebSocketRpcSession for WebSocket
    // - newPostMessageRpcSession for postMessage
    // - newWebRTCRpcSession for WebRTC
    // - newHttpBatchRpcSession for HTTP batch mode

    this.mockTransport = {
      send: (message: any) => {
        // In real implementation, serialize and send over transport
        console.log('[Transport] Sending:', message);
      },
      onMessage: (handler: (message: any) => void) => {
        this.messageHandler = handler;
      }
    };

    // Create remote session stub
    this.remoteSession = this.createRemoteStub();

    // Import remote session at ID 0
    this.capabilityManager.importMain(this.remoteSession);

    // Mark as connected
    this.connectionInfo.state = 'connected';
    this.connectionInfo.connectedAt = Date.now();
  }

  /**
   * Create stub for remote EntitySession
   * In real implementation, this would be auto-generated by Cap'n Web RPC
   */
  private createRemoteStub(): RemoteEntitySession {
    return {
      verifyIdentity: async (proof: Credential): Promise<boolean> => {
        return this.call('verifyIdentity', [proof]);
      },

      allocateRecognition: async (targetId: string, amount: number): Promise<void> => {
        return this.call('allocateRecognition', [targetId, amount]);
      },

      revokeRecognition: async (targetId: string): Promise<void> => {
        return this.call('revokeRecognition', [targetId]);
      },

      getMutualRecognition: async (otherId: string): Promise<number> => {
        return this.call('getMutualRecognition', [otherId]);
      },

      getMRS: async (universeIds: string[]): Promise<any> => {
        return this.call('getMRS', [universeIds]);
      },

      getTMR: async (universeIds: string[]): Promise<number> => {
        return this.call('getTMR', [universeIds]);
      },

      getMRD: async (collectiveMembers: string[]): Promise<number> => {
        return this.call('getMRD', [collectiveMembers]);
      },

      getMyAllocations: async (): Promise<any> => {
        return this.call('getMyAllocations', []);
      },

      subscribeSyncUpdates: async (callback: (update: SyncUpdate) => void): Promise<void> => {
        // Export callback as capability
        const callbackId = this.capabilityManager.exportLocal(callback, 'function');
        return this.call('subscribeSyncUpdates', [['ref', callbackId]]);
      }
    };
  }

  /**
   * Make RPC call to remote peer
   * Simulates Cap'n Web RPC protocol
   */
  private async call(method: string, args: any[]): Promise<any> {
    // In real implementation, this would:
    // 1. Serialize args (handling capability references)
    // 2. Send ["push", ["pipeline", 0, method, args]]
    // 3. Wait for response
    // 4. Deserialize and return result

    // Mock implementation
    const message = {
      type: 'call',
      targetId: 0, // Remote session
      method,
      args
    };

    this.mockTransport?.send(message);

    // Simulate async response
    return new Promise((resolve) => {
      setTimeout(() => resolve(null), 10);
    });
  }

  // ============================================================================
  // Authentication (Symmetric)
  // ============================================================================

  /**
   * Perform mutual authentication
   * Both sides call each other's verifyIdentity
   */
  async mutualAuthenticate(
    myProof: Credential,
    verifyPeerFn?: (proof: Credential) => boolean
  ): Promise<void> {
    if (!this.remoteSession) {
      throw new Error('Not connected');
    }

    // Call remote's verifyIdentity with our proof
    const remoteVerified = await this.remoteSession.verifyIdentity(myProof);

    if (!remoteVerified) {
      throw new AuthenticationError('Remote peer rejected our proof');
    }

    // Remote will call our verifyIdentity (symmetric!)
    // This happens automatically through RPC

    // Mark as authenticated
    this.localSession.markAuthenticated();
    this.connectionInfo.state = 'authenticated';
    this.connectionInfo.authenticatedAt = Date.now();
  }

  /**
   * Check if authenticated
   */
  isAuthenticated(): boolean {
    return this.connectionInfo.state === 'authenticated';
  }

  // ============================================================================
  // Session Access
  // ============================================================================

  /**
   * Get local session
   * Use this to make changes to own recognition
   */
  getLocalSession(): EntitySession {
    return this.localSession;
  }

  /**
   * Get remote session stub
   * Use this to query remote peer's data
   */
  getRemoteSession(): RemoteEntitySession {
    if (!this.remoteSession) {
      throw new Error('Not connected');
    }
    return this.remoteSession;
  }

  // ============================================================================
  // Auto-Sync
  // ============================================================================

  /**
   * Enable auto-sync
   * Automatically sends updates to remote peer
   */
  async enableAutoSync(): Promise<void> {
    if (!this.remoteSession) {
      throw new Error('Not connected');
    }

    // Subscribe to local changes
    await this.localSession.subscribeSyncUpdates(async (update: SyncUpdate) => {
      // In real implementation, would send update via RPC
      console.log('[Auto-sync] Sending update:', update);
    });

    // Subscribe to remote changes
    await this.remoteSession.subscribeSyncUpdates(async (update: SyncUpdate) => {
      // Receive and apply remote updates
      await this.localSession.receiveSyncUpdate(update);
    });

    // Process any queued operations
    await this.localSession.processSyncQueue(async (update: SyncUpdate) => {
      console.log('[Sync queue] Processing:', update);
    });
  }

  // ============================================================================
  // Connection Management
  // ============================================================================

  /**
   * Get connection info
   */
  getConnectionInfo(): ConnectionInfo {
    return { ...this.connectionInfo };
  }

  /**
   * Check if connected
   */
  isConnected(): boolean {
    return this.connectionInfo.state === 'connected' || 
           this.connectionInfo.state === 'authenticated';
  }

  /**
   * Disconnect from peer
   */
  async disconnect(): Promise<void> {
    this.connectionInfo.state = 'disconnected';
    this.capabilityManager.releaseAll();
    this.remoteSession = null;
    await this.localSession.close();
  }

  /**
   * Get capability manager (for debugging)
   */
  getCapabilityManager(): CapabilityManager {
    return this.capabilityManager;
  }

  /**
   * Get storage (for debugging)
   */
  getStorage(): BrowserStorage {
    return this.storage;
  }
}

/**
 * Helper to create a simple peer-to-peer connection
 * Convenient wrapper around PeerConnection.connect
 */
export async function createP2PConnection(
  localEntityId: string,
  transport: TransportType = 'websocket',
  url?: string
): Promise<PeerConnection> {
  const config: PeerConnectionConfig = {
    localEntityId,
    transport: {
      type: transport,
      url,
      options: {
        reconnect: true,
        reconnectDelay: 1000,
        heartbeatInterval: 30000
      }
    },
    autoSync: true,
    reconnect: true
  };

  return PeerConnection.connect(config);
}

