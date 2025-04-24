import { gun } from '../gunSetup';
import { writable } from 'svelte/store';

// Create a store for connection status
export interface ConnectionStatus {
  connected: boolean;
  peerCount: number;
  statusText: string;
}

const initialStatus: ConnectionStatus = {
  connected: false,
  peerCount: 0,
  statusText: 'Waiting for peers...'
};

// Create a readable store for connection status
export const connectionStatus = writable<ConnectionStatus>(initialStatus);

// Function to count connected peers
function countConnectedPeers(): number {
  try {
    const peers = (gun as any)._.opt?.peers;
    if (!peers) return 0;
    
    let count = 0;
    Object.values(peers).forEach((peer: any) => {
      if (peer && peer.wire && peer.wire.readyState === 1) {
        count++;
      }
    });
    
    return count;
  } catch (err) {
    console.error('Error counting connected peers:', err);
    return 0;
  }
}

// Update connection status based on peer count
function updateConnectionStatus(): void {
  const peerCount = countConnectedPeers();
  const connected = peerCount > 0;
  
  connectionStatus.set({
    connected,
    peerCount,
    statusText: connected 
      ? `Connected to ${peerCount} peer${peerCount !== 1 ? 's' : ''}`
      : 'Disconnected from all peers'
  });
}

// Force reconnect to peers
export function reconnectToPeers(): void {
  try {
    console.log('[Gun Connection] Attempting to reconnect to peers...');
    
    // Re-add peers to trigger connection
    gun.opt({
      peers: [
        'http://localhost:8765/gun'
      ]
    });
    
    // Send a heartbeat to activate connections
    gun.get('heartbeat').put({ time: Date.now() });
    
    // Update status after a short delay
    setTimeout(updateConnectionStatus, 1000);
  } catch (err) {
    console.error('[Gun Connection] Error reconnecting to peers:', err);
  }
}

// Initialize the connection manager
let intervalId: any;

export function initConnectionManager(): () => void {
  console.log('[Gun Connection] Initializing connection manager');
  
  // Set up periodic connection status check
  intervalId = setInterval(updateConnectionStatus, 2000);
  
  // Track connected peers
  gun.on('hi', (peer: any) => {
    console.log('[Gun Connection] Peer connected:', peer);
    updateConnectionStatus();
  });
  
  gun.on('bye', (peer: any) => {
    console.log('[Gun Connection] Peer disconnected:', peer);
    updateConnectionStatus();
  });
  
  // Initial connection attempt
  reconnectToPeers();
  
  // Return cleanup function
  return () => {
    console.log('[Gun Connection] Cleaning up connection manager');
    clearInterval(intervalId);
    intervalId = null;
  };
}

// Export a function to manually check connection status
export function checkConnectionStatus(): void {
  updateConnectionStatus();
} 