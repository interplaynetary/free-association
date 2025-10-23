# Gun Relay Server for Decentralized Applications

This project includes a Gun relay server built with TypeScript and Bun to facilitate peer discovery and data synchronization for decentralized applications.

## What is a Gun Relay Server?

A Gun relay server serves several important purposes in the Gun ecosystem:

1. **Peer Discovery**: It helps peers (browsers, devices) find each other on the network.
2. **Signaling**: It assists with WebRTC connection establishment between peers.
3. **Data Persistence**: It can store data persistently, ensuring it remains available even when all clients disconnect.
4. **Mesh Network Relay**: It helps relay messages between peers that can't establish direct connections.

## Understanding Gun's P2P Mechanism

Gun uses a hybrid peer-to-peer approach:

1. **HTTP/WebSocket Relay Servers**: These are traditional servers (like our Bun server) that serve as always-available connection points.

2. **WebRTC Direct Connections**: When two browsers access the same relay server, they can establish a direct peer-to-peer WebRTC connection, bypassing the server for future data transfer.

3. **DAM (Daisy-chain Ad-hoc Mesh-network)**: If direct WebRTC connections aren't possible (e.g., due to firewalls), Gun will relay messages through available network paths.

In practice, the flow works like this:

1. Browsers connect to known relay servers via HTTP/WebSocket
2. The relay servers help browsers discover each other
3. When possible, browsers establish direct WebRTC connections
4. Data synchronizes across all connected peers in a mesh formation

## Running the Server

The Gun relay server is written in TypeScript and runs with Bun for optimal performance.

### Prerequisites

- [Bun](https://bun.sh/) installed on your system

### Starting the Server

```bash
# Navigate to the project directory
cd free

# Install dependencies if you haven't already
bun install

# Start the Gun relay server
bun run gun-relay
```

The server will start on port 8765 and will be available at `http://localhost:8765/gun`.

### Server Features

- **Health Check**: You can verify the server is running by accessing `http://localhost:8765/health`
- **Data Persistence**: Data is stored in the `gun-data` directory using Radisk storage
- **Connection Logging**: The server logs peer connections and disconnections
- **TypeScript Support**: Fully typed for better developer experience

### Bun Compatibility Note

The Gun relay server is configured to work with Bun using the valid Gun options. We're using a custom HTTP handler for Bun's server to ensure proper communication with Gun clients.

## How It Works with the Application

The application is configured to connect to this relay server as well as several public relays:

```typescript
// from free/src/utils/gun/gunSetup.ts
export const gun = Gun({
  peers: [
    'http://localhost:8765/gun', // Local Bun relay peer
    'https://gun-manhattan.herokuapp.com/gun', // Public relay peer
    'https://gun-us.herokuapp.com/gun', 
    'https://gun-eu.herokuapp.com/gun'
  ],
  localStorage: true // Browser persistence
});
```

Data flows as follows:

1. When you make changes in the application, they're sent to all configured peers
2. The local relay server helps ensure data is persisted and available to other devices on your network
3. Public relays help with global synchronization if needed
4. WebRTC connections between browsers will be established when possible through the signaling servers

## Checking Connection Status

You can use the following code to check the connection status with peers:

```javascript
// Get the current peers
const peers = gun._.opt.peers;

// Count connected peers
let connectedPeers = 0;
Object.values(peers).forEach((peer) => {
  // @ts-ignore - Accessing internal properties
  if (peer && peer.wire && peer.wire.readyState === 1) {
    connectedPeers++;
  }
});

console.log(`Connected to ${connectedPeers} peers`);
```

If you need to force reconnection to peers:

```javascript
// Re-add peers to trigger connection
gun.opt({
  peers: [
    'http://localhost:8765/gun',
    'https://gun-manhattan.herokuapp.com/gun'
  ]
});

// Send a small message to activate connections
gun.get('heartbeat').put({ time: Date.now() });
```

## Testing Multi-Device Synchronization

To test synchronization between multiple devices:

1. Start the relay server as described above
2. Run your application with `bun run dev`
3. On multiple devices on the same network, access the application using your local IP:
   - E.g., `http://192.168.1.x:5173` (where x is your computer's IP)
4. Changes made on one device should propagate to others

## Troubleshooting

- **No Synchronization**: Ensure the relay server is running and check the console for connection logs
- **Slow Updates**: Try adding more relays or check network conditions
- **Connection Errors**: Verify firewall settings aren't blocking WebSocket connections
- **WebRTC Issues**: If WebRTC connections aren't establishing, check that your network allows UDP traffic

## Extending the Server

You can extend the server by editing the `server.ts` file. Common customizations include:

- Adding authentication
- Implementing custom data validation
- Setting up additional routes for application-specific needs
- Configuring backup and replication strategies 