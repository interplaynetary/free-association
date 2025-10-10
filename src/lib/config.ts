// Configuration for Gun and Holster peer connections
// Uses environment variables from .env files

export const config = {
  gun: {
    peers: [
      // Use environment variable or fallback to defaults
      import.meta.env.VITE_GUN_PEER_URL || 'http://localhost:8765/gun',
      // Keep single external peer as fallback (matching current branch)
      'https://104.248.129.153/gun'
    ],
    localStorage: false,
    radisk: true
  },
  holster: {
    peers: [
      // Use environment variable or fallback to defaults
      import.meta.env.VITE_HOLSTER_PEER_URL || 'ws://localhost:8766/holster',
      // Keep external peer as fallback
      'wss://holster.haza.website'
    ],
    indexedDB: true,
    secure: true
  },
  dataApi: {
    url: import.meta.env.VITE_DATA_API_URL || 'http://localhost:8767'
  }
};
