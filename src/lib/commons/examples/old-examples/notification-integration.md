# PWA Notification Integration Examples

This document shows how to integrate PWA notifications with your existing P2P components.

## Basic Usage

The notification manager is automatically initialized in the main layout and made available globally via `window.notificationManager`.

## Integration with Existing Components

### 1. Chat Component Integration

```typescript
// In any component that needs notifications
import { browser } from '$app/environment';

let notificationManager: any = null;
let isWindowFocused = $state(true);

onMount(() => {
    // Get notification manager from global window
    if (browser && (window as any).notificationManager) {
        notificationManager = (window as any).notificationManager;
    }

    // Set up focus/blur listeners for smart notifications
    if (browser) {
        window.addEventListener('focus', () => {
            isWindowFocused = true;
            // Clear notifications when window is focused
            notificationManager?.clearNotification(`message-${roomId}`);
        });
        
        window.addEventListener('blur', () => {
            isWindowFocused = false;
        });
    }
});

// Show notification for new messages when window is not focused
function handleNewMessage(message, senderId, senderName) {
    if (!isWindowFocused && notificationManager) {
        notificationManager.onPeerMessage(message, senderId, senderName);
    }
}
```

### 2. Room/Peer Connection Integration

```typescript
// When a peer connects to a room
function handlePeerConnected(peerId, peerName) {
    if (notificationManager) {
        notificationManager.onPeerConnected(peerId, peerName);
    }
}

// When a peer disconnects
function handlePeerDisconnected(peerId, peerName) {
    if (notificationManager) {
        notificationManager.onConnectionLost(peerId, peerName);
    }
}

// When receiving a capacity offer
function handleCapacityOffer(capacity, peerId, peerName) {
    if (notificationManager && !isWindowFocused) {
        notificationManager.onCapacityOffered(capacity, peerId, peerName);
    }
}

// When receiving a room invitation
function handleRoomInvite(roomName, peerId, peerName) {
    if (notificationManager) {
        notificationManager.onRoomInvite(roomName, peerId, peerName);
    }
}
```

### 3. File Transfer Integration

```typescript
// When receiving a file
function handleFileReceived(filename, peerId, peerName) {
    if (notificationManager && !isWindowFocused) {
        notificationManager.onFileReceived(filename, peerId, peerName);
    }
}
```

## Available Notification Methods

The `P2PNotificationManager` provides these methods:

- `onPeerConnected(peerId, peerName?)` - When a peer connects
- `onPeerMessage(message, peerId, peerName?)` - When receiving a message
- `onFileReceived(filename, peerId, peerName?)` - When receiving a file
- `onConnectionLost(peerId, peerName?)` - When losing connection to a peer
- `onCapacityOffered(capacity, peerId, peerName?)` - When receiving a capacity offer
- `onRoomInvite(roomName, peerId, peerName?)` - When receiving a room invitation
- `clearNotification(tag)` - Clear a specific notification
- `clearAllNotifications()` - Clear all notifications
- `requestPermission()` - Request notification permission

## Smart Notification Strategy

The implementation includes smart notification logic:

1. **Window Focus Detection**: Only shows notifications when the window is not focused
2. **Auto-clear**: Clears notifications when the window regains focus
3. **Duplicate Prevention**: Prevents multiple notifications for the same event
4. **Permission Handling**: Gracefully handles notification permissions

## Service Worker Integration

The service worker handles:

- **Notification Display**: Shows notifications even when the app is in the background
- **Click Handling**: Opens appropriate app sections when notifications are clicked
- **Action Buttons**: Supports reply, view, and other actions
- **Badge Management**: Updates app badge with notification count

## Testing

To test notifications:

1. Open the app in two browser windows/tabs
2. Focus one window and blur the other
3. Send a message or trigger a P2P event from the focused window
4. The blurred window should show a notification

## Browser Support

- **Chrome/Edge**: Full support including background notifications
- **Firefox**: Full support with some limitations on mobile
- **Safari**: Limited support, requires user interaction to enable
- **Mobile**: Works best when app is added to home screen (PWA mode)

## Troubleshooting

### Notifications not showing:
1. Check if permission is granted: `Notification.permission === 'granted'`
2. Ensure service worker is registered: Check browser dev tools
3. Verify PWA manifest is loaded: Check Network tab for manifest.json

### Service worker issues:
1. Clear browser cache and reload
2. Check for console errors in service worker
3. Verify workbox is properly configured

### Permission issues:
1. Call `notificationManager.requestPermission()` on user interaction
2. Check browser notification settings
3. Some browsers block notifications on localhost (use HTTPS) 