# Reactive Programming with Gun Subscriptions

This guide demonstrates how to use the reactive programming capabilities of our enhanced `GunSubscription` class to create powerful, composable data flows in your Gun applications.

## Introduction to Reactive Programming

Reactive programming is a declarative programming paradigm concerned with data streams and the propagation of change. It allows you to express static or dynamic data flows with ease, and provides a powerful framework for managing asynchronous events.

Our enhanced `GunSubscription` class turns Gun's real-time database into a full-featured reactive programming framework, similar to RxJS but tailored specifically for Gun's graph database.

## Core Concepts

Before diving into the examples, let's understand the core concepts:

1. **Streams**: Continuous flows of data that you can subscribe to
2. **Operators**: Functions that transform or combine streams 
3. **Subscriptions**: Connections to streams that receive values
4. **Composition**: Building complex data flows by combining simpler ones

## Basic Usage

First, let's start with the basic subscription pattern:

```typescript
import { GunNode } from './GunNode';

// Create a node reference
const userNode = new GunNode(['users', 'alice']);

// Subscribe to changes
userNode.stream().on(userData => {
  console.log('User data updated:', userData);
});
```

## Transformation Operators

### Map

Transform each value emitted by the stream:

```typescript
// Transform user data to only include name and status
userNode.stream().map(user => ({
  displayName: user.name,
  isOnline: user.lastSeen > Date.now() - 300000 // 5 minutes
})).on(userStatus => {
  updateUserStatus(userStatus);
});
```

### SwitchMap

Map each value to a new stream, automatically unsubscribing from previous streams:

```typescript
// When current user changes, switch to their friends list
currentUserNode.stream().switchMap(user => {
  return new GunNode(['users', user.id, 'friends']).stream();
}).on(friends => {
  updateFriendsList(friends);
});
```

## Combination Operators

### Combine

Combine values from multiple streams:

```typescript
// Combine user profile with their preferences
const profile = userNode.stream();
const preferences = preferencesNode.stream();

profile.combine(preferences, (user, prefs) => {
  return {
    ...user,
    theme: prefs.theme,
    notifications: prefs.notifications
  };
}).on(enhancedProfile => {
  renderUserInterface(enhancedProfile);
});
```

### WithLatestFrom

Combine with another stream, but only emit when the source stream emits:

```typescript
// Update messages with user info when new messages arrive
messagesNode.stream().withLatestFrom(userNode.stream(), 
  (message, user) => ({
    ...message,
    isMine: message.senderId === user.id,
    senderAvatar: message.senderId === user.id ? user.avatar : null
  })
).on(enhancedMessage => {
  appendMessage(enhancedMessage);
});
```

### Merge

Merge multiple streams into one:

```typescript
// Combine system messages and user messages into one stream
const systemMessages = systemNode.stream();
const userMessages = messagesNode.stream();

systemMessages.merge(userMessages).on(message => {
  addMessageToChat(message);
});
```

## Timing Operators

### Debounce

Delay emissions, only emitting the most recent value after a specified time window:

```typescript
// Debounce typing updates to avoid excessive database writes
typingNode.stream().debounce(500).on(text => {
  saveTextToDatabase(text);
});
```

### StartWith

Start a stream with a specified initial value:

```typescript
// Start with loading state before data arrives
userNode.stream()
  .startWith({ loading: true })
  .on(state => {
    if (state.loading) {
      showLoadingIndicator();
    } else {
      hideLoadingIndicator();
      displayUserData(state);
    }
  });
```

## Creating Streams

### Static Values

Create a subscription with a static value:

```typescript
// Create a stream with a static value
const defaultSettings = GunSubscription.of({
  theme: 'light',
  fontSize: 'medium',
  notifications: true
});

// Use it in combination with actual user settings
userSettingsNode.stream()
  .withLatestFrom(defaultSettings, (userSettings, defaults) => {
    // Merge with defaults for any missing properties
    return { ...defaults, ...userSettings };
  })
  .on(settings => applySettings(settings));
```

## Advanced Patterns

### Deep Graph Traversal

Automatically resolve references in the Gun graph:

```typescript
// Traditional approach with multiple subscriptions
userNode.get('profile').on(profile => {
  userNode.get('settings').on(settings => {
    userNode.get('friends').on(friends => {
      // Manually combine multiple values
      renderUser({ profile, settings, friends });
    });
  });
});

// Reactive approach with composition
userNode.get('profile').stream().combine(
  userNode.get('settings').stream(),
  (profile, settings) => ({ profile, settings })
).switchMap(({ profile, settings }) => {
  return userNode.get('friends').stream()
    .map(friends => ({ profile, settings, friends }));
}).on(userData => {
  renderUser(userData);
});
```

### Error Handling

Handle errors gracefully in your streams:

```typescript
userNode.deepStream()
  .map(async user => {
    try {
      // Attempt some async operation
      const enrichedData = await fetchAdditionalData(user);
      return { ...user, ...enrichedData, error: null };
    } catch (err) {
      console.error('Error enriching user data:', err);
      return { ...user, error: err.message };
    }
  })
  .on(result => {
    if (result.error) {
      showErrorMessage(result.error);
    } else {
      displayUserProfile(result);
    }
  });
```

## Integration with Svelte Stores

Our `GunSubscription` implementation is fully compatible with Svelte's store contract:

```typescript
<script>
  import { GunNode } from './gun/GunNode';
  
  // Create a subscription that can be used as a Svelte store
  const userProfile = new GunNode(['users', 'current']).stream();
  
  // Use with Svelte's reactive declarations
  $: userName = $userProfile?.name || 'Guest';
  $: isAdmin = $userProfile?.role === 'admin';
</script>

<h1>Welcome, {userName}!</h1>
{#if isAdmin}
  <AdminPanel />
{/if}
```

## Best Practices

1. **Resource Management**: Always capture the return value of `.on()` to unsubscribe when no longer needed:
   ```typescript
   const cleanup = someStream.on(data => { /* ... */ });
   // Later:
   cleanup(); // Unsubscribe to prevent memory leaks
   ```

2. **Composition over Nesting**: Prefer chaining operators over nested subscriptions:
   ```typescript
   // Instead of:
   a.on(dataA => {
     b.on(dataB => {
       c.on(dataC => {
         // Deep nesting - hard to manage
       });
     });
   });
   
   // Do:
   a.combine(b, combineAB)
    .withLatestFrom(c, combineWithC)
    .on(result => {
      // Flat structure - easier to manage
    });
   ```

3. **Stream Reuse**: Create and store streams that will be used in multiple places:
   ```typescript
   // Create once
   const userSettingsStream = userNode.get('settings').stream();
   
   // Use in multiple places
   userSettingsStream.map(s => s.theme).on(updateTheme);
   userSettingsStream.map(s => s.fontSize).on(updateFontSize);
   ```

4. **Error Boundaries**: Add error handling to your streams to prevent failures from breaking the entire flow.

5. **Debugging**: For complex stream compositions, add logging operators to debug:
   ```typescript
   complexStream
     .map(data => {
       console.log('Stream data:', data);
       return data;
     })
     .on(result => handleResult(result));
   ```

## Real-World Examples

### Chat Application

Build a reactive chat system that combines messages with user data:

```typescript
// Get references
const messagesNode = new GunNode(['chats', chatId, 'messages']);
const usersNode = new GunNode(['users']);

// Create a stream of messages with user information
messagesNode.stream().map(async message => {
  // For each message, get the user who sent it
  const senderNode = new GunNode(['users', message.senderId]);
  const sender = await senderNode.once();
  
  return {
    ...message,
    sender: {
      name: sender.name,
      avatar: sender.avatar
    },
    timestamp: new Date(message.timestamp)
  };
}).on(enhancedMessage => {
  appendMessageToChat(enhancedMessage);
});
```

### Collaborative Document Editing

```typescript
// Document with real-time presence information
const docNode = new GunNode(['documents', docId]);
const presenceNode = new GunNode(['documents', docId, 'presence']);

// Combine document content with who's currently editing
docNode.stream().combine(
  presenceNode.stream(),
  (document, presenceList) => {
    const editorsCount = Object.keys(presenceList || {}).length;
    return {
      ...document,
      content: document.content || '',
      editors: presenceList || {},
      editorsCount
    };
  }
)
.on(enhancedDoc => {
  updateDocumentView(enhancedDoc);
});

// Update my presence periodically
const interval = setInterval(() => {
  presenceNode.get(currentUserId).put({
    lastSeen: Date.now(),
    name: currentUserName
  });
}, 5000);

// Clean up when leaving
onLeave(() => {
  clearInterval(interval);
  presenceNode.get(currentUserId).put(null);
});
```

## Conclusion

The reactive programming approach with our enhanced Gun subscriptions brings several benefits:

1. **Declarative Code**: Describe what should happen, not how it happens
2. **Reduced Complexity**: Flatten nested callback pyramids
3. **Better Resource Management**: Automatic cleanup of subscriptions
4. **Composability**: Build complex data flows from simple pieces
5. **Reactivity**: Automatic propagation of changes through your application

By leveraging these reactive patterns, you can build more maintainable, efficient Gun applications that respond elegantly to real-time data changes. 