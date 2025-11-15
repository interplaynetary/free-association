# Share Target - Now Generic & Extensible! 🎉

## What Changed

The share target system is now **fully generic and plugin-based** instead of hardcoded!

### Before ❌
```typescript
// Hardcoded in +page.svelte
sessionStorage.setItem('shared-content', JSON.stringify({
  title, text, url
}));
goto('/');
```

### After ✅
```typescript
// Plugin-based handler system
const handlers = [
  locationHandler,    // Maps links → /map
  videoHandler,       // YouTube → collection
  socialMediaHandler, // Twitter → archive
  urlHandler,         // Generic URLs → recognition
  textHandler,        // Plain text → notes
  defaultHandler      // Fallback
];

// Auto-routes based on content type!
```

---

## 🎯 What Share Targets Do

**Your app appears in the system share menu** alongside native apps!

### Examples:

**Share from Browser:**
```
User finds article → Share → Playnet
→ Automatically added to recognition tree
```

**Share from Maps:**
```
User finds location → Share → Playnet  
→ Opens /map with location pre-loaded
```

**Share from YouTube:**
```
User finds video → Share → Playnet
→ Added to video collection
```

**Share from Twitter:**
```
User sees tweet → Share → Playnet
→ Archived as reference
```

---

## 🏗️ Architecture

```
Share Event
    ↓
/share route receives data
    ↓
processSharedData()
    ↓
Handler Registry (prioritized)
    ↓
Try each handler's canHandle()
    ↓
First match: handler.handle()
    ↓
Navigate to appropriate view
```

---

## ⚡ Built-in Handlers

| Handler | Priority | Detects | Action |
|---------|----------|---------|--------|
| **Location** | 110 | Maps links, geo: URIs | Navigate to `/map` |
| **Video** | 110 | YouTube, Vimeo | Add to collection |
| **Social** | 105 | Twitter, Facebook | Archive post |
| **URL→Recognition** | 100 | Any URL | Create tree node |
| **Text→Note** | 90 | Plain text | Create note |
| **Default** | 0 | Everything | Generic handling |

---

## 🔧 How to Add Custom Handler

### Simple Example

```typescript
import { registerShareHandler } from '$lib/utils/share-handler';

registerShareHandler({
  name: 'my-handler',
  description: 'Handles my custom content',
  priority: 120, // Higher = runs first
  
  canHandle: (data) => {
    return data.url?.includes('mysite.com');
  },
  
  handle: async ({ data, navigate, toast }) => {
    // Process the data
    sessionStorage.setItem('my-data', JSON.stringify(data));
    
    toast('Got it!');
    navigate('/my-route');
    
    return true; // Success!
  }
});
```

### Real Example: Spotify Handler

```typescript
const spotifyHandler = {
  name: 'spotify-handler',
  priority: 115,
  
  canHandle: (data) => {
    return data.url?.includes('spotify.com');
  },
  
  handle: async ({ data, navigate, toast }) => {
    // Extract Spotify track/playlist ID
    const match = data.url.match(/\/track\/([a-zA-Z0-9]+)/);
    
    if (match) {
      sessionStorage.setItem('spotify-track', match[1]);
      toast('Adding to your music collection...');
      navigate('/music');
      return true;
    }
    
    return false;
  }
};

registerShareHandler(spotifyHandler);
```

---

## 📦 Retrieving Shared Data

```typescript
import { getPendingShare } from '$lib/utils/share-handler';

onMount(() => {
  // Check for pending location share
  const location = getPendingShare('pending-location-share');
  
  if (location) {
    // Add to map
    addLocationToMap(location.url, location.title);
  }
  
  // Check for pending video
  const video = getPendingShare('pending-video-share');
  
  if (video) {
    // Add to collection
    addVideoToCollection(video.url, video.title);
  }
});
```

---

## 🎨 Benefits

### ✅ Extensibility
Add new content types without modifying core code:
```typescript
registerShareHandler(githubHandler);
registerShareHandler(pdfHandler);
registerShareHandler(spotifyHandler);
// Add as many as you need!
```

### ✅ Prioritization
Control the order handlers are tried:
```typescript
locationHandler: 110  // Tries first for maps
videoHandler: 110     // Tries first for videos
urlHandler: 100       // Generic fallback
defaultHandler: 0     // Last resort
```

### ✅ Type Safety
Full TypeScript support:
```typescript
const handler: ShareHandlerRegistration = {
  // Fully typed!
};
```

### ✅ Clean Separation
Each handler is independent:
```typescript
// handlers/spotify.ts
export const spotifyHandler = { /* ... */ };

// handlers/github.ts
export const githubHandler = { /* ... */ };

// Register all
import { spotifyHandler } from './handlers/spotify';
import { githubHandler } from './handlers/github';

registerShareHandler(spotifyHandler);
registerShareHandler(githubHandler);
```

---

## 🧪 Testing

### Test Which Handler Matches

```typescript
import { getShareHandlers } from '$lib/utils/share-handler';

const testData = {
  url: 'https://youtube.com/watch?v=test',
  title: 'Test Video',
  timestamp: Date.now()
};

const handlers = getShareHandlers();
const applicable = handlers.filter(h => h.canHandle(testData));

console.log('These handlers can process it:', applicable);
// Output: [videoHandler]
```

### Test Processing

```typescript
import { processSharedData } from '$lib/utils/share-handler';

const result = await processSharedData(testData, {
  navigate: (path) => console.log('Navigate:', path),
  toast: (msg) => console.log('Toast:', msg)
});

console.log('Handled:', result.handled);
console.log('By:', result.handlerName);
// Output: Handled: true, By: video-handler
```

---

## 📚 Files

| File | Purpose |
|------|---------|
| `src/lib/utils/share-handler.ts` | Generic handler system |
| `src/routes/share/+page.svelte` | Entry point for shares |
| `docs/SHARE_TARGET_GUIDE.md` | Complete documentation |

---

## 🚀 What You Can Do Now

### 1. **Add Content Type Handlers**
```typescript
registerShareHandler(pdfHandler);      // PDFs → library
registerShareHandler(imageHandler);    // Images → gallery
registerShareHandler(audioHandler);    // Music → playlist
```

### 2. **Create App-Specific Handlers**
```typescript
registerShareHandler(projectHandler);  // Project invites
registerShareHandler(contactHandler);  // vCards
registerShareHandler(eventHandler);    // Calendar events
```

### 3. **Chain Processing**
```typescript
// Process shared data in multiple ways
registerShareHandler(saveHandler);     // Save to DB
registerShareHandler(notifyHandler);   // Notify team
registerShareHandler(analyzeHandler);  // Analyze content
```

---

## 💡 Use Cases

### Academic Research
```
Share papers → Playnet → Auto-categorize → Add to research tree
```

### Project Management
```
Share task links → Playnet → Extract info → Add to project board
```

### Content Curation
```
Share articles → Playnet → Tag & categorize → Reading list
```

### Collaboration
```
Share doc links → Playnet → Add to team workspace
```

---

## 📖 Documentation

**Full Guide:** `docs/SHARE_TARGET_GUIDE.md`
- Complete handler system documentation
- 20+ code examples
- Advanced patterns
- Testing strategies
- API reference

**Quick Start:** This file (SHARE_TARGET_SUMMARY.md)

---

## ✨ Result

Your share target is now:

✅ **Generic** - Works with any content type  
✅ **Extensible** - Add handlers easily  
✅ **Prioritized** - Smart routing  
✅ **Type-safe** - Full TypeScript  
✅ **Tested** - Built-in test utilities  
✅ **Documented** - Comprehensive guides  

You can handle **any** shared content intelligently! 🎉

