# Generic Share Target System

## 🎯 What Share Targets Enable

**Share targets** make your PWA appear in the **native system share menu** on mobile and desktop. When users share content from ANY app, they can select your app as the destination.

### Real-World Scenarios

| Scenario | Source | Your App Receives | Possible Action |
|----------|--------|-------------------|-----------------|
| **Share a Link** | Browser, Twitter | `{ url, title }` | Create recognition node |
| **Share Text** | Notes app, Email | `{ text }` | Add as note/description |
| **Share Location** | Google Maps | `{ url, title }` | Add to map view |
| **Share Video** | YouTube app | `{ url, title }` | Add to collection |
| **Share Post** | Social media | `{ url, text }` | Archive/reference |

---

## 🏗️ Generic Handler Architecture

The new system uses a **plugin-based approach** where you register handlers for different types of shared content.

### Key Concepts

```
Share Event → Handler Registry → Priority Sort → Try Each Handler → Success!
```

**Benefits:**
- ✅ **Extensible** - Add new handlers easily
- ✅ **Prioritized** - Higher priority handlers run first
- ✅ **Type-Safe** - Full TypeScript support
- ✅ **Flexible** - Each handler decides if it can handle the data
- ✅ **Fallback** - Default handler catches unhandled cases

---

## 📋 Built-in Handlers

### 1. Location Handler (Priority: 110)
**Detects:**
- Google Maps links
- OpenStreetMap links
- `geo:` URIs
- "location" in title

**Action:** Navigates to `/map` view

### 2. Video Handler (Priority: 110)
**Detects:**
- YouTube links
- Vimeo links
- Video-related URLs

**Action:** Adds to collection

### 3. Social Media Handler (Priority: 105)
**Detects:**
- Twitter/X links
- Facebook links
- LinkedIn links
- Instagram links

**Action:** Saves post reference

### 4. URL to Recognition (Priority: 100)
**Detects:**
- Any URL (general fallback)

**Action:** Creates recognition tree node

### 5. Text to Note (Priority: 90)
**Detects:**
- Text without URL

**Action:** Creates note

### 6. Default Handler (Priority: 0)
**Detects:**
- Everything else

**Action:** Generic handling

---

## 🔧 Creating Custom Handlers

### Basic Handler

```typescript
import { registerShareHandler, type ShareHandlerRegistration } from '$lib/utils/share-handler';

const myHandler: ShareHandlerRegistration = {
  name: 'my-custom-handler',
  description: 'Handles my specific content type',
  priority: 120, // Higher = runs first
  
  // Decide if this handler can process the data
  canHandle: (data) => {
    return data.url?.includes('mysite.com');
  },
  
  // Process the data
  handle: async ({ data, navigate, toast }) => {
    // Do something with the data
    sessionStorage.setItem('my-share', JSON.stringify(data));
    
    toast('Processing your custom content!');
    navigate('/my-custom-route');
    
    return true; // Return true if handled successfully
  }
};

// Register it
registerShareHandler(myHandler);
```

### Advanced Handler with Validation

```typescript
const articleHandler: ShareHandlerRegistration = {
  name: 'article-handler',
  description: 'Extracts and saves articles',
  priority: 115,
  
  canHandle: (data) => {
    if (!data.url) return false;
    
    // Check if it's an article URL
    const articlePatterns = [
      'medium.com',
      'substack.com',
      '/blog/',
      '/article/',
      '/post/'
    ];
    
    return articlePatterns.some(pattern => 
      data.url!.toLowerCase().includes(pattern)
    );
  },
  
  handle: async ({ data, navigate, toast }) => {
    try {
      // Extract metadata
      const article = {
        url: data.url,
        title: data.title || 'Untitled Article',
        excerpt: data.text || '',
        savedAt: Date.now(),
        tags: ['article', 'reading-list']
      };
      
      // Save to your system
      sessionStorage.setItem('pending-article', JSON.stringify(article));
      
      toast(`Saved: ${article.title}`);
      navigate('/reading-list');
      
      return true;
    } catch (error) {
      console.error('Article handler failed:', error);
      return false; // Let next handler try
    }
  }
};
```

### Conditional Handler with User Choice

```typescript
const imageHandler: ShareHandlerRegistration = {
  name: 'image-handler',
  description: 'Handles shared images',
  priority: 110,
  
  canHandle: (data) => {
    return data.url?.match(/\.(jpg|jpeg|png|gif|webp)$/i) !== null;
  },
  
  handle: async ({ data, navigate, toast }) => {
    // Store image reference
    const imageData = {
      url: data.url,
      title: data.title || 'Shared Image',
      timestamp: Date.now()
    };
    
    sessionStorage.setItem('pending-image', JSON.stringify(imageData));
    
    // Ask user where to add it
    toast('Where should this image go?', {
      duration: 5000,
      action: {
        label: 'Choose',
        onClick: () => navigate('/image-import')
      }
    });
    
    navigate('/');
    return true;
  }
};
```

---

## 📊 Handler Registration API

### Register Handler

```typescript
import { registerShareHandler } from '$lib/utils/share-handler';

registerShareHandler(myHandler);
```

### Unregister Handler

```typescript
import { unregisterShareHandler } from '$lib/utils/share-handler';

unregisterShareHandler('my-handler-name');
```

### Get All Handlers

```typescript
import { getShareHandlers } from '$lib/utils/share-handler';

const handlers = getShareHandlers();
console.log('Registered handlers:', handlers);
```

### Initialize Defaults

```typescript
import { initializeDefaultHandlers } from '$lib/utils/share-handler';

// Call once on app start
initializeDefaultHandlers();
```

---

## 🎨 Usage Patterns

### Pattern 1: Add Handler in Plugin System

```typescript
// src/lib/plugins/share-handlers.ts
import { 
  registerShareHandler,
  type ShareHandlerRegistration 
} from '$lib/utils/share-handler';

export function registerCustomHandlers() {
  registerShareHandler(spotifyHandler);
  registerShareHandler(githubHandler);
  registerShareHandler(pdfHandler);
}

// In your +layout.svelte
import { registerCustomHandlers } from '$lib/plugins/share-handlers';

onMount(() => {
  registerCustomHandlers();
});
```

### Pattern 2: Retrieve Pending Shares

```typescript
// In any component
import { getPendingShare } from '$lib/utils/share-handler';

onMount(() => {
  const locationShare = getPendingShare('pending-location-share');
  
  if (locationShare) {
    // Process the shared location
    console.log('Shared location:', locationShare);
    addLocationToMap(locationShare);
  }
});
```

### Pattern 3: Check for Any Pending Shares

```typescript
import { hasPendingShares, clearAllPendingShares } from '$lib/utils/share-handler';

onMount(() => {
  if (hasPendingShares()) {
    console.log('There are pending shares to process!');
    
    // Process them...
    
    // Then clear
    clearAllPendingShares();
  }
});
```

---

## 🔍 Advanced Use Cases

### Use Case 1: Multi-Step Processing

```typescript
const complexHandler: ShareHandlerRegistration = {
  name: 'multi-step-handler',
  priority: 100,
  
  canHandle: (data) => data.url?.includes('complex-site.com'),
  
  handle: async ({ data, navigate, toast }) => {
    // Step 1: Extract data
    toast('Extracting data...', { id: 'process' });
    const extracted = await extractData(data.url);
    
    // Step 2: Validate
    toast('Validating...', { id: 'process' });
    if (!validate(extracted)) {
      toast.error('Invalid data', { id: 'process' });
      return false;
    }
    
    // Step 3: Transform
    toast('Processing...', { id: 'process' });
    const transformed = transform(extracted);
    
    // Step 4: Store
    sessionStorage.setItem('complex-data', JSON.stringify(transformed));
    
    toast.success('Done!', { id: 'process' });
    navigate('/result');
    return true;
  }
};
```

### Use Case 2: Batch Import Handler

```typescript
const batchHandler: ShareHandlerRegistration = {
  name: 'batch-import',
  priority: 95,
  
  canHandle: (data) => {
    // Check if text contains multiple URLs
    return (data.text?.match(/https?:\/\//g) || []).length > 1;
  },
  
  handle: async ({ data, navigate, toast }) => {
    // Extract all URLs from text
    const urlRegex = /https?:\/\/[^\s]+/g;
    const urls = data.text!.match(urlRegex) || [];
    
    sessionStorage.setItem('batch-import-urls', JSON.stringify(urls));
    
    toast(`Found ${urls.length} URLs to import`);
    navigate('/batch-import');
    return true;
  }
};
```

### Use Case 3: Collaborative Handler

```typescript
const collaborativeHandler: ShareHandlerRegistration = {
  name: 'collab-handler',
  priority: 105,
  
  canHandle: (data) => data.url?.includes('collaborate'),
  
  handle: async ({ data, navigate, toast }) => {
    // Parse collaboration invite
    const invite = parseInvite(data.url);
    
    if (invite) {
      sessionStorage.setItem('collab-invite', JSON.stringify(invite));
      toast(`Joining ${invite.projectName}...`);
      navigate('/collaborative/join');
      return true;
    }
    
    return false;
  }
};
```

---

## 🧪 Testing Handlers

### Test in Browser Console

```typescript
// Get reference to share handler system
import * as ShareHandler from '$lib/utils/share-handler';

// Create test data
const testData = {
  title: 'Test Share',
  url: 'https://youtube.com/watch?v=test',
  text: 'Test content',
  timestamp: Date.now()
};

// Test which handler would catch it
const handlers = ShareHandler.getShareHandlers();
const applicable = handlers.filter(h => h.canHandle(testData));
console.log('Applicable handlers:', applicable);

// Test processing
await ShareHandler.processSharedData(testData, {
  navigate: (path) => console.log('Navigate to:', path),
  toast: (msg) => console.log('Toast:', msg)
});
```

---

## 📦 Manifest Configuration

The share target is configured in `vite.config.ts`:

```typescript
share_target: {
  action: '/share',           // Route to handle shares
  method: 'POST',
  enctype: 'multipart/form-data',
  params: {
    title: 'title',           // Form field names
    text: 'text',
    url: 'url'
  }
}
```

**Supports:**
- ✅ Text sharing
- ✅ URL sharing
- ✅ Title/description
- 🔜 File sharing (can be added)
- 🔜 Image sharing (can be added)

---

## 🎯 Best Practices

### 1. **Specific Before General**
Higher priority for specific handlers:
```typescript
// ✅ Good
videoHandler: priority: 110
urlHandler: priority: 100

// ❌ Bad (reversed priorities)
```

### 2. **Always Return Boolean**
```typescript
// ✅ Good
handle: async (ctx) => {
  try {
    // ... process
    return true;
  } catch (error) {
    console.error(error);
    return false; // Let next handler try
  }
}
```

### 3. **Clean Up After Processing**
```typescript
// ✅ Good
const share = getPendingShare('my-key');
if (share) {
  process(share);
  // Key is auto-removed by getPendingShare
}
```

### 4. **Use Storage Keys Consistently**
```typescript
// ✅ Good naming
'pending-video-share'
'pending-location-share'

// ❌ Inconsistent
'video_shared'
'location'
```

### 5. **Provide User Feedback**
```typescript
// ✅ Good
handle: async ({ toast }) => {
  toast('Processing your share...');
  // ... process
  toast.success('Added to your collection!');
}
```

---

## 🚀 Production Considerations

### Performance
- Handlers run in priority order
- First successful handler stops the chain
- Use `canHandle` for quick filtering
- Keep `handle` async for long operations

### Error Handling
- Handlers should catch their own errors
- Return `false` to try next handler
- Default handler catches everything

### Storage
- Use sessionStorage for temporary data
- Clear after processing with `getPendingShare()`
- Check `hasPendingShares()` on app mount

### Security
- Validate URLs before opening
- Sanitize text inputs
- Don't trust shared data blindly

---

## 📚 API Reference

### Types

```typescript
interface ShareData {
  title?: string;
  text?: string;
  url?: string;
  files?: File[];
  timestamp: number;
}

interface ShareHandlerContext {
  data: ShareData;
  navigate: (path: string) => void;
  toast: (message: string) => void;
}

type ShareHandler = (context: ShareHandlerContext) => Promise<boolean> | boolean;

interface ShareHandlerRegistration {
  name: string;
  description: string;
  priority: number;
  canHandle: (data: ShareData) => boolean;
  handle: ShareHandler;
}
```

### Functions

```typescript
// Registration
registerShareHandler(handler: ShareHandlerRegistration): void
unregisterShareHandler(name: string): void
getShareHandlers(): ReadonlyArray<ShareHandlerRegistration>
initializeDefaultHandlers(): void

// Processing
processSharedData(data: ShareData, context): Promise<{handled, handlerName}>

// Storage
getPendingShare(key: string): any | null
clearAllPendingShares(): void
hasPendingShares(): boolean
```

---

## 🎉 Summary

The generic share handler system gives you:

✅ **Plugin-based** - Add handlers easily  
✅ **Prioritized** - Control execution order  
✅ **Type-safe** - Full TypeScript support  
✅ **Flexible** - Each handler is independent  
✅ **Extensible** - Add new content types anytime  
✅ **Robust** - Fallback handling built-in  
✅ **Clean API** - Simple, intuitive interface  

Your PWA can now intelligently handle **any** shared content! 🚀

