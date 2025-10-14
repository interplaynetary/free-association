# Holster User Guide

This guide helps you switch from Gun to Holster for data storage in Free Association. Holster is a P2P database alternative that offers improved performance and reliability.

---

## Quick Start: Switching from Gun to Holster

If you're already using Free Association with Gun and want to switch to Holster, follow these steps:

### 1. Enable Holster for All Features

Open the browser console and run:

```javascript
window.toggleHolster.enableAll()
```

This enables Holster for authentication, tree, capacities, contacts, recognition, compose, chat, chat read states, and allocation states.

### 2. Reload the Page

After running the command above, reload the page to apply the changes.

### 3. Create a New Holster Account

**Important:** Gun and Holster use separate databases. You'll need to create a new account for Holster.

On the login page, click "Sign up" and create a new account with:
- Username (can be the same as your Gun username if you prefer)
- Password

### 4. Start Fresh with Holster

Once logged in with your new Holster account:
- Your tree will be empty initially
- You can click "Play!" to create a new tree with example data
- Or build your tree from scratch
- Add contacts, capacities, and start recognizing contributions

### Important Notes

- **Separate Databases:** Gun and Holster store data separately. Switching does not migrate your existing data.
- **Fresh Start:** You'll need to rebuild your tree, contacts, and capacities in Holster.
- **No Data Loss:** Your Gun data remains intact. You can switch back anytime using `window.toggleHolster.disableAll()`.

---

## Switching Back to Gun

If you need to switch back to Gun:

```javascript
window.toggleHolster.disableAll()
```

Then reload the page and log in with your Gun credentials.

---

## Gradual Migration (Advanced)

You can also enable Holster for specific features while keeping others on Gun:

### Enable Individual Features

```javascript
// Enable Holster authentication
window.toggleHolster.auth()

// Enable Holster tree storage
window.toggleHolster.tree()

// Enable Holster capacities
window.toggleHolster.capacities()

// Enable Holster contacts
window.toggleHolster.contacts()

// Enable Holster recognition (SOGF)
window.toggleHolster.recognition()

// Enable Holster compose (slot composition)
window.toggleHolster.compose()

// Enable Holster chat
window.toggleHolster.chat()

// Enable Holster chat read states
window.toggleHolster.chatReadStates()

// Enable Holster allocation states
window.toggleHolster.allocationStates()
```

After toggling any feature, reload the page.

### Check Current Status

See which features are using Holster vs Gun:

```javascript
window.toggleHolster.status()
```

This shows a breakdown of all features and whether they're using Holster (true) or Gun (false).

---

## Testing Notes (Holster-only)

The Notes app is a test feature that only runs on Holster.

### 1. Open the Notes Page

Start dev server and go to: `http://localhost:5173/notes-test`

### 2. Create Test Account

Open the browser console and run:

```javascript
await window.testHolster.auth.createAndLogin()
```

This creates a unique test account and logs you in automatically.

You will need to reload the page initially, but the account credentials
are then stored in local storage.

### 3. Use Notes

Once logged in, the page will show the notes interface:
- Add a note (title + content)
- Edit/delete notes
- Notes persist after page reload

### 4. Test Real-Time Sync

- Open `/notes-test` in two browser tabs
- Create a note in one tab
- It should appear immediately in the other tab

### Useful Console Commands

```javascript
// Check login status
window.testHolster.checkStatus()

// Logout
window.testHolster.auth.logout()

// Login again (creates new account each time)
await window.testHolster.auth.createAndLogin()

// Test basic Holster operations
window.testHolster.testBasic()
```

---

## Console Reference

### Toggle Utilities

```javascript
// Enable/disable all features at once
window.toggleHolster.enableAll()     // Switch everything to Holster
window.toggleHolster.disableAll()    // Switch everything back to Gun

// Toggle individual features (each requires reload)
window.toggleHolster.auth()
window.toggleHolster.tree()
window.toggleHolster.capacities()
window.toggleHolster.contacts()
window.toggleHolster.recognition()
window.toggleHolster.compose()
window.toggleHolster.chat()
window.toggleHolster.chatReadStates()
window.toggleHolster.allocationStates()

// Check status
window.toggleHolster.status()        // Shows which features use Holster
```

### Holster Test Utilities

```javascript
// Authentication
await window.testHolster.auth.createAndLogin()  // Create and login
window.testHolster.auth.logout()                // Logout
await window.testHolster.auth.fullTest()        // Run full test

// Status checks
window.testHolster.checkStatus()                // Show auth status
window.testHolster.testBasic()                  // Test put/get

// Custom user data (when logged in)
await window.testHolster.userData.write('key', 'value')
window.testHolster.userData.read('key')
```

---

## Troubleshooting

### Can't Log In After Switching

Make sure you created a new Holster account. Your Gun credentials won't work with Holster.

### Page Won't Load After Toggle

Clear your browser cache or use incognito mode, then try toggling again.

### Tree Shows as Empty

After switching to Holster, you need to create a new tree:
1. Click the "Play!" button to create a tree with example data, or
2. Start building your tree from scratch

### Data Disappeared

If you switched to Holster, your Gun data is still there. Switch back with:
```javascript
window.toggleHolster.disableAll()
```

Then reload and log in with your Gun credentials.

---

## Migration Status

**Phase 11 Complete:** All core features now support Holster!

Implemented modules:
- ✅ Authentication (login/signup/signout)
- ✅ Tree (recognition tree storage)
- ✅ Capacities (capacity management)
- ✅ Contacts (contact management)
- ✅ Recognition (SOGF/share calculations)
- ✅ Compose (slot composition)
- ✅ Chat (capacity chat messages)
- ✅ Chat Read States (message read tracking)
- ✅ Allocation States (capacity allocation)

**Next Steps:** Phase 12 will remove Gun entirely, making Holster the only backend.
