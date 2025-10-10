# Holster User Guide

This is a guide for testing Holster in Free Association as we
introduce support for it at the data storage layer. The currently
supported modules are notes and contacts.

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

## Switching to Holster Contacts

The contacts module can use either Gun or Holster.

### Enable Holster Contacts

In browser console:

```javascript
window.toggleHolster.contacts()
```

Then reload the page.

### Check Which Backend Is Active

```javascript
window.toggleHolster.status()
```

Shows which features are using Holster vs Gun.

### Switch Back to Gun Contacts

```javascript
window.toggleHolster.contacts()
```

Then reload the page.

### Important Notes

- **Data is separate**: Gun contacts and Holster contacts are in different databases
- **No auto-sync**: Switching backends doesn't migrate data

---

## Console Reference

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

### Toggle Utilities

```javascript
// Toggle backends
window.toggleHolster.contacts()     // Toggle contacts (requires reload)
window.toggleHolster.status()       // Check current settings
```
