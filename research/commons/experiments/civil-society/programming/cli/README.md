# Holster + Git CLI

A command-line interface for managing Git repositories with Holster distributed storage.

## Overview

This CLI protocol enables:

- **User Authentication**: Create and manage Holster identities
- **Git Operations**: Initialize repos, commit changes, view history
- **Distributed Storage**: Push/pull Git objects to/from Holster network
- **Cryptographic Identity**: Commits are signed with your Holster keypair

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Holster + Git CLI                       │
├─────────────────────────────────────────────────────────────┤
│  cli.ts          - Command dispatcher & user interaction    │
│  holster.js      - Holster instance & user auth             │
│  git-adapter.ts  - Git ↔ Holster storage bridge             │
└─────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        ▼                     ▼                     ▼
  ┌──────────┐         ┌─────────────┐      ┌──────────────┐
  │ Holster  │         │ isomorphic  │      │ LightningFS  │
  │ Network  │         │    git      │      │   (local)    │
  └──────────┘         └─────────────┘      └──────────────┘
```

### Storage Model

Git objects are stored in Holster with the following structure:

```
user.get('git/<repo>/objects/<sha>') = {
  type: 'blob' | 'tree' | 'commit',
  data: '<base64-encoded-object>',
  time: <timestamp>
}

user.get('git/<repo>/refs/heads/main') = {
  sha: '<commit-sha>',
  time: <timestamp>
}
```

## Installation

Dependencies are already installed via:

```bash
bun add -d isomorphic-git @isomorphic-git/lightning-fs
```

## Usage

### Quick Start

```bash
# 1. Create a Holster user
bun cli/cli.ts create alice secret123

# 2. Authenticate
bun cli/cli.ts auth alice secret123

# 3. Initialize a Git repository
bun cli/cli.ts git-init myproject

# 4. Create and commit files
bun cli/cli.ts git-write myproject README.md "# My Project"
bun cli/cli.ts git-add myproject README.md
bun cli/cli.ts git-commit myproject "Initial commit"

# 5. Push to Holster network
bun cli/cli.ts git-push myproject

# 6. Pull from another user
bun cli/cli.ts git-pull myproject <their-public-key>
```

### Using npm Scripts

```bash
# Show help
bun run cli

# Create user
bun run cli:create alice secret123

# Authenticate
bun run cli:auth alice secret123

# Git operations
bun run cli:git-init myrepo
bun run cli:git-status myrepo
bun run cli:git-push myrepo
```

## Commands

### Authentication Commands

#### `create <username> <password>`

Creates a new Holster user account.

```bash
bun cli/cli.ts create alice mySecurePassword123
```

**Output:**
```
Creating user: alice
✓ User created: alice
  Public key: SE2G7...XpZWS
```

#### `auth <username> <password>`

Authenticates an existing user and loads their keypair.

```bash
bun cli/cli.ts auth alice mySecurePassword123
```

**Output:**
```
Authenticating: alice
✓ Authenticated: alice
  Public key: SE2G7...XpZWS
```

#### `change-pass <old> <new>`

Changes the password for the currently authenticated user.

```bash
bun cli/cli.ts change-pass myOldPassword newSecurePassword456
```

#### `logout`

Signs out the current user.

```bash
bun cli/cli.ts logout
```

---

### Git Commands

#### `git-init <repo>`

Initializes a new Git repository.

```bash
bun cli/cli.ts git-init myproject
```

**Output:**
```
✓ Initialized repo: myproject
  Path: /holster/SE2G7...XpZWS/git/myproject
```

#### `git-write <repo> <file> <content>`

Writes a file to the repository.

```bash
bun cli/cli.ts git-write myproject README.md "# Hello World"
bun cli/cli.ts git-write myproject src/index.ts "console.log('Hello')"
```

#### `git-add <repo> <file>`

Stages a file for commit.

```bash
bun cli/cli.ts git-add myproject README.md
```

#### `git-commit <repo> <message>`

Commits staged changes.

```bash
bun cli/cli.ts git-commit myproject "Initial commit"
```

**Output:**
```
✓ Commit: 3a7b9d2c...
  Initial commit
```

**Commit Identity:**
- `author.name` = Your Holster username
- `author.email` = `<your-pubkey>@holster`
- All commits are cryptographically linked to your Holster identity

#### `git-status <repo>`

Shows the working tree status.

```bash
bun cli/cli.ts git-status myproject
```

**Output:**
```
On branch main
HEAD: 3a7b9d2c...

Changes to be committed:
  new file:   README.md

Changes not staged for commit:
  modified:   src/index.ts

Untracked files:
  package.json
```

#### `git-log <repo> [limit]`

Shows commit history.

```bash
bun cli/cli.ts git-log myproject 5
```

**Output:**
```
Commit history for myproject:

commit 3a7b9d2c4f5e6a7b8c9d0e1f2a3b4c5d6e7f8a9b
Author: alice <SE2G7...XpZWS@holster>
Date:   2025-11-15T10:30:00.000Z

    Initial commit
```

#### `git-push <repo>`

Pushes all Git objects to the Holster network.

```bash
bun cli/cli.ts git-push myproject
```

**Output:**
```
Pushing myproject to Holster...
  HEAD: 3a7b9d2c...
  Objects: 42
  Stored 10/42 objects...
  Stored 20/42 objects...
  ...
✓ Pushed 42 objects to Holster
  Ref: main -> 3a7b9d2c...
```

**What happens:**
1. Walks all objects reachable from HEAD
2. Serializes each object to base64
3. Stores each object in Holster: `git/<repo>/objects/<sha>`
4. Updates the main branch ref: `git/<repo>/refs/heads/main`

#### `git-pull <repo> [pubkey]`

Pulls Git objects from the Holster network.

```bash
# Pull from your own repos
bun cli/cli.ts git-pull myproject

# Pull from another user's repo
bun cli/cli.ts git-pull myproject SE2G7...XpZWS
```

**Output:**
```
Pulling myproject from Holster...
  Source: SE2G7...
  Remote HEAD: 3a7b9d2c...
  Fetched: 3a7b9d2c... (commit)
  Fetched: 7f8e9a0b... (tree)
  Fetched: 1c2d3e4f... (blob)
  ...
✓ Pulled 42 objects from Holster
  Updated: main -> 3a7b9d2c...
```

**What happens:**
1. Fetches the remote ref from Holster
2. Recursively fetches all objects (commits → trees → blobs)
3. Writes objects to local LightningFS
4. Updates local main branch ref

---

## Advanced Usage

### Collaborative Workflow

**Alice creates and pushes a repo:**

```bash
# Alice's machine
bun cli/cli.ts create alice password1
bun cli/cli.ts auth alice password1
bun cli/cli.ts git-init shared-project
bun cli/cli.ts git-write shared-project README.md "# Shared Project"
bun cli/cli.ts git-add shared-project README.md
bun cli/cli.ts git-commit shared-project "Initial commit"
bun cli/cli.ts git-push shared-project

# Alice shares her public key: SE2G7abc123...
```

**Bob pulls Alice's repo:**

```bash
# Bob's machine
bun cli/cli.ts create bob password2
bun cli/cli.ts auth bob password2
bun cli/cli.ts git-init shared-project
bun cli/cli.ts git-pull shared-project SE2G7abc123...

# Now Bob has Alice's repo!
```

### Scripting

Create a bash script for common workflows:

```bash
#!/bin/bash
# deploy.sh

REPO="my-website"
MESSAGE="${1:-Update}"

bun cli/cli.ts git-add $REPO .
bun cli/cli.ts git-commit $REPO "$MESSAGE"
bun cli/cli.ts git-push $REPO

echo "Deployed: $MESSAGE"
```

### Continuous Sync

```bash
# Watch and auto-commit/push
while true; do
  bun cli/cli.ts git-status myrepo | grep -q "modified" && {
    bun cli/cli.ts git-add myrepo .
    bun cli/cli.ts git-commit myrepo "Auto-sync: $(date)"
    bun cli/cli.ts git-push myrepo
  }
  sleep 60
done
```

---

## Integration with SvelteKit

The Git adapter is also available client-side in your SvelteKit app.

### Example Component

```svelte
<script lang="ts">
  import { initRepo, writeFile, addFile, commit, push, log } from '$lib/git'
  import { holsterUser } from '$lib/network/holster'
  
  let repo = 'my-app-data'
  let commits = $state([])
  
  async function init() {
    await initRepo(repo)
    console.log('Repo initialized!')
  }
  
  async function saveData() {
    const data = JSON.stringify({ user: holsterUser.is.username, timestamp: Date.now() })
    await writeFile(repo, 'data.json', data)
    await addFile(repo, 'data.json')
    await commit(repo, 'Save user data')
    await push(repo)
    console.log('Data pushed to Holster!')
  }
  
  async function loadHistory() {
    commits = await log(repo, 10)
  }
</script>

<button onclick={init}>Initialize Repo</button>
<button onclick={saveData}>Save Data</button>
<button onclick={loadHistory}>Load History</button>

{#each commits as commit}
  <div>
    <strong>{commit.commit.message}</strong>
    <br>
    <small>{commit.commit.author.name} - {new Date(commit.commit.author.timestamp * 1000).toLocaleString()}</small>
  </div>
{/each}
```

### Client-Only Configuration

In your route (e.g., `+page.ts`):

```typescript
export const ssr = false
export const prerender = false
```

This ensures Git operations run only in the browser.

---

## Storage Location

### CLI (Node/Bun)

- Holster data: `.holster-data/` (local directory)
- Git repos: LightningFS in-memory (ephemeral)

### Browser (SvelteKit)

- Holster data: IndexedDB (`holster` database)
- Git repos: LightningFS IndexedDB (`holster-git-browser` database)

---

## Protocol Specification

### Push Protocol

```
┌──────────┐                           ┌──────────────┐
│   CLI    │                           │   Holster    │
└─────┬────┘                           └──────┬───────┘
      │                                       │
      │ 1. git.resolveRef('HEAD')             │
      │────────────────────────────────▶      │
      │                                       │
      │ 2. git.listObjects(oids: [HEAD])      │
      │────────────────────────────────▶      │
      │                                       │
      │ 3. For each object:                   │
      │    git.readObject(oid)                │
      │────────────────────────────────▶      │
      │                                       │
      │ 4. user.put('git/<repo>/objects/<sha>', │
      │            { type, data, time })      │
      │───────────────────────────────────────▶
      │                                       │
      │ 5. user.put('git/<repo>/refs/heads/main', │
      │            { sha, time })             │
      │───────────────────────────────────────▶
      │                                       │
      │ 6. Ack                                │
      │◀───────────────────────────────────────
```

### Pull Protocol

```
┌──────────┐                           ┌──────────────┐
│   CLI    │                           │   Holster    │
└─────┬────┘                           └──────┬───────┘
      │                                       │
      │ 1. user.get(<pub>).get('git/<repo>/  │
      │         refs/heads/main').once()     │
      │───────────────────────────────────────▶
      │                                       │
      │ 2. { sha: <commit-sha> }              │
      │◀───────────────────────────────────────
      │                                       │
      │ 3. Recursive object fetch:            │
      │    user.get(<pub>).get('git/<repo>/   │
      │         objects/<sha>').once()        │
      │───────────────────────────────────────▶
      │                                       │
      │ 4. { type, data }                     │
      │◀───────────────────────────────────────
      │                                       │
      │ 5. git.writeObject({ type, object })  │
      │────────────────────────────────▶      │
      │                                       │
      │ 6. Parse object → find child oids     │
      │    Repeat step 3-5 for each child     │
      │                                       │
      │ 7. git.writeRef('refs/heads/main',    │
      │                 sha)                  │
      │────────────────────────────────▶      │
```

---

## Troubleshooting

### "No authenticated user"

**Solution:** Run `auth` before Git commands:

```bash
bun cli/cli.ts auth alice password123
```

### "Timeout fetching remote ref"

**Causes:**
- Network connectivity issues
- Invalid public key
- Repository doesn't exist on remote

**Solution:**
- Verify the public key
- Ensure the remote user has pushed the repo
- Check Holster peer connectivity

### "Object not found"

**Cause:** Incomplete push or corrupted objects

**Solution:**
- Re-push the repository: `bun cli/cli.ts git-push <repo>`
- Initialize a fresh repo and re-commit

---

## Security

### Identity Verification

All commits are signed with your Holster keypair:

```javascript
author.email = `${user.is.pub}@holster`
```

This cryptographically links every commit to your Holster identity.

### Object Integrity

Git objects are content-addressed (SHA-1/SHA-256), ensuring:
- **Immutability**: Cannot modify objects without changing their hash
- **Verification**: Recipients can verify object integrity
- **Deduplication**: Identical objects share the same storage

### Network Security

- Holster uses WebRTC for peer-to-peer connections
- Optional relay servers (wss://holster.haza.website)
- End-to-end encryption for user data

---

## Performance

### Push Performance

| Objects | Time (approx) |
|---------|---------------|
| 10      | 1-2s          |
| 100     | 10-15s        |
| 1000    | 2-3min        |

*Performance depends on network latency and peer connectivity*

### Pull Performance

Similar to push, but depends on:
- Source peer availability
- Object tree depth
- Network conditions

---

## Roadmap

- [ ] Binary-safe chunked storage for large files
- [ ] P2P direct sync (bypass relay)
- [ ] Merge conflict resolution
- [ ] Branch management
- [ ] Git LFS support
- [ ] Encryption layer for private repos
- [ ] Web UI for repo browsing

---

## Related Files

- `cli/holster.js` - Holster initialization
- `cli/git-adapter.ts` - Git ↔ Holster bridge
- `cli/cli.ts` - CLI dispatcher
- `src/lib/git/git-adapter.svelte.ts` - Browser adapter
- `src/lib/network/holster.ts` - Holster network config

---

## License

Same as parent project (see LICENSE.md)

