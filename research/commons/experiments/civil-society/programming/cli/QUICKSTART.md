# Quickstart Guide

Get started with Holster + Git CLI in 5 minutes.

## Prerequisites

- Bun installed (`curl -fsSL https://bun.sh/install | bash`)
- Dependencies installed (already done via `bun add -d isomorphic-git @isomorphic-git/lightning-fs`)

## 5-Minute Tutorial

### 1. Create Your Identity

```bash
bun cli/cli.ts create alice myPassword123
```

**Output:**
```
Creating user: alice
✓ User created: alice
  Public key: SE2G7abc123...
```

💡 **Save your public key** - others will need it to pull your repos!

### 2. Login

```bash
bun cli/cli.ts auth alice myPassword123
```

### 3. Create Your First Repo

```bash
bun cli/cli.ts git-init hello-world
```

### 4. Add Some Content

```bash
# Write a README
bun cli/cli.ts git-write hello-world README.md "# Hello World
This is my first Holster + Git repo!"

# Stage it
bun cli/cli.ts git-add hello-world README.md

# Commit it
bun cli/cli.ts git-commit hello-world "My first commit"
```

### 5. Push to Holster Network

```bash
bun cli/cli.ts git-push hello-world
```

🎉 **Done!** Your repo is now distributed across the Holster network.

---

## What's Next?

### View Your Commit History

```bash
bun cli/cli.ts git-log hello-world
```

### Check Status

```bash
bun cli/cli.ts git-status hello-world
```

### Pull Someone Else's Repo

```bash
# Initialize a local repo with the same name
bun cli/cli.ts git-init their-project

# Pull from their public key
bun cli/cli.ts git-pull their-project SE2G7xyz789...
```

### Run the Demo Script

```bash
./cli/example.sh
```

This creates a complete demo project and pushes it to Holster.

---

## Using npm Scripts

For convenience, you can use shortcuts:

```bash
# Show help
bun run cli

# Create user (you'll be prompted for username/password)
bun run cli:create alice myPassword123

# Authenticate
bun run cli:auth alice myPassword123

# Git operations
bun run cli:git-init myrepo
bun run cli:git-status myrepo
bun run cli:git-push myrepo
```

---

## Collaborative Example

**Alice** (you):

```bash
# Create and share a project
bun cli/cli.ts create alice pass1
bun cli/cli.ts auth alice pass1
bun cli/cli.ts git-init shared-project
bun cli/cli.ts git-write shared-project README.md "# Shared Project"
bun cli/cli.ts git-add shared-project README.md
bun cli/cli.ts git-commit shared-project "Alice: Initial commit"
bun cli/cli.ts git-push shared-project

# Share your public key with Bob
# (It's shown in the 'create' or 'auth' output)
```

**Bob** (another user):

```bash
# Pull Alice's project
bun cli/cli.ts create bob pass2
bun cli/cli.ts auth bob pass2
bun cli/cli.ts git-init shared-project
bun cli/cli.ts git-pull shared-project <Alice's public key>

# Now Bob has Alice's repo!
bun cli/cli.ts git-log shared-project
```

---

## Browser Integration

You can also use Git operations in your SvelteKit app:

```svelte
<script lang="ts">
  import * as git from '$lib/git'
  
  async function save() {
    await git.initRepo('app-data')
    await git.writeFile('app-data', 'data.json', '{"foo": "bar"}')
    await git.addFile('app-data', 'data.json')
    await git.commit('app-data', 'Save data')
    await git.push('app-data')
  }
</script>

<button onclick={save}>Save to Holster</button>
```

---

## Common Issues

**"No authenticated user"**

Solution: Always run `auth` before Git commands:

```bash
bun cli/cli.ts auth alice myPassword123
```

**"Timeout"**

- Check internet connection
- Verify Holster peers are reachable
- Wait a few seconds and retry

---

## Next Steps

1. Read the [full documentation](./README.md)
2. Explore the [example script](./example.sh)
3. Check out the [SvelteKit integration](../src/lib/git/)
4. Build something cool! 🚀

---

## Help

```bash
bun cli/cli.ts help
```

For issues, see [troubleshooting](./README.md#troubleshooting) in the main README.

