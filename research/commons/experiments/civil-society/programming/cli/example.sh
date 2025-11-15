#!/bin/bash
# Example workflow demonstrating Holster + Git CLI

set -e

echo "════════════════════════════════════════════════════════════════"
echo "  Holster + Git CLI Demo"
echo "════════════════════════════════════════════════════════════════"
echo ""

# Configuration
USERNAME="demo-user-$(date +%s)"
PASSWORD="secure-password-123"
REPO="demo-project"

echo "📝 Step 1: Creating Holster user..."
bun cli/cli.ts create "$USERNAME" "$PASSWORD"
echo ""

echo "🔐 Step 2: Authenticating..."
bun cli/cli.ts auth "$USERNAME" "$PASSWORD"
echo ""

echo "📦 Step 3: Initializing Git repository..."
bun cli/cli.ts git-init "$REPO"
echo ""

echo "✍️  Step 4: Creating files..."
bun cli/cli.ts git-write "$REPO" README.md "# Demo Project

This is a demonstration of Holster + Git integration.

## Features

- Distributed storage via Holster
- Cryptographic identity
- P2P synchronization"

bun cli/cli.ts git-write "$REPO" package.json '{
  "name": "demo-project",
  "version": "1.0.0",
  "type": "module"
}'

bun cli/cli.ts git-write "$REPO" src/index.ts 'console.log("Hello from Holster + Git!")'
echo ""

echo "➕ Step 5: Staging files..."
bun cli/cli.ts git-add "$REPO" README.md
bun cli/cli.ts git-add "$REPO" package.json
bun cli/cli.ts git-add "$REPO" src/index.ts
echo ""

echo "💾 Step 6: Committing changes..."
bun cli/cli.ts git-commit "$REPO" "Initial commit: Add project files"
echo ""

echo "📊 Step 7: Checking status..."
bun cli/cli.ts git-status "$REPO"
echo ""

echo "📜 Step 8: Viewing commit history..."
bun cli/cli.ts git-log "$REPO"
echo ""

echo "☁️  Step 9: Pushing to Holster network..."
bun cli/cli.ts git-push "$REPO"
echo ""

echo "════════════════════════════════════════════════════════════════"
echo "✅ Demo complete!"
echo ""
echo "Your repository '$REPO' is now stored on the Holster network."
echo "Other users can pull it using your public key."
echo ""
echo "To pull this repo from another machine:"
echo "  1. Get your public key from the output above"
echo "  2. On another machine, run:"
echo "     bun cli/cli.ts create <other-user> <password>"
echo "     bun cli/cli.ts auth <other-user> <password>"
echo "     bun cli/cli.ts git-init $REPO"
echo "     bun cli/cli.ts git-pull $REPO <your-public-key>"
echo "════════════════════════════════════════════════════════════════"

