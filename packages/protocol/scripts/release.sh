#!/bin/bash
set -e

# Release script for @playnet/free-association
# Usage: ./scripts/release.sh [major|minor|patch]

VERSION_TYPE=${1:-patch}

echo "🚀 Starting release process for @playnet/free-association..."
echo ""

# Check if we're in the right directory
if [ ! -f "package.json" ]; then
    echo "❌ Error: Must run from packages/protocol directory"
    exit 1
fi

# Check if we're logged in to npm
if ! npm whoami > /dev/null 2>&1; then
    echo "❌ Error: Not logged in to npm. Run: npm login"
    exit 1
fi

# Check if there are uncommitted changes
if ! git diff-index --quiet HEAD --; then
    echo "⚠️  Warning: You have uncommitted changes"
    read -p "Continue anyway? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        exit 1
    fi
fi

# Get current version
CURRENT_VERSION=$(node -p "require('./package.json').version")
echo "📦 Current version: $CURRENT_VERSION"

# Bump version
echo "📝 Bumping version ($VERSION_TYPE)..."
npm version $VERSION_TYPE --no-git-tag-version

# Get new version
NEW_VERSION=$(node -p "require('./package.json').version")
echo "✨ New version: $NEW_VERSION"
echo ""

# Build the package
echo "🔨 Building package..."
npm run clean
npm run build

# Check if build was successful
if [ ! -d "dist" ]; then
    echo "❌ Error: Build failed - dist directory not found"
    exit 1
fi

echo "✅ Build successful"
echo ""

# Show what will be published
echo "📋 Package contents:"
npm pack --dry-run
echo ""

# Confirm before publishing
read -p "🚢 Ready to publish v$NEW_VERSION to npm. Continue? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "❌ Aborted. Reverting version bump..."
    git checkout package.json
    exit 1
fi

# Publish to npm
echo "📤 Publishing to npm..."
npm publish

echo ""
echo "✅ Successfully published @playnet/free-association@$NEW_VERSION"
echo ""
echo "📝 Next steps:"
echo "  1. git add package.json"
echo "  2. git commit -m 'Release @playnet/free-association@$NEW_VERSION'"
echo "  3. git tag protocol-v$NEW_VERSION"
echo "  4. git push origin main --tags"

