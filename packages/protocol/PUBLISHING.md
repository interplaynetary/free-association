# Publishing @playnet/free-association

## Quick Start

```bash
cd packages/protocol

# Build the package
bun run build

# Test locally (dry run)
npm pack --dry-run

# Publish to npm
./scripts/release.sh [major|minor|patch]
```

## Manual Publishing Process

### 1. Prepare for Release

```bash
cd packages/protocol

# Install dependencies
bun install

# Build the package
bun run build

# Verify dist/ directory exists and has content
ls -lh dist/
```

### 2. Update Version

```bash
# Patch version (1.0.0 → 1.0.1)
npm version patch

# Minor version (1.0.0 → 1.1.0)
npm version minor

# Major version (1.0.0 → 2.0.0)
npm version major
```

### 3. Login to npm

If not already logged in:

```bash
npm login
# Enter your npm credentials
# For organization: must be member of @playnet org
```

### 4. Publish

```bash
# Dry run (see what would be published)
npm publish --dry-run

# Actual publish
npm publish
```

### 5. Tag and Push

```bash
# Get the new version
VERSION=$(node -p "require('./package.json').version")

# Commit the version bump
cd ../..  # back to repo root
git add packages/protocol/package.json
git commit -m "Release @playnet/free-association@$VERSION"

# Tag the release
git tag "protocol-v$VERSION"

# Push to GitHub
git push origin main --tags
```

## Automated Release Script

Use the provided release script for a guided process:

```bash
cd packages/protocol
./scripts/release.sh patch  # or minor, or major
```

The script will:
1. Check if you're logged in to npm
2. Bump the version
3. Build the package
4. Show what will be published
5. Confirm before publishing
6. Publish to npm
7. Show instructions for git commit/tag

## Version Strategy

We use Semantic Versioning (semver):

- **Patch** (1.0.0 → 1.0.1): Bug fixes, no API changes
- **Minor** (1.0.0 → 1.1.0): New features, backwards compatible
- **Major** (1.0.0 → 2.0.0): Breaking API changes

## What Gets Published

The package includes:

- `dist/` - Compiled JavaScript + TypeScript declarations
- `src/` - Source TypeScript files
- `README.md` - Package documentation
- `LICENSE` - License file

The following are excluded (see `.npmignore`):

- `tsconfig.json`
- Test files (`*.test.ts`, `*.spec.ts`)
- Development artifacts

## Verifying the Published Package

After publishing:

```bash
# View on npm
npm view @playnet/free-association

# Install in a test project
npm install @playnet/free-association@latest

# Test the import
node -e "const { computeAllocations } = require('@playnet/free-association'); console.log(typeof computeAllocations)"
```

## Troubleshooting

### "You must be logged in to publish"

```bash
npm login
# Make sure you're a member of @playnet organization
```

### "You cannot publish over the previously published versions"

The version already exists. Bump the version:

```bash
npm version patch
```

### Build fails

```bash
# Clean and rebuild
npm run clean
npm run build

# Check for TypeScript errors
bun run build 2>&1 | less
```

### Package size too large

```bash
# Check what's being included
npm pack --dry-run

# Review .npmignore to exclude unnecessary files
```

## Package Registry

- **Registry**: https://registry.npmjs.org/
- **Organization**: @playnet
- **Package**: https://www.npmjs.com/package/@playnet/free-association

## Support

For issues or questions:
- GitHub: https://github.com/playnet/free-association/issues
- npm: https://www.npmjs.com/package/@playnet/free-association

