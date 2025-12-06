# NPM Publishing Checklist

## ✅ Pre-Publish Checklist

### 1. Verify Package Information

```bash
cd src/lib/protocol/lambda-calculus
cat package.json | grep -E "(name|version|description)"
```

**Check**:
- [ ] Package name is `@free-association/lambda-calculus`
- [ ] Version is correct (use semantic versioning)
- [ ] Description is accurate

### 2. Update Version

```bash
npm version patch  # 1.0.0 → 1.0.1
npm version minor  # 1.0.0 → 1.1.0
npm version major  # 1.0.0 → 2.0.0
```

### 3. Run All Tests

```bash
npm test
```

**Expected**: 103/103 tests passing ✅

### 4. Type Check

```bash
npm run type-check
```

**Expected**: No errors ✅

### 5. Build Package

```bash
npm run build
```

**Check**:
- [ ] `dist/` directory created
- [ ] `dist/index.js` exists
- [ ] `dist/index.mjs` exists
- [ ] `dist/index.d.ts` exists
- [ ] `dist/elegant/` directory exists

### 6. Verify Package Contents

```bash
npm pack --dry-run
```

**Check what will be published**:
- [ ] dist/ folder included
- [ ] README.md included
- [ ] LICENSE included
- [ ] Documentation files included
- [ ] src/ folder NOT included
- [ ] tests/ folder NOT included

### 7. Test Local Installation

```bash
# In the package directory
npm pack

# In a test project
npm install /path/to/@free-association-lambda-calculus-1.0.0.tgz

# Test imports
node -e "const {mutual} = require('@free-association/lambda-calculus'); console.log(typeof mutual)"
node -e "import('@free-association/lambda-calculus').then(m => console.log(typeof m.mutual))"
```

### 8. Verify Documentation

**Check that these files exist and are up-to-date**:
- [ ] README.md (or copy README-NPM.md to README.md)
- [ ] LICENSE
- [ ] LAMBDA-R-COMPLIANT.md
- [ ] docs/ELEGANT-API.md
- [ ] docs/CORE-VS-ELEGANT.md
- [ ] docs/QUICK-COMPARISON.md

### 9. Update CHANGELOG

Create `CHANGELOG.md` if it doesn't exist:

```markdown
# Changelog

## [1.0.0] - 2025-12-05

### Added
- Complete λ-R specification implementation
- Dual APIs (core and elegant)
- Full type safety with TypeScript
- 103 comprehensive tests
- Complete documentation

### Features
- Recognition system (mutual, TMR, MRS, MRD)
- Filter system (λ-R compliant)
- Limit system (λ-R compliant)
- Collective formation (SCMRS, SCRMRS)
- Commons & resource allocation
- System evolution
- Lambda calculus combinators (S, K, I, B, C, Y)
- Monads (Maybe, Reader, State)
- Church encodings
```

---

## 🚀 Publishing Steps

### Option 1: Publish to NPM

#### First Time Setup

```bash
# Login to NPM
npm login

# Verify login
npm whoami
```

#### Publish

```bash
# Dry run (see what will happen)
npm publish --dry-run

# Publish (for scoped package, need --access public)
npm publish --access public

# Or if you have 2FA enabled
npm publish --otp=123456 --access public
```

#### Verify Publication

```bash
# Check on NPM
npm view @free-association/lambda-calculus

# Install from NPM to test
npm install @free-association/lambda-calculus
```

---

### Option 2: Publish to GitHub Packages

#### Setup `.npmrc` in package directory

```bash
echo "@free-association:registry=https://npm.pkg.github.com" > .npmrc
```

#### Authenticate

```bash
npm login --scope=@free-association --registry=https://npm.pkg.github.com
# Username: your-github-username
# Password: your-github-personal-access-token
# Email: your-email
```

#### Publish

```bash
npm publish
```

---

### Option 3: Local/Private Registry

```bash
# Set registry
npm config set registry http://your-registry:4873

# Publish
npm publish
```

---

## 📝 Post-Publish Checklist

### 1. Verify Installation

```bash
# Create test project
mkdir test-install
cd test-install
npm init -y
npm install @free-association/lambda-calculus zod

# Test require
node -e "const {mutual} = require('@free-association/lambda-calculus'); console.log('✅ CommonJS works')"

# Test import
node --input-type=module -e "import {mutual} from '@free-association/lambda-calculus'; console.log('✅ ESM works')"

# Test TypeScript
npm install -D typescript @types/node
npx tsc --init
echo "import {mutual} from '@free-association/lambda-calculus'" > test.ts
npx tsc test.ts && echo "✅ TypeScript works"
```

### 2. Test Elegant API

```bash
node --input-type=module -e "import {elegant} from '@free-association/lambda-calculus'; console.log(typeof elegant.pipe)"
```

### 3. Update Repository

```bash
git tag v1.0.0
git push origin v1.0.0
git push origin main
```

### 4. Create GitHub Release

1. Go to repository releases
2. Create new release
3. Tag: `v1.0.0`
4. Title: `v1.0.0 - Complete λ-R Implementation`
5. Description: Copy from CHANGELOG.md
6. Attach tarball: `@free-association-lambda-calculus-1.0.0.tgz`

### 5. Update Documentation

Update main repository README to reference the published package:

```markdown
## Installation

\`\`\`bash
npm install @free-association/lambda-calculus
\`\`\`
```

### 6. Announce

- [ ] Post to project blog/newsletter
- [ ] Share on social media
- [ ] Update documentation site
- [ ] Notify users

---

## 🔄 Publishing Updates

### Patch Release (Bug Fixes)

```bash
npm version patch
npm test
npm publish --access public
git push --tags
```

### Minor Release (New Features)

```bash
npm version minor
npm test
npm publish --access public
git push --tags
```

### Major Release (Breaking Changes)

```bash
npm version major
npm test
npm publish --access public
git push --tags
```

---

## 🛠️ Troubleshooting

### "You do not have permission to publish"

Solution: Add `--access public` for scoped packages

```bash
npm publish --access public
```

### "Package name taken"

Solution: Use a scope

```bash
# In package.json
"name": "@yourscope/lambda-calculus"
```

### "Missing required files"

Check `.npmignore` and `files` in package.json

```bash
npm pack --dry-run
```

### "Build failed"

```bash
npm run clean
npm run build
npm test
```

### "Types not found"

Verify dist/ has .d.ts files:

```bash
ls -R dist/ | grep .d.ts
```

---

## 📊 Success Metrics

After publishing, monitor:

- [ ] NPM download stats
- [ ] GitHub stars/forks
- [ ] Issue reports
- [ ] User feedback
- [ ] Documentation views

---

## 🎯 Quick Publish Commands

```bash
# Complete publish flow
npm run clean
npm test
npm run type-check
npm run build
npm pack --dry-run  # Review
npm version patch   # Or minor/major
npm publish --access public
git push --tags
```

---

## 📚 Additional Resources

- [NPM Publishing Guide](https://docs.npmjs.com/cli/v8/commands/npm-publish)
- [Semantic Versioning](https://semver.org/)
- [NPM Package.json Guide](https://docs.npmjs.com/cli/v8/configuring-npm/package-json)
- [GitHub Packages](https://docs.github.com/en/packages)

---

**Ready to Publish!** 🚀

Your package is **production-ready** with:
- ✅ 103 tests passing
- ✅ Zero type errors  
- ✅ Complete documentation
- ✅ λ-R specification compliant
- ✅ Dual APIs (core + elegant)
- ✅ Full TypeScript support

**Go ahead and publish!**

