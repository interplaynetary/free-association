# Packaging Guide: Lambda Calculus Implementation

This guide explains how to package the Lambda Calculus implementation for distribution across various package managers.

## 📦 NPM Package

### 1. Create Package Configuration

Create a separate `package.json` for the lambda-calculus package:

```json
{
  "name": "@free-association/lambda-calculus",
  "version": "1.0.0",
  "description": "Lambda Calculus implementation of the Free-Association Framework (Recognition Calculus λ-R)",
  "main": "dist/index.js",
  "module": "dist/index.mjs",
  "types": "dist/index.d.ts",
  "exports": {
    ".": {
      "import": "./dist/index.mjs",
      "require": "./dist/index.js",
      "types": "./dist/index.d.ts"
    },
    "./elegant": {
      "import": "./dist/elegant/index.mjs",
      "require": "./dist/elegant/index.js",
      "types": "./dist/elegant/index.d.ts"
    }
  },
  "files": [
    "dist",
    "README.md",
    "LICENSE.md"
  ],
  "scripts": {
    "build": "tsup src/index.ts --format cjs,esm --dts",
    "test": "vitest run",
    "prepublishOnly": "npm run build && npm test"
  },
  "keywords": [
    "lambda-calculus",
    "recognition-calculus",
    "functional-programming",
    "coordination",
    "free-association",
    "monads",
    "combinators",
    "church-encoding"
  ],
  "author": "Free Association Contributors",
  "license": "SEE LICENSE IN LICENSE.md",
  "repository": {
    "type": "git",
    "url": "https://github.com/yourusername/free-association.git",
    "directory": "src/lib/protocol/lambda-calculus"
  },
  "bugs": {
    "url": "https://github.com/yourusername/free-association/issues"
  },
  "homepage": "https://github.com/yourusername/free-association#readme",
  "peerDependencies": {
    "zod": "^3.22.0"
  },
  "devDependencies": {
    "@types/node": "^20.0.0",
    "tsup": "^8.0.0",
    "typescript": "^5.3.0",
    "vitest": "^1.0.0"
  }
}
```

### 2. Build Configuration (tsup.config.ts)

```typescript
import { defineConfig } from 'tsup';

export default defineConfig({
  entry: {
    index: 'src/index.ts',
    'elegant/index': 'src/elegant/index.ts',
  },
  format: ['cjs', 'esm'],
  dts: true,
  splitting: false,
  sourcemap: true,
  clean: true,
  treeshake: true,
  minify: false, // Set to true for production
});
```

### 3. TypeScript Configuration (tsconfig.json)

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ESNext",
    "lib": ["ES2020"],
    "declaration": true,
    "declarationMap": true,
    "sourceMap": true,
    "outDir": "./dist",
    "rootDir": "./src",
    "strict": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "moduleResolution": "bundler",
    "resolveJsonModule": true
  },
  "include": ["src/**/*"],
  "exclude": ["node_modules", "dist", "**/*.test.ts"]
}
```

### 4. Publishing to NPM

```bash
# 1. Build the package
npm run build

# 2. Test the package locally
npm pack
npm install -g ./free-association-lambda-calculus-1.0.0.tgz

# 3. Test in another project
cd /path/to/test-project
npm install /path/to/package

# 4. Publish to NPM
npm login
npm publish --access public

# For scoped packages
npm publish --access public --scope=@free-association
```

### 5. Usage After Publishing

```typescript
// Install
npm install @free-association/lambda-calculus

// Use
import { elegant, mutual, mrs } from '@free-association/lambda-calculus';
import { S, K, I, Y } from '@free-association/lambda-calculus/elegant';
```

## 📦 GNU Guix Package

### 1. Create Guix Package Definition

Create `guix.scm` in the package root:

```scheme
(define-module (free-association lambda-calculus)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages node))

(define-public node-lambda-calculus
  (package
    (name "node-lambda-calculus")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://registry.npmjs.org/@free-association/lambda-calculus/-/"
                    "lambda-calculus-" version ".tgz"))
              (sha256
               (base32
                "INSERT_HASH_HERE"))))
    (build-system node-build-system)
    (arguments
     '(#:tests? #t
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "npm" "run" "build")))
         (replace 'check
           (lambda _
             (invoke "npm" "test"))))))
    (inputs
     `(("node" ,node)
       ("node-zod" ,node-zod)))
    (synopsis "Lambda Calculus implementation of Free-Association Framework")
    (description
     "This package provides a complete Lambda Calculus implementation (λ-R)
of the Free-Association Framework.  It includes combinators (S, K, I, B, C, Y),
monadic patterns (Maybe, Reader, State), Church encodings, and a full
recognition system with mutual recognition, MRS, MRD calculations.")
    (home-page "https://github.com/yourusername/free-association")
    (license license:custom)))  ; Update with actual license
```

### 2. Calculate Hash

```bash
guix download https://registry.npmjs.org/@free-association/lambda-calculus/-/lambda-calculus-1.0.0.tgz
# Copy the hash and update guix.scm
```

### 3. Build with Guix

```bash
# Build the package
guix build -f guix.scm

# Install locally
guix package -f guix.scm

# Test
guix build -f guix.scm --check
```

### 4. Submit to Guix

```bash
# 1. Clone Guix
git clone https://git.savannah.gnu.org/git/guix.git
cd guix

# 2. Create branch
git checkout -b add-lambda-calculus

# 3. Add package definition to gnu/packages/node-xyz.scm

# 4. Test
./pre-inst-env guix build node-lambda-calculus

# 5. Create patch
git add gnu/packages/node-xyz.scm
git commit -m "gnu: Add node-lambda-calculus."
git format-patch -1

# 6. Submit to guix-patches@gnu.org
```

## 📦 Other Package Managers

### Nix/NixOS

Create `default.nix`:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage rec {
  pname = "lambda-calculus";
  version = "1.0.0";

  src = ./.;

  npmDepsHash = "sha256-INSERT_HASH_HERE";

  buildPhase = ''
    npm run build
  '';

  checkPhase = ''
    npm test
  '';

  installPhase = ''
    mkdir -p $out
    cp -r dist $out/
    cp package.json $out/
    cp README.md $out/
  '';

  meta = with pkgs.lib; {
    description = "Lambda Calculus implementation of Free-Association Framework";
    homepage = "https://github.com/yourusername/free-association";
    license = licenses.custom;
    maintainers = [ ];
  };
}
```

### Debian/Ubuntu (deb package)

```bash
# 1. Install packaging tools
sudo apt install debhelper dh-make

# 2. Create debian directory structure
mkdir -p debian
cd debian

# 3. Create control file
cat > control << EOF
Source: lambda-calculus
Section: javascript
Priority: optional
Maintainer: Your Name <your.email@example.com>
Build-Depends: debhelper (>= 10), nodejs, npm
Standards-Version: 4.5.0

Package: node-lambda-calculus
Architecture: all
Depends: nodejs, node-zod
Description: Lambda Calculus implementation
 Complete Lambda Calculus implementation (λ-R) of the
 Free-Association Framework with combinators and monads.
EOF

# 4. Build package
dpkg-buildpackage -us -uc
```

### Arch Linux (AUR)

Create `PKGBUILD`:

```bash
# Maintainer: Your Name <your.email@example.com>
pkgname=nodejs-lambda-calculus
pkgver=1.0.0
pkgrel=1
pkgdesc="Lambda Calculus implementation of Free-Association Framework"
arch=('any')
url="https://github.com/yourusername/free-association"
license=('custom')
depends=('nodejs' 'npm')
makedepends=('npm')
source=("https://registry.npmjs.org/@free-association/lambda-calculus/-/lambda-calculus-$pkgver.tgz")
sha256sums=('INSERT_HASH_HERE')

package() {
  npm install -g --prefix "$pkgdir/usr" "$srcdir/lambda-calculus-$pkgver.tgz"
  
  # Remove unnecessary files
  find "$pkgdir/usr" -type d -name 'node_modules' -prune -exec rm -rf {} \;
}
```

## 📦 Container/Docker Distribution

### Dockerfile

```dockerfile
FROM node:20-alpine

WORKDIR /app

# Copy package files
COPY package*.json ./
COPY tsconfig.json ./
COPY src ./src

# Install dependencies
RUN npm ci --only=production

# Build
RUN npm run build

# Remove source files
RUN rm -rf src

# Expose as volume
VOLUME ["/app/dist"]

CMD ["node", "--version"]
```

### Build and Publish

```bash
# Build image
docker build -t lambda-calculus:1.0.0 .

# Publish to Docker Hub
docker tag lambda-calculus:1.0.0 username/lambda-calculus:1.0.0
docker push username/lambda-calculus:1.0.0

# Publish to GitHub Container Registry
docker tag lambda-calculus:1.0.0 ghcr.io/username/lambda-calculus:1.0.0
docker push ghcr.io/username/lambda-calculus:1.0.0
```

## 📊 Distribution Checklist

Before publishing:

- [ ] **Tests pass**: All tests passing (`npm test`)
- [ ] **Types correct**: No TypeScript errors
- [ ] **Docs complete**: README, API docs, examples
- [ ] **License**: LICENSE.md file included
- [ ] **Version**: Semantic versioning (x.y.z)
- [ ] **Build**: Package builds successfully
- [ ] **Size**: Check bundle size (`npm pack --dry-run`)
- [ ] **Dependencies**: Peer deps documented
- [ ] **Examples**: Working examples included
- [ ] **Changelog**: CHANGELOG.md updated

## 🔄 Continuous Delivery

### GitHub Actions

```yaml
name: Publish Package

on:
  release:
    types: [published]

jobs:
  publish-npm:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '20'
          registry-url: 'https://registry.npmjs.org'
      - run: npm ci
      - run: npm run build
      - run: npm test
      - run: npm publish --access public
        env:
          NODE_AUTH_TOKEN: ${{ secrets.NPM_TOKEN }}
```

## 🎯 Best Practices

1. **Semantic Versioning**: Follow semver strictly
2. **Change Documentation**: Maintain CHANGELOG.md
3. **Breaking Changes**: Major version for breaking changes
4. **Deprecation**: Warn before removing features
5. **Security**: Keep dependencies updated
6. **Testing**: 100% test coverage goal
7. **Documentation**: Keep docs in sync with code
8. **Examples**: Provide working examples

## 📚 Resources

- [NPM Publishing Guide](https://docs.npmjs.com/packages-and-modules/contributing-packages-to-the-registry)
- [GNU Guix Packaging Tutorial](https://guix.gnu.org/manual/en/html_node/Packaging-Guidelines.html)
- [Nix Packaging Guide](https://nixos.org/manual/nixpkgs/stable/#chap-quick-start)
- [Debian Node.js Policy](https://pkg-javascript.alioth.debian.org/policy.html)

