# Lambda Calculus Directory Structure

Clean, organized structure for the Free Association RPC framework.

## Root Level

```
lambda-calculus/
├── README.md                 # Main package documentation
├── LICENSE                   # MIT License
├── CHANGELOG.md              # Version history
├── package.json              # NPM package config
├── tsup.config.ts            # Build configuration
├── index.ts                  # Main entry point
├── itc.ts                    # Interval Tree Clock implementation
│
├── src/                      # Core implementations
│   ├── core/                 # Original dense matrix implementation
│   ├── elegant/              # Elegant API with tests
│   └── sparse/               # Sparse matrix optimizations
│
├── rpc/                      # RPC framework (Cap'n Web style)
│   ├── Core APIs
│   ├── Identity & Security
│   ├── State Restoration
│   ├── Server Implementations
│   ├── Examples
│   └── Documentation
│
├── docs/                     # Package-level documentation
│   └── status/               # Build/publish status docs
│
├── scripts/                  # Build and verification tools
│   └── verify-package.sh
│
├── examples/                 # Top-level examples
└── dist/                     # Built output (generated)
```

## RPC Directory Structure

The `rpc/` directory contains the complete Cap'n Web-style RPC framework:

```
rpc/
├── Core APIs
│   ├── api.ts                      # EntityAPI and EntityFullAPI interfaces
│   ├── rpc-target.ts               # Base class for RPC objects
│   ├── entity-session.ts           # Main session implementation (with SecureContext)
│   ├── types.ts                    # Core type definitions
│   ├── json-rpc.ts                 # JSON-RPC serialization
│   ├── errors.ts                   # Type-safe error classes
│   ├── factories.ts                # One-line object creation
│   ├── simple-api.ts               # Simplified API for common cases
│   └── peer-connection.ts          # Symmetric P2P connections
│
├── Identity & Security
│   ├── identity/
│   │   ├── keypair.ts              # Ed25519 keypair management
│   │   ├── credentials.ts          # Challenge-response auth
│   │   ├── signing.ts              # Cryptographic signing primitives
│   │   ├── index.ts                # Identity exports
│   │   └── __tests__/
│   │       └── signing.test.ts
│   │
│   └── security/
│       ├── secure-context.ts       # Root security context
│       ├── secure-storage.ts       # Auto-signing storage
│       ├── integration-guide.ts    # Integration documentation
│       ├── index.ts                # Security exports
│       └── __tests__/
│           ├── secure-context.test.ts
│           ├── secure-storage.test.ts
│           └── secure-session.test.ts
│
├── State Restoration
│   ├── restoration/
│   │   ├── login.ts                # One-line login with state restoration
│   │   ├── discovery.ts            # Replica discovery (promise pipelining)
│   │   ├── batch.ts                # HTTP batch mode for state loading
│   │   ├── state-proxy.ts          # Lazy state loading proxy
│   │   └── reconstruct.ts          # CRDT-based state merging
│   │
│   ├── verification/
│   │   └── merkle.ts               # Merkle tree verification
│   │
│   └── replication/
│       ├── manager.ts              # Replica management
│       ├── sync-strategy.ts        # Synchronization strategies
│       └── index.ts
│
├── Storage & Caching
│   ├── browser-storage.ts          # IndexedDB persistence
│   ├── cache.ts                    # In-memory cache with TTL
│   └── serialization.ts            # Graph serialization
│
├── Capacity Management
│   └── capacity/
│       ├── rate-limiter.ts         # Rate limiting
│       ├── bandwidth-throttle.ts   # Bandwidth control
│       ├── storage-quota.ts        # Storage limits
│       ├── types.ts                # Capacity types
│       └── index.ts
│
├── Clock & Causality
│   └── clock/
│       ├── itc-adapter.ts          # ITC implementation
│       └── index.ts
│
├── Transport Layer
│   ├── transport.ts                # Abstract transport interface
│   └── transports/
│       ├── websocket.ts            # WebSocket transport
│       ├── postmessage.ts          # PostMessage (iframe) transport
│       ├── webrtc.ts               # WebRTC P2P transport
│       ├── http-batch.ts           # HTTP batch transport
│       ├── types.ts                # Transport types
│       └── index.ts
│
├── Server Implementations
│   └── server/
│       ├── relay-server.ts         # RelayServer (in rpc/ root for visibility)
│       ├── workers.ts              # Cloudflare Workers deployment
│       ├── node.ts                 # Node.js/Bun deployment
│       ├── rpc-dispatcher.ts       # Unified RPC dispatch
│       ├── message-handler.ts      # WebSocket message handling
│       ├── http-handler.ts         # HTTP request handling
│       ├── middleware.ts           # Server middleware pattern
│       └── wrangler.toml           # Cloudflare Workers config
│
├── Advanced Features
│   ├── capability-manager.ts       # Capability token management
│   ├── elegant/
│   │   ├── promise-pipeline.ts     # Promise pipelining (WIP)
│   │   └── index.ts
│   └── protocol/
│       └── index.ts                # Future protocol extensions
│
├── Examples
│   └── examples/
│       ├── secure-authentication.ts  # NEW: Security examples
│       ├── peer-to-peer.ts
│       ├── offline-sync.ts
│       ├── http-batch-example.ts
│       ├── collective-coordination.ts
│       ├── elegance-demo.ts
│       ├── index.ts
│       └── apps/
│           ├── simple-chat.ts
│           └── README.md
│
├── Tests
│   └── __tests__/
│       ├── simple-api.test.ts
│       ├── cache.test.ts
│       ├── serialization.test.ts
│       └── itc-integration.test.ts
│
├── Documentation
│   ├── docs/
│   │   ├── README.md               # RPC documentation index
│   │   ├── OVERVIEW.md             # High-level overview
│   │   ├── STATE-RESTORATION.md    # State restoration guide (UPDATED with security)
│   │   ├── SECURITY-COMPLETE.md    # Complete security documentation
│   │   ├── STATE-SECURITY.md       # Security analysis
│   │   └── archive/                # Historical/status documents (23 files)
│   │       ├── CAPABILITIES-VS-SIGNATURES.md
│   │       ├── CAPNWEB-SECURITY-INSIGHTS.md
│   │       ├── IMPLEMENTATION-COMPLETE.md
│   │       ├── SECURITY-INTEGRATION-COMPLETE.md
│   │       └── ... (19 more status docs)
│   │
│   └── index.ts                    # Main RPC exports
```

## Key Changes Made

### ✅ Root Level - Cleaned Up
**Before**: 15+ files cluttering the root  
**After**: 7 essential files only

- ✅ Moved 8 status/publish docs to `docs/status/`
- ✅ Moved `verify-package.sh` to `scripts/`
- ✅ Deleted empty `universal.md`
- ✅ Kept essential: README, LICENSE, CHANGELOG, package.json, tsup.config.ts, index.ts

### ✅ RPC Directory - Organized
**Before**: 23 markdown files scattered throughout  
**After**: Clean structure with docs in `rpc/docs/archive/`

- ✅ Moved 23 status/progress docs to `rpc/docs/archive/`
- ✅ Kept current docs in `rpc/docs/`: OVERVIEW, README, SECURITY-COMPLETE, STATE-RESTORATION, STATE-SECURITY
- ✅ All code remains in logical subdirectories

## Directory Purpose

| Directory | Purpose |
|-----------|---------|
| `src/` | Core lambda calculus implementations (core, elegant, sparse) |
| `rpc/` | Complete RPC framework with all features |
| `docs/` | Package-level documentation and guides |
| `docs/status/` | Build and publishing status documents |
| `rpc/docs/` | RPC-specific documentation (current) |
| `rpc/docs/archive/` | Historical implementation notes and status updates |
| `scripts/` | Build and verification scripts |
| `examples/` | Top-level package examples |
| `rpc/examples/` | RPC-specific examples |
| `dist/` | Built output (generated by tsup) |

## Finding Documentation

### For Users
- **Getting Started**: [`README.md`](README.md)
- **RPC Overview**: [`rpc/docs/OVERVIEW.md`](rpc/docs/OVERVIEW.md)
- **State Restoration**: [`rpc/docs/STATE-RESTORATION.md`](rpc/docs/STATE-RESTORATION.md)
- **Security Guide**: [`rpc/docs/SECURITY-COMPLETE.md`](rpc/docs/SECURITY-COMPLETE.md)

### For Developers
- **Core vs Elegant**: [`docs/CORE-VS-ELEGANT.md`](docs/CORE-VS-ELEGANT.md)
- **Deployment**: [`docs/DEPLOYMENT-GUIDE.md`](docs/DEPLOYMENT-GUIDE.md)
- **Structure**: [`docs/STRUCTURE.md`](docs/STRUCTURE.md)

### Historical Reference
- **Implementation notes**: [`rpc/docs/archive/`](rpc/docs/archive/)
- **Publishing status**: [`docs/status/`](docs/status/)

## Benefits

1. **Cleaner Root**: Only 7 essential files at package root
2. **Logical Grouping**: All status docs in `docs/status/`, all RPC history in `rpc/docs/archive/`
3. **Easy Navigation**: Current docs are prominent, historical docs are archived
4. **Standard Layout**: Follows npm package best practices
5. **Future-Proof**: Clear places for new docs (guides in `docs/`, RPC docs in `rpc/docs/`)

## Next Steps

The structure is now clean and maintainable! All implementation is complete and properly organized.

