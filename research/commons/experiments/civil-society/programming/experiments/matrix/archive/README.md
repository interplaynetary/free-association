# Archive

This folder contains historical files from the development of the Free Association Protocol. These files are **no longer actively used** but are preserved for reference and historical context.

---

## 📁 Folder Structure

### `waypoint-docs/`

Documentation created during development milestones. Each marks completion of a major phase:

- **IMPLEMENTATION-SUMMARY.md** - Initial RPC implementation complete
- **REFACTORING-COMPLETE.md** - Modular architecture refactoring
- **SPARSE-IMPLEMENTATION-COMPLETE.md** - Sparse matrix optimization
- **ENHANCED-FEATURES-SUMMARY.md** - Feature integration from src/lib/protocol
- **LOCAL-FIRST-IMPLEMENTATION.md** - Local-first client complete
- **CAPNWEB-ELEGANCE-COMPLETE.md** - Promise pipelining & batch mode
- **SYMMETRIC-PROTOCOL-SUMMARY.md** - Symmetric peer-to-peer implementation
- **ELEGANCE-IMPROVEMENTS.md** - Elegance refactoring plan

**Why archived:** These were "checkpoint" documents created during development. The information is now consolidated into the main documentation (README.md, LOCAL-FIRST.md, PIPELINING-GUIDE.md, etc.).

### `comparisons/`

Technical comparison documents created during development:

- **EFFICIENCY-COMPARISON.md** - Sparse vs dense matrix comparison
- **MATHEMATICAL-COMPARISON.md** - research/matrix vs src/lib/protocol math comparison
- **SPARSE-MATRIX-COMPARISON.md** - Detailed sparse optimization analysis

**Why archived:** These comparisons were useful during development to understand tradeoffs. The key findings are now integrated into main documentation.

### `old-examples/`

Examples superseded by newer, more comprehensive versions:

- **example-client.ts** - Basic client example (superseded by example-fluent-api.ts)
- **elegant-usage.ts** - Refactored architecture example (superseded by example-pipelining.ts)
- **tests.ts** - Old test file (superseded by tests.test.ts)

**Why archived:** These examples were replaced by more comprehensive, better-organized examples that demonstrate the same concepts plus additional features.

---

## 📚 Current Active Documentation

The active documentation that supersedes these archived files:

### Primary Documentation
- **README.md** - Main project overview and quick start
- **LOCAL-FIRST.md** - Local-first architecture (memoization, IndexedDB, sync)
- **PIPELINING-GUIDE.md** - Promise pipelining and batch mode guide
- **ELEGANT-ARCHITECTURE.md** - Modular architecture overview
- **SYMMETRIC-ARCHITECTURE.md** - Peer-to-peer deployment patterns

### Reference Documentation
- **math.md** - Mathematical axioms (foundational)
- **rpc.md** - Cap'n Web RPC article (reference)
- **matrix-rpc.md** - RPC protocol design (reference)

### Active Examples
- **example-pipelining.ts** - Promise pipelining examples
- **example-batch-mode.ts** - HTTP batch mode examples
- **example-fluent-api.ts** - Fluent API patterns
- **example-local-first.ts** - Local-first client examples
- **example-symmetric.ts** - Symmetric protocol examples
- **example-peer-to-peer.ts** - Peer-to-peer examples
- **example-server.ts** - Server setup example
- **example-sparse-performance.ts** - Performance benchmarks

---

## 🔍 How to Use These Archives

### If You Need Historical Context

These files document the **evolution** of the protocol:
1. Initial implementation → Modular refactoring
2. Dense matrices → Sparse optimization
3. RPC-only → Local-first
4. Sequential calls → Promise pipelining

### If You Need Implementation Details

Some archived docs contain detailed implementation decisions that might be useful if:
- Modifying core algorithms
- Understanding design rationale
- Comparing architectural approaches
- Teaching/explaining the system

### If You Want to Restore a File

Simply move it back to the parent directory:
```bash
mv archive/waypoint-docs/SOME-FILE.md ./
```

---

## 📊 Archive Statistics

- **Waypoint Docs:** 8 files (~4,000 lines)
- **Comparisons:** 3 files (~1,500 lines)
- **Old Examples:** 3 files (~800 lines)

**Total Archived:** ~6,300 lines of historical documentation and code

**Total Active:** ~14,000 lines of production code and current documentation

---

## 🎯 Why Archive Instead of Delete?

These files represent the **development journey** and contain:
- Design rationale and decisions
- Performance analysis and benchmarks
- Historical context for current implementation
- Useful for onboarding or teaching

Archiving preserves this knowledge while keeping the main project directory clean and focused.

---

**Last Updated:** December 4, 2025  
**Archive Created:** During final project organization

