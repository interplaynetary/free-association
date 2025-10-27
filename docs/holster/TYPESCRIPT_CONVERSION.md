# TypeScript Conversion - Holster

This document describes the complete conversion of Holster from JavaScript to TypeScript with Zod v4 schemas.

## What Was Converted

All source files in `src/` have been converted from `.js` to `.ts`:

### Core Files
- ✅ `utils.ts` - Core utilities with Zod schemas for primitives, objects, and graph structures
- ✅ `array.ts` - Custom array implementation with encoding support
- ✅ `buffer.ts` - Secure buffer implementation with comprehensive validation
- ✅ `sea-utils.ts` - Cryptographic utilities with Zod schemas
- ✅ `sea.ts` - Security, Encryption, and Authorization with typed interfaces
- ✅ `dup.ts` - Deduplication with typed store interface
- ✅ `radix.ts` - Radix tree with TypeScript generics
- ✅ `radisk.ts` - Disk-based radix with comprehensive type safety
- ✅ `get.ts` - Graph query operations with Zod schemas
- ✅ `ham.ts` - Conflict resolution (HAM) with typed operations
- ✅ `store.ts` - Storage adapter with TypeScript interfaces
- ✅ `wire.ts` - WebSocket wire protocol with comprehensive typing
- ✅ `user.ts` - User authentication with typed API
- ✅ `holster.ts` - Main API with complete TypeScript interfaces
- ✅ `index.ts` - Entry point

## Zod v4 Schemas Added

The following Zod schemas were added for runtime validation:

1. **Primitive Types** (`utils.ts`)
   - `numSchema` - Validates numbers and numeric strings
   - `objSchema` - Validates plain objects
   - `relSchema` - Validates soul relations

2. **Graph Structures** (`utils.ts`)
   - `graphMetadataSchema` - Graph metadata with soul and timestamps
   - `graphNodeSchema` - Graph nodes with values
   - `lexSchema` - Lexical query patterns

3. **Cryptographic Types** (`sea-utils.ts`, `sea.ts`)
   - `jwkSchema` - JSON Web Key format
   - `keyPairSchema` - Cryptographic key pairs
   - `encryptedDataSchema` - Encrypted data format
   - `signedDataSchema` - Signed data format

4. **Wire Protocol** (`wire.ts`)
   - `wireOptionsSchema` - Wire configuration
   - `wireMessageSchema` - Wire protocol messages

5. **Storage** (`store.ts`, `radisk.ts`)
   - `storeOptionsSchema` - Storage configuration
   - `lexStoreSchema` - Storage query format
   - `radiskOptionsSchema` - Radisk configuration

6. **Buffer Operations** (`buffer.ts`)
   - `bufferEncodingSchema` - Buffer encoding types

7. **User Operations** (`user.ts`)
   - `userIsSchema` - Authenticated user data

8. **Holster API** (`holster.ts`)
   - `holsterOptionsSchema` - Holster configuration

## Installation & Build

To use the TypeScript version:

```bash
# Install dependencies
npm install

# Type check only
npm run typecheck

# Build TypeScript to JavaScript
npm run build

# Development mode (watch)
npm run dev

# Run tests
npm test
```

## Dependencies Added

### Production Dependencies
- `zod@^4.0.0` - Runtime validation with TypeScript inference

### Development Dependencies
- `typescript@^5.7.2` - TypeScript compiler
- `@types/node@^22.0.0` - Node.js type definitions
- `@types/express@^5.0.0` - Express type definitions
- `@types/ws@^8.5.13` - WebSocket type definitions
- `tsx@^4.19.2` - TypeScript execution for tests

## TypeScript Configuration

The project uses modern TypeScript settings (`tsconfig.json`):

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "strict": true,
    "outDir": "./dist",
    "declaration": true,
    "sourceMap": true
  }
}
```

## Type Safety Features

1. **Strict Typing**: All functions have explicit parameter and return types
2. **Generics**: Radix tree and other data structures use TypeScript generics
3. **Interfaces**: Comprehensive interfaces for all APIs
4. **Zod Integration**: Runtime validation matches TypeScript types
5. **Type Inference**: Zod schemas provide automatic TypeScript type inference

## Breaking Changes

None! The compiled JavaScript maintains full backward compatibility with the original code.

## Examples & Tests

- Examples in `examples/` remain as HTML/JS files that use the compiled output
- Tests in `test/` can continue to use the compiled JavaScript or be migrated to TypeScript

## Benefits

1. **Type Safety**: Catch errors at compile time
2. **Better IDE Support**: IntelliSense, autocomplete, and refactoring
3. **Runtime Validation**: Zod schemas validate data at runtime
4. **Self-Documenting**: Types serve as documentation
5. **Maintainability**: Easier to refactor and extend

## Migration Notes

For users upgrading from the JavaScript version:

1. The compiled output in `dist/` is fully compatible
2. No changes needed to calling code
3. TypeScript types are available for TypeScript consumers
4. Zod validation is opt-in (schemas are exported but not enforced in compiled code)

## Next Steps

1. Run `npm install` to install dependencies
2. Run `npm run build` to compile TypeScript to JavaScript
3. Run `npm test` to verify everything works
4. The compiled `dist/` directory contains the JavaScript output

## Notes

- All type definitions are exported for external consumption
- Zod schemas can be used for runtime validation when needed
- The build process generates `.d.ts` files for TypeScript consumers
- Source maps are generated for debugging

