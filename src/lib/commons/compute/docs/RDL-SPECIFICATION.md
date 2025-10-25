# Reactive Dataflow Language (RDL) - Canonical Specification

**Version:** 1.0.0  
**Specification ID:** RDL-SPEC-2025-01  
**Canonical Implementation:** Zod Schemas in `schema.ts`

---

## Overview

This document establishes **Zod schemas as the authoritative specification for RDL**.

All other representations (JSON Schema, TypeScript interfaces, documentation) are **derived** from the Zod schemas defined in `schema.ts`.

---

## Why Zod as Canonical Specification?

### 1. Single Source of Truth

**Problem with dual schemas:**
- JSON Schema (`rdl-schema.json`) + Zod schemas (`schema.ts`) = 2 sources of truth
- Changes require updates in 2 places
- Risk of divergence and inconsistency

**Solution:**
- Zod schemas = THE authoritative specification
- JSON Schema can be generated from Zod (if needed, via `zod-to-json-schema`)
- TypeScript types auto-generated from Zod via `z.infer<>`

### 2. Type Safety + Runtime Validation

Zod provides both:
- **Compile-time** type checking (TypeScript types)
- **Runtime** validation (`.parse()`, `.safeParse()`)

JSON Schema only provides runtime validation.

### 3. Developer Experience

```typescript
// With Zod as canonical:
const program: ReactiveComputationGraph = { ... };  // TypeScript knows the shape
ReactiveComputationGraphSchema.parse(program);      // Runtime validation
// ONE schema, TWO benefits

// With JSON Schema as canonical:
const program: any = { ... };                       // No TypeScript help
jsonSchemaValidator.validate(program, rdlSchema);   // Runtime only
// Need separate TypeScript interfaces
```

### 4. Self-Documenting

Zod schemas with `.describe()` are both:
- Machine-readable (for validation)
- Human-readable (for documentation)

```typescript
holster_path: HolsterPathSchema.describe('Path in Holster to subscribe to')
// ↑ This description is part of the schema itself
```

### 5. Schema Evolution

Zod makes schema evolution easier:
- `.extend()` for adding fields
- `.merge()` for composition
- `.pick()` / `.omit()` for variants
- Type safety throughout

---

## RDL Specification Structure

### Hierarchy

```
RDL Specification
├── Primitives (foundational types)
│   ├── Identifier
│   ├── HolsterPath
│   ├── PubKey
│   ├── SchemaTypeName
│   └── FunctionName
│
├── Variable Bindings (where data comes from)
│   ├── value          - Static literals
│   ├── subscription   - Reactive Holster subscriptions
│   ├── fetch          - One-time Holster fetch
│   ├── local          - Local state references
│   └── derived        - Computation outputs
│
├── Output Bindings (where results go)
│   ├── holster        - Network-replicated storage
│   ├── local          - Session-local state
│   └── memory         - Ephemeral (for chaining)
│
├── Computation (single computation step)
│   ├── id             - Unique identifier
│   ├── inputs         - Variable bindings
│   ├── compute_fn     - Function name
│   ├── local_bindings - Scoped variables
│   ├── outputs        - Output bindings
│   ├── debounce_ms    - Execution control
│   ├── enabled        - Toggle flag
│   ├── depends_on     - Dependencies
│   ├── description    - Documentation
│   └── version        - Versioning
│
└── ReactiveComputationGraph (complete program)
    ├── id             - Program identifier
    ├── variables      - Global variables
    ├── computations   - Computation pipeline
    ├── version        - Program version
    ├── description    - Program documentation
    └── program_hash   - Namespace identifier
```

### Schema Validation Levels

1. **Syntax** (Zod structure validation)
   - Required fields present
   - Types correct
   - Patterns match (identifiers, paths)

2. **Semantics** (RDL validator)
   - No circular dependencies
   - References resolve correctly
   - Functions exist
   - Schemas registered

3. **Runtime** (Execution validation)
   - Inputs match expected types
   - Outputs conform to schemas
   - Provenance verifiable

---

## Reading the Specification

### Location

**Canonical Source:** `/src/lib/commons/compute/schema.ts`

### Key Exports

```typescript
// Primitive schemas
export const IdentifierSchema: z.ZodSchema<Identifier>;
export const HolsterPathSchema: z.ZodSchema<HolsterPath>;
export const PubKeySchema: z.ZodSchema<PubKey>;
export const SchemaTypeNameSchema: z.ZodSchema<SchemaTypeName>;
export const FunctionNameSchema: z.ZodSchema<FunctionName>;

// Binding schemas
export const VariableBindingSchema: z.ZodSchema<VariableBinding>;
export const OutputBindingSchema: z.ZodSchema<OutputBinding>;

// Computation schemas
export const ComputationSchema: z.ZodSchema<Computation>;
export const ReactiveComputationGraphSchema: z.ZodSchema<ReactiveComputationGraph>;

// Provenance schemas
export const InputProvenanceSchema: z.ZodSchema<InputProvenance>;
export const OutputProvenanceSchema: z.ZodSchema<OutputProvenance>;
export const ComputationProvenanceSchema: z.ZodSchema<ComputationProvenance>;

// Corresponding TypeScript types (auto-generated from Zod)
export type Identifier = z.infer<typeof IdentifierSchema>;
export type HolsterPath = z.infer<typeof HolsterPathSchema>;
// ... (all types auto-generated)
```

---

## Using the Specification

### 1. Writing RDL Programs

```typescript
import { ReactiveComputationGraphSchema, type ReactiveComputationGraph } from './schema';

// TypeScript knows the exact shape
const myProgram: ReactiveComputationGraph = {
  id: 'my_program',
  variables: {
    myData: {
      type: 'subscription',
      holster_path: 'data/input',
      schema_type: 'DataType'
    }
  },
  computations: [{
    id: 'process',
    inputs: {
      data: { type: 'local', state_path: 'myData' }
    },
    compute_fn: 'processData',
    outputs: {
      result: { type: 'holster', holster_path: 'data/output' }
    }
  }]
};

// Validate at runtime
const result = ReactiveComputationGraphSchema.safeParse(myProgram);
if (!result.success) {
  console.error('Invalid program:', result.error);
}
```

### 2. Validating Programs

```typescript
import { validateRDL } from './rdl-validator';

const validation = validateRDL(myProgram);

if (!validation.valid) {
  console.error('Errors:', validation.errors);
  console.warn('Warnings:', validation.warnings);
}
```

### 3. Deploying Programs

```typescript
import { deployReactiveProgram } from './runtime-manager.svelte';
import { registerComputationFunction } from './compute.svelte';

// 1. Register functions
registerComputationFunction('processData', (inputs) => {
  return { result: inputs.data * 2 };
});

// 2. Deploy (auto-validates)
const manager = await deployReactiveProgram(myProgram, {
  enableProvenance: true,
  enableReactivity: true
});

// 3. Program now running!
```

---

## Extending the Specification

### Adding New Binding Types

```typescript
// 1. Extend VariableBindingSchema
export const VariableBindingSchema = z.discriminatedUnion('type', [
  // ... existing bindings ...
  
  // New binding type
  z.object({
    type: z.literal('database'),
    connection_string: z.string(),
    query: z.string(),
    schema_type: SchemaTypeNameSchema
  })
]);

// 2. Update runtime to handle new type (in compute.svelte.ts)
async function resolveVariableBinding(binding: VariableBinding, ...): Promise<ResolvedVariable> {
  switch (binding.type) {
    // ... existing cases ...
    
    case 'database': {
      // Implementation
      return { type: 'database', store: ... };
    }
  }
}

// 3. Update validator (if needed)

// 4. Update documentation
```

### Versioning

When making breaking changes:

```typescript
// v1 schema (existing)
export const ReactiveComputationGraphSchema = z.object({ ... });

// v2 schema (new)
export const ReactiveComputationGraphSchemaV2 = z.object({
  ...ReactiveComputationGraphSchema.shape,  // Extend v1
  new_field: z.string()                     // Add breaking change
});

// Migration function
export function migrateV1toV2(v1: ReactiveComputationGraph): ReactiveComputationGraphV2 {
  return {
    ...v1,
    new_field: 'default_value'
  };
}
```

---

## Generating Alternative Formats

### JSON Schema (Optional)

If you need JSON Schema for external tools:

```bash
npm install zod-to-json-schema
```

```typescript
import { zodToJsonSchema } from 'zod-to-json-schema';
import { ReactiveComputationGraphSchema } from './schema';

const jsonSchema = zodToJsonSchema(ReactiveComputationGraphSchema, {
  name: 'ReactiveComputationGraph',
  target: 'jsonSchema7'
});

// Write to file
fs.writeFileSync('rdl-schema-generated.json', JSON.stringify(jsonSchema, null, 2));
```

### OpenAPI / Swagger

For REST API documentation:

```typescript
import { extendZodWithOpenApi } from '@asteasolutions/zod-to-openapi';

extendZodWithOpenApi(z);

const schema = ReactiveComputationGraphSchema.openapi({
  title: 'RDL Program',
  description: 'Reactive Dataflow Language program specification'
});
```

---

## Benefits of This Approach

### ✅ For Developers

- **Single source of truth**: One schema to maintain
- **Type safety**: Compile-time + runtime validation
- **Auto-completion**: IDE knows the exact shape
- **Refactoring**: TypeScript catches breaking changes

### ✅ For Users

- **Clear errors**: Zod provides detailed validation errors
- **Self-documenting**: Descriptions embedded in schemas
- **Versioning**: Easy to migrate between schema versions

### ✅ For the Project

- **Consistency**: Impossible for TypeScript types and runtime validation to diverge
- **Maintainability**: Changes in one place propagate everywhere
- **Extensibility**: Easy to add new features
- **Tooling**: Can generate docs, JSON Schema, etc. from Zod

---

## Compliance

### Current State

- ✅ Zod schemas comprehensive and documented
- ✅ TypeScript types auto-generated
- ✅ Runtime validation working
- ✅ Validator integrated
- ✅ Tests passing
- ⚠️ JSON Schema is now INFORMATIONAL only (can be regenerated if needed)

### Migration from JSON Schema

**Old workflow:**
1. Update `rdl-schema.json`
2. Update `schema.ts` Zod schemas to match
3. Update TypeScript interfaces
4. Hope they stay in sync

**New workflow:**
1. Update `schema.ts` Zod schemas
2. TypeScript types auto-update
3. Regenerate JSON Schema (if needed)
4. Everything stays in sync automatically

---

## References

- **Canonical Specification**: `/src/lib/commons/compute/schema.ts`
- **Validator**: `/src/lib/commons/compute/rdl-validator.ts`
- **Runtime**: `/src/lib/commons/compute/compute.svelte.ts`
- **Deployment**: `/src/lib/commons/compute/runtime-manager.svelte.ts`
- **Tests**: `/src/lib/commons/compute/rdl-format-check.test.ts`

---

## Conclusion

**Zod schemas in `schema.ts` are the authoritative RDL specification.**

All tooling, validation, types, and documentation derive from these schemas.

This ensures:
- Consistency (one source of truth)
- Type safety (compile-time + runtime)
- Maintainability (changes propagate automatically)
- Extensibility (easy to evolve)

**The JSON Schema (`rdl-schema.json`) is now informational and can be regenerated from Zod if needed.**

---

**Last Updated:** 2025-01-25  
**Specification Version:** 1.0.0  
**Status:** ✅ Production-Ready

