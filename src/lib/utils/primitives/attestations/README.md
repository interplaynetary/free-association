# Generic Attestation System

A platform-agnostic attestation framework based on the Ethereum Attestation Service (EAS) specification, implemented with Zod schemas for runtime validation and TypeScript for type safety.

## Architecture: Layered Validation

This system uses a **two-layer validation architecture** that separates concerns:

```
User Input (untrusted data)
         ↓
┌─────────────────────────┐
│   Layer 1: Zod Schema   │  ← Structural Validation
│   - Valid hex strings?  │     (syntax checking)
│   - Correct types?      │
│   - Required fields?    │
└─────────────────────────┘
         ↓ (typed, valid structure)
┌─────────────────────────┐
│ Layer 2: SchemaResolver │  ← Business Logic Validation
│ - Has permission?       │     (semantic checking)
│ - Refs exist?           │
│ - Business rules met?   │
└─────────────────────────┘
         ↓ (validated attestation)
     Storage/Execution
```

**Why this matters:**
- **Zod** = Fast, synchronous structural validation (catches format errors)
- **Resolver** = Async business logic validation (checks permissions, state, constraints)
- Clear separation makes code easier to understand, test, and maintain

## Why This Data Structure is Powerful

### 1. **Composable Trust Infrastructure**

The attestation data structure creates a **graph of verifiable claims** where each attestation can reference other attestations through `refUID`:

```typescript
// Attestation A: "Alice is a member of the mutual aid network"
const membershipAttestation = {
  uid: "0xaaa...",
  schema: "0x_membership_schema",
  recipient: "0xAlice",
  attester: "0xNetworkCoordinator",
  refUID: EMPTY_UID, // Root attestation
  data: "0x..." // Membership details
};

// Attestation B: "Bob vouches for Alice's membership"
const endorsementAttestation = {
  uid: "0xbbb...",
  schema: "0x_endorsement_schema",
  recipient: "0xAlice",
  attester: "0xBob",
  refUID: "0xaaa...", // References Alice's membership
  data: "0x..." // Endorsement details
};

// Attestation C: "Alice completed a mutual aid task"
const taskAttestation = {
  uid: "0xccc...",
  schema: "0x_task_completion_schema",
  recipient: "0xAlice",
  attester: "0xTaskVerifier",
  refUID: "0xaaa...", // References Alice's membership
  data: "0x..." // Task details
};
```

This creates a **web of trust** where claims build upon each other, enabling:
- Reputation systems
- Credential chains
- Provenance tracking
- Social graphs
- Delegated authority

### 2. **Schema-Based Flexibility**

Each attestation points to a `schema` identifier, allowing **infinite customization** while maintaining a consistent structure:

```typescript
// Different schemas for different use cases:
const schemas = {
  membership: "0x_membership_...",
  taskCompletion: "0x_task_...",
  skillEndorsement: "0x_skill_...",
  resourceAllocation: "0x_resource_...",
  disputeResolution: "0x_dispute_...",
  capacityDeclaration: "0x_capacity_..." // For Free-Association model
};

// Same attestation structure, different meanings
// The `data` field contains schema-specific encoded data
```

### 3. **Time-Bounded Trust**

The temporal fields enable **dynamic trust relationships**:

```typescript
{
  time: 1704067200n,           // When attestation was created
  expirationTime: 1735689600n, // When it expires (or 0 for permanent)
  revocationTime: 0n,          // When it was revoked (0 = still valid)
  revocable: true              // Can this be revoked?
}
```

**Use cases:**
- Temporary access grants (expires automatically)
- Renewable credentials (must be re-attested periodically)
- Time-limited commitments (capacity declarations expire)
- Revocable permissions (can be withdrawn)
- Audit trails (permanent, non-revocable records)

### 4. **Separation of Attester and Recipient**

The dual-address model creates **rich authorization patterns**:

```typescript
{
  attester: "0xAuthority",  // Who makes the claim
  recipient: "0xSubject",   // Who the claim is about
}
```

**This enables:**

- **Self-attestations**: `attester === recipient` (I declare my capacity)
- **Third-party attestations**: `attester !== recipient` (Someone vouches for me)
- **Institutional attestations**: Organizations certify individuals
- **Mutual attestations**: Two parties attest to a shared agreement
- **Delegated attestations**: Authorized parties attest on behalf of others

### 5. **The Resolver Pattern: Programmable Verification**

The **resolver** is what makes this system extraordinarily powerful. It's a **hook that validates attestations before they're created or revoked**.

Unlike Zod (which validates structure), resolvers validate **business logic** - rules that depend on the current state of your system.

```typescript
abstract class SchemaResolver {
  // Override to implement your business rules
  protected abstract onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult>;

  protected abstract onValidateRevoke(
    context: AttestationContext
  ): Promise<ValidationResult>;
}

// ValidationResult is descriptive
type ValidationResult =
  | { valid: true }
  | { valid: false; reason: string; code?: string };
```

**Why this is revolutionary:**

#### A. Programmable Business Logic

You can enforce **arbitrary rules** at attestation time:

```typescript
class MutualAidResolver extends SchemaResolver {
  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    const { attestation } = context;

    // Decode the attestation data
    const task = decodeTaskData(attestation.data);

    // Enforce mutual aid rules:
    // 1. Can't attest to your own task completion
    if (attestation.attester === attestation.recipient) {
      return {
        valid: false,
        reason: "Cannot attest to own task completion",
        code: "SELF_ATTESTATION_FORBIDDEN"
      };
    }

    // 2. Attester must be an active member
    const attesterMembership = await this.getMembershipAttestation(
      attestation.attester
    );
    if (!attesterMembership || this.isExpired(attesterMembership)) {
      return {
        valid: false,
        reason: "Attester membership expired",
        code: "MEMBERSHIP_EXPIRED"
      };
    }

    // 3. Task must be within declared capacity
    const capacity = await this.getCapacityAttestation(attestation.recipient);
    if (task.effort > capacity.available) {
      return {
        valid: false,
        reason: "Task exceeds available capacity",
        code: "CAPACITY_EXCEEDED"
      };
    }

    // 4. Must reference a valid task request
    if (attestation.refUID !== EMPTY_UID) {
      const taskRequest = await this.getAttestation(attestation.refUID);
      if (!taskRequest) {
        return {
          valid: false,
          reason: "Referenced task request not found",
          code: "REF_NOT_FOUND"
        };
      }
    }

    return { valid: true };
  }
}
```

#### B. Economic Mechanisms

The `value` parameter in `AttestationContext` enables **staking, bonds, and economic games**:

```typescript
class StakedDisputeResolver extends SchemaResolver {
  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    // Require stake to file a dispute
    const MIN_STAKE = 100n;
    if (!context.value || context.value < MIN_STAKE) {
      return {
        valid: false,
        reason: `Minimum stake of ${MIN_STAKE} required`,
        code: "INSUFFICIENT_STAKE"
      };
    }

    // Lock the stake
    await this.lockStake(context.attestation.attester, context.value);

    // If dispute is resolved in your favor, get stake back + reward
    // If resolved against you, lose stake
    return { valid: true };
  }

  protected async onValidateRevoke(
    context: AttestationContext
  ): Promise<ValidationResult> {
    // Return stake when dispute is resolved
    await this.returnStake(context.attestation.attester);
    return { valid: true };
  }

  // Enable value transfers
  public isPayable(): boolean {
    return true;
  }
}
```

#### C. Cross-Attestation Validation

Resolvers can **check other attestations** to enforce complex rules:

```typescript
class HierarchicalPermissionResolver extends SchemaResolver {
  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    const { attestation } = context;

    // Can only grant permissions you have yourself
    const permission = decodePermission(attestation.data);

    // Check if attester has the permission they're granting
    const attesterPermissions = await this.getPermissionAttestations(
      attestation.attester
    );

    if (!attesterPermissions.includes(permission.type)) {
      return {
        valid: false,
        reason: "Attester does not have this permission",
        code: "PERMISSION_NOT_HELD"
      };
    }

    // Can only grant to people in your organization
    const recipientOrg = await this.getOrganizationAttestation(
      attestation.recipient
    );
    const attesterOrg = await this.getOrganizationAttestation(
      attestation.attester
    );

    if (recipientOrg.uid !== attesterOrg.uid) {
      return {
        valid: false,
        reason: "Recipient not in same organization",
        code: "ORG_MISMATCH"
      };
    }

    return { valid: true };
  }
}
```

#### D. External System Integration

Resolvers can **query external systems** before allowing attestations:

```typescript
class OracleVerifiedResolver extends SchemaResolver {
  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    const claim = decodeClaim(context.attestation.data);

    // Check with external oracle/API
    const verified = await fetch('https://api.verifier.com/check', {
      method: 'POST',
      body: JSON.stringify({
        claimType: claim.type,
        claimData: claim.data,
        subject: context.attestation.recipient
      })
    }).then(r => r.json());

    if (!verified.isValid) {
      return {
        valid: false,
        reason: verified.reason || "Oracle verification failed",
        code: "ORACLE_REJECTED"
      };
    }

    return { valid: true };
  }
}
```

#### E. Multi-Signature Requirements

Resolvers can enforce **multi-party consensus**:

```typescript
class MultiSigResolver extends SchemaResolver {
  private pendingAttestations = new Map<string, Set<string>>();

  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    const { attestation } = context;
    const requiredSigners = await this.getRequiredSigners(attestation.schema);
    const signers = this.pendingAttestations.get(attestation.uid) ?? new Set();

    signers.add(attestation.attester);
    this.pendingAttestations.set(attestation.uid, signers);

    // Only approve when all required signers have attested
    if (signers.size >= requiredSigners.length) {
      const allRequired = requiredSigners.every(signer => signers.has(signer));
      if (allRequired) {
        this.pendingAttestations.delete(attestation.uid);
        return { valid: true };
      }
    }

    // Store as pending, don't create yet
    return {
      valid: false,
      reason: `Requires ${requiredSigners.length} signatures, have ${signers.size}`,
      code: "INSUFFICIENT_SIGNATURES"
    };
  }
}
```

### 6. **Delegated Attestations: Authorization Without Keys**

The delegated attestation pattern enables **someone to attest on your behalf**:

```typescript
const delegatedRequest: DelegatedAttestationRequest = {
  schema: "0x_task_schema",
  data: {
    recipient: "0xAlice",
    expirationTime: 0n,
    revocable: true,
    refUID: EMPTY_UID,
    data: "0x_task_data",
    value: 0n
  },
  signature: {
    v: 28,
    r: "0x...",
    s: "0x..."
  },
  attester: "0xBob",      // Bob is the actual attester
  deadline: 1735689600n   // Signature must be used by this time
};
```

**Use cases:**
- **Mobile apps**: Sign on device, submit from server
- **Batch processing**: Collect signatures offline, submit later
- **Privacy**: Attester's identity remains private until submission
- **Marketplaces**: Sell/trade attestation rights
- **Automation**: Pre-sign attestations for automated systems

## How It Applies to Free-Association Model

Based on your project's mutual-priority allocation algorithm, this system enables:

### Capacity Declarations
```typescript
const capacitySchema = "0x_capacity_v5";

// Self-attestation of capacity
const capacityAttestation = {
  schema: capacitySchema,
  recipient: "0xAlice",
  attester: "0xAlice",        // Self-attested
  expirationTime: nextWeek,    // Expires weekly
  revocable: true,             // Can update capacity
  data: encodeCapacity({
    skills: ["gardening", "coding"],
    hoursPerWeek: 10,
    priorities: ["food", "housing"]
  })
};
```

### Task Matching & Fulfillment
```typescript
// Task request
const taskRequest = {
  schema: "0x_task_request",
  recipient: "0xCommunity",
  attester: "0xBob",           // Bob needs help
  refUID: EMPTY_UID,
  data: encodeTask({
    type: "gardening",
    hours: 3,
    priority: "food"
  })
};

// Task acceptance (references the request)
const taskAcceptance = {
  schema: "0x_task_acceptance",
  recipient: "0xAlice",
  attester: "0xAlice",
  refUID: taskRequest.uid,     // Links to request
  data: "0x..."
};

// Task completion (vouched by requester)
const taskCompletion = {
  schema: "0x_task_completion",
  recipient: "0xAlice",
  attester: "0xBob",           // Bob verifies completion
  refUID: taskAcceptance.uid,  // Links to acceptance
  data: encodeCompletion({
    actualHours: 3,
    quality: "excellent"
  })
};
```

### Resolver for Mutual Aid Rules
```typescript
class FreeAssociationResolver extends SchemaResolver {
  protected async onAttest(attestation: Attestation): Promise<boolean> {
    const schema = attestation.schema;

    if (schema === "0x_task_acceptance") {
      // Can only accept tasks within your declared capacity
      const task = await this.getTask(attestation.refUID);
      const capacity = await this.getCapacity(attestation.recipient);

      if (task.hours > capacity.available) {
        return false; // Over capacity
      }

      if (!capacity.skills.includes(task.type)) {
        return false; // Don't have the skill
      }

      return true;
    }

    if (schema === "0x_task_completion") {
      // Task completion must be attested by the requester
      const acceptance = await this.getAttestation(attestation.refUID);
      const request = await this.getAttestation(acceptance.refUID);

      if (attestation.attester !== request.attester) {
        return false; // Only requester can verify completion
      }

      return true;
    }

    return true;
  }
}
```

## API Reference

### Core Schemas

#### `AttestationSchema`
The fundamental building block:
```typescript
{
  uid: string;           // Unique identifier (hex bytes32)
  schema: string;        // Schema identifier (hex bytes32)
  time: bigint;          // Creation timestamp (Unix)
  expirationTime: bigint; // Expiration (0 = never)
  revocationTime: bigint; // Revocation timestamp (0 = not revoked)
  refUID: string;        // Reference to another attestation
  recipient: string;     // Subject of attestation (hex address)
  attester: string;      // Creator of attestation (hex address)
  revocable: boolean;    // Can be revoked?
  data: string;          // Arbitrary data (hex bytes)
}
```

#### `AttestationRequestSchema`
Request to create an attestation:
```typescript
{
  schema: string;
  data: {
    recipient: string;
    expirationTime: bigint;
    revocable: boolean;
    refUID: string;
    data: string;
    value: bigint;  // Optional payment/stake
  }
}
```

### Constants

```typescript
EMPTY_UID = '0x0000000000000000000000000000000000000000000000000000000000000000';
NO_EXPIRATION_TIME = 0n;
```

### Error Classes

All errors extend standard `Error`:
- `AccessDeniedError` - Unauthorized operation
- `DeadlineExpiredError` - Signature/request expired
- `InvalidEASError` - Invalid EAS implementation (deprecated in v2)
- `InvalidLengthError` - Array length mismatch
- `InvalidSignatureError` - Signature verification failed
- `NotFoundError` - Attestation not found
- `ValidationError` - Business logic validation failed (includes optional error code)

## Usage Examples

### Complete Flow: Validation with Zod + Resolver

```typescript
import { AttestationSchema, AttestationRequestSchema } from './attestations/ieas.js';
import { EMPTY_UID, NO_EXPIRATION_TIME } from './attestations/common.js';

// Step 1: Zod validates structure
const request = AttestationRequestSchema.parse({
  schema: '0xmySchemaId...',
  data: {
    recipient: '0xAliceAddress...',
    expirationTime: NO_EXPIRATION_TIME,
    revocable: true,
    refUID: EMPTY_UID,
    data: '0xencodedData...',
    value: 0n
  }
});
// ✅ If we reach here, structure is valid

// Step 2: Create attestation object
const attestation = AttestationSchema.parse({
  uid: generateUID(),
  schema: request.schema,
  time: BigInt(Date.now()),
  expirationTime: request.data.expirationTime,
  revocationTime: 0n,
  refUID: request.data.refUID,
  recipient: request.data.recipient,
  attester: getCurrentUser(),
  revocable: request.data.revocable,
  data: request.data.data
});

// Step 3: Resolver validates business logic
const resolver = getResolverForSchema(request.schema);
if (resolver) {
  const result = await resolver.validateAttest({
    attestation,
    value: request.data.value
  });

  if (!result.valid) {
    throw new ValidationError(result.reason, result.code);
  }
}
// ✅ If we reach here, business logic is valid

// Step 4: Store the attestation
await store(attestation);
```

### Creating a Custom Resolver

```typescript
import { SchemaResolver } from './attestations/schema-resolver.js';
import { type AttestationContext, type ValidationResult } from './attestations/ischema-resolver.js';
import { EMPTY_UID } from './attestations/common.js';

class MyCustomResolver extends SchemaResolver {
  protected async onValidateAttest(
    context: AttestationContext
  ): Promise<ValidationResult> {
    const { attestation, value } = context;

    // Example: Only allow attestations with non-empty data
    if (attestation.data === '0x') {
      return {
        valid: false,
        reason: "Attestation data cannot be empty",
        code: "EMPTY_DATA"
      };
    }

    // Example: Check if referenced attestation exists
    if (attestation.refUID !== EMPTY_UID) {
      const refExists = await this.checkAttestationExists(attestation.refUID);
      if (!refExists) {
        return {
          valid: false,
          reason: "Referenced attestation does not exist",
          code: "REF_NOT_FOUND"
        };
      }
    }

    return { valid: true };
  }

  protected async onValidateRevoke(
    context: AttestationContext
  ): Promise<ValidationResult> {
    // Only original attester can revoke
    const currentUser = this.getCurrentUser();
    if (context.attestation.attester !== currentUser) {
      return {
        valid: false,
        reason: "Only the original attester can revoke",
        code: "NOT_ATTESTER"
      };
    }

    return { valid: true };
  }

  // Helper methods (implement as needed)
  private async checkAttestationExists(uid: string): Promise<boolean> {
    // Query your storage backend
    return true;
  }

  private getCurrentUser(): string {
    // Get current user from your auth system
    return '0x...';
  }
}
```

### Implementing the IEAS Interface

```typescript
import { IEAS, AttestationRequest, AttestationRequestSchema } from './attestations/ieas.js';
import { Attestation, AttestationSchema } from './attestations/common.js';
import { ValidationError } from './attestations/schema-resolver.js';

class FileSystemEAS implements IEAS {
  private resolvers = new Map<string, SchemaResolver>();

  async attest(request: AttestationRequest): Promise<string> {
    // Step 1: Zod validates structure
    const validated = AttestationRequestSchema.parse(request);

    // Step 2: Create the attestation
    const uid = generateUID();
    const attestation: Attestation = {
      uid,
      schema: validated.schema,
      time: BigInt(Date.now()),
      expirationTime: validated.data.expirationTime,
      revocationTime: 0n,
      refUID: validated.data.refUID,
      recipient: validated.data.recipient,
      attester: getCurrentUser(),
      revocable: validated.data.revocable,
      data: validated.data.data
    };

    // Validate with Zod (redundant but good for safety)
    AttestationSchema.parse(attestation);

    // Step 3: Run through resolver if exists
    const resolver = this.resolvers.get(validated.schema);
    if (resolver) {
      const result = await resolver.validateAttest({
        attestation,
        value: validated.data.value
      });

      if (!result.valid) {
        throw new ValidationError(result.reason, result.code);
      }
    }

    // Step 4: Store the attestation
    await this.store(attestation);

    return uid;
  }

  // Register a resolver for a schema
  registerResolver(schemaId: string, resolver: SchemaResolver) {
    this.resolvers.set(schemaId, resolver);
  }

  // Implement other methods (revoke, multiAttest, etc.)...
}
```

## Design Principles

1. **Layered Validation**: Zod for structure (fast, synchronous), Resolvers for business logic (async, contextual)
2. **Composability**: Attestations reference each other, building graphs of trust
3. **Flexibility**: Schemas allow infinite customization within consistent structure
4. **Temporality**: Time bounds enable dynamic, evolving trust relationships
5. **Programmability**: Resolvers enable arbitrary business logic enforcement with descriptive error messages
6. **Delegation**: Support for authorized third-party attestations
7. **Platform Agnostic**: Works with any storage/compute backend (no blockchain required)
8. **Type Safety**: Zod + TypeScript prevent runtime errors
9. **Interoperability**: Compatible with EAS standard, can bridge to Ethereum if needed

## Summary

This attestation system gives you:

1. **Structural Safety** (Zod): Guarantees valid data formats, types, and required fields
2. **Business Logic Safety** (Resolvers): Enforces permissions, constraints, and state-dependent rules
3. **Platform Independence**: Use files, databases, blockchains, or any storage backend
4. **Composable Trust**: Build complex trust networks through attestation graphs
5. **Rich Error Messages**: Validation results include human-readable reasons and machine-readable codes

The key insight: **Separate syntax validation from semantic validation**. Zod handles "is this a valid hex string?" while resolvers handle "is this person allowed to make this claim?". This separation makes the codebase cleaner, more testable, and easier to reason about.

## License

MIT
