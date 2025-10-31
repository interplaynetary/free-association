import { z } from 'zod';

/**
 * LWW-Element-Set CRDT implementation for object fields
 * Each field is an independent LWW register that can be merged deterministically
 * 
 * Key CRDT properties:
 * 1. Commutative: merge(A, B) = merge(B, A)
 * 2. Associative: merge(merge(A, B), C) = merge(A, merge(B, C))
 * 3. Idempotent: merge(A, A) = A
 */

/**
 * Type for a versioned field with CRDT metadata
 */
interface VersionedField<T = any> {
  value: T;
  timestamp: number;
  nodeId: string;
  deleted: boolean;
}

/**
 * Type for versioned data object
 */
type VersionedData = Record<string, VersionedField>;

/**
 * Creates a versioned field with CRDT metadata
 * Uses Hybrid Logical Clock (HLC) or Lamport timestamp for causality
 */
const versionedField = (valueSchema: z.ZodSchema) => z.object({
  value: valueSchema,
  timestamp: z.number(), // Logical timestamp (milliseconds or Lamport clock)
  nodeId: z.string(), // Unique node/client ID for tie-breaking
  deleted: z.boolean().default(false)
});

/**
 * Generates a unique node ID (in production, use UUID or client ID)
 */
let NODE_ID = `node-${Math.random().toString(36).substr(2, 9)}`;

function setNodeId(id: string) {
  NODE_ID = id;
}

/**
 * Logical clock for generating monotonically increasing timestamps
 * Implements a simple Lamport clock
 */
class LogicalClock {
  private counter: number;
  constructor() {
    this.counter = 0;
  }
  
  tick() {
    this.counter++;
    return this.counter;
  }
  
  update(remoteTimestamp: number) {
    this.counter = Math.max(this.counter, remoteTimestamp) + 1;
    return this.counter;
  }
  
  now() {
    return this.tick();
  }
}

const clock = new LogicalClock();

/**
 * Transforms a regular Zod schema into a versioned CRDT schema
 */
function createVersionedSchema(baseSchema: z.ZodObject<any>) {
  const shape = baseSchema.shape;
  const versionedShape: Record<string, z.ZodOptional<any>> = {};
  
  for (const [key, fieldSchema] of Object.entries(shape)) {
    versionedShape[key] = versionedField(fieldSchema as z.ZodSchema).optional();
  }
  
  return z.object(versionedShape);
}

/**
 * Converts plain data to versioned CRDT format
 */
function toVersioned(data: Record<string, any>, schema: z.ZodObject<any>): VersionedData {
  const versionedSchema = createVersionedSchema(schema);
  const timestamp = clock.now();
  
  const versionedData: VersionedData = {};
  for (const [key, value] of Object.entries(data)) {
    if (value !== undefined) {
      versionedData[key] = {
        value,
        timestamp,
        nodeId: NODE_ID,
        deleted: false
      };
    }
  }
  
  return versionedSchema.parse(versionedData) as VersionedData;
}

/**
 * CRDT Merge function - implements LWW-Element-Set semantics
 * Deterministic merge that satisfies CRDT properties
 */
function mergeVersioned(base: VersionedData, incoming: VersionedData): VersionedData {
  // Update our logical clock based on incoming timestamps
  for (const field of Object.values(incoming)) {
    if (field?.timestamp) {
      clock.update(field.timestamp);
    }
  }
  
  const result: VersionedData = {};
  const allFields = new Set([
    ...Object.keys(base),
    ...Object.keys(incoming)
  ]);
  
  for (const field of allFields) {
    const baseField = base[field];
    const incomingField = incoming[field];
    
    // If only one side has the field, use it
    if (!baseField) {
      result[field] = incomingField;
      continue;
    }
    if (!incomingField) {
      result[field] = baseField;
      continue;
    }
    
    // LWW conflict resolution
    // 1. Compare timestamps (higher wins)
    // 2. If equal, use nodeId for deterministic tie-breaking
    const baseTime = baseField.timestamp;
    const incomingTime = incomingField.timestamp;
    
    if (incomingTime > baseTime) {
      result[field] = incomingField;
    } else if (baseTime > incomingTime) {
      result[field] = baseField;
    } else {
      // Timestamps equal - use nodeId for deterministic ordering
      // This ensures commutativity: merge(A,B) = merge(B,A)
      result[field] = baseField.nodeId > incomingField.nodeId 
        ? baseField 
        : incomingField;
    }
  }
  
  return result;
}

/**
 * Updates specific fields with new values
 */
function updateFields(versionedData: VersionedData, updates: Record<string, any>): VersionedData {
  const timestamp = clock.now();
  const result = { ...versionedData };
  
  for (const [key, value] of Object.entries(updates)) {
    result[key] = {
      value,
      timestamp,
      nodeId: NODE_ID,
      deleted: false
    };
  }
  
  return result;
}

/**
 * Marks fields as deleted (tombstone)
 */
function deleteFields(versionedData: VersionedData, fieldNames: string[]): VersionedData {
  const timestamp = clock.now();
  const result = { ...versionedData };
  
  for (const fieldName of fieldNames) {
    if (result[fieldName]) {
      result[fieldName] = {
        value: result[fieldName].value,
        timestamp,
        nodeId: NODE_ID,
        deleted: true
      };
    }
  }
  
  return result;
}

/**
 * Extracts plain values from versioned data
 */
function fromVersioned(versionedData: VersionedData, includeDeleted = false): Record<string, any> {
  const result: Record<string, any> = {};
  
  for (const [key, field] of Object.entries(versionedData)) {
    if (field && (!field.deleted || includeDeleted)) {
      result[key] = field.value;
    }
  }
  
  return result;
}

// ============= EXAMPLE: CRDT PROPERTIES =============
/*
const UserSchema = z.object({
  name: z.string(),
  email: z.string().email(),
  status: z.string()
});

console.log('=== Demonstrating CRDT Properties ===\n');

// Simulate two nodes making concurrent edits
setNodeId('client-A');
const clientA_initial = toVersioned({
  name: 'Bob',
  email: 'bob@example.com',
  status: 'active'
}, UserSchema);

setNodeId('client-B');
const clientB_initial = { ...clientA_initial }; // Both start with same state

// Client A updates name
setNodeId('client-A');
const clientA_update = updateFields(clientA_initial, { name: 'Robert' });

// Client B updates email (concurrent with A's update)
setNodeId('client-B');
const clientB_update = updateFields(clientB_initial, { email: 'robert@newmail.com' });

console.log('Client A updated name:', fromVersioned(clientA_update));
console.log('Client B updated email:', fromVersioned(clientB_update));

// Test CRDT property 1: Commutativity
// merge(A, B) should equal merge(B, A)
const mergeAB = mergeVersioned(clientA_update, clientB_update);
const mergeBA = mergeVersioned(clientB_update, clientA_update);

console.log('\n=== Commutativity Test ===');
console.log('merge(A, B):', fromVersioned(mergeAB));
console.log('merge(B, A):', fromVersioned(mergeBA));
console.log('Are equal?', JSON.stringify(mergeAB) === JSON.stringify(mergeBA));

// Test CRDT property 2: Idempotency
// merge(A, A) should equal A
const mergeAA = mergeVersioned(clientA_update, clientA_update);
console.log('\n=== Idempotency Test ===');
console.log('merge(A, A) equals A?', JSON.stringify(mergeAA) === JSON.stringify(clientA_update));

// Test CRDT property 3: Associativity
// merge(merge(A, B), C) should equal merge(A, merge(B, C))
setNodeId('client-C');
const clientC_update = updateFields(clientB_initial, { status: 'away' });

const mergeAB_C = mergeVersioned(mergeVersioned(clientA_update, clientB_update), clientC_update);
const mergeA_BC = mergeVersioned(clientA_update, mergeVersioned(clientB_update, clientC_update));

console.log('\n=== Associativity Test ===');
console.log('merge(merge(A,B), C):', fromVersioned(mergeAB_C));
console.log('merge(A, merge(B,C)):', fromVersioned(mergeA_BC));
console.log('Are equal?', JSON.stringify(mergeAB_C) === JSON.stringify(mergeA_BC));

console.log('\n=== Final Merged State ===');
console.log(fromVersioned(mergeAB));

console.log('\n=== CRDT Comparison ===');
console.log('This implements: LWW-Element-Set (Last-Write-Wins per field)');
console.log('Similar to: Riak, Cassandra conflict resolution');
console.log('Alternative CRDTs: G-Counter, PN-Counter, OR-Set, CRDT JSON');
*/

// ============= EXPORTS =============

export {
  setNodeId,
  toVersioned,
  mergeVersioned,
  updateFields,
  deleteFields,
  fromVersioned,
  createVersionedSchema
};

export type { VersionedField, VersionedData };