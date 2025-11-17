# Coalition Records System

A distributed record-keeping system for the Free Association Coalition Secretariat, implementing the official record framework defined in the Coalition's governance documents.

## Overview

The Records System provides:

- **Distributed Storage**: Records are stored in each user's Holster space
- **Network Synchronization**: Subscribe to other participants' records
- **Schema Validation**: Runtime validation using Zod schemas
- **Versioned Store**: Fine-grained reactivity with ITC causality tracking
- **UN-Style UI**: Formal interface inspired by United Nations systems

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Coalition Records                       │
│                                                              │
│  ┌────────────────┐    ┌────────────────┐                  │
│  │  Record Schema │    │  Versioned     │                  │
│  │  (Zod)         │───▶│  Store (ITC)   │                  │
│  └────────────────┘    └────────────────┘                  │
│                                │                            │
│                                ▼                            │
│  ┌────────────────────────────────────────────┐            │
│  │         Holster Storage Layer              │            │
│  │                                            │            │
│  │  user().get('coalition')                   │            │
│  │    .get('records')                         │            │
│  │      .get(recordId) ─▶ Record             │            │
│  │                                            │            │
│  │    .get('record_index')                    │            │
│  │      ─▶ { ids: [...], updated: timestamp } │            │
│  └────────────────────────────────────────────┘            │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Storage Structure

### Per-User Records

Each user stores their records in their Holster space:

```
user(pubKey)
  └── coalition/
      ├── records/
      │   ├── {recordId-1}  → Record
      │   ├── {recordId-2}  → Record
      │   └── {recordId-3}  → Record
      └── record_index
          └── { ids: [recordId-1, recordId-2, ...], updated: timestamp }
```

### Record Index

The record index provides efficient discovery:

```typescript
{
  ids: string[],        // Array of record UUIDs
  updated: number       // Last update timestamp
}
```

## Record Types

The system supports 27 record types across 12 categories:

### 1. Identity & Membership
- `membership_update` - Add/remove/replace members
- `registry_entry` - Participants, members, contacts, consultants, observers
- `contact_info` - Email, public keys, verification

### 2. Recognition & Relationships
- `recognition_distribution` - Contribution/allocation weight recognition

### 3. State Declarations
- `state_declaration` - Capacities, needs, environment
- `capacity_offer` - Resource offers with conditions

### 4. Proposals & Expressions
- `proposal` - General proposals
- `statement` - Declarations, positions, announcements

### 5. Decision-Making
- `position` - Challenge, oppose, abstain
- `support_expression` - Support weights
- `decision_outcome` - Adopted, rejected, tabled
- `protocol_adoption` - Protocol versions

### 6. Invitations & Responses
- `invitation` - Assemble, membership, consultant, working group
- `invitation_response` - Accept, decline, conditional

### 7. Meetings & Assemblies
- `assembly_minutes` - Meeting records

### 8. Secretariat Actions
- `allocation_decision` - Resource allocations

### 9. Data Subscriptions
- `subscription` - Subscribe to membership, recognition, state, etc.
- `subscription_update` - Pause, resume, cancel

### 10. Derivations & Computations
- `derivation_rule` - Mutual recognition, allocation rules
- `filter_definition` - Filtering criteria
- `computed_result` - Computation outputs

### 11. Maintenance & Governance
- `record_amendment` - Corrections, clarifications, supersede
- `framework_version` - Framework version updates

### 12. Validation & Disputes
- `validation_report` - Format, logic, authority validation
- `dispute` - Factual, procedural, interpretive disputes
- `dispute_resolution` - Accepted, modified, rejected, referred

## Usage

### Initialization

Records are automatically initialized on authentication:

```typescript
// Handled by holster.ts
initializeMyRecords();
```

### Subscribing to Participants

```typescript
import { subscribeToParticipantRecords } from '$lib/network/records.svelte';

// Subscribe to a participant's records
subscribeToParticipantRecords(pubKey);
```

### Creating Records

```typescript
import { issueRecord } from '$lib/network/records.svelte';
import { v4 as uuid } from 'uuid';

const record = {
  id: uuid(),
  timestamp: new Date().toISOString(),
  issuer: myPubKey,
  type: 'proposal',
  status: 'pending',
  data: {
    proposal_type: 'protocol_change',
    title: 'Update Decision Timeline',
    content: { /* proposal details */ },
    requires_decision: true,
    decision_deadline: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString()
  }
};

await issueRecord(record);
```

### Updating Record Status

```typescript
import { updateRecordStatus } from '$lib/network/records.svelte';

// Adopt a pending record
await updateRecordStatus(
  recordId,
  'adopted',
  new Date().toISOString()
);
```

### Querying Records

```typescript
import { 
  getRecordsForParticipant,
  getRecordsByType,
  getRecordsByStatus,
  recordStats
} from '$lib/network/records.svelte';

// Get all records for a participant
const recordsStore = getRecordsForParticipant(pubKey);

// Get specific type
const proposalsStore = getRecordsByType(pubKey, 'proposal');

// Get by status
const pendingStore = getRecordsByStatus(pubKey, 'pending');

// Get statistics
$recordStats // { myRecordsCount, networkParticipantsCount, totalNetworkRecords, byType, byStatus }
```

## UI Components

### Record Page (`/record`)

The main UI for viewing records:

**Features:**
- UN-inspired formal design
- Participant selector dropdown
- Type and status filters
- Record statistics panel
- Expandable record details
- Responsive layout

**Color Palette:**
- UN Blue: `#009edb`
- UN Gold: `#f4b942`
- Status colors for pending/adopted/rejected

### Navigation

Access via:
```
/record
```

## Stores

### Primary Stores

```typescript
// My records (Map<UUID, CoalitionRecord>)
myRecords

// Network records (Map<pubKey, Map<UUID, CoalitionRecord>>)
networkRecords

// Subscribed participants (Set<string>)
subscribedRecordParticipants

// Loading states (Map<pubKey, boolean>)
recordLoadingStates
```

### Derived Stores

```typescript
// All records across all participants
allRecords

// Statistics
recordStats // { myRecordsCount, networkParticipantsCount, totalNetworkRecords, byType, byStatus }
```

### Versioned Store

The system uses a versioned store for advanced use cases:

```typescript
import { recordStore } from '$lib/network/records.svelte';

// Subscribe to field changes
recordStore.subscribeToField('status', (statusMap) => {
  console.log('Status field changed:', statusMap);
});

// Get metadata
const metadata = recordStore.getMetadata(recordId);
```

## Validation

All records are validated using Zod schemas:

```typescript
import { validateRecord } from '$lib/coalition/record';

try {
  const validRecord = validateRecord(record);
  // Record is valid
} catch (error) {
  // Handle validation error
  console.error('Invalid record:', error);
}
```

## Security

### Issuer Verification

Records are verified to ensure the issuer matches the storage location:

```typescript
// Security check in loadNetworkRecord
if (record.issuer !== pubKey) {
  console.warn('Issuer mismatch detected');
}
```

### Schema Validation

All records must pass schema validation before storage:

```typescript
// Validation in issueRecord
validateRecord(record); // Throws if invalid
```

## Performance

### Efficient Loading

- Records are loaded on-demand via subscriptions
- Index-based discovery (only load what's needed)
- Debounced updates to prevent thrashing

### Fine-Grained Reactivity

The versioned store provides field-level tracking:

```typescript
// Only triggers when 'status' field changes
recordStore.subscribeToField('status', callback);
```

### Memory Management

- Automatic cleanup on signout
- Unsubscribe from participants to free memory
- Garbage collection of unused records

## Testing

### Manual Testing

1. Start the dev server: `bun run dev`
2. Navigate to `/record`
3. Select a participant
4. View their records
5. Test filters and search

### Unit Tests

```bash
# Run tests
bun test src/lib/coalition/record.test.ts
```

## Future Enhancements

### Planned Features

1. **Record Creation UI** - Forms for creating new records
2. **Signature Verification** - Cryptographic record signing
3. **Conflict Resolution** - Automatic merge strategies
4. **Export/Import** - JSON/CSV export for archival
5. **Search** - Full-text search across records
6. **Audit Trail** - Complete history tracking
7. **Batch Operations** - Bulk record management

### Integration Opportunities

- **Decider Integration** - Link decisions to records
- **Allocation Integration** - Connect allocations to records
- **Organization Integration** - Org-level record aggregation
- **Timeline View** - Chronological record visualization

## Contributing

When adding new record types:

1. Update `src/lib/coalition/record.ts` with new schemas
2. Update the UI to handle the new type
3. Add validation tests
4. Update this documentation

## License

Part of the Free Association Coalition project.

## References

- [Record Format Specification](../../../docs/coalition/secretariat/record/format.md)
- [Decision-Making Protocol](../../../docs/coalition/secretariat/decision-making-protocol.md)
- [Participation Framework](../../../docs/coalition/participation-framework.md)

