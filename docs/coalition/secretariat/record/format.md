## **Secretariat Record Structure**

### **Base Record Fields**

| Field | Type | Required | Description | Example |
| :---- | :---- | :---- | :---- | :---- |
| **id** | UUID | ✅ | Unique identifier for this record | `"123e4567-e89b-12d3-a456-426614174000"` |
| **timestamp** | ISO DateTime | ✅ | When record was created | `"2024-01-15T10:30:00Z"` |
| **issuer** | UUID | ✅ | Participant who created this record | `"987e6543-e21b-12d3-a456-426614174000"` |
| **type** | String | ✅ | Record type (see below) | `"membership_update"` |
| **status** | Enum | ✅ | `pending`, `adopted`, `rejected` | `"pending"` |
| **decision\_timestamp** | ISO DateTime | ❌ | When Secretariat decided | `"2024-01-20T14:22:00Z"` |

## **Data Topology & Storage Model**

### **Participant-Centric Architecture**

Records use a **participant-centric topology** where each participant maintains their own record space:

```
/participants/
  /{participant-uuid}/
    /records/
      /{record-id}.json    ← Participant writes only here
    /subscriptions/
      /{subscription-id}.json
```

**Write Authority:** Each participant can only write to their own namespace. This provides:
- Natural authentication (issuer verification = namespace ownership)
- No write coordination needed (no concurrent write conflicts)
- Network partition tolerance (local-first architecture)
- Clear data sovereignty (each participant owns their data)

**Read Aggregation:** All members listen to all other participants' record spaces and locally aggregate into derived views. No shared write space exists.

### **Concurrent Record Handling (CRDT Semantics)**

When multiple members post records simultaneously or network partitions occur, the system uses **Conflict-free Replicated Data Type (CRDT)** semantics:

**Per-Field Versioning:**
Each field within a record carries metadata for conflict resolution:
```json
{
  "content": {
    "value": "Proposal text",
    "timestamp": 1700000000000,
    "nodeId": "550e8400-e29b-41d4-a716-446655440001"
  }
}
```

**Conflict Resolution:**
- **Last-Write-Wins (LWW):** On conflict, the field with the higher timestamp wins
- **Tie-Breaking:** If timestamps equal, lexicographically higher nodeId wins
- **Automatic Convergence:** All members' views eventually converge to identical state
- **Amendment Merging:** Field-level merging ensures no data loss when amendments overlap

**Implementation Note:** Implementations MAY optimize by storing plain values when no conflicts exist, only expanding to versioned format when merging is required.

## **Record Types & Their Data Fields**

### **1. Identity & Membership** (Who we are)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `membership_update` | `organization`: string \`members\`: UUID\[\] \`action\`: add/remove/replace | Update org membership |
| `registry_entry` | `registry_type`: participants/members/contacts \`entry\_id\`: UUID \`entry\_data\`: object \`action\`: add/update \`effective\_from\`: ISO DateTime | Generic registry management |
| `contact_info` | `participant_id`: UUID \`email\`: string \`public\_key\`: string \`verification\_method\`: pgp/x509/did \`verified\_at\`: ISO DateTime | Maintain contact registry |

### **2. Recognition & Relationships** (How we relate)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `recognition_distribution` | `recognized_entity`: UUID \`recognition\_percentage\`: \-100 to 100 \`rationale\`: string \`recognition\_type\`: contribution/allocation\_weight | Assign recognition to entity |

### **3. State Declarations** (What we have/need)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `state_declaration` | `category`: capacities/needs/environment \`assets\`: object \`valid\_until\`: ISO DateTime | Declare state |
| `capacity_offer` | `resource_type`: string \`quantity\`: number \`conditions\`: string \`expiry\`: ISO DateTime | Offer specific capacity |

### **4. Proposals & Expressions** (What we propose)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `proposal` | `proposal_type`: string \`title\`: string \`content\`: object \`requires\_decision\`: boolean \`decision\_deadline\`: ISO DateTime | Generic proposal structure |
| `statement` | `statement_type`: declaration/position/announcement \`content\`: string \`referenced\_records\`: UUID\[\] | Public statements/expressions |

### **5. Decision-Making** (How we decide)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `position` | `proposal_id`: UUID \`position\`: challenge/oppose/abstain \`rationale\`: string | Challenge, oppose, or abstain (no weight) |
| `support_expression` | `proposal_id`: UUID \`weights\`: Map<candidate\_uuid, 0-1> \`total\_weight\`: 1.0 | Distribute support across proposal candidates |
| `decision_outcome` | `proposal_id`: UUID \`outcome\`: adopted/rejected/tabled \`vote\_summary\`: object | Record decision |
| `protocol_adoption` | `protocol_name`: string \`protocol\_version\`: string \`rules\`: object \`replaces\_previous\`: UUID \`content\_hash\`: string | Adopt new protocols |

**Note:** `support_expression` is separate from `position` for efficiency. One support expression record maps all candidate versions (original + modifications) to weights, rather than creating N separate position records.

### **6. Invitations & Responses** (How we convene)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `invitation` | `invitation_type`: assemble/secretariat\_membership/consultant/working\_group \`invited\_participants\`: UUID\[\] \`role\`: string \`context\`: object \`response\_deadline\`: ISO DateTime | Generic invitation structure |
| `invitation_response` | `invitation_id`: UUID \`response\`: accept/decline/conditional \`conditions\`: string \`availability\`: object | Respond to invitations |

### **7. Meetings & Assemblies** (How we meet)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `assembly_minutes` | `invitation_id`: UUID \`attendees\`: UUID\[\] \`decisions\_made\`: UUID\[\] \`action\_items\`: string\[\] | Record meeting outcomes |

### **8. Secretariat Actions** (What we allocate)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `allocation_decision` | `resources`: object \`from\_participant\`: UUID \`to\_participant\`: UUID \`recognition\_basis\`: object \`conditions\`: string | Allocate resources |

### **9. Data Subscriptions** (How we stay informed)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `subscription` | `subscription_type`: membership/recognition/state/derivation \`source\_entity\`: UUID \`filters\`: object \`notification\_method\`: webhook/poll | Subscribe to data streams |
| `subscription_update` | `subscription_id`: UUID \`action\`: pause/resume/cancel \`reason\`: string | Manage subscriptions |

**Subscription Lifecycle:**

1. **Creation:** Participant posts `subscription` record to their own record space
2. **Activation:** Implementation begins listening to source entity's record stream
3. **Filtering:** Only records matching `filters` criteria trigger notifications
4. **Notification:** Deliver updates per `notification_method` (webhook or poll endpoint)
5. **Maintenance:** Subscription remains active until cancelled or source unavailable
6. **Cleanup:** Post `subscription_update` with action="cancel" to terminate

**Cleanup Requirement:** Implementations MUST provide unsubscribe mechanisms to prevent resource leaks. Cancelled subscriptions SHOULD be removed from active subscription lists within one deliberation window (7 days default).

**Listener Management Pattern:**
```typescript
// Track cleanup functions
private unsubscribers: Array<() => void> = [];

// When subscribing
const unsub = listenToSource(sourceId, callback);
this.unsubscribers.push(unsub);

// On cleanup
destroy() {
  this.unsubscribers.forEach(unsub => unsub());
  this.unsubscribers = [];
}
```

### **10. Derivations & Computations** (What we compute)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `derivation_rule` | `rule_name`: string \`rule_type\`: mutual\_recognition/org\_recognition/allocation/filter \`algorithm\`: string \`parameters\`: object \`applies\_to\`: UUID\[\] | Define computation rules |
| `filter_definition` | `filter_name`: string \`criteria\`: object \`applies\_to\_type\`: recognition/state/membership \`priority\`: number | Define data filters |
| `computed_result` | `computation_type`: string \`input\_records\`: UUID\[\] \`result\_data\`: object \`algorithm\_version\`: string \`computed\_at\`: ISO DateTime | Store computation outputs |

### **11. Maintenance & Governance** (How we evolve)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `record_amendment` | `original_record`: UUID \`amendment\_type\`: correction/clarification/supersede \`changes\`: object \`justification\`: string | Amend existing records |
| `framework_version` | `version_id`: string \`changes\_from\_previous\`: string \`adoption\_record\`: UUID \`effective\_date\`: ISO DateTime | Track framework versions |

### **12. Validation & Disputes** (How we ensure quality)

| Type | Data Fields | Purpose |
| :---- | :---- | :---- |
| `validation_report` | `validated_record`: UUID \`validation\_type\`: format/logic/authority \`status\`: valid/invalid/warning \`issues\`: object\[\] | Report validation results |
| `dispute` | `disputed_record`: UUID \`dispute\_type\`: factual/procedural/interpretive \`complainant\`: UUID \`grounds\`: string \`proposed\_resolution\`: object | Raise disputes |
| `dispute_resolution` | `dispute_id`: UUID \`resolution\_type\`: accepted/modified/rejected/referred \`resolution\_details\`: object \`decided\_by\`: UUID\[\] | Resolve disputes |

## **Operation Flow**

1. **Any Participant** → Posts records to Secretariat Record  
2. **Secretariat Members** → Post `position` records on proposals  
3. **Secretariat** → Applies decision protocol, updates `status` and `decision_timestamp`  
4. **Adopted Records** → Become binding for Secretariat actions

## **Derived Views (Computed)**

### **Three-Tier Computation Model**

Derived views are organized in three tiers to optimize performance and maintain clear dependency chains:

**Tier 1: Raw Records** (Source of Truth)
- Participant-owned record streams
- Append-only, immutable
- Network-synchronized via subscriptions

**Tier 2: Aggregated Collections** (First-Order Derivations)
- Computed by collecting and filtering raw records
- Recomputed when any source record changes
- Cached for performance

**Tier 3: Computed Views** (Higher-Order Derivations)
- Computed from Tier 2 aggregations
- Recomputed when Tier 2 changes
- May combine multiple Tier 2 sources

### **Tier 2: Aggregated Collections**

| View | Structure | Calculation |
| :---- | :---- | :---- |
| **Current Membership** | Map<organization, UUID[]> | Latest `membership_update` per organization (action="add/replace") |
| **Contact Registry** | Map<participant\_uuid, ContactInfo> | Latest `contact_info` per participant where verified |
| **All Proposals** | Map<proposal\_id, Proposal> | All `proposal` records indexed by id |
| **Challenges By Proposal** | Map<proposal\_id, Position[]> | Group all `position` records where position="challenge" by proposal_id |
| **Support By Proposal** | Map<proposal\_id, SupportExpression[]> | Group all `support_expression` records by proposal_id |
| **Modifications By Proposal** | Map<original\_proposal\_id, Proposal[]> | Group modification proposals by their referenced original |
| **Active Subscriptions** | Map<subscription\_id, Subscription> | All `subscription` records not cancelled/paused |
| **Active Invitations** | Map<invitation\_id, Invitation> | All `invitation` records before response_deadline without responses |

**Performance Benefit:** O(1) lookup per proposal instead of O(n) filtering over all records.

### **Tier 3: Computed Views**

| View | Calculation | Dependencies |
| :---- | :---- | :---- |
| **Effective Recognition** | Aggregate latest `recognition_distribution` per entity | Raw records |
| **Mutual Recognition** | Min(recognition A→B, recognition B→A) for each pair | Effective Recognition |
| **Organizational Recognition** | Aggregate recognition weighted by org membership | Current Membership + Effective Recognition |
| **Available Capacities** | Sum latest `state_declaration` where category="capacities" | Raw records |
| **Unmet Needs** | Latest needs minus allocated resources | Available Capacities + allocation decisions |
| **Pending Proposals** | Filter proposals with status="pending" where requires_decision=true | All Proposals + Challenges By Proposal |
| **Early Adopted Proposals** | Proposals with no challenges after challenge window | All Proposals + Challenges By Proposal + time |
| **Active Decisions** | All records with `status="adopted"` not superseded by amendments | Raw records + amendments |
| **Applied Filters** | All adopted `filter_definition` ordered by priority | Raw records |
| **Computation Rules** | All adopted `derivation_rule` records with their parameters | Raw records |
| **Consensus Results** | For each proposal, highest-weighted candidate version | Support By Proposal + Modifications By Proposal |

### **Reactivity Model**

Implementations SHOULD use reactive programming patterns where view updates propagate automatically through the dependency graph:

```
Raw Record Change
  ↓
Tier 2 Aggregations Update (if affected)
  ↓
Tier 3 Computations Update (if dependencies changed)
  ↓
UI/Application Layer Notified
```

**Optimization:** Implementations MAY use incremental updates rather than full recomputation when record changes are localized.

## **Shared Abstraction Patterns**

### **Generic Records** (for extensibility)

Several record types use generic structures that can be specialized:

1. **`proposal`** — Can represent any proposal type via `proposal_type` field
   - Examples: agenda_proposal, protocol_change, resource_request, policy_recommendation

2. **`invitation`** — Can represent any invitation via `invitation_type` field  
   - Examples: assemble, secretariat_membership, consultant, working_group, observer

3. **`registry_entry`** — Can manage any registry via `registry_type` field
   - Examples: participants, members, contacts, consultants, observers

4. **`statement`** — Can express various statements via `statement_type` field
   - Examples: declaration, position, announcement, clarification

5. **`subscription`** — Can subscribe to any data stream via `subscription_type` field
   - Examples: membership, recognition, state, derivation, proposals, decisions

### **Usage Examples**

**Invite consultant:**
```json
{
  "type": "invitation",
  "invitation_type": "consultant",
  "invited_participants": ["uuid-of-expert"],
  "role": "Climate finance advisor",
  "context": {"project": "COP30 coordination", "duration": "6 months"}
}
```

**Propose agenda:**
```json
{
  "type": "proposal",
  "proposal_type": "agenda",
  "title": "Annual Assembly 2025 Agenda",
  "content": {"items": ["Membership review", "Protocol updates"]},
  "requires_decision": true
}
```

**Subscribe to recognition updates:**
```json
{
  "type": "subscription",
  "subscription_type": "recognition",
  "source_entity": "uuid-of-organization",
  "filters": {"min_percentage": 10},
  "notification_method": "webhook"
}
```
