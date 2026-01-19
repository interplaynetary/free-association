# Resources

Resources are the **capacities** and **needs** that participants declare to the network.

## Available Capacity

**What resources can you offer?**

Declare surplus resources available for allocation.

### Resource Types
- **Funds** - Financial resources
- **Expertise** - Skills, knowledge, consulting
- **Facilities** - Physical spaces, equipment
- **Time** - Volunteer hours, staff time
- **Equipment** - Tools, machinery, vehicles

### Constraints

Capacities can be filtered by:

**Time Windows:**
```
Available: Next 6 months
Available: January-March 2026
Available: Ongoing
```

**Geographic Locations:**
```
Location: Global
Location: East Africa
Location: Remote only
```

**Resource Type Specifications:**
```
Type: Healthcare
Type: Education
Type: Emergency Response
```

### Example

```
Foundation X declares:
- $500K/month operational funding
- Available: Next 6 months
- Filter: Healthcare and education sectors
- Location: Any
```

---

## Declared Needs

**What resources do you require?**

State specific resource requirements.

### Properties

- **Real-time updates**: Needs evolve as circumstances change
- **Caps allocations**: System never allocates more than declared need
- **Prevents accumulation**: Resources flow to where they're needed
- **Enables matching**: Precise compatibility checking

### Example

```
Organization B declares:
- $200K/month operational funding
- $100K emergency medical supplies
- 40 hours/week technical expertise
```

---

## Resource Matching

The system matches capacities to needs based on:

1. **Type compatibility**: Does the capacity match the need type?
2. **Time compatibility**: Do the time windows overlap?
3. **Location compatibility**: Are the locations compatible?
4. **Priority alignment**: Does the provider prioritize this recipient?

**Key Property**: Matching is **automatic** and **continuous**. As network state changes, matches are recomputed in real-time (~100-200ms).

---

## Schema Structure

Resources are published as Verifiable Credentials:

```json
{
  "issuer": "did:key:z6Mk...",
  "credentialSubject": {
    "capacities": [
      {
        "type": "funding",
        "amount": 500000,
        "unit": "USD/month",
        "timeWindow": "2026-01-01/2026-06-30",
        "location": "global",
        "filters": ["healthcare", "education"]
      }
    ],
    "needs": [
      {
        "type": "funding",
        "amount": 200000,
        "unit": "USD/month",
        "timeWindow": "2026-01-01/2026-12-31"
      }
    ]
  }
}
```

---

## Further Reading

- [Priorities](priorities.md) - How recognition determines allocation
- [Allocation](../2-derivation/allocation.md) - How the matching algorithm works
