# Organization-Specific Routes

Dynamic routes for pre-populated trees tailored to specific organizations.

## Usage

Access organization-specific interfaces via:

```
/org/{slug}
```

## Available Organizations

### UNICEF
**URL:** `/org/unicef`

**Focus:** Children's Rights and Wellbeing

**Tree Structure:**
- Child Health & Nutrition (35 points)
  - Immunization Programs
  - Nutrition & Food Security
  - Maternal & Newborn Care
- Education & Learning (30 points)
  - Access to Quality Education
  - Girls' Education
  - Education in Emergencies
- Child Protection (25 points)
  - Protection from Violence
  - End Child Exploitation
  - Child-Friendly Justice
- Water, Sanitation & Hygiene (10 points)
  - Safe Water Access
  - Sanitation Facilities

### World Bank
**URL:** `/org/world-bank`

**Focus:** Economic Growth and Poverty Reduction

**Tree Structure:**
- End Extreme Poverty (40 points)
  - Income Support Programs
  - Social Protection Systems
  - Job Creation & Livelihoods
- Promote Shared Prosperity (30 points)
  - Inclusive Economic Growth
  - Reduce Inequality
  - Equal Opportunity Access
- Infrastructure Development (20 points)
  - Transportation Networks
  - Energy Infrastructure
  - Digital Connectivity
- Climate Action & Sustainability (10 points)
  - Climate Adaptation
  - Green Development

### Red Cross / Red Crescent
**URL:** `/org/red-cross`

**Focus:** Humanitarian Response and Relief

**Tree Structure:**
- Emergency Response (35 points)
  - Disaster Relief
  - Conflict Zone Support
  - Search & Rescue
- Health Services (30 points)
  - First Aid & Primary Care
  - Blood Donation Services
  - Epidemic Preparedness
- Protection & IHL (20 points)
  - International Humanitarian Law
  - Restoring Family Links
  - Support for Detainees
- Community Resilience (15 points)
  - Disaster Preparedness
  - Community Training

## Implementation Details

### Configuration
Organization trees are defined in `src/lib/config/org-trees.json`

Each entry contains:
- `slug`: URL identifier
- `name`: Display name
- `description`: Organization description
- `tree`: Complete RootNode tree structure

### Technical Notes

1. **Client-Side Only**: Routes use CSR (Client-Side Rendering) like the main app
2. **Demo Trees**: All org trees are stored locally (unauthenticated mode)
3. **Force Initialization**: Org routes override existing demo trees to ensure correct tree loads
4. **LocalStorage**: Trees persist in browser localStorage for continuity
5. **Static Build**: Routes work via SPA fallback (index.html) with client-side routing

### Adding New Organizations

1. Edit `src/lib/config/org-trees.json`
2. Add new organization entry with complete tree structure
3. Rebuild the application: `bun run build`
4. New route automatically available at `/org/{new-slug}`

### Tree Structure Requirements

Each tree must conform to `RootNodeSchema`:
- `type: "RootNode"`
- `id`: Unique identifier
- `name`: Display name
- `children`: Array of NonRootNode objects
- `created_at`, `updated_at`: ISO timestamps
- `manual_fulfillment`: null or number

Example:
```json
{
  "slug": "example-org",
  "name": "Example Organization",
  "description": "Description of the organization's mission",
  "tree": {
    "id": "example_root",
    "name": "Example Organization Priorities",
    "type": "RootNode",
    "manual_fulfillment": null,
    "created_at": "2025-01-01T00:00:00Z",
    "updated_at": "2025-01-01T00:00:00Z",
    "children": [
      {
        "id": "example_child",
        "name": "Example Goal",
        "type": "NonRootNode",
        "points": 100,
        "parent_id": "example_root",
        "manual_fulfillment": null,
        "contributors": [],
        "anti_contributors": [],
        "children": []
      }
    ]
  }
}
```

## Use Cases

1. **Tailored Pitches**: Send organization-specific links showing relevant priorities
2. **Partnership Demos**: Demonstrate the platform with familiar terminology
3. **Stakeholder Engagement**: Pre-populate with existing organizational goals
4. **Onboarding**: Provide context-aware starting points for new users

