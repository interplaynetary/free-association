# Expression Protocol Implementation Summary

**Date**: November 15, 2025  
**Protocol Version**: 1.0

---

## The Constitutional Principle

> **Each organization has a folder in which it can express itself.**  
> **Organizations govern their own expressions and how they organize them.**

This is the **constitution of the repository** - simple, sovereign, self-governing.

---

## Structure

```
research/
├── institutions/          # UN, World Bank, IMF, etc.
│   ├── undp/             # UNDP governs this folder
│   ├── unep/             # UNEP governs this folder
│   └── world-bank/       # World Bank governs this folder
│
├── nations/               # National governments
│   ├── kenya/            # Kenya governs this folder
│   └── sweden/           # Sweden governs this folder
│
├── civil-society/         # NGOs, movements
│   ├── climate-action-network/
│   └── greenpeace/
│
├── private-sector/        # Companies, cooperatives
│   ├── platform-coop/
│   └── tech-commons/
│
├── academic/              # Universities, institutes
│   ├── mit/
│   └── oxford/
│
├── individual/            # Individual researchers
│   ├── alice/            # Alice governs this folder
│   └── bob/              # Bob governs this folder
│
└── commons/               # Shared resources
    └── [community governed]
```

---

## Access Control (The Key Innovation)

### Rule
```
IF github_account == org_name AND pr_target == org_folder:
    ACCEPT
ELSE:
    REJECT
```

### Examples

✅ **ALLOWED**:
- @alice makes PR to `research/individual/alice/paper.md`
- @undp makes PR to `research/institutions/undp/climate.md`
- @kenya makes PR to `research/nations/kenya/policy.md`

❌ **REJECTED**:
- @bob makes PR to `research/individual/alice/paper.md` (not their folder)
- @undp makes PR to `research/institutions/unep/doc.md` (not their folder)
- @alice makes PR to `research/nations/kenya/policy.md` (not their folder)

### Enforcement

1. **GitHub Webhooks** - Validate account matches folder
2. **Blockchain Controller** (TBD) - Immutable record + validation
3. **Automated Rejection** - Invalid PRs automatically rejected

---

## Freedom of Expression

Each organization decides:

- ✅ **Structure**: Folders, files, hierarchy - whatever works
- ✅ **Workflow**: Drafts, published, experiments - your choice
- ✅ **Standards**: Quality, review process - self-determined
- ✅ **Timeline**: When to publish - your schedule
- ✅ **Format**: Markdown, code, data - any format
- ✅ **Collaboration**: Internal processes - your governance

### Example: Three Different Approaches

**UNDP** (structured):
```
research/institutions/undp/
├── climate/
│   ├── framework.md
│   └── case-studies/
├── digital/
└── partnerships/
```

**Alice** (academic):
```
research/individual/alice/
├── drafts/
├── published/
├── experiments/
└── notes/
```

**Platform Coop** (agile):
```
research/private-sector/platform-coop/
├── working-docs/
├── decisions/
└── archive/
```

All valid! Each org self-governs.

---

## Cross-Organization Collaboration

### Method 1: Reference
Link to another org's work:
```markdown
See [UNDP Climate Framework](../institutions/undp/climate/framework.md)
```

### Method 2: Copy with Attribution
```markdown
# Adapted from UNDP Climate Framework
Original: research/institutions/undp/climate/framework.md
```

### Method 3: Coordinate via Issues
1. Open issue: "Would like to collaborate on X"
2. Discuss approach
3. One org incorporates the work

### Method 4: Guest Contribution
- Org invites external contributor
- External creates branch
- Org reviews and merges with their account
- Attribution in commit message

---

## Tools Available

### 1. Create Organization Expression
```bash
./scripts/create-org-expression.sh [form] [org-name]

# Examples:
./scripts/create-org-expression.sh institutions undp
./scripts/create-org-expression.sh individual alice
./scripts/create-org-expression.sh civil-society climate-action-network
```

### 2. Validate PR Permissions
```bash
./scripts/validate-org-pr.sh [github-account] [target-path]

# Examples:
./scripts/validate-org-pr.sh alice research/individual/alice/paper.md
# Output: ✓ VALID: PR from @alice is ALLOWED

./scripts/validate-org-pr.sh bob research/individual/alice/paper.md  
# Output: ✗ INVALID: PR from @bob is REJECTED
```

---

## Migration from Previous Structure

### Step 1: Identify Ownership
Determine which org owns which content

### Step 2: Create Org Folders
```bash
./scripts/create-org-expression.sh [form] [org-name]
```

### Step 3: Move Content
Organizations move their content to their folder and self-organize

### Step 4: Commons for Shared Resources
Content without clear ownership → `research/commons/`

### Step 5: Archive Old Structure
Previous structure preserved in `archived-structures/`

---

## Benefits

### Simplicity
- No complex workflow stages
- No prescribed structure
- No external governance
- Just: "This is your folder, organize it"

### Sovereignty
- Each org fully controls its expressions
- No one can PR to your folder except you
- You decide standards, timeline, structure
- Complete organizational autonomy

### Scalability
- Grows organically
- No central bottleneck
- Each org manages its own complexity
- System stays simple as it scales

### Security
- GitHub account + blockchain validation
- Cryptographic identity
- Automated enforcement
- Clear access control

### Flexibility
- Experiment with structure
- Change organization anytime
- No migration needed (it's your folder!)
- Adapt to your needs

---

## Comparison: Old vs New

### Old Model (Complex)
```
Private (local) → Experiments (peer review) → 
Multilateral → Published (finalized) → Archived (historical)

- 5 stages
- Prescribed workflow
- Central structure
- Move between stages
- Complex governance
```

### New Model (Simple)
```
research/[form]/[org-name]/[your expressions]

- 1 "stage" (your folder)
- Self-determined workflow
- Self-organized structure
- No moving files
- Self-governance
```

---

## Implementation Status

### ✅ Completed
- Expression protocol documented
- Scripts created and tested
- Example org folders created
- Access control logic defined

### 🔄 In Progress
- GitHub webhook integration
- Blockchain controller specification

### 📋 Next Steps
1. Migrate existing content to org folders
2. Deploy GitHub webhook
3. Implement blockchain controller
4. Onboard organizations

---

## Examples in Practice

### Example: UNDP Joins

1. UNDP creates GitHub account: `@undp`
2. Runs: `./scripts/create-org-expression.sh institutions undp`
3. Organizes folder however they want:
   ```
   research/institutions/undp/
   ├── climate-coordination/
   ├── sdg-digital-infra/
   └── partnerships/
   ```
4. Makes PRs from @undp account
5. Only their PRs to their folder are accepted
6. Full autonomy within their folder

### Example: Individual Researcher

1. Alice creates GitHub account: `@alice`
2. Runs: `./scripts/create-org-expression.sh individual alice`
3. Organizes academically:
   ```
   research/individual/alice/
   ├── papers/
   │   ├── published/
   │   └── drafts/
   ├── experiments/
   └── notes/
   ```
4. Works at her own pace
5. Publishes when ready
6. Collaborates by opening issues with other orgs

---

## Constitutional Principles

1. **Self-Governance**: Each org governs itself
2. **Sovereignty**: Your folder, your rules
3. **Access Control**: Cryptographic identity enforcement
4. **Expression Freedom**: Organize however you want
5. **Simplicity**: Minimal rules, maximum autonomy

---

**This is organizational self-determination in a git repository.**

Contact: info@openassociation.org  
Protocol: research/EXPRESSION-PROTOCOL.md
