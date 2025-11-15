# Research Expression Protocol

**Version**: 1.0  
**Date**: November 15, 2025  
**Status**: Active

---

## Constitutional Principle

Each organization has a folder in which it can express itself. Organizations govern their own expressions and how they organize them.

---

## Structure

```
research/
└── [organizational-form]/
    └── [org-name]/
        └── [self-organized expressions]
```

### Organizational Forms

Organizations are grouped by form for convenience:

- `institutions/` - UN agencies, World Bank, IMF, etc.
- `nations/` - National governments
- `civil-society/` - NGOs, movements, coalitions
- `private-sector/` - Corporations, cooperatives, startups
- `academic/` - Universities, research institutes
- `individual/` - Individual researchers and contributors

---

## Governance Rules

### 1. Self-Governance

Each organization:
- ✅ Governs its own expressions
- ✅ Organizes its folder structure however it wishes
- ✅ Decides what to publish and when
- ✅ Sets its own quality standards
- ✅ Manages its own workflow

### 2. Access Control

**GitHub Pull Request Model**:

```
IF (github_account == org_name) AND (PR_target == org_folder):
    ACCEPT pull_request
ELSE:
    REJECT pull_request
```

**Rules**:
- Organization name MUST match GitHub account name
- Pull requests MUST target the organization's own folder
- Cross-organization PRs are rejected
- Project controller validates on blockchain (TBD)

### 3. Key-Based Identity

- The GitHub account key acts as the organization's identity
- One key = one organizational account
- Account acts as interface for the organization
- Organizations share account access internally as they see fit

---

## Expression Freedom

Organizations are **invited to experiment** with:

- File organization and structure
- Documentation standards
- Workflow processes
- Publication criteria
- Collaboration methods
- Naming conventions
- Folder hierarchies

**There is no prescribed structure** - each org finds what works.

---

## Convenience Tools

Scripts are provided to automate common tasks:

```bash
# Generate org folder structure
./scripts/create-org-expression.sh [form] [org-name]

# Validate PR permissions
./scripts/validate-org-pr.sh [github-account] [target-folder]

# Organize expression data
./scripts/organize-expression.sh [org-folder]
```

Scripts are **optional** - organizations can manage manually if preferred.

---

## Cross-Organization Collaboration

For collaboration between organizations:

1. **Reference**: Link to other org expressions
2. **Copy**: Copy with attribution to your org folder
3. **Coordinate**: Use issues/discussions for cross-org dialogue
4. **Coauthor**: Multiple orgs can coauthor in one org's folder with permission

**Note**: Direct PR to another org's folder is not allowed - preserves sovereignty.

---

## Examples

### Example 1: UNDP Expression

```
research/institutions/undp/
├── climate-coordination/
│   ├── framework.md
│   └── case-studies/
├── digital-infrastructure/
└── README.md
```

UNDP organizes by project. They decide structure.

### Example 2: Individual Researcher

```
research/individual/alice/
├── experiments/
├── papers/
├── notes/
└── published/
```

Alice uses traditional academic structure. Her choice.

### Example 3: Cooperative

```
research/private-sector/platform-coop/
├── governance/
├── tech-stack/
├── business-model/
└── lessons-learned/
```

Platform cooperative organizes by concern. Self-determined.

---

## Migration from Previous Structure

Existing content can be migrated by:

1. Identifying organizational ownership
2. Moving to appropriate org folder
3. Organizations then self-organize

Community content without clear ownership:
- `research/commons/` - Shared commons resources
- Organizations can reference or fork

---

## Benefits

### For Organizations
- **Autonomy**: Full control over expressions
- **Simplicity**: No complex workflow to learn
- **Flexibility**: Organize as you see fit
- **Security**: GitHub + blockchain access control
- **Sovereignty**: No external governance

### For the Repository
- **Clear ownership**: Every expression has an org
- **Scalability**: Grows organically
- **Decentralized**: No central control
- **Resilient**: Org-level governance
- **Transparent**: GitHub history + blockchain validation

---

## Implementation

### Phase 1: Core Structure
- Create organizational form directories
- Set up GitHub webhook integration
- Deploy blockchain controller (TBD)

### Phase 2: Organization Onboarding
- Invite organizations to create folders
- Provide convenience scripts
- Document access control

### Phase 3: Migration
- Move existing content to org folders
- Establish commons for shared resources
- Archive previous structure

---

## Technical Specification

### GitHub Integration

**Webhook Handler**:
```javascript
function validatePR(pr) {
  const account = pr.author.github_username;
  const target = pr.target_path;
  const org_folder = extractOrgFolder(target);
  const org_name = extractOrgName(org_folder);
  
  return account === org_name;
}
```

### Blockchain Controller

**Smart Contract** (TBD):
- Registers org names and keys
- Validates PR authenticity
- Records expression history
- Enables dispute resolution

---

## Principles

1. **Organizational Sovereignty**: Each org governs itself
2. **Expression Freedom**: No prescribed structure
3. **Access Control**: Cryptographic identity
4. **Transparency**: Public expressions
5. **Simplicity**: Minimal rules, maximum freedom

---

## Questions & Answers

**Q: What if I want to contribute to another org?**  
A: Open an issue, discussion, or coordinate offline. They can incorporate your work if they choose.

**Q: Can individuals participate?**  
A: Yes! `research/individual/[your-name]/` - you are an org of one.

**Q: What about drafts vs published?**  
A: Each org decides. Some may use folders like `drafts/` and `published/`, others may not distinguish.

**Q: How do we collaborate?**  
A: Reference each other's work, coordinate via issues/discussions, or one org can invite another to contribute to their folder.

**Q: What's the blockchain for?**  
A: Validates that PRs come from legitimate org accounts, provides immutable record, enables future governance features.

---

**This is the constitution of the repository: organizational expression with self-governance.**

Contact: info@openassociation.org  
Repository: https://github.com/your-org/free-association

