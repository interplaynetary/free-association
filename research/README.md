# Free Association Research

Welcome to the Free Association research repository.

## Structure: Expression Protocol

This repository follows the **Expression Protocol** - a simple, constitutional approach to research organization.

### The Principle

> **Each organization has a folder in which it can express itself.**  
> **Organizations govern their own expressions and how they organize them.**

### Directory Structure

```
research/
├── institutions/          # UN agencies, World Bank, IMF, etc.
│   └── [org-name]/       # Each org governs its own folder
│
├── nations/               # National governments  
│   └── [org-name]/       # Each nation governs its own folder
│
├── civil-society/         # NGOs, movements, coalitions
│   └── [org-name]/       # Each org governs its own folder
│
├── private-sector/        # Corporations, cooperatives, startups
│   └── [org-name]/       # Each org governs its own folder
│
├── academic/              # Universities, research institutes
│   └── [org-name]/       # Each org governs its own folder
│
├── individual/            # Individual researchers
│   └── [name]/           # Each person governs their own folder
│
└── commons/               # Shared community resources
    ├── experiments/       # Community experimental research
    ├── proposals/         # Major proposals (like DPIv6)
    ├── policy-frameworks/ # Shared policy frameworks
    └── templates/         # Reusable templates
```

---

## Access Control

**The Rule**:
```
IF github_account == org_name AND pr_target == org_folder:
    ACCEPT pull_request
ELSE:
    REJECT pull_request
```

**Examples**:
- ✅ @undp can PR to `research/institutions/undp/`
- ✅ @alice can PR to `research/individual/alice/`
- ❌ @bob cannot PR to `research/individual/alice/`
- ❌ @undp cannot PR to `research/institutions/unep/`

---

## For Organizations

### Joining the Repository

1. **Create your folder**:
   ```bash
   ./scripts/create-org-expression.sh [form] [your-org-name]
   ```

2. **Organize your content**:
   - Structure it however you want
   - No prescribed workflow
   - Complete autonomy

3. **Make pull requests**:
   - Use your organization's GitHub account
   - PR to your own folder only
   - Self-govern your expressions

### Expression Freedom

Each organization decides:
- ✅ Folder structure and organization
- ✅ File formats and standards
- ✅ Review and publication process
- ✅ Collaboration methods
- ✅ Quality criteria
- ✅ Workflow and timeline

---

## For Individual Researchers

You are an organization of one!

1. **Create your space**:
   ```bash
   ./scripts/create-org-expression.sh individual your-github-username
   ```

2. **Organize as you like**:
   ```
   research/individual/your-name/
   ├── papers/
   │   ├── published/
   │   └── drafts/
   ├── experiments/
   └── notes/
   ```

3. **Work at your pace**:
   - No external governance
   - Publish when ready
   - Self-determined standards

---

## Commons

The `commons/` directory contains shared community resources:

- **experiments/** - Community experimental research (96 files)
- **proposals/** - Major proposals like DPIv6
- **policy-frameworks/** - Shared policy documents
- **templates/** - Reusable templates

### Using Commons Resources

Organizations can:
1. **Reference** - Link to commons resources
2. **Fork** - Copy to your org folder with attribution
3. **Contribute** - Submit improvements via PR
4. **Claim** - Move content to your org folder if you own it

### Contributing to Commons

1. Open an issue to discuss
2. Submit PR with community review
3. Consensus-based acceptance

---

## Cross-Organization Collaboration

Since you can only PR to your own folder, here's how to collaborate:

### Method 1: Reference
```markdown
See [UNDP Climate Framework](../institutions/undp/climate/framework.md)
```

### Method 2: Copy with Attribution
```markdown
# Adapted from UNDP Climate Framework
Original: research/institutions/undp/climate/framework.md
License: CC BY 4.0
```

### Method 3: Coordinate via Issues
1. Open issue: "Collaboration on X"
2. Discuss approach
3. One org incorporates the work

### Method 4: Guest Contribution
- Org invites external contributor
- External creates branch
- Org reviews and merges with their account
- Attribution in commit message

---

## Tools & Scripts

### Create Organization Folder
```bash
./scripts/create-org-expression.sh [form] [org-name]

# Forms: institutions, nations, civil-society, private-sector, academic, individual
```

### Validate PR Permissions
```bash
./scripts/validate-org-pr.sh [github-account] [target-path]
```

### Migration
```bash
./scripts/migrate-to-expression-protocol.sh
```

---

## Documentation

- **[EXPRESSION-PROTOCOL.md](./EXPRESSION-PROTOCOL.md)** - Full protocol specification
- **[MIGRATION-GUIDE.md](./MIGRATION-GUIDE.md)** - Migration from old structure
- **[commons/README.md](./commons/README.md)** - Commons governance

---

## Benefits

### Simplicity
- No complex workflow stages
- No prescribed structure
- Just: "This is your folder, organize it"

### Sovereignty
- Each org fully controls its expressions
- No external governance
- Complete organizational autonomy

### Scalability
- Grows organically
- No central bottleneck
- Each org manages its own complexity

### Security
- GitHub account validation
- Blockchain controller (coming)
- Clear access control

---

## Principles

1. **Self-Governance** - Each org governs itself
2. **Sovereignty** - Your folder, your rules
3. **Access Control** - Cryptographic identity enforcement
4. **Expression Freedom** - Organize however you want
5. **Simplicity** - Minimal rules, maximum autonomy

---

## Getting Started

### As an Organization
1. `./scripts/create-org-expression.sh [form] [org-name]`
2. Organize your folder however you like
3. Make PRs from your GitHub account

### As an Individual
1. `./scripts/create-org-expression.sh individual your-username`
2. Structure your personal research space
3. Work at your own pace

### Claiming Commons Content
1. Review `commons/` for relevant content
2. Move to your org folder: `mv commons/[content] [your-org-folder]/`
3. Self-organize from there

---

**Contact**: info@openassociation.org  
**Repository**: Free Association  
**Protocol**: Expression Protocol v1.0

---

*This is organizational self-determination in a git repository.*
