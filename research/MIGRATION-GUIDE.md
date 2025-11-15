# Migration to Expression Protocol

This repository has migrated to the **Expression Protocol** structure.

## What Changed

### Old Structure (Complex)
```
research/
├── private/ (local)
├── draft/experiments/ (peer review)
├── multilateral/ (partnerships)
└── published/ (finalized)
```

### New Structure (Simple)
```
research/
├── institutions/[org-name]/
├── nations/[org-name]/
├── civil-society/[org-name]/
├── private-sector/[org-name]/
├── academic/[org-name]/
├── individual/[name]/
└── commons/
```

## Where Did Content Go?

- **Experiments** → `commons/experiments/`
- **Policy Frameworks** → `commons/policy-frameworks/`
- **DPIv6 Proposal** → `commons/proposals/`
- **Old Structure** → `archived-structures/`

## For Organization Representatives

To claim content from commons:

1. Create your org folder:
   ```bash
   ./scripts/create-org-expression.sh [form] [org-name]
   ```

2. Move content from commons to your folder:
   ```bash
   mv research/commons/[content] research/[form]/[org-name]/
   ```

3. Self-organize however you want!

## For Individual Researchers

Create your personal expression folder:
```bash
./scripts/create-org-expression.sh individual your-github-username
```

Then organize it however you like!

## Questions?

See [EXPRESSION-PROTOCOL.md](./EXPRESSION-PROTOCOL.md) for the full protocol.
