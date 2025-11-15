# Political Sensitivity Audit - Quick Reference Guide

## Overview
This guide identifies politically sensitive terminology in documentation that may be problematic when presenting Free Association to world governments, UN agencies, and institutional partners.

## Quick Stats
- **Total files reviewed**: ~215 markdown files
- **Files with sensitive terms**: 64 files  
- **Total sensitive term occurrences**: ~800+

## Sensitive Term Categories & Recommendations

### 🔴 HIGH PRIORITY - Extremely Sensitive

#### 1. Communist/Marxist Terminology
**Current occurrences**: ~150+
- `communism`, `communist` → Replace with "collaborative coordination" or "peer-to-peer"
- `Marx`, `Marxist`, `Marxism` → Remove or cite as "historical economic theory"
- `bourgeois`, `proletariat` → Replace with "resource holders" and "workers"
- `class struggle`, `class war` → Replace with "resource distribution challenges"

**Files requiring immediate attention**:
- `research/draft/p2p/free-compute/free-computer.md` - Title uses "Communist Computer"
- `research/experiments/README copy.md` - Contains direct Marx quotes
- `research/marx/` - Entire directory

#### 2. Revolutionary Language  
**Current occurrences**: ~145+
- `revolution`, `revolutionary` → Replace with "transformation" or "innovation"
- `radical` → Replace with "fundamental" or "significant"
- `abolish`, `abolition` → Replace with "replace" or "transform"
- `overthrow` → Replace with "transition from"

#### 3. Capitalism Critiques
**Current occurrences**: ~231+
- `capitalism`, `capitalist` → Replace with "market economy" or "current economic system"
- Avoid direct criticism; frame as "alternative coordination mechanism"

### 🟡 MEDIUM PRIORITY - Politically Loaded

#### 4. Exploitation/Oppression Language
**Current occurrences**: ~80+
- `exploitation`, `exploitative` → Replace with "inefficient allocation" or "misaligned incentives"
- `oppression`, `oppressor` → Replace with "power asymmetries" or "unequal access"

#### 5. Private Property Critiques
**Current occurrences**: ~50+
- Critical usage of "private property" → Reframe as "property rights" or "ownership models"
- Focus on coordination benefits, not property critique

#### 6. Alienation (Marxist sense)
**Current occurrences**: ~70+
- `alienation`, `alienated` → Replace with "disconnected" or "lack of agency"

### 🟢 LOW PRIORITY - Review for Context

#### 7. Liberation/Resistance  
**Current occurrences**: ~40+
- `liberation` → Replace with "empowerment" or "enablement"
- `resistance` → Replace with "alternative approach"

#### 8. Wage Labor Critiques
**Current occurrences**: ~15
- `wage slavery`, `wage labor` → Replace with "employment" or "labor markets"

## Recommended Framing Alternatives

### Instead of Ideological Framing:
❌ "Free Association abolishes capitalism and wage slavery"
✅ "Free Association provides an alternative coordination mechanism"

❌ "This represents Marx's vision of communism"
✅ "This implements collaborative resource coordination"

❌ "Revolutionary transformation of property relations"
✅ "Innovative approach to resource sharing"

❌ "Liberating workers from exploitation"
✅ "Enabling more efficient resource allocation"

❌ "Radical restructuring of the economy"
✅ "Fundamental innovation in coordination infrastructure"

### Positive Framing Examples:
- "Digital public infrastructure for coordination"
- "Peer-to-peer resource allocation system"
- "Mutual recognition-based coordination"
- "Decentralized coordination protocol"
- "Efficient resource matching mechanism"
- "Collaborative capacity allocation"

## Priority Actions

### 1. Immediate Fixes (Before any governmental presentation):
- [ ] Rename `free-computer.md` title (remove "Communist")
- [ ] Review and sanitize `README.md`, `DPI.md`, `DPIv6.md`, `PROTOCOL.md`
- [ ] Hide or delete `research/marx/` directory
- [ ] Hide or delete `research/fragmentOnLove.md`, `research/personal.md`
- [ ] Hide or delete `research/situationists/` directory
- [ ] Hide or delete `research/psychoanalysis/` directory

### 2. Secondary Review:
- [ ] Review all files in `research/draft/cooperation/` - heavy Marx references
- [ ] Review `research/svo/history.md` - extensive Marx analysis
- [ ] Review `research/unconditional.md` - Marx references
- [ ] Sanitize `research/overview-article.md` and `research/paper.md`

### 3. Create "Public-Facing" Documentation Subset:
- Create a `docs-public/` directory with sanitized versions
- Include only: technical specs, use cases, mathematical proofs, coalition information
- Exclude: philosophical theory, political critique, academic research

## Search Commands

### Find all sensitive terms:
```bash
bash find-sensitive-terms.sh > report.txt
```

### List files requiring review:
```bash
grep -r -l -i --include="*.md" \
  -E "(communis|marxis|marx\b|revolution|radical|abolish|capitalis|exploit|oppress)" \
  research/ README.md DPI.md PROTOCOL.md | sort
```

### Search for specific term:
```bash
grep -r -i -n --include="*.md" "YOUR_TERM" research/ README.md DPI.md PROTOCOL.md
```

### Count occurrences of a term:
```bash
grep -r -i --include="*.md" "YOUR_TERM" research/ | wc -l
```

## Notes

- **Philosophy vs Practice**: Keep theoretical work in research/, create separate public-facing docs
- **Academic Context**: Some terms acceptable in academic papers but not governmental presentations  
- **Audience Sensitivity**: Different governments have different sensitivities (US vs China vs European)
- **Translation Issues**: Some terms may be more/less sensitive in other languages

## Recommended Documentation Structure

```
/
├── README.md                    [PUBLIC - sanitized]
├── DPI.md                       [PUBLIC - sanitized]  
├── PROTOCOL.md                  [PUBLIC - technical only]
├── docs-public/                 [NEW - public-facing docs]
│   ├── technical-specification.md
│   ├── use-cases.md
│   ├── coalition-information.md
│   └── mathematical-foundations.md
└── research/                    [PRIVATE - keep as-is]
    └── [all current research]
```

## Contact for Questions

If uncertain about specific terminology or framing, consult with:
- Communications team before governmental presentations
- Legal team for IP/trademark concerns
- Coalition partners for regional sensitivity

