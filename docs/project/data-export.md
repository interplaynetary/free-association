# Data Export

## Overview

Free Association allows you to export all your personal data in a standard, non-proprietary JSON format. This ensures you always have access to your data and can transfer it between systems or keep backups.

---

## What Data Can Be Exported?

When you export your data, you receive a comprehensive JSON file containing:

### Core Data
- **Recognition Data**: Your recognition allocations and relationships
- **Capacity Information**: Resources you've declared as available to share
- **Need Information**: Resources you've declared as needs
- **Commitment Data**: Your coordination commitments and metadata

### Network Configuration
- **Subscriptions**: Entities and resources you track
- **Filters**: Criteria you've set for resource matching
- **Organizational Data**: Organizations you're affiliated with

### Social Data
- **Contacts**: Your contact list and associated data

---

## How to Export Your Data

### Method 1: Via User Interface (Recommended)

1. **Log in** to your Free Association account
2. Click on your **username/avatar** in the top navigation bar
3. Click the **Export Data button** (💾 icon)
4. Your data will be:
   - Copied to your clipboard
   - Automatically downloaded as a JSON file named `free-association-export-[timestamp].json`

### Method 2: Via Browser Developer Tools (Advanced)

If you prefer to export programmatically:

```javascript
// Open browser console (F12)
import { exportUserStateAsJSON } from '$lib/utils/data/userStateExport';

// Export data
const jsonData = exportUserStateAsJSON(true);

// Download as file
const blob = new Blob([jsonData], { type: 'application/json' });
const url = URL.createObjectURL(blob);
const link = document.createElement('a');
link.href = url;
link.download = `my-export-${Date.now()}.json`;
link.click();
```

---

## Data Format

The exported data follows a non-proprietary JSON schema documented in our [Protocol Specification](../technical/protocol.md).

### Export Structure

```json
{
  "version": "3.0.0",
  "exported_at": "2025-11-15T12:00:00.000Z",
  "data": {
    "recognition": { ... },
    "capacity": [ ... ],
    "needs": [ ... ],
    "commitment": { ... },
    "subscriptions": { ... },
    "filters": { ... },
    "organizations": { ... },
    "contacts": { ... }
  }
}
```

### Data Types
- **JSON Format**: Standard, human-readable JSON
- **No Proprietary Encoding**: All data is in open formats
- **No Binary Data**: Text-based for maximum portability
- **Schema-Validated**: Follows Zod schema specifications

---

## Importing Your Data

You can also import previously exported data:

1. Click on your **username/avatar** in the top navigation
2. Click the **Import Data button** (📥 icon)
3. Paste your JSON data into the text field
4. Click **Import Data**
5. The page will reload with your imported data

**⚠️ Warning**: Importing will overwrite your current data. Make sure to export first if you want to keep a backup.

---

## Privacy & Security

### What's Included
- ✅ All data you've entered into Free Association
- ✅ Your recognition allocations
- ✅ Your network configuration
- ✅ Your contacts and subscriptions

### What's NOT Included
- ❌ Your password (encrypted separately, cannot be exported)
- ❌ Your private keys (stored separately)
- ❌ Other users' data
- ❌ Network-wide allocation computations

### Security Notes
- Export files contain your complete recognition data
- Store export files securely
- Do not share export files publicly unless you understand the implications
- Exported data is plain text - anyone who accesses the file can read it

---

## Use Cases

### 1. Backup Your Data
Regularly export your data to keep personal backups:
```bash
# Recommended: Weekly exports
free-association-export-2025-11-15.json
free-association-export-2025-11-08.json
free-association-export-2025-11-01.json
```

### 2. Transfer Between Accounts
- Export from Account A
- Import to Account B
- Maintains all your recognition relationships

### 3. Data Portability
- Export from Free Association
- Use data in other compatible systems
- Full control over your coordination data

### 4. Development & Testing
- Export production data
- Import to development environment
- Test new features safely

### 5. Data Analysis
- Export to JSON
- Process with data analysis tools
- Generate custom reports and visualizations

---

## Technical Details

### Export Function

Location: `src/lib/utils/data/userStateExport.ts`

```typescript
export function exportUserState(): UserStateExport;
export function exportUserStateAsJSON(pretty: boolean = true): string;
```

### Schema Validation

All exported data is validated against Zod schemas defined in:
- `src/lib/protocol/schemas.ts`

### File Size

Typical export file sizes:
- **Small account** (10 recognition entries): ~5-10 KB
- **Medium account** (100 recognition entries): ~50-100 KB
- **Large account** (1000+ recognition entries): ~500 KB - 1 MB

JSON format is human-readable but not compressed. For long-term storage, consider compressing the JSON file:

```bash
# Compress export file
gzip free-association-export-*.json

# Decompress for import
gunzip free-association-export-*.json.gz
```

---

## Frequently Asked Questions

### Q: How often should I export my data?
**A**: We recommend exporting:
- After major changes to your recognition tree
- Before importing new data
- At regular intervals (weekly/monthly) for backup

### Q: Can I edit the exported JSON?
**A**: Yes, but:
- Maintain valid JSON syntax
- Follow the schema structure
- Validate before importing
- Risk of data corruption if edited incorrectly

### Q: What if my export fails?
**A**: If export fails:
1. Check browser console for errors
2. Try refreshing the page
3. Ensure you're logged in
4. Contact support: info@openassociation.org

### Q: Can I export only part of my data?
**A**: Currently, exports include all data. If you need selective export:
1. Export full data
2. Use JSON tools to extract specific sections
3. Or contact us about adding selective export features

### Q: Is exported data encrypted?
**A**: No, export files are plain JSON. If you need encryption:
```bash
# Encrypt export file (requires GPG)
gpg -c free-association-export-*.json

# Decrypt for import
gpg free-association-export-*.json.gpg
```

### Q: What format version should I use?
**A**: Current version: `3.0.0` (v5 protocol)
- Exports include version number
- Import validates version compatibility
- Legacy formats supported where possible

---

## Related Documentation

- **[Privacy Policy](../../src/routes/privacy/+page.svelte)**: How we handle your data
- **[Protocol Specification](../technical/protocol.md)**: Technical data format details
- **[Data Import Guide](./data-import.md)**: How to import data (coming soon)
- **[User Rights](../../src/routes/privacy/+page.svelte#your-privacy-rights)**: Your data rights under GDPR/CCPA

---

## Support

**Questions about data export?**
- Email: info@openassociation.org
- GitHub Issues: [Report technical issues](https://github.com/interplaynetary/free-association/issues)
- Documentation: This page and related docs

---

**Last Updated**: November 15, 2025  
**Version**: 3.0.0 (v5 protocol)

