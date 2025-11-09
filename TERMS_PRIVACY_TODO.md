# Terms & Privacy Policy - Action Checklist

## ✅ Completed
- [x] Updated Terms of Service to reflect hybrid architecture
- [x] Updated Privacy Policy to be GDPR compliant
- [x] Added clear warnings about public permanent P2P data
- [x] Added liability protections for P2P network
- [x] Separated centralized vs P2P data responsibilities

## 🔧 Required Before Going Live

### 1. Fill in Placeholders

**In both files**, replace these placeholders:

#### Terms of Service (`src/routes/terms/+page.svelte`)
- [ ] `[YOUR_JURISDICTION]` → e.g., "California, United States"
- [ ] `[SUPPORT_EMAIL]` → e.g., "support@playnet.lol"

#### Privacy Policy (`src/routes/privacy/+page.svelte`)
- [ ] `[LEGAL_ENTITY_NAME]` → Your legal name or entity name
- [ ] `[PRIVACY_CONTACT_EMAIL]` → e.g., "privacy@playnet.lol"
- [ ] `[SERVER_LOCATION/JURISDICTION]` → e.g., "United States"

### 2. Set Up Privacy Request Handling

- [ ] Create email address for privacy requests (privacy@playnet.lol)
- [ ] Set up email monitoring
- [ ] Create procedure to respond within 30 days (GDPR requirement)
- [ ] Document how you'll handle:
  - Data access requests (send user their account data)
  - Account deletion requests (delete from your servers)
  - Data correction requests (update account info)

### 3. Update User Registration Flow

- [ ] Add link to Terms of Service on registration page
- [ ] Add link to Privacy Policy on registration page
- [ ] Add checkbox: "I have read and agree to the Terms of Service and Privacy Policy"
- [x] Consider adding second checkbox: "I understand that all data I share will be public and permanent"
- [x] Make registration conditional on accepting terms

### 4. Add Footer Links

- [ ] Add Terms link to site footer
- [ ] Add Privacy link to site footer
- [ ] Make them easily accessible from all pages

## 💡 Recommended (Not Required)

### Legal Review
- [ ] Have attorney review terms/privacy (cost: $500-2000)
- [ ] Get specific GDPR advice if serving EU users
- [ ] Verify liability limitations are enforceable in your jurisdiction

### Additional Features
- [ ] Add "Data Export" feature (GDPR right to portability)
  - Let users download their account data
  - JSON format is fine
  
- [ ] Add "Delete Account" feature in user settings
  - Delete account from your servers
  - Clear warning: "P2P data will remain"
  
- [ ] Keep versioned copies of terms/privacy
  - Date each version
  - Show users when terms changed
  
- [ ] Add Terms acceptance tracking
  - Store when user accepted terms
  - Track version they accepted
  - Useful for proving consent

### Documentation
- [ ] Document your data handling procedures internally
- [ ] Create incident response plan for data breaches
- [ ] Document backup/retention policies
- [ ] Create data processing agreement templates if needed

## 🚀 Quick Start Commands

### Find and replace placeholders:

```bash
# Edit terms
code src/routes/terms/+page.svelte

# Edit privacy
code src/routes/privacy/+page.svelte

# Search for placeholders
grep -n "\[.*\]" src/routes/terms/+page.svelte
grep -n "\[.*\]" src/routes/privacy/+page.svelte
```

### Test the pages:

```bash
# Start dev server
bun run dev

# Visit:
# http://localhost:5173/terms
# http://localhost:5173/privacy
```

## 📋 Sample Replacements

### If you're an individual in the US:

**Terms (`+page.svelte` line ~211-212):**
```javascript
Governing Law
These terms are governed by the laws of California, United States without regard to conflict of law provisions.

Dispute Resolution
Disputes will be resolved through:
1. Good faith negotiation
2. Mediation (if negotiation fails)
3. Binding arbitration or courts in California, United States
```

**Terms (line ~228):**
```javascript
- Email: support@playnet.lol
```

**Privacy (line ~24-30):**
```javascript
**Service Name:** Free-Association  
**Operator:** [Your Full Legal Name]  
**Contact:** privacy@playnet.lol  
**Website:** https://playnet.lol  
**Data Controller:** [Your Full Legal Name] for centralized account data
```

**Privacy (line ~254):**
```javascript
**Where Data Is Stored:**
- **Centralized Account Data**: United States
- **P2P Network Data**: Distributed globally across all peer devices
```

**Privacy (line ~305):**
```javascript
**Email:** privacy@playnet.lol
```

## 🎯 Why This Matters

### Before (Old Terms)
- ❌ Legally contradicted your implementation
- ❌ High risk of GDPR violations (€20M fines)
- ❌ No protection from P2P data liability
- ❌ False claims could be seen as fraud/misrepresentation

### After (New Terms)
- ✅ Accurately describes your service
- ✅ GDPR compliant for what you control
- ✅ Protected from P2P data liability
- ✅ Honest with users = builds trust
- ✅ Much lower legal risk

## 📞 Need Help?

**Technical Issues:**
- Check: UPDATED_TERMS_PRIVACY_SUMMARY.md
- Review: TERMS_PRIVACY_INCONSISTENCIES.md

**Legal Questions:**
- Consult with attorney in your jurisdiction
- For EU: Lawyer familiar with GDPR
- For California: Lawyer familiar with CCPA

**Quick Questions:**
- GitHub Issues: https://github.com/playnet-lol/free-association
- Community: Telegram group (see README.md)

---

**Status:** ✅ Documents updated and ready  
**Next:** Fill in placeholders and deploy  
**Timeline:** Can go live today after placeholders filled  
**Risk Level:** Much lower than before 🎉

