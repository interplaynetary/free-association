<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const privacyContent = `# Privacy Policy
## Free-Association: Hybrid P2P/Centralized Service (Beta)

**Last Updated:** 2025-11-15  
**Effective Date:** 2025-11-15

---

## 📋 Overview

Free-Association operates as a **hybrid service** with both centralized and peer-to-peer components. This means:
- We operate servers that process limited account data (we ARE a data controller)
- Data you share in recognition trees enters a P2P network and becomes PUBLIC
- We CANNOT delete or control data once it enters the P2P network

This policy describes what data we collect, how we use it, and the limitations of privacy in a hybrid P2P system.

---

## 🏢 Who We Are (Data Controller Information)

**Service Name:** Free-Association  
**Operator:** Open Association  
**Contact:** info@openassociation.org  
**Website:** https://openassociation.org  
**Data Controller:** Open Association for centralized account data

**Our Role Under Privacy Laws:**
- ✅ **Data Controller**: We determine purposes and means of processing account data
- ✅ **Service Provider**: We operate servers and provide centralized services
- ❌ **NOT a Controller of P2P Data**: Once data enters the P2P network, we have no control over it

---

## 📊 What Data We Collect & Process

### 1. Account Data (Centralized - We Control)

**Data We Collect:**
- **Username**: Your chosen display name (public identifier)
- **Email Address**: Stored encrypted, used for validation and password reset
- **Password**: Stored as a cryptographic hash (we never see your plain password)
- **Public Key**: Your cryptographic public key (public identifier)
- **Encrypted Public Key (epub)**: Used for encrypted communications
- **Invite Code**: Code used to create your account (relationship tracking)
- **Account Creation Date**: When you joined
- **Feed Subscription Data**: Number and types of RSS feeds you subscribe to
- **Validation Codes**: Temporary codes for email validation (deleted after use)

**Legal Basis (GDPR):** Legitimate interest and contract performance

**Storage Location:** Our servers (Gun/Mesh relay servers)

**Retention:** Until you request account deletion or service termination

**Who Has Access:** 
- Our system administrators (encrypted data)
- You (via account access)

### 2. Service Usage Data (Centralized)

**Data We May Collect:**
- **API Request Logs**: Authentication attempts, rate limiting data (retained 30 days)
- **Error Logs**: Technical errors for debugging (retained 30 days)
- **Health Monitoring**: Service availability metrics (no personal identifiers)

**Legal Basis (GDPR):** Legitimate interest (security and service operation)

### 3. P2P Network Data (Decentralized - We CANNOT Control)

**Data That Enters P2P Network:**
- **Recognition Trees**: Your declarations of who contributes to your self-actualization
- **Capacity Declarations**: Resources you declare as available
- **Allocation Data**: Calculated allocations and mutual recognition
- **Public Messages/Interactions**: Any content you share
- **Timestamps**: When data was created/modified
- **Cryptographic Signatures**: Proof of authorship

**⚠️ CRITICAL PRIVACY WARNINGS:**

**Once data enters the P2P network:**
1. ❌ **We CANNOT delete it** - Data replicates to other peers
2. ❌ **We CANNOT modify it** - P2P protocol design prevents modification
3. ❌ **We CANNOT control who sees it** - All P2P data is PUBLIC
4. ❌ **We CANNOT prevent copies** - May be stored on thousands of devices
5. ❌ **We CANNOT guarantee deletion** - Even if you request account deletion

**Legal Basis:** Consent (by sharing data, you consent to public distribution)

**Storage Location:** Distributed across all peer devices (worldwide)

**Retention:** PERMANENT (may persist indefinitely, beyond our control)

**Who Has Access:** EVERYONE (all P2P data is public and unencrypted)

---

## 🔒 How We Use Your Data

### Account Data (Centralized)
**We use your account data to:**
- ✅ Authenticate you to the service
- ✅ Send account validation emails
- ✅ Process password reset requests
- ✅ Manage invite code system
- ✅ Track feed subscription limits
- ✅ Provide core service functionality

**We do NOT:**
- ❌ Sell your data to third parties
- ❌ Use your data for advertising
- ❌ Share your email with others (stored encrypted)
- ❌ Track your behavior for profiling
- ❌ Use your data beyond service operation

### P2P Network Data
**We do NOT control P2P data usage because:**
- Data is stored on peer devices (not our servers)
- Peers operate independently
- Protocol is decentralized by design
- We have no technical ability to control P2P data

---

## 🌐 Data Sharing & Third Parties

### Who We Share Data With

**Account Data (Centralized):**
- ❌ **We do NOT share** your email or encrypted account data with third parties

**Optional Service Integrations:**
- **OpenRouter (AI Service)**: Only if you use AI features, we proxy requests (no personal identifiers sent)
- **SMTP Service**: For sending validation/password reset emails (standard email delivery)

**Legal Requirements:**
- ✅ We may disclose data if required by law (court orders, subpoenas)

**P2P Network Data:**
- ⚠️ **ALL P2P data is PUBLIC** - Shared with every peer in the network
- ⚠️ **We have no control** over how peers use public P2P data

---

## 🔐 Data Security

### Security Measures We Implement

**For Centralized Account Data:**
- ✅ Encrypted storage of email addresses
- ✅ Password hashing (bcrypt/scrypt)
- ✅ TLS/SSL encryption for data in transit
- ✅ Authentication and access controls
- ✅ Regular security updates

**Limitations:**
- ❌ **No system is 100% secure** - Breaches are possible
- ❌ **P2P data is unencrypted** - Public by design
- ❌ **We cannot secure P2P data** - It's public across thousands of devices

---

## 📝 Your Rights Under Privacy Laws (GDPR/CCPA)

### Rights for CENTRALIZED Account Data ONLY

**You have the right to:**

1. ✅ **Access**: Request a copy of your account data we store
2. ✅ **Rectification**: Correct inaccurate account information
3. ✅ **Deletion**: Request deletion of your account from our servers
4. ✅ **Data Portability**: Export your account data in structured format
5. ✅ **Withdraw Consent**: Stop using the service and delete your account
6. ✅ **Object**: Object to processing for legitimate interests
7. ✅ **Lodge Complaint**: File complaint with supervisory authority

**How to Exercise Rights:**
- Email: info@openassociation.org
- Subject: "Privacy Rights Request"
- We will respond within 30 days (GDPR requirement)

### Rights for P2P NETWORK Data - CRITICAL LIMITATIONS

**⚠️ WE CANNOT fulfill these rights for P2P data:**

1. ❌ **Right to Deletion**: We CANNOT delete data from P2P network
2. ❌ **Right to Rectification**: We CANNOT modify data in P2P network
3. ❌ **Right to Restrict Processing**: We CANNOT control peer processing
4. ❌ **Right to Object**: We CANNOT stop peers from storing public data

**Why?**
- P2P data is distributed across thousands of independent peer devices
- No central authority controls the P2P network
- Data is replicated and stored independently by each peer
- Protocol design prevents centralized deletion or modification

**What This Means:**
- Once data enters P2P network, it's PERMANENT
- GDPR "right to be forgotten" CANNOT apply to P2P data
- You must treat all P2P data as PUBLIC and PERMANENT before sharing

---

## ⚖️ Applicable Laws & Legal Compliance

### Laws We Comply With

**Data Protection & Privacy Laws:**
- ✅ **General Data Protection Regulation (GDPR)** - European Union/EEA users
- ✅ **California Consumer Privacy Act (CCPA)** - California residents
- ✅ **Personal Information Protection and Electronic Documents Act (PIPEDA)** - Canadian users (where applicable)

**Legal Basis for Data Processing (GDPR Article 6):**

| Data Type | Legal Basis | GDPR Article |
|-----------|-------------|--------------|
| Account registration data | Contract performance | Article 6(1)(b) |
| Authentication/session data | Contract performance | Article 6(1)(b) |
| Service logs (security) | Legitimate interest | Article 6(1)(f) |
| Email validation | Contract performance | Article 6(1)(b) |
| P2P shared data | Consent (by voluntary sharing) | Article 6(1)(a) |

**Our Legitimate Interests (Article 6(1)(f)):**
- Service security and fraud prevention
- Technical error debugging
- Service availability monitoring
- Compliance with legal obligations

### Your Rights Under Each Law

**Under GDPR (EU/EEA):**
- ✅ Right to access (Article 15)
- ✅ Right to rectification (Article 16)
- ✅ Right to erasure/"Right to be forgotten" (Article 17) - *Limited by P2P architecture*
- ✅ Right to data portability (Article 20) - See Data Export section below
- ✅ Right to object (Article 21)
- ✅ Right to restrict processing (Article 18) - *Limited by P2P architecture*
- ✅ Right not to be subject to automated decision-making (Article 22) - *Not applicable*

**Under CCPA (California):**
- ✅ Right to know what personal information is collected
- ✅ Right to know whether personal information is sold or disclosed
- ✅ Right to opt-out of sale of personal information - *We do NOT sell data*
- ✅ Right to deletion - *Limited by P2P architecture*
- ✅ Right to non-discrimination for exercising rights

**Under PIPEDA (Canada):**
- ✅ Right to access personal information
- ✅ Right to correct inaccuracies
- ✅ Right to withdraw consent

### Exercising Your Rights

**Contact for Privacy Requests:**
- **Email:** info@openassociation.org
- **Subject Line:** "Privacy Rights Request - [GDPR/CCPA/PIPEDA]"

**Required Information:**
1. Your username
2. Email associated with account
3. Specific right you want to exercise
4. Proof of identity

**Response Time:**
- GDPR: Within 30 days (Article 12)
- CCPA: Within 45 days (extendable by 45 days)
- PIPEDA: Within 30 days

### Data Protection Authority

**If you are in the EU/EEA:**
You have the right to lodge a complaint with your Data Protection Authority:
- Find your DPA: https://edpb.europa.eu/about-edpb/board/members_en

**If you are in California:**
- California Privacy Protection Agency: https://cppa.ca.gov/

**If you are in Canada:**
- Office of the Privacy Commissioner of Canada: https://www.priv.gc.ca/

### Cross-Border Data Transfers

**Data Transfer Mechanisms:**
- Centralized account data: Stored in France (EU)
- P2P network data: Distributed globally through decentralized architecture
- By using the service, you consent to international data transfers via P2P network

**GDPR Article 49:** P2P data transfers rely on explicit consent for international transfers (Article 49(1)(a)) - you consent by voluntarily sharing data to the P2P network.

### Other Applicable Laws

**Copyright & Intellectual Property:**
- ✅ Software licensed under AGPL-3.0 (GNU Affero General Public License)
- ✅ User-generated content remains user's intellectual property
- ✅ P2P protocol respects copyright (users responsible for what they share)

**Anti-Spam Compliance:**
- ✅ **CAN-SPAM Act (U.S.)**: Emails only for service operation (validation, password reset)
- ✅ **CASL (Canada)**: No commercial electronic messages
- ✅ Users can unsubscribe from service emails

**Age Restrictions:**
- ✅ **COPPA (U.S.)**: Service requires 18+ or parental consent (no collection from children under 13 without consent)
- ✅ **GDPR (EU)**: Minimum age 16 (or lower per member state), parental consent required below age

**Accessibility:**
- 🔄 Working toward WCAG 2.1 Level AA compliance
- 🔄 Accessibility improvements ongoing

### Law Enforcement & Legal Requests

**We may disclose data when:**
- Required by valid legal process (subpoena, court order)
- Necessary to protect rights, property, or safety
- Required to prevent fraud or abuse

**What We Can Provide:**
- ✅ Centralized account data (encrypted email, username, account creation date)
- ✅ Service logs (if within 30-day retention)
- ❌ P2P network data (we have no control or access)

**Process:**
1. We verify legitimacy of legal request
2. We notify affected users unless legally prohibited
3. We provide only data specifically requested and legally required

### Data Protection Officer

**Status:** Not required under GDPR Article 37 (we are not a public authority, our core activities do not require systematic monitoring at large scale)

**Privacy Contact:** info@openassociation.org

---

## 💾 Data Export & Portability

### How to Export Your Data

You have the right to receive your personal data in a structured, commonly used, and machine-readable format (GDPR Article 20, CCPA Right to Know).

**What You Can Export:**
- ✅ Recognition trees
- ✅ Recognition weights (computed from trees)
- ✅ Capacity slots (resources you offer)
- ✅ Need slots (resources you need)
- ✅ Commitment data (full metadata)
- ✅ Network subscriptions and filters
- ✅ Membership lists
- ✅ Contacts

**Export Format:** 
- Standard JSON (non-proprietary)
- Human-readable and machine-parsable
- No proprietary encoding

**How to Export:**
1. Log in to your account
2. Click your username/avatar in top navigation
3. Click the **Export Data button** (💾 icon)
4. Your data will be:
   - Copied to your clipboard
   - Downloaded as free-association-export-[timestamp].json file

**What's NOT Included in Export:**
- ❌ Your password (encrypted, cannot be exported)
- ❌ Private keys (stored separately)
- ❌ Other users' data
- ❌ Network-wide computations

**Documentation:** 
See detailed export guide at [docs/project/data-export.md](https://github.com/interplaynetary/free-association/blob/main/docs/project/data-export.md)

**Technical Implementation:**
- Export function: userStateExport.ts in utils/data directory
- Schema: Validated against Zod schemas in protocol/schemas.ts
- Protocol: Open specification in PROTOCOL.md

---

## ⚠️ CRITICAL PRIVACY WARNINGS

### NO PRIVACY FOR P2P DATA

**🚨 UNDERSTAND BEFORE USING:**

1. **ALL P2P DATA IS PUBLIC**
   - Recognition trees are public
   - Capacity declarations are public
   - Usernames and public keys are public
   - All interactions are public and traceable

2. **P2P DATA IS PERMANENT**
   - May persist indefinitely on peer devices
   - We CANNOT delete it
   - May be archived, copied, analyzed by anyone

3. **NO ENCRYPTION FOR P2P DATA**
   - Data transmitted in clear text
   - Anyone can read it
   - No privacy guarantees

4. **NO CONTROL AFTER SHARING**
   - Cannot modify data once shared
   - Cannot restrict who sees it
   - Cannot prevent copies

### What You Should NEVER Share

**Do NOT share via Free-Association:** (unless you want them public forever)
- Real names 
- Physical addresses or locations
- Phone numbers
- Email addresses (other than encrypted account email)
- Financial information
- Government ID numbers
- Health/medical information
- Passwords or credentials
- Private personal information
- Data about others without their consent

---

## 🌍 International Data Transfers

**Where Data Is Stored:**
- **Centralized Account Data**: France
- **P2P Network Data**: Distributed globally across all peer devices

**Cross-Border Transfers:**
- P2P data automatically transfers globally (no control possible)
- By using the service, you consent to global data distribution

---

## 👶 Children's Privacy

**Age Requirement:** 18+ years old (or parental consent)

**We do NOT knowingly collect data from children under 18 without consent.**

If you believe a child has created an account without consent:
- Contact: info@openassociation.org
- We will delete the centralized account data
- ⚠️ But we CANNOT delete P2P data they shared

---

## 🔄 Data Retention

### Centralized Account Data
- **Active Accounts**: Retained while account is active
- **Deleted Accounts**: Removed from our servers within 30 days
- **Logs**: API/error logs retained 30 days maximum

### P2P Network Data
- **Retention**: PERMANENT (indefinite, beyond our control)
- **Deletion**: NOT POSSIBLE after data enters P2P network

---

## 📱 Cookies & Tracking

**We Use:**
- ✅ Essential session cookies (authentication)
- ✅ Local storage (browser-side data only)

**We do NOT use:**
- ❌ Advertising cookies
- ❌ Third-party tracking cookies
- ❌ Analytics cookies (currently)

---

## 📞 Contact & Privacy Requests

### Data Protection Contact
**Email:** info@openassociation.org  
**Subject Line:** "Privacy Request - [Your Request Type]"

**For Privacy Requests, Include:**
1. Your username
2. Email associated with account
3. Specific request (access, deletion, correction)
4. Verification of identity

**Response Time:** Within 30 days (GDPR requirement)

### Supervisory Authority
If you're in the EU, you have the right to lodge a complaint with your data protection authority:
- Find your authority: https://edpb.europa.eu/about-edpb/board/members_en

---

## 🔄 Changes to This Privacy Policy

**We may update this policy to:**
- Reflect service changes
- Comply with new laws
- Improve clarity

**When We Update:**
- ✅ Post new version with updated "Last Updated" date
- ✅ Notify via email for material changes (if we have your email)
- ✅ Continue using service = acceptance of new policy

---

## ✅ PRIVACY ACKNOWLEDGMENT

**BY USING FREE-ASSOCIATION, YOU ACKNOWLEDGE:**

1. ✅ You understand the **hybrid architecture** (centralized account data + public P2P network)

2. ✅ You understand **all P2P data is PUBLIC, UNENCRYPTED, and PERMANENT**

3. ✅ You understand **we CANNOT delete P2P data** once shared

4. ✅ You understand **GDPR rights do NOT apply** to P2P data (only to centralized account data)

5. ✅ You consent to **account data processing** as described in this policy

6. ✅ You consent to **global distribution** of any data you share in the P2P network

7. ✅ You will **NOT share sensitive personal information** through the service

8. ✅ You are **solely responsible** for privacy implications of data YOU share

---

**Last Updated:** 2025-11-15  
**Version:** 2.1.0 (Added Legal Compliance & Data Portability Sections)  
**Effective Date:** 2025-11-15  
**Jurisdiction:** France (EU) for centralized services  
**Governing Law:** French law and applicable EU regulations`;

	let htmlContent = '';

	onMount(() => {
		htmlContent = marked(privacyContent) as string;
	});
</script>

<div class="privacy-container">
	{@html htmlContent}
</div>

<style>
	.privacy-container {
		max-width: 800px;
		margin: 0 auto;
		padding: 2rem;
		font-family:
			system-ui,
			-apple-system,
			sans-serif;
		line-height: 1.6;
	}

	:global(.privacy-container h1) {
		font-size: 2.5rem;
		margin-bottom: 1.5rem;
		color: #333;
	}

	:global(.privacy-container h2) {
		font-size: 2rem;
		margin-top: 2rem;
		margin-bottom: 1rem;
		color: #444;
	}

	:global(.privacy-container h3) {
		font-size: 1.5rem;
		margin-top: 1.5rem;
		color: #555;
	}

	:global(.privacy-container ul) {
		padding-left: 1.5rem;
	}

	:global(.privacy-container li) {
		margin: 0.5rem 0;
	}

	:global(.privacy-container strong) {
		color: #222;
	}

	:global(.privacy-container hr) {
		margin: 2rem 0;
		border: none;
		border-top: 1px solid #eee;
	}
</style>
