<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const privacyContent = `# Privacy Policy
## Free-Association: Hybrid P2P/Centralized Service (Beta)

**Last Updated:** 2025-11-09  
**Effective Date:** 2025-11-09

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
**Website:** https://playnet.lol  
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

**Storage Location:** Our servers (Gun/Holster relay servers)

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

**Last Updated:** 2025-11-09  
**Version:** 2.0.0 (Hybrid Architecture Disclosure)  
**Effective Date:** 2025-11-09`;

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
