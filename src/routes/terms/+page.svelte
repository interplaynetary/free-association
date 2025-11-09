<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const termsContent = `# Terms of Service
## Free-Association: Hybrid P2P/Centralized Service (Beta)

**Last Updated:** 2025-11-09  
**Effective Date:** 2025-11-09

---

## 🔧 What This Service Is

### Hybrid Architecture
Free-Association is a **hybrid service** combining centralized and peer-to-peer components:

**Centralized Services (We Operate):**
- Account registration and authentication system
- Invite code management
- Gun/Holster relay servers for P2P data synchronization
- AI gateway service (optional AI features)
- Email validation and password reset services
- RSS feed management and data relay services
- Health monitoring and administrative functions

**Decentralized P2P Components:**
- Recognition tree data (stored locally and synced P2P)
- Direct peer-to-peer data synchronization via Gun/Holster
- Local computation of allocations and mutual recognition
- Browser-based interface (runs locally on your device)

### Our Role
- **Service Operators**: We operate servers that provide centralized services
- **Protocol Developers**: We created and maintain the Free-Association protocol
- **Open Source Maintainers**: We maintain open-source software (AGPL-3.0)
- **Data Controllers**: We process limited account data as described in Privacy Policy

### What This Means for You
- **You create an account** with us to access the service
- **Your account data** (username, encrypted email, invite codes) is stored on our servers
- **Your recognition tree data** syncs via our relay servers to other peers
- **All shared data** enters a P2P network and becomes PUBLIC and may be PERMANENT
- **We provide the infrastructure** but cannot control data once it enters the P2P network

---

## 📋 Software License & Service Terms

### Open Source License
The Free-Association software is provided under **GNU Affero General Public License v3.0** with Additional Terms:
- Source code: https://github.com/playnet-lol/free-association
- Free to use, modify, and distribute under AGPL-3.0 terms
- Network service usage requires source code disclosure (AGPL requirement)
- See LICENSE.md and LICENSE-ADDITIONAL-TERMS.md for full terms

### Service Usage Terms
- **Beta Service**: This is experimental software under active development
- **No Guarantees**: Service availability, data persistence, and features may change
- **Account Required**: You must create an account to use the service
- **Invite-Only**: New accounts currently require an invite code
- **Acceptable Use**: Do not use the service for illegal purposes, harassment, or abuse

### Age Requirements
- Must be 18+ or have parental/guardian consent
- Must be legally capable of entering into this agreement
- Parents/guardians are responsible for minors' usage

---

## ⚠️ CRITICAL DATA WARNINGS

### ALL SHARED DATA BECOMES PUBLIC

**🚨 PERMANENT PUBLIC DATA WARNING**

Once you share data through Free-Association (recognition trees, allocations, capacity declarations, messages):

1. **Data Becomes Public**: All shared data is PUBLIC and UNENCRYPTED
2. **Data May Be Permanent**: Data replicates across peer-to-peer network and may persist indefinitely
3. **We Cannot Delete It**: Once data enters the P2P network, we have no ability to delete or modify it
4. **No Control**: Data may be stored on thousands of peer devices worldwide
5. **No Privacy**: Assume all shared data is visible to anyone, anywhere, forever

**What This Means:**
- Recognition trees → Public
- Capacity declarations → Public
- Allocation calculations → Public
- Usernames and public keys → Public
- All interactions → Public and traceable

**We CANNOT:**
- ❌ Delete data from the P2P network
- ❌ Control who stores your data
- ❌ Prevent data from spreading to other peers
- ❌ Modify data once shared
- ❌ Guarantee data privacy or security

### Data You Should NEVER Share (unless you want them public forever)
- Real names
- Physical addresses
- Phone numbers
- Financial information
- Government ID numbers
- Passwords or authentication credentials
- Medical information
- Any data you want to remain private

---

## 🎯 Your Responsibilities

### As a Service User
- **Accurate Information**: Provide accurate account information during registration
- **Secure Credentials**: Keep your password and authentication credentials secure
- **Compliance**: Comply with all applicable laws in your jurisdiction
- **Appropriate Content**: Do not share illegal, harmful, or abusive content
- **Accept Risk**: You use this service at your own risk

### As a P2P Network Participant
- **Public Data**: Accept that all data you share becomes public
- **Permanent Data**: Accept that shared data may be permanent and undeletable
- **No Recourse**: Accept that we cannot delete or modify data in the P2P network
- **Your Liability**: You are solely responsible for what data you share
- **Legal Compliance**: You are responsible for compliance with privacy laws regarding data YOU share about others

---

## 🛡️ Liability Limitations & Disclaimers

### Service Provided "AS-IS"
This service is provided **AS-IS** and **AS-AVAILABLE** without warranties of any kind:

- **No Warranties**: No express or implied warranties
- **No Uptime Guarantees**: Service may be unavailable at any time
- **No Data Guarantees**: Data may be lost, corrupted, or become inaccessible
- **No Security Guarantees**: We implement security measures but cannot guarantee security
- **Beta Software**: Expect bugs, errors, and unexpected behavior

### Limitation of Liability
**TO THE MAXIMUM EXTENT PERMITTED BY LAW:**

1. **Maximum Liability**: Our total liability is limited to $0 (zero dollars)
2. **No Consequential Damages**: We are not liable for indirect, incidental, consequential, or punitive damages
3. **No Data Loss Claims**: We are not liable for data loss or corruption
4. **No P2P Network Claims**: We are not liable for data persistence, deletion, or privacy in the P2P network
5. **No Third-Party Claims**: We are not liable for actions of other users or peers

### Specific P2P Network Disclaimers
**We are NOT liable for:**
- Data that persists in the P2P network after you want it deleted
- Privacy violations resulting from public P2P data
- Other peers' storage or use of your data
- Content shared by other users
- Network behavior or performance
- P2P protocol vulnerabilities or exploits

### Indemnification
You agree to indemnify and hold us harmless from any claims arising from:
- Your use of the service
- Data you share through the service
- Your violation of these terms
- Your violation of any laws or third-party rights

---

## 🔒 Account Management

### Account Creation
- Accounts are created via invite code system
- You must provide a username and email address
- Email is stored encrypted and used only for account validation and password reset
- You are responsible for keeping your password secure

### Account Termination
**You may terminate your account**, but understand:
- ✅ We can delete your account from our servers
- ✅ You will lose access to the service
- ❌ We CANNOT delete your data from the P2P network
- ❌ Your shared recognition trees and data may persist forever on other peers
- ❌ Your username and public key will remain in the P2P network

**We may terminate your account** if:
- You violate these terms
- You engage in illegal activity
- You abuse or harm the service
- We discontinue the service

---

## 📊 Service Changes & Discontinuation

### Changes to Service
We reserve the right to:
- Modify or discontinue features at any time
- Change these terms (with notice)
- Update the protocol or software
- Change pricing (currently free)

### Service Discontinuation
We may discontinue the service at any time:
- We will provide reasonable notice if possible
- Your data on our servers may be deleted
- Data in the P2P network will persist beyond our control
- Open-source software remains available for self-hosting

---

## 🌍 Jurisdiction & Disputes

### Governing Law
These terms are governed by the laws of [YOUR_JURISDICTION] without regard to conflict of law provisions.

### Dispute Resolution
Disputes will be resolved through:
1. Good faith negotiation
2. Mediation (if negotiation fails)
3. Binding arbitration or courts in [YOUR_JURISDICTION]

### Severability
If any provision is found unenforceable, other provisions remain in effect.

---

## 📞 Contact & Support

### Contact Information
- Email: ruzgar@playnet.lol
- Website: https://playnet.lol
- GitHub: https://github.com/playnet-lol/free-association

### Support Limitations
- **No Support Guarantee**: We provide support on a best-effort basis
- **Community Support**: Primary support is community-driven
- **No SLA**: We do not guarantee response times or resolution

---

## ✅ ACKNOWLEDGMENT & ACCEPTANCE

**BY USING FREE-ASSOCIATION, YOU ACKNOWLEDGE AND AGREE:**

1. ✅ This is a **hybrid centralized/P2P service** with both server infrastructure and peer-to-peer components

2. ✅ **All shared data becomes PUBLIC and UNENCRYPTED** and may persist permanently

3. ✅ **We CANNOT delete or modify data** once it enters the P2P network

4. ✅ **You assume all risks** including but not limited to:
   - Permanent public exposure of shared data
   - Inability to delete or modify shared data
   - Privacy implications of P2P data replication
   - Service interruptions or termination
   - Data loss or corruption
   - Security vulnerabilities

5. ✅ **Our liability is limited to $0** and we are not liable for any damages

6. ✅ **You are 18+ years old** (or have parental consent) and legally capable of entering this agreement

7. ✅ **You are solely responsible** for what data you share and compliance with applicable laws

8. ✅ This is **beta software** under active development with no guarantees

---

**Last Updated:** 2025-11-09  
**Version:** 2.0.0 (Hybrid Architecture Disclosure)`;

	let htmlContent = '';

	onMount(() => {
		htmlContent = marked(termsContent) as string;
	});
</script>

<div class="terms-container">
	{@html htmlContent}
</div>

<style>
	.terms-container {
		max-width: 800px;
		margin: 0 auto;
		padding: 2rem;
		font-family:
			system-ui,
			-apple-system,
			sans-serif;
		line-height: 1.6;
	}

	:global(.terms-container h1) {
		font-size: 2.5rem;
		margin-bottom: 1.5rem;
		color: #333;
	}

	:global(.terms-container h2) {
		font-size: 2rem;
		margin-top: 2rem;
		margin-bottom: 1rem;
		color: #444;
	}

	:global(.terms-container h3) {
		font-size: 1.5rem;
		margin-top: 1.5rem;
		color: #555;
	}

	:global(.terms-container ul) {
		padding-left: 1.5rem;
	}

	:global(.terms-container li) {
		margin: 0.5rem 0;
	}

	:global(.terms-container strong) {
		color: #222;
	}

	:global(.terms-container hr) {
		margin: 2rem 0;
		border: none;
		border-top: 1px solid #eee;
	}
</style>
