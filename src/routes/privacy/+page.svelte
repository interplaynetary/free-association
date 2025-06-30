<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const privacyContent = `# Privacy Policy
## P2P Location & Messaging Protocol (Beta)

**Last Updated:** 2025-06-04  
**Effective Date:** 2025-06-04
---

## ⚠️ Technical & Privacy Disclosures

### How Your Data Flows
- **Direct P2P Communication**: Your device connects directly to other protocol users' devices
- **No Central Servers**: No company servers store or process your data
- **Voluntary Participation**: Users voluntarily join the distributed network
- **Autonomous Operation**: Each peer operates independently without central coordination
- **Public Data Sharing**: ALL shared data is transmitted and stored publicly without encryption across the peer network

### Data Storage & Processing
- **Peer-to-Peer Only**: Data shared directly between user devices
- **No Service Provider Storage**: We do not store, access, or control any user data
- **User-Controlled**: Each user decides what data to share and with whom
- **Distributed Replication**: Data may replicate to multiple peer devices based on protocol mechanics
- **ALL DATA PUBLIC BY DEFAULT**: Location coordinates, messages, user interactions, and all shared content are transmitted and stored WITHOUT encryption
- **No Privacy Guarantees**: Any data shared through the protocol should be considered permanently public and globally accessible

### What We Do NOT Do
- ❌ Operate servers or cloud infrastructure
- ❌ Store or process user data
- ❌ Monitor network activity
- ❌ Control data distribution
- ❌ Provide user support or services
- ❌ Moderate content or behavior
- ❌ Have access to user communications
- ❌ Collect analytics or telemetry

## 🌐 Data Protection & Privacy Laws

### GDPR and Other Privacy Laws
- **You are the Data Controller**: Users are data controllers for their own peer operations
- **No Processing by Us**: We do not process personal data as defined by privacy laws
- **User Responsibility**: Peer operators must comply with applicable privacy laws
- **No Data Controller Role**: We are not data controllers or processors under GDPR

### Data Collection & Usage
- **No Collection by Us**: We do not collect any user data
- **Peer Network Data**: All data is shared directly between peers
- **Public Nature**: All shared data should be considered public and permanent
- **User Control**: You control what data you share through the protocol

### Data Security
- **No Encryption**: Data is transmitted and stored WITHOUT encryption
- **Public Network**: All peer communications are public
- **User Responsibility**: Users are responsible for their own data security
- **No Security Guarantees**: Protocol provides no built-in security features

## 🔒 User Privacy Recommendations

### Best Practices
- Only share data you are comfortable being public
- Do not share sensitive personal information
- Be aware that all shared data may be permanent
- Consider local privacy laws before sharing data

### Data Sharing Considerations
- Location data is public and unencrypted
- Messages are public and unencrypted
- User interactions are public and traceable
- No deletion guarantees for shared data

## 📝 Privacy Rights

### Your Rights
- Control what data you share
- Choose when to participate in the network
- Operate your peer independently
- Comply with local privacy laws

### Important Notes
- We cannot assist with data access requests
- We cannot delete or modify shared data
- We have no access to user data
- All privacy controls are user-managed

---

**PRIVACY ACKNOWLEDGMENT**

By using this protocol, I acknowledge that:

1. All shared data will be PUBLIC and UNENCRYPTED
2. The developers do not collect or control any user data
3. I am responsible for my own data privacy
4. Shared data may be permanent and globally accessible`;

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
