<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const termsContent = `# Protocol Terms & Disclosures
## P2P Location & Messaging Protocol (Beta)

**Last Updated:** 2025-06-04  
**Effective Date:** 2025-06-04

---

## 🔧 What This Is (And What It Isn't)

### Protocol vs. Service
- **This is a PROTOCOL**: We have created open-source software code that enables peer-to-peer location sharing and messaging
- **This is NOT a service**: We do not operate servers, host data, or provide centralized services
- **Decentralized by Design**: All data flows directly between user devices via GUN.js peer-to-peer protocol
- **No Central Authority**: No single entity controls or manages the distributed network

### Our Role
- **Protocol Developers**: We created the software code/protocol specification
- **Open Source Contributors**: We maintain and distribute free, open-source software
- **NOT Service Providers**: We do not operate, control, or monetize any network or service
- **NOT Data Controllers**: We do not collect, store, process, or control user data

### User Responsibilities as Network Operators
- **You operate your own peer**: By running this software, YOU become a network operator
- **Direct peer-to-peer communication**: Your device communicates directly with other user devices
- **Independent operation**: Each user independently chooses to run the protocol software
- **Self-hosted infrastructure**: Users provide their own devices, internet, and storage
- **Protocol Augmentation**: Users are fully responsible for augmenting the protocol to suit their social, technical, and legal requirements
- **Compliance Filtering**: Users are wholly responsible for implementing any filters or limitations to their capacities to comply with the socio-technical-legal limits that apply to them

## 📋 Software License & Usage Terms

### Open Source License
This protocol software is provided under [LICENSE_TYPE] open source license:
- Free to use, modify, and distribute
- No warranties or guarantees
- Use at your own risk
- Source code available at [REPOSITORY_URL]

### Protocol Implementation
- **Experimental Software**: This is beta protocol software under active development
- **No Compatibility Guarantees**: Protocol may change, breaking existing implementations
- **Community Driven**: Development guided by open-source community
- **Fork Friendly**: Anyone may create derivative implementations

### Age & Capacity Requirements
- Must be 18+ or have parental consent to download/use protocol software
- Must comply with local laws regarding software use and data sharing
- Parents/guardians responsible for minors' software usage

## 🛡️ Liability Limitations & Disclaimers

### Protocol Developer Immunity
**CRITICAL**: As protocol developers providing open-source software, we are not liable for:

**Network Operations:**
- How users operate their peers
- Data shared between peers
- Network behavior or performance
- Peer storage or deletion policies

**User Actions:**
- How individuals use the protocol software
- Content shared through the protocol
- Privacy or security practices of peer operators
- Compliance with local laws by users

### Software Provided "AS-IS"
- **No Warranties**: Software provided without any warranties
- **No Support**: No obligation to provide user support
- **No Maintenance**: No guaranteed updates or bug fixes
- **No Liability**: Maximum liability limited to $0 (zero dollars)
- **Use at Own Risk**: Users assume all risks of software usage

## ⚠️ FINAL PROTOCOL WARNINGS

**🚨 YOU ARE THE OPERATOR**: By running this software, YOU become a peer operator in a distributed network

**🚨 NO CENTRAL AUTHORITY**: No company or organization controls this network or your data

**🚨 TECHNICAL COMPLEXITY**: P2P protocols require technical understanding to operate safely

**🚨 PERMANENT PUBLIC DATA**: All shared content may persist indefinitely across multiple peers

**🚨 LEGAL RESPONSIBILITY**: You are solely responsible for compliance with applicable laws

**🚨 EXPERIMENTAL PROTOCOL**: This is beta software that may have bugs, security vulnerabilities, or unexpected behavior

**ACKNOWLEDGMENT OF PROTOCOL NATURE**

By using this protocol, I acknowledge that:

1. This is open-source protocol software, not a centralized service
2. I will operate my own peer and am responsible for my participation
3. ALL shared data (locations, messages, interactions) will be PUBLIC and UNENCRYPTED
4. The developers provide software only and assume no operational liability
5. I am 18+ years old and legally capable of operating P2P networking software
6. I accept full responsibility for my use of this protocol software`;

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
