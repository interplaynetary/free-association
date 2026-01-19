# Identity

Your identity in Free Association is based on **Verifiable Credentials (VCs)** - portable, offline-verifiable digital proofs.

## The Problem with Traditional Identity

Traditional systems require you to:
- **Trust a central authority** to verify who you are
- **Be online** to prove your identity
- **Ask permission** to share your credentials

## The Solution: Verifiable Credentials

Think of a VC like a **digital passport** that:
- **Works offline** - No internet connection needed for verification
- **Is portable** - Travels with you, not locked in someone's database
- **Is tamper-proof** - Any modification breaks the cryptographic signature

### Example: A Capacity Credential

```
"I, Alice, have 100 hours/month of consulting capacity available."
- Signed by: Alice's cryptographic key
- Verifiable by: Anyone, anywhere, offline
```

Anyone can verify this claim is authentic without asking a central authority.

## How It Works (Simple Version)

1. **You generate a key pair** (like a password, but mathematical)
2. **You sign your claims** with your private key
3. **Others verify your signature** with your public key

**Key Property**: Only you can create signatures with your private key, but anyone can verify them with your public key.

## What Can You Claim?

### About Yourself (Reflexive)
```
"I need $50K for a project"
"I have 20 hours/week available"
"I prioritize education initiatives"
```

### About Others (Relational)
```
"I recognize Bob's contribution at 30%"
"I endorse Carol for technical expertise"
```

## Why This Matters

**Portability**: Your credentials aren't locked in one platform. You can use them anywhere.

**Offline Verification**: No need to call a server to verify authenticity. Works in remote areas, during disasters, or when networks are down.

**Sovereignty**: You control what you claim and who you share it with. No central authority can revoke or modify your credentials.

## Technical Note

Under the hood, Free Association uses:
- **DID (Decentralized Identifiers)**: Self-sovereign identity standard
- **Ed25519 Signatures**: Fast, secure cryptographic signing
- **W3C VC Standard**: Interoperable credential format

But you don't need to understand these to use the system. The interface handles the complexity.

## Further Reading

- [W3C Verifiable Credentials](https://www.w3.org/TR/vc-data-model/) - Technical specification
- [Resources](resources.md) - What you can declare about your capacities and needs
- [Priorities](priorities.md) - How you express what you value
