# Free Association Coalition
## Draft Participation Framework for Review
*Drafted by: Initial working group convened during 2025 coordination sessions*

This coalition consists of entities experimenting with protocols for voluntary coordination and capacity-building. The coalition proposes a re-engineering of how collective action and resource allocation can be coordinated.

**Coordination is based on:**
- Publishing (what is, what I have/need/prioritize)
- Derivation (what we can infer collectively)
    - Recognition (acknowledgement of who/what contributes)
    - Allocation (how we divide our capacities)

**Foundational Principles:**
- Sovereignty and Interoperability: Participants retain full control over their own data, recognitions, and capacity allocations. Participants explore protocols enabling collaboration without requiring surrender of autonomy.
- Optimized Coordination: The vision is for a significant portion of capacities/resources being allocated by participants informed by relevant derivations on the state of network data, drastically reducing transaction costs and delays.
- Scale-Invariant Relationships: Coordination based on proportions rather than absolute quantities. This allows relationships and potential capacity flows to scale infinitely.

**Participants may publish/derive data from local/network-data.** 
For example: capacities, needs, recognitions, proportions, collective membership, environmental data, qualities, goals, estimates, decisions, sources for deriving, filters and their applications, or any other data.

**Participants can publish/propose/offer/allocate with the help of protocols of their choosing.**

**Try It:** [free.playnet.lol](https://free.playnet.lol) (browser-based, peer-to-peer, no installation required)

## Implementation and Participation

### For Organizations and Institutions

**Pilot Programs**: Organizations interested in piloting Free Association for resource coordination can:
- Start with a discretionary budget allocation
- Implement within specific program areas or partnerships
- Join coordination coalitions with other pilot organizations
- Access technical support and implementation guidance

### For Developers and Contributors

**Development Priorities:**
- User interface refinement
- Protocol implementation and testing
- Documentation and educational materials
- Technical infrastructure and deployment

**Ways to Contribute:**
- **Technical Skills**: Contact team to discuss development needs
- **Resources**: Support infrastructure and operational requirements
- **Network Building**: Share with potential organizational partners

<a href="https://opencollective.com/free-association">
    <img width="300" src="https://opencollective.com/free-association/donate/button@2x.png?color=blue" />
</a>

**Contact:**
- General inquiries: info@openassociation.org
- Coalition and partnerships: coalition@openassociation.org

**Additional Resources:**
- [openassociation.org](https://openassociation.org)
- [Glossary](GLOSSARY.md)
- [Governance](GOVERNANCE.md)

**Interface Demonstration:**

[![Interface Overview](https://img.youtube.com/vi/gapy9mBpP5w/hqdefault.jpg)](https://youtu.be/gapy9mBpP5w?si=B2sbZpoaXStg4eKL)

## Use Cases and Outcomes

### Crisis Response: From Months to Seconds

Traditional coordination requires lengthy political negotiations before resources reach those in need. Free Association transforms this:

**Traditional System:**
- Day 1: Crisis hits
- Day 30: Coordination bodies convene
- Day 90: Political negotiations begin
- Day 180: Pledges finalized
- Day 270+: Resources begin flowing

**Free Association:**
- Day 1: Entity declares need in system
- Immediately: All participants see need; system recalculates optimal allocation
- Day 1-2: Resource commitments transparent and automatic based on pre-established prioritization
- Day 2-3: First resources arrive from mutual partners
- Ongoing: System continuously adapts as needs evolve

### Key Outcomes

**Speed**: Resource allocation occurs in seconds rather than months
- Target: <48 hours from need identification to commitment
- vs. typical 90+ days in traditional systems

**Efficiency**: Direct resource flow with minimal overhead
- Target: >95% of resources deployed to mission
- vs. typical ~70% after administrative costs

**Adaptability**: System responds in real-time as circumstances evolve
- Priorities change → allocations recalculate automatically
- New needs emerge → system converges to new equilibrium
- Partners join/leave → network adapts seamlessly

## Technical Documentation

### Development Setup

**Prerequisites:**
- [Install bun](https://fenilsonani.com/articles/installation-step-of-bun)

**Development:**
```bash
bun install
bun run dev
```

**Testing:**
```bash
npm test  # Uses vitest and playwright
```

**Production Build:**
```bash
bun install
bun run build
```

## License & Governance

**License:** [GNU Affero General Public License v3.0](LICENSE) with 
[Additional Terms](LICENSE-ADDITIONAL-TERMS.md)

### Quick Summary

✅ **Anyone can use Free Association** - individuals, cooperatives, non-profits, 
governments, corporations

✅ **You can modify and deploy it** - fork it, adapt it, run your own instance

✅ **If you run it as a network service** - you must share your source code 
(that's the AGPL requirement)

✅ **You must give attribution** - credit where credit is due, prevents invisible use

✅ **Modified versions must be clearly marked** - prevents protocol fragmentation

### Why AGPL-3.0?

We want **universal access without universal capture**. AGPL ensures:
- Anyone can use it (no restrictions on who)
- Network services must share improvements (prevents proprietary capture)
- Modifications remain open (keeps the commons healthy)

### Why Additional Terms?

The additional terms (permitted by AGPL Section 7) add:
- **Attribution requirement** - prevents invisible cooptation
- **Protocol fidelity marking** - prevents trust-breaking fragmentation
- **Interoperability commitment** - prevents vendor lock-in

Together: Open for all, captured by none.

### Reference Implementation

This repository is the **canonical reference implementation** of the Free 
Association protocol, maintained by the core development team.

Other implementations are encouraged, but protocol conformance is measured 
against this implementation. See [PROTOCOL.md](PROTOCOL.md) for the formal 
specification.

### Questions?

- Licensing: info@openassociation.org
- Protocol: See [PROTOCOL.md](PROTOCOL.md) or open an issue
- Governance: See [GOVERNANCE.md](GOVERNANCE.md)