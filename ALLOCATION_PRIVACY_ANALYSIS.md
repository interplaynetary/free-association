# Allocation Algorithm Privacy Analysis

**Date:** 2025-11-02  
**Algorithm:** Free Association Protocol - Two-Tier Allocation System  
**Files Analyzed:** `allocation.svelte.ts`, `allocation.ts`

## Executive Summary

This document analyzes the privacy requirements of the allocation algorithm to determine:
1. What data **must** be public for computation
2. What data **can** be made private (via ZK proofs or other cryptographic techniques)
3. Trade-offs and feasibility considerations

---

## Algorithm Overview

The allocation algorithm is a **two-tier resource allocation system** that:

**Tier 1 (Mutual Recognition)**: Allocates capacity based on bidirectional recognition relationships  
**Tier 2 (Non-Mutual Recognition)**: Distributes remaining capacity based on unilateral recognition

### Key Formula (Tier 1):
```
Your_Allocation = Provider_Capacity ? (Your_MR_Share ? Your_Active_Need) / Denominator

where:
- Your_MR_Share = Your_MR / Total_MR_of_all_recipients
- Your_Active_Need = Your_Stated_Need ? Damping_Factor
- Denominator = ?(each_recipient's_MR_Share ? their_Active_Need)
```

---

## Data Privacy Classification

### ? MUST BE PUBLIC (Cannot Be Hidden)

These data elements are **required by the algorithm** and cannot be effectively privatized without breaking core functionality:

#### 1. **Provider's Available Capacity** (`capacity_slots[].quantity`)
- **Why Public:** Recipients need to know total available capacity to compute expected allocations
- **Used In:** Allocation formula denominator, convergence metrics
- **ZK Feasibility:** ? Low - would require homomorphic encryption with high computational overhead
- **Impact if Hidden:** Recipients cannot verify allocations are fair or compute their expected share

#### 2. **Mutual Recognition Values** (`MR(A,B)`)
- **Why Public:** Core algorithm driver; determines allocation priority and amounts
- **Formula:** `MR(A,B) = min(A_recognizes_B, B_recognizes_A)`
- **Used In:** Tier 1 allocation weights, recipient eligibility
- **ZK Feasibility:** ?? Medium - could use ZK range proofs to prove `MR > threshold` without revealing exact value
- **Impact if Hidden:** Algorithm cannot compute allocation shares; fairness verification impossible

#### 3. **Need Type IDs** (`need_slots[].need_type_id`)
- **Why Public:** Required for compatibility matching (food needs ? food capacity)
- **Used In:** Type matching, spatial/temporal indexing, convergence tracking
- **ZK Feasibility:** ? Low - needs are inherently categorical
- **Impact if Hidden:** Cannot match providers with recipients; system breaks down

#### 4. **Slot Compatibility Metadata** (time ranges, location constraints)
- **Why Public:** Ensures realistic allocation (can't allocate Tuesday meals for Monday)
- **Used In:** `slotsCompatible()` function, spatial/temporal indexing
- **ZK Feasibility:** ?? Medium - could use ZK set membership proofs for time buckets
- **Impact if Hidden:** Allocations become impractical; recipients get unusable resources

#### 5. **ITC Stamps** (Causal Consistency)
- **Why Public:** Ensures all peers see consistent event ordering in P2P network
- **Used In:** Causal consistency checks, conflict resolution
- **ZK Feasibility:** ? Low - causality is inherently a global property
- **Impact if Hidden:** Network consistency breaks; double-spending and conflicts possible

---

### ?? CAN BE MADE PRIVATE (Privatization Possible)

These data elements **could be hidden or obfuscated** while maintaining algorithm correctness:

#### 1. **Exact Need Quantities** (`need_slots[].quantity`)
- **Why Privatizable:** Only *relative* needs matter for allocation proportions
- **Current Use:** Direct input to allocation formula
- **ZK Approach:** **Range Proofs + Homomorphic Encryption**
  - Prove `Need ? [min, max]` without revealing exact value
  - Use homomorphic operations for `Denominator = ?(MR ? Need)`
  - Recipients receive encrypted allocations, decrypt locally
- **Feasibility:** ?? **Medium-High** - requires significant cryptographic infrastructure
- **Trade-offs:**
  - ? Protects vulnerability information (e.g., "I'm desperate for food")
  - ? 10-100x computational overhead for homomorphic operations
  - ? Convergence metrics become opaque (can't audit system health)
  - ? Debugging becomes extremely difficult

#### 2. **Recognition Weights** (`global_recognition_weights`)
- **Why Privatizable:** Only *mutual recognition* (the minimum) is algorithmically necessary
- **Current Use:** Tier 2 allocation (non-mutual generous giving), MR computation
- **ZK Approach:** **Secure Two-Party Computation (2PC)**
  - Alice and Bob run 2PC protocol to compute `MR(A,B) = min(A?B, B?A)`
  - Only the result is published; individual weights remain private
  - Each party learns only the mutual value, not the other's private weight
- **Feasibility:** ? **High** - well-studied 2PC protocols exist for min function
- **Trade-offs:**
  - ? Protects social relationship privacy ("I don't want Alice to know I prioritize Bob")
  - ? Relatively efficient (few rounds, low bandwidth)
  - ? Tier 2 allocations become impossible (requires unilateral recognition)
  - ?? May reduce transparency and trust in the system

#### 3. **Damping History** (`multi_dimensional_damping.damping_history`)
- **Why Privatizable:** Only current damping *factor* affects allocations, not historical data
- **Current Use:** Oscillation detection, factor computation (internal to recipient)
- **ZK Approach:** **Commit-Reveal + Range Proofs**
  - Commit to damping factor with zero-knowledge proof
  - Prove factor is in valid range [0.5, 1.0]
  - Don't reveal the historical over-allocations that led to it
- **Feasibility:** ? **High** - simple range proofs
- **Trade-offs:**
  - ? Hides past over-allocation patterns (could reveal behavioral information)
  - ? Low computational cost
  - ? Providers can't verify damping is honest (gaming risk)

#### 4. **Allocation Slot IDs** (`slot_allocations[].availability_slot_id`)
- **Why Privatizable:** Internal bookkeeping; not required for algorithm correctness
- **Current Use:** Linking allocations back to provider capacity slots, debugging
- **ZK Approach:** **Pseudonymous IDs + Linkable Ring Signatures**
  - Use cryptographic pseudonyms instead of deterministic IDs
  - Prove allocation came from provider without revealing which specific slot
- **Feasibility:** ? **High** - existing cryptographic primitives
- **Trade-offs:**
  - ? Reduces tracking/profiling of provider behavior over time
  - ? Makes debugging and auditing harder
  - ? May complicate multi-slot coordination

#### 5. **Recipient Public Keys in Allocations** (`recipient_pubkey`)
- **Why Privatizable:** Provider knows who they're allocating to, but network doesn't need to
- **Current Use:** Transparency, audit trails, recipient notification
- **ZK Approach:** **Anonymous Credentials + Blind Signatures**
  - Provider signs allocation for "someone with attribute X"
  - Recipient proves they match attribute without revealing identity
  - Only provider and recipient know the link
- **Feasibility:** ?? **Medium** - complex cryptographic protocols
- **Trade-offs:**
  - ? Protects recipient privacy from network observers
  - ? Reduces surveillance and profiling risks
  - ? Network-wide convergence metrics become impossible to compute
  - ? Harder to detect abuse (e.g., Sybil attacks, allocation hoarding)
  - ? Breaks transparency model of the protocol

---

### ?? HYBRID APPROACHES (Partial Privacy)

#### **Tiered Disclosure Model**
- **Public**: Aggregate statistics (total needs by type, system convergence rate)
- **Semi-Private**: Need ranges instead of exact values ("10-50 meals" vs "42 meals")
- **Private**: Recognition weights (use 2PC for MR computation)
- **Feasibility:** ? High - doesn't require advanced crypto
- **Trade-offs:** Balances privacy with debuggability and trust

#### **Threshold-Based Visibility**
- **Below Threshold**: Full privacy (e.g., needs < 10 units are hidden)
- **Above Threshold**: Public (large needs visible for coordination)
- **Rationale:** Protects vulnerable individuals; large-scale needs are already semi-public
- **Feasibility:** ? High - simple conditional logic
- **Trade-offs:** May create gaming incentives (split needs to stay below threshold)

---

## Convergence Metrics Privacy Impact

The algorithm tracks system-wide convergence using these metrics:

| Metric | Formula | Privacy Impact if Needs Hidden |
|--------|---------|-------------------------------|
| **Total Need Magnitude** | `?N?? = ??(needs?)` | ? **Impossible** - requires all needs |
| **Contraction Rate** | `Current/Previous` | ? **Impossible** - requires magnitude |
| **Percent Needs Met** | `Satisfied / Total` | ? **Impossible** - requires per-person totals |
| **Universal Satisfaction** | `All needs < ?` | ? **Impossible** - requires all needs |
| **Iterations to Convergence** | `log(target/current)/log(rate)` | ? **Impossible** - requires rate |

**Conclusion:** If need quantities are fully hidden, **system health monitoring becomes impossible**. The algorithm would be flying blind, unable to detect divergence, stagnation, or exploitation.

### Possible Mitigations:
1. **Differential Privacy**: Add noise to aggregate statistics
   - Feasibility: ? High
   - Trade-off: Reduced accuracy in convergence detection
2. **Secure Aggregation**: Compute global metrics using MPC
   - Feasibility: ?? Medium (requires coordination among all peers)
   - Trade-off: High latency, requires active participation

---

## Spatial/Temporal Indexing Privacy

The algorithm uses reactive indexes for O(k) lookup optimization:

```typescript
networkNeedsIndex: {
  byType: Map<typeId, Set<pubKeys>>,
  byTypeAndLocation: Map<"typeId|location", Set<pubKeys>>,
  byTypeAndTime: Map<"typeId|time", Set<pubKeys>>,
  byAll: Map<"typeId|location|time", Set<pubKeys>>
}
```

**Privacy Implications:**
- ? **Good**: Public keys are already pseudonymous (no direct identity leak)
- ?? **Risk**: Location/time patterns can be analyzed for profiling
  - Example: "PubKey X always needs food on Tuesday mornings in District 5" ? likely a school
  - Example: "PubKey Y needs medical care in multiple locations" ? possible chronic condition

**ZK Approach:** **Private Information Retrieval (PIR)**
- Provider queries index without revealing *which* locations/times they're interested in
- Feasibility: ?? Medium - PIR has high bandwidth overhead
- Trade-off: 100-1000x bandwidth increase; may not be practical for real-time allocation

---

## Attack Scenarios & Privacy Trade-offs

### Scenario 1: **Sybil Attack Detection**
- **Attack**: Malicious user creates many identities to claim excess resources
- **Detection Method**: Analyze allocation patterns, need-to-capacity ratios
- **Privacy Impact**: Requires visibility into individual allocations and needs
- **Conclusion:** Full privacy makes Sybil detection **impossible** ?

### Scenario 2: **Gaming Damping Mechanism**
- **Attack**: User manipulates damping history to appear more needy
- **Detection Method**: Verify damping factors match historical over-allocation patterns
- **Privacy Impact**: Requires visibility into damping history
- **Conclusion:** Private damping enables **strategic manipulation** ??

### Scenario 3: **Profiling & Surveillance**
- **Attack**: Observer analyzes need patterns to identify vulnerable individuals/groups
- **Mitigation**: Hide exact need quantities, use range proofs
- **Trade-off**: Reduces allocation precision, breaks convergence metrics
- **Conclusion:** Privacy vs. efficiency trade-off is **fundamental** ??

### Scenario 4: **Capacity Hoarding**
- **Attack**: Provider falsely claims zero capacity to avoid contribution
- **Detection Method**: Compare declared capacity to historical patterns
- **Privacy Impact**: Requires visibility into capacity slot quantities
- **Conclusion:** Private capacity enables **free-riding** ?

---

## Recommended Privacy Architecture

### **Phase 1: Low-Hanging Fruit (Immediate)**
1. ? **Use 2PC for Mutual Recognition Computation**
   - Protects individual recognition weights
   - Publishes only `MR(A,B)` result
   - Minimal overhead, high privacy gain

2. ? **Commit-Reveal for Damping Factors**
   - Hide historical over-allocation data
   - Prove factor is in valid range with ZK proof
   - Prevents behavioral profiling

3. ? **Differential Privacy for Convergence Metrics**
   - Add calibrated noise to aggregate statistics
   - Preserves individual-level privacy
   - Maintains approximate system health monitoring

4. ? **Threshold-Based Need Visibility**
   - Hide needs below vulnerability threshold
   - Public visibility for large-scale coordination needs
   - Simple, no crypto required

### **Phase 2: Advanced Cryptography (Research)**
1. ?? **Homomorphic Encryption for Need Aggregation**
   - Research target: 10x overhead (currently 100x+)
   - Enables private allocation computation
   - Requires significant R&D investment

2. ?? **Private Information Retrieval for Index Lookups**
   - Research target: 10x bandwidth overhead (currently 1000x)
   - Protects location/time pattern privacy
   - May require novel PIR schemes

3. ?? **Verifiable Computation for Allocation Proofs**
   - Providers generate ZK proof that allocation is correct
   - Recipients verify without seeing other recipients' data
   - Enables private allocations with public auditability

### **Phase 3: Governance & Social Norms (Long-Term)**
1. ? **Community-Based Privacy Norms**
   - Establish guidelines for what data to share publicly
   - Voluntary privacy levels (e.g., "I share aggregates, not details")
   - Build trust through transparency culture, not forced visibility

2. ? **Federated Privacy Zones**
   - Different sub-networks have different privacy policies
   - High-trust communities: more transparency
   - Low-trust / vulnerable populations: more privacy
   - Enables pluralistic privacy models

---

## Feasibility Matrix

| Privacy Enhancement | Feasibility | Performance Impact | Privacy Gain | Implementation Complexity |
|---------------------|-------------|-------------------|--------------|--------------------------|
| 2PC for MR | ? High | <10% overhead | ?? High | Low (2-3 weeks) |
| Range proofs for damping | ? High | <5% overhead | ?? Medium | Low (1-2 weeks) |
| Differential privacy for metrics | ? High | <1% overhead | ?? Medium | Low (1 week) |
| Threshold-based visibility | ? High | 0% overhead | ?? Medium | Very Low (days) |
| Homomorphic allocation | ?? Medium | 10-100x overhead | ?? High | High (6+ months) |
| PIR for indexes | ?? Medium | 100-1000x bandwidth | ?? High | High (6+ months) |
| Private recipient keys | ?? Medium | 20-50% overhead | ?? High | Medium (2-3 months) |
| Full ZK allocation proof | ? Low | 100x+ overhead | ???? Very High | Very High (1+ years) |

---

## Conclusions

### **What MUST be Public:**
1. ? Provider available capacity (per type)
2. ? Mutual recognition values `MR(A,B)`
3. ? Need type IDs (categorical matching)
4. ? Slot compatibility constraints (time/location)
5. ? ITC causality stamps

**Rationale:** These are fundamental to the algorithm's correctness and cannot be hidden without breaking core functionality.

### **What CAN be Private:**
1. ?? Exact need quantities (use ranges + ZK proofs)
2. ?? Individual recognition weights (use 2PC, publish only MR)
3. ?? Damping history (commit to factors, hide history)
4. ?? Allocation slot internal IDs (pseudonymize)
5. ?? (Partial) Recipient identities in allocations

**Rationale:** These enhance privacy without catastrophically breaking the algorithm, though with trade-offs in transparency and efficiency.

### **Critical Trade-off:**
**Privacy ?? Convergence Monitoring**

If need quantities are fully hidden, the system cannot:
- Detect divergence (needs growing instead of shrinking)
- Estimate time to convergence
- Identify stuck participants
- Monitor fairness and equity

**Recommendation:** Use **differential privacy** or **secure aggregation** to compute noisy global statistics. This preserves approximate monitoring while protecting individual-level data.

### **Security vs. Privacy Tension:**
- **Transparency helps security** (Sybil detection, gaming prevention, capacity verification)
- **Privacy helps vulnerable populations** (protection from profiling, surveillance)
- **Resolution:** Multi-tier model with **privacy options** and **voluntary disclosure**

---

## Next Steps

1. **Prototype 2PC Mutual Recognition** - Highest privacy gain, lowest overhead
2. **Implement Differential Privacy for Metrics** - Essential for monitoring with privacy
3. **Design Privacy Policy Framework** - Let users choose privacy levels
4. **Research Homomorphic Allocation** - Long-term goal for full privacy
5. **Security Audit** - Ensure privacy enhancements don't introduce vulnerabilities

---

## References

- **Secure Two-Party Computation**: [Yao's Garbled Circuits](https://en.wikipedia.org/wiki/Garbled_circuit)
- **Homomorphic Encryption**: [SEAL Library](https://github.com/microsoft/SEAL)
- **Differential Privacy**: [Dwork & Roth, 2014](https://www.cis.upenn.edu/~aaroth/Papers/privacybook.pdf)
- **Zero-Knowledge Proofs**: [ZK-SNARKs Overview](https://z.cash/technology/zksnarks/)
- **Private Information Retrieval**: [PIR Survey](https://www.cs.umd.edu/~gasarch/TOPICS/pir/pir.html)

---

**End of Analysis**
