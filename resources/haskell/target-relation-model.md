The domain model for the provided code architecture revolves around a hierarchical system of players, trees, and nodes with resource sharing based on mutual contributions. Here's a structured outline:

---

### **Core Domain Entities**

1. **Player**
   - **Attributes**: 
     - Unique ID
     - Forest (collection of trees)
     - Shares (in others' capacities)
     - Cache for mutual fulfillment calculations
   - **Responsibilities**:
     - Own/manage trees and their nodes
     - Calculate shares in others' capacities
     - Track mutual fulfillment relationships

2. **Tree**
   - **Attributes**:
     - Hierarchical structure of nodes (via `TreeZipper` navigation)
     - Root node
   - **Relationships**:
     - Belongs to a `Player`
     - Nodes reference contributors via `TreeRef`

3. **Node**
   - **Attributes**:
     - ID, Name, Points (contribution value)
     - Children (sub-nodes)
     - Contributors (other nodes via `TreeRef`)
     - Manual fulfillment override
     - Capacities (resources offered)
     - Shares (in others' capacities)
     - Cache for performance optimization
   - **Key Operations**:
     - Calculate weight (proportional contribution)
     - Determine fulfillment (automatic or manual)
     - Manage capacity shares

4. **Capacity**
   - **Attributes**:
     - ID, Name, Quantity, Unit
     - Recurrence rules (time/location constraints)
     - Divisibility constraints (max share percentages/natural units)
   - **Relationships**:
     - Owned by a `Node`
     - Shared via `CapacityShare` entries in other nodes

5. **CapacityShare**
   - **Attributes**:
     - Target capacity (reference)
     - Share percentage
     - Computed quantity (enforcing divisibility rules)
   - **Relationships**:
     - Linked to a provider `Node`'s capacity

6. **TreeRef**
   - **Attributes**:
     - Player ID + Tree ID (globally unique reference)
   - **Purpose**:
     - Uniquely identify a node in another player's forest

---

### **Key Domain Concepts**

1. **Contribution System**
   - Nodes contribute to others' fulfillment via `TreeRef` references.
   - **Mutual Fulfillment**: Reciprocal contribution strength between two nodes, calculated as the minimum of their directional contributions.
   - **Weight**: A node's influence based on its points relative to siblings.

2. **Resource Sharing (Capacities)**
   - Nodes declare `Capacities` (e.g., physical resources, services).
   - Shares are distributed based on mutual fulfillment scores:
     - Direct shares (1st degree) from immediate contributors.
     - Transitive shares (nth degree) via provider chains.
   - Shares respect divisibility constraints (natural units/max percentages).

3. **Caching & Performance**
   - Nodes cache frequently computed values (weights, fulfillment, mutuals).
   - ProviderSharesCache optimizes share distribution calculations.

4. **Navigation & Modification**
   - **TreeZipper**: Functional data structure for safe tree traversal/modification.
   - Operations: Add/remove nodes, update capacities/shares.

---

### **Relationships Diagram**

```
Player 1--* Tree 
Tree 1--1 Node (Root)
Node *--* Node (Children)
Node *--* TreeRef (Contributors)
Node *--* Capacity (Owned Resources)
Node *--* CapacityShare (Shares in Others' Resources)
CapacityShare --> Capacity (Target)
TreeRef --> Node (Cross-Player Reference)
```

---

### **Key Workflows**

1. **Tree Navigation & Modification**
   - Players add/remove nodes, set manual fulfillment, or declare capacities.
   - TreeZipper ensures safe traversal and updates.

2. **Fulfillment Calculation**
   - Automatically computed for nodes without manual overrides:
     - Weighted sum of children's fulfillment.
   - Manual fulfillment blends contribution/non-contribution children.

3. **Capacity Sharing**
   - Shares are distributed using `providerShares`, considering:
     - Direct contributions (1st degree).
     - Transitive contributions (up to `shareDepth`).
   - Shares enforce divisibility rules (e.g., minimum slice sizes).

4. **Mutual Fulfillment Updates**
   - Triggered when contributors change.
   - Cached to avoid recomputation.

---

### **Example Scenario**
- **Alice** owns a `Capacity` (spare room). 
- **Bob** and **Charlie** contribute to Alice's nodes, influencing their `mutualFulfillment` scores. 
- Alice's `providerShares` distributes room access based on these scores, while respecting divisibility rules (max 10% per share). 
- Bob receives 50% of the room capacity due to high mutual fulfillment, while Charlie gets 30%.