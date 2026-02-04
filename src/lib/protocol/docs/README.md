Here is the outline of the new Decoupled Architecture.

The system is now split into three distinct "Pillars" (Domains) that interact through well-defined layers.

1. The Three Domains (Data Schemas)
These files define the shape of the data. They don't know about each other.

Physics (resources.ts): Defines what exists. (Needs, Capacities, Slots).
Trust (recognition.ts): Defines who is trusted. (Social Graph, Recognition Weights).
Logic (allocation.ts): Defines what happens. (Flows, Allocations, Solvers).

2. The Logic Layer (Pure Functions)
These modules contain the algorithms. They now operate on Interfaces, not the monolithic Commitment.

ipf-core.ts: The "Physics Engine". It calculates flow seeds ($K_{pr}$) and priorities. It asks generic questions like "Who owns this slot?" via the ResourceOwner interface.

solver.ts: The "Solver". It runs the iterative loop to compute scaling factors ($x_p, y_r$). It imports types from the domains above but doesn't manage state itself.

3. The State Layer (Svelte Stores)
This is where the application lives. It acts as the glue.

Granular Stores: myResourcesStore, myRecognitionTreeStore, myAllocationStateStore. Each manages one domain independently.

Reactive Engine (allocation.svelte.ts): Subscribes to the granular stores -> Runs the Logic Layer -> Updates myAllocationStateStore.

4. The Network Layer (Integration)
myCommitmentStore (Derived): This is the Compatibility Layer. It automatically gathers data from the granular stores and bundles it into the legacy "Commitment" object.

commitmentPublisher: Watches the derived commitment and saves it to the network.
Visualization
mermaid
graph TD
    classDef domain fill:#f9f,stroke:#333,stroke-width:2px;
    classDef logic fill:#ccf,stroke:#333,stroke-width:1px;
    classDef store fill:#dfd,stroke:#333,stroke-width:1px;
    classDef net fill:#ffc,stroke:#333,stroke-width:1px;
    subgraph Domains [Data Definitions]
        RES(resources.ts):::domain
        REC(recognition.ts):::domain
        ALL(allocation.ts):::domain
    end
    subgraph State [Reactive State Stores]
        S_RES(myResourcesStore):::store
        S_REC(myRecognitionTreeStore):::store
        S_ALL(myAllocationStateStore):::store
    end
    subgraph Logic [Allocation Engine]
        IPF(ipf-core.ts):::logic
        SOLVER(solver.ts):::logic
    end
    subgraph Network [Network / Legacy]
        COMMIT(myCommitmentStore):::net
        PUB(commitmentPublisher):::net
    end
    %% Relationships
    RES --> S_RES
    REC --> S_REC
    ALL --> S_ALL
    S_RES & S_REC & S_ALL --> SOLVER
    S_RES & S_REC & S_ALL --> COMMIT
    
    SOLVER --> IPF
    SOLVER -- Updates --> S_ALL
    COMMIT --> PUB
    PUB -- Syncs --> P2P(Mesh / IPFS)
Key Benefit: You can now create a new experimental solver in docs/experimental/ that listens to myResourcesStore but calculates allocations completely differently (e.g., using an auction mechanism), without breaking the network protocol or the existing UI.