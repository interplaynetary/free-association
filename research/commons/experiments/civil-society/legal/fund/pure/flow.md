sequenceDiagram
    participant User as Users
    participant Mesh as Mesh Network
    participant Server as Server Instance
    participant Blockchain as Blockchain
    participant Swiss as Swiss Banking API
    participant KYC as KYC Provider
    participant Wallet as Crypto Wallets

    Note over User,Mesh: Account Setup & Identity
    User->>Mesh: Create account (public/private key pair)
    User->>Mesh: Associate crypto wallet address
    User->>Mesh: Associate bank account details
    User->>Mesh: Provide KYC details
    Mesh->>Mesh: Store user identity & associations

    Note over Server,Mesh: Weekly Recognition Cycle
    User->>Mesh: Submit recognition data (100% distributed)
    Server->>Mesh: Read all recognition relationships
    Server->>Server: Compute mutual recognition for all pairs
    Server->>Server: Calculate MRS for each participant
    Server->>Server: Calculate network average MRS
    Server->>Server: Calculate MRD for each participant
    Server->>Server: Determine membership set (MRD ≥ 0.5)
    Server->>Server: Compute collective-recognition-shares<br/>from membership set
    
    Server->>Mesh: Publish membership list + MRD scores
    Server->>Blockchain: Publish membership list + MRD scores
    
    Note over Server,Mesh: Daily Needs Declaration
    User->>Mesh: Declare needs (e.g., "$1000 for infrastructure")
    Server->>Mesh: Read all open needs from members

    Note over Server,Swiss: Balance Verification & Capacity Declaration
    Server->>Wallet: Query crypto wallet balance
    Wallet-->>Server: Return balance (e.g., 2.5 ETH)
    Server->>Swiss: Query bank account balance via API
    Swiss-->>Server: Return balance (e.g., CHF 100,000)
    
    Server->>Server: Determine total available capacity<br/>(crypto + fiat balances)
    Server->>Server: Generate automatic capacity declaration<br/>Set: All current members<br/>Amount: Total available balance
    
    Server->>Mesh: Post capacity to server's own Mesh instance
    Note over Server,Mesh: Server is itself a user on Mesh<br/>with its own capacity declarations

    Note over Server,KYC: Compliance Filtering & Validation
    Server->>Server: Prepare membership list with<br/>initial allocation amounts<br/>(based on collective-recognition-shares)
    
    Server->>KYC: Send membership list for validation
    KYC->>KYC: Verify KYC status for each member
    KYC->>KYC: Screen against sanctions lists (OFAC, UN, EU)
    KYC->>KYC: Determine jurisdiction limits
    KYC->>KYC: Calculate Filter(Member) for each:<br/>$0 (cannot receive)<br/>$X (capped amount)<br/>Unlimited (no restrictions)
    KYC-->>Server: Return validated list with caps
    
    Server->>Server: Apply filters to allocations:<br/>Allocation = min(Share × Capacity,<br/>Filter(Member), Member-Need)
    Server->>Server: Redistribute from filtered members<br/>to compliant members by recognition share
    Server->>Server: Generate final KYC-filtered<br/>capacity distribution list
    
    Server->>Mesh: Publish allocation decisions with rationale
    Server->>Blockchain: Publish allocation decisions with rationale

    Note over Server,User: Execute Transfers (Multi-Currency)
    loop For each allocation
        alt Crypto payment preferred
            Server->>Wallet: Execute crypto transfer<br/>to user's wallet address
            Wallet->>User: Receive crypto (ETH/USDC/etc.)
        else Bank payment preferred
            Server->>Swiss: Execute bank transfer via API<br/>to user's bank account
            Swiss->>User: Receive fiat (CHF/EUR/USD)
        end
        Server->>Mesh: Record transfer completion
        Server->>Blockchain: Record transfer completion
    end
    
    Server->>Server: Update need fulfillment status
    Server->>Mesh: Publish updated need status
    Server->>Blockchain: Publish updated need status

    Note over User,Mesh: External Provider Scenario (Optional)
    User->>Mesh: External provider declares capacity<br/>Set: {Alice, Bob, Charlie}, Amount: $50K
    Server->>Mesh: Read external provider capacity declaration
    Server->>Server: Compute collective-recognition-shares<br/>within provider's specified set
    Server->>KYC: Validate provider's member set
    KYC-->>Server: Return filters for provider's set
    Server->>Server: Apply union of filters:<br/>Effective-Filter = min(Provider-Filter, Server-Filter)
    Server->>Server: Generate allocation for external provider
    Server->>Mesh: Post allocation instructions to provider
    User->>Wallet: External provider executes transfers<br/>(or delegates to server)