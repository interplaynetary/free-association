graph TD
    A[Minimal Push Server] -->|Generic wake-up signal| B[Browser Push Service]
    B -->|Push event| C[Service Worker]
    
    C -->|Wakes up & connects| D[Gun.js P2P Network]
    
    D -->|Subscribe to user paths| E[Gun.js Data Sync]
    E -->|New messages| F[Process & Store in IndexedDB]
    
    F --> G{Filter Messages}
    G -->|Check notification rules| H[Stored User Preferences]
    
    
    G -->|Should notify?| I{Notification Decision}
    H --> I
    
    I -->|Yes| J[Show Notification]
    I -->|No| K[Store silently]
    
    J --> L[User sees notification]
    K --> M[Available when app opens]
    
    subgraph "Client Device"
        C
        E
        F
        G
        H
        I
        J
        K
    end
    
    subgraph "Distributed P2P Network"
        D
    end
    
    subgraph "Minimal Infrastructure"
        A
        B
    end
    
    style C fill:#e1f5fe
    style D fill:#f3e5f5
    style H fill:#f3e5f5
    style I fill:#fff3e0