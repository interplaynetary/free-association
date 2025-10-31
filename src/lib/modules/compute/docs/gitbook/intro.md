# Overview

## Storage
- IndexedDB in the browser (no need for a download)
- localStorage
- Radix : Save to Disk
- Specs Max size etc.

## User Space
- Encrypted (signed public/private key pair)
- A tree

### General structure:
- /programs
    - /active
        programHash/program
        ...
    - /inactive
        programHash/program
        ...
    - /subscribed
        /peerPubKey/path
        /peerPubKey/programHash/outputs
        ...
    ...

- /state
    /programHash/outputs
    ...
- /replication
    /peerPubKey/peerPubKeyEncryptedData
    ...

(Currently in holster replication is outside the user-space, but we want it to be stored in the user space)

### Subscriptions

### Reactive computations
