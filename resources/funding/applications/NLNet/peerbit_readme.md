# Free Association Peerbit Integration

## Overview

This implementation integrates the Free Association mutual contribution model with Peerbit's decentralized data storage framework. Peerbit provides a powerful way to implement our mutually-fulfilling resource distribution model where data persists across the network at exactly those parts of the network topology that have a vested interest in a node's well-being.

The key feature of this integration is that storage and compute capacity are distributed according to mutual contribution scores. This means:

1. Your data persists across the network in places that have a direct interest in your well-being
2. The network becomes a "distributed constantly repartitioning communist computer"
3. Resources scale with transitive-mutual-contribution rather than fixed allotments that are bought or sold

## Replication Strategy

Peerbit allows us to define custom replication domains based on any property. For our implementation, we use several domain mapping strategies:

**Mutual Fulfillment**: Root nodes are replicated based on their cumulative mutual fulfillment score in the network. Nodes with higher scores get higher replication priority.

These strategies ensure that the most valuable and mutually-beneficial resources get the highest replication priority across the network.

## Dynamic Repartitioning

The system continuously updates mutual fulfillment scores between nodes, which then directly impacts:

1. How much of a capacity a node can access
2. Where in the network a node's data is replicated
3. The priority level of that replication

As mutual fulfillment scores change over time (based on ongoing contribution relationships), the replication domains shift automatically. This creates an adaptive, self-optimizing network that privileges mutual-contribution over fixed allocations.

## Key Benefits

- **Antifragile Data Storage**: Your data is replicated in the most efficient and resilient parts of the network topology - with those who value it most
- **Organic Scaling**: Digital resources grow or shrink naturally based on their contribution to the network's "aliveness"
- **No Centralized Control**: Resource allocation emerges from mutual-recognition of contribution, not from a central authority
- **Automatic Pruning**: "Unhealthy" data (not contributing to aliveness) decreases over time in storage/compute capacity
