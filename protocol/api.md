# Protocol API Endpoints

The API uses DIDs (Decentralized Identifiers) for tree ownership and access control. Each tree is owned by a DID, and a DID can own multiple trees. Access is granted based on DID verification and tree-level contributor relationships.

## Authentication

All requests must include a DID in the `Authorization` header:

```
Authorization: Bearer <did>
```

## Access Control

- Tree owners (DID matches ownerId) have full read/write access to their trees
- Contributors (tree IDs listed in node's contributors) have read access to specific nodes
- Nodes with mutual fulfillment relationships have read access to relevant nodes

1. Tree Management Endpoints:

```
POST /trees
- Create a new root node
- Parameters:
  - ownerId: string (DID of the tree owner)
  - id: string (unique identifier for the tree)
  - name: string
  - points: number
  - contributors: string[] (list of tree IDs that can contribute to this node)
  - manualFulfillment: number? (optional, between 0 and 1)

GET /trees/{treeId}
- Get a specific tree by ID
- Returns the tree structure with all nodes
- Access: Tree owner

POST /trees/{treeId}/children
- Add a child node to an existing tree
- Parameters:
  - name: string
  - points: number
  - contributors: string[] (list of tree IDs that can contribute to this node)
  - manualFulfillment: number? (optional, between 0 and 1)
- Access: Tree owner only
```

2. Capacity Management Endpoints:

```
POST /trees/{treeId}/capacities
- Add a new capacity to a node
- Parameters:
  - capacityId: string
  - capacityName: string
  - quantity: number
  - unit: string
  - shareDepth: number
  - expanded: boolean
  - coordinates: {
      locationType: "Undefined" | "LiveLocation" | "Specific"
      allDay: boolean
      recurrence?: string
      customRecurrence?: {
        repeatEvery: number
        repeatUnit: "Days" | "Weeks" | "Months" | "Years"
        recurrenceEnd: {
          type: "Never" | "EndsOn" | "EndsAfter"
          value?: string | number
        }
      }
      startDate: string (ISO date)
      startTime: string (ISO time)
      endDate: string (ISO date)
      endTime: string (ISO time)
      timeZone: string
    }
  - maxDivisibility: {
      naturalDiv: number
      percentageDiv: number
    }
  - hiddenUntilRequestAccepted: boolean
- Access: Tree owner only

POST /trees/{treeId}/capacity-shares
- Add a share in another node's capacity
- Parameters:
  - shareId: string
  - targetCapacityId: string
  - sharePercentage: number
- Access: Tree owner only
```

3. Calculation Endpoints:

```
GET /trees/{treeId}/weight
- Get the weight of a node
- Returns: number (float between 0 and 1)
- Access: Tree owner

GET /trees/{treeId}/fulfillment
- Get the fulfillment value of a node
- Returns: number (float between 0 and 1)
- Access: Tree owner

GET /trees/{treeId}/mutual-fulfillment/{otherTreeId}
- Get mutual fulfillment between two nodes
- Returns: number (float between 0 and 1)
- Access: Tree owner

GET /trees/{treeId}/provider-shares
- Get provider-centric shares for a node
- Parameters:
  - depth: number
- Returns: Map<string, number> (normalized shares)
- Access: Tree owner

GET /trees/{treeId}/capacity-shares/{capacityId}
- Get a receiver's share from a specific capacity provider
- Parameters:
  - maxDepth: number
- Returns: number (float between 0 and 1)
- Access: Tree owner
```

4. Navigation Endpoints:

```
GET /trees/{treeId}/path
- Get the current path from root
- Returns: string[] (array of node IDs)
- Access: Tree owner

GET /trees/{treeId}/children
- Get all child nodes
- Returns: TreeZipper[]
- Access: Tree owner

GET /trees/{treeId}/descendants
- Get all descendant nodes
- Returns: TreeZipper[]
- Access: Tree owner
```

5. Cache Management Endpoints:

```
GET /trees/{treeId}/cache/stats
- Get cache statistics for a node
- Returns: {
    hits: number
    misses: number
  }
- Access: Tree owner only

POST /trees/{treeId}/cache/clear
- Clear the cache for a node
- Access: Tree owner only
```

## Error Responses

```
401 Unauthorized
- When the provided DID is invalid or missing

403 Forbidden
- When the DID doesn't have permission to access the requested resource
- When the tree is not listed as a contributor for the requested node

404 Not Found
- When the tree or node doesn't exist
```

The API follows RESTful principles and uses JSON for request/response bodies. All numeric values are properly typed (integers for points and quantities, floats for percentages and weights).