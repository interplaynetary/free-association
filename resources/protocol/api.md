# Protocol API Endpoints

The API uses Auth.js for tree ownership and access control. Each tree is owned by a User. Access is granted based on User verification and tree-level contributor relationships.

All calculations will be done on the client-side
The server should only serve as a place to get/set a User's nodes, trees, get/set SOGF, ProviderShares, and Capacities

## Access Control

- Tree owners (User matches ownerId) have full read/write access to their forest
- Contributors (tree IDs listed in node's contributors) have read access to specific nodes
- Nodes with mutual fulfillment relationships have read access to relevant nodes

- Note: treeId is just a nodeId that is the root of a tree. and the treeid = the userID

1. Tree Management Endpoints:

```
POST /
- Create a new root node
- Parameters:
  - ownerId: string (Id of the tree owner)
  - id: string (unique identifier for the tree)
  - name: string
  - points: number
  - contributors: string[] (list of tree IDs that can contribute to this node)
  - manualFulfillment: number? (optional, between 0 and 1)

GET /forest
- Get all roots
- Returns all the root nodeIds (all treeIds)
- Access: Public

GET /{nodeId}
- Get a specific tree/node by ID
- Returns the tree structure with all nodes
- Access: Tree owner

2. Capacity Management Endpoints:

```

POST /{treeId}/capacities

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

```

3. Calculation Endpoints:

```

GET /trees/{treeId}/shares-of-general-fulfillment

- Get SOGF Map for a tree
- Parameters:
  - depth: number
- Returns: Map<string, number> (normalized shares)
- Access: Tree owner

GET /trees/{treeId}/provider-shares

- Get provider-centric shares for a node
- Parameters:
  - depth: number
- Returns: Map<string, number> (normalized shares)
- Access: Tree owner

```

## Error Responses

```

401 Unauthorized

- When the provided User is invalid or missing

403 Forbidden

- When the User doesn't have permission to access the requested resource
- When the tree is not listed as a contributor for the requested node

404 Not Found

- When the tree or node doesn't exist

```

The API follows RESTful principles and uses JSON for request/response bodies. All numeric values are properly typed (integers for points and quantities, floats for percentages and weights).

// we will want to use svelte-kit routes +
```
