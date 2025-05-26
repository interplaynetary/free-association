# Capacity Request System

The Free Association Protocol includes a comprehensive request system that allows mutual contributors to request access to capacities with varying levels of specificity. This system respects the space-time location properties of capacities and the `hidden_until_request_accepted` flag.

## Overview

Capacities can be requested in three different ways:

1. **Basic Request** - Simple request with no additional parameters
2. **Quantity/Percentage Request** - Request for a specific amount or percentage
3. **Location Request** - Request for specific space-time coordinates

## Request Types

### 1. Basic Request

The simplest form of request where a mutual contributor asks for access to a capacity without specifying quantity, percentage, or location details.

```typescript
const basicRequest = createBasicRequest(
    capacityId: string,
    requesterId: string,
    message?: string,
    expiresInHours?: number
);
```

**Example:**

```typescript
const request = createBasicRequest(
	'programming-capacity-1',
	'bob-user-id',
	'Hi Alice! Could I get some programming time for our shared project?',
	72 // Expires in 72 hours
);
```

### 2. Quantity Request

Request for a specific natural number quantity of the capacity.

```typescript
const quantityRequest = createQuantityRequest(
    capacityId: string,
    requesterId: string,
    requestedQuantity: number,
    message?: string,
    expiresInHours?: number
);
```

**Example:**

```typescript
const request = createQuantityRequest(
	'programming-capacity-1',
	'charlie-user-id',
	8, // Requesting exactly 8 hours
	'Need exactly 8 hours for the database migration task',
	48
);
```

### 3. Percentage Request

Request for a specific percentage of the total capacity.

```typescript
const percentageRequest = createPercentageRequest(
    capacityId: string,
    requesterId: string,
    requestedPercentage: number, // 0.0 to 1.0
    message?: string,
    expiresInHours?: number
);
```

**Example:**

```typescript
const request = createPercentageRequest(
	'programming-capacity-1',
	'bob-user-id',
	0.25, // Requesting 25% of capacity
	'Would like 25% of your programming capacity for frontend work',
	24
);
```

### 4. Location Request

Request for specific space-time coordinates, useful when the capacity has location and timing constraints.

```typescript
const locationRequest = createLocationRequest(
    capacityId: string,
    requesterId: string,
    locationDetails: {
        location_type?: string;
        start_date?: string;
        start_time?: string;
        end_date?: string;
        end_time?: string;
        time_zone?: string;
    },
    message?: string,
    expiresInHours?: number
);
```

**Example:**

```typescript
const request = createLocationRequest(
	'meeting-room-capacity',
	'charlie-user-id',
	{
		location_type: 'office',
		start_date: '2024-01-15',
		start_time: '10:00',
		end_date: '2024-01-15',
		end_time: '14:00',
		time_zone: 'UTC'
	},
	'Need pair programming session in the office on Jan 15th',
	96
);
```

## Request Lifecycle

### 1. Request Creation

Requests are created using the appropriate creation function and added to the request collection:

```typescript
const request = createBasicRequest(capacityId, requesterId, message, expiresInHours);
addRequest(requests, request);
```

### 2. Request Validation

Before processing, requests are validated for:

- **Eligibility**: Can the requester make requests for this capacity?
- **Validity**: Is the request still pending and not expired?
- **Fulfillability**: Can the capacity fulfill this specific request?

```typescript
// Check if user can make requests
const eligibility = canMakeRequest(requesterId, capacity, provider, nodesMap);

// Check if request is valid
const isValid = isRequestValid(request);

// Check if request can be fulfilled
const fulfillment = canFulfillRequest(request, capacity);
```

### 3. Request Processing

Capacity owners can accept or reject requests:

```typescript
// Accept a request
const result = acceptRequest(request, capacity, provider, nodesMap);
if (result.success) {
	console.log(`Created share: ${result.share.computed_quantity} ${capacity.unit}`);
}

// Reject a request
rejectRequest(request, 'Reason for rejection');
```

### 4. Request Expiration

Requests can have expiration times. Expired requests are automatically marked:

```typescript
markExpiredRequests(requests);
```

## Request Status

Requests have four possible statuses:

- **pending**: Awaiting response from capacity owner
- **accepted**: Request approved and capacity share created
- **rejected**: Request denied by capacity owner
- **expired**: Request timed out before response

## Eligibility Rules

For a user to make a request for a capacity, they must:

1. **Not be the capacity owner** - Can't request your own capacity
2. **Be a mutual contributor** - Must have a share in the provider's general fulfillment
3. **Pass capacity filters** - Must meet any filter rules applied to the capacity

## Validation Rules

### Quantity Requests

- Requested quantity must not exceed total capacity
- Must respect `max_natural_div` constraints
- Must respect `max_percentage_div` when converted to percentage

### Percentage Requests

- Requested percentage must not exceed `max_percentage_div`
- Must be between 0.0 and 1.0

### Location Requests

- Generally fulfillable unless specific location/time conflicts exist
- Additional validation can be implemented for scheduling conflicts

## Hidden Capacities

Capacities with `hidden_until_request_accepted: true` are only visible to mutual contributors after they make a request and it gets accepted. This allows for:

- **Privacy**: Sensitive capacities remain hidden
- **Selective sharing**: Only revealed to those who actively request access
- **Trust-based access**: Requires explicit request and approval

## Utility Functions

### Request Management

```typescript
// Get pending requests for a capacity
const pendingRequests = getPendingRequestsForCapacity(capacityId, requests);

// Get all requests by a user
const userRequests = getRequestsByRequester(userId, requests);

// Get requests for capacities owned by a user
const ownerRequests = getRequestsForCapacityOwner(ownerId, requests, capacities);

// Remove a request
const removed = removeRequest(requests, requestId);
```

### Request Statistics

```typescript
const stats = getRequestStats(capacityId, requests);
console.log(`Total: ${stats.total}, Pending: ${stats.pending}, Accepted: ${stats.accepted}`);
```

## Integration with Mutual Fulfillment

The request system integrates seamlessly with the mutual fulfillment protocol:

1. **Share Calculation**: For basic and location requests, shares are calculated using the existing `providerShares()` function
2. **Filter Application**: Capacity filters are applied to determine eligibility
3. **Automatic Share Creation**: Accepted requests automatically create `CapacityShare` objects
4. **Recipient Share Updates**: The `recipient_shares` map is updated when requests are accepted

## Example Usage

See the `demonstrateCapacityRequests()` function in `src/lib/protocol.ts` for a complete working example that shows:

- Creating all four types of requests
- Validating request eligibility
- Processing requests (accept/reject)
- Viewing final capacity state
- Request statistics

## Best Practices

1. **Set Reasonable Expiration Times**: Prevent stale requests from accumulating
2. **Provide Clear Messages**: Help capacity owners understand the request context
3. **Respect Capacity Constraints**: Check divisibility and percentage limits before requesting
4. **Handle Rejections Gracefully**: Provide feedback and alternative options
5. **Monitor Request Statistics**: Track patterns to optimize capacity allocation

## Future Enhancements

Potential improvements to the request system:

- **Recurring Requests**: Support for ongoing/repeated access requests
- **Conditional Requests**: Requests that depend on other conditions being met
- **Request Negotiation**: Allow counter-offers and request modifications
- **Batch Requests**: Request multiple capacities in a single transaction
- **Priority Queuing**: Handle competing requests with priority systems
