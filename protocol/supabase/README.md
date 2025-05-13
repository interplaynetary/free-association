# Enhanced Database Design for Free Association

This directory contains an elegant database schema for the Free Association project using Supabase, optimized for efficient queries while maintaining flexibility.

## Setup Instructions

1. Create a new Supabase project at [database.new](https://database.new)

2. Copy your project URL and anon key from the Supabase dashboard

3. Create a `.env.local` file in your project root with the following content:

   ```
   VITE_SUPABASE_URL=your-project-url
   VITE_SUPABASE_ANON_KEY=your-anon-key
   ```

4. Run the migration SQL in the Supabase SQL Editor:
   - Navigate to the SQL Editor in your Supabase dashboard
   - Execute `migrations/schema.sql` to set up your database schema

## Database Architecture

The database schema is designed to be both elegant and performant, with specialized indexes and helper functions that make complex tree operations efficient.

### Core Tables

1. **Users**: User information with settings storage

   - Basic user attributes such as ID and name
   - JSONB field for flexible settings storage
   - Reference to their root node

2. **Nodes**: The core entity that represents items in the tree structure

   - Contains tree structure relationships via parent_id
   - Stores contributors directly as a string array
   - Maintains points and manual fulfillment values
   - Uses GIN indexes for efficient querying

3. **Capacities**: Resources that can be shared

   - Contains all capacity-related information
   - Stores space-time coordinates as structured JSONB
   - Maintains sharing information as JSONB
   - Uses JSON path operators for better query performance

4. **Calculation Cache**: Optimizes performance for expensive calculations
   - Stores computed values for weight, fulfillment, etc.
   - Supports different value types (float, int, string arrays, maps)
   - Automatically invalidated when related nodes change

### Helper SQL Functions

The schema includes built-in SQL functions that make tree traversal more efficient:

1. **get_descendants(node_id)**: Recursively finds all descendant nodes

   - Returns IDs, names, and depth from the specified node
   - Much faster than recursive application-level queries

2. **get_ancestors(node_id)**: Recursively finds all ancestor nodes
   - Returns IDs, names, and depth from the root
   - Optimized for traversing upward in the tree

### Design Principles

- **Balance of Server/Client Calculation**: While most business logic happens client-side, the schema provides caching and helper functions to optimize performance
- **Proper Constraints**: Foreign keys with appropriate ON DELETE behaviors and CHECK constraints for data integrity
- **Efficient Indexing**: GIN indexes for arrays and JSONB, trigram indexes for name search
- **Flexibility with Structure**: Uses native array types and structured JSONB objects to provide both flexibility and queryability

## Data Structure Examples

### Node Contributors (Array)

```
['user1', 'user2', 'user3']
```

### Capacity Coordinates (JSONB)

```json
{
	"locationType": "specific",
	"allDay": true,
	"startDate": "2023-01-01",
	"endDate": "2023-12-31",
	"timezone": "UTC",
	"recurrence": null,
	"customRecurrence": null
}
```

### Capacity Shares (JSONB)

```json
{
	"user1": {
		"sharePercentage": 0.5,
		"computedQuantity": 5
	},
	"user2": {
		"sharePercentage": 0.25,
		"computedQuantity": 2
	}
}
```

### Calculation Cache Example

```sql
SELECT * FROM calculation_cache
WHERE node_id = 'alice_node'
AND cache_key = 'alice_node_weight';
```

## Implementation Notes

- Tree traversal is accelerated using SQL recursive CTEs in the helper functions
- Calculations can be cached server-side but are primarily performed client-side
- The schema uses a hybrid approach that allows for both flexibility and performance
- PostgreSQL features like array types and GIN indices are used to optimize typical query patterns
