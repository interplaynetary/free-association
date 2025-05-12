-- Free Association Database Schema
-- Elegant design for persistent data with improved types

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pg_trgm"; -- For better text search

------------------------------------------
-- CORE TABLES WITH IMPROVED STRUCTURE
------------------------------------------

-- Node table - stores tree structure with direct contributors
CREATE TABLE nodes (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  points INTEGER NOT NULL,
  parent_id TEXT REFERENCES nodes(id) ON DELETE CASCADE,
  manual_fulfillment FLOAT,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  -- Additional metadata
  metadata JSONB DEFAULT '{}'::jsonb,
  CONSTRAINT valid_fulfillment CHECK (manual_fulfillment IS NULL OR (manual_fulfillment >= 0 AND manual_fulfillment <= 1))
);

-- Create better indices
CREATE INDEX idx_nodes_parent ON nodes(parent_id);
CREATE INDEX idx_nodes_metadata ON nodes USING GIN (metadata);
CREATE INDEX idx_nodes_name_trgm ON nodes USING GIN (name gin_trgm_ops); -- For partial name search

-- Node contributors junction table for normalized many-to-many relationship
CREATE TABLE node_contributors (
  node_id TEXT NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
  contributor_id TEXT NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  PRIMARY KEY (node_id, contributor_id)
);

-- Create index for better performance
CREATE INDEX idx_node_contributors_node ON node_contributors(node_id);
CREATE INDEX idx_node_contributors_contributor ON node_contributors(contributor_id);

-- User table with better structure
CREATE TABLE users (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  root_node_id TEXT REFERENCES nodes(id) ON DELETE SET NULL, -- Set NULL if root node deleted
  settings JSONB DEFAULT '{}'::jsonb, -- Store user settings
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_users_root_node ON users(root_node_id);
CREATE INDEX idx_users_name_trgm ON users USING GIN (name gin_trgm_ops);

-- Capacity table with improved structure
CREATE TABLE capacities (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  quantity INTEGER NOT NULL,
  unit TEXT NOT NULL,
  owner_id TEXT REFERENCES nodes(id) ON DELETE CASCADE,
  -- Divisibility constraints
  natural_div INTEGER NOT NULL DEFAULT 1,
  percentage_div FLOAT NOT NULL DEFAULT 0.1,
  -- Sharing configuration
  share_depth INTEGER NOT NULL DEFAULT 2,
  hidden_until_request_accepted BOOLEAN DEFAULT FALSE,
  -- Space-time coordinates as JSONB - more detailed structure
  coordinates JSONB NOT NULL DEFAULT '{
    "locationType": "undefined", 
    "allDay": true, 
    "startDate": null, 
    "endDate": null, 
    "timezone": "UTC"
  }'::jsonb,
  -- Sharing information as JSONB with better structure
  shares JSONB DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  CONSTRAINT valid_divisibility CHECK (natural_div >= 1 AND percentage_div > 0 AND percentage_div <= 1)
);

CREATE INDEX idx_capacity_owner ON capacities(owner_id);
CREATE INDEX idx_capacities_coordinates ON capacities USING GIN (coordinates);
CREATE INDEX idx_capacities_shares ON capacities USING GIN (shares jsonb_path_ops);
CREATE INDEX idx_capacities_name_trgm ON capacities USING GIN (name gin_trgm_ops);

-- Cache table for storing calculation results
-- This improves performance by allowing server-side caching when needed
CREATE TABLE calculation_cache (
  id SERIAL PRIMARY KEY,
  node_id TEXT NOT NULL REFERENCES nodes(id) ON DELETE CASCADE,
  cache_key TEXT NOT NULL,
  cache_type TEXT NOT NULL, -- 'float', 'int', 'stringList', 'shareMap'
  numeric_value FLOAT, -- For float or int values
  text_array_value TEXT[], -- For string lists
  map_value JSONB, -- For ShareMap
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  CONSTRAINT unique_cache_entry UNIQUE (node_id, cache_key)
);

CREATE INDEX idx_cache_node ON calculation_cache(node_id);
CREATE INDEX idx_cache_key ON calculation_cache(cache_key);

-- Update timestamp trigger for created_at and updated_at
CREATE OR REPLACE FUNCTION trigger_update_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER set_timestamp_users
BEFORE UPDATE ON users
FOR EACH ROW EXECUTE FUNCTION trigger_update_timestamp();

CREATE TRIGGER set_timestamp_nodes
BEFORE UPDATE ON nodes
FOR EACH ROW EXECUTE FUNCTION trigger_update_timestamp();

CREATE TRIGGER set_timestamp_capacities
BEFORE UPDATE ON capacities
FOR EACH ROW EXECUTE FUNCTION trigger_update_timestamp();

CREATE TRIGGER set_timestamp_cache
BEFORE UPDATE ON calculation_cache
FOR EACH ROW EXECUTE FUNCTION trigger_update_timestamp();

CREATE TRIGGER set_timestamp_node_contributors
BEFORE UPDATE ON node_contributors
FOR EACH ROW EXECUTE FUNCTION trigger_update_timestamp();

------------------------------------------
-- SAMPLE DATA
------------------------------------------

-- Insert sample nodes without contributors array
INSERT INTO nodes (id, name, points, parent_id, metadata)
VALUES 
  ('alice_node', 'Alice Node', 100, NULL, '{}'),
  ('bob_node', 'Bob Node', 100, NULL, '{}'),
  ('charlie_node', 'Charlie Node', 100, NULL, '{}');

-- Insert sample users with links to their root nodes
INSERT INTO users (id, name, root_node_id)
VALUES 
  ('alice', 'Alice', 'alice_node'),
  ('bob', 'Bob', 'bob_node'),
  ('charlie', 'Charlie', 'charlie_node');

-- Insert node-contributor relationships
INSERT INTO node_contributors (node_id, contributor_id)
VALUES
  ('alice_node', 'bob_node'),
  ('alice_node', 'charlie_node'),
  ('bob_node', 'alice_node'),
  ('bob_node', 'charlie_node'),
  ('charlie_node', 'alice_node'),
  ('charlie_node', 'bob_node');

-- Insert child nodes without contributors array
INSERT INTO nodes (id, name, points, parent_id, metadata)
VALUES 
  ('alice_child', 'Alice Child', 30, 'alice_node', '{}'),
  ('bob_child', 'Bob Child', 40, 'bob_node', '{}'),
  ('charlie_child', 'Charlie Child', 50, 'charlie_node', '{}');

-- Insert node-contributor relationships for child nodes
INSERT INTO node_contributors (node_id, contributor_id)
VALUES
  ('alice_child', 'bob_node'),
  ('alice_child', 'charlie_node'),
  ('bob_child', 'alice_node'),
  ('bob_child', 'charlie_node'),
  ('charlie_child', 'alice_node'),
  ('charlie_child', 'bob_node');

-- Add sample capacities with more detailed coordinates
INSERT INTO capacities (
  id, name, quantity, unit, owner_id, 
  natural_div, percentage_div, share_depth, 
  coordinates, shares
)
VALUES 
  ('room1', 'Spare Room', 10, 'room', 'alice_node', 1, 0.1, 2, 
   '{
     "locationType": "specific", 
     "allDay": true, 
     "startDate": "2023-01-01", 
     "endDate": "2023-12-31", 
     "timezone": "UTC", 
     "recurrence": null,
     "customRecurrence": null
   }',
   '{"bob": {"sharePercentage": 0.5, "computedQuantity": 5}}'
  ),
  ('pie1', 'Apple Pie', 8, 'slices', 'bob_node', 1, 0.125, 3,
   '{
     "locationType": "specific", 
     "allDay": true, 
     "startDate": "2023-01-01", 
     "endDate": "2023-01-02", 
     "timezone": "UTC",
     "recurrence": null,
     "customRecurrence": null
   }',
   '{"charlie": {"sharePercentage": 0.25, "computedQuantity": 2}}'
  );

-- Add some cache entries for demonstration
INSERT INTO calculation_cache (node_id, cache_key, cache_type, numeric_value)
VALUES
  ('alice_node', 'alice_node_weight', 'float', 1.0),
  ('bob_node', 'bob_node_weight', 'float', 1.0),
  ('charlie_node', 'charlie_node_weight', 'float', 1.0),
  ('alice_child', 'alice_child_weight', 'float', 0.3),
  ('alice_node', 'alice_node_fulfillment', 'float', 0.75);

------------------------------------------
-- HELPER FUNCTIONS FOR TREE OPERATIONS
------------------------------------------

-- Get all descendants of a node
CREATE OR REPLACE FUNCTION get_descendants(node_id TEXT)
RETURNS TABLE (id TEXT, name TEXT, depth INT) AS $$
WITH RECURSIVE descendants AS (
  SELECT n.id, n.name, 1 AS depth
  FROM nodes n
  WHERE n.parent_id = node_id
  
  UNION ALL
  
  SELECT n.id, n.name, d.depth + 1
  FROM nodes n
  JOIN descendants d ON n.parent_id = d.id
)
SELECT * FROM descendants;
$$ LANGUAGE SQL;

-- Get all ancestors of a node
CREATE OR REPLACE FUNCTION get_ancestors(node_id TEXT)
RETURNS TABLE (id TEXT, name TEXT, depth INT) AS $$
WITH RECURSIVE ancestors AS (
  SELECT n.id, n.name, 1 AS depth, n.parent_id
  FROM nodes n
  WHERE n.id = node_id
  
  UNION ALL
  
  SELECT n.id, n.name, a.depth + 1, n.parent_id
  FROM nodes n
  JOIN ancestors a ON n.id = a.parent_id
  WHERE n.parent_id IS NOT NULL
)
SELECT id, name, depth FROM ancestors WHERE id != node_id;
$$ LANGUAGE SQL;

-- Get all contributors for a node
CREATE OR REPLACE FUNCTION get_node_contributors(node_id TEXT)
RETURNS TABLE (contributor_id TEXT, contributor_name TEXT) AS $$
SELECT n.id, n.name
FROM node_contributors nc
JOIN nodes n ON nc.contributor_id = n.id
WHERE nc.node_id = node_id;
$$ LANGUAGE SQL;

------------------------------------------
-- ROW LEVEL SECURITY
------------------------------------------

-- Enable RLS on main tables
ALTER TABLE users ENABLE ROW LEVEL SECURITY;
ALTER TABLE nodes ENABLE ROW LEVEL SECURITY;
ALTER TABLE capacities ENABLE ROW LEVEL SECURITY;
ALTER TABLE calculation_cache ENABLE ROW LEVEL SECURITY;
ALTER TABLE node_contributors ENABLE ROW LEVEL SECURITY;

-- Create RLS policies for public read access
CREATE POLICY "Public read access for users" 
ON users FOR SELECT USING (true);

CREATE POLICY "Public read access for nodes" 
ON nodes FOR SELECT USING (true);

CREATE POLICY "Public read access for capacities" 
ON capacities FOR SELECT USING (true);

CREATE POLICY "Public read access for cache" 
ON calculation_cache FOR SELECT USING (true);

CREATE POLICY "Public read access for node_contributors" 
ON node_contributors FOR SELECT USING (true); 