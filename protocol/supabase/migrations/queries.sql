SELECT 
  n.id AS node_id,
  n.name AS node_name,
  n.points,
  n.manual_fulfillment,
  ARRAY_AGG(
    JSON_BUILD_OBJECT(
      'id', c.id,
      'name', c.name
    )
  ) AS contributors
FROM 
  nodes n
LEFT JOIN 
  node_contributors nc ON n.id = nc.node_id
LEFT JOIN 
  nodes c ON nc.contributor_id = c.id
WHERE 
  n.id = 'alice_node'  -- Replace with any node ID you want to query
GROUP BY 
  n.id, n.name, n.points, n.manual_fulfillment;


-- Identify all queries / all recursive queries (given a node-id) all that is needed for our calculations.

-- Enhanced get_descendants function that returns more node data
CREATE OR REPLACE FUNCTION get_descendants_with_data(node_id TEXT)
RETURNS TABLE (
  id TEXT, 
  name TEXT, 
  points INTEGER,
  parent_id TEXT,
  manual_fulfillment FLOAT,
  depth INTEGER
) AS $$
WITH RECURSIVE descendants AS (
  SELECT n.id, n.name, n.points, n.parent_id, n.manual_fulfillment, 1 AS depth
  FROM nodes n
  WHERE n.parent_id = node_id
  
  UNION ALL
  
  SELECT n.id, n.name, n.points, n.parent_id, n.manual_fulfillment, d.depth + 1
  FROM nodes n
  JOIN descendants d ON n.parent_id = d.id
)
SELECT * FROM descendants;
$$ LANGUAGE SQL;

-- Get all descendants including self (used in central.hs getAllDescendants)
CREATE OR REPLACE FUNCTION get_all_descendants(node_id TEXT)
RETURNS TABLE (
  id TEXT, 
  name TEXT, 
  points INTEGER,
  parent_id TEXT,
  manual_fulfillment FLOAT,
  depth INTEGER
) AS $$
WITH RECURSIVE all_descendants AS (
  SELECT n.id, n.name, n.points, n.parent_id, n.manual_fulfillment, 0 AS depth
  FROM nodes n
  WHERE n.id = node_id
  
  UNION ALL
  
  SELECT n.id, n.name, n.points, n.parent_id, n.manual_fulfillment, d.depth + 1
  FROM nodes n
  JOIN all_descendants d ON n.parent_id = d.id
)
SELECT * FROM all_descendants;
$$ LANGUAGE SQL;

-- Calculate weight of a node
CREATE OR REPLACE FUNCTION calculate_node_weight(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  parent_id TEXT;
  parent_weight FLOAT;
  current_points INTEGER;
  total_child_points INTEGER;
  node_weight FLOAT;
BEGIN
  -- Get node data and parent ID
  SELECT n.parent_id, n.points INTO parent_id, current_points 
  FROM nodes n 
  WHERE n.id = node_id;
  
  -- If root node (no parent), weight is 1
  IF parent_id IS NULL THEN
    RETURN 1.0;
  END IF;
  
  -- Get total points from all children of parent
  SELECT COALESCE(SUM(n.points), 0) INTO total_child_points
  FROM nodes n
  WHERE n.parent_id = parent_id;
  
  -- Get parent's weight (recursive call)
  SELECT calculate_node_weight(parent_id) INTO parent_weight;
  
  -- Calculate weight based on parent's weight and sibling points
  IF total_child_points = 0 THEN
    node_weight := 0;
  ELSE
    node_weight := (current_points::FLOAT / total_child_points) * parent_weight;
  END IF;
  
  RETURN node_weight;
END;
$$ LANGUAGE plpgsql;

-- Check if a node is a contribution node (has contributors and is not root)
CREATE OR REPLACE FUNCTION is_contribution_node(node_id TEXT)
RETURNS BOOLEAN AS $$
DECLARE
  has_contributors BOOLEAN;
  is_root BOOLEAN;
BEGIN
  -- Check if node has contributors
  SELECT EXISTS(
    SELECT 1 FROM node_contributors WHERE node_id = $1
  ) INTO has_contributors;
  
  -- Check if node is root (no parent)
  SELECT (parent_id IS NULL) INTO is_root
  FROM nodes WHERE id = node_id;
  
  -- A contribution node has contributors and is not root
  RETURN has_contributors AND NOT is_root;
END;
$$ LANGUAGE plpgsql;

