-- Implementing the functionality from centralized.hs using SQL functions
-- This file contains PostgreSQL functions to replicate the core functionality
-- of the original Haskell implementation with effective caching

-- ----------------------
-- -- Caching System --
-- ----------------------

-- Function to get a cached float value, or NULL if not in cache
CREATE OR REPLACE FUNCTION get_cached_float(node_id TEXT, cache_key TEXT)
RETURNS FLOAT AS $$
DECLARE
  result FLOAT;
BEGIN
  SELECT numeric_value INTO result
  FROM calculation_cache
  WHERE node_id = $1 AND cache_key = $2 AND cache_type = 'float';
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Function to get a cached string list, or NULL if not in cache
CREATE OR REPLACE FUNCTION get_cached_string_list(node_id TEXT, cache_key TEXT)
RETURNS TEXT[] AS $$
DECLARE
  result TEXT[];
BEGIN
  SELECT text_array_value INTO result
  FROM calculation_cache
  WHERE node_id = $1 AND cache_key = $2 AND cache_type = 'stringList';
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Function to get a cached map value, or NULL if not in cache
CREATE OR REPLACE FUNCTION get_cached_map(node_id TEXT, cache_key TEXT)
RETURNS JSONB AS $$
DECLARE
  result JSONB;
BEGIN
  SELECT map_value INTO result
  FROM calculation_cache
  WHERE node_id = $1 AND cache_key = $2 AND cache_type = 'shareMap';
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Function to store a float value in cache
CREATE OR REPLACE FUNCTION cache_float_value(node_id TEXT, cache_key TEXT, value FLOAT)
RETURNS VOID AS $$
BEGIN
  INSERT INTO calculation_cache (node_id, cache_key, cache_type, numeric_value)
  VALUES (node_id, cache_key, 'float', value)
  ON CONFLICT (node_id, cache_key) 
  DO UPDATE SET numeric_value = value, updated_at = NOW();
END;
$$ LANGUAGE plpgsql;

-- Function to store a string list in cache
CREATE OR REPLACE FUNCTION cache_string_list(node_id TEXT, cache_key TEXT, value TEXT[])
RETURNS VOID AS $$
BEGIN
  INSERT INTO calculation_cache (node_id, cache_key, cache_type, text_array_value)
  VALUES (node_id, cache_key, 'stringList', value)
  ON CONFLICT (node_id, cache_key) 
  DO UPDATE SET text_array_value = value, updated_at = NOW();
END;
$$ LANGUAGE plpgsql;

-- Function to store a map in cache
CREATE OR REPLACE FUNCTION cache_map_value(node_id TEXT, cache_key TEXT, value JSONB)
RETURNS VOID AS $$
BEGIN
  INSERT INTO calculation_cache (node_id, cache_key, cache_type, map_value)
  VALUES (node_id, cache_key, 'shareMap', value)
  ON CONFLICT (node_id, cache_key) 
  DO UPDATE SET map_value = value, updated_at = NOW();
END;
$$ LANGUAGE plpgsql;

-- -------------------------
-- -- Core Calculations --
-- -------------------------

-- Calculate total points from all children of a node
CREATE OR REPLACE FUNCTION total_child_points(node_id TEXT)
RETURNS INTEGER AS $$
DECLARE
  total INTEGER;
  cache_key TEXT := node_id || '_total_points';
  cached_value FLOAT;
BEGIN
  -- Check cache first
  cached_value := get_cached_float(node_id, cache_key);
  
  IF cached_value IS NOT NULL THEN
    RETURN cached_value::INTEGER;
  END IF;
  
  -- Calculate total points
  SELECT COALESCE(SUM(points), 0) INTO total
  FROM nodes
  WHERE parent_id = node_id;
  
  -- Store in cache
  PERFORM cache_float_value(node_id, cache_key, total::FLOAT);
  
  RETURN total;
END;
$$ LANGUAGE plpgsql;

-- Calculate a node's weight with caching
CREATE OR REPLACE FUNCTION weight(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  parent_id TEXT;
  parent_weight FLOAT;
  current_points INTEGER;
  total_points INTEGER;
  result FLOAT;
  cache_key TEXT := node_id || '_weight';
  cached_value FLOAT;
BEGIN
  -- Check cache first
  cached_value := get_cached_float(node_id, cache_key);
  
  IF cached_value IS NOT NULL THEN
    RETURN cached_value;
  END IF;
  
  -- Get node's parent
  SELECT n.parent_id, n.points INTO parent_id, current_points
  FROM nodes n
  WHERE n.id = node_id;
  
  -- Root node has weight 1.0
  IF parent_id IS NULL THEN
    result := 1.0;
  ELSE
    -- Get parent's weight
    parent_weight := weight(parent_id);
    
    -- Get total points of all siblings (including self)
    total_points := total_child_points(parent_id);
    
    -- Calculate weight
    IF total_points = 0 THEN
      result := 0;
    ELSE
      result := (current_points::FLOAT / total_points) * parent_weight;
    END IF;
  END IF;
  
  -- Store in cache
  PERFORM cache_float_value(node_id, cache_key, result);
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Calculate share of parent (ratio of points to total sibling points)
CREATE OR REPLACE FUNCTION share_of_parent(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  parent_id TEXT;
  current_points INTEGER;
  total_points INTEGER;
BEGIN
  -- Get node's parent
  SELECT n.parent_id, n.points INTO parent_id, current_points
  FROM nodes n
  WHERE n.id = node_id;
  
  -- Root node has 100% share
  IF parent_id IS NULL THEN
    RETURN 1.0;
  END IF;
  
  -- Get total points of all siblings (including self)
  total_points := total_child_points(parent_id);
  
  -- Calculate share
  IF total_points = 0 THEN
    RETURN 0;
  ELSE
    RETURN current_points::FLOAT / total_points;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Check if a node is a contribution node (has contributors and is not root)
CREATE OR REPLACE FUNCTION is_contribution(node_id TEXT)
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

-- Check if a node has direct contribution children
CREATE OR REPLACE FUNCTION has_direct_contribution_child(node_id TEXT)
RETURNS BOOLEAN AS $$
BEGIN
  RETURN EXISTS (
    SELECT 1
    FROM nodes n
    WHERE n.parent_id = node_id
    AND is_contribution(n.id)
  );
END;
$$ LANGUAGE plpgsql;

-- Check if a node has non-contribution children
CREATE OR REPLACE FUNCTION has_non_contribution_child(node_id TEXT)
RETURNS BOOLEAN AS $$
BEGIN
  RETURN EXISTS (
    SELECT 1
    FROM nodes n
    WHERE n.parent_id = node_id
    AND NOT is_contribution(n.id)
  );
END;
$$ LANGUAGE plpgsql;

-- Calculate the proportion of total child points from contribution children
CREATE OR REPLACE FUNCTION contribution_children_weight(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  contribWeight FLOAT := 0;
  totalWeight FLOAT := 0;
  child_weight FLOAT;
  child_record RECORD;
BEGIN
  FOR child_record IN
    SELECT id
    FROM nodes
    WHERE parent_id = node_id
  LOOP
    child_weight := weight(child_record.id);
    totalWeight := totalWeight + child_weight;
    
    IF is_contribution(child_record.id) THEN
      contribWeight := contribWeight + child_weight;
    END IF;
  END LOOP;
  
  IF totalWeight = 0 THEN
    RETURN 0;
  ELSE
    RETURN contribWeight / totalWeight;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Sum fulfillment from children matching a specific type (contribution or non-contribution)
CREATE OR REPLACE FUNCTION children_fulfillment(node_id TEXT, contribution_only BOOLEAN)
RETURNS FLOAT AS $$
DECLARE
  total_fulfillment FLOAT := 0;
  child_record RECORD;
  child_fulfilled FLOAT;
  child_share FLOAT;
BEGIN
  FOR child_record IN
    SELECT id
    FROM nodes
    WHERE parent_id = node_id
  LOOP
    -- Check if we should consider this child based on contribution status
    IF (contribution_only AND is_contribution(child_record.id)) OR 
       (NOT contribution_only AND NOT is_contribution(child_record.id)) THEN
      child_fulfilled := fulfilled(child_record.id);
      child_share := share_of_parent(child_record.id);
      total_fulfillment := total_fulfillment + (child_fulfilled * child_share);
    END IF;
  END LOOP;
  
  RETURN total_fulfillment;
END;
$$ LANGUAGE plpgsql;

-- Calculate the fulfillment from contribution children
CREATE OR REPLACE FUNCTION contribution_children_fulfillment(node_id TEXT)
RETURNS FLOAT AS $$
BEGIN
  RETURN children_fulfillment(node_id, TRUE);
END;
$$ LANGUAGE plpgsql;

-- Calculate the fulfillment from non-contribution children
CREATE OR REPLACE FUNCTION non_contribution_children_fulfillment(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  total_weight FLOAT := 0;
  weighted_fulfillment FLOAT := 0;
  child_record RECORD;
  child_weight FLOAT;
  child_fulfilled FLOAT;
BEGIN
  FOR child_record IN
    SELECT id
    FROM nodes
    WHERE parent_id = node_id
    AND NOT is_contribution(child_record.id)
  LOOP
    child_weight := weight(child_record.id);
    child_fulfilled := fulfilled(child_record.id);
    
    total_weight := total_weight + child_weight;
    weighted_fulfillment := weighted_fulfillment + (child_weight * child_fulfilled);
  END LOOP;
  
  IF total_weight = 0 THEN
    RETURN 0;
  ELSE
    RETURN weighted_fulfillment / total_weight;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Calculate fulfillment with caching
CREATE OR REPLACE FUNCTION fulfilled(node_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  result FLOAT;
  cache_key TEXT := node_id || '_fulfillment';
  cached_value FLOAT;
  node_record RECORD;
  contribWeight FLOAT;
  nonContribFulfillment FLOAT;
  manual_value FLOAT;
  has_contrib_child BOOLEAN;
  has_non_contrib_child BOOLEAN;
BEGIN
  -- Check cache first
  cached_value := get_cached_float(node_id, cache_key);
  
  IF cached_value IS NOT NULL THEN
    RETURN cached_value;
  END IF;
  
  -- Get node data
  SELECT n.manual_fulfillment INTO node_record
  FROM nodes n
  WHERE n.id = node_id;
  
  -- If leaf node (no children)
  IF NOT EXISTS (SELECT 1 FROM nodes WHERE parent_id = node_id) THEN
    IF is_contribution(node_id) THEN
      result := 1.0;
    ELSE
      result := 0.0;
    END IF;
  -- If has manual fulfillment and direct contribution children
  ELSIF node_record.manual_fulfillment IS NOT NULL AND has_direct_contribution_child(node_id) THEN
    manual_value := node_record.manual_fulfillment;
    has_non_contrib_child := has_non_contribution_child(node_id);
    
    IF NOT has_non_contrib_child THEN
      result := manual_value;
    ELSE
      -- Calculate weighted average of manual value for contributions and regular fulfillment for non-contributions
      contribWeight := contribution_children_weight(node_id);
      nonContribFulfillment := non_contribution_children_fulfillment(node_id);
      result := (manual_value * contribWeight) + (nonContribFulfillment * (1.0 - contribWeight));
    END IF;
  -- Otherwise, calculate from children
  ELSE
    -- Sum up weighted fulfillment from all children
    SELECT COALESCE(SUM(fulfilled(n.id) * share_of_parent(n.id)), 0) INTO result
    FROM nodes n
    WHERE n.parent_id = node_id;
  END IF;
  
  -- Store in cache
  PERFORM cache_float_value(node_id, cache_key, result);
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Calculate the desire (unfulfilled need) of a node
CREATE OR REPLACE FUNCTION desire(node_id TEXT)
RETURNS FLOAT AS $$
BEGIN
  RETURN 1.0 - fulfilled(node_id);
END;
$$ LANGUAGE plpgsql;

-- ---------------------------
-- -- Mutual Fulfillment --
-- ---------------------------

-- Get all descendants including self
CREATE OR REPLACE FUNCTION get_all_descendants_cached(node_id TEXT)
RETURNS TABLE (id TEXT, depth INT) AS $$
DECLARE
  cache_key TEXT := node_id || '_descendants';
  cached_ids TEXT[];
BEGIN
  -- Check cache first
  cached_ids := get_cached_string_list(node_id, cache_key);
  
  IF cached_ids IS NOT NULL THEN
    -- Return from cache
    RETURN QUERY
    WITH descendants(id, depth) AS (
      SELECT unnest(cached_ids), 0
    )
    SELECT * FROM descendants;
  ELSE
    -- Calculate and cache
    CREATE TEMP TABLE IF NOT EXISTS temp_descendants (
      id TEXT,
      depth INT
    ) ON COMMIT DROP;
    
    -- Clear the temp table
    DELETE FROM temp_descendants;
    
    -- Add self
    INSERT INTO temp_descendants VALUES (node_id, 0);
    
    -- Add all descendants recursively
    WITH RECURSIVE descendants AS (
      SELECT id, 1 AS depth
      FROM nodes
      WHERE parent_id = node_id
      
      UNION ALL
      
      SELECT n.id, d.depth + 1
      FROM nodes n
      JOIN descendants d ON n.parent_id = d.id
    )
    INSERT INTO temp_descendants
    SELECT * FROM descendants;
    
    -- Cache the results
    WITH ids AS (
      SELECT array_agg(id) AS id_array
      FROM temp_descendants
    )
    SELECT PERFORM cache_string_list(node_id, cache_key, id_array)
    FROM ids;
    
    -- Return results
    RETURN QUERY SELECT * FROM temp_descendants;
  END IF;
END;
$$ LANGUAGE plpgsql;

-- Calculate the share of fulfillment one node provides to another
CREATE OR REPLACE FUNCTION share_of_general_fulfillment(target_id TEXT, contributor_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  total FLOAT := 0;
  weighted_total FLOAT := 0;
  node_record RECORD;
  contributors_count INTEGER;
  node_weight FLOAT;
  node_fulfilled FLOAT;
BEGIN
  -- Find all nodes that have the contributor in their contributor list
  FOR node_record IN
    SELECT n.id
    FROM nodes n
    JOIN node_contributors nc ON n.id = nc.node_id
    WHERE nc.contributor_id = contributor_id
    AND (
      n.id = target_id 
      OR EXISTS (
        SELECT 1 FROM get_all_descendants_cached(target_id) d
        WHERE d.id = n.id
      )
    )
    AND is_contribution(n.id)
  LOOP
    node_weight := weight(node_record.id);
    node_fulfilled := fulfilled(node_record.id);
    
    -- Count contributors for this node
    SELECT COUNT(*) INTO contributors_count
    FROM node_contributors
    WHERE node_id = node_record.id;
    
    -- Add to weighted total
    weighted_total := weighted_total + (node_weight * node_fulfilled / contributors_count);
  END LOOP;
  
  RETURN weighted_total;
END;
$$ LANGUAGE plpgsql;

-- Calculate mutual fulfillment between two nodes with caching
CREATE OR REPLACE FUNCTION mutual_fulfillment(node_id1 TEXT, node_id2 TEXT)
RETURNS FLOAT AS $$
DECLARE
  result FLOAT;
  cache_key TEXT;
  cached_value FLOAT;
  a_to_b FLOAT;
  b_to_a FLOAT;
BEGIN
  -- Create consistent cache key regardless of parameter order
  IF node_id1 < node_id2 THEN
    cache_key := node_id1 || '_mutual_' || node_id2;
  ELSE
    cache_key := node_id2 || '_mutual_' || node_id1;
  END IF;
  
  -- Check cache first
  cached_value := get_cached_float(node_id1, cache_key);
  
  IF cached_value IS NOT NULL THEN
    RETURN cached_value;
  END IF;
  
  -- Calculate mutual fulfillment
  a_to_b := share_of_general_fulfillment(node_id1, node_id2);
  b_to_a := share_of_general_fulfillment(node_id2, node_id1);
  
  result := LEAST(a_to_b, b_to_a);
  
  -- Store in cache (store for both nodes for easier lookup)
  PERFORM cache_float_value(node_id1, cache_key, result);
  PERFORM cache_float_value(node_id2, cache_key, result);
  
  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Find the highest mutual fulfillment node from a target to any node in a subtree
CREATE OR REPLACE FUNCTION find_highest_mutual_path(root_id TEXT, target_id TEXT)
RETURNS TEXT[] AS $$
DECLARE
  best_path TEXT[] := ARRAY[root_id];
  best_score FLOAT := 0;
  current_path TEXT[];
  current_score FLOAT;
  descendant RECORD;
BEGIN
  -- Check mutual fulfillment with root
  best_score := mutual_fulfillment(root_id, target_id);
  
  -- Check all descendants
  FOR descendant IN
    SELECT id FROM get_all_descendants_cached(root_id) WHERE id != root_id
  LOOP
    current_score := mutual_fulfillment(descendant.id, target_id);
    
    -- If better score, track this path
    IF current_score > best_score THEN
      -- Build path to this node
      WITH RECURSIVE path_to_node AS (
        SELECT id, parent_id, ARRAY[id] AS path
        FROM nodes
        WHERE id = descendant.id
        
        UNION ALL
        
        SELECT n.id, n.parent_id, p.path || n.id
        FROM nodes n
        JOIN path_to_node p ON n.id = p.parent_id
        WHERE n.id != root_id
      )
      SELECT path INTO current_path
      FROM path_to_node
      WHERE parent_id IS NULL OR parent_id = root_id
      LIMIT 1;
      
      -- Update best path and score
      best_path := ARRAY[root_id] || current_path;
      best_score := current_score;
    END IF;
  END LOOP;
  
  RETURN best_path;
END;
$$ LANGUAGE plpgsql;

-- Get all nodes with mutual fulfillment above a threshold
CREATE OR REPLACE FUNCTION get_high_mutual_nodes(node_id TEXT, threshold FLOAT)
RETURNS TABLE (id TEXT, mutual_score FLOAT) AS $$
BEGIN
  RETURN QUERY
  WITH all_nodes AS (
    SELECT id FROM get_all_descendants_cached(node_id)
    UNION
    SELECT node_id
  )
  SELECT n.id, mutual_fulfillment(node_id, n.id) AS mutual_score
  FROM all_nodes n
  WHERE mutual_fulfillment(node_id, n.id) > threshold;
END;
$$ LANGUAGE plpgsql;

-- ------------------------------------------
-- -- Provider-centric share calculation --
-- ------------------------------------------

-- Calculate provider shares (depth 1 - direct contributors only)
CREATE OR REPLACE FUNCTION provider_shares_depth1(provider_id TEXT)
RETURNS JSONB AS $$
DECLARE
  shares JSONB := '{}'::jsonb;
  contributor_record RECORD;
  mutual_val FLOAT;
  total FLOAT := 0;
BEGIN
  -- Calculate initial mutual values
  FOR contributor_record IN
    SELECT nc.contributor_id
    FROM node_contributors nc
    WHERE nc.node_id = provider_id
  LOOP
    mutual_val := mutual_fulfillment(provider_id, contributor_record.contributor_id);
    total := total + mutual_val;
    
    -- Add to shares
    shares := jsonb_set(
      shares, 
      ARRAY[contributor_record.contributor_id], 
      to_jsonb(mutual_val)
    );
  END LOOP;
  
  -- Normalize shares
  IF total > 0 THEN
    FOR contributor_record IN
      SELECT jsonb_object_keys(shares) AS id
    LOOP
      shares := jsonb_set(
        shares,
        ARRAY[contributor_record.id],
        to_jsonb((shares->contributor_record.id)::float / total)
      );
    END LOOP;
  END IF;
  
  RETURN shares;
END;
$$ LANGUAGE plpgsql;

-- Core provider shares calculation with variable depth
CREATE OR REPLACE FUNCTION provider_shares(provider_id TEXT, depth INTEGER)
RETURNS JSONB AS $$
DECLARE
  shares JSONB;
  cache_key TEXT := provider_id || '_shares_' || depth;
  cached_shares JSONB;
  processed_shares JSONB;
  visited JSONB := '[]'::jsonb;
  current_depth INTEGER := 1;
  recipient_id TEXT;
  recipient_shares JSONB;
  recipient_share FLOAT;
  weighted_shares JSONB;
  total_share FLOAT;
BEGIN
  -- Check cache first
  cached_shares := get_cached_map(provider_id, cache_key);
  
  IF cached_shares IS NOT NULL THEN
    RETURN cached_shares;
  END IF;
  
  -- Base case: depth 1
  IF depth <= 1 THEN
    shares := provider_shares_depth1(provider_id);
    PERFORM cache_map_value(provider_id, cache_key, shares);
    RETURN shares;
  END IF;
  
  -- Start with depth 1 shares
  shares := provider_shares_depth1(provider_id);
  
  -- Process each additional depth
  WHILE current_depth < depth LOOP
    processed_shares := shares;
    
    -- Process each recipient
    FOR recipient_id IN 
      SELECT jsonb_object_keys(shares)
      WHERE NOT (visited @> to_jsonb(jsonb_object_keys(shares)))
    LOOP
      -- Skip if already visited
      IF visited @> to_jsonb(recipient_id) THEN
        CONTINUE;
      END IF;
      
      -- Mark as visited
      visited := visited || to_jsonb(recipient_id);
      
      -- Get recipient's share
      recipient_share := (shares->recipient_id)::float;
      
      -- Get recipient's shares (depth 1 only)
      recipient_shares := provider_shares_depth1(recipient_id);
      
      -- Weight recipient's shares by their share
      weighted_shares := '{}'::jsonb;
      
      FOR transitive_id IN SELECT jsonb_object_keys(recipient_shares) LOOP
        weighted_shares := jsonb_set(
          weighted_shares,
          ARRAY[transitive_id],
          to_jsonb((recipient_shares->transitive_id)::float * recipient_share)
        );
      END LOOP;
      
      -- Merge into main shares
      FOR transitive_id IN SELECT jsonb_object_keys(weighted_shares) LOOP
        IF shares ? transitive_id THEN
          shares := jsonb_set(
            shares,
            ARRAY[transitive_id],
            to_jsonb((shares->transitive_id)::float + (weighted_shares->transitive_id)::float)
          );
        ELSE
          shares := jsonb_set(
            shares,
            ARRAY[transitive_id],
            weighted_shares->transitive_id
          );
        END IF;
      END LOOP;
    END LOOP;
    
    current_depth := current_depth + 1;
  END LOOP;
  
  -- Normalize final shares
  total_share := 0;
  
  FOR recipient_id IN SELECT jsonb_object_keys(shares) LOOP
    total_share := total_share + (shares->recipient_id)::float;
  END LOOP;
  
  IF total_share > 0 THEN
    FOR recipient_id IN SELECT jsonb_object_keys(shares) LOOP
      shares := jsonb_set(
        shares,
        ARRAY[recipient_id],
        to_jsonb((shares->recipient_id)::float / total_share)
      );
    END LOOP;
  END IF;
  
  -- Cache the result
  PERFORM cache_map_value(provider_id, cache_key, shares);
  
  RETURN shares;
END;
$$ LANGUAGE plpgsql;

-- Get a direct share (depth 1)
CREATE OR REPLACE FUNCTION direct_share(provider_id TEXT, recipient_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  shares JSONB;
BEGIN
  shares := provider_shares(provider_id, 1);
  RETURN COALESCE((shares->recipient_id)::float, 0);
END;
$$ LANGUAGE plpgsql;

-- Get a receiver's share from a specific capacity provider
CREATE OR REPLACE FUNCTION receiver_share_from(receiver_id TEXT, provider_id TEXT, capacity_id TEXT, max_depth INTEGER)
RETURNS FLOAT AS $$
DECLARE
  provider_share_map JSONB;
BEGIN
  -- Check if provider has the capacity
  IF NOT EXISTS (
    SELECT 1 FROM capacities 
    WHERE id = capacity_id AND owner_id = provider_id
  ) THEN
    RETURN 0;
  END IF;
  
  provider_share_map := provider_shares(provider_id, max_depth);
  RETURN COALESCE((provider_share_map->receiver_id)::float, 0);
END;
$$ LANGUAGE plpgsql;

-- Get a person's total share in a specific capacity
CREATE OR REPLACE FUNCTION get_personal_capacity_share(person_id TEXT, capacity_id TEXT)
RETURNS FLOAT AS $$
DECLARE
  total_share FLOAT := 0;
  current_share FLOAT;
  owner_record RECORD;
BEGIN
  -- Find all owners of this capacity
  FOR owner_record IN
    SELECT owner_id
    FROM capacities
    WHERE id = capacity_id
  LOOP
    current_share := receiver_share_from(person_id, owner_record.owner_id, capacity_id, 2);
    
    IF current_share > total_share THEN
      total_share := current_share;
    END IF;
  END LOOP;
  
  RETURN total_share;
END;
$$ LANGUAGE plpgsql;

-- -----------------------
-- -- Capacity Utils --
-- -----------------------

-- Compute quantity share based on percentage and divisibility constraints
CREATE OR REPLACE FUNCTION compute_quantity_share(capacity_id TEXT, percentage FLOAT)
RETURNS INTEGER AS $$
DECLARE
  cap_record RECORD;
  raw_quantity INTEGER;
  percent_constrained INTEGER;
  natural_constrained INTEGER;
BEGIN
  -- Get capacity details
  SELECT c.quantity, c.natural_div, c.percentage_div
  INTO cap_record
  FROM capacities c
  WHERE c.id = capacity_id;
  
  -- Calculate raw quantity
  raw_quantity := ROUND(cap_record.quantity * percentage);
  
  -- Apply percentage divisibility constraint
  IF percentage > cap_record.percentage_div THEN
    percent_constrained := ROUND(cap_record.quantity * cap_record.percentage_div);
  ELSE
    percent_constrained := raw_quantity;
  END IF;
  
  -- Apply natural number divisibility constraint
  natural_constrained := (percent_constrained / cap_record.natural_div) * cap_record.natural_div;
  
  RETURN natural_constrained;
END;
$$ LANGUAGE plpgsql;

-- Update computed quantities for capacity shares
CREATE OR REPLACE FUNCTION update_computed_quantities(node_id TEXT)
RETURNS VOID AS $$
DECLARE
  share_record RECORD;
  share_obj JSONB;
  new_quantity INTEGER;
BEGIN
  -- Process each share in the node's capacity shares
  FOR share_record IN
    SELECT c.id, c.shares
    FROM capacities c
    JOIN jsonb_object_keys(c.shares) AS recipient(id) 
      ON recipient.id = node_id
    WHERE c.shares ? node_id
  LOOP
    -- Get the current share object
    share_obj := share_record.shares->node_id;
    
    -- Compute the new quantity
    new_quantity := compute_quantity_share(
      share_record.id, 
      (share_obj->>'sharePercentage')::float
    );
    
    -- Update the computed quantity
    share_obj := jsonb_set(
      share_obj,
      ARRAY['computedQuantity'],
      to_jsonb(new_quantity)
    );
    
    -- Update the capacity record
    UPDATE capacities
    SET shares = jsonb_set(
      shares,
      ARRAY[node_id],
      share_obj
    )
    WHERE id = share_record.id;
  END LOOP;
END;
$$ LANGUAGE plpgsql; 