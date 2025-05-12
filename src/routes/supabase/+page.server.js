import { supabase } from "$lib/supabaseClient";

export async function load() {
  const { data: nodes, error } = await supabase
    .from("nodes")
    .select(`
      id,
      name,
      points,
      parent_id,
      manual_fulfillment,
      contributors
    `);

  if (error) {
    console.error('Error fetching nodes:', error);
    return {
      nodes: []
    };
  }

  return {
    nodes: nodes || []
  };
} 