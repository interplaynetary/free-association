<script lang="ts">
  import * as d3 from "d3";
  import { type RecNode } from "../stores/rec";
  export let child: d3.HierarchyRectangularNode<RecNode>;

  // Helper function to get color based on name
  function getColorForName(name: string): string {
    if (!name) return "#64748b"; // Default slate color

    // Simple hash function for consistent colors
    let hash = 0;
    for (let i = 0; i < name.length; i++) {
      hash = name.charCodeAt(i) + ((hash << 5) - hash);
    }

    // Generate colors in a pleasant range
    const h = Math.abs(hash) % 360;
    const s = 65 + (Math.abs(hash) % 20); // 65-85% saturation
    const l = 60 + (Math.abs(hash) % 15); // 60-75% lightness

    return `hsl(${h}, ${s}%, ${l}%)`;
  }

  // Create unique ID for this node
  const uid = (name: string) => {
    const id = `id-${Math.random().toString(36).substring(2, 9)}`;
    return {
      id,
      href: `#${id}`,
    };
  };

  const leafUid = uid("leaf");
  const clipUid = uid("clip");

  // Prepare text segments
  const name = child.data?.name || "Unnamed";
  const segments = name === "Unnamed" ? [name] : name.split(/(?=[A-Z][^A-Z])/g);

  // Calculate relative size for text scaling
  $: nodeWidth = child.x1 - child.x0;
  $: nodeHeight = child.y1 - child.y0;
  $: fontSize = Math.min(nodeWidth, nodeHeight) * 0.1; // Scale font based on node size
</script>

<g class="treemap-node" transform={`translate(${child.x0},${child.y0})`}>
  <rect
    id={child.data && child.data._key ? `leaf-${child.data._key}` : leafUid.id}
    x="0"
    y="0"
    width={nodeWidth}
    height={nodeHeight}
    fill={getColorForName(child.data?.name || "")}
    stroke={child.data?.hasDirectContributionChild ? "#2196f3" : "#fff"}
    stroke-width="0.002"
    vector-effect="non-scaling-stroke"
  />

  <clipPath id={clipUid.id}>
    <use
      href={`#${child.data && child.data._key ? `leaf-${child.data._key}` : leafUid.id}`}
    />
  </clipPath>

  <text
    clip-path={`url(#${clipUid.id})`}
    font-weight="normal"
    style="user-select: none; -webkit-user-select: none; -moz-user-select: none; -ms-user-select: none; text-anchor: middle; dominant-baseline: middle;"
    transform={`translate(${nodeWidth / 2},${nodeHeight / 2})`}
    class="edit-text-field"
    font-size={fontSize}
  >
    {#each segments as segment, i}
      <tspan
        x="0"
        dy={i === 0 ? `${(-(segments.length - 1) * 1.2) / 2}em` : "1.2em"}
      >
        {segment}
      </tspan>
    {/each}
  </text>

  <title>{name}</title>
</g>
