<script lang="ts">
  import { onMount, onDestroy } from "svelte";
  import * as d3 from "d3";
  import { getColorForName } from "../../utils/colorUtils";
  import { calculateFontSize } from "../../utils/fontUtils";
  import TagPill from "../../TagPill.svelte";
  import { TreeNode } from "./models/TreeNode";
  import type { RecognitionStore } from "../../stores/rec";

  // Props
  export let node: TreeNode;
  export let store: RecognitionStore | null = null;
  export let isRoot: boolean = false;
  export let width: number;
  export let height: number;
  export let x: d3.ScaleLinear<number, number>;
  export let y: d3.ScaleLinear<number, number>;
  export let currentView: TreeNode | null = null;
  export let deleteMode: boolean = false;
  export let parentId: string | null = null;

  // Internal state
  let domElement: HTMLElement;
  let nodeTagComponents: any[] = [];
  let uid = Math.random().toString(36).substring(2, 11);

  // Lifecycle
  onMount(() => {
    renderNode();

    // Add a special class to indicate this component is mounted
    if (domElement) {
      domElement.classList.add("svelte-node-mounted");
    }
  });

  onDestroy(() => {
    // Clean up tag components
    if (nodeTagComponents.length > 0) {
      nodeTagComponents.forEach((component) => {
        if (component && typeof component.$destroy === "function") {
          component.$destroy();
        }
      });
    }
  });

  // Generate a unique ID
  function generateId(prefix: string) {
    const id = `${prefix}-${uid}`;
    return { id, href: `#${id}` };
  }

  // Function to render node using D3
  function renderNode() {
    // Skip if element not ready
    if (!domElement) return;

    // Create D3 selection for the node
    const selection = d3.select(domElement);

    // Clear any existing content
    selection.selectAll("*").remove();

    // Create leaf ID for clipping
    const leafUid = generateId("leaf");
    const clipUid = generateId("clip");

    // Add title for tooltips
    selection.append("title").text(node?.name || "Unnamed");

    // Add rectangle
    selection
      .append("rect")
      .attr("id", leafUid.id)
      .attr("width", isRoot ? width : x(1) - x(0))
      .attr("height", isRoot ? 50 : y(1) - y(0))
      .attr("fill", isRoot ? "#fff" : getColorForName(node?.name || "Unnamed"))
      .attr("stroke", "#fff")
      .attr("stroke-width", "2");

    // Add clip path
    selection
      .append("clipPath")
      .attr("id", clipUid.id)
      .append("use")
      .attr("xlink:href", leafUid.href);

    // Add text
    const text = selection
      .append("text")
      .attr("clip-path", `url(${clipUid.href})`)
      .attr("font-weight", isRoot ? "bold" : null)
      .style("user-select", "none")
      .style("-webkit-user-select", "none")
      .style("-moz-user-select", "none")
      .style("-ms-user-select", "none")
      .attr("transform", () => {
        const rectWidth = isRoot ? width : x(1) - x(0);
        const rectHeight = isRoot ? 50 : y(1) - y(0);
        return `translate(${rectWidth / 2},${rectHeight / 2})`;
      })
      .style("text-anchor", "middle")
      .style("dominant-baseline", "middle")
      .style("font-size", () => {
        return getNodeFontSize() + "px";
      });

    // Add text spans
    const nodeName = node?.name || "Unnamed";
    text
      .selectAll("tspan")
      .data(isRoot ? [nodeName] : nodeName.split(/(?=[A-Z][^A-Z])/g))
      .join("tspan")
      .attr("x", 0)
      .attr("dy", (d, i, nodes) => {
        if (i === 0) {
          return `${(-(nodes.length - 1) * 1.2) / 2}em`;
        }
        return "1.2em";
      })
      .text((d) => d)
      .attr("class", "edit-text-field") // Use class for cursor style
      .on("click", handleTextClick);

    // Only add tags container if not in deleteMode and node is large enough
    const rectWidth = isRoot ? width : x(1) - x(0);
    const rectHeight = isRoot ? 50 : y(1) - y(0);

    if (rectWidth >= 60 && rectHeight >= 60 && !deleteMode) {
      addTagsContainer(selection, rectWidth, rectHeight);
    }

    // Add border style for contribution nodes
    if (node?.isContribution) {
      selection
        .select("rect")
        .attr("stroke", "#2196f3")
        .attr("stroke-width", "3");
    }
  }

  // Get font size for the node
  function getNodeFontSize(): number {
    const rectWidth = isRoot ? width : x(1) - x(0);
    const rectHeight = isRoot ? 50 : y(1) - y(0);

    // Basic size calculation
    const minDimension = Math.min(rectWidth, rectHeight);
    const size = Math.max(minDimension / 10, 10);

    // Adjust based on text length
    const textLength = node?.name ? node.name.length : 10;
    return Math.min(size, 180 / textLength);
  }

  // Add tags container with contributor tags
  function addTagsContainer(
    selection: d3.Selection<HTMLElement, unknown, null, undefined>,
    rectWidth: number,
    rectHeight: number,
  ) {
    // Calculate position for tags
    const nodeName = node?.name || "Unnamed";
    const textLines = isRoot ? 1 : nodeName.split(/(?=[A-Z][^A-Z])/g).length;
    const fontSize = getNodeFontSize();
    const verticalOffset = (textLines * 1.2 * fontSize) / 2 + 10;

    // Create a foreign object to host HTML content
    const foreignObject = selection
      .append("foreignObject")
      .attr("class", "tags-container")
      .attr("x", 10)
      .attr("y", rectHeight / 2 + verticalOffset)
      .attr("width", rectWidth - 20)
      .attr("height", 40); // Height for tag pills

    // Add wrapper div for pills
    const wrapper = foreignObject
      .append("xhtml:div")
      .attr("class", "tag-wrapper");

    // Add contributor button if we have a store and it's not the root
    if (store && !isRoot) {
      wrapper
        .append("xhtml:div")
        .attr("class", "add-contributor-button")
        .text("+")
        .on("click", (event) => {
          event.stopPropagation();
          // Send custom event instead of using Svelte dispatcher
          if (domElement && node?.id) {
            const customEvent = new CustomEvent("addContributor", {
              detail: { nodeId: node.id },
              bubbles: true,
            });
            domElement.dispatchEvent(customEvent);
          }
        });
    }

    // Add tags as pills
    if (node) {
      node.getContributors().forEach((contributor) => {
        const pillContainer = wrapper
          .append("xhtml:div")
          .attr("class", "pill-container");

        // Create the TagPill component
        const tagPillComponent = new TagPill({
          target: pillContainer.node() as HTMLElement,
          props: {
            userId: contributor.id,
            truncateLength: 10,
            removable: true,
          },
        });

        // Add event listeners that dispatch custom events
        tagPillComponent.$on("click", () => {
          // Send custom event
          if (domElement) {
            const customEvent = new CustomEvent("tagClick", {
              detail: { tagId: contributor.id },
              bubbles: true,
            });
            domElement.dispatchEvent(customEvent);
          }
        });

        tagPillComponent.$on("remove", () => {
          // Send custom event
          if (domElement && node?.id) {
            const customEvent = new CustomEvent("removeTag", {
              detail: { nodeId: node.id, tagId: contributor.id },
              bubbles: true,
            });
            domElement.dispatchEvent(customEvent);
          }
        });

        // Track for cleanup
        nodeTagComponents.push(tagPillComponent);
      });
    }
  }

  // Handle text click for editing
  function handleTextClick(event: MouseEvent) {
    event.stopPropagation(); // Prevent node click/zoom

    // Don't edit text in delete mode or if this is the root node
    if (deleteMode || isRoot) return;

    // Only proceed if we have a store for operations
    if (!store) return;

    // Create an input element for editing
    const textElement = event.target as HTMLElement;
    const textBox = textElement.getBoundingClientRect();

    // Get the parent node's dimensions
    const parentNode = d3.select(domElement);
    const rect = parentNode.select("rect").node() as SVGRectElement;
    const nodeWidth = rect.width.baseVal.value;
    const nodeHeight = rect.height.baseVal.value;

    // Create a foreign object for the input
    const foreignObject = parentNode
      .append("foreignObject")
      .attr("width", nodeWidth)
      .attr("height", nodeHeight)
      .style("pointer-events", "none");

    // Add the input container
    const container = foreignObject
      .append("xhtml:div")
      .attr("class", "new-node-container");

    // Add the input element
    const input = container
      .append("xhtml:input")
      .attr("class", "new-node-input")
      .style("font-size", getNodeFontSize() + "px")
      .attr("value", node?.name || "Unnamed");

    // Focus the input
    const inputElement = input.node() as HTMLInputElement;
    if (inputElement) {
      setTimeout(() => {
        inputElement.focus();
        inputElement.select();
      }, 0);
    }

    // Complete the edit
    const completeEdit = async () => {
      const newName = inputElement?.value;
      foreignObject.remove();

      if (newName && newName !== node?.name && node?.id) {
        // If we have the store and it's a direct child
        if (store && parentId) {
          const childStore = store.getChild(node.id);
          await childStore.updateName(newName);

          // Send custom event
          if (domElement) {
            const customEvent = new CustomEvent("nameUpdated", {
              detail: { nodeId: node.id, name: newName },
              bubbles: true,
            });
            domElement.dispatchEvent(customEvent);
          }
        }
      }
    };

    // Handle keyboard events
    input.on("keydown", (event: KeyboardEvent) => {
      if (event.key === "Enter") {
        event.preventDefault();
        completeEdit();
      } else if (event.key === "Escape") {
        event.preventDefault();
        foreignObject.remove();
      }
    });

    // Handle blur event
    input.on("blur", completeEdit);
  }

  // Handle node click
  function handleNodeClick(event: MouseEvent) {
    event.stopPropagation();

    // Handle node deletion in delete mode
    if (deleteMode && !isRoot && store && parentId && node?.id) {
      // Confirm deletion
      if (confirm(`Delete "${node.name || "Unnamed"}"?`)) {
        // Remove the node using the store method
        store
          .removeChild(node.id)
          .then(() => {
            // Send custom event
            if (domElement) {
              const customEvent = new CustomEvent("nodeDelete", {
                detail: { nodeId: node.id },
                bubbles: true,
              });
              domElement.dispatchEvent(customEvent);
            }
          })
          .catch((err) => {
            console.error("Error deleting node:", err);
          });
      }
      return;
    }

    // Handle node navigation (zooming in)
    if (!isRoot && node?.id) {
      // Send custom event
      if (domElement) {
        const customEvent = new CustomEvent("nodeClick", {
          detail: { nodeId: node.id },
          bubbles: true,
        });
        domElement.dispatchEvent(customEvent);
      }
    }
  }

  // Reactively update node when props change
  $: if (domElement && (node || width || height || deleteMode)) {
    renderNode();
  }
</script>

<div
  class="node-container"
  class:root={isRoot}
  class:contribution={node?.isContribution}
  class:delete-mode={deleteMode}
  bind:this={domElement}
  onclick={handleNodeClick}
>
  <!-- Node will be rendered here by D3 -->
</div>

<style>
  .node-container {
    width: 100%;
    height: 100%;
    position: relative;
    cursor: pointer;
    overflow: visible;
  }

  .root {
    cursor: default;
  }

  :global(.edit-text-field) {
    cursor: text;
  }

  :global(.tag-wrapper) {
    display: flex;
    flex-wrap: wrap;
    justify-content: center;
    gap: 4px;
    align-items: center;
    height: 100%;
    width: 100%;
  }

  :global(.pill-container) {
    display: inline-block;
    margin: 2px;
  }

  :global(.add-contributor-button) {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 22px;
    height: 22px;
    border-radius: 11px;
    background: rgba(200, 200, 200, 0.7);
    font-size: 16px;
    line-height: 1;
    color: #333;
    cursor: pointer;
    margin: 2px;
    transition: all 0.2s ease;
  }

  :global(.add-contributor-button:hover) {
    background: rgba(200, 200, 200, 0.9);
    transform: scale(1.1);
  }

  /* New node input styling */
  :global(.new-node-input) {
    width: 80%;
    font-family: inherit;
    text-align: center;
    border: none;
    outline: none;
    background: rgba(255, 255, 255, 0.9);
    pointer-events: auto;
  }

  :global(.new-node-container) {
    width: 100%;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
  }
</style>
