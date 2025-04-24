<script lang="ts">
  /*
   * TreeMap.svelte - Svelte component for visualizing tree structures using D3 treemap layout
   *
   * This component visualizes hierarchical recognition data using a D3 treemap layout.
   * It works directly with RecognitionStore data structures without requiring intermediate transformations.
   *
   * Usage:
   * <TreeMap bind:this={treeMapComponent} store={recognitionStore} width={500} height={400} />
   */

  import * as d3 from "d3";
  import { getColorForName } from "../lib/utils/colorUtils";
  import { calculateFontSize } from "../lib/utils/fontUtils";
  import { usersMap } from "../lib/utils/userUtils";
  import { onMount, onDestroy } from "svelte";
  import { mount, unmount } from "svelte";
  import {
    type RecognitionStore,
    type NodeEntry,
    type RecNode,
  } from "../../stores/rec.svelte";
  import DropDown from "../DropDown.svelte";
  import ToastNotification from "./old/ToastNotification.svelte";
  import TagPill from "../TagPill.svelte";
  import LogIn from "../LogIn.svelte";

  // Add log in button to the top right of the treemap
  // add settings button to the top left of the treemap
  // for login.svelte

  // currently we are in a public tree map
  // how do we transition to a user-space tree map

  /*
    For some reason it is possible for the interface to not always reflect the current state of the store.
    Deleting shows on our own interface immediately upon the first delete, but the second and subsequent deletes are unreliably being removed from the interface.
    Although they do get deleted from the store. (Which updates the interfaces of other users)
    This is a problem.
    */

  /*
    lets make it so that logging in updates the store.
    */

  // Initialize the global app object for tree map operations
  if (typeof window !== "undefined") {
    // console.log('Initializing TreeMap app object');
    (window as any).isGrowingActive = false;
  }

  // Main props
  export let store: RecognitionStore;
  export let width: number = 900;
  export let height: number = 600;
  export const showDebug: boolean = false;

  // State variables for growth animation
  let growthInterval: ReturnType<typeof setInterval> | null = null;
  let growthTimeout: ReturnType<typeof setTimeout> | null = null;
  const GROWTH_RATE = (d: d3.HierarchyRectangularNode<RecNode>) =>
    d.data.points ? d.data.points * 0.05 : 0;
  const GROWTH_TICK = 50;
  const GROWTH_DELAY = 500;
  let isGrowing = false;
  const SHRINK_RATE = (d: d3.HierarchyRectangularNode<RecNode>) =>
    d.data.points ? d.data.points * -0.05 : 0; // Negative growth rate for shrinking

  // Additional intervals that might be used by the component or parent
  let updateInterval: number | null = null;
  let saveInterval: number | null = null;

  // Create scales and references
  let x = d3.scaleLinear();
  let y = d3.scaleLinear();
  let hierarchy: d3.HierarchyNode<RecNode>;
  let root: d3.HierarchyRectangularNode<RecNode> = undefined as any;
  let currentView: d3.HierarchyRectangularNode<RecNode> = undefined as any;
  let svg: d3.Selection<SVGSVGElement, unknown, null, undefined>;
  let group: d3.Selection<SVGGElement, unknown, null, undefined>;

  let node: RecNode = undefined as any;

  // Container reference with custom interface to allow adding methods
  interface TreeMapElement extends HTMLDivElement {
    getElement?: () => HTMLElement | SVGSVGElement;
  }

  let treeMap: TreeMapElement;
  let childrenData: NodeEntry[] = [];
  let name: string = "";
  let points: number = 0;
  let contributors: NodeEntry[] = [];

  // Track subscriptions to store data
  let unsubscribeChildren: () => void;
  let unsubscribeName: () => void;
  let unsubscribePoints: () => void;
  let unsubscribeContributors: () => void;

  // Single array to track all dynamic subscriptions
  let nodeSubscriptions: (() => void)[] = [];

  // Define the activeNode variable at the top level
  let activeNode: d3.HierarchyRectangularNode<RecNode> | null = null;

  // Component state
  let deleteMode = false; // Track if delete mode is active
  let nodeTagComponents: any[] = []; // Array to track TagPill components

  // Add this near the start of your script section
  let resolvedPathNames: string[] = [];

  // Add a flag to prevent recursive updates
  let isUpdatingTreeData = false;

  // Add a flag to properly control recursive update cycles
  let isUpdatingVisualization = false;
  let lastUpdateTimestamp = 0;
  const UPDATE_THROTTLE_MS = 100; // Minimum ms between updates

  // Handle window resize
  function handleResize() {
    update(window.innerWidth, window.innerHeight);
  }

  // Replace the implementation with one that uses getPathToRoot
  async function resolvedStorePathNames() {
    if (!store) {
      return [];
    }

    try {
      // Get the path from the current node to the root
      const pathToRoot = await store.getPathToRoot();

      // Extract names in reverse order (from root to current node)
      const names = pathToRoot.map((item) => item.name || item.id).reverse();

      // Update the cache for synchronous access
      resolvedPathNames = names;
      return names;
    } catch (err) {
      console.error("Error resolving path names:", err);
      return [];
    }
  }

  // Add a reactive statement to update path names when store changes
  $: if (store) {
    resolvedStorePathNames().catch((err) => {
      console.error("Error resolving path names:", err);
    });
  }

  // Helper to get store value synchronously without subscription side effects
  function get<T>(store: {
    subscribe: (callback: (value: T) => void) => () => void;
  }): T | undefined {
    let value: T | undefined;
    const unsubscribe = store.subscribe((v) => {
      value = v;
    });
    unsubscribe();
    return value;
  }

  // Helper function to generate unique IDs
  const uid = (function () {
    let id = 0;
    return function (prefix: string) {
      const uniqueId = `${prefix}-${++id}`;
      return { id: uniqueId, href: `#${uniqueId}` };
    };
  })();

  // Get the element - used to match TreemapInstance.element
  export function getElement(): HTMLElement | SVGSVGElement {
    return svg?.node() || treeMap;
  }

  // Add a helper function to check growing state more safely
  function isGrowingActiveState(): boolean {
    return (window as any).isGrowingActive || false;
  }

  export function destroy() {
    // Clean up all timeouts and intervals
    if (growthInterval) clearInterval(growthInterval);
    if (growthTimeout) clearTimeout(growthTimeout);
    if (updateInterval) clearInterval(updateInterval);
    if (saveInterval) clearInterval(saveInterval);
    if (pendingUpdateTimeout !== null) clearTimeout(pendingUpdateTimeout);
    if (pendingVisualTimeout !== null) clearTimeout(pendingVisualTimeout);

    // Unsubscribe from stores
    if (unsubscribeChildren) unsubscribeChildren();
    if (unsubscribeName) unsubscribeName();
    if (unsubscribePoints) unsubscribePoints();
    if (unsubscribeContributors) unsubscribeContributors();

    // Clean up node subscriptions
    nodeSubscriptions.forEach((unsub) => unsub());
    nodeSubscriptions = [];

    // Remove resize listener
    window.removeEventListener("resize", handleResize);
  }

  export function update(newWidth: number, newHeight: number) {
    if (!svg) return;

    // Update dimensions
    width = newWidth || window.innerWidth;
    height = newHeight || window.innerHeight;

    // Update scales
    x.rangeRound([0, width]);
    y.rangeRound([0, height]);

    // Update SVG viewBox
    svg.attr("viewBox", [0.5, -50.5, width, height + 50]);

    // Clear existing content and create new group
    group.remove(); // Remove old group
    group = svg.append("g"); // Create new group

    // Update visualization with current view
    if (currentView) {
      group.call(render, currentView);
    } else if (root) {
      group.call(render, root);
    }
  }

  // Treemap tile function
  function tile(
    node: d3.HierarchyRectangularNode<RecNode>,
    x0: number,
    y0: number,
    x1: number,
    y1: number,
  ) {
    if (!node.children || node.children.length === 0) return;

    // Calculate available space
    const availableWidth = x1 - x0;
    const availableHeight = y1 - y0;

    // Ensure each child has a valid points value
    node.children.forEach((child) => {
      (child as any).value =
        child.data && typeof child.data.points === "number"
          ? child.data.points
          : 0;
      // console.log(`Child ${child.data.name || 'unnamed'} has ${(child as any).value} points/value`);
    });

    // Create a simpler hierarchy object that matches d3's expectations
    const tempRoot = {
      children: node.children.map((child) => ({
        data: child.data,
        value: (child as any).value,
      })),
    };

    // Create hierarchy and apply squarify directly
    try {
      const tempHierarchy = d3.hierarchy(tempRoot).sum((d) => (d as any).value);

      // Apply squarify directly with the available space
      d3.treemapSquarify(
        tempHierarchy as d3.HierarchyRectangularNode<any>,
        0,
        0,
        availableWidth,
        availableHeight,
      );

      // Transfer positions back to our nodes
      node.children.forEach((child, i) => {
        if (tempHierarchy.children && tempHierarchy.children[i]) {
          const tempNode = tempHierarchy.children[
            i
          ] as d3.HierarchyRectangularNode<any>;
          child.x0 = x0 + tempNode.x0;
          child.x1 = x0 + tempNode.x1;
          child.y0 = y0 + tempNode.y0;
          child.y1 = y0 + tempNode.y1;

          // console.log(`Set child ${child.data.name || 'unnamed'} position: x0=${child.x0}, y0=${child.y0}, x1=${child.x1}, y1=${child.y1}`);
        }
      });
    } catch (error) {
      console.error("Error in tile function:", error);
    }
  }

  function subscribeToStoreData() {
    // Clean up previous subscriptions first
    if (unsubscribeChildren) unsubscribeChildren();
    if (unsubscribeName) unsubscribeName();
    if (unsubscribePoints) unsubscribePoints();
    if (unsubscribeContributors) unsubscribeContributors();

    // Clean up node subscriptions
    nodeSubscriptions.forEach((unsub) => unsub());
    nodeSubscriptions = [];

    // Only subscribe if we have a valid store
    if (!store) {
      console.warn("No store provided to TreeMap");
      return;
    }

    // Pre-fetch data to avoid recursive subscription triggers
    // This gets the current state before setting up new subscriptions
    childrenData = get(store.childrenStore) || [];
    name = get(store.nameStore) || "";
    points = get(store.pointsStore) || 0;
    contributors = get(store.contributorsStore) || [];

    // Now, set up data subscriptions with careful update triggers
    unsubscribeChildren = store.childrenStore.subscribe((newChildrenData) => {
      // Only update if data actually changed
      if (JSON.stringify(childrenData) !== JSON.stringify(newChildrenData)) {
        childrenData = newChildrenData;
        // Setup unified data subscriptions without triggering updates
        setupNodeSubscriptions(false);
        // Schedule a single tree data update
        requestDataUpdate();
      }
    });

    unsubscribeName = store.nameStore.subscribe((newName) => {
      if (name !== newName) {
        name = newName || "";
        requestDataUpdate();
      }
    });

    unsubscribePoints = store.pointsStore.subscribe((newPoints) => {
      if (points !== newPoints) {
        points = typeof newPoints === "number" ? newPoints : 0;
        requestDataUpdate();
      }
    });

    // Subscribe to contributors for the current node
    unsubscribeContributors = store.contributorsStore.subscribe(
      (newContributors) => {
        // Only update if contributors actually changed
        if (JSON.stringify(contributors) !== JSON.stringify(newContributors)) {
          contributors = newContributors;
          // Update hierarchy data if it exists without triggering a full rebuild
          if (hierarchy && hierarchy.data) {
            hierarchy.data.contributors = newContributors.map(([id]) => id);
            // Only update visualization if needed and not in update cycle
            requestVisualUpdate();
          }
        }
      },
    );

    // Initial update of visualization (only once after all data is available)
    requestDataUpdate();
  }

  // Throttled request for data update
  function requestDataUpdate() {
    const now = Date.now();
    if (now - lastUpdateTimestamp < UPDATE_THROTTLE_MS) {
      // If we're updating too frequently, schedule a delayed update
      if (pendingUpdateTimeout !== null) {
        clearTimeout(pendingUpdateTimeout);
      }
      pendingUpdateTimeout = setTimeout(() => {
        lastUpdateTimestamp = Date.now();
        updateTreeData();
      }, UPDATE_THROTTLE_MS);
    } else {
      // Otherwise update immediately
      lastUpdateTimestamp = now;
      updateTreeData();
    }
  }

  // Throttled request for visualization update
  function requestVisualUpdate() {
    const now = Date.now();
    if (now - lastUpdateTimestamp < UPDATE_THROTTLE_MS) {
      // If we're updating too frequently, schedule a delayed update
      if (pendingVisualTimeout !== null) {
        clearTimeout(pendingVisualTimeout);
      }
      pendingVisualTimeout = setTimeout(() => {
        lastUpdateTimestamp = Date.now();
        updateVisualization();
      }, UPDATE_THROTTLE_MS);
    } else {
      // Otherwise update immediately
      lastUpdateTimestamp = now;
      updateVisualization();
    }
  }

  // Setup subscriptions for all nodes in a unified way
  function setupNodeSubscriptions(triggerUpdates = true) {
    // Clean up existing subscriptions
    nodeSubscriptions.forEach((unsub) => unsub());
    nodeSubscriptions = [];

    // Subscribe to current node and all children
    if (childrenData && childrenData.length) {
      childrenData.forEach(([childId]) => {
        const childStore = store.getChild(childId);

        // Pre-fetch current contributors to avoid triggering updates
        const currentContributors = get(childStore.contributorsStore) || [];

        // Subscribe to child contributors
        const contributorSub = childStore.contributorsStore.subscribe(
          (contributors) => {
            // Skip initial update to avoid recursion (first call after subscription)
            if (
              JSON.stringify(contributors) ===
              JSON.stringify(currentContributors)
            ) {
              return;
            }

            // Only update hierarchy and visualization if we actually have hierarchy data
            if (hierarchy) {
              // Find node in hierarchy
              const node = hierarchy
                .descendants()
                .find((n) => n.data._key === childId);
              if (node) {
                // Update contributors directly in the node data
                node.data.contributors = contributors.map(([id]) => id);

                // Only update visualization if the affected node is visible
                if (currentView) {
                  const isVisible =
                    currentView === root ||
                    (currentView.children &&
                      currentView.children.some(
                        (c) => c.data._key === childId,
                      ));

                  if (isVisible && triggerUpdates) {
                    requestVisualUpdate();
                  }
                }
              }
            }
          },
        );

        // Store subscription for cleanup
        nodeSubscriptions.push(contributorSub);
      });
    }
  }

  // Update tree data and visualization
  function updateTreeData() {
    // Prevent recursive calls
    if (isUpdatingTreeData) return;
    isUpdatingTreeData = true;

    console.log("TreeMap updateTreeData called", {
      time: new Date().toISOString(),
    });

    try {
      if (!name || childrenData === undefined) {
        return;
      }

      // Collect all necessary data upfront to avoid triggering reactive updates during processing
      const nodeDataMap = new Map();
      const hierarchyData: RecNode = {
        name,
        points,
        _key: store.path[store.path.length - 1] || "root",
        contributors: contributors.map(([id]) => id), // Use the already fetched data
        children: childrenData.map(([id, data]) => {
          // Get child store but don't subscribe
          const childStore = store.getChild(id);

          // Get contributors - use the map if available to avoid redundant fetches
          let childContributorIds;
          if (nodeDataMap.has(id)) {
            childContributorIds = nodeDataMap.get(id);
          } else {
            // Fetch synchronously without subscription
            const childContributors = get(childStore.contributorsStore) || [];
            childContributorIds = childContributors.map(([cid]) => cid);
            nodeDataMap.set(id, childContributorIds);
          }

          return {
            ...data,
            _key: id,
            // Ensure points is a number
            points: typeof data.points === "number" ? data.points : 0,
            // Add contributors directly to the node structure
            contributors: childContributorIds,
          } satisfies RecNode;
        }),
      };

      node = hierarchyData as RecNode;

      console.log("Created unified hierarchy data:", hierarchyData);

      // Create hierarchy with the complete data set
      hierarchy = d3.hierarchy<RecNode>(hierarchyData as RecNode).sum((d) => {
        const points = d.points || 0;
        return points;
      });

      // Apply treemap layout with improved error handling
      try {
        const treemap = d3.treemap<RecNode>().tile(tile as any);

        root = treemap(
          hierarchy as any,
        ) as d3.HierarchyRectangularNode<RecNode>;

        // Only set currentView to root if we don't have a currentView
        if (!currentView) {
          currentView = root;
        }

        // Reset domains to full extent
        x.domain([root.x0, root.x1]);
        y.domain([root.y0, root.y1]);

        // Update visualization (but only if we have SVG)
        if (svg && group) {
          updateVisualization();
        }
      } catch (err) {
        console.error("Error applying treemap layout:", err);
      }
    } catch (err) {
      console.error("Error creating hierarchy:", err);
    } finally {
      // Always reset update flag
      isUpdatingTreeData = false;
    }
  }

  // Helper function to update visualization without full data rebuild
  function updateVisualization() {
    if (!svg || !group || !currentView) return;

    // Prevent recursive visualization updates
    if (isUpdatingVisualization) return;
    isUpdatingVisualization = true;

    try {
      console.log("Updating visualization", { time: new Date().toISOString() });

      // Remove old group and create a new one
      group.remove();
      group = svg.append("g");

      // Render with currentView
      group.call(render, currentView);
    } catch (err) {
      console.error("Error updating visualization:", err);
    } finally {
      isUpdatingVisualization = false;
    }
  }

  // Create timeout references for cleanup
  let pendingUpdateTimeout: ReturnType<typeof setTimeout> | null = null;
  let pendingVisualTimeout: ReturnType<typeof setTimeout> | null = null;

  // Initialize on mount
  onMount(() => {
    console.log("TreeMap component mounting", { store, width, height });

    if (!width) width = window.innerWidth;
    if (!height) height = window.innerHeight;

    // console.log('TreeMap dimensions', { width, height });

    // Set up scales
    x.rangeRound([0, width]);
    y.rangeRound([0, height]);

    // Create SVG
    svg = d3
      .select(treeMap)
      .append("svg")
      .attr("viewBox", [0.5, -50.5, width, height + 50])
      .attr("width", width)
      .attr("height", height + 50)
      .style("font", "10px sans-serif");

    // console.log('TreeMap SVG created', { svg: svg.node() });

    // Create initial group
    group = svg.append("g");

    // Subscribe to store data
    subscribeToStoreData();

    // Expose element accessor
    treeMap.getElement = getElement;

    // Set up window resize handler
    window.addEventListener("resize", handleResize);

    return () => {
      window.removeEventListener("resize", handleResize);
      destroy();
    };
  });

  // Clean up on destroy
  onDestroy(() => {
    // console.log("TreeMap: cleanup");

    // Clear all intervals and timeouts
    if (growthInterval) {
      clearInterval(growthInterval);
      growthInterval = null;
    }

    if (growthTimeout) {
      clearTimeout(growthTimeout);
      growthTimeout = null;
    }

    // Clean up all TagPill components
    if (nodeTagComponents.length > 0) {
      nodeTagComponents.forEach((component) => {
        if (component) {
          try {
            unmount(component);
          } catch (e) {
            console.error("Error unmounting TagPill component:", e);
          }
        }
      });
      nodeTagComponents = [];
    }
  });

  // Add a debug log function for the store state
  function logStoreState(location: string) {
    console.log(`[Store State at ${location}]`, {
      storePath: store?.path.join("/"),
      originalStorePath: store?.path.join("/"),
      currentViewName: currentView?.data?.name || "undefined",
      sameReference: store === store,
      rootName: root?.data?.name || "undefined",
    });
  }

  // Update the reactive store handling with protection against recursion
  $: if (store) {
    logStoreState("Before store reactive update");

    // Use a store identity check to avoid resubscribing if just internal data changed
    if (!previousStore || previousStore !== store) {
      console.log("TreeMap store reference changed - resubscribing...");

      // Store the reference for identity comparison
      previousStore = store;

      // Set up new subscriptions with the updated store
      if (svg) {
        subscribeToStoreData();
      }
    } else {
      console.log(
        "TreeMap detected store update but reference unchanged - skipping resubscription",
      );
    }

    logStoreState("After store reactive update");
  }

  // Track store reference for identity check
  let previousStore: RecognitionStore | null = null;

  // Update when dimensions change
  $: if (svg && (width || height)) {
    update(width, height);
  }

  function position(
    group: d3.Selection<any, any, any, any>,
    root: d3.HierarchyRectangularNode<RecNode>,
  ) {
    // Update all g elements except the navigation buttons
    group
      .selectAll(
        "g:not(.home-button):not(.add-button):not(.peer-button):not(.delete-button):not(.inventory-button)",
      )
      .attr("transform", (d: any) => {
        if (!d || typeof d.x0 === "undefined") return "";
        return d === root
          ? `translate(0,-50)`
          : `translate(${x(d.x0)},${y(d.y0)})`;
      });

    // Update home button position (keep it at the left)
    group.selectAll(".home-button").attr("transform", "translate(20, 25)");

    // Update peer button position (keep it to the left of the add button)
    group.selectAll(".peer-button").attr("transform", (d: any) => {
      if (!d) return "";
      const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
      return `translate(${rectWidth - 60}, 25)`;
    });

    // Update add button position (keep it at the right)
    group.selectAll(".add-button").attr("transform", (d: any) => {
      if (!d) return "";
      const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
      return `translate(${rectWidth - 20}, 25)`;
    });

    group.selectAll(".delete-button").attr("transform", (d: any) => {
      if (!d) return "";
      const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
      return `translate(20, 25)`;
    });

    group.selectAll(".inventory-button").attr("transform", (d: any) => {
      if (!d) return "";
      const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
      return `translate(${rectWidth - 100}, 25)`;
    });

    group
      .selectAll("rect")
      .attr("width", (d: any) => {
        if (!d || typeof d.x0 === "undefined") return 0;
        return d === root ? width : x(d.x1) - x(d.x0);
      })
      .attr("height", (d: any) => {
        if (!d || typeof d.y0 === "undefined") return 0;
        return d === root ? 50 : y(d.y1) - y(d.y0);
      });

    // Update type tags container position
    group
      .selectAll(".contributor-tags-container")
      .style("opacity", () => (isGrowingActiveState() ? 0 : 1)) // Hide during growing
      .style("transition", "opacity 0.15s ease") // Add smooth transition
      .attr("transform", (d: any) => {
        if (!d || typeof d.x0 === "undefined") return "";
        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);

        // Position centered horizontally, below the text
        // Calculate vertical position based on text lines
        const textLines =
          d === root
            ? 1
            : d.data && d.data.name
              ? d.data.name.split(/(?=[A-Z][^A-Z])/g).length
              : 1;

        // Offset from center - move down by half the text height plus padding
        const fontSize = calculateFontSize(d, root, x, y, currentView);
        const fontSizeNumber = typeof fontSize === "number" ? fontSize : 12; // Fallback to reasonable default
        const verticalOffset = (textLines * 1.2 * fontSizeNumber) / 2 + 10;

        return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
      });
  }

  // Keep track of TagPill components with their tag IDs to prevent duplicates
  const tagPillMap = new Map<string, any>();

  // The render function implements the treemap visualization
  function render(
    group: d3.Selection<SVGGElement, unknown, null, undefined>,
    root: d3.HierarchyRectangularNode<RecNode>,
  ) {
    // Clean up existing TagPill components before creating new ones
    if (nodeTagComponents.length > 0) {
      // console.log(`Cleaning up ${nodeTagComponents.length} existing TagPill components`);
      nodeTagComponents.forEach((component) => {
        if (component) {
          try {
            unmount(component);
          } catch (e) {
            // console.error("Error unmounting TagPill component:", e);
          }
        }
      });
      nodeTagComponents = [];
      tagPillMap.clear();
    }

    // Ensure we include all children of the root
    const nodeData = root.children ? [root, ...root.children] : [root];

    // Create node selection
    const node = group
      .selectAll<SVGGElement, d3.HierarchyRectangularNode<RecNode>>("g")
      .data(nodeData)
      .join("g")
      .attr("class", "treemap-node") // Add class for CSS targeting
      .filter((d) => {
        const isRoot = d === root;
        const hasPoints =
          d.data && typeof d.data.points === "number" && d.data.points > 0;
        const result = isRoot || hasPoints;
        // console.log(`Node ${d.data.name || 'unnamed'} filtered: ${result}`);
        return result;
      });

    node.append("title").text(resolvedPathNames?.join("/"));

    node.selectAll("text").remove();

    // Debugging the rect creation
    // console.log('Adding rectangles to nodes');

    node
      .append("rect")
      .attr("id", (d) => {
        (d as any).leafUid = uid("leaf");

        // Also add the node's actual ID to make it findable for drag-drop
        if (d === root) return (d as any).leafUid.id;
        if (d.data && d.data.id) return `leaf-${d.data.id}`;
        if (d.data && d.data._key) return `leaf-${d.data._key}`;
        return (d as any).leafUid.id;
      })
      .attr("fill", (d) => {
        if (d === root) return "#fff";
        return getColorForName((d.data && d.data.name) || "");
      })
      .attr("stroke", (d) => {
        /* Only add special outline for nodes with non-contributor children - now in blue */
        return d.data && d.data.hasDirectContributionChild ? "#2196f3" : "#fff";
      })
      .attr("stroke-width", (d) => {
        // Only make stroke wider for nodes with non-contributor children
        return d.data && d.data.hasDirectContributionChild ? "3" : "2";
      });

    node
      .append("clipPath")
      .attr("id", (d) => {
        (d as any).clipUid = uid("clip");
        return (d as any).clipUid.id;
      })
      .append("use")
      .attr("xlink:href", (d) => (d as any).leafUid.href);

    node
      .append("text")
      .attr("clip-path", (d) => (d as any).clipUid)
      .attr("font-weight", (d) => (d === root ? "bold" : null))
      .style("user-select", "none")
      .style("-webkit-user-select", "none")
      .style("-moz-user-select", "none")
      .style("-ms-user-select", "none")
      .attr("transform", (d) => {
        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
        return `translate(${rectWidth / 2},${rectHeight / 2})`;
      })
      .style("text-anchor", "middle")
      .style("dominant-baseline", "middle")
      .style("font-size", (d) => {
        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);
        return calculateFontSize(d, root, x, y, currentView) + "px";
      })
      .selectAll("tspan")
      .data((d) => {
        if (d === root) return [resolvedPathNames?.join("/")];
        return d.data && d.data.name
          ? d.data.name.split(/(?=[A-Z][^A-Z])/g)
          : [""];
      })
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
      .on("click", async (event, d) => {
        event.stopPropagation(); // Prevent node click/zoom

        // Get the parent node data
        const parentNode = d3
          .select((event.target as any).parentNode)
          .datum() as d3.HierarchyRectangularNode<RecNode>;
        if (!parentNode || parentNode === root) return; // Don't edit root node

        // Skip text editing if in delete mode
        if (deleteMode) return;

        // Create an input element
        const textElement = event.target;
        const textBox = textElement.getBoundingClientRect();

        const foreignObject = group
          .append("foreignObject")
          .attr("x", x(parentNode.x0))
          .attr("y", y(parentNode.y0))
          .attr("width", x(parentNode.x1) - x(parentNode.x0))
          .attr("height", y(parentNode.y1) - y(parentNode.y0))
          .style("pointer-events", "none");

        // Use CSS classes instead of inline styles
        const container = foreignObject
          .append("xhtml:div")
          .attr("class", "new-node-container");

        const input = container
          .append("xhtml:input")
          .attr("class", "new-node-input")
          .style("font-size", d3.select(textElement).style("font-size"))
          .attr("value", parentNode.data.name || "");

        // Focus the input
        const inputElement = input.node() as HTMLInputElement;
        if (inputElement) {
          inputElement.focus();
          inputElement.select();
        }

        // Handle input completion
        const completeEdit = async () => {
          const newName = inputElement?.value || parentNode.data.name;

          // Remove the foreignObject safely
          try {
            // First detach event listeners
            input.on("keydown", null);
            input.on("blur", null);

            // Remove the foreignObject if it exists
            if (foreignObject) {
              foreignObject.remove();
            }
          } catch (err) {
            console.error("Error cleaning up edit UI:", err);
          }

          // Update the name in the store if changed
          if (newName && newName !== parentNode.data.name) {
            if (parentNode.data._key) {
              const childStore = store.getChild(parentNode.data._key);
              await childStore.updateName(newName);
            }
          }
        };

        // Handle enter key and blur
        input.on("keydown", (event: KeyboardEvent) => {
          if (event.key === "Enter") {
            event.preventDefault();
            completeEdit();
          } else if (event.key === "Escape") {
            event.preventDefault();
            // Check if the foreignObject still exists before trying to remove it
            const foreignNode = foreignObject.node();
            if (foreignNode && foreignNode.parentNode) {
              foreignObject.remove();
            }
          }
        });

        input.on("blur", completeEdit);
      });

    group.call(position, root);

    // Replace contributors container with tag pills container
    const contributorsContainer = node
      .append("g")
      .attr("class", "contributor-tags-container")
      .style("opacity", () => (isGrowingActiveState() ? 0 : 1)) // Hide during growing
      .style("transition", "opacity 0.15s ease") // Add smooth transition
      .attr("transform", (d) => {
        const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
        const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);

        // Position centered horizontally, below the text
        // Calculate vertical position based on text lines
        const textLines =
          d === root
            ? 1
            : d.data && d.data.name
              ? d.data.name.split(/(?=[A-Z][^A-Z])/g).length
              : 1;

        // Offset from center - move down by half the text height plus padding
        const fontSize = calculateFontSize(d, root, x, y, currentView);
        const fontSizeNumber = typeof fontSize === "number" ? fontSize : 12; // Fallback to reasonable default
        const verticalOffset = (textLines * 1.2 * fontSizeNumber) / 2 + 10;

        return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
      });

    // Add tag pills for each type
    contributorsContainer.each(function (
      d: d3.HierarchyRectangularNode<RecNode>,
    ) {
      if (!d || d === root) return; // Skip for root node

      const container = d3.select(this);

      // Calculate rect dimensions for space check
      const rectWidth = d === root ? width : x(d.x1) - x(d.x0);
      const rectHeight = d === root ? 50 : y(d.y1) - y(d.y0);

      // Skip all additions if rect is too small
      if (rectWidth < 60 || rectHeight < 60) return;

      // Get contributors directly from the node data in our unified hierarchy
      let contributorIds: string[] = [];

      if (d.data && d.data.contributors) {
        // Use contributors from our unified data model
        contributorIds = Array.isArray(d.data.contributors)
          ? d.data.contributors
          : Object.keys(d.data.contributors);

        console.log(
          "[TreeMap] Node contributors from unified model:",
          d.data.name,
          contributorIds,
        );
      }

      // Create a tag wrapper to hold all pills in a flex layout
      const tagWrapper = container
        .append("foreignObject")
        .attr("class", "tag-wrapper-container")
        .attr("x", -rectWidth / 2 + 10) // Adjust left position with padding
        .attr("y", 0)
        .attr("width", rectWidth - 20) // Full width minus padding
        .attr("height", 60) // Fixed height to contain a couple rows of tags
        .append("xhtml:div")
        .attr("class", "tag-wrapper");

      // Add the "Add contributor" button
      const addContributorButton = tagWrapper
        .append("div")
        .attr("class", "add-contributor-button")
        .text("+")
        .on("click", (event) => {
          event.stopPropagation();

          // Show our Svelte dropdown component
          showUserDropdown = true;
          dropdownPosition = {
            x: event.clientX - 100,
            y: event.clientY + 20,
          };
          dropdownNodeData = d.data;
        });

      // Log the number of TagPills being created
      if (contributorIds.length > 0) {
        console.log(
          `Creating ${contributorIds.length} TagPills for node ${d.data.name || "unknown"}`,
        );
      }

      // Add existing tags as pills
      contributorIds.forEach((contributorId: string, index: number) => {
        // Check if we already have a component for this tag ID to prevent duplicates
        const nodeKey = `${d.data.name || "node"}-${contributorId}`;
        if (tagPillMap.has(nodeKey)) {
          return;
        }

        console.log(
          `Creating TagPill ${index + 1}/${contributorIds.length} for ${contributorId} on node ${d.data.name}`,
        );

        // Create a div to host our Svelte TagPill component
        const pillContainer = tagWrapper
          .append("xhtml:div")
          .attr("class", "pill-container");

        try {
          // Create the TagPill component using Svelte 5's mount API
          const tagPillComponent = mount(TagPill, {
            target: pillContainer.node() as HTMLElement,
            props: {
              userId: contributorId,
              truncateLength: 10,
              removable: true,
              onClick: (clickedUserId: string) => {
                // Get the current resolved name for logging
                const currentName = usersMap.has(clickedUserId)
                  ? usersMap.get(clickedUserId)!
                  : clickedUserId;
                console.log("Tag clicked:", currentName);
              },
              onRemove: (clickedUserId: string) => {
                // Remove contributor using the appropriate store API
                const nodeKey = d.data._key;
                if (!nodeKey) {
                  console.error("Cannot remove contributor: node has no _key");
                  return;
                }

                // Get the appropriate store
                const targetStore =
                  nodeKey === store.path[store.path.length - 1]
                    ? store
                    : store.getChild(nodeKey);

                // Remove the contributor
                targetStore
                  .removeContributor(clickedUserId)
                  .then(() => {
                    console.log(
                      `Removed contributor ${clickedUserId} from node ${nodeKey}`,
                    );

                    // Remove tag pill with animation
                    pillContainer
                      .style("opacity", "0")
                      .style("transform", "scale(0.8)")
                      .remove();

                    // Remove from map
                    tagPillMap.delete(nodeKey);
                  })
                  .catch((err) => {
                    console.error("Error removing contributor:", err);
                  });
              },
            },
          });

          // Store component in map with unique key
          tagPillMap.set(nodeKey, tagPillComponent);
          // Keep track of components to destroy them when appropriate
          nodeTagComponents.push(tagPillComponent);
        } catch (e) {
          console.error("Error creating TagPill component:", e);
        }
      });
    });

    // Add touch state tracking at the top
    let touchStartTime = 0;
    let isTouching = false;
    // Use the already declared activeNode variable

    // Add function to check if we're in a contributor tree
    function isInContributorTree() {
      let tempNode: d3.HierarchyRectangularNode<RecNode> | null = currentView;
      while (tempNode) {
        if (tempNode.data === root.data) {
          return false;
        }
        tempNode = tempNode.parent || null;
      }
      return true;
    }

    node
      .filter((d) => true)
      .attr("class", "clickable")
      .on("contextmenu", (event) => {
        event.preventDefault(); // This prevents the context menu from showing up
      })
      .on(
        "mousedown touchstart",
        (event, d: d3.HierarchyRectangularNode<RecNode>) => {
          event.preventDefault();

          // Always set touch state for navigation purposes
          isTouching = true;
          touchStartTime = Date.now();
          activeNode = d;

          // Only proceed with growth/shrink if not in contributor tree
          if (!isInContributorTree()) {
            // Clear any existing growth state
            if (growthInterval) clearInterval(growthInterval as any);
            if (growthTimeout) clearTimeout(growthTimeout);
            isGrowing = false;

            if (d !== root) {
              // Determine if this is a right-click or two-finger touch
              const isShrinking =
                event.type === "mousedown"
                  ? event.button === 2 // right click
                  : event.touches.length === 2; // two finger touch

              growthTimeout = setTimeout(() => {
                // Only start growing/shrinking if still touching the same node
                if (isTouching && activeNode === d) {
                  isGrowing = true;
                  // Set the app's growing active flag
                  (window as any).isGrowingActive = true;

                  // Hide contributor tags immediately when growing starts
                  group
                    .selectAll(".contributor-tags-container")
                    .style("opacity", 0);

                  growthInterval = setInterval(() => {
                    // Only continue if still touching
                    if (!isTouching) {
                      clearInterval(growthInterval as any);
                      isGrowing = false;
                      growthInterval = null;
                      // Clear the app's growing active flag
                      (window as any).isGrowingActive = false;

                      // Ensure contributor tags are shown when click finishes
                      group
                        .selectAll(".contributor-tags-container")
                        .style("opacity", 1);

                      // Save the final points value to Gun
                      if (d.data) {
                        // console.log(`Growth stopped. Saving final points value: ${d.data.points}`);
                        saveNodePoints(d, d.data.points || 0);
                      }

                      return;
                    }

                    // Calculate growth/shrink amount
                    const rate = isShrinking ? SHRINK_RATE(d) : GROWTH_RATE(d);
                    const currentPoints =
                      d.data && typeof d.data.points === "number"
                        ? d.data.points
                        : 0;
                    const newPoints = Math.max(0, currentPoints + rate); // Prevent negative points
                    if (isNaN(newPoints)) {
                      // console.error('Growth calculation resulted in NaN:', {
                      //     currentPoints,
                      //     rate,
                      //     isShrinking
                      // });
                      return;
                    }

                    // Only update if points actually changed
                    if (d.data && newPoints !== currentPoints) {
                      d.data.points = newPoints;

                      // Log points changes (helpful for debugging)
                      if (Math.floor(newPoints) !== Math.floor(currentPoints)) {
                        // console.log(`Growing ${d.data.name}: ${Math.floor(currentPoints)} -> ${Math.floor(newPoints)}`);
                      }
                    }

                    // Apply treemap
                    const treemap = d3.treemap().tile(tile as any);
                    treemap(hierarchy as any);

                    // Update visualization including contributor indicators
                    const nodes = group
                      .selectAll("g")
                      .filter((node) => node !== root);

                    // Existing transitions
                    nodes
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("transform", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        return nd === root
                          ? `translate(0,-50)`
                          : `translate(${x(nd.x0)},${y(nd.y0)})`;
                      });

                    // Transition rectangles
                    nodes
                      .select("rect")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("width", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        return nd === root
                          ? width
                          : Math.max(0, x(nd.x1) - x(nd.x0));
                      })
                      .attr("height", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        return nd === root
                          ? 50
                          : Math.max(0, y(nd.y1) - y(nd.y0));
                      });

                    // Update text positions
                    nodes
                      .select("text")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("transform", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        const rectHeight =
                          nd === root ? 50 : y(nd.y1) - y(nd.y0);
                        return `translate(${rectWidth / 2},${rectHeight / 2})`;
                      })
                      .style("font-size", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        const rectHeight =
                          nd === root ? 50 : y(nd.y1) - y(nd.y0);
                        return (
                          calculateFontSize(nd, root, x, y, currentView) + "px"
                        );
                      });

                    // Update tag container positions
                    nodes
                      .select(".contributor-tags-container")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("transform", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        const rectHeight =
                          nd === root ? 50 : y(nd.y1) - y(nd.y0);

                        // Position centered horizontally, below the text
                        // Calculate vertical position based on text lines
                        const textLines =
                          nd === root
                            ? 1
                            : nd.data && nd.data.name
                              ? nd.data.name.split(/(?=[A-Z][^A-Z])/g).length
                              : 1;

                        // Offset from center - move down by half the text height plus padding
                        const fontSize = calculateFontSize(
                          nd,
                          root,
                          x,
                          y,
                          currentView,
                        );
                        const fontSizeNumber =
                          typeof fontSize === "number" ? fontSize : 12; // Fallback to reasonable default
                        const verticalOffset =
                          (textLines * 1.2 * fontSizeNumber) / 2 + 10;

                        return `translate(${rectWidth / 2}, ${rectHeight / 2 + verticalOffset})`;
                      });

                    // Update foreign object tag wrappers
                    nodes
                      .select(".tag-wrapper")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("x", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        return -rectWidth / 2 + 10; // Adjust left position with padding
                      })
                      .attr("width", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        return rectWidth - 20; // Full width minus padding
                      });

                    // Update peer button position during growth
                    group
                      .select(".peer-button")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("transform", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        return `translate(${rectWidth - 60}, 25)`;
                      });

                    // Update add button position during growth
                    group
                      .select(".add-button")
                      .transition()
                      .duration(GROWTH_TICK)
                      .attr("transform", (d) => {
                        const nd = d as d3.HierarchyRectangularNode<RecNode>;
                        const rectWidth =
                          nd === root ? width : x(nd.x1) - x(nd.x0);
                        return `translate(${rectWidth - 20}, 25)`;
                      });
                  }, GROWTH_TICK);
                }
              }, GROWTH_DELAY);
            }
          }
        },
        { passive: false },
      )
      .on(
        "mouseup touchend touchcancel",
        (event, d) => {
          // Only handle if not in contributor tree
          if (!isInContributorTree()) {
            // Clear all states
            isTouching = false;

            // Save the current points if we were growing
            if (isGrowing && activeNode && activeNode.data) {
              // console.log(`Touch ended. Saving points value: ${activeNode.data.points}`);
              saveNodePoints(activeNode, activeNode.data.points || 0);
            }

            activeNode = null;

            // Stop growth
            if (growthTimeout) clearTimeout(growthTimeout);
            if (growthInterval) clearInterval(growthInterval as any);
            growthInterval = null;
            isGrowing = false;

            // Clear the app's growing active flag
            (window as any).isGrowingActive = false;

            // Ensure contributor tags are shown when click finishes
            group.selectAll(".contributor-tags-container").style("opacity", 1);
          }
        },
        { passive: true },
      )
      .on(
        "click touchend",
        (event, d) => {
          event.preventDefault();

          const touchDuration = Date.now() - touchStartTime;
          // console.log('Click detected on:', d.data.name);
          // console.log('Is root?', d === root);
          // console.log('Has parent?', d.parent ? 'yes' : 'no');
          // console.log('Touch duration:', touchDuration);
          // console.log('Is growing?', isGrowing);

          // Allow navigation (zooming) regardless of tree
          if (touchDuration < GROWTH_DELAY && !isGrowing) {
            if (d === root && !deleteMode) {
              console.log("Attempting zoom out from:", d.data.name);
              zoomout(); // Use internal zoom function
            } else if (d !== root && !d.data.isContribution && !deleteMode) {
              // Check isContribution directly
              // console.log('Attempting zoom in to:', d.data.name);
              zoomin(d.data && d.data._key ? d.data._key : ""); // Use internal zoom function
            } else if (deleteMode && d !== root) {
              console.log("Deleting node:", d.data.name);
              // Get the node ID from data
              const nodeId = d.data._key;
              if (nodeId && deleteMode) {
                // Confirm deletion
                if (confirm(`Delete "${d.data.name || "Unnamed node"}"?`)) {
                  // Remove the node using the store method
                  store
                    .removeChild(nodeId)
                    .then(() => {
                      // Exit delete mode after successful deletion

                      showToastMessage("Node deleted successfully", "success");
                      // Show success message
                      deleteMode = false;
                    })
                    .catch((err) => {
                      console.error("Error deleting node:", err);

                      // Show error message
                      showToastMessage("Error deleting node", "warning");
                    });
                }
              }
            }
          } else {
            // console.log('Navigation blocked because:',
            //     touchDuration >= GROWTH_DELAY ? 'touch too long' : 'growing active');
          }

          // Clear states only if not in contributor tree
          if (!isInContributorTree()) {
            isTouching = false;
            activeNode = null;
            isGrowing = false;

            // Ensure app's growing active flag is cleared
            (window as any).isGrowingActive = false;

            // Ensure contributor tags are shown when click finishes
            group.selectAll(".contributor-tags-container").style("opacity", 1);
          }
        },
        { passive: false },
      );

    // Check if view is empty (no children or all children have 0 points)
    if (
      (!root.children || root.children.length === 0) &&
      !(root.data && root.data.id === root.data._key)
    ) {
      group
        .append("text")
        .attr("class", "helper-text")
        .attr("text-anchor", "middle")
        .attr("dominant-baseline", "middle")
        .attr("x", width / 2)
        .attr("y", height / 2)
        .style("font-size", "24px")
        .style("fill", "#666")
        .style("pointer-events", "none")
        .style("user-select", "none")
        .text("Add Values / Contributors");
    }

    // After creating the node groups and before position call
    node
      .filter((d) => d === root) // Only for the root navigation rectangle
      .each(function (d) {
        // Check if we're in a contributor tree (no path to original data)
        let isContributorTree = true;
        let currentNode: d3.HierarchyRectangularNode<RecNode> | null = d;
        while (currentNode) {
          if (currentNode.data === root.data) {
            // data is the original root passed to createTreemap
            isContributorTree = false;
            break;
          }
          currentNode = currentNode.parent || null;
        }

        // Add home button only for contributor trees
        if (isContributorTree) {
          // Add home button
          d3.select(this)
            .append("g")
            .attr("class", "home-button")
            .attr("transform", "translate(20, 25)") // 25 is 50% of the 50px height
            .style("cursor", "pointer")
            .on("click", (event) => {
              event.stopPropagation();
              // Clear existing content
              group.selectAll("*").remove();

              // Apply treemap layout
              const treemap = d3.treemap().tile(tile as any);
              root = treemap(
                hierarchy as any,
              ) as d3.HierarchyRectangularNode<RecNode>;
              currentView = root as d3.HierarchyRectangularNode<RecNode>;

              // Reset domains
              x.domain([root.x0, root.x1]);
              y.domain([root.y0, root.y1]);

              // Render new view
              render(group, root);
            })
            .append("text")
            .attr("fill", "#000")
            .attr("font-size", "20px")
            .attr("dominant-baseline", "middle") // Vertically center the text
            .text("🏠"); // Unicode home emoji
        }
        // Only add navigation buttons on our user's tree views (not contributor trees)
        if (!isContributorTree) {
          const rectWidth = d === root ? width : x(d.x1) - x(d.x0);

          // Add settings button (leftmost)
          d3.select(this)
            .append("g")
            .attr("class", "inventory-button")
            .attr("transform", `translate(${rectWidth - 100}, 25)`)
            .style("cursor", "pointer")
            .on("click", (event) => {
              event.stopPropagation();
              // Add settings button click handler here
            })
            .append("text")
            .attr("fill", "#000")
            .attr("font-size", "20px")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "middle")
            .text("🎒");

          // Add peer button (on the left of the plus button)
          d3.select(this)
            .append("g")
            .attr("class", "peer-button")
            .attr("transform", `translate(${rectWidth - 60}, 25)`) // Position it to the left of plus button
            .style("cursor", "pointer")
            .on("click", (event) => {
              event.stopPropagation();

              // Show login component
              showLogin = true;
            })
            .append("text")
            .attr("fill", "#000")
            .attr("font-size", "20px")
            .attr("text-anchor", "middle") // Center the text horizontally
            .attr("dominant-baseline", "middle") // Center the text vertically
            .text("🔍");

          // Add plus button for adding values
          d3.select(this)
            .append("g")
            .attr("class", "add-button")
            .attr("transform", `translate(${rectWidth - 20}, 25)`)
            .style("cursor", "pointer")
            .on("click", async (event) => {
              event.stopPropagation();

              // Calculate 10% of current level's total points
              const currentLevelPoints = currentView.children
                ? currentView.children.reduce(
                    (sum, node) => sum + (node.data.points || 0),
                    0,
                  )
                : currentView.data.points || 0;
              const newNodePoints = Math.max(1, currentLevelPoints * 0.1);

              try {
                // Add debug logging to show which store we're using
                console.log(
                  "Adding child node using store with path:",
                  store.path.join("/"),
                );

                // Create new node with temporary name and calculated points
                const newNodeId = await store.addChild(
                  "Undefined",
                  newNodePoints,
                );

                console.log("Child node created with ID:", newNodeId);

                // Set up a one-time subscription to childrenStore to detect when the new node appears
                const unsubscribe = store.childrenStore.subscribe(
                  (children) => {
                    // Check if our new node exists in children
                    const newNodeExists = children.some(
                      ([id]) => id === newNodeId,
                    );

                    if (newNodeExists) {
                      unsubscribe(); // Clean up subscription
                      console.log("New node appeared in children collection");

                      // Find the newly created node's group
                      const newNodeGroup = group
                        .selectAll("g")
                        .filter(
                          (d: any) => d.data && d.data._key === newNodeId,
                        );

                      if (!newNodeGroup.empty()) {
                        const nodeData =
                          newNodeGroup.datum() as d3.HierarchyRectangularNode<RecNode>;

                        // Create the edit input immediately
                        const foreignObject = group
                          .append("foreignObject")
                          .attr("x", x(nodeData.x0))
                          .attr("y", y(nodeData.y0))
                          .attr("width", x(nodeData.x1) - x(nodeData.x0))
                          .attr("height", y(nodeData.y1) - y(nodeData.y0))
                          .style("pointer-events", "none");

                        const container = foreignObject
                          .append("xhtml:div")
                          .attr("class", "new-node-container");

                        const input = container
                          .append("xhtml:input")
                          .attr("class", "new-node-input")
                          .style(
                            "font-size",
                            calculateFontSize(
                              nodeData,
                              root,
                              x,
                              y,
                              currentView,
                            ) + "px",
                          )
                          .attr("value", "")
                          .attr("placeholder", "Enter name...");

                        // Focus the input immediately
                        const inputElement = input.node() as HTMLInputElement;
                        if (inputElement) {
                          // Use requestAnimationFrame to ensure the input is rendered
                          requestAnimationFrame(() => {
                            inputElement.focus();
                          });
                        }

                        // Handle input completion
                        const completeEdit = async () => {
                          const newName = inputElement?.value;
                          foreignObject.remove();

                          if (newName) {
                            // Update the name in the store
                            const childStore = store.getChild(newNodeId);
                            await childStore.updateName(newName);
                          }
                        };

                        // Handle enter key and blur
                        input.on("keydown", (event: KeyboardEvent) => {
                          if (event.key === "Enter") {
                            event.preventDefault();
                            completeEdit();
                          } else if (event.key === "Escape") {
                            event.preventDefault();
                            // Check if the foreignObject still exists before trying to remove it
                            const foreignNode = foreignObject.node();
                            if (foreignNode && foreignNode.parentNode) {
                              foreignObject.remove();
                            }
                          }
                        });

                        input.on("blur", completeEdit);
                      }
                    }
                  },
                );

                // Clean up subscription after a timeout just in case
                setTimeout(() => unsubscribe(), 5000);
              } catch (err) {
                console.error("Error creating new node:", err);
              }
            })
            .append("text")
            .attr("fill", "#000")
            .attr("font-size", "20px")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "middle")
            .text("➕");

          // Add delete button (trash icon) on the left side
          d3.select(this as Element)
            .append("g")
            .attr("class", "delete-button")
            .attr("transform", `translate(20, 25)`)
            .on("click", (event) => {
              event.stopPropagation();

              // Toggle delete mode
              deleteMode = !deleteMode;

              // Update the appearance of all delete buttons to show the active state
              d3.selectAll(".delete-button text").attr("fill", () =>
                deleteMode ? "#ff4136" : "#000",
              );

              // Show a visual indicator or toast message
              showToastMessage(
                deleteMode
                  ? "Delete mode activated. Click a node to delete it."
                  : "Delete mode deactivated.",
                deleteMode ? "warning" : "success",
              );
            })
            .append("text")
            .attr("fill", "#000")
            .attr("font-size", "20px")
            .attr("text-anchor", "middle")
            .attr("dominant-baseline", "middle")
            .text("🗑️");
        }
      });
  }

  // Internal zoom functions for direct D3 hierarchy node operations
  export function zoomin(childId: string) {
    console.log("ALERT: Internal zoomin called with childId:", childId);
    logStoreState("Before zoom in");

    // Find the target node in the hierarchy
    const target =
      (hierarchy
        .descendants()
        .find(
          (d) => d.data && d.data._key === childId,
        ) as d3.HierarchyRectangularNode<RecNode>) || null;
    if (!target) {
      console.error(`Could not find node with ID: ${childId}`);
      return;
    }

    // Store previous view and update current view
    const prevView = currentView;
    currentView = target as d3.HierarchyRectangularNode<RecNode>;

    // Update store to reflect the new view but don't trigger reactivity
    store = store.getChild(childId);
    console.log("ALERT: Updated store to child path:", store.path.join("/"));

    // Debug after update
    logStoreState("After updating store in zoom in");

    // Create a new group that will replace the old one
    const group0 = group.attr("pointer-events", "none");

    // Update domains for the zoomed-in view
    x.domain([target.x0, target.x1]);
    y.domain([target.y0, target.y1]);

    // Create new visualization with the target node as root for rendering purposes only
    // This critical line keeps the hierarchy and real root intact
    const group1 = (group = svg.insert("g", "*").call(render, target));

    // Make transitions between views
    svg
      .transition()
      .duration(750)
      .call((t: any) => {
        group0
          .transition(t as any)
          .attrTween("opacity", () => d3.interpolate("1", "0") as any)
          .call(position as any, prevView);
      })
      .call((t: any) => {
        group1
          .transition(t as any)
          .attrTween("opacity", () => d3.interpolate("0", "1") as any)
          .call(position as any, target);
      });
  }

  export function zoomout() {
    console.log("ALERT: Internal zoomout called");
    logStoreState("Before zoom out");

    if (!currentView) {
      console.error("No current view to zoom out from");
      return;
    }

    // Store the current view before changing it
    const prevView = currentView;
    if (currentView.parent) {
      currentView = currentView.parent as d3.HierarchyRectangularNode<RecNode>;
    }

    // Update the store asynchronously (getParent is async)
    // But don't trigger any reactivity
    store
      .getParent()
      .then((parentStore) => {
        if (parentStore) {
          console.log(
            "ALERT: Updating store to parent path:",
            parentStore.path.join("/"),
          );
          store = parentStore;
          logStoreState("After updating to parent store");
          // Prevent interaction during transition
          const group0 = group.attr("pointer-events", "none");

          // Update domains for the zoomed-out view
          x.domain([currentView.x0, currentView.x1]);
          y.domain([currentView.y0, currentView.y1]);

          // Create new visualization with the parent as root
          const group1 = (group = svg
            .insert("g", "*")
            .call(render, currentView));

          // Make transitions between views
          svg
            .transition()
            .duration(750)
            .call((t: any) => {
              group0
                .transition(t as any)
                .attrTween("opacity", () => d3.interpolate("1", "0") as any)
                .call(position as any, prevView);
            })
            .call((t: any) => {
              group1
                .transition(t as any)
                .attrTween("opacity", () => d3.interpolate("0", "1") as any)
                .call(position as any, currentView);
            });
        }
      })
      .catch((err) => {
        console.error("ALERT: Error getting parent store:", err);
      });
  }

  // Add a save function to persist points to the store
  async function saveNodePoints(
    node: d3.HierarchyRectangularNode<RecNode>,
    points: number,
  ) {
    if (!node || !node.data) return;

    try {
      // Only save if the points actually changed
      const storePoints = store.pointsStore ? get(store.pointsStore) : 0;

      if (node === root) {
        // Save points to the current store
        // console.log(`Saving root node points: ${points} (was ${storePoints})`);
        await store.updatePoints(points);
      } else if (node.data._key) {
        // This is a child node, get its store and update points
        const childId = node.data._key;
        // console.log(`Saving child node ${node.data.name} (${childId}) points: ${points}`);
        const childStore = store.getChild(childId);
        await childStore.updatePoints(points);
      } else {
        //console.warn("Could not save points - node has no _key:", node);
      }
    } catch (err) {
      console.error("Error saving points to Gun:", err);
    }
  }

  // Dropdown state
  let showUserDropdown = false;
  let dropdownPosition = { x: 0, y: 0 };
  let dropdownNodeData: RecNode | null = null;

  // Login state
  let showLogin = false;

  // Replace the event handler for adding contributor to a node with a more streamlined version
  async function handleAddContributor(userId: string) {
    console.log("Adding contributor:", userId, "to node:", dropdownNodeData);

    // Hide dropdown after selection
    showUserDropdown = false;

    if (!dropdownNodeData) return;

    try {
      // Get the node's key
      const nodeKey = dropdownNodeData._key;
      if (!nodeKey) {
        console.error("Cannot add contributor: node has no _key");
        return;
      }

      // Get the appropriate store for this node
      const targetStore =
        nodeKey === store.path[store.path.length - 1]
          ? store
          : store.getChild(nodeKey);

      // Add the contributor
      await targetStore.addContributor(userId);
      console.log(`Added contributor ${userId} to node ${nodeKey}`);

      // Show success message
      showToastMessage(`Added contributor successfully`, "success");
    } catch (err: unknown) {
      console.error("Error adding contributor:", err);
      // Type guard to avoid TypeScript error
      const errorMessage = err instanceof Error ? err.message : String(err);
      showToastMessage(`Error adding contributor: ${errorMessage}`, "warning");
    }
  }

  // Replace the handleUserSelect function with a simpler function that uses our unified model
  function handleUserSelect(event: CustomEvent<{ id: string; name: string }>) {
    const { id } = event.detail;

    if (!dropdownNodeData) {
      console.error("No node selected for adding contributor");
      return;
    }

    // Call our streamlined handleAddContributor function
    handleAddContributor(id)
      .then(() => {
        console.log(`Successfully added contributor ${id}`);
      })
      .catch((err) => {
        console.error("Error in handleUserSelect:", err);
      });

    // Hide dropdown
    showUserDropdown = false;
  }

  function handleDropdownClose() {
    showUserDropdown = false;
  }

  // Login handlers
  function handleLoginClose() {
    showLogin = false;
  }

  function handleAuthChange(
    event: CustomEvent<{ isAuthenticated: boolean; user: string | null }>,
  ) {
    const { isAuthenticated, user } = event.detail;
    if (isAuthenticated) {
      // Show success message
      showToastMessage(`Logged in as ${user}`, "success");
    }
    // Keep the dialog open to show the welcome message or error
  }

  // Toast state
  let showToast = false;
  let toastMessage = "";
  let toastType: "success" | "warning" = "success";

  // Function to show toast
  function showToastMessage(
    message: string,
    type: "success" | "warning" = "success",
  ) {
    toastMessage = message;
    toastType = type;
    showToast = true;
  }

  // Handle toast close
  function handleToastClose() {
    showToast = false;
  }

  // Add this in the script section, with other state variables
  let showNodeInfo = true;
</script>

<div bind:this={treeMap} id="treemap-container"></div>

<!-- Add the dropdown component -->
{#if showUserDropdown}
  <DropDown
    title="Select User"
    searchPlaceholder="Search users..."
    position={dropdownPosition}
    width={280}
    maxHeight={320}
    excludeIds={[]}
    rootId={store?.path[store.path.length - 1]}
    show={showUserDropdown}
    on:select={handleUserSelect}
    on:close={handleDropdownClose}
  />
{/if}

<!-- Toast notification -->
{#if showToast}
  <ToastNotification
    message={toastMessage}
    type={toastType}
    show={showToast}
    on:close={handleToastClose}
  />
{/if}

<!-- Login dialog -->
{#if showLogin}
  <!-- Fixed overlay covering the entire screen -->
  <div class="login-modal-overlay" on:click|self={() => (showLogin = false)}>
    <!-- Centered modal content -->
    <div class="login-modal-content">
      <LogIn on:authchange={handleAuthChange} on:close={handleLoginClose} />
    </div>
  </div>
{/if}

{#if showNodeInfo}
  <div class="node-info">
    <div class="node-info-header">
      <h3>{node?.name || "Unnamed Node"}</h3>
      <button class="close-button" on:click={() => (showNodeInfo = false)}
        >×</button
      >
    </div>
    {#if node}
      <div class="node-info-content">
        <div class="node-stats">
          <div class="stat-item">
            <span class="stat-label">Points:</span>
            <span class="stat-value">{node.points?.toFixed(2) || 0}</span>
          </div>
          {#if node.fulfilled !== undefined}
            <div class="stat-item">
              <span class="stat-label">Fulfillment:</span>
              <span class="stat-value"
                >{(node.fulfilled * 100).toFixed(1)}%</span
              >
            </div>
          {/if}
          {#if node.manualFulfillment !== undefined}
            <div class="stat-item">
              <span class="stat-label">Manual Fulfillment:</span>
              <span class="stat-value"
                >{(node.manualFulfillment * 100).toFixed(1)}%</span
              >
            </div>
          {/if}
          {#if node.isContribution !== undefined}
            <div class="stat-item">
              <span class="stat-label">Type:</span>
              <span class="stat-value"
                >{node.isContribution ? "Contribution" : "Regular"}</span
              >
            </div>
          {/if}
        </div>

        {#if node._key}
          <div class="node-detail">
            <span class="detail-label">ID:</span>
            <span class="detail-value">{node._key}</span>
          </div>
        {/if}

        {#if node.contributors && Object.keys(node.contributors).length > 0}
          <div class="node-section">
            <h4>Contributors ({Object.keys(node.contributors).length})</h4>
            <div class="contributors-list">
              {#each Object.keys(node.contributors) as id}
                <div class="contributor-item" title={id}>
                  {id.slice(0, 10)}{id.length > 10 ? "..." : ""}
                </div>
              {/each}
            </div>
          </div>
        {/if}

        {#if node.children && node.children.length > 0}
          <div class="node-section">
            <h4>Children ({node.children.length})</h4>
            <div class="children-list">
              {#each node.children.slice(0, 5) as child}
                <div class="child-item">
                  <span class="child-name">{child.name || "Unnamed"}</span>
                  <span class="child-points">{child.points || 0} pts</span>
                </div>
              {/each}
              {#if node.children.length > 5}
                <div class="more-children">
                  +{node.children.length - 5} more
                </div>
              {/if}
            </div>
          </div>
        {/if}
      </div>
    {/if}
  </div>
{:else}
  <button class="info-toggle-button" on:click={() => (showNodeInfo = true)}>
    <span>i</span>
  </button>
{/if}

<style>
  /* Node info styling */
  .node-info {
    position: absolute;
    bottom: 10px;
    right: 10px;
    width: 300px;
    max-width: 90vw;
    max-height: 400px;
    background: rgba(255, 255, 255, 0.95);
    border-radius: 8px;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.15);
    overflow: auto;
    z-index: 900;
    padding: 0;
    transition: all 0.2s ease;
  }

  .node-info-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 16px 8px;
    border-bottom: 1px solid #eee;
  }

  .node-info-header h3 {
    margin: 0;
    font-size: 18px;
    font-weight: 600;
    color: #333;
  }

  .close-button {
    background: none;
    border: none;
    font-size: 24px;
    color: #999;
    cursor: pointer;
    padding: 0;
    line-height: 1;
    transition: color 0.2s;
  }

  .close-button:hover {
    color: #333;
  }

  .node-info-content {
    padding: 16px;
  }

  .node-info h4 {
    margin: 12px 0 8px 0;
    font-size: 14px;
    font-weight: 600;
    color: #555;
  }

  .node-stats {
    display: flex;
    flex-wrap: wrap;
    gap: 10px;
    margin-bottom: 12px;
  }

  .stat-item {
    background: #f5f7fa;
    padding: 6px 10px;
    border-radius: 6px;
    font-size: 13px;
  }

  .stat-label,
  .detail-label {
    font-weight: 500;
    color: #555;
    margin-right: 5px;
  }

  .stat-value,
  .detail-value {
    font-weight: 600;
    color: #333;
  }

  .node-detail {
    padding: 8px 0;
    font-size: 13px;
    border-bottom: 1px solid #eee;
  }

  .node-section {
    margin-top: 12px;
    padding-top: 8px;
    border-top: 1px solid #eee;
  }

  .contributors-list {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
  }

  .contributor-item {
    background: #e9f0f7;
    color: #3a6b9e;
    padding: 4px 8px;
    border-radius: 4px;
    font-size: 12px;
    font-weight: 500;
  }

  .children-list {
    display: flex;
    flex-direction: column;
    gap: 6px;
  }

  .child-item {
    display: flex;
    justify-content: space-between;
    background: #f8f8f8;
    padding: 6px 10px;
    border-radius: 4px;
    font-size: 13px;
  }

  .child-name {
    font-weight: 500;
    color: #444;
  }

  .child-points {
    font-weight: 600;
    color: #666;
  }

  .more-children {
    text-align: center;
    font-size: 12px;
    color: #888;
    padding: 4px 0;
  }

  .info-toggle-button {
    position: absolute;
    bottom: 10px;
    right: 10px;
    width: 36px;
    height: 36px;
    border-radius: 50%;
    background: rgba(255, 255, 255, 0.9);
    border: 1px solid #ddd;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.1);
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    z-index: 900;
    transition: all 0.2s ease;
  }

  .info-toggle-button span {
    font-weight: bold;
    font-style: italic;
    color: #3a6b9e;
    font-size: 18px;
  }

  .info-toggle-button:hover {
    background: rgba(255, 255, 255, 1);
    box-shadow: 0 3px 8px rgba(0, 0, 0, 0.15);
  }

  :global(body) {
    margin: 0;
    padding: 0;
    overflow: hidden;
  }

  div {
    width: 100%;
    height: 100%;
    overflow: hidden;
  }

  :global(.treemap-node rect) {
    transition: fill 0.2s ease;
  }

  :global(.treemap-node text) {
    text-shadow:
      0px 0px 3px rgba(255, 255, 255, 0.8),
      0px 0px 2px rgba(255, 255, 255, 0.6);
    font-weight: 500;
  }

  :global(.edit-text-field) {
    cursor: text;
  }

  :global(.clickable),
  :global(.home-button),
  :global(.add-button),
  :global(.peer-button),
  :global(.delete-button) {
    cursor: pointer;
  }

  :global(.contributor-search-dropdown) {
    box-shadow:
      0 6px 16px rgba(0, 0, 0, 0.12),
      0 3px 6px rgba(0, 0, 0, 0.08);
    border: none !important;
    border-radius: 8px !important;
    overflow: hidden;
    transition: opacity 0.2s ease;
  }

  :global(.contributor-search-dropdown input) {
    border-radius: 0;
    height: 40px;
    font-size: 14px !important;
    padding: 8px 12px !important;
    background: #f9f9f9;
  }

  :global(.user-item) {
    padding: 10px 12px !important;
    border-bottom: 1px solid #f0f0f0;
    font-size: 14px !important;
  }

  :global(.user-item:hover) {
    background-color: #f5f7fa !important;
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

  :global(.tag-wrapper-container) {
    display: block;
    overflow: visible;
    z-index: 10;
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

  .login-modal-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    width: 100vw;
    height: 100vh;
    z-index: 10000;
    display: flex;
    align-items: center;
    justify-content: center;
    background-color: rgba(0, 0, 0, 0.5);
    backdrop-filter: blur(2px);
  }

  .login-modal-content {
    position: relative;
    z-index: 10001;
    animation: fadeIn 0.2s ease-out;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
      transform: translateY(-10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
</style>
