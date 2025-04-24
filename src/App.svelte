<script lang="ts">
  import { onMount, onDestroy, setContext } from "svelte";
  import { createRec, type RecognitionStore } from "./stores/rec.svelte";
  import Parent from "./lib/components/Parent.svelte";
  import Header from "./lib/components/Header.svelte";
  import { get } from "svelte/store";

  // Path to the recognition data
  let path: string[] = ["recognition"];

  // Store reference
  let recStore: RecognitionStore;

  // State
  let loading = $state(true);
  let error = $state<string | null>(null);
  let viewportWidth = $state(window.innerWidth);
  let viewportHeight = $state(window.innerHeight);

  // Panel management state
  let activePanel = $state<"node" | "inventory" | "login" | "charts">("node");

  // Dynamic component registry
  const panelComponents = $state({
    inventory: {
      component: null as any,
      loader: async () => import("./lib/components/Inventory.svelte"),
      props: () => ({ store: recStore }),
    },
    login: {
      component: null as any,
      loader: async () => import("./lib/components/LogIn.svelte"),
      props: () => ({
        onclose: () => setActivePanel("node"),
        onauthchange: (e: CustomEvent) =>
          console.log("Auth changed:", e.detail),
      }),
    },
    charts: {
      component: null as any,
      loader: async () => import("./lib/components/StackedBar.svelte"),
      props: () => ({
        data: [
          { category: "Recognition", peer: "Self", value: 25 },
          { category: "Recognition", peer: "Peer", value: 15 },
          { category: "Contributions", peer: "Self", value: 20 },
          { category: "Contributions", peer: "Peer", value: 10 },
          { category: "Innovation", peer: "Self", value: 15 },
          { category: "Innovation", peer: "Peer", value: 30 },
        ],
        width: 800,
      }),
    },
  });

  // Panel component reference
  const PanelComponent = $derived(
    activePanel !== "node" ? panelComponents[activePanel].component : null,
  );

  // Panel props
  const panelProps = $derived(
    activePanel !== "node" ? panelComponents[activePanel].props() : {},
  );

  // Toast notification state
  let toast = $state({
    visible: false,
    message: "",
    type: "info" as "info" | "success" | "warning" | "error",
    timeoutId: null as number | null,
  });

  // Set the active panel
  function setActivePanel(panel: "node" | "inventory" | "login" | "charts") {
    // Only load component if it's a different panel
    if (panel !== "node" && panel !== activePanel) {
      loadPanelComponent(panel);
    }
    activePanel = panel;
  }

  // Load a panel component
  async function loadPanelComponent(panel: "inventory" | "login" | "charts") {
    if (!panelComponents[panel].component) {
      try {
        const module = await panelComponents[panel].loader();
        panelComponents[panel].component = module.default;
      } catch (error) {
        console.error(`Error loading ${panel} component:`, error);
      }
    }
  }

  // Panel function handlers
  function handleInventoryClick() {
    setActivePanel(activePanel === "inventory" ? "node" : "inventory");
  }

  function handlePeerClick() {
    setActivePanel(activePanel === "login" ? "node" : "login");
  }

  function handleChartsClick() {
    setActivePanel(activePanel === "charts" ? "node" : "charts");
  }

  // Show toast message
  function showToast(
    message: string,
    type: "info" | "success" | "warning" | "error" = "info",
  ) {
    // Clear any existing toast timeout
    if (toast.timeoutId) {
      clearTimeout(toast.timeoutId);
    }

    // Show new toast
    toast = {
      visible: true,
      message,
      type,
      timeoutId: window.setTimeout(() => {
        toast.visible = false;
      }, 3000) as unknown as number,
    };
  }

  // Initialize on mount
  onMount(async () => {
    try {
      loading = true;

      // Create the recognition store directly
      recStore = createRec(path);

      loading = false;
    } catch (err) {
      console.error("Error initializing:", err);
      error = err instanceof Error ? err.message : "Unknown error";
      loading = false;
    }
  });

  // Handle window resize
  function handleResize() {
    viewportWidth = window.innerWidth;
    viewportHeight = window.innerHeight;
  }

  // Set up the panels context
  setContext("panels", {
    get activePanel() {
      return activePanel;
    },
    setPanel: setActivePanel,
    handleInventoryClick,
    handlePeerClick,
    handleChartsClick,
  });

  // Set up the toast context
  setContext("toast", {
    showToast,
  });
</script>

<svelte:window on:resize={handleResize} />

<main>
  <div class="container">
    {#if loading}
      <div class="loading">Loading recognition data...</div>
    {:else if error}
      <div class="error">{error}</div>
    {:else if recStore}
      <div class="app-layout">
        {#if activePanel === "node"}
          <!-- Parent component with hierarchyData slot prop for Header -->
          <Parent store={recStore} let:hierarchyData>
            <div class="app-header">
              <Header node={hierarchyData} store={recStore} />
            </div>
          </Parent>
        {:else if PanelComponent}
          <div class="app-header">
            <Header store={recStore} />
          </div>

          <div class="app-content">
            <div class="{activePanel}-wrapper panel-wrapper">
              <svelte:component this={PanelComponent} {...panelProps} />
            </div>
          </div>
        {/if}
      </div>
    {:else}
      <div class="empty">No recognition data available</div>
    {/if}
  </div>

  <!-- Toast notification -->
  {#if toast.visible}
    <div class="toast-container">
      <div class="toast toast-{toast.type}">
        {toast.message}
      </div>
    </div>
  {/if}
</main>

<style>
  :global(*) {
    margin: 0;
    padding: 0;
    box-sizing: border-box;
  }

  :global(html, body) {
    margin: 0;
    padding: 0;
    overflow: hidden;
    width: 100vw;
    height: 100vh;
    background-color: #ffffff;
  }

  :global(#app) {
    width: 100vw;
    height: 100vh;
    position: absolute;
    top: 0;
    left: 0;
    margin: 0;
    padding: 0;
    background-color: #888888;
  }

  main {
    width: 100vw;
    height: 100vh;
    margin: 0;
    padding: 0;
    overflow: hidden;
    display: block;
    position: absolute;
    top: 0;
    left: 0;
    background-color: #ffffff;
  }

  .container {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
    margin: 0;
    padding: 0;
  }

  .app-layout {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
  }

  .app-header {
    width: 100%;
    height: 50px;
    flex-shrink: 0;
  }

  .app-content {
    flex: 1;
    overflow: auto;
    position: relative;
  }

  .loading,
  .error,
  .empty {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100%;
    width: 100%;
    font-size: 1.2rem;
    color: #ffffff;
  }

  .error {
    color: #d32f2f;
    padding: 1rem;
  }

  /* Panel wrapper styles */
  .panel-wrapper {
    flex: 1;
    overflow: auto;
    position: relative;
  }

  /* Specific panel styling */
  .inventory-wrapper {
    /* Inventory specific styles */
  }

  .login-wrapper {
    display: flex;
    justify-content: center;
    align-items: center;
    background-color: rgba(0, 0, 0, 0.05);
  }

  .charts-wrapper {
    display: flex;
    justify-content: center;
    align-items: center;
    padding: 20px;
    background-color: white;
  }

  /* Toast notification styles */
  .toast-container {
    position: absolute;
    bottom: 20px;
    left: 50%;
    transform: translateX(-50%);
    z-index: 1000;
  }

  .toast {
    padding: 10px 20px;
    border-radius: 4px;
    background-color: #333;
    color: white;
    box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
    animation: fadeIn 0.3s ease-out;
  }

  .toast-success {
    background-color: #28a745;
  }

  .toast-warning {
    background-color: #ffc107;
    color: #333;
  }

  .toast-error {
    background-color: #dc3545;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
      transform: translateY(20px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }
</style>
