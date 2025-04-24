<script lang="ts">
  import { onDestroy, onMount } from "svelte";
  import { createReactiveGraph } from "../../lib/utils/reactive/ReactiveGraph";
  import { createReactiveComponent } from "../../lib/utils/reactive/ReactiveComponent";
  import { gun, transientGun } from "../../lib/utils/gun/gunSetup";
  import { writable, type Writable } from "svelte/store";

  // Create two graphs - one persistent and one transient
  const persistentGraph = createReactiveGraph();
  const transientGraph = createReactiveGraph(undefined, true);

  // Create two components - one for each graph
  const persistentComponent = createReactiveComponent(persistentGraph, {
    defaultPath: ["examples", "counter"],
  });

  const transientComponent = createReactiveComponent(transientGraph, {
    defaultPath: ["examples", "counter"],
    transient: true,
  });

  // Initialize both components
  onMount(async () => {
    await persistentComponent.initialize();
    await transientComponent.initialize();

    // Initialize counters if they don't exist
    const persistentData = await persistentComponent.getDataStore("value", 0);
    const transientData = await transientComponent.getDataStore("value", 0);

    if (!$persistentData) {
      await persistentComponent.updateData("value", 0);
    }

    if (!$transientData) {
      await transientComponent.updateData("value", 0);
    }
  });

  // Clean up on component destruction
  onDestroy(() => {
    persistentComponent.destroy();
    transientComponent.destroy();
  });

  // Create stores for both counters
  const persistentCounter = persistentComponent.getDataStore("value", 0);
  const transientCounter = transientComponent.getDataStore("value", 0);

  // Functions to increment each counter
  async function incrementPersistent() {
    const currentValue = $persistentCounter || 0;
    await persistentComponent.updateData("value", currentValue + 1);
  }

  async function incrementTransient() {
    const currentValue = $transientCounter || 0;
    await transientComponent.updateData("value", currentValue + 1);
  }

  // Function to reset both counters
  async function resetCounters() {
    await persistentComponent.updateData("value", 0);
    await transientComponent.updateData("value", 0);
  }
</script>

<div class="container">
  <h1>Transient vs Persistent Storage Example</h1>

  <div class="counters">
    <div class="counter persistent">
      <h2>Persistent Counter</h2>
      <p>Value: {$persistentCounter || 0}</p>
      <p class="description">
        This counter uses persistent storage. Its value will be saved in
        localStorage and will persist between page refreshes and browser
        restarts.
      </p>
      <button onclick={incrementPersistent}>Increment</button>
    </div>

    <div class="counter transient">
      <h2>Transient Counter</h2>
      <p>Value: {$transientCounter || 0}</p>
      <p class="description">
        This counter uses transient storage. Its value will be synchronized
        between peers but NOT saved to localStorage. It will reset on page
        refresh.
      </p>
      <button onclick={incrementTransient}>Increment</button>
    </div>
  </div>

  <div class="controls">
    <button onclick={resetCounters}>Reset Both Counters</button>
  </div>

  <div class="explanation">
    <h3>How it Works</h3>
    <p>
      Both counters use ReactiveGraph and ReactiveComponent, but with different
      storage options:
    </p>
    <ul>
      <li>
        <strong>Persistent Counter:</strong> Uses the standard Gun instance with
        localStorage enabled. Data is persisted to localStorage and synchronized
        with peers.
      </li>
      <li>
        <strong>Transient Counter:</strong> Uses the transientGun instance with localStorage
        disabled. Data is only synchronized with peers while the app is running,
        but not saved locally.
      </li>
    </ul>
    <p>
      Refresh the page to see that the persistent counter maintains its value,
      while the transient counter resets to its initial value.
    </p>
  </div>
</div>

<style>
  .container {
    max-width: 800px;
    margin: 0 auto;
    padding: 20px;
    font-family:
      system-ui,
      -apple-system,
      BlinkMacSystemFont,
      sans-serif;
  }

  h1 {
    text-align: center;
    margin-bottom: 30px;
  }

  .counters {
    display: flex;
    gap: 30px;
    margin-bottom: 30px;
  }

  .counter {
    flex: 1;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
  }

  .persistent {
    background-color: #e3f2fd;
    border: 1px solid #90caf9;
  }

  .transient {
    background-color: #fff3e0;
    border: 1px solid #ffcc80;
  }

  .counter h2 {
    margin-top: 0;
    text-align: center;
  }

  .counter p {
    text-align: center;
    font-size: 24px;
    font-weight: bold;
  }

  .description {
    font-size: 14px !important;
    font-weight: normal !important;
    margin-bottom: 20px;
    text-align: left !important;
  }

  button {
    width: 100%;
    padding: 10px;
    background-color: #1976d2;
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 16px;
    transition: background-color 0.3s;
  }

  button:hover {
    background-color: #1565c0;
  }

  .controls {
    text-align: center;
    margin-bottom: 30px;
  }

  .controls button {
    width: auto;
    padding: 10px 20px;
    background-color: #f44336;
  }

  .controls button:hover {
    background-color: #d32f2f;
  }

  .explanation {
    background-color: #f5f5f5;
    padding: 20px;
    border-radius: 8px;
  }

  .explanation h3 {
    margin-top: 0;
  }

  .explanation ul {
    padding-left: 20px;
  }
</style>
