<script lang="ts">
  import {createEventDispatcher} from "svelte"

  export let types: string[]
  export let selectedType: string | null = null

  const dispatch = createEventDispatcher()

  function selectType(type: string) {
    selectedType = type
    dispatch("select", {type})
  }
</script>

<div class="type-selector">
  {#each types as type}
    <button
      class="type-button"
      class:active={type === selectedType}
      on:click={() => selectType(type)}
    >
      {type}
    </button>
  {/each}
</div>

<style>
  .type-selector {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }

  .type-button {
    background: white;
    border: 1px solid #d1d5db;
    padding: 0.5rem 1rem;
    border-radius: 6px;
    cursor: pointer;
    font-size: 0.875rem;
    font-weight: 500;
    transition: all 0.2s;
  }

  .type-button:hover {
    border-color: #3b82f6;
    background: #eff6ff;
  }

  .type-button.active {
    background: #3b82f6;
    color: white;
    border-color: #3b82f6;
  }
</style>

