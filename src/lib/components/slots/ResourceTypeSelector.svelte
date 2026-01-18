<!--
  ResourceTypeSelector.svelte
  
  Component for selecting a need type (food, housing, healthcare, etc.)
  
  Usage:
    <ResourceTypeSelector 
      selected={resourceTypeId}
      onSelect={(id) => resourceTypeId = id}
      required={true}
    />
-->

<script lang="ts">
  import { types, type ResourceType } from '$lib/protocol/resource-types';
  
  interface Props {
    /** Currently selected need type ID */
    selected?: string;
    /** Callback when a need type is selected */
    onSelect?: (resourceTypeId: string) => void;
    /** Whether selection is required */
    required?: boolean;
    /** Show as inline buttons or dropdown */
    variant?: 'buttons' | 'dropdown';
  }
  
  let { selected, onSelect, required = false, variant = 'buttons' }: Props = $props();
  
  // Group types by category
  const categories = $derived.by(() => {
    const groups: Record<string, ResourceType[]> = {};
    const categoryOrder: string[] = [];

    types.forEach(type => {
        const cat = type.category || 'Other';
        if (!groups[cat]) {
            groups[cat] = [];
            categoryOrder.push(cat);
        }
        groups[cat].push(type);
    });

    return categoryOrder.map(cat => ({
        name: cat,
        types: groups[cat]
    }));
  });

  function handleSelect(resourceTypeId: string) {
    onSelect?.(resourceTypeId);
  }
</script>

{#if variant === 'buttons'}
  <div class="resource-type-selector" data-testid="resource-type-selector">
    <span class="label">
      Need Type {#if required}<span class="required">*</span>{/if}
    </span>
    
    <div class="category-list">
        {#each categories as category}
            <div class="category-section">
                <h4 class="category-title">{category.name}</h4>
                <div class="type-grid">
                    {#each category.types as type (type.id)}
                        <button
                        type="button"
                        class="type-button"
                        class:selected={selected === type.id}
                        onclick={() => handleSelect(type.id)}
                        data-testid="resource-type-{type.id}"
                        title={type.description}
                        >
                        <span class="emoji">{type.emoji}</span>
                        <span class="name">{type.label}</span>
                        </button>
                    {/each}
                </div>
            </div>
        {/each}
    </div>
    
    {#if selected}
      {@const selectedType = types.find(t => t.id === selected)}
      {#if selectedType}
        <div class="selected-info">
          <p class="description">{selectedType.description}</p>
        </div>
      {/if}
    {/if}
  </div>
{:else}
  <!-- Dropdown variant -->
  <div class="resource-type-selector dropdown" data-testid="resource-type-selector-dropdown">
    <label class="label" for="resource-type-select">
      Need Type {#if required}<span class="required">*</span>{/if}
    </label>
    
    <select
      id="resource-type-select"
      class="select"
      value={selected}
      onchange={(e) => handleSelect(e.currentTarget.value)}
      required={required}
      data-testid="resource-type-select"
    >
      <option value="">Select a type...</option>
      {#each categories as category}
        <optgroup label={category.name}>
            {#each category.types as type (type.id)}
                <option value={type.id}>
                {type.emoji} {type.label}
                </option>
            {/each}
        </optgroup>
      {/each}
    </select>
    
    {#if selected}
      {@const selectedType = types.find(t => t.id === selected)}
      {#if selectedType}
        <div class="selected-info">
          <p class="description">{selectedType.description}</p>
        </div>
      {/if}
    {/if}
  </div>
{/if}

<style>
  .resource-type-selector {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }
  
  .label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #374151;
  }
  
  .required {
    color: #ef4444;
  }

  .category-list {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }
  
  .category-title {
    font-size: 0.8rem;
    text-transform: uppercase;
    color: #6b7280;
    font-weight: 600;
    margin: 0 0 0.5rem 0;
    padding-bottom: 0.25rem;
    border-bottom: 1px solid #e5e7eb;
  }
  
  .type-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(130px, 1fr));
    gap: 0.5rem;
  }
  
  .type-button {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.25rem;
    padding: 0.75rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.5rem;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
    min-height: 80px;
    justify-content: center;
  }
  
  .type-button:hover {
    border-color: #3b82f6;
    background: #eff6ff;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }
  
  .type-button.selected {
    border-color: #3b82f6;
    background: #dbeafe;
    box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.2);
  }
  
  .emoji {
    font-size: 1.75rem;
    line-height: 1;
  }
  
  .name {
    font-size: 0.8rem;
    font-weight: 500;
    color: #374151;
    text-align: center;
    line-height: 1.2;
  }
  
  .selected-info {
    padding: 0.75rem;
    background: #f9fafb;
    border-radius: 0.375rem;
    border-left: 3px solid #3b82f6;
    margin-top: 0.5rem;
  }
  
  .description {
    font-size: 0.875rem;
    color: #6b7280;
    margin: 0;
  }
  

  
  /* Dropdown variant styles */
  .dropdown .select {
    width: 100%;
    padding: 0.5rem 0.75rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #1f2937;
    background: white;
    cursor: pointer;
  }
  
  .dropdown .select:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
</style>



