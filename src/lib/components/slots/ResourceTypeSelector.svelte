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
  import type { ResourceType } from '@playnet/free-association/schemas';
  
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
  
  // Common need types (can be extended via a global registry)
  // In a real app, this would come from a store or configuration
  const typeS: ResourceType[] = [
    {
      id: 'food',
      name: 'Food',
      emoji: '🍎',
      unit: 'servings',
      description: 'Meals, groceries, and food supplies'
    },
    {
      id: 'housing',
      name: 'Housing',
      emoji: '🏠',
      unit: 'nights',
      description: 'Shelter, accommodation, and housing'
    },
    {
      id: 'healthcare',
      name: 'Healthcare',
      emoji: '🏥',
      unit: 'hours',
      description: 'Medical care, therapy, and health services'
    },
    {
      id: 'education',
      name: 'Education',
      emoji: '📚',
      unit: 'hours',
      description: 'Teaching, tutoring, and learning'
    },
    {
      id: 'transportation',
      name: 'Transportation',
      emoji: '🚗',
      unit: 'trips',
      description: 'Rides, transit, and travel'
    },
    {
      id: 'childcare',
      name: 'Childcare',
      emoji: '👶',
      unit: 'hours',
      description: 'Babysitting, daycare, and child supervision'
    },
    {
      id: 'eldercare',
      name: 'Eldercare',
      emoji: '👴',
      unit: 'hours',
      description: 'Care and support for elderly'
    },
    {
      id: 'labor',
      name: 'Labor',
      emoji: '🔨',
      unit: 'hours',
      description: 'Physical work, repairs, and manual tasks'
    },
    {
      id: 'skills',
      name: 'Skills & Services',
      emoji: '💼',
      unit: 'hours',
      description: 'Professional services and expertise'
    },
    {
      id: 'goods',
      name: 'Goods & Supplies',
      emoji: '📦',
      unit: 'items',
      description: 'Physical items, tools, and supplies'
    },
    {
      id: 'other',
      name: 'Other',
      emoji: '✨',
      unit: 'units',
      description: 'Other types of needs'
    }
  ];
  
  function handleSelect(resourceTypeId: string) {
    onSelect?.(resourceTypeId);
  }
</script>

{#if variant === 'buttons'}
  <div class="resource-type-selector" data-testid="resource-type-selector">
    <label class="label">
      Need Type {#if required}<span class="required">*</span>{/if}
    </label>
    
    <div class="type-grid">
      {#each typeS as type (type.id)}
        <button
          type="button"
          class="type-button"
          class:selected={selected === type.id}
          onclick={() => handleSelect(type.id)}
          data-testid="resource-type-{type.id}"
          title={type.description}
        >
          <span class="emoji">{type.emoji}</span>
          <span class="name">{type.name}</span>
        </button>
      {/each}
    </div>
    
    {#if selected}
      {@const selectedType = typeS.find(t => t.id === selected)}
      {#if selectedType}
        <div class="selected-info">
          <p class="description">{selectedType.description}</p>
          <p class="unit-hint">Default unit: <strong>{selectedType.unit}</strong></p>
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
      {#each typeS as type (type.id)}
        <option value={type.id}>
          {type.emoji} {type.name}
        </option>
      {/each}
    </select>
    
    {#if selected}
      {@const selectedType = typeS.find(t => t.id === selected)}
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
  
  .type-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(140px, 1fr));
    gap: 0.75rem;
  }
  
  .type-button {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.5rem;
    padding: 1rem;
    border: 2px solid #e5e7eb;
    border-radius: 0.5rem;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .type-button:hover {
    border-color: #3b82f6;
    background: #eff6ff;
    transform: translateY(-2px);
    box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1);
  }
  
  .type-button.selected {
    border-color: #3b82f6;
    background: #dbeafe;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .emoji {
    font-size: 2rem;
  }
  
  .name {
    font-size: 0.875rem;
    font-weight: 500;
    color: #1f2937;
    text-align: center;
  }
  
  .selected-info {
    padding: 0.75rem;
    background: #f9fafb;
    border-radius: 0.375rem;
    border-left: 3px solid #3b82f6;
  }
  
  .description {
    font-size: 0.875rem;
    color: #6b7280;
    margin: 0;
  }
  
  .unit-hint {
    font-size: 0.75rem;
    color: #9ca3af;
    margin: 0.5rem 0 0 0;
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



