<!--
  FilterRuleEditor.svelte
  
  Editor for bilateral filter rules (trust-based, location-based, etc.)
  Simplified version focusing on trust filters for now
  
  Usage:
    <FilterRuleEditor 
      filterRule={slot.filter_rule}
      onChange={(rule) => {...}}
    />
-->

<script lang="ts">
  interface Props {
    filterRule?: any;
    onChange?: (rule: any) => void;
    readonly?: boolean;
  }
  
  let { 
    filterRule = $bindable(),
    onChange,
    readonly = false
  }: Props = $props();
  
  const FILTER_TYPES = [
    { value: 'none', label: 'No Filter', description: 'Allow everyone' },
    { value: 'trust', label: 'Trust-Based', description: 'Require mutual recognition' },
    { value: 'allow_all', label: 'Allow All', description: 'Explicitly allow everyone' },
    { value: 'deny_all', label: 'Deny All', description: 'Private/invitation only' }
  ];
  
  // Determine current filter type
  let currentType = $state<string>(
    !filterRule
      ? 'none'
      : filterRule.type === 'allow_all'
      ? 'allow_all'
      : filterRule.type === 'deny_all'
      ? 'deny_all'
      : filterRule.type === 'trust'
      ? 'trust'
      : 'none'
  );
  
  // Trust filter settings
  let onlyMutual = $state(filterRule?.only_mutual || false);
  let minMutualRecognition = $state(filterRule?.min_mutual_recognition || 0);
  
  function handleTypeChange(type: string) {
    currentType = type;
    updateFilter();
  }
  
  function handleOnlyMutualChange(e: Event) {
    onlyMutual = (e.target as HTMLInputElement).checked;
    updateFilter();
  }
  
  function handleMinMRChange(e: Event) {
    minMutualRecognition = parseFloat((e.target as HTMLInputElement).value);
    updateFilter();
  }
  
  function updateFilter() {
    if (currentType === 'none') {
      filterRule = null;
    } else if (currentType === 'allow_all') {
      filterRule = { type: 'allow_all' };
    } else if (currentType === 'deny_all') {
      filterRule = { type: 'deny_all' };
    } else if (currentType === 'trust') {
      filterRule = {
        type: 'trust',
        only_mutual: onlyMutual,
        min_mutual_recognition: minMutualRecognition
      };
    }
    onChange?.(filterRule);
  }
</script>

<div class="filter-rule-editor" data-testid="filter-rule-editor">
  <h3 class="section-title">Access Filter (optional)</h3>
  <p class="description">
    Control who can access this slot based on trust and recognition
  </p>
  
  <!-- Filter Type Selector -->
  {#if !readonly}
    <div class="type-selector">
      {#each FILTER_TYPES as type (type.value)}
        <button
          type="button"
          class="type-button"
          class:selected={currentType === type.value}
          onclick={() => handleTypeChange(type.value)}
          data-testid="filter-type-{type.value}"
        >
          <span class="type-label">{type.label}</span>
          <span class="type-desc">{type.description}</span>
        </button>
      {/each}
    </div>
  {:else}
    <div class="readonly-type">
      <strong>Filter:</strong> {FILTER_TYPES.find(t => t.value === currentType)?.label}
    </div>
  {/if}
  
  <!-- Trust Filter Options -->
  {#if currentType === 'trust'}
    <div class="trust-options">
      <div class="checkbox-field">
        <label class="checkbox-label">
          <input
            type="checkbox"
            checked={onlyMutual}
            onchange={handleOnlyMutualChange}
            disabled={readonly}
            data-testid="only-mutual-checkbox"
          />
          <span>Only people with mutual recognition (MR > 0)</span>
        </label>
        <p class="field-hint">
          Requires that you and the other person both recognize each other
        </p>
      </div>
      
      <div class="field">
        <label class="label" for="min-mr">
          Minimum Mutual Recognition
          <span class="hint">Value between 0 and 1</span>
        </label>
        <div class="range-field">
          <input
            id="min-mr"
            type="range"
            class="range-input"
            min="0"
            max="1"
            step="0.01"
            value={minMutualRecognition}
            oninput={handleMinMRChange}
            disabled={readonly}
            data-testid="min-mr-range"
          />
          <span class="range-value">{minMutualRecognition.toFixed(2)}</span>
        </div>
        <p class="field-hint">
          Only people with at least this level of mutual recognition can access
        </p>
      </div>
    </div>
  {/if}
  
  <!-- Info Boxes -->
  {#if currentType === 'none'}
    <div class="info-box">
      <p>No filter applied - anyone in the network can potentially access this slot.</p>
    </div>
  {:else if currentType === 'deny_all'}
    <div class="info-box warning">
      <p><strong>Private Slot:</strong> No one can automatically access this. You'll need to manually approve requests.</p>
    </div>
  {/if}
</div>

<style>
  .filter-rule-editor {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }
  
  .section-title {
    margin: 0;
    font-size: 1.125rem;
    font-weight: 600;
    color: #1f2937;
  }
  
  .description {
    margin: 0;
    font-size: 0.875rem;
    color: #6b7280;
  }
  
  .type-selector {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 0.75rem;
  }
  
  .type-button {
    display: flex;
    flex-direction: column;
    align-items: start;
    gap: 0.25rem;
    padding: 0.75rem;
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
  }
  
  .type-button.selected {
    border-color: #3b82f6;
    background: #dbeafe;
  }
  
  .type-label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #1f2937;
  }
  
  .type-desc {
    font-size: 0.75rem;
    color: #6b7280;
  }
  
  .readonly-type {
    padding: 0.75rem;
    background: #f9fafb;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #6b7280;
  }
  
  .trust-options {
    padding: 1rem;
    background: #fafafa;
    border-radius: 0.5rem;
    border: 1px solid #e5e7eb;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  .checkbox-field {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  
  .checkbox-label {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.875rem;
    color: #374151;
    cursor: pointer;
  }
  
  .checkbox-label input[type="checkbox"] {
    width: 1rem;
    height: 1rem;
    cursor: pointer;
  }
  
  .field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #374151;
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  
  .hint {
    font-weight: 400;
    color: #9ca3af;
    font-size: 0.75rem;
  }
  
  .field-hint {
    margin: 0;
    font-size: 0.75rem;
    color: #9ca3af;
  }
  
  .range-field {
    display: flex;
    align-items: center;
    gap: 0.75rem;
  }
  
  .range-input {
    flex: 1;
    height: 0.5rem;
    border-radius: 0.25rem;
    background: #e5e7eb;
    outline: none;
    -webkit-appearance: none;
    appearance: none;
  }
  
  .range-input::-webkit-slider-thumb {
    -webkit-appearance: none;
    appearance: none;
    width: 1rem;
    height: 1rem;
    border-radius: 50%;
    background: #3b82f6;
    cursor: pointer;
  }
  
  .range-input::-moz-range-thumb {
    width: 1rem;
    height: 1rem;
    border-radius: 50%;
    background: #3b82f6;
    cursor: pointer;
    border: none;
  }
  
  .range-value {
    min-width: 3rem;
    text-align: right;
    font-weight: 600;
    font-size: 0.875rem;
    color: #1f2937;
  }
  
  .info-box {
    padding: 1rem;
    background: #f0f9ff;
    border-left: 3px solid #3b82f6;
    border-radius: 0.375rem;
  }
  
  .info-box.warning {
    background: #fef2f2;
    border-left-color: #ef4444;
  }
  
  .info-box p {
    margin: 0;
    font-size: 0.875rem;
    color: #1e40af;
  }
  
  .info-box.warning p {
    color: #991b1b;
  }
</style>

