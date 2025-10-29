<!--
  QuantityInput.svelte
  
  Quantity input with unit selector
  
  Usage:
    <QuantityInput 
      quantity={slot.quantity}
      unit={slot.unit}
      onUpdate={handleUpdate}
    />
-->

<script lang="ts">
  interface Props {
    quantity: number;
    unit?: string;
    onUpdate?: (field: string, value: number | string) => void;
    readonly?: boolean;
    min?: number;
    max?: number;
    step?: number;
  }
  
  let { 
    quantity = $bindable(),
    unit = $bindable(),
    onUpdate,
    readonly = false,
    min = 0,
    max,
    step = 1
  }: Props = $props();
  
  // Common units
  const COMMON_UNITS = [
    'hours', 'minutes', 'days', 'weeks', 'months',
    'servings', 'meals', 'items', 'units', 'slots',
    'sessions', 'trips', 'nights', 'people', 'kg', 'lbs'
  ];
  
  function handleQuantityChange(e: Event) {
    const value = parseFloat((e.target as HTMLInputElement).value);
    if (!isNaN(value)) {
      quantity = value;
      onUpdate?.('quantity', value);
    }
  }
  
  function handleUnitChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    unit = value;
    onUpdate?.('unit', value);
  }
  
  function incrementQuantity() {
    const newValue = quantity + step;
    if (max === undefined || newValue <= max) {
      quantity = newValue;
      onUpdate?.('quantity', newValue);
    }
  }
  
  function decrementQuantity() {
    const newValue = quantity - step;
    if (newValue >= min) {
      quantity = newValue;
      onUpdate?.('quantity', newValue);
    }
  }
</script>

<div class="quantity-input" data-testid="quantity-input">
  <div class="field-row">
    <!-- Quantity Input -->
    <div class="quantity-field">
      <label class="label" for="quantity">
        Quantity <span class="required">*</span>
      </label>
      <div class="number-input-group">
        <button
          type="button"
          class="stepper-button"
          onclick={decrementQuantity}
          disabled={readonly || quantity <= min}
          data-testid="decrement-button"
        >
          −
        </button>
        <input
          id="quantity"
          type="number"
          class="input number-input"
          value={quantity}
          oninput={handleQuantityChange}
          min={min}
          max={max}
          step={step}
          required
          readonly={readonly}
          data-testid="quantity-number-input"
        />
        <button
          type="button"
          class="stepper-button"
          onclick={incrementQuantity}
          disabled={readonly || (max !== undefined && quantity >= max)}
          data-testid="increment-button"
        >
          +
        </button>
      </div>
    </div>
    
    <!-- Unit Input -->
    <div class="unit-field">
      <label class="label" for="unit">Unit</label>
      <input
        id="unit"
        type="text"
        class="input"
        value={unit || ''}
        oninput={handleUnitChange}
        placeholder="e.g., hours"
        list="common-units"
        readonly={readonly}
        data-testid="unit-input"
      />
      <datalist id="common-units">
        {#each COMMON_UNITS as commonUnit}
          <option value={commonUnit}></option>
        {/each}
      </datalist>
    </div>
  </div>
  
  <!-- Display formatted quantity -->
  <div class="quantity-display">
    <span class="display-text">
      <strong>{quantity}</strong> 
      {unit || 'units'}
    </span>
  </div>
</div>

<style>
  .quantity-input {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }
  
  .field-row {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 1rem;
  }
  
  .quantity-field,
  .unit-field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #374151;
  }
  
  .required {
    color: #ef4444;
  }
  
  .number-input-group {
    display: flex;
    align-items: stretch;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    overflow: hidden;
    background: white;
  }
  
  .number-input-group:focus-within {
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .stepper-button {
    flex-shrink: 0;
    width: 2.5rem;
    border: none;
    background: #f9fafb;
    color: #374151;
    font-size: 1.25rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .stepper-button:hover:not(:disabled) {
    background: #f3f4f6;
    color: #1f2937;
  }
  
  .stepper-button:disabled {
    cursor: not-allowed;
    opacity: 0.4;
  }
  
  .number-input {
    flex: 1;
    text-align: center;
    border: none !important;
    font-weight: 600;
    padding: 0.5rem 0.25rem;
  }
  
  .number-input:focus {
    outline: none;
    box-shadow: none !important;
  }
  
  /* Remove spinner arrows in Chrome, Safari, Edge, Opera */
  .number-input::-webkit-outer-spin-button,
  .number-input::-webkit-inner-spin-button {
    -webkit-appearance: none;
    appearance: none;
    margin: 0;
  }
  
  /* Remove spinner arrows in Firefox */
  .number-input[type=number] {
    -moz-appearance: textfield;
    appearance: textfield;
  }
  
  .input {
    width: 100%;
    padding: 0.5rem 0.75rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #1f2937;
    background: white;
  }
  
  .input:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .input:read-only {
    background: #f9fafb;
    cursor: not-allowed;
  }
  
  .input::placeholder {
    color: #9ca3af;
  }
  
  .quantity-display {
    padding: 0.75rem;
    background: #f0f9ff;
    border-radius: 0.375rem;
    border-left: 3px solid #3b82f6;
  }
  
  .display-text {
    font-size: 0.875rem;
    color: #1e40af;
  }
</style>

