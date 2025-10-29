<!--
  SlotForm.svelte
  
  Main slot form - works for BOTH need slots and availability slots
  
  Usage:
    <SlotForm 
      slot={slot}
      slotType="need" or "capacity"
      onSave={(slot) => {...}}
      onCancel={() => {...}}
    />
-->

<script lang="ts">
  import type { NeedSlot, AvailabilitySlot } from '$lib/commons/v5/schemas';
  import { NeedSlotSchema, AvailabilitySlotSchema } from '$lib/commons/v5/schemas';
  import NeedTypeSelector from './NeedTypeSelector.svelte';
  import BasicInfo from './form/BasicInfo.svelte';
  import QuantityInput from './form/QuantityInput.svelte';
  import TimeSelector from './form/TimeSelector.svelte';
  import LocationSelector from './form/LocationSelector.svelte';
  import FilterRuleEditor from './form/FilterRuleEditor.svelte';
  
  interface Props {
    slot?: NeedSlot | AvailabilitySlot;
    slotType: 'need' | 'capacity';
    onSave?: (slot: NeedSlot | AvailabilitySlot) => void;
    onCancel?: () => void;
    readonly?: boolean;
  }
  
  let { slot, slotType, onSave, onCancel, readonly = false }: Props = $props();
  
  // Form state - initialize from slot or defaults
  let formData = $state({
    id: slot?.id || crypto.randomUUID(),
    need_type_id: slot?.need_type_id || '',
    name: slot?.name || '',
    emoji: slot?.emoji,
    description: slot?.description,
    resource_type: slot?.resource_type,
    quantity: slot?.quantity || 1,
    unit: slot?.unit,
    start_date: slot?.start_date,
    end_date: slot?.end_date,
    recurrence: slot?.recurrence,
    availability_window: slot?.availability_window,
    time_zone: slot?.time_zone,
    location_type: slot?.location_type || 'Flexible',
    street_address: slot?.street_address,
    city: slot?.city,
    state_province: slot?.state_province,
    postal_code: slot?.postal_code,
    country: slot?.country,
    latitude: slot?.latitude,
    longitude: slot?.longitude,
    online_link: slot?.online_link,
    filter_rule: slot?.filter_rule,
    max_natural_div: slot?.max_natural_div,
    max_percentage_div: slot?.max_percentage_div,
    advance_notice_hours: slot?.advance_notice_hours,
    booking_window_hours: slot?.booking_window_hours,
    mutual_agreement_required: slot?.mutual_agreement_required,
    priority: slot?.priority,
    hidden_until_request_accepted: slot?.hidden_until_request_accepted
  });
  
  // Validation state
  let errors = $state<Record<string, string>>({});
  
  // Form sections visibility
  let showAdvancedOptions = $state(false);
  
  function updateField(field: string, value: any) {
    formData[field] = value;
    // Clear error for this field
    if (errors[field]) {
      delete errors[field];
    }
  }
  
  function validateForm(): boolean {
    const newErrors: Record<string, string> = {};
    
    if (!formData.need_type_id) {
      newErrors.need_type_id = 'Need type is required';
    }
    
    if (!formData.name?.trim()) {
      newErrors.name = 'Name is required';
    }
    
    if (formData.quantity <= 0) {
      newErrors.quantity = 'Quantity must be greater than 0';
    }
    
    errors = newErrors;
    return Object.keys(newErrors).length === 0;
  }
  
  function handleSubmit(e: Event) {
    e.preventDefault();
    
    if (!validateForm()) {
      console.warn('Form validation failed:', errors);
      return;
    }
    
    // Validate with Zod schema
    const schema = slotType === 'need' ? NeedSlotSchema : AvailabilitySlotSchema;
    const result = schema.safeParse(formData);
    
    if (!result.success) {
      console.error('Schema validation failed:', result.error);
      // Convert Zod errors to our format
      result.error.errors.forEach(err => {
        errors[err.path[0]] = err.message;
      });
      return;
    }
    
    onSave?.(result.data as any);
  }
  
  function handleCancel() {
    onCancel?.();
  }
</script>

<form class="slot-form" onsubmit={handleSubmit} data-testid="slot-form">
  <div class="form-header">
    <h2 class="form-title">
      {#if readonly}
        View {slotType === 'need' ? 'Need' : 'Capacity'}
      {:else if slot}
        Edit {slotType === 'need' ? 'Need' : 'Capacity'}
      {:else}
        Add {slotType === 'need' ? 'Need' : 'Capacity'}
      {/if}
    </h2>
    <p class="form-description">
      {#if slotType === 'need'}
        Define what resources or support you need from the community
      {:else}
        Define what resources or support you can provide to the community
      {/if}
    </p>
  </div>
  
  <div class="form-sections">
    <!-- Need Type Selection -->
    <section class="form-section">
      <NeedTypeSelector
        selected={formData.need_type_id}
        onSelect={(id) => updateField('need_type_id', id)}
        required
        variant="buttons"
      />
      {#if errors.need_type_id}
        <p class="error-message">{errors.need_type_id}</p>
      {/if}
    </section>
    
    <!-- Basic Info -->
    <section class="form-section">
      <BasicInfo
        bind:name={formData.name}
        bind:emoji={formData.emoji}
        bind:description={formData.description}
        bind:resourceType={formData.resource_type}
        onUpdate={updateField}
        {readonly}
      />
      {#if errors.name}
        <p class="error-message">{errors.name}</p>
      {/if}
    </section>
    
    <!-- Quantity -->
    <section class="form-section">
      <QuantityInput
        bind:quantity={formData.quantity}
        bind:unit={formData.unit}
        onUpdate={updateField}
        {readonly}
      />
      {#if errors.quantity}
        <p class="error-message">{errors.quantity}</p>
      {/if}
    </section>
    
    <!-- Time & Schedule -->
    <section class="form-section">
      <TimeSelector
        bind:startDate={formData.start_date}
        bind:endDate={formData.end_date}
        bind:recurrence={formData.recurrence}
        bind:availabilityWindow={formData.availability_window}
        bind:timeZone={formData.time_zone}
        onChange={updateField}
        {readonly}
      />
    </section>
    
    <!-- Location -->
    <section class="form-section">
      <LocationSelector
        bind:locationType={formData.location_type}
        bind:streetAddress={formData.street_address}
        bind:city={formData.city}
        bind:stateProvince={formData.state_province}
        bind:postalCode={formData.postal_code}
        bind:country={formData.country}
        bind:latitude={formData.latitude}
        bind:longitude={formData.longitude}
        bind:onlineLink={formData.online_link}
        onChange={updateField}
        {readonly}
      />
    </section>
    
    <!-- Filter Rules -->
    <section class="form-section">
      <FilterRuleEditor
        bind:filterRule={formData.filter_rule}
        onChange={(rule) => updateField('filter_rule', rule)}
        {readonly}
      />
    </section>
    
    <!-- Advanced Options (collapsible) -->
    {#if !readonly}
      <section class="form-section">
        <button
          type="button"
          class="toggle-button"
          onclick={() => showAdvancedOptions = !showAdvancedOptions}
          data-testid="toggle-advanced"
        >
          <span>Advanced Options</span>
          <span class="toggle-icon" class:rotated={showAdvancedOptions}>▼</span>
        </button>
        
        {#if showAdvancedOptions}
          <div class="advanced-options">
            <div class="field-group">
              <label class="label">
                <input
                  type="checkbox"
                  bind:checked={formData.mutual_agreement_required}
                />
                Require mutual agreement before matching
              </label>
            </div>
            
            <div class="field-group">
              <label class="label">
                <input
                  type="checkbox"
                  bind:checked={formData.hidden_until_request_accepted}
                />
                Hide details until request is accepted
              </label>
            </div>
            
            <div class="field">
              <label class="label" for="priority">
                Priority (higher = more important)
              </label>
              <input
                id="priority"
                type="number"
                class="input"
                bind:value={formData.priority}
                placeholder="0"
              />
            </div>
            
            <div class="field">
              <label class="label" for="advance-notice">
                Advance Notice (hours)
              </label>
              <input
                id="advance-notice"
                type="number"
                class="input"
                bind:value={formData.advance_notice_hours}
                min="0"
                placeholder="24"
              />
            </div>
            
            <div class="field">
              <label class="label" for="booking-window">
                Booking Window (hours)
              </label>
              <input
                id="booking-window"
                type="number"
                class="input"
                bind:value={formData.booking_window_hours}
                min="0"
                placeholder="168"
              />
            </div>
          </div>
        {/if}
      </section>
    {/if}
  </div>
  
  <!-- Form Actions -->
  {#if !readonly}
    <div class="form-actions">
      <button
        type="button"
        class="button button-secondary"
        onclick={handleCancel}
        data-testid="cancel-button"
      >
        Cancel
      </button>
      <button
        type="submit"
        class="button button-primary"
        data-testid="save-button"
      >
        {slot ? 'Save Changes' : 'Create Slot'}
      </button>
    </div>
  {/if}
</form>

<style>
  .slot-form {
    max-width: 800px;
    margin: 0 auto;
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }
  
  .form-header {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .form-title {
    margin: 0;
    font-size: 1.5rem;
    font-weight: 700;
    color: #1f2937;
  }
  
  .form-description {
    margin: 0;
    font-size: 1rem;
    color: #6b7280;
  }
  
  .form-sections {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }
  
  .form-section {
    padding: 1.5rem;
    background: white;
    border: 1px solid #e5e7eb;
    border-radius: 0.75rem;
    box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05);
  }
  
  .toggle-button {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.75rem;
    border: none;
    background: transparent;
    font-weight: 600;
    font-size: 1rem;
    color: #374151;
    cursor: pointer;
    transition: color 0.2s;
  }
  
  .toggle-button:hover {
    color: #3b82f6;
  }
  
  .toggle-icon {
    transition: transform 0.2s;
  }
  
  .toggle-icon.rotated {
    transform: rotate(180deg);
  }
  
  .advanced-options {
    padding-top: 1rem;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  .field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .field-group {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  
  .label {
    font-weight: 500;
    font-size: 0.875rem;
    color: #374151;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }
  
  .input {
    padding: 0.5rem 0.75rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #1f2937;
  }
  
  .input:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .error-message {
    margin: 0.5rem 0 0 0;
    padding: 0.5rem 0.75rem;
    background: #fef2f2;
    border-left: 3px solid #ef4444;
    border-radius: 0.25rem;
    font-size: 0.875rem;
    color: #991b1b;
  }
  
  .form-actions {
    display: flex;
    justify-content: flex-end;
    gap: 0.75rem;
    padding: 1rem;
    background: #f9fafb;
    border-top: 1px solid #e5e7eb;
    border-radius: 0.5rem;
    position: sticky;
    bottom: 0;
  }
  
  .button {
    padding: 0.625rem 1.25rem;
    border-radius: 0.375rem;
    font-weight: 600;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.2s;
    border: none;
  }
  
  .button-secondary {
    background: white;
    color: #374151;
    border: 1px solid #d1d5db;
  }
  
  .button-secondary:hover {
    background: #f9fafb;
    border-color: #9ca3af;
  }
  
  .button-primary {
    background: #3b82f6;
    color: white;
  }
  
  .button-primary:hover {
    background: #2563eb;
    transform: translateY(-1px);
    box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1);
  }
</style>

