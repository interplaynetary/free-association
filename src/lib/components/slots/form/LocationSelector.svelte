<!--
  LocationSelector.svelte
  
  Location selection: specific address, online, or flexible
  
  Usage:
    <LocationSelector 
      locationType={slot.location_type}
      city={slot.city}
      country={slot.country}
      onlineLink={slot.online_link}
      onChange={(field, value) => {...}}
    />
-->

<script lang="ts">
  interface Props {
    locationType?: string;
    streetAddress?: string;
    city?: string;
    stateProvince?: string;
    postalCode?: string;
    country?: string;
    latitude?: number;
    longitude?: number;
    onlineLink?: string;
    onChange?: (field: string, value: any) => void;
    readonly?: boolean;
  }
  
  let { 
    locationType = $bindable(),
    streetAddress = $bindable(),
    city = $bindable(),
    stateProvince = $bindable(),
    postalCode = $bindable(),
    country = $bindable(),
    latitude = $bindable(),
    longitude = $bindable(),
    onlineLink = $bindable(),
    onChange,
    readonly = false
  }: Props = $props();
  
  const LOCATION_TYPES = [
    { value: 'Flexible', label: 'Flexible', description: 'Location can be determined later' },
    { value: 'Specific', label: 'Specific Address', description: 'At a particular physical location' },
    { value: 'Online', label: 'Online/Remote', description: 'Via internet or phone' }
  ];
  
  // Set default location type if not specified
  if (!locationType) {
    locationType = 'Flexible';
  }
  
  function handleLocationTypeChange(type: string) {
    locationType = type;
    onChange?.('location_type', type);
    
    // Clear fields not relevant to this type
    if (type === 'Online') {
      streetAddress = undefined;
      city = undefined;
      stateProvince = undefined;
      postalCode = undefined;
      country = undefined;
      latitude = undefined;
      longitude = undefined;
    } else if (type === 'Flexible') {
      onlineLink = undefined;
    }
  }
  
  function handleFieldChange(field: string, e: Event) {
    const value = (e.target as HTMLInputElement).value;
    onChange?.(field, value || undefined);
  }
  
  function handleCoordinateChange(field: 'latitude' | 'longitude', e: Event) {
    const value = parseFloat((e.target as HTMLInputElement).value);
    onChange?.(field, isNaN(value) ? undefined : value);
  }
</script>

<div class="location-selector" data-testid="location-selector">
  <h3 class="section-title">Location</h3>
  
  <!-- Location Type Selector -->
  {#if !readonly}
    <div class="type-selector">
      {#each LOCATION_TYPES as type (type.value)}
        <button
          type="button"
          class="type-button"
          class:selected={locationType === type.value}
          onclick={() => handleLocationTypeChange(type.value)}
          data-testid="location-type-{type.value}"
        >
          <span class="type-label">{type.label}</span>
          <span class="type-desc">{type.description}</span>
        </button>
      {/each}
    </div>
  {:else}
    <div class="readonly-type">
      <strong>Type:</strong> {locationType}
    </div>
  {/if}
  
  <!-- Location Details -->
  <div class="location-details">
    {#if locationType === 'Specific'}
      <!-- Physical Address -->
      <div class="field">
        <label class="label" for="street-address">Street Address</label>
        <input
          id="street-address"
          type="text"
          class="input"
          value={streetAddress || ''}
          oninput={(e) => handleFieldChange('street_address', e)}
          placeholder="123 Main St, Apt 4B"
          readonly={readonly}
          data-testid="street-address-input"
        />
      </div>
      
      <div class="field-row">
        <div class="field">
          <label class="label" for="city">City</label>
          <input
            id="city"
            type="text"
            class="input"
            value={city || ''}
            oninput={(e) => handleFieldChange('city', e)}
            placeholder="San Francisco"
            readonly={readonly}
            data-testid="city-input"
          />
        </div>
        
        <div class="field">
          <label class="label" for="state-province">State/Province</label>
          <input
            id="state-province"
            type="text"
            class="input"
            value={stateProvince || ''}
            oninput={(e) => handleFieldChange('state_province', e)}
            placeholder="CA"
            readonly={readonly}
            data-testid="state-province-input"
          />
        </div>
      </div>
      
      <div class="field-row">
        <div class="field">
          <label class="label" for="postal-code">Postal Code</label>
          <input
            id="postal-code"
            type="text"
            class="input"
            value={postalCode || ''}
            oninput={(e) => handleFieldChange('postal_code', e)}
            placeholder="94103"
            readonly={readonly}
            data-testid="postal-code-input"
          />
        </div>
        
        <div class="field">
          <label class="label" for="country">Country</label>
          <input
            id="country"
            type="text"
            class="input"
            value={country || ''}
            oninput={(e) => handleFieldChange('country', e)}
            placeholder="USA"
            readonly={readonly}
            data-testid="country-input"
          />
        </div>
      </div>
      
      <!-- Optional Coordinates -->
      <details class="coordinates-section">
        <summary class="coordinates-summary">Advanced: GPS Coordinates (optional)</summary>
        <div class="field-row">
          <div class="field">
            <label class="label" for="latitude">Latitude</label>
            <input
              id="latitude"
              type="number"
              class="input"
              value={latitude || ''}
              oninput={(e) => handleCoordinateChange('latitude', e)}
              placeholder="37.7749"
              step="0.0001"
              min="-90"
              max="90"
              readonly={readonly}
              data-testid="latitude-input"
            />
          </div>
          
          <div class="field">
            <label class="label" for="longitude">Longitude</label>
            <input
              id="longitude"
              type="number"
              class="input"
              value={longitude || ''}
              oninput={(e) => handleCoordinateChange('longitude', e)}
              placeholder="-122.4194"
              step="0.0001"
              min="-180"
              max="180"
              readonly={readonly}
              data-testid="longitude-input"
            />
          </div>
        </div>
      </details>
      
    {:else if locationType === 'Online'}
      <!-- Online Link -->
      <div class="field">
        <label class="label" for="online-link">
          Meeting Link (optional)
          <span class="hint">Zoom, Google Meet, phone number, etc.</span>
        </label>
        <input
          id="online-link"
          type="text"
          class="input"
          value={onlineLink || ''}
          oninput={(e) => handleFieldChange('online_link', e)}
          placeholder="https://zoom.us/j/123456789 or +1-234-567-8900"
          readonly={readonly}
          data-testid="online-link-input"
        />
      </div>
      
      <div class="info-box">
        <p>
          💡 You can share meeting details later or send them privately to matched participants.
        </p>
      </div>
      
    {:else if locationType === 'Flexible'}
      <!-- Flexible Location -->
      <div class="info-box">
        <p>
          <strong>Flexible location</strong> - The specific location can be determined later
          through mutual agreement with matched participants.
        </p>
        <p>
          You can optionally specify a general area (city/country) to help with matching.
        </p>
      </div>
      
      <div class="field-row">
        <div class="field">
          <label class="label" for="city-flexible">City (optional)</label>
          <input
            id="city-flexible"
            type="text"
            class="input"
            value={city || ''}
            oninput={(e) => handleFieldChange('city', e)}
            placeholder="e.g., San Francisco"
            readonly={readonly}
            data-testid="city-flexible-input"
          />
        </div>
        
        <div class="field">
          <label class="label" for="country-flexible">Country (optional)</label>
          <input
            id="country-flexible"
            type="text"
            class="input"
            value={country || ''}
            oninput={(e) => handleFieldChange('country', e)}
            placeholder="e.g., USA"
            readonly={readonly}
            data-testid="country-flexible-input"
          />
        </div>
      </div>
    {/if}
  </div>
</div>

<style>
  .location-selector {
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
  
  .type-selector {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
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
  
  .location-details {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  .field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .field-row {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 1rem;
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
  
  .coordinates-section {
    padding: 1rem;
    background: #fafafa;
    border-radius: 0.375rem;
    border: 1px solid #e5e7eb;
  }
  
  .coordinates-summary {
    cursor: pointer;
    font-weight: 600;
    font-size: 0.875rem;
    color: #6b7280;
    user-select: none;
  }
  
  .coordinates-summary:hover {
    color: #3b82f6;
  }
  
  .info-box {
    padding: 1rem;
    background: #f0f9ff;
    border-left: 3px solid #3b82f6;
    border-radius: 0.375rem;
  }
  
  .info-box p {
    margin: 0 0 0.5rem 0;
    font-size: 0.875rem;
    color: #1e40af;
  }
  
  .info-box p:last-child {
    margin-bottom: 0;
  }
</style>



