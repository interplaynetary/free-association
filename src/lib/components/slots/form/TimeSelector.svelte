<!--
  TimeSelector.svelte
  
  Complete time selection: dates, recurrence, and availability windows
  
  Usage:
    <TimeSelector 
      startDate={slot.start_date}
      endDate={slot.end_date}
      recurrence={slot.recurrence}
      availabilityWindow={slot.availability_window}
      onChange={(field, value) => {...}}
    />
-->

<script lang="ts">
  import type { AvailabilityWindow } from '$lib/protocol/schemas';
  import AvailabilityWindowEditor from './time/AvailabilityWindowEditor.svelte';
  
  interface Props {
    startDate?: string | null;
    endDate?: string | null;
    recurrence?: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
    availabilityWindow?: AvailabilityWindow;
    timeZone?: string;
    onChange?: (field: string, value: any) => void;
    readonly?: boolean;
  }
  
  let { 
    startDate = $bindable(),
    endDate = $bindable(),
    recurrence = $bindable(),
    availabilityWindow = $bindable(),
    timeZone = $bindable(),
    onChange,
    readonly = false
  }: Props = $props();
  
  const RECURRENCE_OPTIONS = [
    { value: null, label: 'One-time' },
    { value: 'daily', label: 'Daily' },
    { value: 'weekly', label: 'Weekly' },
    { value: 'monthly', label: 'Monthly' },
    { value: 'yearly', label: 'Yearly' }
  ];
  
  // Whether this is a recurring slot
  let isRecurring = $derived(recurrence !== null && recurrence !== undefined);
  
  function handleStartDateChange(e: Event) {
    const value = (e.target as HTMLInputElement).value || null;
    startDate = value;
    onChange?.('start_date', value);
  }
  
  function handleEndDateChange(e: Event) {
    const value = (e.target as HTMLInputElement).value || null;
    endDate = value;
    onChange?.('end_date', value);
  }
  
  function handleRecurrenceChange(e: Event) {
    const value = (e.target as HTMLSelectElement).value;
    recurrence = value === '' ? null : value as any;
    onChange?.('recurrence', recurrence);
  }
  
  function handleTimezoneChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    timeZone = value;
    onChange?.('time_zone', value);
  }
  
  function handleWindowChange(window: AvailabilityWindow | undefined) {
    availabilityWindow = window;
    onChange?.('availability_window', window);
  }
  
  // Get today's date in YYYY-MM-DD format
  const today = $derived(new Date().toISOString().split('T')[0]);
</script>

<div class="time-selector" data-testid="time-selector">
  <h3 class="section-title">Time & Schedule</h3>
  
  <!-- Date Range -->
  <div class="date-range">
    <div class="field">
      <label class="label" for="start-date">
        {isRecurring ? 'Start Date (pattern begins)' : 'Date'}
      </label>
      <input
        id="start-date"
        type="date"
        class="input"
        value={startDate || ''}
        oninput={handleStartDateChange}
        min={today}
        readonly={readonly}
        data-testid="start-date-input"
      />
    </div>
    
    {#if isRecurring}
      <div class="field">
        <label class="label" for="end-date">
          End Date (optional)
          <span class="hint">When does the pattern stop?</span>
        </label>
        <input
          id="end-date"
          type="date"
          class="input"
          value={endDate || ''}
          oninput={handleEndDateChange}
          min={startDate || today}
          readonly={readonly}
          data-testid="end-date-input"
        />
      </div>
    {/if}
  </div>
  
  <!-- Recurrence -->
  <div class="field">
    <label class="label" for="recurrence">Recurrence</label>
    <select
      id="recurrence"
      class="select"
      value={recurrence || ''}
      onchange={handleRecurrenceChange}
      disabled={readonly}
      data-testid="recurrence-select"
    >
      {#each RECURRENCE_OPTIONS as option (option.value || 'none')}
        <option value={option.value || ''}>{option.label}</option>
      {/each}
    </select>
  </div>
  
  <!-- Timezone -->
  <div class="field">
    <label class="label" for="timezone">
      Timezone
      <span class="hint">IANA timezone (e.g., America/New_York)</span>
    </label>
    <input
      id="timezone"
      type="text"
      class="input"
      value={timeZone || ''}
      oninput={handleTimezoneChange}
      placeholder="UTC (default)"
      readonly={readonly}
      data-testid="timezone-input"
    />
  </div>
  
  <!-- Availability Window -->
  {#if isRecurring}
    <div class="availability-window-section">
      <AvailabilityWindowEditor
        bind:window={availabilityWindow}
        onChange={handleWindowChange}
        {readonly}
      />
    </div>
  {:else}
    <div class="info-box">
      <p>
        <strong>One-time slot</strong> - Use start date to specify when this occurs.
        For specific times, add availability window details above.
      </p>
    </div>
  {/if}
</div>

<style>
  .time-selector {
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
  
  .date-range {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1rem;
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
  
  .input,
  .select {
    width: 100%;
    padding: 0.5rem 0.75rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #1f2937;
    background: white;
  }
  
  .input:focus,
  .select:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .input:read-only,
  .select:disabled {
    background: #f9fafb;
    cursor: not-allowed;
  }
  
  .input::placeholder {
    color: #9ca3af;
  }
  
  .availability-window-section {
    padding: 1.5rem;
    background: #fafafa;
    border-radius: 0.5rem;
    border: 1px solid #e5e7eb;
  }
  
  .info-box {
    padding: 1rem;
    background: #f0f9ff;
    border-left: 3px solid #3b82f6;
    border-radius: 0.375rem;
  }
  
  .info-box p {
    margin: 0;
    font-size: 0.875rem;
    color: #1e40af;
  }
</style>



