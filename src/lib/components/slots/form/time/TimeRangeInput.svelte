<!--
  TimeRangeInput.svelte
  
  Single time range input (HH:MM - HH:MM)
  
  Usage:
    <TimeRangeInput 
      startTime="09:00"
      endTime="17:00"
      onChange={(start, end) => {...}}
    />
-->

<script lang="ts">
  import type { TimeRange } from '$lib/protocol/schemas';
  
  interface Props {
    startTime: string;
    endTime: string;
    onChange?: (startTime: string, endTime: string) => void;
    onRemove?: () => void;
    readonly?: boolean;
    showRemove?: boolean;
  }
  
  let { 
    startTime = $bindable(),
    endTime = $bindable(),
    onChange,
    onRemove,
    readonly = false,
    showRemove = true
  }: Props = $props();
  
  // Validation state
  let isValid = $derived(startTime < endTime);
  
  function handleStartTimeChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    startTime = value;
    onChange?.(value, endTime);
  }
  
  function handleEndTimeChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    endTime = value;
    onChange?.(startTime, value);
  }
  
  // Format time for display (remove leading zeros: 08:30 → 8:30)
  function formatTime(time: string): string {
    if (!time) return time;
    const [hours, minutes] = time.split(':');
    return `${parseInt(hours)}:${minutes}`;
  }
</script>

<div class="time-range-input" class:invalid={!isValid} data-testid="time-range-input">
  <div class="time-inputs">
    <div class="time-field">
      <label class="label" for="start-time">Start</label>
      <input
        id="start-time"
        type="time"
        class="input time-input"
        value={startTime}
        oninput={handleStartTimeChange}
        readonly={readonly}
        required
        data-testid="start-time-input"
      />
      <span class="time-display">{formatTime(startTime)}</span>
    </div>
    
    <span class="separator">→</span>
    
    <div class="time-field">
      <label class="label" for="end-time">End</label>
      <input
        id="end-time"
        type="time"
        class="input time-input"
        value={endTime}
        oninput={handleEndTimeChange}
        readonly={readonly}
        required
        data-testid="end-time-input"
      />
      <span class="time-display">{formatTime(endTime)}</span>
    </div>
  </div>
  
  {#if showRemove && !readonly}
    <button
      type="button"
      class="remove-button"
      onclick={onRemove}
      title="Remove time range"
      data-testid="remove-time-range"
    >
      ✕
    </button>
  {/if}
  
  {#if !isValid}
    <div class="error-message">
      End time must be after start time
    </div>
  {/if}
</div>

<style>
  .time-range-input {
    display: flex;
    align-items: start;
    gap: 0.5rem;
    padding: 0.75rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.375rem;
    background: white;
  }
  
  .time-range-input.invalid {
    border-color: #ef4444;
    background: #fef2f2;
  }
  
  .time-inputs {
    flex: 1;
    display: flex;
    align-items: center;
    gap: 1rem;
  }
  
  .time-field {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  
  .label {
    font-weight: 500;
    font-size: 0.75rem;
    color: #6b7280;
  }
  
  .time-input {
    padding: 0.375rem 0.5rem;
    border: 1px solid #d1d5db;
    border-radius: 0.25rem;
    font-size: 0.875rem;
    color: #1f2937;
    background: white;
  }
  
  .time-input:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 2px rgba(59, 130, 246, 0.1);
  }
  
  .time-input:read-only {
    background: #f9fafb;
    cursor: not-allowed;
  }
  
  .time-display {
    font-size: 0.75rem;
    color: #9ca3af;
  }
  
  .separator {
    font-size: 1.25rem;
    color: #9ca3af;
    margin-top: 1.5rem;
  }
  
  .remove-button {
    flex-shrink: 0;
    width: 1.75rem;
    height: 1.75rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.25rem;
    background: white;
    color: #6b7280;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.2s;
    margin-top: 1.25rem;
  }
  
  .remove-button:hover {
    border-color: #ef4444;
    background: #fef2f2;
    color: #ef4444;
  }
  
  .error-message {
    position: absolute;
    top: 100%;
    left: 0;
    margin-top: 0.25rem;
    font-size: 0.75rem;
    color: #ef4444;
  }
</style>



