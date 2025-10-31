<!--
  DayScheduleEditor.svelte
  
  Editor for day schedules: select days + time ranges
  Example: "Monday & Friday: 9am-12pm, 2pm-5pm"
  
  Usage:
    <DayScheduleEditor 
      days={['monday', 'friday']}
      timeRanges={[...]}
      onChange={(days, ranges) => {...}}
    />
-->

<script lang="ts">
  import type { DayOfWeek, TimeRange } from '$lib/protocol/schemas';
  import TimeRangeInput from './TimeRangeInput.svelte';
  
  interface Props {
    days: DayOfWeek[];
    timeRanges: TimeRange[];
    onChange?: (days: DayOfWeek[], timeRanges: TimeRange[]) => void;
    onRemove?: () => void;
    readonly?: boolean;
    showRemove?: boolean;
  }
  
  let { 
    days = $bindable(),
    timeRanges = $bindable(),
    onChange,
    onRemove,
    readonly = false,
    showRemove = true
  }: Props = $props();
  
  const ALL_DAYS: { value: DayOfWeek; label: string; short: string }[] = [
    { value: 'monday', label: 'Monday', short: 'Mon' },
    { value: 'tuesday', label: 'Tuesday', short: 'Tue' },
    { value: 'wednesday', label: 'Wednesday', short: 'Wed' },
    { value: 'thursday', label: 'Thursday', short: 'Thu' },
    { value: 'friday', label: 'Friday', short: 'Fri' },
    { value: 'saturday', label: 'Saturday', short: 'Sat' },
    { value: 'sunday', label: 'Sunday', short: 'Sun' }
  ];
  
  function toggleDay(day: DayOfWeek) {
    if (readonly) return;
    
    if (days.includes(day)) {
      days = days.filter(d => d !== day);
    } else {
      days = [...days, day];
    }
    onChange?.(days, timeRanges);
  }
  
  function addTimeRange() {
    if (readonly) return;
    
    timeRanges = [
      ...timeRanges,
      { start_time: '09:00', end_time: '17:00' }
    ];
    onChange?.(days, timeRanges);
  }
  
  function updateTimeRange(index: number, start: string, end: string) {
    const updated = [...timeRanges];
    updated[index] = { start_time: start, end_time: end };
    timeRanges = updated;
    onChange?.(days, updated);
  }
  
  function removeTimeRange(index: number) {
    timeRanges = timeRanges.filter((_, i) => i !== index);
    onChange?.(days, timeRanges);
  }
  
  // Quick select helpers
  function selectWeekdays() {
    if (readonly) return;
    days = ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'];
    onChange?.(days, timeRanges);
  }
  
  function selectWeekends() {
    if (readonly) return;
    days = ['saturday', 'sunday'];
    onChange?.(days, timeRanges);
  }
  
  function selectAllDays() {
    if (readonly) return;
    days = ALL_DAYS.map(d => d.value);
    onChange?.(days, timeRanges);
  }
  
  function clearDays() {
    if (readonly) return;
    days = [];
    onChange?.(days, timeRanges);
  }
</script>

<div class="day-schedule-editor" data-testid="day-schedule-editor">
  <div class="header">
    <h4 class="title">Day Schedule</h4>
    {#if showRemove && !readonly}
      <button
        type="button"
        class="remove-button"
        onclick={onRemove}
        title="Remove day schedule"
        data-testid="remove-day-schedule"
      >
        ✕
      </button>
    {/if}
  </div>
  
  <!-- Day Selection -->
  <div class="days-section">
    <label class="section-label">Days</label>
    
    {#if !readonly}
      <div class="quick-select">
        <button type="button" class="quick-button" onclick={selectWeekdays}>Weekdays</button>
        <button type="button" class="quick-button" onclick={selectWeekends}>Weekends</button>
        <button type="button" class="quick-button" onclick={selectAllDays}>All Days</button>
        <button type="button" class="quick-button" onclick={clearDays}>Clear</button>
      </div>
    {/if}
    
    <div class="days-grid">
      {#each ALL_DAYS as day (day.value)}
        <button
          type="button"
          class="day-button"
          class:selected={days.includes(day.value)}
          onclick={() => toggleDay(day.value)}
          disabled={readonly}
          data-testid="day-{day.value}"
          title={day.label}
        >
          <span class="day-short">{day.short}</span>
          <span class="day-full">{day.label}</span>
        </button>
      {/each}
    </div>
  </div>
  
  <!-- Time Ranges -->
  <div class="times-section">
    <div class="section-header">
      <label class="section-label">Time Ranges</label>
      {#if !readonly}
        <button
          type="button"
          class="add-button"
          onclick={addTimeRange}
          data-testid="add-time-range"
        >
          + Add Time
        </button>
      {/if}
    </div>
    
    {#if timeRanges.length === 0}
      <div class="empty-state">
        No time ranges specified (available all day)
      </div>
    {:else}
      <div class="time-ranges-list">
        {#each timeRanges as range, index (index)}
          <TimeRangeInput
            bind:startTime={range.start_time}
            bind:endTime={range.end_time}
            onChange={(start, end) => updateTimeRange(index, start, end)}
            onRemove={() => removeTimeRange(index)}
            {readonly}
          />
        {/each}
      </div>
    {/if}
  </div>
</div>

<style>
  .day-schedule-editor {
    padding: 1rem;
    border: 2px solid #e5e7eb;
    border-radius: 0.5rem;
    background: #fafafa;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  .header {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .title {
    margin: 0;
    font-size: 0.875rem;
    font-weight: 600;
    color: #374151;
  }
  
  .remove-button {
    width: 1.5rem;
    height: 1.5rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.25rem;
    background: white;
    color: #6b7280;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .remove-button:hover {
    border-color: #ef4444;
    background: #fef2f2;
    color: #ef4444;
  }
  
  .days-section,
  .times-section {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .section-label {
    font-weight: 600;
    font-size: 0.75rem;
    color: #6b7280;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  
  .section-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .quick-select {
    display: flex;
    gap: 0.5rem;
    flex-wrap: wrap;
  }
  
  .quick-button {
    padding: 0.25rem 0.5rem;
    border: 1px solid #d1d5db;
    border-radius: 0.25rem;
    background: white;
    font-size: 0.75rem;
    color: #6b7280;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .quick-button:hover {
    border-color: #3b82f6;
    background: #eff6ff;
    color: #3b82f6;
  }
  
  .days-grid {
    display: grid;
    grid-template-columns: repeat(7, 1fr);
    gap: 0.5rem;
  }
  
  .day-button {
    padding: 0.5rem 0.25rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    background: white;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.25rem;
  }
  
  .day-button:hover:not(:disabled) {
    border-color: #3b82f6;
    background: #eff6ff;
    transform: translateY(-2px);
  }
  
  .day-button.selected {
    border-color: #3b82f6;
    background: #dbeafe;
    font-weight: 600;
  }
  
  .day-button:disabled {
    cursor: not-allowed;
    opacity: 0.6;
  }
  
  .day-short {
    font-size: 0.875rem;
    font-weight: 600;
    color: #1f2937;
  }
  
  .day-full {
    display: none;
    font-size: 0.75rem;
    color: #6b7280;
  }
  
  @media (min-width: 640px) {
    .day-short {
      display: none;
    }
    
    .day-full {
      display: block;
    }
  }
  
  .add-button {
    padding: 0.375rem 0.75rem;
    border: 1px solid #3b82f6;
    border-radius: 0.25rem;
    background: white;
    color: #3b82f6;
    font-size: 0.75rem;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .add-button:hover {
    background: #3b82f6;
    color: white;
  }
  
  .time-ranges-list {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .empty-state {
    padding: 1rem;
    text-align: center;
    font-size: 0.875rem;
    color: #9ca3af;
    background: white;
    border: 1px dashed #d1d5db;
    border-radius: 0.375rem;
  }
</style>



