<!--
  AvailabilityWindowEditor.svelte
  
  Editor for hierarchical availability windows
  Supports day schedules and simple time ranges (Level 3 and 4 of the hierarchy)
  
  Usage:
    <AvailabilityWindowEditor 
      window={slot.availability_window}
      onChange={(window) => {...}}
    />
-->

<script lang="ts">
  import type { AvailabilityWindow, DaySchedule, TimeRange } from '$lib/protocol/schemas';
  import DayScheduleEditor from './DayScheduleEditor.svelte';
  import TimeRangeInput from './TimeRangeInput.svelte';
  
  interface Props {
    window?: AvailabilityWindow;
    onChange?: (window: AvailabilityWindow | undefined) => void;
    readonly?: boolean;
  }
  
  let { 
    window = $bindable(),
    onChange,
    readonly = false
  }: Props = $props();
  
  // Track which level of specificity to use
  type WindowLevel = 'none' | 'time-only' | 'day-schedules';
  let currentLevel = $state<WindowLevel>(
    window?.day_schedules && window.day_schedules.length > 0
      ? 'day-schedules'
      : window?.time_ranges && window.time_ranges.length > 0
      ? 'time-only'
      : 'none'
  );
  
  // Initialize window structure if needed
  function ensureWindow(): AvailabilityWindow {
    if (!window) {
      window = {};
    }
    return window;
  }
  
  function handleLevelChange(newLevel: WindowLevel) {
    currentLevel = newLevel;
    
    const w = ensureWindow();
    
    // Clear all levels
    w.day_schedules = undefined;
    w.time_ranges = undefined;
    
    // Initialize the selected level
    if (newLevel === 'day-schedules') {
      w.day_schedules = [{
        days: ['monday', 'tuesday', 'wednesday', 'thursday', 'friday'],
        time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
      }];
    } else if (newLevel === 'time-only') {
      w.time_ranges = [{ start_time: '09:00', end_time: '17:00' }];
    } else {
      // 'none' - keep window empty (available anytime)
      window = undefined;
    }
    
    onChange?.(window);
  }
  
  // Day schedules management
  function addDaySchedule() {
    const w = ensureWindow();
    w.day_schedules = [
      ...(w.day_schedules || []),
      {
        days: [],
        time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
      }
    ];
    onChange?.(w);
  }
  
  function updateDaySchedule(index: number, days: any[], timeRanges: TimeRange[]) {
    const w = ensureWindow();
    if (w.day_schedules) {
      const updated = [...w.day_schedules];
      updated[index] = { days, time_ranges: timeRanges };
      w.day_schedules = updated;
      onChange?.(w);
    }
  }
  
  function removeDaySchedule(index: number) {
    const w = ensureWindow();
    if (w.day_schedules) {
      w.day_schedules = w.day_schedules.filter((_, i) => i !== index);
      onChange?.(w);
    }
  }
  
  // Simple time ranges management
  function addTimeRange() {
    const w = ensureWindow();
    w.time_ranges = [
      ...(w.time_ranges || []),
      { start_time: '09:00', end_time: '17:00' }
    ];
    onChange?.(w);
  }
  
  function updateTimeRange(index: number, start: string, end: string) {
    const w = ensureWindow();
    if (w.time_ranges) {
      const updated = [...w.time_ranges];
      updated[index] = { start_time: start, end_time: end };
      w.time_ranges = updated;
      onChange?.(w);
    }
  }
  
  function removeTimeRange(index: number) {
    const w = ensureWindow();
    if (w.time_ranges) {
      w.time_ranges = w.time_ranges.filter((_, i) => i !== index);
      onChange?.(w);
    }
  }
</script>

<div class="availability-window-editor" data-testid="availability-window-editor">
  <div class="header">
    <h3 class="title">When is this available?</h3>
    <p class="description">
      Define when this slot is available using specific days and times
    </p>
  </div>
  
  <!-- Level Selector -->
  {#if !readonly}
    <div class="level-selector">
      <button
        type="button"
        class="level-button"
        class:selected={currentLevel === 'none'}
        onclick={() => handleLevelChange('none')}
        data-testid="level-none"
      >
        <span class="level-title">Anytime</span>
        <span class="level-desc">No specific schedule</span>
      </button>
      
      <button
        type="button"
        class="level-button"
        class:selected={currentLevel === 'time-only'}
        onclick={() => handleLevelChange('time-only')}
        data-testid="level-time-only"
      >
        <span class="level-title">Time Only</span>
        <span class="level-desc">Same times every day</span>
      </button>
      
      <button
        type="button"
        class="level-button"
        class:selected={currentLevel === 'day-schedules'}
        onclick={() => handleLevelChange('day-schedules')}
        data-testid="level-day-schedules"
      >
        <span class="level-title">Specific Days</span>
        <span class="level-desc">Different days + times</span>
      </button>
    </div>
  {/if}
  
  <!-- Content based on selected level -->
  <div class="level-content">
    {#if currentLevel === 'none'}
      <div class="info-box">
        <p>This slot is available at any time. No specific schedule restrictions.</p>
      </div>
      
    {:else if currentLevel === 'time-only'}
      <div class="time-only-section">
        <div class="section-header">
          <label class="section-label">Daily Time Ranges</label>
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
        
        {#if window?.time_ranges && window.time_ranges.length > 0}
          <div class="time-ranges-list">
            {#each window.time_ranges as range, index (index)}
              <TimeRangeInput
                bind:startTime={range.start_time}
                bind:endTime={range.end_time}
                onChange={(start, end) => updateTimeRange(index, start, end)}
                onRemove={() => removeTimeRange(index)}
                {readonly}
              />
            {/each}
          </div>
        {:else}
          <div class="empty-state">
            No time ranges specified. Click "Add Time" to add one.
          </div>
        {/if}
      </div>
      
    {:else if currentLevel === 'day-schedules'}
      <div class="day-schedules-section">
        <div class="section-header">
          <label class="section-label">Day-Specific Schedules</label>
          {#if !readonly}
            <button
              type="button"
              class="add-button"
              onclick={addDaySchedule}
              data-testid="add-day-schedule"
            >
              + Add Schedule
            </button>
          {/if}
        </div>
        
        {#if window?.day_schedules && window.day_schedules.length > 0}
          <div class="day-schedules-list">
            {#each window.day_schedules as schedule, index (index)}
              <DayScheduleEditor
                bind:days={schedule.days}
                bind:timeRanges={schedule.time_ranges}
                onChange={(days, ranges) => updateDaySchedule(index, days, ranges)}
                onRemove={() => removeDaySchedule(index)}
                {readonly}
              />
            {/each}
          </div>
        {:else}
          <div class="empty-state">
            No day schedules specified. Click "Add Schedule" to add one.
          </div>
        {/if}
      </div>
    {/if}
  </div>
</div>

<style>
  .availability-window-editor {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }
  
  .header {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .title {
    margin: 0;
    font-size: 1rem;
    font-weight: 600;
    color: #1f2937;
  }
  
  .description {
    margin: 0;
    font-size: 0.875rem;
    color: #6b7280;
  }
  
  .level-selector {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 0.75rem;
  }
  
  .level-button {
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
  
  .level-button:hover {
    border-color: #3b82f6;
    background: #eff6ff;
    transform: translateY(-2px);
  }
  
  .level-button.selected {
    border-color: #3b82f6;
    background: #dbeafe;
  }
  
  .level-title {
    font-weight: 600;
    font-size: 0.875rem;
    color: #1f2937;
  }
  
  .level-desc {
    font-size: 0.75rem;
    color: #6b7280;
  }
  
  .level-content {
    padding: 1rem;
    background: #f9fafb;
    border-radius: 0.5rem;
    border: 1px solid #e5e7eb;
  }
  
  .section-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }
  
  .section-label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #374151;
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
  
  .time-ranges-list,
  .day-schedules-list {
    display: flex;
    flex-direction: column;
    gap: 1rem;
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
  
  .empty-state {
    padding: 2rem;
    text-align: center;
    font-size: 0.875rem;
    color: #9ca3af;
    background: white;
    border: 1px dashed #d1d5db;
    border-radius: 0.375rem;
  }
</style>



