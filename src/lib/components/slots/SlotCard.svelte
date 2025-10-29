<!--
  SlotCard.svelte
  
  Display a single slot in card format
  
  Usage:
    <SlotCard 
      slot={slot}
      slotType="need" or "capacity"
      onEdit={(slot) => {...}}
      onDelete={(id) => {...}}
    />
-->

<script lang="ts">
  import type { NeedSlot, AvailabilitySlot } from '$lib/commons/v5/schemas';
  import { formatSlotTimeDisplay, formatSlotLocationDisplay } from '$lib/commons/v5/protocol';
  
  interface Props {
    slot: NeedSlot | AvailabilitySlot;
    slotType?: 'need' | 'capacity';
    onEdit?: (slot: NeedSlot | AvailabilitySlot) => void;
    onDelete?: (id: string) => void;
    readonly?: boolean;
    compact?: boolean;
  }
  
  let { 
    slot, 
    slotType, 
    onEdit, 
    onDelete, 
    readonly = false,
    compact = false
  }: Props = $props();
  
  // Format time display
  const timeDisplay = $derived(formatSlotTimeDisplay(slot));
  const locationDisplay = $derived(formatSlotLocationDisplay(slot));
  
  // Check if recurring
  const isRecurring = $derived(
    slot.recurrence && slot.recurrence !== null && slot.recurrence !== ''
  );
  
  function handleEdit() {
    onEdit?.(slot);
  }
  
  function handleDelete() {
    if (confirm(`Delete "${slot.name}"?`)) {
      onDelete?.(slot.id);
    }
  }
</script>

<article 
  class="slot-card" 
  class:compact
  data-testid="slot-card-{slot.id}"
>
  <div class="card-header">
    <div class="header-left">
      {#if slot.emoji}
        <span class="emoji">{slot.emoji}</span>
      {/if}
      <div class="title-group">
        <h3 class="title" data-testid="slot-name">{slot.name}</h3>
        {#if slot.need_type_id}
          <span class="need-type-badge">{slot.need_type_id}</span>
        {/if}
      </div>
    </div>
    
    {#if !readonly && (onEdit || onDelete)}
      <div class="actions">
        {#if onEdit}
          <button
            type="button"
            class="action-button"
            onclick={handleEdit}
            title="Edit"
            data-testid="edit-button"
          >
            ✏️
          </button>
        {/if}
        {#if onDelete}
          <button
            type="button"
            class="action-button delete"
            onclick={handleDelete}
            title="Delete"
            data-testid="delete-button"
          >
            🗑️
          </button>
        {/if}
      </div>
    {/if}
  </div>
  
  <div class="card-body">
    <!-- Quantity -->
    <div class="info-row">
      <span class="icon">📊</span>
      <span class="info-label">Quantity:</span>
      <span class="info-value" data-testid="slot-quantity">
        <strong>{slot.quantity}</strong> {slot.unit || 'units'}
      </span>
    </div>
    
    <!-- Time -->
    <div class="info-row">
      <span class="icon">🕐</span>
      <span class="info-label">When:</span>
      <span class="info-value">
        {timeDisplay}
        {#if isRecurring}
          <span class="badge recurring">Recurring</span>
        {/if}
      </span>
    </div>
    
    <!-- Location -->
    <div class="info-row">
      <span class="icon">📍</span>
      <span class="info-label">Where:</span>
      <span class="info-value">{locationDisplay}</span>
    </div>
    
    <!-- Description (if not compact) -->
    {#if !compact && slot.description}
      <div class="description">
        <p>{slot.description}</p>
      </div>
    {/if}
    
    <!-- Filters (if present) -->
    {#if slot.filter_rule}
      <div class="info-row">
        <span class="icon">🔒</span>
        <span class="info-label">Access:</span>
        <span class="info-value">
          {#if slot.filter_rule.type === 'trust'}
            Trust-based
          {:else if slot.filter_rule.type === 'deny_all'}
            Private
          {:else}
            Filtered
          {/if}
        </span>
      </div>
    {/if}
  </div>
</article>

<style>
  .slot-card {
    background: white;
    border: 1px solid #e5e7eb;
    border-radius: 0.5rem;
    box-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1);
    transition: all 0.2s;
    overflow: hidden;
  }
  
  .slot-card:hover {
    box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1);
    transform: translateY(-2px);
  }
  
  .slot-card.compact {
    padding: 0.75rem;
  }
  
  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: start;
    padding: 1rem;
    border-bottom: 1px solid #f3f4f6;
    background: #fafafa;
  }
  
  .compact .card-header {
    padding: 0 0 0.5rem 0;
    border-bottom: none;
    background: transparent;
  }
  
  .header-left {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    flex: 1;
  }
  
  .emoji {
    font-size: 2rem;
    line-height: 1;
  }
  
  .compact .emoji {
    font-size: 1.5rem;
  }
  
  .title-group {
    display: flex;
    flex-direction: column;
    gap: 0.25rem;
  }
  
  .title {
    margin: 0;
    font-size: 1.125rem;
    font-weight: 600;
    color: #1f2937;
  }
  
  .compact .title {
    font-size: 1rem;
  }
  
  .need-type-badge {
    display: inline-flex;
    padding: 0.125rem 0.5rem;
    background: #dbeafe;
    color: #1e40af;
    font-size: 0.75rem;
    font-weight: 500;
    border-radius: 0.25rem;
    text-transform: capitalize;
    width: fit-content;
  }
  
  .actions {
    display: flex;
    gap: 0.25rem;
  }
  
  .action-button {
    width: 2rem;
    height: 2rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.25rem;
    background: white;
    font-size: 1rem;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .action-button:hover {
    background: #f9fafb;
    border-color: #d1d5db;
  }
  
  .action-button.delete:hover {
    background: #fef2f2;
    border-color: #ef4444;
  }
  
  .card-body {
    padding: 1rem;
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }
  
  .compact .card-body {
    padding: 0;
    gap: 0.5rem;
  }
  
  .info-row {
    display: grid;
    grid-template-columns: auto auto 1fr;
    gap: 0.5rem;
    align-items: start;
    font-size: 0.875rem;
  }
  
  .compact .info-row {
    font-size: 0.8125rem;
  }
  
  .icon {
    font-size: 1rem;
  }
  
  .info-label {
    font-weight: 500;
    color: #6b7280;
    white-space: nowrap;
  }
  
  .info-value {
    color: #1f2937;
    display: flex;
    align-items: center;
    gap: 0.5rem;
    flex-wrap: wrap;
  }
  
  .badge {
    padding: 0.125rem 0.5rem;
    font-size: 0.6875rem;
    font-weight: 600;
    border-radius: 0.25rem;
    text-transform: uppercase;
  }
  
  .badge.recurring {
    background: #dbeafe;
    color: #1e40af;
  }
  
  .description {
    padding-top: 0.5rem;
    border-top: 1px solid #f3f4f6;
  }
  
  .description p {
    margin: 0;
    font-size: 0.875rem;
    color: #6b7280;
    line-height: 1.5;
  }
</style>



