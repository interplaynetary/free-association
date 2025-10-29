<!--
  SlotEditor.svelte
  
  Main slot editor component - provides CRUD interface for slots
  Works for both needs and capacity
  
  Usage:
    <SlotEditor 
      slots={mySlots}
      slotType="need" or "capacity"
      onUpdate={(slots) => {...}}
    />
-->

<script lang="ts">
  import type { NeedSlot, AvailabilitySlot } from '$lib/commons/v5/schemas';
  import SlotForm from './SlotForm.svelte';
  import SlotsList from './SlotsList.svelte';
  
  interface Props {
    slots: (NeedSlot | AvailabilitySlot)[];
    slotType: 'need' | 'capacity';
    onUpdate?: (slots: (NeedSlot | AvailabilitySlot)[]) => void;
    readonly?: boolean;
  }
  
  let { 
    slots = $bindable(),
    slotType, 
    onUpdate,
    readonly = false
  }: Props = $props();
  
  // Editor state
  let showForm = $state(false);
  let editingSlot = $state<(NeedSlot | AvailabilitySlot) | undefined>(undefined);
  
  function handleAddNew() {
    editingSlot = undefined;
    showForm = true;
  }
  
  function handleEdit(slot: NeedSlot | AvailabilitySlot) {
    editingSlot = slot;
    showForm = true;
  }
  
  function handleSave(slot: NeedSlot | AvailabilitySlot) {
    let updatedSlots: (NeedSlot | AvailabilitySlot)[];
    
    if (editingSlot) {
      // Update existing slot
      updatedSlots = slots.map(s => s.id === slot.id ? slot : s);
    } else {
      // Add new slot
      updatedSlots = [...slots, slot];
    }
    
    slots = updatedSlots;
    onUpdate?.(updatedSlots);
    
    // Close form
    showForm = false;
    editingSlot = undefined;
  }
  
  function handleCancel() {
    showForm = false;
    editingSlot = undefined;
  }
  
  function handleDelete(id: string) {
    const updatedSlots = slots.filter(s => s.id !== id);
    slots = updatedSlots;
    onUpdate?.(updatedSlots);
  }
</script>

<div class="slot-editor" data-testid="slot-editor">
  {#if showForm}
    <!-- Editing Mode -->
    <div class="form-container">
      <SlotForm
        slot={editingSlot}
        {slotType}
        onSave={handleSave}
        onCancel={handleCancel}
      />
    </div>
  {:else}
    <!-- List Mode -->
    <div class="list-container">
      <div class="header">
        <div class="header-content">
          <h2 class="title">
            {#if slotType === 'need'}
              My Needs
            {:else}
              My Capacity
            {/if}
          </h2>
          <p class="description">
            {#if slotType === 'need'}
              Resources and support you need from the community
            {:else}
              Resources and support you can provide to the community
            {/if}
          </p>
        </div>
        
        {#if !readonly}
          <button
            type="button"
            class="add-button"
            onclick={handleAddNew}
            data-testid="add-slot-button"
          >
            <span class="button-icon">+</span>
            <span>Add {slotType === 'need' ? 'Need' : 'Capacity'}</span>
          </button>
        {/if}
      </div>
      
      <SlotsList
        {slots}
        {slotType}
        onEdit={handleEdit}
        onDelete={handleDelete}
        {readonly}
        emptyMessage={
          slotType === 'need' 
            ? 'No needs added yet. Click "Add Need" to get started.'
            : 'No capacity added yet. Click "Add Capacity" to get started.'
        }
      />
    </div>
  {/if}
</div>

<style>
  .slot-editor {
    width: 100%;
    min-height: 400px;
  }
  
  .form-container {
    padding: 2rem;
    background: #f9fafb;
    border-radius: 0.75rem;
  }
  
  .list-container {
    display: flex;
    flex-direction: column;
    gap: 2rem;
  }
  
  .header {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  @media (min-width: 640px) {
    .header {
      flex-direction: row;
      justify-content: space-between;
      align-items: start;
    }
  }
  
  .header-content {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .title {
    margin: 0;
    font-size: 1.5rem;
    font-weight: 700;
    color: #1f2937;
  }
  
  .description {
    margin: 0;
    font-size: 1rem;
    color: #6b7280;
  }
  
  .add-button {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.75rem 1.5rem;
    border: none;
    border-radius: 0.5rem;
    background: #3b82f6;
    color: white;
    font-size: 0.875rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
    white-space: nowrap;
  }
  
  .add-button:hover {
    background: #2563eb;
    transform: translateY(-2px);
    box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1);
  }
  
  .button-icon {
    font-size: 1.25rem;
    font-weight: 600;
  }
</style>



