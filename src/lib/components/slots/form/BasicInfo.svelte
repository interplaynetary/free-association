<!--
  BasicInfo.svelte
  
  Basic slot information: name, emoji, description, resource type
  
  Usage:
    <BasicInfo 
      name={slot.name}
      emoji={slot.emoji}
      description={slot.description}
      resourceType={slot.resource_type}
      onUpdate={handleUpdate}
    />
-->

<script lang="ts">
  interface Props {
    name: string;
    emoji?: string;
    description?: string;
    resourceType?: string;
    onUpdate?: (field: string, value: string) => void;
    readonly?: boolean;
  }
  
  let { 
    name = $bindable(),
    emoji = $bindable(),
    description = $bindable(),
    resourceType = $bindable(),
    onUpdate,
    readonly = false
  }: Props = $props();
  
  // Emoji picker state
  let showEmojiPicker = $state(false);
  
  // Common emojis for quick selection
  const QUICK_EMOJIS = [
    '🍎', '🏠', '🏥', '📚', '🚗', '👶', '👴', '🔨', '💼', '📦',
    '⏰', '📅', '📍', '💡', '🎯', '⭐', '🌟', '✨', '🎉', '🎊',
    '❤️', '💚', '💙', '💜', '🧡', '💛', '🤍', '🖤', '🤎', '💖'
  ];
  
  function handleNameChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    name = value;
    onUpdate?.('name', value);
  }
  
  function handleEmojiSelect(selectedEmoji: string) {
    emoji = selectedEmoji;
    onUpdate?.('emoji', selectedEmoji);
    showEmojiPicker = false;
  }
  
  function handleDescriptionChange(e: Event) {
    const value = (e.target as HTMLTextAreaElement).value;
    description = value;
    onUpdate?.('description', value);
  }
  
  function handleResourceTypeChange(e: Event) {
    const value = (e.target as HTMLInputElement).value;
    resourceType = value;
    onUpdate?.('resource_type', value);
  }
</script>

<div class="basic-info" data-testid="basic-info">
  <!-- Name + Emoji -->
  <div class="field-row">
    <!-- Emoji Selector -->
    <div class="emoji-field">
      <label class="label" for="emoji-input">Emoji</label>
      <div class="emoji-container">
        <button
          type="button"
          class="emoji-button"
          onclick={() => showEmojiPicker = !showEmojiPicker}
          disabled={readonly}
          data-testid="emoji-button"
          title="Select emoji"
        >
          {emoji || '➕'}
        </button>
        
        {#if showEmojiPicker && !readonly}
          <div class="emoji-picker" data-testid="emoji-picker">
            <div class="emoji-grid">
              {#each QUICK_EMOJIS as quickEmoji}
                <button
                  type="button"
                  class="quick-emoji"
                  onclick={() => handleEmojiSelect(quickEmoji)}
                  data-testid="emoji-option-{quickEmoji}"
                >
                  {quickEmoji}
                </button>
              {/each}
            </div>
            <button
              type="button"
              class="clear-emoji"
              onclick={() => handleEmojiSelect('')}
            >
              Clear
            </button>
          </div>
        {/if}
      </div>
    </div>
    
    <!-- Name Input -->
    <div class="name-field">
      <label class="label" for="name-input">
        Name <span class="required">*</span>
      </label>
      <input
        id="name-input"
        type="text"
        class="input"
        value={name}
        oninput={handleNameChange}
        placeholder="e.g., Weekly tutoring session"
        required
        readonly={readonly}
        data-testid="name-input"
      />
    </div>
  </div>
  
  <!-- Description -->
  <div class="field">
    <label class="label" for="description-input">Description</label>
    <textarea
      id="description-input"
      class="textarea"
      value={description || ''}
      oninput={handleDescriptionChange}
      placeholder="Provide additional details..."
      rows="3"
      readonly={readonly}
      data-testid="description-input"
    />
  </div>
  
  <!-- Resource Type (optional categorization) -->
  <div class="field">
    <label class="label" for="resource-type-input">
      Resource Type
      <span class="hint">(optional categorization)</span>
    </label>
    <input
      id="resource-type-input"
      type="text"
      class="input"
      value={resourceType || ''}
      oninput={handleResourceTypeChange}
      placeholder="e.g., academic, medical, physical"
      readonly={readonly}
      data-testid="resource-type-input"
    />
  </div>
</div>

<style>
  .basic-info {
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }
  
  .field-row {
    display: grid;
    grid-template-columns: auto 1fr;
    gap: 1rem;
    align-items: start;
  }
  
  .field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .emoji-field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  
  .name-field {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    flex: 1;
  }
  
  .label {
    font-weight: 600;
    font-size: 0.875rem;
    color: #374151;
  }
  
  .required {
    color: #ef4444;
  }
  
  .hint {
    font-weight: 400;
    color: #9ca3af;
    font-size: 0.75rem;
  }
  
  .emoji-container {
    position: relative;
  }
  
  .emoji-button {
    width: 3.5rem;
    height: 3.5rem;
    border: 2px solid #e5e7eb;
    border-radius: 0.5rem;
    background: white;
    font-size: 2rem;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .emoji-button:hover:not(:disabled) {
    border-color: #3b82f6;
    background: #eff6ff;
    transform: scale(1.05);
  }
  
  .emoji-button:disabled {
    cursor: not-allowed;
    opacity: 0.6;
  }
  
  .emoji-picker {
    position: absolute;
    top: 100%;
    left: 0;
    margin-top: 0.5rem;
    background: white;
    border: 1px solid #e5e7eb;
    border-radius: 0.5rem;
    padding: 0.75rem;
    box-shadow: 0 10px 15px -3px rgb(0 0 0 / 0.1);
    z-index: 50;
    width: 240px;
  }
  
  .emoji-grid {
    display: grid;
    grid-template-columns: repeat(6, 1fr);
    gap: 0.25rem;
    margin-bottom: 0.5rem;
  }
  
  .quick-emoji {
    width: 2rem;
    height: 2rem;
    border: 1px solid transparent;
    border-radius: 0.25rem;
    background: transparent;
    font-size: 1.25rem;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    justify-content: center;
  }
  
  .quick-emoji:hover {
    background: #f3f4f6;
    border-color: #d1d5db;
    transform: scale(1.1);
  }
  
  .clear-emoji {
    width: 100%;
    padding: 0.375rem;
    border: 1px solid #e5e7eb;
    border-radius: 0.25rem;
    background: white;
    font-size: 0.75rem;
    color: #6b7280;
    cursor: pointer;
    transition: all 0.2s;
  }
  
  .clear-emoji:hover {
    background: #f9fafb;
    border-color: #d1d5db;
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
  
  .textarea {
    width: 100%;
    padding: 0.5rem 0.75rem;
    border: 1px solid #d1d5db;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    color: #1f2937;
    background: white;
    resize: vertical;
    font-family: inherit;
  }
  
  .textarea:focus {
    outline: none;
    border-color: #3b82f6;
    box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1);
  }
  
  .textarea:read-only {
    background: #f9fafb;
    cursor: not-allowed;
  }
  
  .textarea::placeholder {
    color: #9ca3af;
  }
</style>



