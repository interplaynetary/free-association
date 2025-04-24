<script lang="ts">
  import { onMount, onDestroy, createEventDispatcher } from "svelte";
  import {
    RelationalForm,
    type RelationalFormDefinition,
    type RelationalField,
    type ValidationResult,
  } from "../../utils/basic/RelationalForm";
  import { ReactiveGraph } from "../../utils/reactive/ReactiveGraph";

  // Define option interface for select fields
  interface SelectOption {
    value: string;
    label: string;
  }

  // Extend RelationalField to include options
  interface ExtendedField extends RelationalField {
    options?: SelectOption[];
  }

  const dispatch = createEventDispatcher();

  // Component props
  export let definition: RelationalFormDefinition;
  export let graph: ReactiveGraph;
  export let contextPath: string[] = [];
  export let showSubmitButton: boolean = true;
  export let submitButtonText: string = "Submit";
  export let resetButtonText: string = "Reset";
  export let showResetButton: boolean = true;
  export let autoSubmit: boolean = false;
  export let debounceSubmitMs: number = 500;
  export let preserveStateAfterSubmit: boolean = true; // Controls if form state persists after submit

  // Form instance
  let form: RelationalForm;
  let formState: any; // Will be populated by subscription
  let autoSubmitTimer: ReturnType<typeof setTimeout> | null = null;
  let lastSubmittedValues: any = null;
  let submissionSuccessful: boolean = false;

  // Initialize the form when component mounts
  onMount(() => {
    form = new RelationalForm(graph, definition);

    // Subscribe to form state changes
    const unsubscribe = form.getState().subscribe((state) => {
      formState = state;

      // Handle auto-submit if enabled
      if (autoSubmit && formState.formValid && !formState.submitting) {
        if (autoSubmitTimer) {
          clearTimeout(autoSubmitTimer);
        }

        autoSubmitTimer = setTimeout(() => {
          handleSubmit();
        }, debounceSubmitMs);
      }

      // Dispatch state change event
      dispatch("stateChange", { state: formState });
    });

    // Set context path if provided
    if (contextPath.length > 0) {
      form.setContextPath(contextPath);
    }

    return () => {
      unsubscribe();
      if (autoSubmitTimer) clearTimeout(autoSubmitTimer);
    };
  });

  // Clean up resources when component is destroyed
  onDestroy(() => {
    if (form) {
      form.destroy();
    }

    if (autoSubmitTimer) {
      clearTimeout(autoSubmitTimer);
    }
  });

  // Update context path when prop changes
  $: if (form && contextPath) {
    form.setContextPath(contextPath);
  }

  // Handle value changes
  function handleChange(fieldName: string, value: any) {
    if (!form) return;

    // Reset submission success state when user modifies the form
    submissionSuccessful = false;

    form.setFieldValue(fieldName, value);
    form.touchField(fieldName);

    // Just validate the field, don't try to submit it
    setTimeout(() => {
      if (form) {
        // This will just validate the form without submitting
        form.validateField?.(fieldName);
      }
    }, 100);

    // Dispatch change event
    dispatch("change", { field: fieldName, value });
  }

  // Handle field blur
  function handleBlur(fieldName: string) {
    if (!form) return;

    form.touchField(fieldName);
    form.setSuggestionsVisible(fieldName, false);

    // Dispatch blur event
    dispatch("blur", { field: fieldName });
  }

  // Handle field focus
  function handleFocus(fieldName: string) {
    if (!form) return;

    form.setSuggestionsVisible(fieldName, true);

    // Dispatch focus event
    dispatch("focus", { field: fieldName });
  }

  // Handle suggestion selection
  function handleSelectSuggestion(fieldName: string, suggestion: any) {
    if (!form) return;

    form.selectSuggestion(fieldName, suggestion);
    form.setSuggestionsVisible(fieldName, false);

    // Dispatch suggestion event
    dispatch("suggestion", { field: fieldName, suggestion });
  }

  // Handle form submission
  async function handleSubmit() {
    if (!form) return;

    try {
      const success = await form.submit();

      if (success) {
        // Store the submitted values for potential display
        lastSubmittedValues = { ...formState.values };
        submissionSuccessful = true;

        // Emit the submit event with values
        dispatch("submit", { success, values: lastSubmittedValues });

        // If we're not preserving state, reset the form manually
        if (!preserveStateAfterSubmit) {
          form.reset();
          submissionSuccessful = false;
        }
      } else {
        dispatch("error", { message: formState.error });
      }
    } catch (error) {
      console.error("Error submitting form:", error);
      dispatch("error", { message: "Error submitting form" });
    }
  }

  // Handle form reset
  function handleReset() {
    if (!form) return;

    submissionSuccessful = false;
    form.reset();
    dispatch("reset");
  }

  // Helper function to determine if a field has errors
  function hasError(fieldName: string): boolean {
    if (!formState?.fields?.[fieldName]?.validation) return false;

    return formState.fields[fieldName].validation.some(
      (v: ValidationResult) => !v.valid && v.level === "error",
    );
  }

  // Helper function to get validation messages for a field
  function getValidationMessages(fieldName: string): string[] {
    if (!formState?.fields?.[fieldName]?.validation) return [];

    return formState.fields[fieldName].validation
      .filter((v: ValidationResult) => !v.valid && v.message)
      .map((v: ValidationResult) => v.message);
  }

  // Helper function to determine field CSS classes
  function getFieldClasses(fieldName: string): string {
    if (!formState?.fields?.[fieldName]) return "";

    const fieldState = formState.fields[fieldName];
    const classes = [];

    if (fieldState.touched) classes.push("touched");
    if (fieldState.dirty) classes.push("dirty");
    if (hasError(fieldName)) classes.push("has-error");
    if (!fieldState.enabled) classes.push("disabled");

    return classes.join(" ");
  }

  // Type-safe event handlers
  function handleTextInput(event: Event, fieldName: string) {
    const input = event.target as HTMLInputElement;
    handleChange(fieldName, input.value);
  }

  function handleNumberInput(event: Event, fieldName: string) {
    const input = event.target as HTMLInputElement;
    handleChange(fieldName, parseFloat(input.value) || 0);
  }

  function handleCheckboxChange(event: Event, fieldName: string) {
    const input = event.target as HTMLInputElement;
    handleChange(fieldName, input.checked);
  }

  function handleSelectChange(event: Event, fieldName: string) {
    const select = event.target as HTMLSelectElement;
    handleChange(fieldName, select.value);
  }
</script>

<div class="relational-form">
  {#if formState}
    {#if submissionSuccessful && preserveStateAfterSubmit}
      <div class="success-message">
        <h3>Form Submitted Successfully!</h3>
        <p>The form was submitted with the current values.</p>
        <button
          class="close-button"
          onclick={() => (submissionSuccessful = false)}>Close</button
        >
      </div>
    {/if}

    <form
      on:submit|preventDefault={handleSubmit}
      class:valid={formState.formValid}
      class:submitting={formState.submitting}
      class:success={submissionSuccessful}
    >
      {#if definition.title}
        <h2 class="form-title">{definition.title}</h2>
      {/if}

      {#if definition.description}
        <p class="form-description">{definition.description}</p>
      {/if}

      {#each definition.fields as field}
        {#if formState.fields[field.name]?.visible}
          <div class="form-field {getFieldClasses(field.name)}">
            <label for={field.name}>
              {field.label}
              {#if field.required}<span class="required-mark">*</span>{/if}
            </label>

            {#if field.type === "text"}
              <input
                type="text"
                id={field.name}
                name={field.name}
                value={formState.values[field.name] || ""}
                placeholder={field.placeholder || ""}
                disabled={!formState.fields[field.name].enabled}
                on:input={(e) => handleTextInput(e, field.name)}
                on:blur={() => handleBlur(field.name)}
                on:focus={() => handleFocus(field.name)}
              />
            {:else if field.type === "number"}
              <input
                type="number"
                id={field.name}
                name={field.name}
                value={formState.values[field.name] || ""}
                placeholder={field.placeholder || ""}
                disabled={!formState.fields[field.name].enabled}
                on:input={(e) => handleNumberInput(e, field.name)}
                on:blur={() => handleBlur(field.name)}
                on:focus={() => handleFocus(field.name)}
              />
            {:else if field.type === "boolean"}
              <input
                type="checkbox"
                id={field.name}
                name={field.name}
                checked={formState.values[field.name] || false}
                disabled={!formState.fields[field.name].enabled}
                on:change={(e) => handleCheckboxChange(e, field.name)}
                on:blur={() => handleBlur(field.name)}
              />
            {:else if field.type === "select"}
              <select
                id={field.name}
                name={field.name}
                value={formState.values[field.name] || ""}
                disabled={!formState.fields[field.name].enabled}
                on:change={(e) => handleSelectChange(e, field.name)}
                on:blur={() => handleBlur(field.name)}
                on:focus={() => handleFocus(field.name)}
              >
                <option value="" disabled
                  >{field.placeholder || "Select an option"}</option
                >
                {#if (field as ExtendedField).options}
                  {#each (field as ExtendedField).options || [] as option}
                    <option value={option.value}>{option.label}</option>
                  {/each}
                {/if}
              </select>
            {:else if field.type === "node-reference" || field.type === "collection"}
              <div class="combobox">
                <input
                  type="text"
                  id={field.name}
                  name={field.name}
                  value={formState.values[field.name] || ""}
                  placeholder={field.placeholder || ""}
                  disabled={!formState.fields[field.name].enabled}
                  on:input={(e) => handleTextInput(e, field.name)}
                  on:blur={() => handleBlur(field.name)}
                  on:focus={() => handleFocus(field.name)}
                />

                {#if formState.fields[field.name].loading}
                  <div class="loading-indicator">Loading...</div>
                {/if}

                {#if formState.suggestionsVisible[field.name] && formState.fields[field.name].suggestions.length > 0}
                  <ul class="suggestions">
                    {#each formState.fields[field.name].suggestions as suggestion}
                      <li
                        onclick={() =>
                          handleSelectSuggestion(field.name, suggestion)}
                        aria-label={field.displayProperty
                          ? suggestion[field.displayProperty]
                          : suggestion}
                      >
                        {field.displayProperty
                          ? suggestion[field.displayProperty]
                          : suggestion}
                      </li>
                    {/each}
                  </ul>
                {/if}
              </div>
            {/if}

            {#if field.helperText}
              <div class="helper-text">{field.helperText}</div>
            {/if}

            {#if hasError(field.name) && formState.fields[field.name].touched}
              <div class="error-messages">
                {#each getValidationMessages(field.name) as message}
                  <div class="error-message">{message}</div>
                {/each}
              </div>
            {/if}
          </div>
        {/if}
      {/each}

      {#if formState.error}
        <div class="form-error">{formState.error}</div>
      {/if}

      <div class="form-actions">
        {#if showSubmitButton}
          <button
            type="submit"
            class="submit-button"
            disabled={!formState.formValid || formState.submitting}
          >
            {formState.submitting ? "Submitting..." : submitButtonText}
          </button>
        {/if}

        {#if showResetButton}
          <button
            type="button"
            class="reset-button"
            onclick={handleReset}
            disabled={formState.submitting}
          >
            {resetButtonText}
          </button>
        {/if}
      </div>
    </form>
  {:else}
    <div class="loading">Loading form...</div>
  {/if}
</div>

<style>
  .relational-form {
    font-family:
      system-ui,
      -apple-system,
      BlinkMacSystemFont,
      "Segoe UI",
      Roboto,
      Oxygen,
      Ubuntu,
      Cantarell,
      "Open Sans",
      "Helvetica Neue",
      sans-serif;
    max-width: 600px;
    margin: 0 auto;
    padding: 1rem;
  }

  .form-title {
    font-size: 1.5rem;
    margin-bottom: 0.75rem;
  }

  .form-description {
    margin-bottom: 1.5rem;
    color: #555;
  }

  .form-field {
    margin-bottom: 1.25rem;
  }

  label {
    display: block;
    margin-bottom: 0.5rem;
    font-weight: 500;
  }

  .required-mark {
    color: #d32f2f;
    margin-left: 0.25rem;
  }

  input[type="text"],
  input[type="number"],
  select {
    width: 100%;
    padding: 0.5rem;
    border: 1px solid #ccc;
    border-radius: 4px;
    font-size: 1rem;
  }

  input[type="checkbox"] {
    margin-right: 0.5rem;
  }

  .helper-text {
    margin-top: 0.25rem;
    color: #666;
    font-size: 0.875rem;
  }

  .has-error input,
  .has-error select {
    border-color: #d32f2f;
  }

  .error-messages {
    margin-top: 0.5rem;
    color: #d32f2f;
    font-size: 0.875rem;
  }

  .form-error {
    margin: 1rem 0;
    padding: 0.75rem;
    background-color: #ffebee;
    border: 1px solid #ffcdd2;
    border-radius: 4px;
    color: #c62828;
  }

  .form-actions {
    display: flex;
    justify-content: flex-end;
    margin-top: 1.5rem;
    gap: 0.75rem;
  }

  button {
    padding: 0.5rem 1rem;
    border-radius: 4px;
    font-size: 1rem;
    cursor: pointer;
    border: none;
  }

  .submit-button {
    background-color: #1976d2;
    color: white;
  }

  .submit-button:disabled {
    background-color: #bbdefb;
    cursor: not-allowed;
  }

  .reset-button {
    background-color: #f5f5f5;
    color: #424242;
    border: 1px solid #e0e0e0;
  }

  .combobox {
    position: relative;
  }

  .suggestions {
    position: absolute;
    top: 100%;
    left: 0;
    width: 100%;
    max-height: 200px;
    overflow-y: auto;
    background-color: white;
    border: 1px solid #ccc;
    border-top: none;
    border-radius: 0 0 4px 4px;
    z-index: 10;
    margin: 0;
    padding: 0;
    list-style: none;
  }

  .suggestions li {
    padding: 0.5rem;
    cursor: pointer;
  }

  .suggestions li:hover {
    background-color: #f5f5f5;
  }

  .loading-indicator {
    position: absolute;
    right: 0.5rem;
    top: 50%;
    transform: translateY(-50%);
    font-size: 0.875rem;
    color: #757575;
  }

  .loading {
    text-align: center;
    padding: 2rem;
    color: #757575;
  }

  .disabled {
    opacity: 0.7;
    cursor: not-allowed;
  }

  .success-message {
    margin-bottom: 1rem;
    padding: 0.75rem;
    background-color: #e8f5e9;
    border: 1px solid #a5d6a7;
    border-radius: 4px;
    position: relative;
  }

  .success-message h3 {
    color: #2e7d32;
    margin-top: 0;
    margin-bottom: 0.5rem;
  }

  .close-button {
    position: absolute;
    top: 0.5rem;
    right: 0.5rem;
    background: none;
    border: none;
    color: #2e7d32;
    cursor: pointer;
    font-size: 0.875rem;
    padding: 0.25rem 0.5rem;
  }

  form.success {
    border: 1px solid #a5d6a7;
    border-radius: 4px;
    padding: 1rem;
    background-color: #f1f8e9;
  }
</style>
