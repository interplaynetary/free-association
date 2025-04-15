<script lang="ts">
    import Form from '../Form.svelte';
    import { ReactiveGraph } from '../../utils/reactive/ReactiveGraph';
    import type { RelationalFormDefinition, RelationalField } from '../../utils/basic/RelationalForm';
    
    // Create a graph instance
    const graph = new ReactiveGraph();
    
    // For debugging
    let formState: any = null;
    let formSubmitted = false;
    let lastSubmittedValues: any = null;
    
    // Define your form with explicit type annotation
    const definition: RelationalFormDefinition = {
      title: "User Profile",
      description: "Please enter your user information",
      fields: [
        {
          name: "name",
          label: "Full Name",
          type: "text" as const,
          required: true,
          placeholder: "Enter your full name"
        },
        {
          name: "age",
          label: "Age",
          type: "number" as const,
          required: true,
          placeholder: "Enter your age"
        }
      ],
      // Add formValidators
      formValidators: [
        (values) => {
          return { valid: true }; // Always valid for testing
        }
      ],
      submitOptions: {
        targetPath: ["users"],
        collectionKey: "profiles",
        generateId: true,
        relationshipBehavior: "reference",
        // Remove clearAfterSubmit to keep values after submission
        clearAfterSubmit: false
      }
    };
    
    // Handle form submission
    function handleSubmit(event: CustomEvent) {
      console.log('Form submitted with values:', event.detail.values);
      formSubmitted = true;
      lastSubmittedValues = {...event.detail.values};
      
      // Optional: Add a timeout to clear the success message after a few seconds
      setTimeout(() => {
        formSubmitted = false;
      }, 5000);
    }
    
    // Handle form errors
    function handleError(event: CustomEvent) {
      console.error('Form error:', event.detail.message);
      alert('Form error: ' + event.detail.message);
    }
    
    // Handle state changes for debugging
    function handleStateChange(event: CustomEvent) {
      formState = event.detail.state;
      console.log('Form state updated:', formState);
    }
    
    // Force submission for testing
    function forceSubmit() {
      const event = new CustomEvent('submit');
      document.querySelector('form')?.dispatchEvent(event);
    }
  </script>
  
  {#if formSubmitted}
    <div class="success-message">
      <h3>Form Submitted Successfully!</h3>
      <p>The following data was submitted:</p>
      <pre>{JSON.stringify(lastSubmittedValues, null, 2)}</pre>
      <button on:click={() => formSubmitted = false}>Close</button>
    </div>
  {:else}
    <Form 
      {definition}
      {graph}
      contextPath={["users", "current"]}
      preserveStateAfterSubmit={true}
      on:submit={handleSubmit}
      on:error={handleError}
      on:stateChange={handleStateChange}
    />
    
    <!-- Manual submit button for testing -->
    <button class="force-submit" on:click={forceSubmit}>
      Force Submit Form
    </button>
  {/if}
  
  <!-- Debugging info -->
  {#if formState}
    <div class="debug-info">
      <h3>Form Debug Info</h3>
      <p>Form Valid: {formState.formValid}</p>
      <p>Is Submitting: {formState.submitting}</p>
      <p>Error: {formState.error || 'None'}</p>
      
      <h4>Field Values:</h4>
      <pre>{JSON.stringify(formState.values, null, 2)}</pre>
      
      <h4>Validation:</h4>
      {#each Object.entries(formState.fields) as [fieldName, fieldData]}
        <div>
          <strong>{fieldName}:</strong> 
          {(fieldData as any).validation?.map((v: any) => v.valid ? '✓' : `✗ ${v.message || 'Invalid'}`).join(', ')}
        </div>
      {/each}
    </div>
  {/if}
  
  <style>
    .debug-info {
      margin-top: 2rem;
      padding: 1rem;
      border: 1px solid #ccc;
      border-radius: 4px;
      background-color: #f9f9f9;
      font-family: monospace;
    }
    
    .debug-info pre {
      background-color: #f0f0f0;
      padding: 0.5rem;
      border-radius: 4px;
      overflow: auto;
    }
    
    .force-submit {
      display: block;
      margin: 1rem 0;
      padding: 0.5rem 1rem;
      background-color: #f44336;
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
    }
    
    .success-message {
      background-color: #e8f5e9;
      border: 1px solid #a5d6a7;
      border-radius: 4px;
      padding: 1rem;
      margin: 1rem 0;
      max-width: 600px;
    }
    
    .success-message h3 {
      color: #2e7d32;
      margin-top: 0;
    }
    
    .success-message pre {
      background-color: #f5f5f5;
      padding: 0.5rem;
      border-radius: 4px;
      overflow: auto;
    }
    
    .success-message button {
      background-color: #4caf50;
      color: white;
      border: none;
      border-radius: 4px;
      padding: 0.5rem 1rem;
      cursor: pointer;
      margin-top: 1rem;
    }
  </style>