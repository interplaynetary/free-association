import { ReactiveGraph, safeGet } from "../reactive/ReactiveGraph";
import { writable, derived, get, type Readable, type Writable } from "svelte/store";
import type { GunStore, CollectionItem } from "../svelte/reactiveStores";
import { ReactiveComponent, type ReactiveComponentApi, type ComponentRelationship, ComponentRelationshipType } from "../reactive/ReactiveComponent";


/*
Here's how this powerful pattern works:
The RelationalForm implementation enables building interfaces that are deeply integrated with the graph data structure. It provides:
Relationship Context Awareness: Forms understand their place in the graph structure and can adapt behavior based on context paths, making them aware of parent-child and other relationships.
Dynamic Field Management: Fields can show, hide, or change behavior based on relationships in the graph, enabling adaptive forms that present only relevant options.
Intelligent Suggestions: The form provides context-aware suggestions from the graph, with built-in caching and debouncing to ensure performance.
Relationship Validation: Automatic validation of references to ensure data integrity, preventing broken links in the graph.
Multiple Relationship Types: Support for different types of relationships (parent-child, bidirectional, tagging, inheritance) with appropriate handling for each.
Contextual Defaults: Forms can derive default values based on their position in the graph, making new entries easier.
Relationship-Specific Behavior: Different submission strategies for relationships (create, reference, update) handle common graph operations elegantly.
To use this pattern in your application, you can create form definitions that describe fields and their relationship characteristics, then let the RelationalForm handle all the complex logic of validating, suggesting, and persisting relationship data.
This implementation enables building sophisticated interfaces like knowledge graphs, social network connections, and collaborative editing tools with remarkably clean code and excellent developer experience.
*/

/**
 * Types of relationships that can be modeled in forms
 */
export enum RelationshipType {
  PARENT_CHILD = "parent_child",      // Hierarchical relationship
  REFERENCE = "reference",            // Simple reference/pointer
  BIDIRECTIONAL = "bidirectional",    // Mutual relationship (A→B and B→A)
  COLLECTION = "collection",          // One-to-many relationship
  TAGGING = "tagging",                // Categorization relationship
  INHERITANCE = "inheritance",        // Type inheritance relationship
  CUSTOM = "custom"                   // Custom relationship with custom behavior
}

/**
 * Validation rule for form fields
 */
export interface ValidationRule<T = any> {
  validate: (value: T, formValues: Record<string, any>, graph: ReactiveGraph) => boolean | Promise<boolean>;
  message: string | ((value: T, formValues: Record<string, any>) => string);
  level?: 'error' | 'warning' | 'info';
}

/**
 * Field type with relationship awareness
 */
export interface RelationalField<T = any> {
  /** Field name/key */
  name: string;
  /** Human-readable label */
  label: string;
  /** Field data type */
  type: 'text' | 'number' | 'boolean' | 'date' | 'select' | 'node-reference' | 'collection';
  /** Optional placeholder */
  placeholder?: string;
  /** Default value */
  defaultValue?: T;
  /** Whether field is required */
  required?: boolean;
  /** Additional helper text */
  helperText?: string;
  /** Field is read-only */
  readOnly?: boolean;
  /** Field is disabled */
  disabled?: boolean;
  /** Validation rules */
  validators?: ValidationRule<T>[];
  
  // Relational properties
  /** Whether the field represents a relationship */
  isRelationship?: boolean;
  /** Type of relationship if isRelationship is true */
  relationshipType?: RelationshipType;
  /** Path to related node/collection if this is a relationship field */
  relatedPath?: string[];
  /** Property on the related node to display */
  displayProperty?: string;
  /** Whether to verify referenced nodes exist */
  verifyReference?: boolean;
  /** Whether to prevent duplicate references */
  preventDuplicates?: boolean;
  /** Additional relationship metadata */
  metadata?: Record<string, any>;
  
  // Dynamic form behavior
  /** Function to generate suggestions based on input */
  getSuggestions?: (input: string, formValues: Record<string, any>, graph: ReactiveGraph) => Promise<T[]>;
  /** Condition for field visibility */
  visibleWhen?: (formValues: Record<string, any>, graph: ReactiveGraph) => boolean;
  /** Condition for field being enabled */
  enabledWhen?: (formValues: Record<string, any>, graph: ReactiveGraph) => boolean;
  /** Transform input before storage */
  transform?: (value: any, formValues: Record<string, any>) => T;
  /** Optional field weight for automatic ordering */
  weight?: number;
}

/**
 * Form submission behavior 
 */
export interface FormSubmitOptions {
  /** Path where the form data should be stored */
  targetPath: string[];
  /** Collection key if adding to a collection */
  collectionKey?: string;
  /** Auto-generate ID if adding to a collection */
  generateId?: boolean;
  /** What to do with relationship fields */
  relationshipBehavior: 'create' | 'reference' | 'update' | 'custom';
  /** Whether to clear form after successful submit */
  clearAfterSubmit?: boolean;
  /** Custom data transformer before saving */
  transformData?: (data: Record<string, any>) => Record<string, any>;
}

/**
 * Overall form definition with fields and behavior
 */
export interface RelationalFormDefinition {
  /** Form title */
  title: string;
  /** Form description */
  description?: string;
  /** Fields in the form */
  fields: RelationalField[];
  /** Form-level validation */
  formValidators?: ((values: Record<string, any>, graph: ReactiveGraph) => { valid: boolean, message?: string })[];
  /** Submission behavior */
  submitOptions: FormSubmitOptions;
  /** Get default values dynamically */
  getDefaultValues?: (graph: ReactiveGraph, contextPath?: string[]) => Promise<Record<string, any>>;
}

/**
 * Result of field validation
 */
export interface ValidationResult {
  valid: boolean;
  message?: string;
  level: 'error' | 'warning' | 'info';
}

/**
 * State of a form field
 */
export interface FieldState<T = any> {
  value: T;
  dirty: boolean;
  touched: boolean;
  validation: ValidationResult[];
  suggestions: T[];
  loading: boolean;
  visible: boolean;
  enabled: boolean;
}

/**
 * Internal state of the form
 */
interface FormState {
  values: Record<string, any>;
  fields: Record<string, FieldState>;
  formValid: boolean;
  submitting: boolean;
  submitted: boolean;
  error?: string;
  suggestionsVisible: Record<string, boolean>;
  contextPath?: string[];
}

/**
 * Main RelationalForm class providing the form API
 */
export class RelationalForm {
  private reactiveComponent: ReactiveComponentApi;
  private definition: RelationalFormDefinition;
  private state: Writable<FormState>;
  private suggestionsCache: Map<string, any[]> = new Map();
  private debouncers: Map<string, NodeJS.Timeout> = new Map();
  
  /**
   * Create a new RelationalForm
   */
  constructor(graph: ReactiveGraph, definition: RelationalFormDefinition) {
    this.definition = definition;
    
    // Initialize ReactiveComponent
    this.reactiveComponent = new ReactiveComponent(graph, {
      defaultPath: [],
      autoActivate: true,
      relationships: this.buildRelationships(),
      defaultTraversalDepth: 2,
      debounceMs: 250
    });
    
    // Create form state
    this.state = writable({
      values: {},
      fields: {},
      formValid: false,
      submitting: false,
      submitted: false,
      suggestionsVisible: {},
      contextPath: []
    });
    
    // Initialize field states
    const initialFields: Record<string, FieldState> = {};
    
    for (const field of this.definition.fields) {
      initialFields[field.name] = {
        value: field.defaultValue !== undefined ? field.defaultValue : null,
        dirty: false,
        touched: false,
        validation: [],
        suggestions: [],
        loading: false,
        visible: true,
        enabled: !field.disabled
      };
    }
    
    // Update state with initial fields
    this.state.update((state) => ({
      ...state,
      fields: initialFields
    }));
    
    // Subscribe to validation and dynamic field state updates
    this.reactiveComponent.context.subscribe(() => {
      // Only run validation and dynamic updates when fully initialized
      const context = get(this.reactiveComponent.context);
      if (context.initialized) {
        this.validateForm();
        this.updateDynamicFieldState();
      }
    });
  }
  
  /**
   * Build relationships from form definition
   */
  private buildRelationships(): ComponentRelationship[] {
    const relationships: ComponentRelationship[] = [];
    
    // Add relationship for each relational field
    for (const field of this.definition.fields) {
      if (field.isRelationship && field.relatedPath) {
        relationships.push({
          type: this.mapRelationshipType(field.relationshipType),
          relativePath: field.relatedPath,
          reactive: true,
          reactiveDepth: 1,
          autoTraverse: true,
          metadata: {
            fieldName: field.name,
            ...field.metadata
          }
        });
      }
    }
    
    // Add target path as a relationship
    if (this.definition.submitOptions.targetPath) {
      relationships.push({
        type: ComponentRelationshipType.PARENT,
        relativePath: this.definition.submitOptions.targetPath,
        reactive: true,
        autoTraverse: true,
        metadata: {
          isTargetPath: true
        }
      });
    }
    
    return relationships;
  }
  
  /**
   * Map RelationshipType to ComponentRelationshipType
   */
  private mapRelationshipType(type?: RelationshipType): ComponentRelationshipType {
    if (!type) return ComponentRelationshipType.REFERENCE;
    
    switch (type) {
      case RelationshipType.PARENT_CHILD:
        return ComponentRelationshipType.PARENT;
      case RelationshipType.BIDIRECTIONAL:
        return ComponentRelationshipType.SIBLING;
      case RelationshipType.COLLECTION:
        return ComponentRelationshipType.CHILD;
      case RelationshipType.TAGGING:
        return ComponentRelationshipType.CONTRIBUTOR;
      case RelationshipType.INHERITANCE:
        return ComponentRelationshipType.EXTENSION;
      case RelationshipType.CUSTOM:
        return ComponentRelationshipType.CUSTOM;
      default:
        return ComponentRelationshipType.REFERENCE;
    }
  }
  
  /**
   * Set the context path for this form
   * @param path Context path array
   */
  public setContextPath(path: string[]): this {
    // Initialize the ReactiveComponent with the new path
    this.reactiveComponent.initialize(path).then(() => {
      // Update the form's context path
      this.state.update(state => ({
        ...state,
        contextPath: path
      }));
      
      // Load default values that might depend on context
      this.loadDefaults();
    }).catch(error => {
      console.error('Error setting context path:', error);
    });
    
    return this;
  }
  
  /**
   * Load default values for fields
   */
  public async loadDefaults(): Promise<void> {
    try {
      // Get current state
      const state = get(this.state);
      const context = get(this.reactiveComponent.context);
      
      // Get custom default values if provided
      let defaultValues: Record<string, any> = {};
      
      if (this.definition.getDefaultValues) {
        defaultValues = await this.definition.getDefaultValues(
          this.reactiveComponent.getGraph(), 
          state.contextPath
        );
      }
      
      // Update field values with defaults, giving priority to custom defaults
      const updatedFields = { ...state.fields };
      
      for (const field of this.definition.fields) {
        // Skip fields that are already dirty (user modified)
        if (state.fields[field.name]?.dirty) continue;
        
        // Use custom default if available, otherwise use field's default
        const defaultValue = field.name in defaultValues
          ? defaultValues[field.name]
          : field.defaultValue;
          
        if (defaultValue !== undefined) {
          updatedFields[field.name] = {
            ...updatedFields[field.name],
            value: defaultValue
          };
        }
        
        // For relationship fields, try to get values from the relationship data
        if (field.isRelationship && field.relatedPath && context.relationships) {
          // Find relationship data based on field metadata
          const relationshipData = Object.values(context.relationships).find(rel => 
            rel && rel.metadata && rel.metadata.fieldName === field.name
          );
          
          // Use relationship value if available and field isn't already set
          if (relationshipData && relationshipData.data && defaultValue === undefined) {
            updatedFields[field.name] = {
              ...updatedFields[field.name],
              value: this.extractRelationshipValue(relationshipData.data, field)
            };
          }
        }
      }
      
      // Update state with default values
      this.state.update(state => ({
        ...state,
        fields: updatedFields,
        values: Object.entries(updatedFields).reduce((values, [name, field]) => {
          values[name] = field.value;
          return values;
        }, {} as Record<string, any>)
      }));
      
      // Validate fields with new values
      await this.validateForm();
    } catch (error) {
      console.error('Error loading defaults:', error);
    }
  }
  
  /**
   * Extract a value from relationship data based on field configuration
   */
  private extractRelationshipValue(data: any, field: RelationalField): any {
    if (!data) return undefined;
    
    // For node references, extract the reference ID or display property
    if (field.type === 'node-reference' && field.displayProperty) {
      return data[field.displayProperty];
    }
    
    // For collections, return array of items
    if (field.type === 'collection') {
      return Array.isArray(data) ? data : Object.values(data || {});
    }
    
    return data;
  }
  
  /**
   * Get the form state as a readable store
   */
  public getState(): Readable<FormState> {
    return { subscribe: this.state.subscribe };
  }
  
  /**
   * Get a store for a specific field's value
   */
  public getFieldValueStore<T>(fieldName: string): Readable<T> {
    return derived(this.state, $state => {
      const field = $state.fields[fieldName];
      return field ? field.value as T : undefined as any;
    });
  }
  
  /**
   * Get a store for a specific field's state
   */
  public getFieldStateStore(fieldName: string): Readable<FieldState> {
    return derived(this.state, $state => {
      return $state.fields[fieldName] || {
        value: undefined,
        dirty: false,
        touched: false,
        validation: [],
        suggestions: [],
        loading: false,
        visible: false,
        enabled: false
      };
    });
  }
  
  /**
   * Set a field's value
   */
  public setFieldValue<T>(fieldName: string, value: T): this {
    this.state.update(state => {
      if (fieldName in state.fields) {
        // Apply transform if specified
        const field = this.definition.fields.find(f => f.name === fieldName);
        const transformedValue = field?.transform ? 
          field.transform(value, state.values) : value;
        
        state.values[fieldName] = transformedValue;
        state.fields[fieldName].value = transformedValue;
        state.fields[fieldName].dirty = true;
        
        // Generate suggestions if this is a relationship field with getSuggestions
        this.generateSuggestionsForField(fieldName, transformedValue);
      }
      return state;
    });
    
    // Update field validation
    this.validateField(fieldName);
    
    // Update fields that depend on this value
    this.updateDynamicFieldState();
    
    return this;
  }
  
  /**
   * Mark a field as touched (user has interacted with it)
   */
  public touchField(fieldName: string): this {
    this.state.update(state => {
      if (fieldName in state.fields) {
        state.fields[fieldName].touched = true;
      }
      return state;
    });
    
    return this;
  }
  
  /**
   * Set visibility of suggestions for a field
   */
  public setSuggestionsVisible(fieldName: string, visible: boolean): this {
    this.state.update(state => {
      state.suggestionsVisible[fieldName] = visible;
      return state;
    });
    
    return this;
  }
  
  /**
   * Select a suggestion for a field
   */
  public selectSuggestion(fieldName: string, suggestion: any): this {
    this.setFieldValue(fieldName, suggestion);
    this.setSuggestionsVisible(fieldName, false);
    return this;
  }
  
  /**
   * Reset the form to default values
   */
  public reset(): this {
    const initialValues: Record<string, any> = {};
    
    // Get default values for each field
    for (const field of this.definition.fields) {
      initialValues[field.name] = field.defaultValue;
    }
    
    this.state.update(state => {
      state.values = {...initialValues};
      
      for (const fieldName in state.fields) {
        state.fields[fieldName].value = initialValues[fieldName];
        state.fields[fieldName].dirty = false;
        state.fields[fieldName].touched = false;
        state.fields[fieldName].validation = [];
      }
      
      state.submitted = false;
      state.submitting = false;
      state.error = undefined;
      
      return state;
    });
    
    // Update dynamic field state after reset
    this.updateDynamicFieldState();
    
    return this;
  }
  
  /**
   * Validate a specific field
   */
  public validateField(fieldName: string): Promise<void> {
    return this._validateField(fieldName);
  }

  /**
   * Internal implementation of field validation
   */
  private async _validateField(fieldName: string): Promise<void> {
    try {
      const state = get(this.state);
      const field = this.definition.fields.find(f => f.name === fieldName);
      
      if (!field) return;
      
      const value = state.values[fieldName];
      const results: ValidationResult[] = [];
      
      // Required field validation
      if (field.required && (value === undefined || value === null || value === '')) {
        results.push({
          valid: false,
          message: `${field.label} is required`,
          level: 'error'
        });
      }
      
      // Run custom validators if present
      if (field.validators && field.validators.length > 0) {
        for (const validator of field.validators) {
          try {
            const valid = await validator.validate(
              value, 
              state.values, 
              this.reactiveComponent.getGraph()
            );
            
            if (!valid) {
              const message = typeof validator.message === 'function'
                ? validator.message(value, state.values)
                : validator.message;
                
              results.push({
                valid: false,
                message,
                level: validator.level || 'error'
              });
            }
          } catch (error) {
            console.error(`Error in validator for field ${fieldName}:`, error);
            results.push({
              valid: false,
              message: 'Validation error: ' + (error instanceof Error ? error.message : String(error)),
              level: 'error'
            });
          }
        }
      }
      
      // Validate relationships if applicable
      if (field.isRelationship && field.relatedPath) {
        const valid = await this.validateRelationship(field, value);
        if (!valid.valid) {
          results.push(valid);
        }
      }
      
      // If no validation errors, add successful validation
      if (results.length === 0) {
        results.push({
          valid: true,
          level: 'info'
        });
      }
      
      // Update field state with validation results
      this.state.update(state => {
        const updatedFields = { ...state.fields };
        updatedFields[fieldName] = {
          ...updatedFields[fieldName],
          validation: results
        };
        
        return {
          ...state,
          fields: updatedFields
        };
      });
      
      // Avoid recursion by not calling validateForm here - just update formValid directly
      this.state.update(state => {
        // Check if any visible fields have validation errors
        const hasErrors = Object.entries(state.fields)
          .filter(([_, fieldState]) => fieldState.visible)
          .some(([_, fieldState]) => 
            fieldState.validation.some(v => !v.valid && v.level === 'error')
          );
          
        return {
          ...state,
          formValid: !hasErrors
        };
      });
    } catch (error) {
      console.error(`Error validating field ${fieldName}:`, error);
    }
  }

  /**
   * Validate a relationship field
   */
  private async validateRelationship(field: RelationalField, value: any): Promise<ValidationResult> {
    // Skip validation for empty values (required validation is handled separately)
    if (value === undefined || value === null || value === '') {
      return { valid: true, level: 'info' };
    }
    
    // Skip validation if verification isn't required
    if (!field.verifyReference) {
      return { valid: true, level: 'info' };
    }
    
    try {
      // Use the ReactiveComponent to check if the related path exists
      const relativePath = [...field.relatedPath!];
      
      // For node references, append the value as the final path segment
      if (field.type === 'node-reference' && typeof value === 'string') {
        relativePath.push(value);
      }
      
      // Try to traverse to the path
      const resolvedPath = await this.reactiveComponent.traverseTo(relativePath);
      
      // If we made it here, the path was traversable
      return { valid: true, level: 'info' };
    } catch (error) {
      return {
        valid: false,
        message: `Referenced item does not exist: ${value}`,
        level: 'error'
      };
    }
  }

  /**
   * Validate the entire form
   */
  private async validateForm(): Promise<boolean> {
    try {
      const state = get(this.state);
      
      // Validate each field individually
      const fieldPromises = this.definition.fields
        .filter(field => {
          // Skip fields that aren't visible
          const fieldState = state.fields[field.name];
          return fieldState && fieldState.visible;
        })
        .map(field => this._validateField(field.name));
        
      await Promise.all(fieldPromises);
      
      // Get updated state after field validations
      const updatedState = get(this.state);
      
      // Check if any visible fields have validation errors
      const hasErrors = Object.entries(updatedState.fields)
        .filter(([_, fieldState]) => fieldState.visible)
        .some(([_, fieldState]) => 
          fieldState.validation.some(v => !v.valid && v.level === 'error')
        );
      
      // Run form-level validators
      let formError: string | undefined;
      
      if (!hasErrors && this.definition.formValidators) {
        for (const validator of this.definition.formValidators) {
          const result = await validator(
            updatedState.values, 
            this.reactiveComponent.getGraph()
          );
          
          if (!result.valid) {
            formError = result.message;
            break;
          }
        }
      }
      
      // Update form validation state
      this.state.update(state => ({
        ...state,
        formValid: !hasErrors && !formError,
        error: formError
      }));
      
      return !hasErrors && !formError;
    } catch (error) {
      console.error('Error validating form:', error);
      
      this.state.update(state => ({
        ...state,
        formValid: false,
        error: 'Error validating form: ' + (error instanceof Error ? error.message : String(error))
      }));
      
      return false;
    }
  }
  
  /**
   * Update dynamic field visibility and enabled state
   */
  private updateDynamicFieldState(): void {
    this.state.update(state => {
      // Get the graph instance from the component
      const graph = this.reactiveComponent.getGraph();
      
      // Update visibility and enabled state based on conditions
      for (const field of this.definition.fields) {
        if (field.visibleWhen) {
          state.fields[field.name].visible = field.visibleWhen(state.values, graph);
        }
        
        if (field.enabledWhen) {
          state.fields[field.name].enabled = field.enabledWhen(state.values, graph);
        }
      }
      return state;
    });
  }
  
  /**
   * Generate suggestions for a field
   */
  private generateSuggestionsForField(fieldName: string, value: any): void {
    const field = this.definition.fields.find(f => f.name === fieldName);
    if (!field || !field.getSuggestions) return;
    
    // Get current state
    const state = get(this.state);
    
    // Clear any existing debouncer for this field
    if (this.debouncers.has(fieldName)) {
      clearTimeout(this.debouncers.get(fieldName)!);
    }
    
    // Set loading state
    this.state.update(state => {
      state.fields[fieldName].loading = true;
      return state;
    });
    
    // Debounce suggestion generation
    this.debouncers.set(fieldName, setTimeout(async () => {
      try {
        // Get the graph instance from the component
        const graph = this.reactiveComponent.getGraph();
        
        // Generate suggestions
        const suggestions = await field.getSuggestions!(
          value,
          state.values,
          graph
        );
        
        // Cache suggestions
        this.suggestionsCache.set(fieldName, suggestions);
        
        // Update state with suggestions
        this.state.update(state => {
          state.fields[fieldName].suggestions = suggestions;
          state.fields[fieldName].loading = false;
          return state;
        });
      } catch (error) {
        console.error(`Error generating suggestions for ${fieldName}:`, error);
        
        // Clear loading state on error
        this.state.update(state => {
          state.fields[fieldName].loading = false;
          return state;
        });
      }
    }, 300));
  }
  
  /**
   * Submit the form data
   */
  public async submit(): Promise<boolean> {
    try {
      // Update state to indicate submission in progress
      this.state.update(state => ({
        ...state,
        submitting: true,
        error: undefined
      }));
      
      // Validate the form before submission
      const isValid = await this.validateForm();
      
      if (!isValid) {
        this.state.update(state => ({
          ...state,
          submitting: false,
          error: 'Please fix the validation errors before submitting.'
        }));
        return false;
      }
      
      // Get current state and options
      const state = get(this.state);
      const { targetPath, collectionKey, generateId, relationshipBehavior, transformData, clearAfterSubmit } = this.definition.submitOptions;
      
      // Process form values
      let formData = { ...state.values };
      
      // Apply custom data transformation if provided
      if (transformData) {
        formData = transformData(formData);
      }
      
      // Process relationship fields
      for (const field of this.definition.fields) {
        if (field.isRelationship && formData[field.name] !== undefined) {
          formData[field.name] = await this.processRelationshipField(
            field, 
            formData[field.name], 
            relationshipBehavior
          );
        }
      }
      
      // Use ReactiveComponent to submit the data
      try {
        if (collectionKey) {
          // Adding to a collection
          const targetCollectionPath = [...targetPath, collectionKey];
          const itemId = generateId ? Date.now().toString() : undefined;
          
          // Use the component to add to the collection
          if (itemId) {
            // Add with specific ID
            await this.reactiveComponent.traverseTo(targetCollectionPath);
            await this.reactiveComponent.updateData(itemId, formData);
          } else {
            // Add with auto-generated ID using the graph directly
            const graph = this.reactiveComponent.getGraph();
            const itemId = await graph.addNode(
              targetPath,
              collectionKey,
              formData
            );
          }
        } else {
          // Updating a node directly
          await this.reactiveComponent.traverseTo(targetPath);
          
          // Update each property individually to avoid overwriting existing data
          for (const [key, value] of Object.entries(formData)) {
            if (value !== undefined) {
              await this.reactiveComponent.updateData(key, value);
            }
          }
        }
        
        // Update state to indicate successful submission
        this.state.update(state => ({
          ...state,
          submitting: false,
          submitted: true
        }));
        
        // Reset form if requested
        if (clearAfterSubmit) {
          this.reset();
        }
        
        return true;
      } catch (error) {
        console.error('Error saving form data:', error);
        
        this.state.update(state => ({
          ...state,
          submitting: false,
          error: 'Error saving data: ' + (error instanceof Error ? error.message : String(error))
        }));
        
        return false;
      }
    } catch (error) {
      console.error('Error submitting form:', error);
      
      this.state.update(state => ({
        ...state,
        submitting: false,
        error: 'Error submitting form: ' + (error instanceof Error ? error.message : String(error))
      }));
      
      return false;
    }
  }

  /**
   * Process a relationship field based on the relationship behavior
   */
  private async processRelationshipField(
    field: RelationalField,
    value: any,
    behavior: string
  ): Promise<any> {
    if (!field.relatedPath || value === undefined || value === null) {
      return value;
    }
    
    switch (behavior) {
      case 'reference':
        // Just return the reference value
        return value;
        
      case 'create':
        // Create a new node at the related path with this value
        if (typeof value === 'object') {
          // For complex objects, create a new node
          const newPath = [...field.relatedPath];
          if (typeof value._key === 'string') {
            newPath.push(value._key);
          } else {
            // Generate an ID if not provided
            newPath.push(Date.now().toString());
          }
          
          // Create the node
          await this.reactiveComponent.traverseTo(newPath);
          await this.reactiveComponent.updateData('', value);
          
          // Return the ID to reference
          return newPath[newPath.length - 1];
        }
        return value;
        
      case 'update':
        // Update an existing node at the related path
        if (typeof value === 'object') {
          const newPath = [...field.relatedPath];
          if (typeof value._key === 'string') {
            newPath.push(value._key);
          } else if (typeof value === 'string') {
            newPath.push(value);
          }
          
          // Update the node
          if (newPath.length > field.relatedPath.length) {
            await this.reactiveComponent.traverseTo(newPath);
            
            // Update each property individually
            for (const [key, propValue] of Object.entries(value)) {
              if (key !== '_key' && propValue !== undefined) {
                await this.reactiveComponent.updateData(key, propValue);
              }
            }
          }
        }
        return value;
        
      case 'custom':
        // For custom behavior, return the raw value and let transformData handle it
        return value;
        
      default:
        return value;
    }
  }
  
  /**
   * Clean up resources when form is no longer needed
   */
  public destroy(): void {
    // Clean up ReactiveComponent
    this.reactiveComponent.destroy();
    
    // Clear any remaining timeouts
    for (const timeout of this.debouncers.values()) {
      clearTimeout(timeout);
    }
    this.debouncers.clear();
    
    // Clear caches
    this.suggestionsCache.clear();
  }
  
  /**
   * Create a validator that checks if a value is unique in a collection
   */
  public static createUniqueValidator(
    relatedPath: string[],
    collectionKey: string,
    property: string = 'id'
  ): ValidationRule {
    return {
      validate: async (value: any, formValues: Record<string, any>, graph: ReactiveGraph) => {
        if (!value) return true; // Empty values are considered unique
        
        try {
          // Use the graph to check if the value exists
          const items = await graph.getFilteredNodes(
            relatedPath,
            collectionKey,
            (node: any) => node[property] === value
          );
          
          return items.length === 0;
        } catch (error) {
          console.error('Error in unique validator:', error);
          return false;
        }
      },
      message: (value: any) => `Value "${value}" is already in use.`,
      level: 'error'
    };
  }

  /**
   * Create a validator that checks if a referenced item exists
   */
  public static createExistsValidator(
    relatedPath: string[],
    errorMessage: string = 'Referenced item does not exist'
  ): ValidationRule {
    return {
      validate: async (value: any, formValues: Record<string, any>, graph: ReactiveGraph) => {
        if (!value) return true; // Empty values are fine
        
        try {
          // Create a path to the referenced item
          const itemPath = [...relatedPath];
          
          if (typeof value === 'string') {
            itemPath.push(value);
          } else if (typeof value === 'object' && value._key) {
            itemPath.push(value._key);
          }
          
          // Try to get the item from the graph
          const nodeStore = graph.getNodeStore(itemPath);
          const data = safeGet(nodeStore, null);
          
          return !!data;
        } catch (error) {
          console.error('Error in exists validator:', error);
          return false;
        }
      },
      message: errorMessage,
      level: 'error'
    };
  }

  /**
   * Create a function that provides suggestions from a collection
   */
  public static createCollectionSuggestions<T extends CollectionItem>(
    collectionPath: string[],
    searchProperty: keyof T = 'name' as keyof T,
    displayProperty: keyof T = searchProperty,
    maxResults: number = 10
  ): RelationalField['getSuggestions'] {
    return async (input: string, formValues: Record<string, any>, graph: ReactiveGraph) => {
      if (!input || typeof input !== 'string' || input.length < 2) {
        return [];
      }
      
      try {
        // Get the collection store
        const collectionStore = graph.getCollectionStore<T>(collectionPath);
        const items = safeGet(collectionStore, []);
        
        // Filter and sort items based on the search term
        const normalizedInput = input.toLowerCase();
        const filtered = items
          // Use different parameter names to avoid duplicate identifier errors
          .filter(([itemId, itemData]) => {
            const value = itemData[searchProperty];
            if (typeof value === 'string') {
              return value.toLowerCase().includes(normalizedInput);
            }
            return false;
          })
          // Use different parameter names to avoid duplicate identifier errors
          .sort(([idA, itemA], [idB, itemB]) => {
            const aValue = String(itemA[searchProperty]);
            const bValue = String(itemB[searchProperty]);
            // Sort exact matches first, then by startsWith, then alphabetically
            if (aValue.toLowerCase() === normalizedInput) return -1;
            if (bValue.toLowerCase() === normalizedInput) return 1;
            if (aValue.toLowerCase().startsWith(normalizedInput) && !bValue.toLowerCase().startsWith(normalizedInput)) return -1;
            if (bValue.toLowerCase().startsWith(normalizedInput) && !aValue.toLowerCase().startsWith(normalizedInput)) return 1;
            return aValue.localeCompare(bValue);
          })
          .map(([id, item]) => ({
            _key: id,
            [displayProperty]: item[displayProperty],
            ...item
          }))
          .slice(0, maxResults);
          
        return filtered;
      } catch (error) {
        console.error('Error getting suggestions:', error);
        return [];
      }
    };
  }
  
  /**
   * Factory method to create a new RelationalForm
   */
  public static create(
    graph: ReactiveGraph,
    definition: RelationalFormDefinition
  ): RelationalForm {
    return new RelationalForm(graph, definition);
  }
}
