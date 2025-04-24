// Advanced type system for proportions



// Branded type for proportions to prevent mixing with regular numbers
export type Proportion = number & { readonly __brand: unique symbol };

// Type guard to validate and convert a number to a Proportion
export function asProportion(num: number): Proportion {
  if (num < 0 || num > 1) {
    throw new Error(`Invalid proportion value: ${num}. Must be between 0 and 1.`);
  }
  return num as Proportion;
}

// Type for collections of proportions that must sum to 1.0
export type ProportionDistribution<K extends string | number | symbol> = {
  [P in K]: Proportion;
} & {
  readonly __normalized: true; // Phantom property indicating the distribution sums to 1
};

// Helper to create and validate a proportion distribution
export function createDistribution<K extends string | number | symbol>(
  distribution: Record<K, number>
): ProportionDistribution<K> {
  const values = Object.values(distribution) as number[];
  const sum = values.reduce((a, b) => a + b, 0);
  if (Math.abs(sum - 1) > 0.00001) { // Allow for floating point imprecision
    throw new Error(`Distribution values must sum to 1.0, got ${sum}`);
  }
  
  // Convert all values to proportions
  const result = {} as Record<K, Proportion>;
  for (const [key, value] of Object.entries(distribution) as [K, number][]) {
    result[key] = asProportion(value);
  }
  
  return result as ProportionDistribution<K>;
}

// Type-safe map for storing proportions
export class ProportionMap<K> extends Map<K, Proportion> {
  set(key: K, value: number): this {
    super.set(key, asProportion(value));
    return this;
  }
}

// Type-safe map for storing distributions that must sum to 1.0
export class DistributionMap<K> extends Map<K, Proportion> {
  private _isNormalized = false;
  
  setAll(entries: [K, number][]): this {
    // Clear existing entries
    this.clear();
    
    // Add all new entries
    for (const [key, value] of entries) {
      super.set(key, asProportion(value));
    }
    
    // Validate sum
    this.normalize();
    return this;
  }
  
  // Enforce normalization
  normalize(): this {
    const sum = Array.from(this.values()).reduce((a: number, b: number) => a + b, 0);
    if (Math.abs(sum - 1) > 0.00001) {
      // Auto-normalize
      for (const [key, value] of this.entries()) {
        super.set(key, asProportion(value / sum));
      }
    }
    this._isNormalized = true;
    return this;
  }
  
  // Override set to enforce validation
  set(key: K, value: number): this {
    super.set(key, asProportion(value));
    this._isNormalized = false; // Mark as requiring normalization
    return this;
  }
  
  // Prevent using a distribution that isn't normalized
  get isNormalized(): boolean {
    return this._isNormalized;
  }
  
  // Safe getter that ensures normalization
  getDistribution(): ProportionDistribution<any> {
    if (!this._isNormalized) {
      this.normalize();
    }
    return Object.fromEntries(this) as ProportionDistribution<any>;
  }
}

// Interface to enforce that proportions sum to 1.0 across a collection of entities with the same ID
export interface ProportionalRegistry<T extends { id: string }> {
  // Register a new entity with proportions
  register(entity: T, proportions: Map<string, Proportion>): void;
  
  // Validate that proportions for a given ID sum to 1.0
  validateProportions(id: string): boolean;
  
  // Get all entities with a specific ID
  getEntitiesById(id: string): T[];
}

// Implementation of ProportionalRegistry
export class EntityProportionRegistry<T extends { id: string }> implements ProportionalRegistry<T> {
  private _entities: Map<string, T[]> = new Map(); // ID -> entities with that ID
  private _proportions: Map<string, Map<T, Proportion>> = new Map(); // ID -> (entity -> proportion)
  
  // Register a new entity with its proportions
  register(entity: T, proportions: Map<string, Proportion>): void {
    // Store the entity by its ID
    const entitiesWithId = this._entities.get(entity.id) || [];
    entitiesWithId.push(entity);
    this._entities.set(entity.id, entitiesWithId);
    
    // Store each proportion relationship
    for (const [targetId, proportion] of proportions.entries()) {
      const proportionsForId = this._proportions.get(targetId) || new Map();
      proportionsForId.set(entity, proportion);
      this._proportions.set(targetId, proportionsForId);
      
      // Validate that proportions for this ID now sum to 1.0
      this.normalizeProportionsForId(targetId);
    }
  }
  
  // Normalize proportions for a given ID to ensure they sum to 1.0
  private normalizeProportionsForId(id: string): void {
    const proportionsForId = this._proportions.get(id);
    if (!proportionsForId) return;
    
    const sum = Array.from(proportionsForId.values()).reduce((a: number, b: number) => a + b, 0);
    if (Math.abs(sum - 1) > 0.00001) {
      // Auto-normalize
      for (const [entity, proportion] of proportionsForId.entries()) {
        proportionsForId.set(entity, asProportion(Number(proportion) / sum));
      }
    }
  }
  
  // Validate that proportions for a given ID sum to 1.0
  validateProportions(id: string): boolean {
    const proportionsForId = this._proportions.get(id);
    if (!proportionsForId) return true; // No proportions means nothing to validate
    
    const sum = Array.from(proportionsForId.values()).reduce((a: number, b: number) => a + b, 0);
    return Math.abs(sum - 1) <= 0.00001;
  }
  
  // Get all entities with a specific ID
  getEntitiesById(id: string): T[] {
    return this._entities.get(id) || [];
  }
  
  // Get the proportion of an entity for a specific ID
  getProportion(id: string, entity: T): Proportion | undefined {
    return this._proportions.get(id)?.get(entity);
  }
  
  // Set proportion and normalize
  setProportion(id: string, entity: T, proportion: number): void {
    const proportionsForId = this._proportions.get(id) || new Map();
    proportionsForId.set(entity, asProportion(proportion));
    this._proportions.set(id, proportionsForId);
    this.normalizeProportionsForId(id);
  }
}


// Helper function to validate that proportions sum to 1.0 across properties of objects with the same ID
export function validateProportionsByProperty<T extends { id: string }>(
  entities: T[],
  getProportions: (entity: T) => Map<string, Proportion>
): boolean {
  const registry = new EntityProportionRegistry<T>();
  
  // Register all entities
  for (const entity of entities) {
    registry.register(entity, getProportions(entity));
  }
  
  // Validate proportions for all unique IDs
  const uniqueIds = new Set(entities.map(e => e.id));
  for (const id of uniqueIds) {
    if (!registry.validateProportions(id)) {
      return false;
    }
  }
  
  return true;
}