// Core vector operations and types
type ResourceId = string;
type ActivityId = string;

/**
 * Represents the three states of activity realization:
 * - PotentialPotential: What could theoretically happen given available activities
 * - ActualPotential: What is planned/committed to happen
 * - Actual: What has actually happened
 */
enum ActivityState {
  PotentialPotential = 'potential-potential',
  ActualPotential = 'actual-potential', 
  Actual = 'actual'
}

/**
 * Resource consumption tracking across different states
 */
interface ResourceConsumption {
  /** Total theoretical capacity available */
  totalCapacity: number;
  
  /** Amount reserved by actual-potential activities (committed but not yet executed) */
  reservedByPlanned: number;
  
  /** Amount actually consumed by completed activities */
  actuallyConsumed: number;
  
  /** Amount available for new potential-potential activities */
  get availableForPlanning(): number;
  
  /** Amount that will be available after planned activities complete */
  get projectedAvailable(): number;
}

class ResourceConsumptionTracker implements ResourceConsumption {
  constructor(
    public totalCapacity: number,
    public reservedByPlanned: number = 0,
    public actuallyConsumed: number = 0
  ) {}
  
  get availableForPlanning(): number {
    return this.totalCapacity - this.reservedByPlanned - this.actuallyConsumed;
  }
  
  get projectedAvailable(): number {
    return this.totalCapacity - this.actuallyConsumed;
  }
  
  /** Reserve resources for a planned activity */
  reserve(amount: number): ResourceConsumptionTracker {
    if (amount > this.availableForPlanning) {
      throw new Error(`Cannot reserve ${amount}, only ${this.availableForPlanning} available`);
    }
    return new ResourceConsumptionTracker(
      this.totalCapacity,
      this.reservedByPlanned + amount,
      this.actuallyConsumed
    );
  }
  
  /** Convert reserved resources to actually consumed (execute planned activity) */
  executeReserved(amount: number): ResourceConsumptionTracker {
    if (amount > this.reservedByPlanned) {
      throw new Error(`Cannot execute ${amount}, only ${this.reservedByPlanned} reserved`);
    }
    return new ResourceConsumptionTracker(
      this.totalCapacity,
      this.reservedByPlanned - amount,
      this.actuallyConsumed + amount
    );
  }
  
  /** Cancel reserved resources (unreserve) */
  cancelReserved(amount: number): ResourceConsumptionTracker {
    if (amount > this.reservedByPlanned) {
      throw new Error(`Cannot cancel ${amount}, only ${this.reservedByPlanned} reserved`);
    }
    return new ResourceConsumptionTracker(
      this.totalCapacity,
      this.reservedByPlanned - amount,
      this.actuallyConsumed
    );
  }
}

/**
 * Activity Vector with state awareness
 */
class ActivityVector {
  private coefficients = new Map<ResourceId, number>();
  
  constructor(
    public readonly id: ActivityId,
    public readonly state: ActivityState,
    coefficients: Record<ResourceId, number> = {},
    public readonly timestamp: Date = new Date()
  ) {
    Object.entries(coefficients).forEach(([resource, value]) => {
      if (value !== 0) this.coefficients.set(resource, value);
    });
  }
  
  get(resource: ResourceId): number {
    return this.coefficients.get(resource) ?? 0;
  }
  
  /** Scale activity by intensity, preserving state */
  scale(intensity: number): ActivityVector {
    if (intensity < 0) throw new Error('Activity intensity must be non-negative');
    
    const scaled = new Map<ResourceId, number>();
    this.coefficients.forEach((value, resource) => {
      scaled.set(resource, value * intensity);
    });
    
    return new ActivityVector(
      `${this.id}*${intensity}`, 
      this.state,
      Object.fromEntries(scaled),
      this.timestamp
    );
  }
  
  /** Transition activity to next state */
  transitionTo(newState: ActivityState): ActivityVector {
    const validTransitions = {
      [ActivityState.PotentialPotential]: [ActivityState.ActualPotential],
      [ActivityState.ActualPotential]: [ActivityState.Actual],
      [ActivityState.Actual]: [] // Terminal state
    };
    
    if (!validTransitions[this.state].includes(newState)) {
      throw new Error(`Invalid transition from ${this.state} to ${newState}`);
    }
    
    return new ActivityVector(this.id, newState, this.toObject(), new Date());
  }
  
  get resources(): ResourceId[] {
    return Array.from(this.coefficients.keys());
  }
  
  get inputs(): [ResourceId, number][] {
    return Array.from(this.coefficients.entries())
      .filter(([_, value]) => value < 0)
      .map(([resource, value]) => [resource, -value]);
  }
  
  get outputs(): [ResourceId, number][] {
    return Array.from(this.coefficients.entries()).filter(([_, value]) => value > 0);
  }
  
  toObject(): Record<ResourceId, number> {
    return Object.fromEntries(this.coefficients);
  }
}

/**
 * State-aware Activity Combination
 */
class ActivityCombination {
  constructor(
    private activities: Map<ActivityId, ActivityVector>,
    private intensities: Map<ActivityId, number>,
    public readonly state: ActivityState
  ) {
    // Validate that all activities have compatible states
    const states = new Set(Array.from(activities.values()).map(a => a.state));
    if (states.size > 1) {
      throw new Error('All activities in combination must have the same state');
    }
    
    intensities.forEach((intensity, id) => {
      if (intensity < 0) throw new Error(`Negative intensity for activity ${id}`);
    });
  }
  
  static from(
    activityIntensities: Array<[ActivityVector, number]>,
    combinationState?: ActivityState
  ): ActivityCombination {
    const activities = new Map<ActivityId, ActivityVector>();
    const intensities = new Map<ActivityId, number>();
    
    activityIntensities.forEach(([activity, intensity]) => {
      activities.set(activity.id, activity);
      intensities.set(activity.id, intensity);
    });
    
    // Infer state from activities if not provided
    const inferredState = combinationState ?? 
      (activities.size > 0 ? Array.from(activities.values())[0].state : ActivityState.PotentialPotential);
    
    return new ActivityCombination(activities, intensities, inferredState);
  }
  
  /** Compute net resource vector for this combination */
  computeNetVector(): Map<ResourceId, number> {
    const netVector = new Map<ResourceId, number>();
    
    this.intensities.forEach((intensity, activityId) => {
      const activity = this.activities.get(activityId)!;
      
      activity.resources.forEach(resource => {
        const coefficient = activity.get(resource);
        const contribution = coefficient * intensity;
        const current = netVector.get(resource) ?? 0;
        netVector.set(resource, current + contribution);
      });
    });
    
    return netVector;
  }
  
  /** Check feasibility against resource consumption state */
  isFeasible(resourceState: Map<ResourceId, ResourceConsumptionTracker>): boolean {
    const netVector = this.computeNetVector();
    
    for (const [resource, netAmount] of netVector) {
      if (netAmount < 0) { // Net consumption
        const consumption = resourceState.get(resource);
        if (!consumption) return false;
        
        const requiredAmount = -netAmount;
        
        // Check availability based on combination state
        switch (this.state) {
          case ActivityState.PotentialPotential:
            if (requiredAmount > consumption.availableForPlanning) return false;
            break;
          case ActivityState.ActualPotential:
            if (requiredAmount > consumption.availableForPlanning) return false;
            break;
          case ActivityState.Actual:
            // For actual activities, we check if resources were properly reserved
            if (requiredAmount > consumption.reservedByPlanned + consumption.availableForPlanning) return false;
            break;
        }
      }
    }
    
    return true;
  }
  
  /** Transition combination to next state */
  transitionTo(newState: ActivityState): ActivityCombination {
    const transitionedActivities = new Map<ActivityId, ActivityVector>();
    
    this.activities.forEach((activity, id) => {
      transitionedActivities.set(id, activity.transitionTo(newState));
    });
    
    return new ActivityCombination(transitionedActivities, this.intensities, newState);
  }
  
  /** Get resource impact by type */
  getResourceImpact(): {
    consumption: Map<ResourceId, number>;
    production: Map<ResourceId, number>;
    net: Map<ResourceId, number>;
  } {
    const consumption = new Map<ResourceId, number>();
    const production = new Map<ResourceId, number>();
    const net = this.computeNetVector();
    
    net.forEach((netAmount, resource) => {
      if (netAmount < 0) {
        consumption.set(resource, -netAmount);
      } else if (netAmount > 0) {
        production.set(resource, netAmount);
      }
    });
    
    return { consumption, production, net };
  }
}

/**
 * Multi-State Production System
 */
class ProductionSystem {
  private activities = new Map<ActivityId, ActivityVector>();
  private resourceConsumption = new Map<ResourceId, ResourceConsumptionTracker>();
  
  /** Add activity in any state */
  addActivity(activity: ActivityVector): void {
    this.activities.set(activity.id, activity);
  }
  
  /** Set initial resource capacity */
  setResourceCapacity(resource: ResourceId, capacity: number): void {
    this.resourceConsumption.set(resource, new ResourceConsumptionTracker(capacity));
  }
  
  /** Get activities by state */
  getActivitiesByState(state: ActivityState): ActivityVector[] {
    return Array.from(this.activities.values()).filter(a => a.state === state);
  }
  
  /** Get all potential-potential combinations (what could happen) */
  *getPotentialPotentialCombinations(maxIntensity = 5): Generator<ActivityCombination> {
    const potentialActivities = this.getActivitiesByState(ActivityState.PotentialPotential);
    
    for (const combination of this.enumerateCombinations(potentialActivities, maxIntensity)) {
      if (combination.isFeasible(this.resourceConsumption)) {
        yield combination;
      }
    }
  }
  
  /** Plan a combination (transition from potential-potential to actual-potential) */
  planCombination(combination: ActivityCombination): {
    success: boolean;
    updatedSystem?: ProductionSystem;
    error?: string;
  } {
    if (combination.state !== ActivityState.PotentialPotential) {
      return { success: false, error: 'Can only plan potential-potential combinations' };
    }
    
    if (!combination.isFeasible(this.resourceConsumption)) {
      return { success: false, error: 'Combination not feasible with current resources' };
    }
    
    // Create new system with reserved resources
    const newSystem = new ProductionSystem();
    
    // Copy existing state
    this.activities.forEach(activity => newSystem.addActivity(activity));
    this.resourceConsumption.forEach((tracker, resource) => {
      newSystem.resourceConsumption.set(resource, tracker);
    });
    
    // Reserve resources for the planned combination
    const impact = combination.getResourceImpact();
    try {
      impact.consumption.forEach((amount, resource) => {
        const currentTracker = newSystem.resourceConsumption.get(resource)!;
        newSystem.resourceConsumption.set(resource, currentTracker.reserve(amount));
      });
      
      // Add planned activities
      const plannedCombination = combination.transitionTo(ActivityState.ActualPotential);
      plannedCombination['activities'].forEach(activity => {
        newSystem.addActivity(activity);
      });
      
      return { success: true, updatedSystem: newSystem };
    } catch (error) {
      return { success: false, error: (error as Error).message };
    }
  }
  
  /** Execute a planned combination (transition from actual-potential to actual) */
  executeCombination(combination: ActivityCombination): {
    success: boolean;
    updatedSystem?: ProductionSystem;
    error?: string;
  } {
    if (combination.state !== ActivityState.ActualPotential) {
      return { success: false, error: 'Can only execute actual-potential combinations' };
    }
    
    const newSystem = new ProductionSystem();
    
    // Copy existing state
    this.activities.forEach(activity => newSystem.addActivity(activity));
    this.resourceConsumption.forEach((tracker, resource) => {
      newSystem.resourceConsumption.set(resource, tracker);
    });
    
    // Execute the combination
    const impact = combination.getResourceImpact();
    try {
      impact.consumption.forEach((amount, resource) => {
        const currentTracker = newSystem.resourceConsumption.get(resource)!;
        newSystem.resourceConsumption.set(resource, currentTracker.executeReserved(amount));
      });
      
      // Add executed activities
      const executedCombination = combination.transitionTo(ActivityState.Actual);
      executedCombination['activities'].forEach(activity => {
        newSystem.addActivity(activity);
      });
      
      return { success: true, updatedSystem: newSystem };
    } catch (error) {
      return { success: false, error: (error as Error).message };
    }
  }
  
  /** Get resource consumption overview */
  getResourceOverview(): Map<ResourceId, ResourceConsumption> {
    return new Map(this.resourceConsumption);
  }
  
  /** Get system state summary */
  getSystemSummary(): {
    potentialActivities: number;
    plannedActivities: number;
    executedActivities: number;
    resourceUtilization: Map<ResourceId, {
      capacity: number;
      reserved: number;
      consumed: number;
      available: number;
      utilization: number;
    }>;
  } {
    const activities = Array.from(this.activities.values());
    
    return {
      potentialActivities: activities.filter(a => a.state === ActivityState.PotentialPotential).length,
      plannedActivities: activities.filter(a => a.state === ActivityState.ActualPotential).length,
      executedActivities: activities.filter(a => a.state === ActivityState.Actual).length,
      resourceUtilization: new Map(
        Array.from(this.resourceConsumption.entries()).map(([resource, tracker]) => [
          resource,
          {
            capacity: tracker.totalCapacity,
            reserved: tracker.reservedByPlanned,
            consumed: tracker.actuallyConsumed,
            available: tracker.availableForPlanning,
            utilization: (tracker.reservedByPlanned + tracker.actuallyConsumed) / tracker.totalCapacity
          }
        ])
      )
    };
  }
  
  private *enumerateCombinations(
    activities: ActivityVector[], 
    maxIntensity: number
  ): Generator<ActivityCombination> {
    if (activities.length === 0) return;
    
    function* generateIntensities(
      activityIndex: number, 
      currentIntensities: number[]
    ): Generator<number[]> {
      if (activityIndex >= activities.length) {
        yield [...currentIntensities];
        return;
      }
      
      for (let intensity = 0; intensity <= maxIntensity; intensity++) {
        currentIntensities[activityIndex] = intensity;
        yield* generateIntensities(activityIndex + 1, currentIntensities);
      }
    }
    
    for (const intensities of generateIntensities(0, new Array(activities.length))) {
      if (intensities.some(i => i > 0)) { // Skip all-zero combination
        const combination = ActivityCombination.from(
          activities.map((activity, i) => [activity, intensities[i]]),
          activities[0].state
        );
        yield combination;
      }
    }
  }
}

// Builder classes with state support
class ActivityBuilder {
  private coefficients: Record<ResourceId, number> = {};
  
  constructor(
    private id: ActivityId, 
    private state: ActivityState = ActivityState.PotentialPotential
  ) {}
  
  input(resource: ResourceId, amount: number): ActivityBuilder {
    this.coefficients[resource] = -Math.abs(amount);
    return this;
  }
  
  output(resource: ResourceId, amount: number): ActivityBuilder {
    this.coefficients[resource] = Math.abs(amount);
    return this;
  }
  
  build(): ActivityVector {
    return new ActivityVector(this.id, this.state, this.coefficients);
  }
}

class SystemBuilder {
  private system = new ProductionSystem();
  
  activity(
    id: ActivityId, 
    build: (builder: ActivityBuilder) => void,
    state: ActivityState = ActivityState.PotentialPotential
  ): SystemBuilder {
    const builder = new ActivityBuilder(id, state);
    build(builder);
    this.system.addActivity(builder.build());
    return this;
  }
  
  resourceCapacity(resource: ResourceId, capacity: number): SystemBuilder {
    this.system.setResourceCapacity(resource, capacity);
    return this;
  }
  
  build(): ProductionSystem {
    return this.system;
  }
}

export {
  ActivityVector,
  ActivityCombination,
  ProductionSystem,
  ActivityBuilder,
  SystemBuilder,
  ResourceConsumptionTracker,
  ActivityState,
  type ResourceId,
  type ActivityId,
  type ResourceConsumption
};

// Example usage showing the three states:
/*
const system = new SystemBuilder()
  .resourceCapacity('developer-hours', 100)
  .resourceCapacity('designer-hours', 50)
  .activity('feature-dev', builder => builder
    .input('developer-hours', 20)
    .output('features', 1)
  )
  .activity('ui-design', builder => builder
    .input('designer-hours', 10)
    .output('designs', 1)
  )
  .build();

// Explore potential-potential space
console.log('Potential combinations:');
for (const combo of system.getPotentialPotentialCombinations()) {
  console.log(combo.getResourceImpact());
}

// Plan a specific combination
const potentialCombos = Array.from(system.getPotentialPotentialCombinations());
const chosenCombo = potentialCombos[0];
const planResult = system.planCombination(chosenCombo);

if (planResult.success) {
  console.log('Planned successfully!');
  console.log(planResult.updatedSystem!.getSystemSummary());
  
  // Later, execute the planned combination
  const plannedCombos = planResult.updatedSystem!.getActivitiesByState(ActivityState.ActualPotential);
  // ... execution logic
}
*/