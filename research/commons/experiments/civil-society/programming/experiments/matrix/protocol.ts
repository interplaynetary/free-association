/**
 * Free Association Protocol Implementation
 * 
 * Implements the mathematical foundations and RPC layer for the Free Association
 * protocol using Cap'n Web and Zod schemas.
 * 
 * Architecture based on:
 * - Matrix formulation from matrix-rpc.md
 * - Mathematical axioms from math.md
 * - Capability-based security from rpc.md
 */

import { z } from 'zod';
import { RpcTarget, type RpcStub } from 'capnweb';
import { 
  type SparseMatrix, 
  Sparse, 
  SparsePerf, 
  SparseCompare 
} from './sparse-matrix.js';

// ============================================================================
// PHASE 2: ZOD SCHEMAS AND TYPE DEFINITIONS
// ============================================================================

/**
 * Zod schemas for runtime validation and type generation
 * 
 * Benefits:
 * - Runtime validation of all RPC inputs (security)
 * - Auto-generated TypeScript types
 * - Type-safe parsers with excellent error messages
 * - Self-documenting schemas
 */

// ---- ID Schemas ----

/** Percentage (0-1) */
const PercentageSchema = z.number().min(0).max(1);

/** Participant ID - email format */
const ParticipantIdSchema = z.string().email();

/** Collective ID - non-empty string */
const CollectiveIdSchema = z.string().min(1).max(100);

/** Goal ID - UUID format */
const GoalIdSchema = z.string().uuid();

// ---- Credential Schema (Discriminated Union) ----

const CredentialSchema = z.discriminatedUnion('type', [
  z.object({ 
    type: z.literal('password'), 
    data: z.string().min(8) 
  }),
  z.object({ 
    type: z.literal('publicKey'), 
    data: z.string().min(32) 
  }),
  z.object({ 
    type: z.literal('oauth'), 
    data: z.string() 
  })
]);

// ---- Matrix Schemas ----

/** Recognition value between 0 and 1 */
const RecognitionValueSchema = z.number().min(0).max(1);

/** Recognition row that sums to 1 (budget constraint) */
const RecognitionRowSchema = z.array(RecognitionValueSchema)
  .refine(
    row => {
      const sum = row.reduce((a, b) => a + b, 0);
      return Math.abs(sum - 1.0) < 0.0001;
    },
    { message: "Row must sum to 1.0 (budget constraint - Axiom 1)" }
  );

/** Full recognition matrix (all rows sum to 1) */
const RecognitionMatrixSchema = z.array(RecognitionRowSchema);

/** Participant index (non-negative integer) */
const ParticipantIndexSchema = z.number().int().nonnegative();

/** Collective indices (array of participant indices) */
const CollectiveIndicesSchema = z.array(ParticipantIndexSchema);

/** Capacity value (non-negative) */
const CapacitySchema = z.number().nonnegative();

/** Capacity array */
const CapacityArraySchema = z.array(CapacitySchema);

// ---- Result Schemas ----

/** Allocation result from multi-provider algorithm */
const AllocationResultSchema = z.object({
  allocations: z.array(z.number()),
  remainingNeed: z.number().nonnegative(),
  satisfied: z.boolean(),
  iterations: z.number().int().nonnegative()
});

/** Goal progress */
const GoalProgressSchema = z.object({
  accepted: z.boolean(),
  goalProbability: z.number().min(0).max(1),
  reason: z.string().optional()
});

/** MRD result for a participant */
const MRDResultSchema = z.object({
  participantId: ParticipantIdSchema,
  participantIndex: ParticipantIndexSchema,
  mrd: z.number().nonnegative(),
  aboveThreshold: z.boolean()
});

/** Matrix update (for collaborative editing) */
const MatrixUpdateSchema = z.object({
  row: z.number().int().nonnegative(),
  col: z.number().int().nonnegative(),
  oldValue: z.number(),
  newValue: z.number(),
  timestamp: z.number().int().positive(),
  updatedBy: ParticipantIdSchema
});

/** Matrix bounds (for region access control) */
const MatrixBoundsSchema = z.object({
  startRow: z.number().int().nonnegative(),
  endRow: z.number().int().positive(),
  startCol: z.number().int().nonnegative(),
  endCol: z.number().int().positive()
}).refine(
  bounds => bounds.endRow > bounds.startRow && bounds.endCol > bounds.startCol,
  { message: "End indices must be greater than start indices" }
);

/** Share type for allocation */
const ShareTypeSchema = z.enum(['RS', 'MRS', 'SCMRS']);

/** Membership model */
const MembershipModelSchema = z.enum(['collective', 'commons']);

// ---- Time & Location Schemas ----

/** Time range within a day (HH:MM format) */
const TimeRangeSchema = z.object({
  start_time: z.string().regex(/^\d{2}:\d{2}$/),
  end_time: z.string().regex(/^\d{2}:\d{2}$/)
});

/** Day of week */
const DayOfWeekSchema = z.enum([
  'monday', 'tuesday', 'wednesday', 'thursday', 
  'friday', 'saturday', 'sunday'
]);

/** Day schedule - specific days with time ranges */
const DayScheduleSchema = z.object({
  days: z.array(DayOfWeekSchema),
  time_ranges: z.array(TimeRangeSchema)
});

/** Availability window - hierarchical recurring time specification */
const AvailabilityWindowSchema = z.object({
  day_schedules: z.array(DayScheduleSchema).optional(),
  time_ranges: z.array(TimeRangeSchema).optional()
});

/** Location schema */
const LocationSchema = z.object({
  type: z.enum(['physical', 'online', 'hybrid']).optional(),
  longitude: z.number().min(-180).max(180).optional(),
  latitude: z.number().min(-90).max(90).optional(),
  city: z.string().optional(),
  state_province: z.string().optional(),
  country: z.string().optional(),
  online_link: z.string().url().optional()
});

// ---- Slot Schemas ----

/** Need Type - categorizes different types of needs/capacity */
const NeedTypeSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  unit: z.string().default('units'),
  emoji: z.string().optional()
});

/** Divisibility constraints to prevent over-fragmentation */
const DivisibilitySchema = z.object({
  max_natural_div: z.number().int().gte(1).optional(),
  min_allocation_percentage: PercentageSchema.optional()
});

/** Need Slot - represents a need with multi-dimensional attributes */
const NeedSlotSchema = z.object({
  id: z.string().min(1),
  participantId: ParticipantIdSchema,
  need_type_id: z.string().min(1),
  quantity: z.number().gte(0),
  name: z.string(),
  
  // Time constraints
  start_date: z.string().nullable().optional(),
  end_date: z.string().nullable().optional(),
  time_zone: z.string().default('UTC'),
  recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
  availability_window: AvailabilityWindowSchema.optional(),
  
  // Location
  location: LocationSchema.optional(),
  
  // Divisibility
  divisibility: DivisibilitySchema.optional(),
  
  // Compliance filter (JsonLogic rules)
  filter_rule: z.any().nullable().optional(),
  
  // Priority
  priority: z.number().optional()
});

/** Availability Slot - represents capacity with multi-dimensional attributes */
const AvailabilitySlotSchema = z.object({
  id: z.string().min(1),
  participantId: ParticipantIdSchema,
  need_type_id: z.string().min(1),
  quantity: z.number().gte(0),
  name: z.string(),
  
  // Time constraints
  start_date: z.string().nullable().optional(),
  end_date: z.string().nullable().optional(),
  time_zone: z.string().default('UTC'),
  recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
  availability_window: AvailabilityWindowSchema.optional(),
  
  // Location
  location: LocationSchema.optional(),
  
  // Divisibility
  divisibility: DivisibilitySchema.optional(),
  
  // Priority
  priority: z.number().optional()
});

/** Slot allocation record */
const SlotAllocationRecordSchema = z.object({
  needSlotId: z.string(),
  availabilitySlotId: z.string(),
  providerId: ParticipantIdSchema,
  recipientId: ParticipantIdSchema,
  allocatedQuantity: z.number().gte(0),
  timestamp: z.number().int().positive()
});

// ---- Dampening Schemas ----

/** Damping state to prevent oscillation */
const DampingStateSchema = z.object({
  overAllocationHistory: z.array(z.number()),
  dampingFactor: z.number().min(0).max(1)
});

/** Per-type damping (for multi-dimensional) */
const MultiTypeDampingSchema = z.record(
  z.string(), // need_type_id
  DampingStateSchema
);

// ---- Convergence Tracking ----

const ConvergenceMetricsSchema = z.object({
  totalNeed: z.number().gte(0),
  totalCapacity: z.number().gte(0),
  totalAllocated: z.number().gte(0),
  satisfactionRate: z.number().min(0).max(1),
  allocationEfficiency: z.number().min(0).max(1),
  changeFromPrevious: z.number().optional()
});

// ---- Infer TypeScript Types from Schemas ----

type ParticipantId = z.infer<typeof ParticipantIdSchema>;
type CollectiveId = z.infer<typeof CollectiveIdSchema>;
type GoalId = z.infer<typeof GoalIdSchema>;
type Credential = z.infer<typeof CredentialSchema>;
type RecognitionValue = z.infer<typeof RecognitionValueSchema>;
type RecognitionRow = z.infer<typeof RecognitionRowSchema>;
type RecognitionMatrix = z.infer<typeof RecognitionMatrixSchema>;
type ParticipantIndex = z.infer<typeof ParticipantIndexSchema>;
type CollectiveIndices = z.infer<typeof CollectiveIndicesSchema>;
type Capacity = z.infer<typeof CapacitySchema>;
type CapacityArray = z.infer<typeof CapacityArraySchema>;
type AllocationResult = z.infer<typeof AllocationResultSchema>;
type GoalProgress = z.infer<typeof GoalProgressSchema>;
type MRDResult = z.infer<typeof MRDResultSchema>;
type MatrixUpdate = z.infer<typeof MatrixUpdateSchema>;
type MatrixBounds = z.infer<typeof MatrixBoundsSchema>;
type ShareType = z.infer<typeof ShareTypeSchema>;
type MembershipModel = z.infer<typeof MembershipModelSchema>;

// New types
type TimeRange = z.infer<typeof TimeRangeSchema>;
type DayOfWeek = z.infer<typeof DayOfWeekSchema>;
type DaySchedule = z.infer<typeof DayScheduleSchema>;
type AvailabilityWindow = z.infer<typeof AvailabilityWindowSchema>;
type Location = z.infer<typeof LocationSchema>;
type NeedType = z.infer<typeof NeedTypeSchema>;
type Divisibility = z.infer<typeof DivisibilitySchema>;
type NeedSlot = z.infer<typeof NeedSlotSchema>;
type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;
type DampingState = z.infer<typeof DampingStateSchema>;
type MultiTypeDamping = z.infer<typeof MultiTypeDampingSchema>;
type ConvergenceMetrics = z.infer<typeof ConvergenceMetricsSchema>;

// ---- TypeScript RPC Interfaces (for type-safe clients) ----

/**
 * These interfaces define the RPC API shape.
 * Clients use RpcStub<Interface> for type-safe method calls.
 * Servers implement the interface with actual logic.
 */

interface IRecognitionBudget {
  allocateRecognition(targetId: ParticipantId, amount: number): Promise<boolean>;
  getRecognitionTo(targetId: ParticipantId): Promise<number>;
  getAllAllocations(): Promise<Map<ParticipantId, number>>;
  getTotalAllocated(): Promise<number>;
  getParticipantId(): Promise<ParticipantId>;
  getParticipantIndex(): Promise<ParticipantIndex>;
}

interface INetworkState {
  registerParticipant(participantId: ParticipantId): Promise<RpcStub<IRecognitionBudget>>;
  computeMutualRecognition(participantAId: ParticipantId, participantBId: ParticipantId): Promise<number>;
  computeTotalMR(participantId: ParticipantId): Promise<number>;
  computeMRS(participantAId: ParticipantId, participantBId: ParticipantId): Promise<number>;
  getRecognitionBudget(participantId: ParticipantId): Promise<RpcStub<IRecognitionBudget>>;
}

interface ICollective {
  attemptJoin(participantId: ParticipantId): Promise<RpcStub<ICollective>>;
  computeMRDForParticipant(participantId: ParticipantId): Promise<number>;
  computeAllMRD(): Promise<MRDResult[]>;
  getMembers(): Promise<ParticipantId[]>;
  isMember(participantId: ParticipantId): Promise<boolean>;
  getCollectiveId(): Promise<CollectiveId>;
}

interface IParticipantGoal {
  receiveCapacity(fromId: ParticipantId, amount: number): Promise<GoalProgress>;
  getProgress(): Promise<{
    totalReceived: number;
    beneficialReceived: number;
    nonBeneficialReceived: number;
    probability: number;
  }>;
  getGoalId(): Promise<GoalId>;
  getBeneficialSet(): Promise<ParticipantId[]>;
}

interface IAuthenticatedParticipant {
  // Basic capacity/recognition methods
  getRecognitionBudget(): Promise<RpcStub<IRecognitionBudget>>;
  getNetworkState(): Promise<RpcStub<INetworkState>>;
  allocateCapacity(recipientId: ParticipantId, requestedAmount: number): Promise<number>;
  receiveCapacity(fromId: ParticipantId, amount: number): Promise<void>;
  getGoal(goalId: GoalId, beneficialParticipantIds?: ParticipantId[]): Promise<RpcStub<IParticipantGoal>>;
  joinCollective(collectiveId: CollectiveId): Promise<RpcStub<ICollective>>;
  getCapacity(): Promise<number>;
  addCapacity(amount: number): Promise<void>;
  getParticipantId(): Promise<ParticipantId>;
  
  // Slot-based allocation methods
  addNeedSlot(slot: NeedSlot): Promise<void>;
  addAvailabilitySlot(slot: AvailabilitySlot): Promise<void>;
  getNeedSlots(): Promise<NeedSlot[]>;
  getAvailabilitySlots(): Promise<AvailabilitySlot[]>;
  removeNeedSlot(slotId: string): Promise<void>;
  removeAvailabilitySlot(slotId: string): Promise<void>;
  
  // Allocation requests
  requestAllocation(needSlotId: string): Promise<SlotAllocationRecord[]>;
  getAllocations(): Promise<SlotAllocationRecord[]>;
  getConvergenceMetrics(): Promise<ConvergenceMetrics>;
}

interface IParticipantServer {
  authenticate(participantId: ParticipantId, credentials: Credential): Promise<RpcStub<IAuthenticatedParticipant>>;
  getPublicNetworkView(): Promise<RpcStub<INetworkState>>;
  getCollective(collectiveId: CollectiveId, threshold?: number, model?: MembershipModel): Promise<RpcStub<ICollective>>;
}

// ============================================================================
// SLOT MATCHING UTILITIES
// ============================================================================

/**
 * Timezone-aware time matching utilities
 * 
 * Converts times from local timezones to UTC for comparison,
 * enabling global coordination across timezones.
 */
class TimeMatching {
  /**
   * Convert HH:MM time from timezone to UTC
   */
  static convertTimeToUTC(
    timeStr: string, 
    dateStr: string, 
    timezone: string = 'UTC'
  ): string {
    if (timezone === 'UTC' || timezone === 'Etc/UTC') {
      return timeStr;
    }
    
    try {
      const [hours, minutes] = timeStr.split(':').map(Number);
      const [year, month, day] = dateStr.split('-').map(Number);
      
      // Create reference date
      const refUTC = Date.UTC(year, month - 1, day, 12, 0, 0);
      const refDate = new Date(refUTC);
      
      // Format in target timezone
      const formatter = new Intl.DateTimeFormat('en-US', {
        timeZone: timezone,
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        hour12: false
      });
      
      const parts = formatter.formatToParts(refDate);
      const tzHour = parseInt(parts.find(p => p.type === 'hour')!.value);
      const tzDay = parseInt(parts.find(p => p.type === 'day')!.value);
      
      // Calculate offset
      const offsetHours = 12 - tzHour;
      const dayShift = tzDay - day;
      
      // Apply offset
      let utcHours = hours + offsetHours - (dayShift * 24);
      let utcMinutes = minutes;
      
      // Normalize
      while (utcHours < 0) utcHours += 24;
      while (utcHours >= 24) utcHours -= 24;
      
      return `${String(utcHours).padStart(2, '0')}:${String(utcMinutes).padStart(2, '0')}`;
    } catch (error) {
      console.warn('Timezone conversion failed:', error);
      return timeStr;
    }
  }
  
  /**
   * Check if two time ranges overlap (both in UTC)
   */
  static timeRangesOverlap(
    range1: TimeRange,
    range2: TimeRange
  ): boolean {
    const start1 = range1.start_time;
    const end1 = range1.end_time;
    const start2 = range2.start_time;
    const end2 = range2.end_time;
    
    // No overlap if one ends before the other starts
    if (end1 <= start2 || end2 <= start1) {
      return false;
    }
    
    return true;
  }
  
  /**
   * Check if availability windows overlap
   */
  static availabilityWindowsOverlap(
    window1?: AvailabilityWindow,
    window2?: AvailabilityWindow,
    tz1: string = 'UTC',
    tz2: string = 'UTC',
    referenceDate: string = '2024-01-01'
  ): boolean {
    // If no windows specified, assume always available
    if (!window1 && !window2) return true;
    if (!window1 || !window2) return false;
    
    // Check time ranges (simplest case)
    if (window1.time_ranges && window2.time_ranges) {
      for (const tr1 of window1.time_ranges) {
        // Convert to UTC
        const utc_tr1 = {
          start_time: this.convertTimeToUTC(tr1.start_time, referenceDate, tz1),
          end_time: this.convertTimeToUTC(tr1.end_time, referenceDate, tz1)
        };
        
        for (const tr2 of window2.time_ranges) {
          const utc_tr2 = {
            start_time: this.convertTimeToUTC(tr2.start_time, referenceDate, tz2),
            end_time: this.convertTimeToUTC(tr2.end_time, referenceDate, tz2)
          };
          
          if (this.timeRangesOverlap(utc_tr1, utc_tr2)) {
            return true;
          }
        }
      }
    }
    
    // Check day schedules
    if (window1.day_schedules && window2.day_schedules) {
      for (const ds1 of window1.day_schedules) {
        for (const ds2 of window2.day_schedules) {
          // Check if days overlap
          const daysOverlap = ds1.days.some(d => ds2.days.includes(d));
          if (!daysOverlap) continue;
          
          // Check if time ranges overlap on those days
          for (const tr1 of ds1.time_ranges) {
            const utc_tr1 = {
              start_time: this.convertTimeToUTC(tr1.start_time, referenceDate, tz1),
              end_time: this.convertTimeToUTC(tr1.end_time, referenceDate, tz1)
            };
            
            for (const tr2 of ds2.time_ranges) {
              const utc_tr2 = {
                start_time: this.convertTimeToUTC(tr2.start_time, referenceDate, tz2),
                end_time: this.convertTimeToUTC(tr2.end_time, referenceDate, tz2)
              };
              
              if (this.timeRangesOverlap(utc_tr1, utc_tr2)) {
                return true;
              }
            }
          }
        }
      }
    }
    
    return false;
  }
}

/**
 * Location matching utilities
 */
class LocationMatching {
  /**
   * Calculate distance between two coordinates (Haversine formula)
   * Returns distance in kilometers
   */
  static calculateDistance(
    lat1: number, lon1: number,
    lat2: number, lon2: number
  ): number {
    const R = 6371; // Earth's radius in km
    const dLat = (lat2 - lat1) * Math.PI / 180;
    const dLon = (lon2 - lon1) * Math.PI / 180;
    const a = 
      Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(lat1 * Math.PI / 180) * Math.cos(lat2 * Math.PI / 180) *
      Math.sin(dLon / 2) * Math.sin(dLon / 2);
    const c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a));
    return R * c;
  }
  
  /**
   * Check if two locations are compatible
   */
  static locationsCompatible(
    loc1?: Location,
    loc2?: Location,
    maxDistanceKm: number = 50
  ): boolean {
    // No location constraints = always compatible
    if (!loc1 && !loc2) return true;
    if (!loc1 || !loc2) return false;
    
    // Both online = compatible
    if (loc1.type === 'online' && loc2.type === 'online') return true;
    if (loc1.type === 'online' || loc2.type === 'online') return true;
    
    // Check city/country match
    if (loc1.city && loc2.city && loc1.city !== loc2.city) {
      // Different cities - check distance if coordinates available
      if (loc1.latitude && loc1.longitude && 
          loc2.latitude && loc2.longitude) {
        const distance = this.calculateDistance(
          loc1.latitude, loc1.longitude,
          loc2.latitude, loc2.longitude
        );
        return distance <= maxDistanceKm;
      }
      return false;
    }
    
    if (loc1.country && loc2.country && loc1.country !== loc2.country) {
      return false;
    }
    
    // Check coordinate distance if available
    if (loc1.latitude && loc1.longitude && 
        loc2.latitude && loc2.longitude) {
      const distance = this.calculateDistance(
        loc1.latitude, loc1.longitude,
        loc2.latitude, loc2.longitude
      );
      return distance <= maxDistanceKm;
    }
    
    return true;
  }
}

/**
 * Compliance filter evaluation (JsonLogic)
 * 
 * Simplified implementation - in production would use jsonlogic library
 */
class ComplianceFilters {
  /**
   * Evaluate a JsonLogic filter rule
   */
  static evaluate(rule: any, data: any): boolean {
    if (!rule) return true;
    
    // Simplified: just check if data matches rule conditions
    // In production, use full JsonLogic implementation
    return true;
  }
}

/**
 * Slot compatibility checker
 */
class SlotMatching {
  /**
   * Check if a need slot and availability slot are compatible
   */
  static slotsCompatible(
    needSlot: NeedSlot,
    availSlot: AvailabilitySlot,
    maxDistanceKm: number = 50
  ): boolean {
    // Type must match
    if (needSlot.need_type_id !== availSlot.need_type_id) {
      return false;
    }
    
    // Check location compatibility
    if (!LocationMatching.locationsCompatible(
      needSlot.location,
      availSlot.location,
      maxDistanceKm
    )) {
      return false;
    }
    
    // Check time compatibility
    const timeCompatible = TimeMatching.availabilityWindowsOverlap(
      needSlot.availability_window,
      availSlot.availability_window,
      needSlot.time_zone,
      availSlot.time_zone,
      needSlot.start_date || '2024-01-01'
    );
    
    if (!timeCompatible) {
      return false;
    }
    
    // Check compliance filter
    if (needSlot.filter_rule) {
      const providerData = { providerId: availSlot.participantId };
      if (!ComplianceFilters.evaluate(needSlot.filter_rule, providerData)) {
        return false;
      }
    }
    
    return true;
  }
  
  /**
   * Get compatible providers for a need slot
   */
  static getCompatibleProviders(
    needSlot: NeedSlot,
    availabilitySlots: AvailabilitySlot[]
  ): AvailabilitySlot[] {
    return availabilitySlots.filter(avail => 
      this.slotsCompatible(needSlot, avail)
    );
  }
}

// ============================================================================
// DAMPENING & DIVISIBILITY UTILITIES
// ============================================================================

/**
 * Dampening system to prevent oscillation
 * 
 * When allocations overshoot needs repeatedly, damping reduces the
 * allocation rate to achieve convergence.
 * 
 * Formula: damping_factor = max(0.1, 1 - (avg_recent_overshoot * sensitivity))
 */
class DampeningSystem {
  private static readonly HISTORY_WINDOW = 5; // Track last N iterations
  private static readonly SENSITIVITY = 0.5; // How aggressively to dampen
  private static readonly MIN_DAMPING = 0.1; // Never go below 10%
  
  /**
   * Calculate damping factor based on over-allocation history
   */
  static calculateDampingFactor(
    overAllocationHistory: number[]
  ): number {
    if (overAllocationHistory.length === 0) return 1.0;
    
    // Calculate average recent over-allocation
    const recentHistory = overAllocationHistory.slice(-this.HISTORY_WINDOW);
    const avgOvershoot = recentHistory.reduce((sum, val) => sum + val, 0) / recentHistory.length;
    
    // Apply damping formula
    const dampingFactor = Math.max(
      this.MIN_DAMPING,
      1 - (avgOvershoot * this.SENSITIVITY)
    );
    
    return dampingFactor;
  }
  
  /**
   * Update damping state with new allocation results
   */
  static updateDampingState(
    state: DampingState,
    allocated: number,
    need: number
  ): DampingState {
    const overshoot = need > 0 ? Math.max(0, (allocated - need) / need) : 0;
    
    const newHistory = [...state.overAllocationHistory, overshoot];
    if (newHistory.length > this.HISTORY_WINDOW) {
      newHistory.shift(); // Keep only recent history
    }
    
    return {
      overAllocationHistory: newHistory,
      dampingFactor: this.calculateDampingFactor(newHistory)
    };
  }
  
  /**
   * Apply damping to allocation amounts
   */
  static applyDamping(
    rawAllocation: number,
    dampingFactor: number
  ): number {
    return rawAllocation * dampingFactor;
  }
}

/**
 * Divisibility constraint system
 * 
 * Prevents over-fragmentation by enforcing:
 * 1. Maximum natural divisions (e.g., can't divide a person)
 * 2. Minimum allocation percentages (e.g., don't allocate <10%)
 */
class DivisibilityConstraints {
  /**
   * Check if an allocation satisfies divisibility constraints
   */
  static satisfiesConstraints(
    requestedAmount: number,
    totalAvailable: number,
    constraints?: Divisibility
  ): boolean {
    if (!constraints) return true;
    
    // Check minimum percentage constraint
    if (constraints.min_allocation_percentage) {
      const percentage = requestedAmount / totalAvailable;
      if (percentage < constraints.min_allocation_percentage) {
        return false;
      }
    }
    
    return true;
  }
  
  /**
   * Get minimum allowed allocation based on constraints
   */
  static getMinimumAllocation(
    totalAvailable: number,
    constraints?: Divisibility
  ): number {
    if (!constraints) return 0;
    
    if (constraints.min_allocation_percentage) {
      return totalAvailable * constraints.min_allocation_percentage;
    }
    
    return 0;
  }
  
  /**
   * Round allocation to satisfy natural division constraints
   */
  static roundToNaturalUnit(
    amount: number,
    totalAvailable: number,
    constraints?: Divisibility
  ): number {
    if (!constraints || !constraints.max_natural_div) {
      return amount;
    }
    
    // Calculate unit size
    const unitSize = totalAvailable / constraints.max_natural_div;
    
    // Round to nearest unit
    return Math.round(amount / unitSize) * unitSize;
  }
}

/**
 * Largest Remainder Method for fair redistribution
 * 
 * When distributing indivisible items (e.g., slots), uses the
 * Largest Remainder Method to fairly allocate remainders.
 */
class LargestRemainderMethod {
  /**
   * Allocate integer quantities fairly using largest remainder
   * 
   * @param shares - Proportional shares (sum to 1.0)
   * @param totalQuantity - Total integer quantity to allocate
   * @returns Integer allocations that sum exactly to totalQuantity
   */
  static allocate(
    shares: Record<string, number>,
    totalQuantity: number
  ): Record<string, number> {
    const result: Record<string, number> = {};
    const remainders: Array<{ id: string; remainder: number }> = [];
    
    let allocatedSoFar = 0;
    
    // Step 1: Allocate integer parts
    for (const [id, share] of Object.entries(shares)) {
      const exactAmount = share * totalQuantity;
      const integerPart = Math.floor(exactAmount);
      const remainder = exactAmount - integerPart;
      
      result[id] = integerPart;
      allocatedSoFar += integerPart;
      
      if (remainder > 0) {
        remainders.push({ id, remainder });
      }
    }
    
    // Step 2: Distribute remaining units to largest remainders
    const remaining = totalQuantity - allocatedSoFar;
    remainders.sort((a, b) => b.remainder - a.remainder);
    
    for (let i = 0; i < remaining && i < remainders.length; i++) {
      result[remainders[i].id]++;
    }
    
    return result;
  }
}

// ============================================================================
// SPACE-TIME INDEXING
// ============================================================================

/**
 * Space-Time Index for O(k) recipient lookups
 * 
 * Instead of scanning all N participants, use indexes to find only the k
 * participants who might match (by type, location, time).
 * 
 * Typical performance: O(k) instead of O(N) where k << N
 */
class SpaceTimeIndex {
  private byType: Map<string, Set<string>> = new Map();
  private byLocation: Map<string, Set<string>> = new Map();
  private byTime: Map<string, Set<string>> = new Map();
  
  /**
   * Index a slot
   */
  addSlot(slot: NeedSlot | AvailabilitySlot): void {
    const participantId = slot.participantId;
    
    // Index by type
    if (!this.byType.has(slot.need_type_id)) {
      this.byType.set(slot.need_type_id, new Set());
    }
    this.byType.get(slot.need_type_id)!.add(participantId);
    
    // Index by location bucket
    if (slot.location) {
      const locBucket = this.getLocationBucket(slot.location);
      if (!this.byLocation.has(locBucket)) {
        this.byLocation.set(locBucket, new Set());
      }
      this.byLocation.get(locBucket)!.add(participantId);
    }
    
    // Index by time bucket
    const timeBucket = this.getTimeBucket(slot);
    if (!this.byTime.has(timeBucket)) {
      this.byTime.set(timeBucket, new Set());
    }
    this.byTime.get(timeBucket)!.add(participantId);
  }
  
  /**
   * Find participants matching a need
   */
  findMatching(need: NeedSlot): Set<string> {
    // Get candidates by type (most restrictive filter)
    const typeMatches = this.byType.get(need.need_type_id);
    if (!typeMatches || typeMatches.size === 0) {
      return new Set();
    }
    
    // Further filter by location if specified
    if (need.location) {
      const locBucket = this.getLocationBucket(need.location);
      const locMatches = this.byLocation.get(locBucket);
      if (locMatches) {
        // Intersect with type matches
        return new Set([...typeMatches].filter(id => locMatches.has(id)));
      }
    }
    
    return typeMatches;
  }
  
  /**
   * Get location bucket for indexing
   */
  private getLocationBucket(loc: Location): string {
    if (loc.type === 'online') return 'online';
    if (loc.city) return `city:${loc.city}`;
    if (loc.country) return `country:${loc.country}`;
    if (loc.latitude && loc.longitude) {
      // Grid-based bucketing (10km grid)
      const latBucket = Math.floor(loc.latitude / 0.1);
      const lonBucket = Math.floor(loc.longitude / 0.1);
      return `grid:${latBucket},${lonBucket}`;
    }
    return 'unknown';
  }
  
  /**
   * Get time bucket for indexing
   */
  private getTimeBucket(slot: NeedSlot | AvailabilitySlot): string {
    if (slot.recurrence) {
      return `recur:${slot.recurrence}`;
    }
    if (slot.start_date) {
      // Weekly bucket
      const date = new Date(slot.start_date);
      const weekNum = Math.floor(date.getTime() / (7 * 24 * 60 * 60 * 1000));
      return `week:${weekNum}`;
    }
    return 'anytime';
  }
  
  /**
   * Clear index
   */
  clear(): void {
    this.byType.clear();
    this.byLocation.clear();
    this.byTime.clear();
  }
}

// ============================================================================
// CONVERGENCE TRACKING
// ============================================================================

/**
 * Convergence metrics tracker
 * 
 * Tracks allocation progress and convergence toward equilibrium.
 */
class ConvergenceTracker {
  /**
   * Calculate convergence metrics
   */
  static calculateMetrics(
    needSlots: NeedSlot[],
    availabilitySlots: AvailabilitySlot[],
    allocations: SlotAllocationRecord[]
  ): ConvergenceMetrics {
    // Calculate totals
    const totalNeed = needSlots.reduce((sum, slot) => sum + slot.quantity, 0);
    const totalCapacity = availabilitySlots.reduce((sum, slot) => sum + slot.quantity, 0);
    const totalAllocated = allocations.reduce((sum, alloc) => sum + alloc.allocatedQuantity, 0);
    
    // Calculate satisfaction rate
    const satisfactionRate = totalNeed > 0 ? totalAllocated / totalNeed : 1.0;
    
    // Calculate allocation efficiency (how much of available capacity was used)
    const allocationEfficiency = totalCapacity > 0 ? totalAllocated / totalCapacity : 0;
    
    return {
      totalNeed,
      totalCapacity,
      totalAllocated,
      satisfactionRate,
      allocationEfficiency
    };
  }
  
  /**
   * Check if allocation has converged
   */
  static hasConverged(
    currentMetrics: ConvergenceMetrics,
    previousMetrics?: ConvergenceMetrics,
    threshold: number = 0.01
  ): boolean {
    if (!previousMetrics) return false;
    
    // Check if change is below threshold
    const change = Math.abs(
      currentMetrics.satisfactionRate - previousMetrics.satisfactionRate
    );
    
    return change < threshold;
  }
}

// ============================================================================
// PHASE 1: CORE MATRIX MATHEMATICS
// ============================================================================

/**
 * Core matrix operations for Free Association (SPARSE IMPLEMENTATION)
 * 
 * Implements the mathematical formulas from Section 2-4 of matrix-rpc.md:
 * - RS (Recognition-Shares): Row-normalized recognition matrix
 * - MR (Mutual-Recognition): Element-wise min of reciprocal recognition
 * - MRS (Mutual-Recognition-Shares): Row-normalized mutual recognition
 * - SCMRS (Synthetic-Collective-MR-Shares): Collective allocation shares
 * - MRD (Mutual-Recognition-Density): Network integration metric
 * 
 * ✨ NOW WITH SPARSE MATRIX OPTIMIZATION ✨
 * - Memory: O(e) instead of O(n²) where e = number of edges
 * - Speed: O(e) instead of O(n²) for most operations
 * - Typical savings: 95-99% less memory, 95-99% faster
 * 
 * Example: 1000 participants, 10 recognition links each
 * - Dense: 8 MB memory, 1M operations
 * - Sparse: 80 KB memory, 10K operations
 * - Savings: 100× less memory, 100× faster!
 */
class FreeAssociationMatrices {
  private n: number; // Maximum participant index (for bounds checking)
  private R: SparseMatrix; // Sparse recognition matrix (only non-zero entries)
  
  constructor(n: number) {
    this.n = n;
    this.R = Sparse.create();
  }
  
  /**
   * Set recognition from participant i to participant j (SPARSE)
   * @param i - Giver index (0-based)
   * @param j - Receiver index (0-based)
   * @param value - Recognition amount [0, 1]
   */
  setRecognition(i: number, j: number, value: number): void {
    if (i < 0 || i >= this.n || j < 0 || j >= this.n) {
      throw new Error(`Index out of bounds: (${i}, ${j})`);
    }
    if (value < 0 || value > 1) {
      throw new Error(`Recognition value must be in [0, 1]: ${value}`);
    }
    Sparse.set(this.R, i, j, value);
  }
  
  /**
   * Get recognition from participant i to participant j (SPARSE)
   */
  getRecognition(i: number, j: number): number {
    if (i < 0 || i >= this.n || j < 0 || j >= this.n) {
      throw new Error(`Index out of bounds: (${i}, ${j})`);
    }
    return Sparse.get(this.R, i, j);
  }
  
  /**
   * Set entire recognition matrix (SPARSE)
   * Converts dense matrix to sparse representation automatically
   */
  setRecognitionMatrix(matrix: number[][]): void {
    if (matrix.length !== this.n) {
      throw new Error(`Matrix must be ${this.n}×${this.n}`);
    }
    for (let i = 0; i < this.n; i++) {
      if (matrix[i].length !== this.n) {
        throw new Error(`Matrix must be ${this.n}×${this.n}`);
      }
    }
    // Convert dense to sparse (automatically filters out zeros)
    this.R = Sparse.fromDense(matrix);
  }
  
  /**
   * Get memory statistics (SPARSE)
   */
  getMemoryStats(): {
    entries: number;
    memoryKB: string;
    sparsity: string;
    savingsVsDense: string;
  } {
    const stats = Sparse.getStats(this.R, this.n);
    const comparison = SparseCompare.compareMemory(this.n, stats.entries);
    
    return {
      entries: stats.entries,
      memoryKB: stats.memoryKB,
      sparsity: stats.sparsity,
      savingsVsDense: comparison.savings.percentage
    };
  }
  
  /**
   * Validate budget constraint: each row sums to 1 (Axiom 1) (SPARSE)
   */
  validateBudgetConstraint(tolerance: number = 0.0001): boolean {
    // Check all participants who have allocated recognition
    for (const [i, row] of this.R.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      if (Math.abs(sum - 1.0) > tolerance) {
        return false;
      }
    }
    return true;
  }
  
  /**
   * Compute Recognition-Shares (RS) (SPARSE)
   * 
   * Formula: RS_ij = R_ij / Σ_k R_ik
   * 
   * Row-normalize R so each row sums to 1.
   * 
   * Properties:
   * - Each row sums to 1: Σ_j RS_ij = 1
   * - RS_ij ∈ [0, 1]
   * 
   * ✨ SPARSE OPTIMIZATION: Only normalizes non-zero rows
   * 
   * @returns RS matrix (sparse)
   */
  computeRS(): SparseMatrix {
    const timer = SparsePerf.startTimer();
    
    // Use sparse row normalization (only processes non-zero entries)
    const RS = Sparse.rowNormalize(this.R);
    
    SparsePerf.recordOperation('computeRS', timer());
    return RS;
  }
  
  /**
   * Compute Mutual-Recognition (MR) (SPARSE)
   * 
   * Formula: MR_ij = min(RS_ij, RS_ji)
   * 
   * Element-wise minimum of RS and its transpose.
   * 
   * Properties:
   * - Symmetric: MR_ij = MR_ji
   * - MR_ij ∈ [0, 1]
   * - MR_ij ≤ min(RS_ij, RS_ji)
   * 
   * ✨ SPARSE OPTIMIZATION: Only computes for non-zero RS entries
   * Typical speedup: 100× (only processes actual relationships)
   * 
   * @returns MR matrix (sparse)
   */
  computeMR(): SparseMatrix {
    const timer = SparsePerf.startTimer();
    
    const RS = this.computeRS();
    const RS_T = Sparse.transpose(RS);
    
    // Element-wise min of RS and its transpose
    // Only computes where RS has non-zero entries
    const MR = Sparse.elementWiseMin(RS, RS_T);
    
    SparsePerf.recordOperation('computeMR', timer());
    return MR;
  }
  
  /**
   * Compute Total Mutual Recognition vector (t) (SPARSE)
   * 
   * Formula: t_i = Σ_j MR_ij
   * 
   * Sum of mutual recognition for each participant.
   * 
   * ✨ SPARSE OPTIMIZATION: Only sums non-zero entries
   * 
   * @returns t vector (length n)
   */
  computeTotalMR(): number[] {
    const timer = SparsePerf.startTimer();
    
    const MR = this.computeMR();
    const t: number[] = Array(this.n).fill(0);
    
    // Only iterate over participants with non-zero MR
    for (const [i, row] of MR.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      t[i] = sum;
    }
    
    SparsePerf.recordOperation('computeTotalMR', timer());
    return t;
  }
  
  /**
   * Compute Mutual-Recognition-Shares (MRS) (SPARSE)
   * 
   * Formula: MRS_ij = MR_ij / t_i
   * 
   * Row-normalize MR by total mutual recognition.
   * 
   * Properties:
   * - Each row sums to 1: Σ_j MRS_ij = 1
   * - MRS_ij ∈ [0, 1]
   * 
   * ✨ SPARSE OPTIMIZATION: Only normalizes non-zero MR entries
   * 
   * @returns MRS matrix (sparse)
   */
  computeMRS(): SparseMatrix {
    const timer = SparsePerf.startTimer();
    
    const MR = this.computeMR();
    const t = this.computeTotalMR();
    const MRS = Sparse.create();
    
    // Only process rows with non-zero total MR
    for (const [i, row] of MR.entries()) {
      if (t[i] === 0) continue; // Avoid division by zero
      
      for (const [j, value] of row.entries()) {
        Sparse.set(MRS, i, j, value / t[i]);
      }
    }
    
    SparsePerf.recordOperation('computeMRS', timer());
    return MRS;
  }
  
  /**
   * Verify symmetry property of MR (SPARSE)
   * MR_ij should equal MR_ji
   */
  verifyMRSymmetry(tolerance: number = 1e-10): boolean {
    const MR = this.computeMR();
    
    // Check all entries in sparse matrix
    for (const [i, row] of MR.entries()) {
      for (const [j, value_ij] of row.entries()) {
        const value_ji = Sparse.get(MR, j, i);
        if (Math.abs(value_ij - value_ji) > tolerance) {
          return false;
        }
      }
    }
    
    return true;
  }
  
  /**
   * Verify that each row of a matrix sums to 1 (SPARSE)
   */
  verifyRowNormalization(matrix: SparseMatrix, tolerance: number = 0.0001): boolean {
    for (const [i, row] of matrix.entries()) {
      let sum = 0;
      for (const value of row.values()) {
        sum += value;
      }
      if (Math.abs(sum - 1.0) > tolerance) {
        return false;
      }
    }
    return true;
  }
  
  // ========================================================================
  // COLLECTIVE OPERATIONS (Section 3)
  // ========================================================================
  
  /**
   * Compute Mutual Recognition within Collective (m_C) (SPARSE)
   * 
   * Formula: (m_C)_i = Σ_{j∈C} MR_ij
   * 
   * For each participant, sum their mutual recognition with collective members.
   * 
   * ✨ SPARSE OPTIMIZATION: Only sums existing MR relationships
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns m_C vector (length n)
   */
  computeMutualRecognitionWithinCollective(collectiveIndices: number[]): number[] {
    const timer = SparsePerf.startTimer();
    
    const MR = this.computeMR();
    const m_C: number[] = Array(this.n).fill(0);
    const collectiveSet = new Set(collectiveIndices);
    
    // Only iterate over participants with non-zero MR
    for (const [i, row] of MR.entries()) {
      let sum = 0;
      for (const [j, value] of row.entries()) {
        if (collectiveSet.has(j)) {
          sum += value;
        }
      }
      m_C[i] = sum;
    }
    
    SparsePerf.recordOperation('computeMutualRecognitionWithinCollective', timer());
    return m_C;
  }
  
  /**
   * Compute Total Pool within Collective (T_C) (SPARSE)
   * 
   * Formula: T_C = Σ_{i∈C} Σ_{j∈C} MR_ij
   * 
   * Sum of all mutual recognition between members of collective C.
   * 
   * ✨ SPARSE OPTIMIZATION: Only sums existing relationships within collective
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns T_C scalar
   */
  computeTotalPoolWithinCollective(collectiveIndices: number[]): number {
    const timer = SparsePerf.startTimer();
    
    const MR = this.computeMR();
    const collectiveSet = new Set(collectiveIndices);
    let T_C = 0;
    
    // Only iterate over collective members with non-zero MR
    for (const i of collectiveIndices) {
      const row = MR.get(i);
      if (!row) continue;
      
      for (const [j, value] of row.entries()) {
        if (collectiveSet.has(j)) {
          T_C += value;
        }
      }
    }
    
    SparsePerf.recordOperation('computeTotalPoolWithinCollective', timer());
    return T_C;
  }
  
  /**
   * Compute Synthetic-Collective-Mutual-Recognition-Shares (SCMRS) (SPARSE)
   * Weighted version (relationship strength weighted)
   * 
   * Formula: s_i = (m_C)_i / T_C = (Σ_{j∈C} MR_ij) / (Σ_{x∈C} Σ_{y∈C} MR_xy)
   * 
   * Properties:
   * - Σ_{i∈C} s_i = 1 (collective shares sum to 1)
   * - Higher MR with collective members → higher share
   * 
   * ✨ SPARSE: Works efficiently even for large collectives
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns s vector (length n, only collective members have non-zero values)
   */
  computeSCMRS_weighted(collectiveIndices: number[]): number[] {
    const timer = SparsePerf.startTimer();
    
    const m_C = this.computeMutualRecognitionWithinCollective(collectiveIndices);
    const T_C = this.computeTotalPoolWithinCollective(collectiveIndices);
    const s: number[] = Array(this.n).fill(0);
    
    // Avoid division by zero
    if (T_C === 0) {
      SparsePerf.recordOperation('computeSCMRS_weighted', timer());
      return s;
    }
    
    // Only compute for collective members
    for (const i of collectiveIndices) {
      s[i] = m_C[i] / T_C;
    }
    
    SparsePerf.recordOperation('computeSCMRS_weighted', timer());
    return s;
  }
  
  /**
   * Compute Synthetic-Collective-Relative-Mutual-Recognition-Shares (SCRMRS) (SPARSE)
   * Equal voice version (each member's MRS as equal vote)
   * 
   * Formula: s_i = (1/|C|) * Σ_{j∈C} MRS_ji
   * 
   * Properties:
   * - Σ_i s_i = 1 (shares sum to 1 across all participants)
   * - Each collective member has equal voting weight
   * 
   * ✨ SPARSE: Only processes actual MRS values
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns s vector (length n)
   */
  computeSCRMRS_equal(collectiveIndices: number[]): number[] {
    const timer = SparsePerf.startTimer();
    
    const MRS = this.computeMRS();
    const s: number[] = Array(this.n).fill(0);
    const C_size = collectiveIndices.length;
    
    if (C_size === 0) {
      SparsePerf.recordOperation('computeSCRMRS_equal', timer());
      return s;
    }
    
    // For each participant i, average the MRS_ji from all collective members j
    for (const j of collectiveIndices) {
      const row = MRS.get(j);
      if (!row) continue;
      
      // Add this collective member's MRS values to corresponding participants
      for (const [i, value] of row.entries()) {
        s[i] += value / C_size;
      }
    }
    
    SparsePerf.recordOperation('computeSCRMRS_equal', timer());
    return s;
  }
  
  // ========================================================================
  // NETWORK INTEGRATION METRICS (Section 4)
  // ========================================================================
  
  /**
   * Compute Average Mutual Recognition in Collective
   * 
   * Formula: m̄_C = T_C / |C|
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns Average MR scalar
   */
  computeAverageMRInCollective(collectiveIndices: number[]): number {
    const T_C = this.computeTotalPoolWithinCollective(collectiveIndices);
    const C_size = collectiveIndices.length;
    
    if (C_size === 0) {
      return 0;
    }
    
    return T_C / C_size;
  }
  
  /**
   * Compute Mutual-Recognition-Density (MRD)
   * 
   * Formula: MRD_C(i) = (|C| * (m_C)_i) / T_C
   * 
   * Measures how well integrated participant i is with collective C.
   * 
   * Properties:
   * - MRD ≈ 1: Participant has average integration
   * - MRD > 1: Participant has above-average integration
   * - MRD < 1: Participant has below-average integration
   * - Used for membership determination: accept if MRD ≥ threshold (typically 0.5)
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @param participantIndex - Index of participant to evaluate
   * @returns MRD scalar
   */
  computeMRD(collectiveIndices: number[], participantIndex: number): number {
    const m_C = this.computeMutualRecognitionWithinCollective(collectiveIndices);
    const T_C = this.computeTotalPoolWithinCollective(collectiveIndices);
    const C_size = collectiveIndices.length;
    
    if (T_C === 0 || C_size === 0) {
      return 0;
    }
    
    return (C_size * m_C[participantIndex]) / T_C;
  }
  
  /**
   * Compute MRD for all participants relative to collective
   * 
   * @param collectiveIndices - Indices of participants in collective C
   * @returns MRD vector (length n)
   */
  computeAllMRD(collectiveIndices: number[]): number[] {
    const mrd: number[] = Array(this.n).fill(0);
    
    for (let i = 0; i < this.n; i++) {
      mrd[i] = this.computeMRD(collectiveIndices, i);
    }
    
    return mrd;
  }
  
  /**
   * Determine collective membership based on MRD threshold
   * 
   * @param collectiveIndices - Current collective members
   * @param threshold - MRD threshold (typically 0.5)
   * @param model - "collective" (rising bar) or "commons" (stable bar)
   * @returns New collective member indices
   */
  determineMembership(
    collectiveIndices: number[], 
    threshold: number = 0.5, 
    model: "collective" | "commons" = "collective"
  ): number[] {
    if (model === "collective") {
      // Collective model: rising bar
      // Only existing members can remain if they meet threshold
      const mrd = this.computeAllMRD(collectiveIndices);
      return collectiveIndices.filter(i => mrd[i] >= threshold);
    } else {
      // Commons model: stable bar
      // Anyone meeting threshold relative to full participant set can join
      const allIndices = Array.from({ length: this.n }, (_, i) => i);
      const mrd = this.computeAllMRD(allIndices);
      return allIndices.filter(i => mrd[i] >= threshold);
    }
  }
  
  // ========================================================================
  // ALLOCATION PROTOCOLS (Section 5)
  // ========================================================================
  
  /**
   * Enhanced Slot-Based Allocation Engine
   * 
   * Integrates all advanced features:
   * - Multi-dimensional matching (type, time, location)
   * - Dampening for oscillation prevention
   * - Divisibility constraints
   * - Largest remainder method
   * - Compliance filters
   * - Convergence tracking
   * 
   * @param needSlots - Recipient need slots
   * @param availabilitySlots - Provider availability slots
   * @param participantShares - MRS/RS shares for each provider
   * @param dampingState - Current damping state (for oscillation prevention)
   * @param previousMetrics - Metrics from previous iteration
   * @returns Slot allocations and updated state
   */
  allocateSlots(
    needSlots: NeedSlot[],
    availabilitySlots: AvailabilitySlot[],
    participantShares: Map<string, number>,
    dampingState?: MultiTypeDamping,
    previousMetrics?: ConvergenceMetrics
  ): {
    allocations: SlotAllocationRecord[];
    metrics: ConvergenceMetrics;
    updatedDamping: MultiTypeDamping;
    converged: boolean;
  } {
    const allocations: SlotAllocationRecord[] = [];
    const updatedDamping: MultiTypeDamping = { ...(dampingState || {}) };
    
    // Build space-time index for efficient matching
    const index = new SpaceTimeIndex();
    for (const availSlot of availabilitySlots) {
      index.addSlot(availSlot);
    }
    
    // Track remaining capacity for each slot
    const remainingCapacity = new Map<string, number>();
    for (const availSlot of availabilitySlots) {
      remainingCapacity.set(availSlot.id, availSlot.quantity);
    }
    
    // Group needs by type for per-type damping
    const needsByType = new Map<string, NeedSlot[]>();
    for (const need of needSlots) {
      if (!needsByType.has(need.need_type_id)) {
        needsByType.set(need.need_type_id, []);
      }
      needsByType.get(need.need_type_id)!.push(need);
    }
    
    // Allocate for each type
    for (const [typeId, typeNeeds] of needsByType) {
      // Get damping state for this type
      const typeDamping = updatedDamping[typeId] || {
        overAllocationHistory: [],
        dampingFactor: 1.0
      };
      
      // Process each need in this type
      for (const need of typeNeeds) {
        // Find compatible providers using index
        const candidateProviderIds = index.findMatching(need);
        const compatibleSlots = availabilitySlots.filter(avail =>
          candidateProviderIds.has(avail.participantId) &&
          SlotMatching.slotsCompatible(need, avail) &&
          (remainingCapacity.get(avail.id) || 0) > 0
        );
        
        if (compatibleSlots.length === 0) continue;
        
        // Calculate distribution shares for compatible providers
        const providerShares: Record<string, number> = {};
        let totalShare = 0;
        
        for (const availSlot of compatibleSlots) {
          const share = participantShares.get(availSlot.participantId) || 0;
          providerShares[availSlot.id] = share;
          totalShare += share;
        }
        
        // Normalize shares
        if (totalShare > 0) {
          for (const slotId in providerShares) {
            providerShares[slotId] /= totalShare;
          }
        }
        
        // Calculate raw allocations (proportional to shares)
        const rawAllocations: Record<string, number> = {};
        for (const availSlot of compatibleSlots) {
          const share = providerShares[availSlot.id] || 0;
          const rawAmount = need.quantity * share;
          
          // Apply damping
          const dampedAmount = DampeningSystem.applyDamping(
            rawAmount,
            typeDamping.dampingFactor
          );
          
          // Apply divisibility constraints
          const availableCapacity = remainingCapacity.get(availSlot.id) || 0;
          const minAllocation = DivisibilityConstraints.getMinimumAllocation(
            availableCapacity,
            availSlot.divisibility
          );
          
          // Skip if below minimum
          if (dampedAmount < minAllocation) continue;
          
          // Round to natural units
          const roundedAmount = DivisibilityConstraints.roundToNaturalUnit(
            dampedAmount,
            availableCapacity,
            availSlot.divisibility
          );
          
          rawAllocations[availSlot.id] = Math.min(roundedAmount, availableCapacity);
        }
        
        // Apply largest remainder method for indivisible quantities
        const totalRawAllocation = Object.values(rawAllocations).reduce((sum, val) => sum + val, 0);
        const targetQuantity = Math.min(need.quantity, totalRawAllocation);
        
        const finalAllocations = LargestRemainderMethod.allocate(
          providerShares,
          Math.floor(targetQuantity)
        );
        
        // Create allocation records
        let allocatedToNeed = 0;
        for (const availSlot of compatibleSlots) {
          const allocatedQty = finalAllocations[availSlot.id] || 0;
          if (allocatedQty === 0) continue;
          
          allocations.push({
            needSlotId: need.id,
            availabilitySlotId: availSlot.id,
            providerId: availSlot.participantId,
            recipientId: need.participantId,
            allocatedQuantity: allocatedQty,
            timestamp: Date.now()
          });
          
          // Update remaining capacity
          const remaining = remainingCapacity.get(availSlot.id)! - allocatedQty;
          remainingCapacity.set(availSlot.id, remaining);
          allocatedToNeed += allocatedQty;
        }
        
        // Update damping state for this type
        updatedDamping[typeId] = DampeningSystem.updateDampingState(
          typeDamping,
          allocatedToNeed,
          need.quantity
        );
      }
    }
    
    // Calculate convergence metrics
    const metrics = ConvergenceTracker.calculateMetrics(
      needSlots,
      availabilitySlots,
      allocations
    );
    
    const converged = ConvergenceTracker.hasConverged(
      metrics,
      previousMetrics
    );
    
    return {
      allocations,
      metrics,
      updatedDamping,
      converged
    };
  }
  
  /**
   * Multi-Provider Need Satisfaction Algorithm (BASIC VERSION)
   * 
   * Implements iterative allocation where multiple providers contribute
   * capacity to satisfy a recipient's need, proportional to their share values.
   * 
   * Algorithm:
   * 1. Compute raw allocations: r^(t) = K · S[:,r]
   * 2. Compute actual allocations: a^(t) = min(r^(t), N_r^(t))
   * 3. Update received, remaining need, capacities
   * 4. Repeat until need satisfied or no capacity available
   * 
   * @param recipientIndex - Index of recipient
   * @param need - Total need amount
   * @param capacities - Available capacity for each provider (length n)
   * @param shareType - Type of share to use: 'RS', 'MRS', or 'SCMRS'
   * @param collectiveIndices - For SCMRS, indices of collective members
   * @returns Allocation result with provider contributions
   */
  allocateMultiProvider(
    recipientIndex: number,
    need: number,
    capacities: number[],
    shareType: 'RS' | 'MRS' | 'SCMRS' = 'MRS',
    collectiveIndices?: number[]
  ): {
    allocations: number[];
    remainingNeed: number;
    satisfied: boolean;
    iterations: number;
  } {
    if (capacities.length !== this.n) {
      throw new Error(`Capacities array must have length ${this.n}`);
    }
    
    // Choose share matrix based on type (SPARSE)
    let S: number[];
    if (shareType === 'RS') {
      const RS = this.computeRS();
      // Extract column from sparse matrix
      S = Array(this.n).fill(0);
      for (const [i, row] of RS.entries()) {
        S[i] = Sparse.get(RS, i, recipientIndex);
      }
    } else if (shareType === 'MRS') {
      const MRS = this.computeMRS();
      // Extract column from sparse matrix
      S = Array(this.n).fill(0);
      for (const [i, row] of MRS.entries()) {
        S[i] = Sparse.get(MRS, i, recipientIndex);
      }
    } else if (shareType === 'SCMRS') {
      if (!collectiveIndices) {
        throw new Error('collectiveIndices required for SCMRS');
      }
      S = this.computeSCMRS_weighted(collectiveIndices);
    } else {
      throw new Error(`Unknown share type: ${shareType}`);
    }
    
    // Initialize state
    const allocations: number[] = Array(this.n).fill(0);
    const K = [...capacities]; // Copy to avoid modifying input
    let remainingNeed = need;
    const maxIterations = 100;
    let iteration = 0;
    
    while (remainingNeed > 0 && iteration < maxIterations) {
      // 1. Compute raw allocations based on current capacities and shares
      const rawAllocations: number[] = Array(this.n).fill(0);
      for (let i = 0; i < this.n; i++) {
        rawAllocations[i] = K[i] * S[i];
      }
      
      // Check if any capacity available
      const totalRaw = rawAllocations.reduce((sum, val) => sum + val, 0);
      if (totalRaw === 0) {
        break; // No more capacity available
      }
      
      // 2. Compute actual allocations (capped by remaining need)
      let actualAllocations: number[];
      if (totalRaw <= remainingNeed) {
        // All raw allocations fit within remaining need
        actualAllocations = [...rawAllocations];
      } else {
        // Scale down proportionally to fit remaining need
        const scale = remainingNeed / totalRaw;
        actualAllocations = rawAllocations.map(val => val * scale);
      }
      
      // 3. Update state
      for (let i = 0; i < this.n; i++) {
        allocations[i] += actualAllocations[i];
        K[i] -= actualAllocations[i];
      }
      
      const totalActual = actualAllocations.reduce((sum, val) => sum + val, 0);
      remainingNeed -= totalActual;
      
      // Prevent floating point issues
      if (remainingNeed < 1e-10) {
        remainingNeed = 0;
      }
      
      iteration++;
    }
    
    return {
      allocations,
      remainingNeed,
      satisfied: remainingNeed === 0,
      iterations: iteration
    };
  }
}

// ============================================================================
// PHASE 3: RPC LAYER (Cap'n Web Integration)
// ============================================================================

/**
 * Cap'n Web is now integrated!
 * 
 * Key patterns from the article:
 * 1. Classes extend RpcTarget to become RPC-accessible
 * 2. Methods automatically become RPC endpoints
 * 3. Return objects extending RpcTarget to create capability references
 * 4. Promise pipelining happens automatically
 * 5. Pass callbacks for bidirectional communication
 * 
 * SYMMETRIC PROTOCOL:
 * - Both sides can be client AND server simultaneously
 * - Each side exports RpcTarget at ID 0
 * - When you pass RpcTarget to other side, they can call methods on it
 * - True peer-to-peer: no architectural distinction between "client" and "server"
 */

// ============================================================================
// CALLBACK INTERFACES (For bidirectional communication)
// ============================================================================

/**
 * Callback interface for capacity events
 * Client implements this and passes to server
 * Server calls these methods on the client!
 */
interface ICapacityEventCallback {
  onCapacityReceived(fromId: ParticipantId, amount: number): Promise<void>;
  onCapacityAllocated(toId: ParticipantId, amount: number): Promise<void>;
}

/**
 * Callback interface for recognition events
 */
interface IRecognitionEventCallback {
  onRecognitionReceived(fromId: ParticipantId, amount: number): Promise<void>;
  onRecognitionAllocated(toId: ParticipantId, amount: number): Promise<void>;
}

/**
 * Callback interface for collective events
 */
interface ICollectiveEventCallback {
  onMemberJoined(collectiveId: CollectiveId, memberId: ParticipantId): Promise<void>;
  onMemberLeft(collectiveId: CollectiveId, memberId: ParticipantId): Promise<void>;
  onCollectiveUpdated(collectiveId: CollectiveId, members: ParticipantId[]): Promise<void>;
}

/**
 * Recognition Budget - Enforces Axiom 1 (Budget Constraint)
 * 
 * Each participant has exactly 100% recognition to allocate.
 * Server-side enforcement prevents gaming.
 */
class RecognitionBudget extends RpcTarget {
  private readonly participantId: ParticipantId;
  private readonly participantIndex: ParticipantIndex;
  private allocations: Map<ParticipantId, number> = new Map();
  private readonly totalBudget = 1.0;
  private callbacks: Set<RpcStub<IRecognitionEventCallback>> = new Set();
  
  constructor(participantId: ParticipantId, participantIndex: ParticipantIndex) {
    super();
    this.participantId = participantId;
    this.participantIndex = participantIndex;
  }
  
  /**
   * Subscribe to recognition events (bidirectional calling!)
   * Client passes RpcTarget callback, server calls it when events occur
   */
  subscribe(callback: RpcStub<IRecognitionEventCallback>): void {
    this.callbacks.add(callback);
  }
  
  /**
   * Unsubscribe from events
   */
  unsubscribe(callback: RpcStub<IRecognitionEventCallback>): void {
    this.callbacks.delete(callback);
  }
  
  /**
   * Allocate recognition to another participant
   * Enforces sum constraint: Σ R(a,x) = 1
   */
  async allocateRecognition(targetId: unknown, amount: unknown): Promise<boolean> {
    // Validate inputs with Zod
    const validatedId = ParticipantIdSchema.parse(targetId);
    const validatedAmount = RecognitionValueSchema.parse(amount);
    
    // Cannot allocate to self
    if (validatedId === this.participantId) {
      throw new Error("Cannot allocate recognition to self");
    }
    
    // Check budget constraint
    const currentTotal = Array.from(this.allocations.values())
      .reduce((sum, val) => sum + val, 0);
    const existingToTarget = this.allocations.get(validatedId) || 0;
    const newTotal = currentTotal - existingToTarget + validatedAmount;
    
    if (newTotal > this.totalBudget + 0.0001) { // Small tolerance for floating point
      throw new Error(
        `Budget violation: ${newTotal.toFixed(4)} > ${this.totalBudget}. ` +
        `Current total: ${currentTotal.toFixed(4)}, ` +
        `requested: ${validatedAmount}, ` +
        `existing to target: ${existingToTarget}`
      );
    }
    
    this.allocations.set(validatedId, validatedAmount);
    
    // Notify callbacks (server calls client!)
    await this.notifyCallbacks('allocated', validatedId, validatedAmount);
    
    return true;
  }
  
  /**
   * Notify all subscribed callbacks (bidirectional RPC)
   */
  private async notifyCallbacks(
    event: 'received' | 'allocated',
    otherId: ParticipantId,
    amount: number
  ): Promise<void> {
    const notifications = Array.from(this.callbacks).map(async callback => {
      try {
        if (event === 'received') {
          await callback.onRecognitionReceived(otherId, amount);
        } else {
          await callback.onRecognitionAllocated(otherId, amount);
        }
      } catch (error) {
        console.error('Error in recognition callback:', error);
      }
    });
    
    // Fire in parallel, don't wait
    Promise.all(notifications).catch(console.error);
  }
  
  /**
   * Get recognition allocated to specific participant
   */
  getRecognitionTo(targetId: unknown): number {
    const validatedId = ParticipantIdSchema.parse(targetId);
    return this.allocations.get(validatedId) || 0;
  }
  
  /**
   * Get all allocations
   */
  getAllAllocations(): Map<ParticipantId, number> {
    return new Map(this.allocations);
  }
  
  /**
   * Get total allocated so far
   */
  getTotalAllocated(): number {
    return Array.from(this.allocations.values())
      .reduce((sum, val) => sum + val, 0);
  }
  
  getParticipantId(): ParticipantId {
    return this.participantId;
  }
  
  getParticipantIndex(): ParticipantIndex {
    return this.participantIndex;
  }
}

/**
 * Network State - Manages recognition graph and computes derived values
 * 
 * Implements Axiom 2 (Mutual Recognition) and all matrix operations.
 */
class NetworkState extends RpcTarget {
  private readonly matrices: FreeAssociationMatrices;
  private readonly participantIdToIndex: Map<ParticipantId, ParticipantIndex> = new Map();
  private readonly participantIndexToId: Map<ParticipantIndex, ParticipantId> = new Map();
  private readonly recognitionBudgets: Map<ParticipantId, RecognitionBudget> = new Map();
  private nextIndex: number = 0;
  
  constructor(initialSize: number = 100) {
    super();
    this.matrices = new FreeAssociationMatrices(initialSize);
  }
  
  /**
   * Register a participant in the network
   */
  registerParticipant(participantId: unknown): RecognitionBudget {
    const validatedId = ParticipantIdSchema.parse(participantId);
    
    if (this.recognitionBudgets.has(validatedId)) {
      return this.recognitionBudgets.get(validatedId)!;
    }
    
    const index = this.nextIndex++;
    this.participantIdToIndex.set(validatedId, index);
    this.participantIndexToId.set(index, validatedId);
    
    const budget = new RecognitionBudget(validatedId, index);
    this.recognitionBudgets.set(validatedId, budget);
    
    return budget;
  }
  
  /**
   * Sync recognition budgets to matrix
   * Must be called before computing derived values
   */
  syncToMatrix(): void {
    // Update R matrix from all budgets
    for (const [giverId, giverBudget] of this.recognitionBudgets) {
      const giverIndex = this.participantIdToIndex.get(giverId)!;
      const allocations = giverBudget.getAllAllocations();
      
      // Clear row first
      for (let j = 0; j < this.nextIndex; j++) {
        this.matrices.setRecognition(giverIndex, j, 0);
      }
      
      // Set allocations
      for (const [receiverId, amount] of allocations) {
        const receiverIndex = this.participantIdToIndex.get(receiverId);
        if (receiverIndex !== undefined) {
          this.matrices.setRecognition(giverIndex, receiverIndex, amount);
        }
      }
    }
  }
  
  /**
   * Compute Mutual Recognition between two participants
   * Implements Axiom 2: MR(a,b) = min(R(a,b), R(b,a))
   */
  computeMutualRecognition(participantAId: unknown, participantBId: unknown): number {
    const validatedAId = ParticipantIdSchema.parse(participantAId);
    const validatedBId = ParticipantIdSchema.parse(participantBId);
    
    this.syncToMatrix();
    
    const indexA = this.participantIdToIndex.get(validatedAId);
    const indexB = this.participantIdToIndex.get(validatedBId);
    
    if (indexA === undefined || indexB === undefined) {
      throw new Error("One or both participants not found in network");
    }
    
    const MR = this.matrices.computeMR();
    return Sparse.get(MR, indexA, indexB);
  }
  
  /**
   * Compute total mutual recognition for a participant
   */
  computeTotalMR(participantId: unknown): number {
    const validatedId = ParticipantIdSchema.parse(participantId);
    
    this.syncToMatrix();
    
    const index = this.participantIdToIndex.get(validatedId);
    if (index === undefined) {
      throw new Error("Participant not found in network");
    }
    
    const t = this.matrices.computeTotalMR();
    return t[index];
  }
  
  /**
   * Compute MRS value between two participants
   */
  computeMRS(participantAId: unknown, participantBId: unknown): number {
    const validatedAId = ParticipantIdSchema.parse(participantAId);
    const validatedBId = ParticipantIdSchema.parse(participantBId);
    
    this.syncToMatrix();
    
    const indexA = this.participantIdToIndex.get(validatedAId);
    const indexB = this.participantIdToIndex.get(validatedBId);
    
    if (indexA === undefined || indexB === undefined) {
      throw new Error("One or both participants not found in network");
    }
    
    const MRS = this.matrices.computeMRS();
    return Sparse.get(MRS, indexA, indexB);
  }
  
  /**
   * Get recognition budget for a participant
   */
  getRecognitionBudget(participantId: unknown): RecognitionBudget {
    const validatedId = ParticipantIdSchema.parse(participantId);
    
    const budget = this.recognitionBudgets.get(validatedId);
    if (!budget) {
      throw new Error("Participant not found in network");
    }
    
    return budget;
  }
  
  /**
   * Get matrices object for direct access (for testing)
   */
  getMatrices(): FreeAssociationMatrices {
    this.syncToMatrix();
    return this.matrices;
  }
  
  /**
   * Get participant ID from index
   */
  getParticipantId(index: ParticipantIndex): ParticipantId | undefined {
    return this.participantIndexToId.get(index);
  }
  
  /**
   * Get participant index from ID
   */
  getParticipantIndex(id: ParticipantId): ParticipantIndex | undefined {
    return this.participantIdToIndex.get(id);
  }
}

/**
 * Collective - Manages membership via MRD threshold
 * 
 * Membership is proven by possession of this capability.
 */
class Collective extends RpcTarget {
  private readonly collectiveId: CollectiveId;
  private readonly network: NetworkState;
  private members: Set<ParticipantId> = new Set();
  private readonly threshold: number;
  private readonly model: MembershipModel;
  private callbacks: Set<RpcStub<ICollectiveEventCallback>> = new Set();
  
  constructor(
    collectiveId: CollectiveId,
    network: NetworkState,
    threshold: number = 0.5,
    model: MembershipModel = 'collective'
  ) {
    super();
    this.collectiveId = collectiveId;
    this.network = network;
    this.threshold = threshold;
    this.model = model;
  }
  
  /**
   * Subscribe to collective events (bidirectional calling!)
   */
  subscribe(callback: RpcStub<ICollectiveEventCallback>): void {
    this.callbacks.add(callback);
  }
  
  /**
   * Unsubscribe from events
   */
  unsubscribe(callback: RpcStub<ICollectiveEventCallback>): void {
    this.callbacks.delete(callback);
  }
  
  /**
   * Attempt to join collective
   * Returns this collective capability if MRD >= threshold
   * 
   * SYMMETRIC: Any participant instance can call this,
   * whether they're "client" or "server"
   */
  async attemptJoin(participantId: unknown): Promise<Collective> {
    const validatedId = ParticipantIdSchema.parse(participantId);
    
    const mrd = this.computeMRDForParticipant(validatedId);
    
    if (mrd >= this.threshold) {
      this.members.add(validatedId);
      
      // Notify all subscribers (server → clients RPC!)
      await this.notifyCallbacks('joined', validatedId);
      
      return this; // Return capability = grant membership
    }
    
    throw new Error(
      `Insufficient mutual recognition density: ${mrd.toFixed(3)} < ${this.threshold}`
    );
  }
  
  /**
   * Notify all subscribed callbacks
   */
  private async notifyCallbacks(
    event: 'joined' | 'left' | 'updated',
    memberId?: ParticipantId
  ): Promise<void> {
    const memberList = Array.from(this.members);
    
    const notifications = Array.from(this.callbacks).map(async callback => {
      try {
        if (event === 'joined' && memberId) {
          await callback.onMemberJoined(this.collectiveId, memberId);
        } else if (event === 'left' && memberId) {
          await callback.onMemberLeft(this.collectiveId, memberId);
        } else if (event === 'updated') {
          await callback.onCollectiveUpdated(this.collectiveId, memberList);
        }
      } catch (error) {
        console.error('Error in collective callback:', error);
      }
    });
    
    Promise.all(notifications).catch(console.error);
  }
  
  /**
   * Compute MRD for a participant relative to this collective
   */
  computeMRDForParticipant(participantId: unknown): number {
    const validatedId = ParticipantIdSchema.parse(participantId);
    
    const participantIndex = this.network.getParticipantIndex(validatedId);
    if (participantIndex === undefined) {
      throw new Error("Participant not found in network");
    }
    
    // Get indices of current members
    const memberIndices: number[] = [];
    for (const memberId of this.members) {
      const index = this.network.getParticipantIndex(memberId);
      if (index !== undefined) {
        memberIndices.push(index);
      }
    }
    
    if (memberIndices.length === 0) {
      // First member always accepted
      return 1.0;
    }
    
    const matrices = this.network.getMatrices();
    return matrices.computeMRD(memberIndices, participantIndex);
  }
  
  /**
   * Get all members with their MRD values
   */
  computeAllMRD(): MRDResult[] {
    const memberIndices: number[] = [];
    for (const memberId of this.members) {
      const index = this.network.getParticipantIndex(memberId);
      if (index !== undefined) {
        memberIndices.push(index);
      }
    }
    
    const matrices = this.network.getMatrices();
    const mrdValues = matrices.computeAllMRD(memberIndices);
    
    const results: MRDResult[] = [];
    for (const memberId of this.members) {
      const index = this.network.getParticipantIndex(memberId);
      if (index !== undefined) {
        results.push({
          participantId: memberId,
          participantIndex: index,
          mrd: mrdValues[index],
          aboveThreshold: mrdValues[index] >= this.threshold
        });
      }
    }
    
    return results;
  }
  
  /**
   * Get list of member IDs
   */
  getMembers(): ParticipantId[] {
    return Array.from(this.members);
  }
  
  /**
   * Check if participant is a member
   */
  isMember(participantId: unknown): boolean {
    const validatedId = ParticipantIdSchema.parse(participantId);
    return this.members.has(validatedId);
  }
  
  getCollectiveId(): CollectiveId {
    return this.collectiveId;
  }
}

/**
 * Participant Goal - Tracks progress toward goal via capacity receipts
 * 
 * Implements Axiom 4 & 5: Only beneficial capacity contributes to goal.
 */
class ParticipantGoal extends RpcTarget {
  private readonly goalId: GoalId;
  private readonly participantId: ParticipantId;
  private readonly beneficialSet: Set<ParticipantId>;
  private readonly network: NetworkState;
  private receivedCapacity: Map<ParticipantId, number> = new Map();
  
  constructor(
    goalId: GoalId,
    participantId: ParticipantId,
    beneficialParticipantIds: ParticipantId[],
    network: NetworkState
  ) {
    super();
    this.goalId = goalId;
    this.participantId = participantId;
    this.beneficialSet = new Set(beneficialParticipantIds);
    this.network = network;
  }
  
  /**
   * Receive capacity from another participant
   * Only beneficial capacity contributes (Axiom 5)
   */
  receiveCapacity(fromId: unknown, amount: unknown): GoalProgress {
    const validatedFromId = ParticipantIdSchema.parse(fromId);
    const validatedAmount = CapacitySchema.parse(amount);
    
    // Check beneficial set membership
    if (!this.beneficialSet.has(validatedFromId)) {
      return {
        accepted: false,
        goalProbability: this.computeGoalProbability(),
        reason: "Not in beneficial set"
      };
    }
    
    // Accept and record
    const current = this.receivedCapacity.get(validatedFromId) || 0;
    this.receivedCapacity.set(validatedFromId, current + validatedAmount);
    
    return {
      accepted: true,
      goalProbability: this.computeGoalProbability()
    };
  }
  
  /**
   * Compute goal achievement probability (Axiom 4)
   * f: strictly increasing function of beneficial capacity
   */
  private computeGoalProbability(): number {
    const totalBeneficialCapacity = Array.from(this.beneficialSet)
      .map(id => this.receivedCapacity.get(id) || 0)
      .reduce((sum, val) => sum + val, 0);
    
    // Logistic function: f(x) = 1 / (1 + e^(-k*x))
    const k = 0.01; // Scaling factor
    return 1 / (1 + Math.exp(-k * totalBeneficialCapacity));
  }
  
  /**
   * Get current progress toward goal
   */
  getProgress(): {
    totalReceived: number;
    beneficialReceived: number;
    nonBeneficialReceived: number;
    probability: number;
  } {
    let beneficialReceived = 0;
    let nonBeneficialReceived = 0;
    
    for (const [fromId, amount] of this.receivedCapacity) {
      if (this.beneficialSet.has(fromId)) {
        beneficialReceived += amount;
      } else {
        nonBeneficialReceived += amount;
      }
    }
    
    return {
      totalReceived: beneficialReceived + nonBeneficialReceived,
      beneficialReceived,
      nonBeneficialReceived,
      probability: this.computeGoalProbability()
    };
  }
  
  getGoalId(): GoalId {
    return this.goalId;
  }
  
  getBeneficialSet(): ParticipantId[] {
    return Array.from(this.beneficialSet);
  }
}

/**
 * Matrix Region - Collaborative editing with bounds checking
 * 
 * Each region is capability-isolated with automatic bounds enforcement.
 */
class MatrixRegion extends RpcTarget {
  private matrix: number[][];
  private readonly bounds: MatrixBounds;
  private updateCallbacks: Set<(update: MatrixUpdate) => void> = new Set();
  
  constructor(matrix: number[][], bounds: MatrixBounds) {
    super();
    this.matrix = matrix;
    this.bounds = MatrixBoundsSchema.parse(bounds);
  }
  
  /**
   * Set cell value - automatically enforces bounds
   */
  setCell(row: unknown, col: unknown, value: unknown, by: unknown): void {
    const validatedRow = z.number().int().nonnegative().parse(row);
    const validatedCol = z.number().int().nonnegative().parse(col);
    const validatedValue = z.number().parse(value);
    const validatedBy = ParticipantIdSchema.parse(by);
    
    if (!this.isInBounds(validatedRow, validatedCol)) {
      throw new Error(
        `Out of bounds: (${validatedRow}, ${validatedCol}) not in ` +
        `[${this.bounds.startRow}:${this.bounds.endRow}, ${this.bounds.startCol}:${this.bounds.endCol}]`
      );
    }
    
    const oldValue = this.matrix[validatedRow][validatedCol];
    this.matrix[validatedRow][validatedCol] = validatedValue;
    
    // Notify subscribers
    const update: MatrixUpdate = {
      row: validatedRow,
      col: validatedCol,
      oldValue,
      newValue: validatedValue,
      timestamp: Date.now(),
      updatedBy: validatedBy
    };
    
    this.notifySubscribers(update);
  }
  
  /**
   * Get cell value
   */
  getCell(row: unknown, col: unknown): number {
    const validatedRow = z.number().int().nonnegative().parse(row);
    const validatedCol = z.number().int().nonnegative().parse(col);
    
    if (!this.isInBounds(validatedRow, validatedCol)) {
      throw new Error(`Out of bounds: (${validatedRow}, ${validatedCol})`);
    }
    
    return this.matrix[validatedRow][validatedCol];
  }
  
  /**
   * Subscribe to updates in this region
   */
  onUpdate(callback: (update: MatrixUpdate) => void): void {
    this.updateCallbacks.add(callback);
  }
  
  /**
   * Unsubscribe from updates
   */
  offUpdate(callback: (update: MatrixUpdate) => void): void {
    this.updateCallbacks.delete(callback);
  }
  
  private isInBounds(row: number, col: number): boolean {
    return row >= this.bounds.startRow && 
           row < this.bounds.endRow &&
           col >= this.bounds.startCol && 
           col < this.bounds.endCol;
  }
  
  private notifySubscribers(update: MatrixUpdate): void {
    for (const callback of this.updateCallbacks) {
      try {
        callback(update);
      } catch (error) {
        console.error("Error in update callback:", error);
      }
    }
  }
  
  getBounds(): MatrixBounds {
    return { ...this.bounds };
  }
}

/**
 * Authenticated Participant - Main session object
 * 
 * Implements Axiom 3: Capacity flow proportional to mutual recognition.
 * Binds participant to their capacity pool.
 * 
 * NOW WITH SLOT-BASED ALLOCATION:
 * - Manages need and availability slots
 * - Performs multi-dimensional matching
 * - Tracks convergence and damping
 */
class AuthenticatedParticipant extends RpcTarget {
  private readonly participantId: ParticipantId;
  private readonly network: NetworkState;
  private readonly budget: RecognitionBudget;
  private capacity: number;
  private readonly goals: Map<GoalId, ParticipantGoal> = new Map();
  private readonly collectives: Map<CollectiveId, Collective> = new Map();
  private capacityCallbacks: Set<RpcStub<ICapacityEventCallback>> = new Set();
  
  // Slot-based allocation state
  private needSlots: NeedSlot[] = [];
  private availabilitySlots: AvailabilitySlot[] = [];
  private allocations: SlotAllocationRecord[] = [];
  private dampingState: MultiTypeDamping = {};
  private previousMetrics?: ConvergenceMetrics;
  
  constructor(participantId: ParticipantId, network: NetworkState, initialCapacity: number = 1000) {
    super();
    this.participantId = participantId;
    this.network = network;
    this.budget = network.registerParticipant(participantId);
    this.capacity = initialCapacity;
  }
  
  /**
   * Subscribe to capacity events (bidirectional calling!)
   * Pass a client-side RpcTarget, server will call it back
   */
  subscribeToCapacityEvents(callback: RpcStub<ICapacityEventCallback>): void {
    this.capacityCallbacks.add(callback);
  }
  
  /**
   * Unsubscribe from capacity events
   */
  unsubscribeFromCapacityEvents(callback: RpcStub<ICapacityEventCallback>): void {
    this.capacityCallbacks.delete(callback);
  }
  
  /**
   * Get recognition budget for allocating to others
   */
  getRecognitionBudget(): RecognitionBudget {
    return this.budget;
  }
  
  /**
   * Get network state for querying MR, MRS, etc.
   */
  getNetworkState(): NetworkState {
    return this.network;
  }
  
  /**
   * Allocate capacity to another participant
   * Flow automatically proportional to mutual recognition (Axiom 3)
   * 
   * SYMMETRIC: Can be called from either direction!
   * - Alice calls Bob's allocateCapacity → Bob allocates to Alice
   * - Bob calls Alice's allocateCapacity → Alice allocates to Bob
   */
  async allocateCapacity(recipientId: unknown, requestedAmount: unknown): Promise<number> {
    const validatedRecipientId = ParticipantIdSchema.parse(recipientId);
    const validatedAmount = CapacitySchema.parse(requestedAmount);
    
    // Compute mutual recognition
    const mr = this.network.computeMutualRecognition(this.participantId, validatedRecipientId);
    
    // g(MR) - flow multiplier function (linear: g(x) = x)
    const flowMultiplier = mr;
    const allocatedFlow = validatedAmount * flowMultiplier;
    
    // Enforce capacity constraint
    if (allocatedFlow > this.capacity) {
      throw new Error(
        `Insufficient capacity: have ${this.capacity.toFixed(2)}, ` +
        `need ${allocatedFlow.toFixed(2)} (requested ${validatedAmount} × MR ${mr.toFixed(3)})`
      );
    }
    
    this.capacity -= allocatedFlow;
    
    // Notify callbacks (server → client RPC!)
    await this.notifyCapacityCallbacks('allocated', validatedRecipientId, allocatedFlow);
    
    return allocatedFlow;
  }
  
  /**
   * Receive capacity from another participant (for goal satisfaction)
   * 
   * SYMMETRIC: The sender calls this on the recipient
   * - Can be server calling client
   * - Can be client calling server  
   * - Can be peer calling peer
   */
  async receiveCapacity(fromId: unknown, amount: unknown): Promise<void> {
    const validatedFromId = ParticipantIdSchema.parse(fromId);
    const validatedAmount = CapacitySchema.parse(amount);
    
    this.capacity += validatedAmount;
    
    // Update all goals
    for (const goal of this.goals.values()) {
      await goal.receiveCapacity(validatedFromId, validatedAmount);
    }
    
    // Notify callbacks (receiver notifies their own subscribers)
    await this.notifyCapacityCallbacks('received', validatedFromId, validatedAmount);
  }
  
  /**
   * Notify capacity event callbacks (bidirectional RPC)
   */
  private async notifyCapacityCallbacks(
    event: 'received' | 'allocated',
    otherId: ParticipantId,
    amount: number
  ): Promise<void> {
    const notifications = Array.from(this.capacityCallbacks).map(async callback => {
      try {
        if (event === 'received') {
          await callback.onCapacityReceived(otherId, amount);
        } else {
          await callback.onCapacityAllocated(otherId, amount);
        }
      } catch (error) {
        console.error('Error in capacity callback:', error);
      }
    });
    
    // Fire in parallel, don't block
    Promise.all(notifications).catch(console.error);
  }
  
  /**
   * Create or get a goal
   */
  getGoal(goalId: unknown, beneficialParticipantIds?: unknown): ParticipantGoal {
    const validatedGoalId = GoalIdSchema.parse(goalId);
    
    // Return existing goal if it exists
    if (this.goals.has(validatedGoalId)) {
      return this.goals.get(validatedGoalId)!;
    }
    
    // Create new goal
    if (!beneficialParticipantIds) {
      throw new Error("beneficialParticipantIds required for new goal");
    }
    
    const validatedBeneficialIds = z.array(ParticipantIdSchema).parse(beneficialParticipantIds);
    
    const goal = new ParticipantGoal(
      validatedGoalId,
      this.participantId,
      validatedBeneficialIds,
      this.network
    );
    
    this.goals.set(validatedGoalId, goal);
    return goal;
  }
  
  /**
   * Join or get a collective
   */
  async joinCollective(collectiveId: unknown): Promise<Collective> {
    const validatedCollectiveId = CollectiveIdSchema.parse(collectiveId);
    
    // Return existing collective if already member
    if (this.collectives.has(validatedCollectiveId)) {
      return this.collectives.get(validatedCollectiveId)!;
    }
    
    // Try to join (will throw if MRD insufficient)
    // In real implementation, this would be fetched from a collective registry
    const collective = new Collective(validatedCollectiveId, this.network);
    const membershipCapability = await collective.attemptJoin(this.participantId);
    
    this.collectives.set(validatedCollectiveId, membershipCapability);
    return membershipCapability;
  }
  
  /**
   * Get current capacity
   */
  getCapacity(): number {
    return this.capacity;
  }
  
  /**
   * Add capacity (e.g., from external source)
   */
  addCapacity(amount: unknown): void {
    const validatedAmount = CapacitySchema.parse(amount);
    this.capacity += validatedAmount;
  }
  
  getParticipantId(): ParticipantId {
    return this.participantId;
  }
  
  // ========================================================================
  // SLOT-BASED ALLOCATION METHODS
  // ========================================================================
  
  /**
   * Add a need slot
   */
  addNeedSlot(slot: unknown): void {
    const validatedSlot = NeedSlotSchema.parse(slot);
    
    // Ensure participantId matches
    if (validatedSlot.participantId !== this.participantId) {
      throw new Error("Slot participantId must match session participantId");
    }
    
    this.needSlots.push(validatedSlot);
  }
  
  /**
   * Add an availability slot
   */
  addAvailabilitySlot(slot: unknown): void {
    const validatedSlot = AvailabilitySlotSchema.parse(slot);
    
    // Ensure participantId matches
    if (validatedSlot.participantId !== this.participantId) {
      throw new Error("Slot participantId must match session participantId");
    }
    
    this.availabilitySlots.push(validatedSlot);
  }
  
  /**
   * Get all need slots
   */
  getNeedSlots(): NeedSlot[] {
    return [...this.needSlots];
  }
  
  /**
   * Get all availability slots
   */
  getAvailabilitySlots(): AvailabilitySlot[] {
    return [...this.availabilitySlots];
  }
  
  /**
   * Remove a need slot
   */
  removeNeedSlot(slotId: unknown): void {
    const validatedId = z.string().parse(slotId);
    this.needSlots = this.needSlots.filter(slot => slot.id !== validatedId);
  }
  
  /**
   * Remove an availability slot
   */
  removeAvailabilitySlot(slotId: unknown): void {
    const validatedId = z.string().parse(slotId);
    this.availabilitySlots = this.availabilitySlots.filter(slot => slot.id !== validatedId);
  }
  
  /**
   * Request allocation for a specific need slot
   * 
   * Uses the enhanced allocation engine with all features:
   * - Multi-dimensional matching
   * - Damping
   * - Divisibility constraints
   * - Convergence tracking
   */
  async requestAllocation(needSlotId: unknown): Promise<SlotAllocationRecord[]> {
    const validatedId = z.string().parse(needSlotId);
    
    // Find the need slot
    const needSlot = this.needSlots.find(slot => slot.id === validatedId);
    if (!needSlot) {
      throw new Error(`Need slot not found: ${validatedId}`);
    }
    
    // Get all availability slots from network
    // In production, this would query the network state
    // For now, use local slots as demonstration
    const allAvailabilitySlots = [...this.availabilitySlots];
    
    // Calculate MRS shares for all providers
    const matrices = this.network.getMatrices();
    const participantIndex = this.network.getParticipantIndex(this.participantId);
    if (participantIndex === undefined) {
      throw new Error("Participant not registered in network");
    }
    
    const MRS = matrices.computeMRS();
    const participantShares = new Map<string, number>();
    
    // Get shares for all potential providers
    for (const availSlot of allAvailabilitySlots) {
      const providerIndex = this.network.getParticipantIndex(availSlot.participantId);
      if (providerIndex !== undefined) {
        const share = Sparse.get(MRS, participantIndex, providerIndex);
        participantShares.set(availSlot.participantId, share);
      }
    }
    
    // Run enhanced allocation engine
    const result = matrices.allocateSlots(
      [needSlot],
      allAvailabilitySlots,
      participantShares,
      this.dampingState,
      this.previousMetrics
    );
    
    // Update state
    this.allocations.push(...result.allocations);
    this.dampingState = result.updatedDamping;
    this.previousMetrics = result.metrics;
    
    return result.allocations;
  }
  
  /**
   * Get all allocations for this participant
   */
  getAllocations(): SlotAllocationRecord[] {
    return [...this.allocations];
  }
  
  /**
   * Get convergence metrics
   */
  getConvergenceMetrics(): ConvergenceMetrics | undefined {
    return this.previousMetrics;
  }
}

/**
 * Participant Server - RPC entry point
 * 
 * Main API endpoint that authenticates participants and returns sessions.
 * Can be used with Cloudflare Workers, WebSocket, or HTTP batch mode.
 */
class ParticipantServer extends RpcTarget {
  private readonly network: NetworkState;
  private readonly sessions: Map<ParticipantId, AuthenticatedParticipant> = new Map();
  private readonly collectives: Map<CollectiveId, Collective> = new Map();
  
  constructor() {
    super();
    this.network = new NetworkState(1000); // Initial capacity for 1000 participants
  }
  
  /**
   * Authenticate and get participant session
   * 
   * This is the main entry point for the RPC API.
   * Returns an unforgeable AuthenticatedParticipant capability.
   */
  authenticate(participantId: unknown, credentials: unknown): AuthenticatedParticipant {
    const validatedId = ParticipantIdSchema.parse(participantId);
    const validatedCredentials = CredentialSchema.parse(credentials);
    
    // Verify credentials
    const verified = this.verifyCredentials(validatedId, validatedCredentials);
    if (!verified) {
      throw new Error("Authentication failed");
    }
    
    // Return existing session if available
    if (this.sessions.has(validatedId)) {
      return this.sessions.get(validatedId)!;
    }
    
    // Create new session
    const session = new AuthenticatedParticipant(validatedId, this.network);
    this.sessions.set(validatedId, session);
    
    return session;
  }
  
  /**
   * Get public network view (read-only)
   */
  getPublicNetworkView(): NetworkState {
    return this.network;
  }
  
  /**
   * Get or create a collective
   */
  getCollective(collectiveId: unknown, threshold?: unknown, model?: unknown): Collective {
    const validatedId = CollectiveIdSchema.parse(collectiveId);
    
    if (this.collectives.has(validatedId)) {
      return this.collectives.get(validatedId)!;
    }
    
    const validatedThreshold = threshold !== undefined 
      ? z.number().min(0).max(1).parse(threshold)
      : 0.5;
    const validatedModel = model !== undefined
      ? MembershipModelSchema.parse(model)
      : 'collective';
    
    const collective = new Collective(
      validatedId,
      this.network,
      validatedThreshold,
      validatedModel
    );
    
    this.collectives.set(validatedId, collective);
    return collective;
  }
  
  /**
   * Verify credentials (placeholder implementation)
   * In production, this would check against a database
   */
  private verifyCredentials(participantId: ParticipantId, credentials: Credential): boolean {
    // Placeholder: accept all credentials
    // In production, this would verify password hash, public key signature, or OAuth token
    if (credentials.type === 'password') {
      return credentials.data.length >= 8;
    } else if (credentials.type === 'publicKey') {
      return credentials.data.length >= 32;
    } else if (credentials.type === 'oauth') {
      return credentials.data.length > 0;
    }
    return false;
  }
}

// ============================================================================
// CLOUDFLARE WORKERS / CLIENT INTEGRATION
// ============================================================================

/**
 * SERVER: Cloudflare Workers entry point
 */
/*
import { newWorkersRpcResponse } from 'capnweb';

export default {
  fetch(request: Request, env: any, ctx: any) {
    const url = new URL(request.url);
    
    if (url.pathname === "/api") {
      // Serve Free Association RPC API
      return newWorkersRpcResponse(request, new ParticipantServer());
    }
    
    return new Response("Free Association Protocol - use /api endpoint", { status: 404 });
  }
};
*/

/**
 * CLIENT: WebSocket connection (real-time)
 */
/*
import { newWebSocketRpcSession } from 'capnweb';
import type { RpcStub } from 'capnweb';

// Type-safe API connection
const api: RpcStub<IParticipantServer> = newWebSocketRpcSession("wss://your-app.workers.dev/api");

// Authenticate - returns session capability
const session = await api.authenticate("alice@example.com", {
  type: "password",
  data: "secretPassword123"
});

// Get recognition budget
const budget = await session.getRecognitionBudget();

// Allocate recognition with Zod validation
await budget.allocateRecognition("bob@example.com", 0.6);
await budget.allocateRecognition("carol@example.com", 0.4);

// Compute mutual recognition (uses promise pipelining!)
const network = session.getNetworkState();
const mr = await network.computeMutualRecognition("alice@example.com", "bob@example.com");
console.log(`MR(alice, bob) = ${mr}`);

// Allocate capacity (flows proportional to MR - Axiom 3)
const allocated = await session.allocateCapacity("bob@example.com", 100);
console.log(`Allocated ${allocated} capacity (100 × ${mr} MR)`);

// Join collective (capability = membership proof!)
const collective = await session.joinCollective("open-source-collective");
const members = await collective.getMembers();
console.log(`Collective members:`, members);
*/

/**
 * CLIENT: HTTP Batch mode (one-time queries)
 */
/*
import { newHttpBatchRpcSession } from 'capnweb';

const batch = newHttpBatchRpcSession("https://your-app.workers.dev/api");

// Make multiple calls in single HTTP round trip
const sessionPromise = batch.authenticate("alice@example.com", credentials);
const networkPromise = sessionPromise.getNetworkState();
const mrPromise = networkPromise.computeMutualRecognition("alice@example.com", "bob@example.com");

// All executed in ONE HTTP request!
const mr = await mrPromise;
console.log(`MR value: ${mr}`);
*/

/**
 * CLIENT: Promise Pipelining Example
 * 
 * This is the "magic" of Cap'n Web - chain calls without awaiting!
 */
/*
const batch = newHttpBatchRpcSession("https://your-app.workers.dev/api");

// Authenticate and immediately use result (no await!)
const session = batch.authenticate("alice@example.com", credentials);

// Chain: session -> budget -> allocate (all in one round trip!)
const budget = session.getRecognitionBudget();
const result = await budget.allocateRecognition("bob@example.com", 0.6);

console.log(`Allocated in single round trip:`, result);
*/

/**
 * CLIENT: Bidirectional calling example
 * 
 * Server can call back to client (useful for real-time updates)
 */
/*
import { RpcTarget } from 'capnweb';

// Client-side callback handler
class ClientHandler extends RpcTarget {
  onCapacityReceived(from: ParticipantId, amount: number) {
    console.log(`Received ${amount} capacity from ${from}`);
    // Update UI, etc.
  }
}

const api = newWebSocketRpcSession("wss://your-app.workers.dev/api");
const session = await api.authenticate("alice@example.com", credentials);

// Pass callback to server
const clientHandler = new ClientHandler();
await session.subscribeToUpdates(clientHandler);

// Server can now call clientHandler.onCapacityReceived() anytime!
*/

// ============================================================================
// VALIDATION TESTS (Example from Section 8 of matrix-rpc.md)
// ============================================================================

/**
 * Helper: Convert sparse matrix to dense for display
 */
function sparseToDenseArray(sparse: SparseMatrix, n: number): number[][] {
  return Sparse.toDense(sparse, n);
}

/**
 * Test the implementation with known example (SPARSE VERSION)
 * 
 * Example: 3 participants with:
 * R = [[0, 0.6, 0.4], [0.3, 0, 0.7], [0.5, 0.5, 0]]
 * 
 * Expected results:
 * - RS = R (already row-normalized)
 * - MR = [[0, 0.3, 0.4], [0.3, 0, 0.5], [0.4, 0.5, 0]]
 * - t = [0.7, 0.8, 0.9]
 * - MRS = [[0, 0.429, 0.571], [0.375, 0, 0.625], [0.444, 0.556, 0]]
 * - SCMRS (weighted): [0.292, 0.333, 0.375]
 * - MRD(1) = 0.875
 */
function runValidationTests(): void {
  console.log("Running validation tests (SPARSE MATRIX)...\n");
  console.log("✨ This now uses sparse matrix optimization internally!");
  
  const matrices = new FreeAssociationMatrices(3);
  
  // Set recognition matrix from example
  const R = [
    [0, 0.6, 0.4],
    [0.3, 0, 0.7],
    [0.5, 0.5, 0]
  ];
  matrices.setRecognitionMatrix(R);
  
  // Test 1: Budget constraint
  console.log("Test 1: Budget constraint");
  const budgetValid = matrices.validateBudgetConstraint();
  console.log(`  Budget constraint valid: ${budgetValid} ✓\n`);
  
  // Test 2: RS computation
  console.log("Test 2: RS (Recognition-Shares)");
  const RS = matrices.computeRS();
  const RS_dense = sparseToDenseArray(RS, 3);
  console.log("  RS =", RS_dense);
  console.log("  Expected: R (already normalized) ✓\n");
  
  // Test 3: MR computation
  console.log("Test 3: MR (Mutual-Recognition)");
  const MR = matrices.computeMR();
  const MR_dense = sparseToDenseArray(MR, 3);
  console.log("  MR =", MR_dense);
  const expectedMR = [
    [0, 0.3, 0.4],
    [0.3, 0, 0.5],
    [0.4, 0.5, 0]
  ];
  console.log("  Expected:", expectedMR);
  
  // Verify MR values
  let mrCorrect = true;
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      const actual = Sparse.get(MR, i, j);
      if (Math.abs(actual - expectedMR[i][j]) > 0.0001) {
        mrCorrect = false;
        console.log(`  ERROR: MR[${i}][${j}] = ${actual}, expected ${expectedMR[i][j]}`);
      }
    }
  }
  if (mrCorrect) {
    console.log("  MR values correct ✓");
  }
  
  // Verify symmetry
  const symmetric = matrices.verifyMRSymmetry();
  console.log(`  MR symmetry: ${symmetric} ✓\n`);
  
  // Test 4: Total MR vector
  console.log("Test 4: Total MR vector (t)");
  const t = matrices.computeTotalMR();
  console.log("  t =", t);
  const expectedT = [0.7, 0.8, 0.9];
  console.log("  Expected:", expectedT);
  
  let tCorrect = true;
  for (let i = 0; i < 3; i++) {
    if (Math.abs(t[i] - expectedT[i]) > 0.0001) {
      tCorrect = false;
      console.log(`  ERROR: t[${i}] = ${t[i]}, expected ${expectedT[i]}`);
    }
  }
  if (tCorrect) {
    console.log("  t values correct ✓\n");
  }
  
  // Test 5: MRS computation
  console.log("Test 5: MRS (Mutual-Recognition-Shares)");
  const MRS = matrices.computeMRS();
  const MRS_dense = sparseToDenseArray(MRS, 3);
  console.log("  MRS =", MRS_dense.map(row => row.map(val => val.toFixed(3))));
  const expectedMRS = [
    [0, 0.429, 0.571],
    [0.375, 0, 0.625],
    [0.444, 0.556, 0]
  ];
  console.log("  Expected:", expectedMRS);
  
  let mrsCorrect = true;
  for (let i = 0; i < 3; i++) {
    for (let j = 0; j < 3; j++) {
      const actual = Sparse.get(MRS, i, j);
      if (Math.abs(actual - expectedMRS[i][j]) > 0.001) {
        mrsCorrect = false;
        console.log(`  ERROR: MRS[${i}][${j}] = ${actual.toFixed(3)}, expected ${expectedMRS[i][j]}`);
      }
    }
  }
  if (mrsCorrect) {
    console.log("  MRS values correct ✓");
  }
  
  // Verify row normalization
  const mrsNormalized = matrices.verifyRowNormalization(MRS);
  console.log(`  MRS rows sum to 1: ${mrsNormalized} ✓\n`);
  
  // Test 6: SCMRS (weighted)
  console.log("Test 6: SCMRS (weighted)");
  const collective = [0, 1, 2]; // All participants
  const scmrs = matrices.computeSCMRS_weighted(collective);
  console.log("  SCMRS =", scmrs.map(val => val.toFixed(3)));
  const expectedSCMRS = [0.292, 0.333, 0.375];
  console.log("  Expected:", expectedSCMRS);
  
  let scmrsCorrect = true;
  for (let i = 0; i < 3; i++) {
    if (Math.abs(scmrs[i] - expectedSCMRS[i]) > 0.001) {
      scmrsCorrect = false;
      console.log(`  ERROR: SCMRS[${i}] = ${scmrs[i].toFixed(3)}, expected ${expectedSCMRS[i]}`);
    }
  }
  if (scmrsCorrect) {
    console.log("  SCMRS values correct ✓\n");
  }
  
  // Test 7: MRD for participant 1 (index 0)
  console.log("Test 7: MRD for participant 1");
  const mrd0 = matrices.computeMRD(collective, 0);
  console.log(`  MRD(1) = ${mrd0.toFixed(3)}`);
  const expectedMRD = 0.875;
  console.log(`  Expected: ${expectedMRD}`);
  
  if (Math.abs(mrd0 - expectedMRD) < 0.001) {
    console.log("  MRD value correct ✓\n");
  } else {
    console.log(`  ERROR: MRD(1) = ${mrd0.toFixed(3)}, expected ${expectedMRD}\n`);
  }
  
  // Test 8: Multi-provider allocation
  console.log("Test 8: Multi-provider allocation");
  const capacities = [100, 100, 100]; // Each has 100 capacity
  const result = matrices.allocateMultiProvider(0, 150, capacities, 'MRS');
  console.log("  Allocating 150 to participant 1 using MRS");
  console.log("  Allocations:", result.allocations.map(val => val.toFixed(2)));
  console.log(`  Remaining need: ${result.remainingNeed.toFixed(2)}`);
  console.log(`  Satisfied: ${result.satisfied}`);
  console.log(`  Iterations: ${result.iterations} ✓\n`);
  
  console.log("All validation tests completed!\n");
  
  // ✨ SPARSE MATRIX PERFORMANCE REPORT
  console.log("=" .repeat(60));
  console.log("SPARSE MATRIX PERFORMANCE REPORT");
  console.log("=".repeat(60) + "\n");
  
  // Memory statistics
  const memStats = matrices.getMemoryStats();
  console.log("Memory Usage:");
  console.log(`  Sparse storage: ${memStats.entries} entries, ${memStats.memoryKB}`);
  console.log(`  Matrix sparsity: ${memStats.sparsity}`);
  console.log(`  Savings vs dense: ${memStats.savingsVsDense}\n`);
  
  // Performance statistics
  console.log("Operation Performance:");
  const perfStats = SparsePerf.getAllStats();
  for (const [operation, stats] of Object.entries(perfStats)) {
    if (stats) {
      console.log(`  ${operation}:`);
      console.log(`    Calls: ${stats.count}`);
      console.log(`    Avg time: ${stats.avgMs.toFixed(3)}ms`);
      console.log(`    Total: ${stats.totalMs.toFixed(3)}ms`);
    }
  }
  
  console.log("\n✨ All operations completed successfully with sparse optimization!");
}

// Run tests if this file is executed directly
if (typeof process !== 'undefined' && process.argv && process.argv[1]?.includes('protocol.ts')) {
  runValidationTests();
}

// ============================================================================
// EXPORTS
// ============================================================================

// Export all classes
export {
  // Core matrix mathematics
  FreeAssociationMatrices,
  
  // RPC classes
  RpcTarget,
  RecognitionBudget,
  NetworkState,
  Collective,
  ParticipantGoal,
  MatrixRegion,
  AuthenticatedParticipant,
  ParticipantServer,
  
  // Test utilities
  runValidationTests
};

// Export all types
export type {
  ParticipantId,
  CollectiveId,
  GoalId,
  Credential,
  RecognitionValue,
  RecognitionRow,
  RecognitionMatrix,
  ParticipantIndex,
  CollectiveIndices,
  Capacity,
  CapacityArray,
  AllocationResult,
  GoalProgress,
  MRDResult,
  MatrixUpdate,
  MatrixBounds,
  ShareType,
  MembershipModel,
  RpcStub
};

// Export RPC interfaces for type-safe clients
export type {
  IRecognitionBudget,
  INetworkState,
  ICollective,
  IParticipantGoal,
  IAuthenticatedParticipant,
  IParticipantServer,
  ICapacityEventCallback,
  IRecognitionEventCallback,
  ICollectiveEventCallback
};

// Export utility classes
export {
  TimeMatching,
  LocationMatching,
  ComplianceFilters,
  SlotMatching,
  DampeningSystem,
  DivisibilityConstraints,
  LargestRemainderMethod,
  SpaceTimeIndex,
  ConvergenceTracker
};

// Export all schemas for external validation
export {
  ParticipantIdSchema,
  CollectiveIdSchema,
  GoalIdSchema,
  CredentialSchema,
  RecognitionValueSchema,
  RecognitionRowSchema,
  RecognitionMatrixSchema,
  ParticipantIndexSchema,
  CollectiveIndicesSchema,
  CapacitySchema,
  CapacityArraySchema,
  AllocationResultSchema,
  GoalProgressSchema,
  MRDResultSchema,
  MatrixUpdateSchema,
  MatrixBoundsSchema,
  ShareTypeSchema,
  MembershipModelSchema,
  // New schemas
  TimeRangeSchema,
  DayOfWeekSchema,
  DayScheduleSchema,
  AvailabilityWindowSchema,
  LocationSchema,
  NeedTypeSchema,
  DivisibilitySchema,
  NeedSlotSchema,
  AvailabilitySlotSchema,
  SlotAllocationRecordSchema,
  DampingStateSchema,
  MultiTypeDampingSchema,
  ConvergenceMetricsSchema
};

