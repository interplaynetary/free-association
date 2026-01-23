/**
 * Slot System Schemas
 * 
 * Multi-dimensional slot specifications using Zod for validation.
 */

import { z } from 'zod';

// ═══════════════════════════════════════════════════════════════════
// BASIC TYPES
// ═══════════════════════════════════════════════════════════════════

export const PercentageSchema = z.number().min(0).max(1);
export const ParticipantIdSchema = z.string().email();

// ═══════════════════════════════════════════════════════════════════
// TIME SCHEMAS
// ═══════════════════════════════════════════════════════════════════

export const TimeRangeSchema = z.object({
  start_time: z.string().regex(/^\d{2}:\d{2}$/),
  end_time: z.string().regex(/^\d{2}:\d{2}$/)
});

export const DayOfWeekSchema = z.enum([
  'monday', 'tuesday', 'wednesday', 'thursday',
  'friday', 'saturday', 'sunday'
]);

export const DayScheduleSchema = z.object({
  days: z.array(DayOfWeekSchema),
  time_ranges: z.array(TimeRangeSchema)
});

export const AvailabilityWindowSchema = z.object({
  day_schedules: z.array(DayScheduleSchema).optional(),
  time_ranges: z.array(TimeRangeSchema).optional()
});

// ═══════════════════════════════════════════════════════════════════
// LOCATION SCHEMAS
// ═══════════════════════════════════════════════════════════════════

export const LocationSchema = z.object({
  type: z.enum(['physical', 'online', 'hybrid']).optional(),
  longitude: z.number().min(-180).max(180).optional(),
  latitude: z.number().min(-90).max(90).optional(),
  city: z.string().optional(),
  state_province: z.string().optional(),
  country: z.string().optional(),
  online_link: z.string().url().optional()
});

// ═══════════════════════════════════════════════════════════════════
// NEED TYPE & DIVISIBILITY
// ═══════════════════════════════════════════════════════════════════

export const ResourceTypeSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  unit: z.string().default('units'),
  emoji: z.string().optional()
});

export const DivisibilitySchema = z.object({
  max_natural_div: z.number().int().gte(1).optional(),
});

// ═══════════════════════════════════════════════════════════════════
// SLOT SCHEMAS
// ═══════════════════════════════════════════════════════════════════

export const NeedSlotSchema = z.object({
  id: z.string().min(1),
  participantId: ParticipantIdSchema,
  type_id: z.string().min(1),
  quantity: z.number().gte(0),
  name: z.string(),

  // Time
  start_date: z.string().nullable().optional(),
  end_date: z.string().nullable().optional(),
  time_zone: z.string().default('UTC'),
  recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
  availability_window: AvailabilityWindowSchema.optional(),

  // Location
  location: LocationSchema.optional(),

  // Divisibility
  divisibility: DivisibilitySchema.optional(),

  // Compliance
  filter_rule: z.any().nullable().optional(),

  // Priority
  priority: z.number().optional()
});

export const AvailabilitySlotSchema = z.object({
  id: z.string().min(1),
  participantId: ParticipantIdSchema,
  type_id: z.string().min(1),
  quantity: z.number().gte(0),
  name: z.string(),

  // Time
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

// ═══════════════════════════════════════════════════════════════════
// ALLOCATION RECORD
// ═══════════════════════════════════════════════════════════════════

export const SlotAllocationRecordSchema = z.object({
  needSlotId: z.string(),
  availabilitySlotId: z.string(),
  providerId: ParticipantIdSchema,
  recipientId: ParticipantIdSchema,
  allocatedQuantity: z.number().gte(0),
  timestamp: z.number().int().positive()
});

// ═══════════════════════════════════════════════════════════════════
// TYPE INFERENCE
// ═══════════════════════════════════════════════════════════════════

export type Percentage = z.infer<typeof PercentageSchema>;
export type ParticipantId = z.infer<typeof ParticipantIdSchema>;
export type TimeRange = z.infer<typeof TimeRangeSchema>;
export type DayOfWeek = z.infer<typeof DayOfWeekSchema>;
export type DaySchedule = z.infer<typeof DayScheduleSchema>;
export type AvailabilityWindow = z.infer<typeof AvailabilityWindowSchema>;
export type Location = z.infer<typeof LocationSchema>;
export type ResourceType = z.infer<typeof ResourceTypeSchema>;
export type Divisibility = z.infer<typeof DivisibilitySchema>;
export type NeedSlot = z.infer<typeof NeedSlotSchema>;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;
export type SlotAllocationRecord = z.infer<typeof SlotAllocationRecordSchema>;

