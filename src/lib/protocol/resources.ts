import * as z from 'zod';
import type { Id as ITCId, Event as ITCEvent, Stamp as ITCStamp } from '$lib/utils/primitives/itc';

// ═══════════════════════════════════════════════════════════════════
// BASIC TYPES
// ═══════════════════════════════════════════════════════════════════

export const IdSchema = z.string().min(1);
export const NameSchema = z.string().min(1);
export const PointsSchema = z.number().gte(0);
export const PercentageSchema = z.number().gte(0).lte(1);

// ═══════════════════════════════════════════════════════════════════
// SKILLS & CAPABILITIES
// ═══════════════════════════════════════════════════════════════════

// Future: This will likely evolve into a Verifiable Credential (VC) structure
// where the skill claim is cryptographically signed by an issuer.
// For now, it is a self-attested structured data object.
export const SkillSchema = z.object({
    id: z.string().min(1), // URI or unique identifier (e.g. ESCO URI)
    name: z.string().optional(),
    category: z.string().optional(),
    level: z.union([z.number(), z.string()]).optional(),
    // Future VC fields:
    // context: z.array(z.string()).optional(),
    // type: z.array(z.string()).optional(),
    // issuer: z.string().optional(),
    // issuanceDate: z.string().optional(),
    // proof: z.any().optional()
});

export type Skill = z.infer<typeof SkillSchema>;

// ═══════════════════════════════════════════════════════════════════
// ITC CAUSALITY SCHEMAS
// ═══════════════════════════════════════════════════════════════════

// Use passthrough schemas that accept the ITC types directly
export const ITCIdSchema = z.any() as z.ZodType<ITCId>;
export const ITCEventSchema = z.any() as z.ZodType<ITCEvent>;
export const ITCStampSchema = z.any() as z.ZodType<ITCStamp>;

// Re-export the actual ITC types
export type { ITCId, ITCEvent, ITCStamp };

// ═══════════════════════════════════════════════════════════════════
// RESOURCE METADATA
// ═══════════════════════════════════════════════════════════════════

export const ResourceMetadataSchema = z.object({
    name: z.string(),
    emoji: z.optional(z.string()),
    unit: z.optional(z.string()),
    description: z.optional(z.string()),
    resource_type: z.optional(z.string()),
    filter_rule: z.optional(z.nullable(z.any())),
    hidden_until_request_accepted: z.optional(z.boolean())
});

export type ResourceMetadata = z.infer<typeof ResourceMetadataSchema>;

// ═══════════════════════════════════════════════════════════════════
// NEED TYPE SYSTEM
// ═══════════════════════════════════════════════════════════════════

export const ResourceTypeSchema = z.object({
    id: z.string().min(1),
    name: z.string().min(1),
    description: z.string().optional(),
    unit: z.string().default('units'),
    emoji: z.string().optional(),
    substitution_weights: z.record(z.string(), z.number()).optional(),
    complementary_types: z.array(z.string()).optional(),
    complementary_ratios: z.record(z.string(), z.number()).optional()
});

export type ResourceType = z.infer<typeof ResourceTypeSchema>;

// ═══════════════════════════════════════════════════════════════════
// AVAILABILITY WINDOW SYSTEM
// ═══════════════════════════════════════════════════════════════════

export const TimeRangeSchema = z.object({
    start_time: z.string(), // HH:MM format
    end_time: z.string()     // HH:MM format
});

export type TimeRange = z.infer<typeof TimeRangeSchema>;

export const DayOfWeekSchema = z.enum([
    'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'
]);

export type DayOfWeek = z.infer<typeof DayOfWeekSchema>;

export const DayScheduleSchema = z.object({
    days: z.array(DayOfWeekSchema),
    time_ranges: z.array(TimeRangeSchema)
});

export type DaySchedule = z.infer<typeof DayScheduleSchema>;

export const WeekScheduleSchema = z.object({
    weeks: z.array(z.number().int().min(1).max(5)),
    day_schedules: z.array(DayScheduleSchema)
});

export type WeekSchedule = z.infer<typeof WeekScheduleSchema>;

export const MonthScheduleSchema = z.object({
    month: z.number().int().min(1).max(12),
    week_schedules: z.array(WeekScheduleSchema).optional(),
    day_schedules: z.array(DayScheduleSchema).optional(),
    time_ranges: z.array(TimeRangeSchema).optional()
});

export type MonthSchedule = z.infer<typeof MonthScheduleSchema>;

export const AvailabilityWindowSchema = z.object({
    month_schedules: z.array(MonthScheduleSchema).optional(),
    week_schedules: z.array(WeekScheduleSchema).optional(),
    day_schedules: z.array(DayScheduleSchema).optional(),
    time_ranges: z.array(TimeRangeSchema).optional()
});

export type AvailabilityWindow = z.infer<typeof AvailabilityWindowSchema>;

// ═══════════════════════════════════════════════════════════════════
// SLOTS
// ═══════════════════════════════════════════════════════════════════

// proffers have goals? aims, compose needs
// project network - gantt chart
// optimizers 
/*
Identifier
Descriptive label
Activity duration
Early start time
Early finish time
Late start time
Late finish time
Activity float (slack)

Activity Node Labels
Start and finish times are used to determine the critical path of a project. 
Activity float, or slack, time is used in project crashing.
*/

export const BaseSlotSchema = z.object({
    id: z.string().min(1),
    name: z.string(),
    type_id: z.string().min(1),
    emoji: z.string().optional(),
    description: z.string().optional(),

    author: z.string().optional(), // DID of the slot creator
    offerer: z.string().optional(), // ID of Contact/Org author attests is offering

    quantity: z.number().gte(0),
    unit: z.string().optional(),

    // Throughput Constraints
    min_atomic_size: z.number().positive().optional(), // Granularity (e.g. min duration or min qty)
    max_participation: z.number().int().positive().optional(), // Total unique agents allowed (Fan-In)
    max_concurrency: z.number().int().positive().optional(), // Max simultaneous agents (Bandwidth)
    min_calendar_duration: z.number().positive().optional(), // Physics floor (Min total time)

    required_skills: z.array(SkillSchema).optional(), // Skills required by this slot
    filter_rule: z.any().optional(),

    // for collective-slots? // We are currently using this wrong
    // we should remove this from match.ts
    // members: z.array(z.string()).optional(),

    // time constraints
    time_zone: z.string().optional(),
    start_date: z.string().nullable().optional(),
    end_date: z.string().nullable().optional(),
    availability_window: AvailabilityWindowSchema.optional(),
    recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
    advance_notice_hours: z.number().gte(0).optional(),
    booking_window_hours: z.number().gte(0).optional(),

    // space constraints
    search_radius_km: z.number().gte(0).optional(), // for matching
    hidden_until_request_accepted: z.boolean().optional(), // only reveal specific location after provider accepts
    location_type: z.string().optional(),
    longitude: z.number().min(-180).max(180).optional(),
    latitude: z.number().min(-90).max(90).optional(),
    street_address: z.string().optional(),
    city: z.string().optional(),
    state_province: z.string().optional(),
    postal_code: z.string().optional(),
    country: z.string().optional(),
    online_link: z.string().url().or(z.string().length(0)).optional(),
    h3_index: z.string().optional(),
    h3_resolution: z.number().int().min(0).max(15).optional(),

    mutual_agreement_required: z.boolean().default(false).optional(),
    priority: z.number().optional(),

    priority_distribution: z.record(z.string(), z.number().min(0).max(1)).optional(),
});

export type BaseSlot = z.infer<typeof BaseSlotSchema>;

export const AvailabilitySlotSchema = BaseSlotSchema;
export type AvailabilitySlot = z.infer<typeof AvailabilitySlotSchema>;

export const NeedSlotSchema = BaseSlotSchema;
export type NeedSlot = z.infer<typeof NeedSlotSchema>;

// ═══════════════════════════════════════════════════════════════════
// IDENTITY RESOURCES (Contacts, Orgs)
// ═══════════════════════════════════════════════════════════════════

export const ContactSchema = z.object({
    contact_id: z.string(),
    name: z.string(),
    public_key: z.string().optional(),
    emoji: z.string().optional(),
    notes: z.string().optional(),
    skills: z.array(SkillSchema).default([]),
    created_at: z.number().optional(),
    updated_at: z.number().optional(),
});

export type Contact = z.infer<typeof ContactSchema>;

export const ContactsCollectionSchema = z.preprocess(
    (data: any) => {
        if (data && typeof data === 'object') {
            const { _updatedAt, ...rest } = data;
            return rest;
        }
        return data;
    },
    z.record(z.string(), ContactSchema)
);

export type ContactsCollectionData = z.infer<typeof ContactsCollectionSchema>;

export const OrganizationSchema = z.object({
    org_id: z.string(),
    names: z.record(z.string(), z.string()),
    emoji: z.string().optional(),
    description: z.string().optional(),
    skills: z.array(SkillSchema).default([]),
    created_at: z.number().optional(),
    updated_at: z.number().optional(),
});

export type Organization = z.infer<typeof OrganizationSchema>;

export const OrganizationsCollectionSchema = z.record(z.string(), OrganizationSchema);
export type OrganizationsCollection = z.infer<typeof OrganizationsCollectionSchema>;

export const MembersSchema = z.array(z.string());
export type Members = z.infer<typeof MembersSchema>;

// ═══════════════════════════════════════════════════════════════════
// FILTERS & SUBSCRIPTIONS
// ═══════════════════════════════════════════════════════════════════

export const SlotFilterSchema = z.object({
    filter_id: z.string(),
    name: z.string(),
    enabled: z.boolean().default(true),
    applies_to: z.enum(['capacity', 'need', 'both']).default('both'),
    source_pubkeys: z.array(z.string()).optional(),
    type_ids: z.array(z.string()).optional(),
    required_skills: z.array(SkillSchema).optional(),
    must_include_me: z.boolean().optional(),
    must_include_ids: MembersSchema.optional(),
    location_max_distance_km: z.number().optional(),
    min_quantity: z.number().optional(),
    created_at: z.number().optional(),
    updated_at: z.number().optional()
});

export type SlotFilter = z.infer<typeof SlotFilterSchema>;

export const SlotFiltersCollectionSchema = z.record(z.string(), SlotFilterSchema);
export type SlotFiltersCollection = z.infer<typeof SlotFiltersCollectionSchema>;

export const SlotSubscriptionsSchema = z.record(
    z.string(),
    z.object({
        capacity: z.boolean().default(false),
        needs: z.boolean().default(false)
    })
);

export type SlotSubscriptions = z.infer<typeof SlotSubscriptionsSchema>;

// ═══════════════════════════════════════════════════════════════════
// RESOURCE BUNDLE (Store Schema)
// ═══════════════════════════════════════════════════════════════════

export const MyResourcesSchema = z.object({
    need_slots: z.array(NeedSlotSchema).default([]),
    capacity_slots: z.array(AvailabilitySlotSchema).default([])
});

export type MyResources = z.infer<typeof MyResourcesSchema>;


