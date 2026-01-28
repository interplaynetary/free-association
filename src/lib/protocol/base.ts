import * as z from 'zod';

// Resource Template - Content Addressed, Immutable
export const ResourceTemplate = z.object({
    id: CID.optional(), // Content-addressed ID
    name: z.string(),
    type_id: z.string().min(1),
    description: z.string().optional(),

    // Quantity Specification
    quantity: z.number().gte(0),
    unit: z.string().optional(),

    // Throughput Constraints
    throughput: z.object({
        min_atomic_size: z.number().positive().optional(),
        max_participation: z.number().int().positive().optional(),
        max_concurrency: z.number().int().positive().optional(),
        min_calendar_duration: z.number().positive().optional(),
    }).optional(),

    // Time Constraints
    time_constraints: z.object({
        time_zone: z.string().optional(),
        start_date: z.string().nullable().optional(),
        end_date: z.string().nullable().optional(),
        availability_window: AvailabilityWindowSchema.optional(),
        recurrence: z.enum(['daily', 'weekly', 'monthly', 'yearly']).nullable().optional(),
        advance_notice_hours: z.number().gte(0).optional(),
        booking_window_hours: z.number().gte(0).optional(),
    }).optional(),

    // Space Constraints
    space_constraints: z.object({
        search_radius_km: z.number().gte(0).optional(),
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
        hidden_until_request_accepted: z.boolean().optional(),
    }).optional(),

    // Match Constraints
    match_constraints: z.object({
        required_skills: z.array(SkillSchema).optional(),
        filter_rule: z.any().optional(),
        mutual_agreement_required: z.boolean().default(false),
    }).optional(),
});

export type ResourceTemplate = z.infer<typeof ResourceTemplate>;

// Resource with required ID
export const ResourceTemplateWithId = ResourceTemplate.required({ id: true });
export type ResourceTemplateWithId = z.infer<typeof ResourceTemplateWithId>;

// Generate content-addressed ID for resource template
export async function generateResourceTemplateId(
    resourceData: Omit<ResourceTemplate, 'id'>
): Promise<CID> {
    const normalized = ResourceTemplate.parse(resourceData);
    const { id, ...hashableContent } = normalized;
    const canonical = canonicalize(hashableContent);
    return await sha256Hex(canonical) as CID;
}

// Helper to create a resource with its computed ID
export async function createResourceWithId(
    resourceData: Omit<ResourceTemplate, 'id'>
): Promise<ResourceTemplateWithId> {
    const id = await generateResourceTemplateId(resourceData);
    return { ...resourceData, id } as ResourceTemplateWithId;
}

// Resource Instance - Runtime State
export const ResourceInstance = z.object({
    instance_id: NanoId,
    resource: ResourceTemplateWithId, // Reference to template
    author: z.string(), // DID of creator
    offerer: z.string().optional(), // Who author attests is offering

    // Instance-specific overrides (if needed)
    actual_quantity: z.number().gte(0).optional(), // Actual available vs template quantity

    // Allocation tracking
    allocations: z.array(z.object({
        allocated_to: z.string(), // Instance ID of proffer/resource using this
        quantity_allocated: z.number().gte(0),
        status: z.enum(['reserved', 'confirmed', 'consumed']),
    })).optional(),

    // Metadata
    created_at: z.date(),
    updated_at: z.date(),
    status: z.enum(['available', 'allocated', 'exhausted']).default('available'),
});

export type ResourceInstance = z.infer<typeof ResourceInstance>;


// Now InputResource just references a resource (like InputProffer does)
const InputResource = z.object({
    kind: z.literal('resource'),
    template_id: CID.optional(), // Reference to ResourceTemplate
    instance_id: NanoId.optional(), // Reference to specific ResourceInstance

    // Constraints on how this slot uses the resource
    quantity_needed: z.number().gte(0).optional(), // How much of the resource
    match_override: z.object({
        filter_rule: z.any().optional(), // Additional matching rules
    }).optional(),
});

export const InputDefinition = z.union([
    InputGeneric,
    InputResource, // Now much simpler!
    InputProffer,
]);