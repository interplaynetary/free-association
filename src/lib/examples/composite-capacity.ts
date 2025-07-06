import { z } from 'zod/v4';
import { SlotSchema, ProfferRegistryManager, type Slot, type Proffer } from './experiments/proffer';
import { BaseCapacitySchema, ShareMapSchema, type BaseCapacity, type ShareMap } from '../schema';

// Extended input definition that can reference capacity shares
const CapacityShareInputSchema = z.object({
	type: z.literal('capacity_share'),
	capacityId: z.string(),
	providerId: z.string(),
	requiredQuantity: z.number().gte(0),
	requiredPercentage: z.number().gte(0).lte(1).optional(), // Alternative to quantity
	fallbackCapacityIds: z.array(z.string()).optional() // Fallback capacities if primary unavailable
});

// Union of original input definitions plus capacity share
const CompositeInputDefinitionSchema = z.union([
	z.object({
		type: z.literal('strict_type'),
		options: z.array(z.string())
	}),
	z.object({
		type: z.literal('strict_quantity'),
		amount: z.union([
			z.number(),
			z.object({
				min: z.number(),
				max: z.number()
			})
		])
	}),
	z.object({
		type: z.literal('descriptive'),
		category: z.enum(['image', 'text', 'other']),
		description: z.string()
	}),
	z.object({
		type: z.literal('combination'),
		descriptive: z.object({
			category: z.enum(['image', 'text', 'other']),
			description: z.string()
		}),
		strict: z.union([
			z.object({
				options: z.array(z.string())
			}),
			z.object({
				amount: z.union([
					z.number(),
					z.object({
						min: z.number(),
						max: z.number()
					})
				])
			})
		])
	}),
	z.object({
		type: z.literal('proffer'),
		profferTemplateId: z.string(),
		completionRequirement: z.enum(['draft', 'active', 'completed']).default('completed'),
		allowPartialCompletion: z.boolean().default(false),
		minimumProgress: z.number().min(0).max(100).optional()
	}),
	CapacityShareInputSchema
]);

// Composite slot that can handle capacity shares
const CompositeSlotSchema = SlotSchema.extend({
	inputDefinition: CompositeInputDefinitionSchema
});

// Resolution context for evaluating capacity shares
const CapacityResolutionContextSchema = z.object({
	providerShares: ShareMapSchema, // Current mutual recognition shares
	capacities: z.record(z.string(), BaseCapacitySchema), // Available capacities by ID
	networkCapacities: z.record(z.string(), z.record(z.string(), BaseCapacitySchema)), // Network capacities by provider
	networkShares: z.record(z.string(), z.record(z.string(), z.number())) // Our shares in network capacities
});

// Composite capacity that combines traditional capacity with proffer-style composition
const CompositeCapacitySchema = BaseCapacitySchema.extend({
	// Traditional capacity fields inherited from BaseCapacitySchema

	// Compositional elements
	compositionSlots: z.array(CompositeSlotSchema).optional(),

	// Aggregation rules for combining component capacities
	aggregationRule: z.enum(['sum', 'min', 'max', 'weighted_average']).default('sum'),

	// Weight configuration for weighted_average aggregation
	componentWeights: z.record(z.string(), z.number()).optional(),

	// Derived properties
	isComposite: z.boolean(),
	componentCapacityIds: z.array(z.string()).optional(),

	// Fulfillment tracking
	requiredSlotsFulfilled: z.number().default(0),
	totalRequiredSlots: z.number().default(0),
	compositionProgress: z.number().min(0).max(100).default(0)
});

// Result of resolving a composite capacity
const ResolvedCompositeCapacitySchema = z.object({
	capacity: CompositeCapacitySchema,
	resolvedComponents: z.record(
		z.string(),
		z.object({
			capacityId: z.string(),
			providerId: z.string(),
			ourShare: z.number(),
			availableQuantity: z.number(),
			allocatedQuantity: z.number()
		})
	),
	totalAvailableQuantity: z.number(),
	compositionFeasible: z.boolean(),
	missingRequirements: z.array(z.string())
});

// Export types
export type CapacityShareInput = z.infer<typeof CapacityShareInputSchema>;
export type CompositeInputDefinition = z.infer<typeof CompositeInputDefinitionSchema>;
export type CompositeSlot = z.infer<typeof CompositeSlotSchema>;
export type CompositeCapacity = z.infer<typeof CompositeCapacitySchema>;
export type CapacityResolutionContext = z.infer<typeof CapacityResolutionContextSchema>;
export type ResolvedCompositeCapacity = z.infer<typeof ResolvedCompositeCapacitySchema>;

// Export schemas
export {
	CapacityShareInputSchema,
	CompositeInputDefinitionSchema,
	CompositeSlotSchema,
	CompositeCapacitySchema,
	CapacityResolutionContextSchema,
	ResolvedCompositeCapacitySchema
};

/**
 * Composite Capacity Manager
 * Handles the creation and resolution of composite capacities
 */
export class CompositeCapacityManager {
	constructor(
		private profferRegistry: ProfferRegistryManager,
		private capacityRegistry: Map<string, BaseCapacity> = new Map(),
		private networkCapacityRegistry: Map<string, Map<string, BaseCapacity>> = new Map()
	) {}

	/**
	 * Create a composite capacity from template
	 */
	createCompositeCapacity(
		baseCapacity: Omit<BaseCapacity, 'id'>,
		compositionSlots: CompositeSlot[],
		aggregationRule: 'sum' | 'min' | 'max' | 'weighted_average' = 'sum',
		componentWeights?: Record<string, number>
	): CompositeCapacity {
		const id = crypto.randomUUID();

		const requiredSlots = compositionSlots.filter(
			(slot) => slot.inputDefinition.type === 'capacity_share'
		);

		return {
			...baseCapacity,
			id,
			compositionSlots,
			aggregationRule,
			componentWeights,
			isComposite: true,
			componentCapacityIds: this.extractCapacityIds(compositionSlots),
			requiredSlotsFulfilled: 0,
			totalRequiredSlots: requiredSlots.length,
			compositionProgress: 0
		};
	}

	/**
	 * Resolve a composite capacity against current context
	 */
	resolveCompositeCapacity(
		compositeCapacity: CompositeCapacity,
		context: CapacityResolutionContext
	): ResolvedCompositeCapacity {
		if (!compositeCapacity.isComposite || !compositeCapacity.compositionSlots) {
			return {
				capacity: compositeCapacity,
				resolvedComponents: {},
				totalAvailableQuantity: compositeCapacity.quantity || 0,
				compositionFeasible: true,
				missingRequirements: []
			};
		}

		const resolvedComponents: Record<string, any> = {};
		const missingRequirements: string[] = [];
		let totalAvailableQuantity = 0;

		// Resolve each capacity share slot
		for (const slot of compositeCapacity.compositionSlots) {
			if (slot.inputDefinition.type === 'capacity_share') {
				const resolution = this.resolveCapacityShareSlot(slot.inputDefinition, context);

				if (resolution.success) {
					resolvedComponents[slot.id] = resolution.component;

					// Aggregate quantities based on rule
					switch (compositeCapacity.aggregationRule) {
						case 'sum':
							totalAvailableQuantity += resolution.component.availableQuantity;
							break;
						case 'min':
							totalAvailableQuantity = Math.min(
								totalAvailableQuantity || Infinity,
								resolution.component.availableQuantity
							);
							break;
						case 'max':
							totalAvailableQuantity = Math.max(
								totalAvailableQuantity,
								resolution.component.availableQuantity
							);
							break;
						case 'weighted_average':
							// Handle weighted average (would need more complex logic)
							const weight = compositeCapacity.componentWeights?.[slot.id] || 1;
							totalAvailableQuantity += resolution.component.availableQuantity * weight;
							break;
					}
				} else {
					missingRequirements.push(...resolution.errors);
				}
			}
		}

		const compositionFeasible = missingRequirements.length === 0;
		const progress =
			compositeCapacity.totalRequiredSlots > 0
				? (Object.keys(resolvedComponents).length / compositeCapacity.totalRequiredSlots) * 100
				: 100;

		return {
			capacity: {
				...compositeCapacity,
				quantity: totalAvailableQuantity,
				compositionProgress: progress,
				requiredSlotsFulfilled: Object.keys(resolvedComponents).length
			},
			resolvedComponents,
			totalAvailableQuantity,
			compositionFeasible,
			missingRequirements
		};
	}

	/**
	 * Resolve a single capacity share slot
	 */
	private resolveCapacityShareSlot(
		input: CapacityShareInput,
		context: CapacityResolutionContext
	): { success: boolean; component?: any; errors: string[] } {
		const errors: string[] = [];

		// Try to find the capacity in our own capacities first
		let capacity = context.capacities[input.capacityId];
		let providerId = input.providerId;
		let ourShare = 1.0; // We own 100% of our own capacities

		// If not found, look in network capacities
		if (!capacity) {
			const providerCapacities = context.networkCapacities[input.providerId];
			if (providerCapacities) {
				capacity = providerCapacities[input.capacityId];
				if (capacity) {
					// Get our share in this network capacity
					ourShare = context.networkShares[input.providerId]?.[input.capacityId] || 0;
				}
			}
		}

		if (!capacity) {
			errors.push(`Capacity ${input.capacityId} not found`);
			return { success: false, errors };
		}

		// Calculate available quantity based on our share
		const baseQuantity = capacity.quantity || 0;
		const ourQuantity = Math.round(baseQuantity * ourShare);

		// Apply divisibility constraints
		const maxNatural = capacity.max_natural_div || 1;
		const maxPercent = capacity.max_percentage_div || 1;

		const percentConstrained =
			ourShare > maxPercent ? Math.round(baseQuantity * maxPercent) : ourQuantity;
		const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

		const availableQuantity = naturalConstrained;

		// Check if we have enough for the required quantity
		if (input.requiredQuantity > availableQuantity) {
			errors.push(
				`Insufficient quantity: need ${input.requiredQuantity}, have ${availableQuantity}`
			);
		}

		// Check required percentage if specified
		if (input.requiredPercentage && input.requiredPercentage > ourShare) {
			errors.push(
				`Insufficient share: need ${input.requiredPercentage * 100}%, have ${ourShare * 100}%`
			);
		}

		if (errors.length > 0) {
			// Try fallback capacities
			if (input.fallbackCapacityIds && input.fallbackCapacityIds.length > 0) {
				for (const fallbackId of input.fallbackCapacityIds) {
					const fallbackInput: CapacityShareInput = {
						...input,
						capacityId: fallbackId
					};
					const fallbackResult = this.resolveCapacityShareSlot(fallbackInput, context);
					if (fallbackResult.success) {
						return fallbackResult;
					}
				}
			}
			return { success: false, errors };
		}

		return {
			success: true,
			component: {
				capacityId: input.capacityId,
				providerId,
				ourShare,
				availableQuantity,
				allocatedQuantity: input.requiredQuantity
			},
			errors: []
		};
	}

	/**
	 * Extract capacity IDs from composition slots
	 */
	private extractCapacityIds(slots: CompositeSlot[]): string[] {
		return slots
			.filter((slot) => slot.inputDefinition.type === 'capacity_share')
			.map((slot) => (slot.inputDefinition as CapacityShareInput).capacityId);
	}

	/**
	 * Update capacity registries
	 */
	updateCapacityRegistry(capacities: Record<string, BaseCapacity>): void {
		this.capacityRegistry.clear();
		Object.entries(capacities).forEach(([id, capacity]) => {
			this.capacityRegistry.set(id, capacity);
		});
	}

	updateNetworkCapacityRegistry(
		networkCapacities: Record<string, Record<string, BaseCapacity>>
	): void {
		this.networkCapacityRegistry.clear();
		Object.entries(networkCapacities).forEach(([providerId, capacities]) => {
			const providerMap = new Map();
			Object.entries(capacities).forEach(([id, capacity]) => {
				providerMap.set(id, capacity);
			});
			this.networkCapacityRegistry.set(providerId, providerMap);
		});
	}
}

// Global instance
export const globalCompositeCapacityManager = new CompositeCapacityManager(
	new ProfferRegistryManager()
);

/**
 * Example usage:
 *
 * // Create a potluck composite capacity
 * const potluckCapacity = compositeCapacityManager.createCompositeCapacity(
 *   {
 *     name: "Community Potluck",
 *     emoji: "🥘",
 *     quantity: 20, // 20 people
 *     unit: "servings",
 *     location_type: "community_center"
 *   },
 *   [
 *     {
 *       id: "pie-slot",
 *       inputDefinition: {
 *         type: 'capacity_share',
 *         capacityId: "apple-pie-001",
 *         providerId: "baker-alice",
 *         requiredQuantity: 8,
 *         fallbackCapacityIds: ["pumpkin-pie-002", "cherry-pie-003"]
 *       },
 *       acceptanceLogic: { type: 'automatic', conditions: [] },
 *       status: 'empty',
 *       createdAt: new Date(),
 *       updatedAt: new Date()
 *     }
 *   ],
 *   'sum'
 * );
 */
