import { z } from 'zod/v4';
import { derived, get } from 'svelte/store';
import { userCapacities, userNetworkCapacitiesWithShares } from '../state/core.svelte';
import { BaseCapacitySchema, type BaseCapacity, type RecipientCapacity } from '../schema';

// Simple component reference for local composition
const ComponentReferenceSchema = z.object({
	type: z.literal('capacity_component'),
	capacityId: z.string(),
	providerId: z.string().optional(), // Optional - if not provided, assumes our own capacity
	requiredQuantity: z.number().gte(0),
	displayName: z.string().optional() // For UI display
});

// Local composite capacity that references available capacities
const LocalCompositeCapacitySchema = BaseCapacitySchema.extend({
	// Mark as composite
	isComposite: z.boolean().default(true),

	// Component requirements
	components: z.array(ComponentReferenceSchema),

	// How to combine component quantities
	aggregationRule: z.enum(['sum', 'min', 'max']).default('sum'),

	// Derived fields
	availableQuantity: z.number().default(0),
	isAvailable: z.boolean().default(false),
	missingComponents: z.array(z.string()).default([])
});

// Resolution result
const CompositionResultSchema = z.object({
	compositeCapacity: LocalCompositeCapacitySchema,
	resolvedComponents: z.array(
		z.object({
			componentId: z.string(),
			capacityId: z.string(),
			providerId: z.string(),
			required: z.number(),
			available: z.number(),
			sufficient: z.boolean(),
			capacity: z.union([BaseCapacitySchema, z.any()]) // BaseCapacity or RecipientCapacity
		})
	),
	totalAvailable: z.number(),
	isComposable: z.boolean(),
	issues: z.array(z.string())
});

// Export types
export type ComponentReference = z.infer<typeof ComponentReferenceSchema>;
export type LocalCompositeCapacity = z.infer<typeof LocalCompositeCapacitySchema>;
export type CompositionResult = z.infer<typeof CompositionResultSchema>;

/**
 * Local Composite Capacity Manager
 * Works with existing distributed capacity system to enable local composition
 */
export class LocalCompositeCapacityManager {
	/**
	 * Create a composite capacity from components
	 */
	createCompositeCapacity(
		baseCapacity: Omit<BaseCapacity, 'id'>,
		components: ComponentReference[],
		aggregationRule: 'sum' | 'min' | 'max' = 'sum'
	): LocalCompositeCapacity {
		const id = crypto.randomUUID();

		return {
			...baseCapacity,
			id,
			isComposite: true,
			components,
			aggregationRule,
			availableQuantity: 0,
			isAvailable: false,
			missingComponents: []
		};
	}

	/**
	 * Resolve a composite capacity against current available capacities
	 */
	resolveComposition(compositeCapacity: LocalCompositeCapacity): CompositionResult {
		const ourCapacities = get(userCapacities) || {};
		const networkCapacities = get(userNetworkCapacitiesWithShares) || {};

		const resolvedComponents = [];
		const issues = [];
		let totalAvailable = 0;
		let allComponentsSufficient = true;

		// Resolve each component
		for (const component of compositeCapacity.components) {
			const resolution = this.resolveComponent(component, ourCapacities, networkCapacities);
			resolvedComponents.push(resolution);

			if (!resolution.sufficient) {
				allComponentsSufficient = false;
				issues.push(
					`${resolution.capacity.name}: need ${resolution.required}, have ${resolution.available}`
				);
			}
		}

		// Calculate total based on aggregation rule
		if (allComponentsSufficient && resolvedComponents.length > 0) {
			switch (compositeCapacity.aggregationRule) {
				case 'sum':
					totalAvailable = resolvedComponents.reduce((sum, comp) => sum + comp.available, 0);
					break;
				case 'min':
					totalAvailable = Math.min(...resolvedComponents.map((comp) => comp.available));
					break;
				case 'max':
					totalAvailable = Math.max(...resolvedComponents.map((comp) => comp.available));
					break;
			}
		}

		return {
			compositeCapacity: {
				...compositeCapacity,
				availableQuantity: totalAvailable,
				isAvailable: allComponentsSufficient,
				missingComponents: issues
			},
			resolvedComponents,
			totalAvailable,
			isComposable: allComponentsSufficient,
			issues
		};
	}

	/**
	 * Resolve a single component reference
	 */
	private resolveComponent(
		component: ComponentReference,
		ourCapacities: Record<string, BaseCapacity>,
		networkCapacities: Record<string, RecipientCapacity>
	) {
		// First try our own capacities
		if (!component.providerId || component.providerId === 'self') {
			const capacity = ourCapacities[component.capacityId];
			if (capacity) {
				const available = capacity.quantity || 0;
				return {
					componentId: `${component.capacityId}-self`,
					capacityId: component.capacityId,
					providerId: 'self',
					required: component.requiredQuantity,
					available,
					sufficient: available >= component.requiredQuantity,
					capacity
				};
			}
		}

		// Try network capacities
		const networkCapacity = networkCapacities[component.capacityId];
		if (networkCapacity) {
			const available = networkCapacity.computed_quantity || 0;
			const providerId = networkCapacity.provider_id || 'unknown';

			return {
				componentId: `${component.capacityId}-${providerId}`,
				capacityId: component.capacityId,
				providerId,
				required: component.requiredQuantity,
				available,
				sufficient: available >= component.requiredQuantity,
				capacity: networkCapacity
			};
		}

		// Component not found
		return {
			componentId: `${component.capacityId}-missing`,
			capacityId: component.capacityId,
			providerId: component.providerId || 'unknown',
			required: component.requiredQuantity,
			available: 0,
			sufficient: false,
			capacity: {
				id: component.capacityId,
				name: `Missing: ${component.displayName || component.capacityId}`,
				quantity: 0
			} as BaseCapacity
		};
	}

	/**
	 * Get available components for composition UI
	 */
	getAvailableComponents(): Array<{
		id: string;
		name: string;
		providerId: string;
		available: number;
		unit?: string;
		emoji?: string;
	}> {
		const ourCapacities = get(userCapacities) || {};
		const networkCapacities = get(userNetworkCapacitiesWithShares) || {};

		const components = [];

		// Add our own capacities
		Object.entries(ourCapacities).forEach(([id, capacity]) => {
			components.push({
				id,
				name: capacity.name,
				providerId: 'self',
				available: capacity.quantity || 0,
				unit: capacity.unit,
				emoji: capacity.emoji
			});
		});

		// Add network capacities
		Object.entries(networkCapacities).forEach(([id, capacity]) => {
			components.push({
				id,
				name: capacity.name,
				providerId: capacity.provider_id || 'network',
				available: capacity.computed_quantity || 0,
				unit: capacity.unit,
				emoji: capacity.emoji
			});
		});

		return components;
	}
}

// Global instance
export const localCompositeManager = new LocalCompositeCapacityManager();

/**
 * Reactive store that automatically resolves all composite capacities
 */
export const resolvedCompositeCapacities = derived(
	[userCapacities, userNetworkCapacitiesWithShares],
	([ourCapacities, networkCapacities]) => {
		if (!ourCapacities) return [];

		// Find composite capacities and resolve them
		return Object.values(ourCapacities)
			.filter(
				(capacity): capacity is LocalCompositeCapacity =>
					'isComposite' in capacity && capacity.isComposite === true
			)
			.map((compositeCapacity) => localCompositeManager.resolveComposition(compositeCapacity));
	}
);

/**
 * Helper functions for creating common composition patterns
 */
export const CompositionHelpers = {
	/**
	 * Create a potluck-style composition
	 */
	createPotluck(
		name: string,
		components: Array<{
			capacityId: string;
			providerId?: string;
			quantity: number;
			displayName?: string;
		}>
	): LocalCompositeCapacity {
		return localCompositeManager.createCompositeCapacity(
			{
				name,
				emoji: '🍽️',
				unit: 'servings',
				location_type: 'event'
			},
			components.map((comp) => ({
				type: 'capacity_component' as const,
				capacityId: comp.capacityId,
				providerId: comp.providerId,
				requiredQuantity: comp.quantity,
				displayName: comp.displayName
			})),
			'sum'
		);
	},

	/**
	 * Create a workshop-style composition (limited by smallest component)
	 */
	createWorkshop(
		name: string,
		components: Array<{
			capacityId: string;
			providerId?: string;
			quantity: number;
			displayName?: string;
		}>
	): LocalCompositeCapacity {
		return localCompositeManager.createCompositeCapacity(
			{
				name,
				emoji: '🛠️',
				unit: 'participants',
				location_type: 'workshop'
			},
			components.map((comp) => ({
				type: 'capacity_component' as const,
				capacityId: comp.capacityId,
				providerId: comp.providerId,
				requiredQuantity: comp.quantity,
				displayName: comp.displayName
			})),
			'min' // Limited by the most constraining resource
		);
	}
};

/**
 * Example usage:
 *
 * // Create a potluck using available network capacities
 * const potluck = CompositionHelpers.createPotluck(
 *   "Community Dinner",
 *   [
 *     { capacityId: "apple-pie-001", providerId: "alice", quantity: 2, displayName: "🥧 Apple Pie" },
 *     { capacityId: "garden-salad-001", providerId: "alice", quantity: 1, displayName: "🥗 Salad" },
 *     { capacityId: "bbq-ribs-001", providerId: "bob", quantity: 3, displayName: "🍖 BBQ Ribs" }
 *   ]
 * );
 *
 * // Resolve it against current network state
 * const result = localCompositeManager.resolveComposition(potluck);
 * console.log(`Potluck feasible: ${result.isComposable}, total servings: ${result.totalAvailable}`);
 */
