import { tick } from 'svelte';
import { get } from 'svelte/store';
import type {
	CompositeCapacity,
	CompositeSlot,
	CapacityShareInput,
	CapacityResolutionContext,
	ResolvedCompositeCapacity
} from '$lib/examples/composite-capacity';
import type { BaseCapacity, ShareMap } from '$lib/schema';
import { computeQuantityShare } from '$lib/protocol';
import {
	mutualRecognition,
	userCapacities,
	networkCapacities,
	networkCapacityShares,
	generalShares
} from '../state/core.svelte';

// ============================================================================
// REACTIVE CONSTRAINT STATE
// ============================================================================

// Individual capacity constraints as reactive state
export const capacityConstraints = $state<
	Record<
		string,
		{
			totalQuantity: number;
			maxPercentageDiv: number;
			maxNaturalDiv: number;
			providerId: string;
			unit: string;
			name: string;
			emoji?: string;
		}
	>
>({});

// Composite capacity registry
export const compositeCapacities = $state<Map<string, ReactiveCompositeCapacity>>(new Map());

// Derived available shares - automatically recalculates when mutual recognition changes
export const availableShares = $derived(() => {
	const currentMutualRecognition = get(mutualRecognition);
	const currentUserCapacities = get(userCapacities);
	const currentNetworkCapacities = get(networkCapacities);
	const currentGeneralShares = get(generalShares);

	if (!currentMutualRecognition || !currentGeneralShares) {
		return {};
	}

	const shares: Record<string, Record<string, number>> = {};

	// Process our own capacities (we have 100% share)
	if (currentUserCapacities) {
		Object.entries(currentUserCapacities).forEach(([capacityId, capacity]) => {
			const ownerId = 'self'; // We own our own capacities
			if (!shares[ownerId]) shares[ownerId] = {};

			const quantity = capacity.quantity || 0;
			const maxNatural = capacity.max_natural_div || 1;
			const maxPercent = capacity.max_percentage_div || 1;

			// We have full access to our own capacities, subject to our own constraints
			const constrainedQuantity = Math.min(quantity, quantity * maxPercent);
			const naturalConstrained = Math.floor(constrainedQuantity / maxNatural) * maxNatural;

			shares[ownerId][capacityId] = naturalConstrained;
		});
	}

	// Process network capacities based on mutual recognition
	if (currentNetworkCapacities) {
		Object.entries(currentNetworkCapacities).forEach(([providerId, providerCapacities]) => {
			const recognition = currentGeneralShares[providerId] || 0;
			if (recognition <= 0) return;

			if (!shares[providerId]) shares[providerId] = {};

			Object.entries(providerCapacities).forEach(([capacityId, capacity]) => {
				const baseQuantity = (capacity.quantity || 0) * recognition;
				const maxPercent = capacity.max_percentage_div || 1;
				const maxNatural = capacity.max_natural_div || 1;

				const percentConstrained =
					recognition > maxPercent ? (capacity.quantity || 0) * maxPercent : baseQuantity;
				const naturalConstrained = Math.floor(percentConstrained / maxNatural) * maxNatural;

				shares[providerId][capacityId] = naturalConstrained;
			});
		});
	}

	return shares;
});

// System-wide constraint status
export const systemConstraintStatus = $derived(() => {
	const total = compositeCapacities.size;
	const feasible = Array.from(compositeCapacities.values()).filter((cap) => {
		const feasibility = cap.feasibility();
		return feasibility.feasible;
	}).length;
	const infeasible = total - feasible;

	const totalAvailableCapacity = Array.from(compositeCapacities.values()).reduce((sum, cap) => {
		const available = cap.totalAvailable();
		return sum + available;
	}, 0);

	return {
		total,
		feasible,
		infeasible,
		totalAvailableCapacity,
		feasibilityRate: total > 0 ? feasible / total : 1
	};
});

// ============================================================================
// REACTIVE COMPOSITE CAPACITY CLASS
// ============================================================================

export class ReactiveCompositeCapacity {
	// Reactive state for the composite capacity definition
	definition = $state<CompositeCapacity>();

	// Derived constraint feasibility - automatically updates when dependencies change
	feasibility = $derived(() => {
		if (!this.definition?.compositionSlots) {
			return { feasible: true, results: [], reasons: [] };
		}

		const currentAvailableShares = availableShares();
		const results = this.definition.compositionSlots.map((slot) => {
			if (slot.inputDefinition.type !== 'capacity_share') {
				return {
					slotId: slot.id,
					feasible: true,
					available: 0,
					required: 0,
					shortfall: 0
				};
			}

			const shareInput = slot.inputDefinition as CapacityShareInput;
			const available = currentAvailableShares[shareInput.providerId]?.[shareInput.capacityId] || 0;
			const required = shareInput.requiredQuantity;

			// Check percentage requirement if specified
			let percentageFeasible = true;
			if (shareInput.requiredPercentage) {
				const totalCapacity = this.getCapacityTotal(shareInput.capacityId, shareInput.providerId);
				const requiredQuantityFromPercent = totalCapacity * shareInput.requiredPercentage;
				percentageFeasible = available >= requiredQuantityFromPercent;
			}

			const quantityFeasible = available >= required;
			const feasible = quantityFeasible && percentageFeasible;

			return {
				slotId: slot.id,
				feasible,
				available,
				required,
				shortfall: Math.max(0, required - available),
				capacityId: shareInput.capacityId,
				providerId: shareInput.providerId
			};
		});

		const feasible = results.every((r) => r.feasible);
		const reasons = results
			.filter((r) => !r.feasible)
			.map(
				(r) => `${r.slotId}: need ${r.required}, have ${r.available} (shortfall: ${r.shortfall})`
			);

		return { feasible, results, reasons };
	});

	// Total available quantity - reactive aggregation
	totalAvailable = $derived(() => {
		const currentFeasibility = this.feasibility();
		if (!currentFeasibility.feasible) return 0;

		const quantities = currentFeasibility.results.map((r: any) => r.available);

		switch (this.definition?.aggregationRule) {
			case 'sum':
				return quantities.reduce((sum: number, q: number) => sum + q, 0);
			case 'min':
				return quantities.length > 0 ? Math.min(...quantities) : 0;
			case 'max':
				return quantities.length > 0 ? Math.max(...quantities) : 0;
			case 'weighted_average':
				return this.calculateWeightedAverage(quantities);
			default:
				return quantities.reduce((sum: number, q: number) => sum + q, 0);
		}
	});

	// Resolved composite capacity for external use
	resolved = $derived((): ResolvedCompositeCapacity => {
		const resolvedComponents: Record<string, any> = {};
		const currentFeasibility = this.feasibility();
		const currentTotalAvailable = this.totalAvailable();

		currentFeasibility.results.forEach((result: any) => {
			if (result.capacityId && result.providerId) {
				resolvedComponents[result.slotId] = {
					capacityId: result.capacityId,
					providerId: result.providerId,
					ourShare: this.calculateOurShare(result.capacityId, result.providerId),
					availableQuantity: result.available,
					allocatedQuantity: result.required
				};
			}
		});

		return {
			capacity: {
				...this.definition!,
				quantity: currentTotalAvailable,
				compositionProgress: this.calculateProgress(),
				requiredSlotsFulfilled: currentFeasibility.results.filter((r: any) => r.feasible).length
			},
			resolvedComponents,
			totalAvailableQuantity: currentTotalAvailable,
			compositionFeasible: currentFeasibility.feasible,
			missingRequirements: currentFeasibility.reasons
		};
	});

	// Effect to log constraint changes (useful for debugging)
	constraintChangeEffect = $effect(() => {
		const currentFeasibility = this.feasibility();
		const currentTotalAvailable = this.totalAvailable();

		if (this.definition?.name) {
			console.log(`[CONSTRAINT] ${this.definition.name}:`, {
				feasible: currentFeasibility.feasible,
				totalAvailable: currentTotalAvailable,
				progress: this.calculateProgress(),
				reasons: currentFeasibility.reasons
			});
		}
	});

	constructor(definition: CompositeCapacity) {
		this.definition = definition;
	}

	private getCapacityTotal(capacityId: string, providerId: string): number {
		if (providerId === 'self') {
			const currentUserCapacities = get(userCapacities);
			return currentUserCapacities?.[capacityId]?.quantity || 0;
		}
		const currentNetworkCapacities = get(networkCapacities);
		return currentNetworkCapacities?.[providerId]?.[capacityId]?.quantity || 0;
	}

	private calculateOurShare(capacityId: string, providerId: string): number {
		if (providerId === 'self') return 1.0;
		const currentGeneralShares = get(generalShares);
		return currentGeneralShares[providerId] || 0;
	}

	private calculateProgress(): number {
		const total = this.definition?.totalRequiredSlots || 0;
		if (total === 0) return 100;

		const currentFeasibility = this.feasibility();
		const fulfilled = currentFeasibility.results.filter((r: any) => r.feasible).length;
		return Math.round((fulfilled / total) * 100);
	}

	private calculateWeightedAverage(quantities: number[]): number {
		if (!this.definition?.componentWeights || quantities.length === 0) {
			return quantities.reduce((sum, q) => sum + q, 0) / quantities.length;
		}

		let weightedSum = 0;
		let totalWeight = 0;
		const currentFeasibility = this.feasibility();

		currentFeasibility.results.forEach((result: any, index: number) => {
			const weight = this.definition?.componentWeights?.[result.slotId] || 1;
			weightedSum += quantities[index] * weight;
			totalWeight += weight;
		});

		return totalWeight > 0 ? weightedSum / totalWeight : 0;
	}

	// Method to check if capacity can be improved with better relationships
	getSuggestions(): Array<{
		providerId: string;
		currentRecognition: number;
		neededRecognition: number;
		impact: string;
	}> {
		const suggestions: Array<{
			providerId: string;
			currentRecognition: number;
			neededRecognition: number;
			impact: string;
		}> = [];

		const currentFeasibility = this.feasibility();
		const currentGeneralShares = get(generalShares);
		const currentNetworkCapacities = get(networkCapacities);

		currentFeasibility.results.forEach((result: any) => {
			if (!result.feasible && result.shortfall > 0) {
				const currentRecognition = currentGeneralShares[result.providerId] || 0;
				const totalCapacity = this.getCapacityTotal(result.capacityId, result.providerId);

				if (totalCapacity > 0) {
					const neededRecognition = result.required / totalCapacity;
					const maxAllowed =
						currentNetworkCapacities?.[result.providerId]?.[result.capacityId]
							?.max_percentage_div || 1;

					if (neededRecognition <= maxAllowed && neededRecognition > currentRecognition) {
						suggestions.push({
							providerId: result.providerId,
							currentRecognition,
							neededRecognition: Math.min(neededRecognition, maxAllowed),
							impact: `Would enable ${result.slotId} (+${result.shortfall} units)`
						});
					}
				}
			}
		});

		return suggestions;
	}
}

// ============================================================================
// REACTIVE CONSTRAINT REGISTRY
// ============================================================================

export class ReactiveConstraintRegistry {
	// Effect to handle constraint propagation across the system
	propagationEffect = $effect(() => {
		// This automatically runs when any constraint changes
		const infeasibleCapacities = Array.from(compositeCapacities.values()).filter((cap) => {
			const feasibility = cap.feasibility();
			return !feasibility.feasible;
		});

		if (infeasibleCapacities.length > 0) {
			console.log(
				'[CONSTRAINT-VIOLATIONS] Detected:',
				infeasibleCapacities.map((cap) => cap.definition?.name)
			);

			// Could trigger notifications, suggest alternatives, etc.
			this.handleConstraintViolations(infeasibleCapacities);
		}
	});

	// Batched constraint updates for performance
	private updateTimeout: ReturnType<typeof setTimeout> | null = null;
	pendingUpdates = $state<Set<string>>(new Set());

	// Effect with cleanup for handling batched updates
	batchEffect = $effect(() => {
		if (this.pendingUpdates.size > 0) {
			// Clear previous timeout
			if (this.updateTimeout) clearTimeout(this.updateTimeout);

			// Debounce constraint solving
			this.updateTimeout = setTimeout(() => {
				this.processBatchUpdates();
				this.pendingUpdates.clear();
			}, 50); // 50ms debounce
		}

		// Cleanup function
		return () => {
			if (this.updateTimeout) clearTimeout(this.updateTimeout);
		};
	});

	addCompositeCapacity(definition: CompositeCapacity): ReactiveCompositeCapacity {
		const reactive = new ReactiveCompositeCapacity(definition);
		compositeCapacities.set(definition.id, reactive);
		this.scheduleUpdate(definition.id);
		return reactive;
	}

	removeCompositeCapacity(id: string): boolean {
		const removed = compositeCapacities.delete(id);
		if (removed) {
			this.scheduleUpdate(id);
		}
		return removed;
	}

	getCompositeCapacity(id: string): ReactiveCompositeCapacity | undefined {
		return compositeCapacities.get(id);
	}

	getAllCompositeCapacities(): ReactiveCompositeCapacity[] {
		return Array.from(compositeCapacities.values());
	}

	getFeasibleCapacities(): ReactiveCompositeCapacity[] {
		return Array.from(compositeCapacities.values()).filter((cap) => {
			const feasibility = cap.feasibility();
			return feasibility.feasible;
		});
	}

	getInfeasibleCapacities(): ReactiveCompositeCapacity[] {
		return Array.from(compositeCapacities.values()).filter((cap) => {
			const feasibility = cap.feasibility();
			return !feasibility.feasible;
		});
	}

	// Get suggestions for improving constraint violations
	getSystemSuggestions(): Array<{
		capacityName: string;
		suggestions: Array<{
			providerId: string;
			currentRecognition: number;
			neededRecognition: number;
			impact: string;
		}>;
	}> {
		return this.getInfeasibleCapacities()
			.map((cap) => ({
				capacityName: cap.definition?.name || 'Unknown',
				suggestions: cap.getSuggestions()
			}))
			.filter((item) => item.suggestions.length > 0);
	}

	private scheduleUpdate(capacityId: string) {
		this.pendingUpdates.add(capacityId);
	}

	private async processBatchUpdates() {
		await tick(); // Ensure DOM updates are complete

		const affectedCapacities = Array.from(this.pendingUpdates);
		console.log(
			'[CONSTRAINT-BATCH] Processing updates for:',
			affectedCapacities.length,
			'capacities'
		);

		// Additional batch processing logic could go here
		// For now, the reactive system handles the updates automatically
	}

	private handleConstraintViolations(violations: ReactiveCompositeCapacity[]) {
		// Implement fallback strategies, suggestions, etc.
		violations.forEach((cap) => {
			const suggestions = cap.getSuggestions();
			if (suggestions.length > 0) {
				console.log(`[CONSTRAINT-SUGGESTION] ${cap.definition?.name}:`, suggestions);
			}
		});
	}

	// Utility method to update mutual recognition and see impact
	simulateRecognitionChange(
		providerId: string,
		newValue: number
	): {
		before: { feasible: number; total: number };
		after: { feasible: number; total: number };
		impactedCapacities: string[];
	} {
		// Capture current state
		const beforeFeasible = this.getFeasibleCapacities().length;
		const beforeTotal = compositeCapacities.size;

		// Store original value
		const currentMutualRecognition = get(mutualRecognition);
		const original = currentMutualRecognition[providerId];

		// Note: This is a simulation method - in real implementation we would
		// need to properly update the reactive stores, which requires different approach
		// For now, just return the current state as simulation
		console.log(
			`[SIMULATION] Would change ${providerId} recognition from ${original} to ${newValue}`
		);

		// Find impacted capacities
		const impactedCapacities = Array.from(compositeCapacities.values())
			.filter((cap) => {
				const feasibility = cap.feasibility();
				return feasibility.results.some((r: any) => r.providerId === providerId);
			})
			.map((cap) => cap.definition?.name || 'Unknown');

		return {
			before: { feasible: beforeFeasible, total: beforeTotal },
			after: { feasible: beforeFeasible, total: beforeTotal }, // Same for simulation
			impactedCapacities
		};
	}
}

// ============================================================================
// GLOBAL REGISTRY INSTANCE
// ============================================================================

export const globalConstraintRegistry = new ReactiveConstraintRegistry();

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

// Helper to create composite capacity with automatic registration
export function createReactiveCompositeCapacity(
	baseCapacity: Omit<BaseCapacity, 'id'>,
	compositionSlots: CompositeSlot[],
	aggregationRule: 'sum' | 'min' | 'max' | 'weighted_average' = 'sum',
	componentWeights?: Record<string, number>
): ReactiveCompositeCapacity {
	const id = crypto.randomUUID();

	const requiredSlots = compositionSlots.filter(
		(slot) => slot.inputDefinition.type === 'capacity_share'
	);

	const definition: CompositeCapacity = {
		...baseCapacity,
		id,
		compositionSlots,
		aggregationRule,
		componentWeights,
		isComposite: true,
		componentCapacityIds: compositionSlots
			.filter((slot) => slot.inputDefinition.type === 'capacity_share')
			.map((slot) => (slot.inputDefinition as CapacityShareInput).capacityId),
		requiredSlotsFulfilled: 0,
		totalRequiredSlots: requiredSlots.length,
		compositionProgress: 0
	};

	return globalConstraintRegistry.addCompositeCapacity(definition);
}

// Helper to get current resolution context
export function getCurrentResolutionContext(): CapacityResolutionContext {
	return {
		generalShares: get(generalShares) || {},
		capacities: get(userCapacities) || {},
		networkCapacities: get(networkCapacities) || {},
		networkShares: get(networkCapacityShares) || {}
	};
}
