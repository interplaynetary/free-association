import { derived } from 'svelte/store';
import {
	userCapacities,
	userNetworkCapacitiesWithSlotQuantities,
	mutualFeasibleSlotCompositionsWithRedistribution
} from './core.svelte';
import type {
	CommonsCollection,
	CommonsCapacity,
	CommonsComposition,
	BaseCapacity,
	AvailabilitySlot,
	CapacitiesCollection
} from '$lib/schema';

// =============================================================================
// SPACE-TIME OVERLAP UTILITIES
// =============================================================================

// Helper to calculate if two locations are the same
const isSameLocation = (slot1: AvailabilitySlot, slot2: AvailabilitySlot): boolean => {
	// Exact location match
	return (
		slot1.location_type === slot2.location_type &&
		slot1.street_address === slot2.street_address &&
		slot1.city === slot2.city &&
		slot1.state_province === slot2.state_province &&
		slot1.postal_code === slot2.postal_code &&
		slot1.country === slot2.country &&
		slot1.online_link === slot2.online_link
	);
};

// Helper to parse time string to minutes since midnight
const parseTimeToMinutes = (timeStr?: string | null): number => {
	if (!timeStr) return 0;
	const [hours, minutes] = timeStr.split(':').map(Number);
	return (hours || 0) * 60 + (minutes || 0);
};

// Helper to calculate time window overlap
const calculateTimeWindowOverlap = (
	slot1: AvailabilitySlot,
	slot2: AvailabilitySlot
): {
	overlaps: boolean;
	overlapStart: number;
	overlapEnd: number;
	overlapMinutes: number;
} => {
	// All-day slots always overlap
	if (slot1.all_day && slot2.all_day) {
		return { overlaps: true, overlapStart: 0, overlapEnd: 24 * 60, overlapMinutes: 24 * 60 };
	}

	const start1 = parseTimeToMinutes(slot1.start_time);
	const end1 = parseTimeToMinutes(slot1.end_time);
	const start2 = parseTimeToMinutes(slot2.start_time);
	const end2 = parseTimeToMinutes(slot2.end_time);

	const overlapStart = Math.max(start1, start2);
	const overlapEnd = Math.min(end1, end2);
	const overlapMinutes = Math.max(0, overlapEnd - overlapStart);

	return {
		overlaps: overlapMinutes > 0,
		overlapStart,
		overlapEnd,
		overlapMinutes
	};
};

// Helper to check recurrence pattern compatibility
const areRecurrencePatternsCompatible = (
	slot1: AvailabilitySlot,
	slot2: AvailabilitySlot
): boolean => {
	// Both non-recurring
	if (!slot1.recurrence && !slot2.recurrence) {
		return true;
	}

	// One recurring, one not - compatible if dates align
	if (!slot1.recurrence || !slot2.recurrence) {
		return true; // Will be filtered by date alignment later
	}

	// Both recurring - check pattern compatibility
	return (
		slot1.recurrence === slot2.recurrence &&
		slot1.custom_recurrence_repeat_every === slot2.custom_recurrence_repeat_every &&
		slot1.custom_recurrence_repeat_unit === slot2.custom_recurrence_repeat_unit
	);
};

// Create a shared time window key for overlapping slots
const createSharedTimeWindowKey = (
	slot1: AvailabilitySlot,
	slot2: AvailabilitySlot,
	overlapInfo: ReturnType<typeof calculateTimeWindowOverlap>
): string => {
	// Location key (must be same for shared time windows)
	const locationKey = [
		slot1.location_type,
		slot1.street_address,
		slot1.city,
		slot1.state_province,
		slot1.postal_code,
		slot1.country,
		slot1.online_link
	].join('|');

	// Time window key (the overlapping portion)
	const timeKey =
		slot1.all_day && slot2.all_day
			? 'all-day'
			: `${Math.floor(overlapInfo.overlapStart / 60)
					.toString()
					.padStart(
						2,
						'0'
					)}:${(overlapInfo.overlapStart % 60).toString().padStart(2, '0')}-${Math.floor(
					overlapInfo.overlapEnd / 60
				)
					.toString()
					.padStart(2, '0')}:${(overlapInfo.overlapEnd % 60).toString().padStart(2, '0')}`;

	// Recurrence key (use the more specific one, or combine if both exist)
	const recurrenceKey = slot1.recurrence || slot2.recurrence || 'once';

	return `${locationKey}:::${timeKey}:::${recurrenceKey}`;
};

// =============================================================================
// COMMONS COMPOSITION SYSTEM
// =============================================================================

/**
 * Commons Composition Derived Store
 *
 * Captures the actual shared compositions happening between network participants.
 * The commons represents mutual slot-to-slot compositions, not just summed individual capacities.
 *
 * Key insight: Commons are the SHARED COORDINATION patterns, where:
 * - Alice's "Web Development" slot → Bob's "Project Work" slot (5 hours coordinated)
 * - Carol's "Design" slot → Alice's "Marketing" slot (3 hours coordinated)
 *
 * This captures the actual network effects and emergent coordination.
 */

// Helper to create capacity matching key for grouping compositions
const createCapacityMatchKey = (capacity: BaseCapacity): string =>
	`${capacity.name}|${capacity.unit || ''}|${capacity.description || ''}`.toLowerCase();

// Helper to get capacity metadata from ID
const getCapacityMetadata = (
	capacityId: string,
	userCapacities: CapacitiesCollection | null,
	networkCapacities: CapacitiesCollection
): { name: string; unit?: string; description?: string } | null => {
	// Check user capacities first
	if (userCapacities?.[capacityId]) {
		const cap = userCapacities[capacityId];
		return { name: cap.name, unit: cap.unit, description: cap.description };
	}

	// Check network capacities
	if (networkCapacities[capacityId]) {
		const cap = networkCapacities[capacityId];
		return { name: cap.name, unit: cap.unit, description: cap.description };
	}

	return null;
};

// Helper to get slot metadata from capacity and slot ID
const getSlotMetadata = (
	capacityId: string,
	slotId: string,
	userCapacities: CapacitiesCollection | null,
	networkCapacities: CapacitiesCollection
): AvailabilitySlot | null => {
	// Check user capacities first
	if (userCapacities?.[capacityId]) {
		const cap = userCapacities[capacityId];
		return cap.availability_slots?.find((slot) => slot.id === slotId) || null;
	}

	// Check network capacities
	if (networkCapacities[capacityId]) {
		const cap = networkCapacities[capacityId];
		return cap.availability_slots?.find((slot) => slot.id === slotId) || null;
	}

	return null;
};

// Helper to create slot space-time key for merging slots with same location and timing
const createSlotSpaceTimeKey = (slot: AvailabilitySlot): string => {
	const location = [
		slot.location_type,
		slot.longitude,
		slot.latitude,
		slot.street_address,
		slot.city,
		slot.state_province,
		slot.postal_code,
		slot.country,
		slot.online_link
	].join('|');

	const timing = [
		slot.all_day,
		slot.recurrence,
		slot.custom_recurrence_repeat_every,
		slot.custom_recurrence_repeat_unit,
		slot.custom_recurrence_end_type,
		slot.custom_recurrence_end_value,
		slot.start_date,
		slot.start_time,
		slot.end_date,
		slot.end_time,
		slot.time_zone
	].join('|');

	return `${location}:::${timing}`;
};

export const commonsCompositions = derived(
	[
		userCapacities,
		userNetworkCapacitiesWithSlotQuantities,
		mutualFeasibleSlotCompositionsWithRedistribution
	],
	([$userCapacities, $userNetworkCapacities, $mutualFeasibleCompositions]): CommonsCollection => {
		console.log('[COMMONS-COMPOSITIONS] Capturing shared coordination patterns...');

		if (!$userCapacities || !$userNetworkCapacities) return {};

		const now = new Date().toISOString();

		// Group compositions by capacity type (name, unit, description)
		const compositionGroups = new Map<
			string,
			{
				matchKey: string;
				name: string;
				unit?: string;
				description?: string;
				compositions: CommonsComposition[];
			}
		>();

		// First, collect all compositions by capacity metadata (slot-ID agnostic)
		const capacityCompositions = new Map<
			string,
			{
				metadata: {
					sourceMetadata: { name: string; unit?: string; description?: string };
					targetMetadata: { name: string; unit?: string; description?: string };
					sharedTimeWindow: string;
					timeOverlapMinutes: number;
					compositionTypeKey: string;
				};
				compositions: Array<{
					sourceCapacityId: string;
					sourceSlotId: string;
					targetCapacityId: string;
					targetSlotId: string;
					sourceProviderId: string;
					targetProviderId: string;
					ourFeasibleAmount: number;
					compositionType: 'from' | 'into';
					desireViability: number;
					feasibleViability: number;
					constraintRatio: number;
				}>;
			}
		>();

		// Process each mutual feasible composition
		Object.entries($mutualFeasibleCompositions).forEach(([compositionKey, composition]) => {
			const {
				sourceCapacityId,
				sourceSlotId,
				targetCapacityId,
				targetSlotId,
				ourFeasibleAmount,
				providerId,
				compositionType,
				desireViability,
				feasibleViability,
				constraintRatio
			} = composition;

			// Get metadata for both source and target capacities
			const sourceMetadata = getCapacityMetadata(
				sourceCapacityId,
				$userCapacities,
				$userNetworkCapacities
			);
			const targetMetadata = getCapacityMetadata(
				targetCapacityId,
				$userCapacities,
				$userNetworkCapacities
			);

			if (!sourceMetadata || !targetMetadata) return;

			// Create match keys for both source and target
			const sourceMatchKey = createCapacityMatchKey(sourceMetadata as BaseCapacity);
			const targetMatchKey = createCapacityMatchKey(targetMetadata as BaseCapacity);

			// Get slot metadata to check for space-time overlap
			const sourceSlot = getSlotMetadata(
				sourceCapacityId,
				sourceSlotId,
				$userCapacities,
				$userNetworkCapacities
			);
			const targetSlot = getSlotMetadata(
				targetCapacityId,
				targetSlotId,
				$userCapacities,
				$userNetworkCapacities
			);

			if (!sourceSlot || !targetSlot) return;

			// Check if locations are the same (required for commons)
			if (!isSameLocation(sourceSlot, targetSlot)) {
				return; // Skip compositions that don't share the same location
			}

			// Check for time window overlap
			const timeOverlap = calculateTimeWindowOverlap(sourceSlot, targetSlot);
			if (!timeOverlap.overlaps) {
				return; // Skip compositions with no time overlap
			}

			// Check recurrence pattern compatibility
			if (!areRecurrencePatternsCompatible(sourceSlot, targetSlot)) {
				return; // Skip compositions with incompatible recurrence patterns
			}

			// Create shared time window key for grouping overlapping compositions
			const sharedTimeWindowKey = createSharedTimeWindowKey(sourceSlot, targetSlot, timeOverlap);

			// Create a composition-type key: "source-capacity → target-capacity @ shared-time-window"
			const compositionTypeKey = `${sourceMatchKey}→${targetMatchKey}@${sharedTimeWindowKey}`;

			// Initialize composition type group if not exists
			if (!capacityCompositions.has(compositionTypeKey)) {
				capacityCompositions.set(compositionTypeKey, {
					metadata: {
						sourceMetadata,
						targetMetadata,
						sharedTimeWindow: sharedTimeWindowKey,
						timeOverlapMinutes: timeOverlap.overlapMinutes,
						compositionTypeKey
					},
					compositions: []
				});
			}

			// Add composition to this source→target composition type at this space-time
			capacityCompositions.get(compositionTypeKey)!.compositions.push({
				sourceCapacityId,
				sourceSlotId,
				targetCapacityId,
				targetSlotId,
				sourceProviderId: compositionType === 'from' ? providerId : 'user',
				targetProviderId: compositionType === 'into' ? providerId : 'user',
				ourFeasibleAmount,
				compositionType,
				desireViability,
				feasibleViability,
				constraintRatio
			});

			console.log(
				`[COMMONS-COMPOSITIONS] Found overlapping composition: ${sourceMetadata.name} → ${targetMetadata.name} @ ${sharedTimeWindowKey.substring(0, 40)}... - ${ourFeasibleAmount} units (${timeOverlap.overlapMinutes}min overlap)`
			);
		});

		// Now group compositions by source-target provider pairs (slot-ID agnostic)
		capacityCompositions.forEach(({ metadata, compositions }, compositionTypeKey) => {
			// Group compositions by source-provider → target-provider pairs
			const providerPairGroups = new Map<
				string,
				{
					sourceProviderId: string;
					targetProviderId: string;
					compositions: typeof compositions;
					totalQuantity: number;
				}
			>();

			compositions.forEach((comp) => {
				const pairKey = `${comp.sourceProviderId}→${comp.targetProviderId}`;

				if (!providerPairGroups.has(pairKey)) {
					providerPairGroups.set(pairKey, {
						sourceProviderId: comp.sourceProviderId,
						targetProviderId: comp.targetProviderId,
						compositions: [],
						totalQuantity: 0
					});
				}

				const group = providerPairGroups.get(pairKey)!;
				group.compositions.push(comp);
				group.totalQuantity += comp.ourFeasibleAmount;
			});

			// Create commons compositions for each provider pair
			const commonsCompositions: CommonsComposition[] = [];

			providerPairGroups.forEach((group, pairKey) => {
				// Calculate aggregated metrics for this provider pair
				const avgDesireViability =
					group.compositions.reduce((sum, c) => sum + c.desireViability, 0) /
					group.compositions.length;
				const avgFeasibleViability =
					group.compositions.reduce((sum, c) => sum + c.feasibleViability, 0) /
					group.compositions.length;
				const avgConstraintRatio =
					group.compositions.reduce((sum, c) => sum + c.constraintRatio, 0) /
					group.compositions.length;

				// Create a single commons composition representing all slot-to-slot compositions
				// between this source-provider and target-provider for this capacity type
				commonsCompositions.push({
					id: `commons_${compositionTypeKey}_${pairKey}`,
					source_capacity_id: `aggregated_${group.sourceProviderId}`, // Aggregated across all source capacity instances
					source_slot_id: `aggregated_slots`, // Aggregated across all source slots
					target_capacity_id: `aggregated_${group.targetProviderId}`, // Aggregated across all target capacity instances
					target_slot_id: `aggregated_slots`, // Aggregated across all target slots
					source_provider_id: group.sourceProviderId,
					target_provider_id: group.targetProviderId,
					mutual_feasible_quantity: group.totalQuantity,
					composition_type: group.compositions[0].compositionType, // Use first composition's type
					desire_viability: avgDesireViability,
					feasible_viability: avgFeasibleViability,
					constraint_ratio: avgConstraintRatio,
					created_at: now,
					updated_at: now
				});

				console.log(
					`[COMMONS-COMPOSITIONS] Aggregated ${metadata.sourceMetadata.name} → ${metadata.targetMetadata.name} @ shared window: ${group.sourceProviderId} → ${group.targetProviderId} = ${group.totalQuantity} total units across ${group.compositions.length} slot compositions (${metadata.timeOverlapMinutes}min overlap)`
				);
			});

			// Store the aggregated compositions for this composition type
			if (commonsCompositions.length > 0) {
				compositionGroups.set(compositionTypeKey, {
					matchKey: compositionTypeKey,
					name: `${metadata.sourceMetadata.name} → ${metadata.targetMetadata.name}`,
					unit: metadata.sourceMetadata.unit || metadata.targetMetadata.unit,
					description: `${metadata.sourceMetadata.description || metadata.sourceMetadata.name} composing into ${metadata.targetMetadata.description || metadata.targetMetadata.name} during shared ${metadata.timeOverlapMinutes}min window @ ${metadata.sharedTimeWindow.substring(0, 50)}...`,
					compositions: commonsCompositions
				});
			}
		});

		// Create commons capacities from composition groups
		const commons: CommonsCollection = {};

		compositionGroups.forEach((group, matchKey) => {
			if (group.compositions.length === 0) return;

			// Calculate network effects
			const totalQuantity = group.compositions.reduce(
				(sum, comp) => sum + comp.mutual_feasible_quantity,
				0
			);
			const participatingProviders = [
				...new Set([
					...group.compositions.map((c) => c.source_provider_id),
					...group.compositions.map((c) => c.target_provider_id)
				])
			];

			// Coordination density: how interconnected are the compositions
			const uniqueConnections = new Set(
				group.compositions.map((c) => `${c.source_provider_id}-${c.target_provider_id}`)
			).size;
			const maxPossibleConnections =
				participatingProviders.length * (participatingProviders.length - 1);
			const coordinationDensity =
				maxPossibleConnections > 0 ? uniqueConnections / maxPossibleConnections : 0;

			// Average viability across all compositions
			const averageViability =
				group.compositions.reduce((sum, comp) => sum + comp.feasible_viability, 0) /
				group.compositions.length;

			commons[matchKey] = {
				id: `commons_${matchKey.substring(0, 12)}`,
				name: group.name,
				unit: group.unit,
				description: group.description,
				mutual_compositions: group.compositions,
				total_commons_quantity: totalQuantity,
				participating_providers: participatingProviders,
				coordination_density: coordinationDensity,
				average_viability: averageViability,
				match_key: matchKey,
				created_at: now,
				updated_at: now
			};

			console.log(
				`[COMMONS-COMPOSITIONS] Created commons "${group.name}": ${totalQuantity} total coordinated quantity across ${group.compositions.length} mutual compositions`
			);
		});

		console.log(
			`[COMMONS-COMPOSITIONS] Generated ${Object.keys(commons).length} commons from shared coordination`
		);
		return commons;
	}
);

// =============================================================================
// TARGET-CENTRIC COMMONS: "What Composes INTO This?"
// =============================================================================

// Target-centric commons schema
interface TargetCapacityCommons {
	name: string;
	target_capacity_metadata: { name: string; unit?: string; description?: string };
	incoming_compositions: Array<{
		source_name: string;
		composition_key: string;
		total_quantity: number;
		provider_pairs: number;
		space_time_contexts: string[];
	}>;
	total_incoming_quantity: number;
	source_capacity_types: string[];
	coordination_density: number;
	average_viability: number;
	participating_providers: string[];
}

export const targetCapacityCommons = derived(
	[commonsCompositions],
	([$commons]): Record<string, TargetCapacityCommons> => {
		console.log('[TARGET-COMMONS] Analyzing what flows INTO each capacity type...');

		const targetCommons: Record<string, TargetCapacityCommons> = {};

		// Process each commons composition
		Object.entries($commons).forEach(([compositionKey, commons]) => {
			// Extract target capacity metadata from the composition name
			const compositionName = commons.name;
			const arrowIndex = compositionName.indexOf(' → ');
			if (arrowIndex === -1) return;

			const targetCapacityName = compositionName.substring(arrowIndex + 3);
			const sourceCapacityName = compositionName.substring(0, arrowIndex);

			// Initialize target commons if not exists
			if (!targetCommons[targetCapacityName]) {
				targetCommons[targetCapacityName] = {
					name: `→ ${targetCapacityName}`,
					target_capacity_metadata: {
						name: targetCapacityName,
						unit: commons.unit,
						description: commons.description
					},
					incoming_compositions: [],
					total_incoming_quantity: 0,
					source_capacity_types: [],
					coordination_density: 0,
					average_viability: 0,
					participating_providers: []
				};
			}

			const target = targetCommons[targetCapacityName];

			// Add this composition as an incoming flow
			target.incoming_compositions.push({
				source_name: sourceCapacityName,
				composition_key: compositionKey,
				total_quantity: commons.total_commons_quantity,
				provider_pairs: commons.mutual_compositions.length,
				space_time_contexts: [commons.match_key.split('@')[1] || 'unknown']
			});

			// Update aggregated metrics
			target.total_incoming_quantity += commons.total_commons_quantity;

			if (!target.source_capacity_types.includes(sourceCapacityName)) {
				target.source_capacity_types.push(sourceCapacityName);
			}

			// Merge participating providers
			commons.participating_providers.forEach((provider) => {
				if (!target.participating_providers.includes(provider)) {
					target.participating_providers.push(provider);
				}
			});
		});

		// Calculate final metrics for each target
		Object.values(targetCommons).forEach((target) => {
			// Coordination density: how many different source types flow into this target
			const maxPossibleSources = target.source_capacity_types.length;
			target.coordination_density =
				target.incoming_compositions.length / Math.max(maxPossibleSources, 1);

			// Average viability across all incoming compositions
			const totalViability = target.incoming_compositions.reduce((sum, comp) => {
				const commons = $commons[comp.composition_key];
				return sum + (commons?.average_viability || 0);
			}, 0);
			target.average_viability = totalViability / Math.max(target.incoming_compositions.length, 1);
		});

		console.log(
			`[TARGET-COMMONS] Generated ${Object.keys(targetCommons).length} target capacity commons`
		);
		return targetCommons;
	}
);

// Convenience stores for UI
export const commonsCompositionsList = derived(commonsCompositions, ($commons): CommonsCapacity[] =>
	Object.values($commons).sort((a, b) => b.total_commons_quantity - a.total_commons_quantity)
);

export const commonsCompositionsByName = derived(
	commonsCompositions,
	($commons): Record<string, CommonsCapacity> =>
		Object.values($commons).reduce(
			(acc, capacity) => ({
				...acc,
				[capacity.name.toLowerCase()]: capacity
			}),
			{} as Record<string, CommonsCapacity>
		)
);

export const targetCapacityCommonsList = derived(targetCapacityCommons, ($target) =>
	Object.values($target).sort((a, b) => b.total_incoming_quantity - a.total_incoming_quantity)
);

// =============================================================================
// SOURCE-CENTRIC COMMONS: "What Does This Compose INTO?"
// =============================================================================

// Source-centric commons schema
interface SourceCapacityCommons {
	name: string;
	source_capacity_metadata: { name: string; unit?: string; description?: string };
	outgoing_compositions: Array<{
		target_name: string;
		composition_key: string;
		total_quantity: number;
		provider_pairs: number;
		space_time_contexts: string[];
	}>;
	total_outgoing_quantity: number;
	target_capacity_types: string[];
	utilization_rate: number;
	average_viability: number;
	participating_providers: string[];
}

export const sourceCapacityCommons = derived(
	[commonsCompositions],
	([$commons]): Record<string, SourceCapacityCommons> => {
		console.log('[SOURCE-COMMONS] Analyzing what flows OUT OF each capacity type...');

		const sourceCommons: Record<string, SourceCapacityCommons> = {};

		// Process each commons composition
		Object.entries($commons).forEach(([compositionKey, commons]) => {
			// Extract source capacity metadata from the composition name
			const compositionName = commons.name;
			const arrowIndex = compositionName.indexOf(' → ');
			if (arrowIndex === -1) return;

			const sourceCapacityName = compositionName.substring(0, arrowIndex);
			const targetCapacityName = compositionName.substring(arrowIndex + 3);

			// Initialize source commons if not exists
			if (!sourceCommons[sourceCapacityName]) {
				sourceCommons[sourceCapacityName] = {
					name: `${sourceCapacityName} →`,
					source_capacity_metadata: {
						name: sourceCapacityName,
						unit: commons.unit,
						description: commons.description
					},
					outgoing_compositions: [],
					total_outgoing_quantity: 0,
					target_capacity_types: [],
					utilization_rate: 0,
					average_viability: 0,
					participating_providers: []
				};
			}

			const source = sourceCommons[sourceCapacityName];

			// Add this composition as an outgoing flow
			source.outgoing_compositions.push({
				target_name: targetCapacityName,
				composition_key: compositionKey,
				total_quantity: commons.total_commons_quantity,
				provider_pairs: commons.mutual_compositions.length,
				space_time_contexts: [commons.match_key.split('@')[1] || 'unknown']
			});

			// Update aggregated metrics
			source.total_outgoing_quantity += commons.total_commons_quantity;

			if (!source.target_capacity_types.includes(targetCapacityName)) {
				source.target_capacity_types.push(targetCapacityName);
			}

			// Merge participating providers
			commons.participating_providers.forEach((provider) => {
				if (!source.participating_providers.includes(provider)) {
					source.participating_providers.push(provider);
				}
			});
		});

		// Calculate final metrics for each source
		Object.values(sourceCommons).forEach((source) => {
			// Utilization rate: how much of this capacity type is being composed
			// (This is a simplified calculation - in reality would need total available capacity)
			source.utilization_rate = Math.min(source.total_outgoing_quantity / 100, 1); // Placeholder

			// Average viability across all outgoing compositions
			const totalViability = source.outgoing_compositions.reduce((sum, comp) => {
				const commons = $commons[comp.composition_key];
				return sum + (commons?.average_viability || 0);
			}, 0);
			source.average_viability = totalViability / Math.max(source.outgoing_compositions.length, 1);
		});

		console.log(
			`[SOURCE-COMMONS] Generated ${Object.keys(sourceCommons).length} source capacity commons`
		);
		return sourceCommons;
	}
);

export const sourceCapacityCommonsList = derived(sourceCapacityCommons, ($source) =>
	Object.values($source).sort((a, b) => b.total_outgoing_quantity - a.total_outgoing_quantity)
);

// =============================================================================
// LOCATION-BASED COMMONS: "Coordination Hubs"
// =============================================================================

// Location-based commons schema
interface LocationCommons {
	name: string;
	location_key: string;
	active_time_windows: Array<{
		time_pattern: string;
		compositions: Array<{
			composition_name: string;
			total_quantity: number;
			provider_pairs: number;
		}>;
		utilization_density: number;
		capacity_diversity: number;
	}>;
	total_coordinated_quantity: number;
	participating_providers: string[];
	coordination_density: number;
	location_metadata: {
		location_type?: string;
		city?: string;
		country?: string;
		online_link?: string;
	};
}

export const locationCommons = derived(
	[commonsCompositions],
	([$commons]): Record<string, LocationCommons> => {
		console.log('[LOCATION-COMMONS] Analyzing coordination hubs by location...');

		const locationCommons: Record<string, LocationCommons> = {};

		// Process each commons composition
		Object.entries($commons).forEach(([compositionKey, commons]) => {
			// Extract location information from the match key
			const matchKeyParts = commons.match_key.split('@');
			if (matchKeyParts.length < 2) return;

			const spaceTimeInfo = matchKeyParts[1];
			const spaceTimeParts = spaceTimeInfo.split(':::');
			if (spaceTimeParts.length < 3) return;

			const locationKey = spaceTimeParts[0];
			const timeKey = spaceTimeParts[1];
			const recurrenceKey = spaceTimeParts[2];

			// Parse location metadata
			const locationParts = locationKey.split('|');
			const locationMetadata = {
				location_type: locationParts[0] || undefined,
				city: locationParts[4] || undefined,
				country: locationParts[7] || undefined,
				online_link: locationParts[8] || undefined
			};

			// Create human-readable location name
			const locationName = locationMetadata.online_link
				? 'Online'
				: locationMetadata.city
					? `${locationMetadata.city}${locationMetadata.country ? `, ${locationMetadata.country}` : ''}`
					: locationMetadata.location_type || 'Unknown Location';

			// Initialize location commons if not exists
			if (!locationCommons[locationKey]) {
				locationCommons[locationKey] = {
					name: `${locationName} Coordination Hub`,
					location_key: locationKey,
					active_time_windows: [],
					total_coordinated_quantity: 0,
					participating_providers: [],
					coordination_density: 0,
					location_metadata: locationMetadata
				};
			}

			const location = locationCommons[locationKey];

			// Find or create time window
			let timeWindow = location.active_time_windows.find(
				(tw) => tw.time_pattern === `${timeKey} (${recurrenceKey})`
			);
			if (!timeWindow) {
				timeWindow = {
					time_pattern: `${timeKey} (${recurrenceKey})`,
					compositions: [],
					utilization_density: 0,
					capacity_diversity: 0
				};
				location.active_time_windows.push(timeWindow);
			}

			// Add composition to this time window
			timeWindow.compositions.push({
				composition_name: commons.name,
				total_quantity: commons.total_commons_quantity,
				provider_pairs: commons.mutual_compositions.length
			});

			// Update location metrics
			location.total_coordinated_quantity += commons.total_commons_quantity;

			// Merge participating providers
			commons.participating_providers.forEach((provider) => {
				if (!location.participating_providers.includes(provider)) {
					location.participating_providers.push(provider);
				}
			});
		});

		// Calculate final metrics for each location
		Object.values(locationCommons).forEach((location) => {
			// Calculate time window metrics
			location.active_time_windows.forEach((timeWindow) => {
				timeWindow.utilization_density =
					timeWindow.compositions.reduce((sum, comp) => sum + comp.total_quantity, 0) / 100; // Placeholder
				timeWindow.capacity_diversity = new Set(
					timeWindow.compositions.map((comp) => comp.composition_name.split(' → ')[0])
				).size;
			});

			// Overall coordination density
			const totalCompositions = location.active_time_windows.reduce(
				(sum, tw) => sum + tw.compositions.length,
				0
			);
			const totalProviders = location.participating_providers.length;
			const maxPossibleConnections = totalProviders * (totalProviders - 1);
			location.coordination_density =
				maxPossibleConnections > 0 ? totalCompositions / maxPossibleConnections : 0;
		});

		console.log(
			`[LOCATION-COMMONS] Generated ${Object.keys(locationCommons).length} location commons`
		);
		return locationCommons;
	}
);

export const locationCommonsList = derived(locationCommons, ($location) =>
	Object.values($location).sort(
		(a, b) => b.total_coordinated_quantity - a.total_coordinated_quantity
	)
);

// =============================================================================
// TEMPORAL COMMONS: "Coordination Rhythms"
// =============================================================================

// Temporal commons schema
interface TemporalCommons {
	name: string;
	time_pattern: string;
	recurring_compositions: Array<{
		composition_name: string;
		total_quantity_per_cycle: number;
		locations: string[];
		provider_pairs: number;
	}>;
	temporal_density: number;
	cross_location_coordination: boolean;
	rhythm_stability: number;
	total_coordinated_quantity: number;
	participating_providers: string[];
}

export const temporalCommons = derived(
	[commonsCompositions],
	([$commons]): Record<string, TemporalCommons> => {
		console.log('[TEMPORAL-COMMONS] Analyzing coordination rhythms by time patterns...');

		const temporalCommons: Record<string, TemporalCommons> = {};

		// Process each commons composition
		Object.entries($commons).forEach(([compositionKey, commons]) => {
			// Extract time pattern from the match key
			const matchKeyParts = commons.match_key.split('@');
			if (matchKeyParts.length < 2) return;

			const spaceTimeInfo = matchKeyParts[1];
			const spaceTimeParts = spaceTimeInfo.split(':::');
			if (spaceTimeParts.length < 3) return;

			const locationKey = spaceTimeParts[0];
			const timeKey = spaceTimeParts[1];
			const recurrenceKey = spaceTimeParts[2];

			const timePatternKey = `${timeKey}_${recurrenceKey}`;
			const humanTimePattern = `${timeKey} (${recurrenceKey})`;

			// Initialize temporal commons if not exists
			if (!temporalCommons[timePatternKey]) {
				temporalCommons[timePatternKey] = {
					name: `${humanTimePattern} Coordination Wave`,
					time_pattern: humanTimePattern,
					recurring_compositions: [],
					temporal_density: 0,
					cross_location_coordination: false,
					rhythm_stability: 0,
					total_coordinated_quantity: 0,
					participating_providers: []
				};
			}

			const temporal = temporalCommons[timePatternKey];

			// Add composition to this time pattern
			temporal.recurring_compositions.push({
				composition_name: commons.name,
				total_quantity_per_cycle: commons.total_commons_quantity,
				locations: [locationKey],
				provider_pairs: commons.mutual_compositions.length
			});

			// Update temporal metrics
			temporal.total_coordinated_quantity += commons.total_commons_quantity;

			// Merge participating providers
			commons.participating_providers.forEach((provider) => {
				if (!temporal.participating_providers.includes(provider)) {
					temporal.participating_providers.push(provider);
				}
			});
		});

		// Calculate final metrics for each temporal pattern
		Object.values(temporalCommons).forEach((temporal) => {
			// Temporal density: how consistently this time slot is used
			temporal.temporal_density = Math.min(temporal.total_coordinated_quantity / 50, 1); // Placeholder

			// Cross-location coordination: does this time pattern span multiple locations?
			const allLocations = temporal.recurring_compositions.flatMap((comp) => comp.locations);
			temporal.cross_location_coordination = new Set(allLocations).size > 1;

			// Rhythm stability: how stable this pattern is (placeholder - would need historical data)
			temporal.rhythm_stability = 0.8; // Placeholder
		});

		console.log(
			`[TEMPORAL-COMMONS] Generated ${Object.keys(temporalCommons).length} temporal commons`
		);
		return temporalCommons;
	}
);

export const temporalCommonsList = derived(temporalCommons, ($temporal) =>
	Object.values($temporal).sort(
		(a, b) => b.total_coordinated_quantity - a.total_coordinated_quantity
	)
);

// =============================================================================
// CAPACITY ECOSYSTEMS: "Frequently Co-Composing Clusters"
// =============================================================================

// Capacity ecosystem schema
interface CapacityEcosystem {
	name: string;
	core_capacities: string[];
	mutual_compositions: Array<{
		capacity_pair: string;
		total_coordinated_quantity: number;
		bidirectional: boolean;
		composition_keys: string[];
	}>;
	ecosystem_density: number;
	boundary_compositions: Array<{
		internal_capacity: string;
		external_capacity: string;
		total_quantity: number;
	}>;
	participating_providers: string[];
	total_internal_coordination: number;
}

export const capacityEcosystems = derived(
	[commonsCompositions],
	([$commons]): Record<string, CapacityEcosystem> => {
		console.log('[ECOSYSTEMS] Detecting capacity ecosystems from composition patterns...');

		// Build capacity co-occurrence matrix
		const capacityConnections = new Map<string, Map<string, number>>();
		const allCapacities = new Set<string>();

		// Analyze all compositions to find capacity relationships
		Object.entries($commons).forEach(([compositionKey, commons]) => {
			const compositionName = commons.name;
			const arrowIndex = compositionName.indexOf(' → ');
			if (arrowIndex === -1) return;

			const sourceCapacity = compositionName.substring(0, arrowIndex);
			const targetCapacity = compositionName.substring(arrowIndex + 3);

			allCapacities.add(sourceCapacity);
			allCapacities.add(targetCapacity);

			// Record bidirectional relationships
			[sourceCapacity, targetCapacity].forEach((cap1) => {
				if (!capacityConnections.has(cap1)) {
					capacityConnections.set(cap1, new Map());
				}
				[sourceCapacity, targetCapacity].forEach((cap2) => {
					if (cap1 !== cap2) {
						const connections = capacityConnections.get(cap1)!;
						connections.set(cap2, (connections.get(cap2) || 0) + commons.total_commons_quantity);
					}
				});
			});
		});

		// Simple clustering algorithm: find highly connected capacity groups
		const ecosystems: Record<string, CapacityEcosystem> = {};
		const processedCapacities = new Set<string>();

		Array.from(allCapacities).forEach((capacity) => {
			if (processedCapacities.has(capacity)) return;

			// Find all capacities strongly connected to this one
			const connections = capacityConnections.get(capacity) || new Map();
			const strongConnections = Array.from(connections.entries())
				.filter(([_, quantity]) => quantity > 10) // Threshold for strong connection
				.map(([cap, _]) => cap);

			if (strongConnections.length >= 2) {
				// Create ecosystem
				const coreCapacities = [capacity, ...strongConnections];
				const ecosystemName = `${capacity} Ecosystem`;

				// Mark all as processed
				coreCapacities.forEach((cap) => processedCapacities.add(cap));

				// Build ecosystem data
				const mutualCompositions: CapacityEcosystem['mutual_compositions'] = [];
				const boundaryCompositions: CapacityEcosystem['boundary_compositions'] = [];
				let totalInternalCoordination = 0;
				const participatingProviders = new Set<string>();

				// Analyze compositions within this ecosystem
				Object.entries($commons).forEach(([compositionKey, commons]) => {
					const compositionName = commons.name;
					const arrowIndex = compositionName.indexOf(' → ');
					if (arrowIndex === -1) return;

					const sourceCapacity = compositionName.substring(0, arrowIndex);
					const targetCapacity = compositionName.substring(arrowIndex + 3);

					const sourceInEcosystem = coreCapacities.includes(sourceCapacity);
					const targetInEcosystem = coreCapacities.includes(targetCapacity);

					if (sourceInEcosystem && targetInEcosystem) {
						// Internal composition
						mutualCompositions.push({
							capacity_pair: `${sourceCapacity} ↔ ${targetCapacity}`,
							total_coordinated_quantity: commons.total_commons_quantity,
							bidirectional: true, // Simplified
							composition_keys: [compositionKey]
						});
						totalInternalCoordination += commons.total_commons_quantity;
					} else if (sourceInEcosystem || targetInEcosystem) {
						// Boundary composition
						boundaryCompositions.push({
							internal_capacity: sourceInEcosystem ? sourceCapacity : targetCapacity,
							external_capacity: sourceInEcosystem ? targetCapacity : sourceCapacity,
							total_quantity: commons.total_commons_quantity
						});
					}

					// Collect providers
					commons.participating_providers.forEach((provider) =>
						participatingProviders.add(provider)
					);
				});

				// Calculate ecosystem density
				const maxPossibleConnections = coreCapacities.length * (coreCapacities.length - 1);
				const actualConnections = mutualCompositions.length;
				const ecosystemDensity =
					maxPossibleConnections > 0 ? actualConnections / maxPossibleConnections : 0;

				ecosystems[ecosystemName] = {
					name: ecosystemName,
					core_capacities: coreCapacities,
					mutual_compositions: mutualCompositions,
					ecosystem_density: ecosystemDensity,
					boundary_compositions: boundaryCompositions,
					participating_providers: Array.from(participatingProviders),
					total_internal_coordination: totalInternalCoordination
				};
			}
		});

		console.log(`[ECOSYSTEMS] Detected ${Object.keys(ecosystems).length} capacity ecosystems`);
		return ecosystems;
	}
);

export const capacityEcosystemsList = derived(capacityEcosystems, ($ecosystems) =>
	Object.values($ecosystems).sort(
		(a, b) => b.total_internal_coordination - a.total_internal_coordination
	)
);
