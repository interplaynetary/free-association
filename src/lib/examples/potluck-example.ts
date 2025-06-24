import {
	globalCompositeCapacityManager,
	type CompositeCapacity,
	type CapacityResolutionContext
} from './composite-capacity';
import { computeQuantityShare } from '../protocol';
import type { BaseCapacity } from '../schema';

/**
 * Example: Community Potluck with Composed Capacities
 *
 * This demonstrates how someone can create a potluck offering that depends on
 * their shares in other people's capacities (pie, salad, etc.)
 */

// Mock capacities that exist in the network
const mockNetworkCapacities = {
	alice: {
		'apple-pie-001': {
			id: 'apple-pie-001',
			name: 'Homemade Apple Pie',
			emoji: '🥧',
			quantity: 12,
			unit: 'slices',
			max_natural_div: 1,
			max_percentage_div: 0.8, // Alice keeps at least 20% for herself
			owner_id: 'alice'
		} as BaseCapacity,
		'garden-salad-001': {
			id: 'garden-salad-001',
			name: 'Fresh Garden Salad',
			emoji: '🥗',
			quantity: 8,
			unit: 'servings',
			max_natural_div: 1,
			max_percentage_div: 0.9,
			owner_id: 'alice'
		} as BaseCapacity
	},
	bob: {
		'bbq-ribs-001': {
			id: 'bbq-ribs-001',
			name: 'BBQ Ribs',
			emoji: '🍖',
			quantity: 15,
			unit: 'servings',
			max_natural_div: 1,
			max_percentage_div: 0.7,
			owner_id: 'bob'
		} as BaseCapacity
	}
};

// Mock mutual recognition shares
// These represent how much Alice and Bob recognize Charlie's contributions
const mockProviderShares = {
	alice: 0.15, // Alice gives Charlie 15% based on mutual recognition
	bob: 0.22 // Bob gives Charlie 22% based on mutual recognition
};

// Mock our shares in network capacities (calculated from mutual recognition)
const mockNetworkShares = {
	alice: {
		'apple-pie-001': 0.15, // 15% share = 1.8 slices
		'garden-salad-001': 0.15 // 15% share = 1.2 servings
	},
	bob: {
		'bbq-ribs-001': 0.22 // 22% share = 3.3 servings
	}
};

/**
 * Create a potluck composite capacity
 */
function createPotluckCapacity(): CompositeCapacity {
	return globalCompositeCapacityManager.createCompositeCapacity(
		{
			name: 'Community Potluck Dinner',
			emoji: '🍽️',
			quantity: 0, // Will be calculated from components
			unit: 'servings',
			location_type: 'community_center',
			start_date: '2024-12-15',
			start_time: '18:00',
			end_time: '21:00',
			max_natural_div: 1,
			max_percentage_div: 1.0
		},
		[
			// Pie component
			{
				id: 'pie-component',
				inputDefinition: {
					type: 'capacity_share',
					capacityId: 'apple-pie-001',
					providerId: 'alice',
					requiredQuantity: 2, // Need at least 2 slices for the potluck
					fallbackCapacityIds: [] // Could add backup pie options
				},
				acceptanceLogic: { type: 'automatic', conditions: [] },
				status: 'empty',
				createdAt: new Date(),
				updatedAt: new Date()
			},
			// Salad component
			{
				id: 'salad-component',
				inputDefinition: {
					type: 'capacity_share',
					capacityId: 'garden-salad-001',
					providerId: 'alice',
					requiredQuantity: 1,
					requiredPercentage: 0.1 // Need at least 10% share
				},
				acceptanceLogic: { type: 'automatic', conditions: [] },
				status: 'empty',
				createdAt: new Date(),
				updatedAt: new Date()
			},
			// Ribs component
			{
				id: 'ribs-component',
				inputDefinition: {
					type: 'capacity_share',
					capacityId: 'bbq-ribs-001',
					providerId: 'bob',
					requiredQuantity: 3
				},
				acceptanceLogic: { type: 'automatic', conditions: [] },
				status: 'empty',
				createdAt: new Date(),
				updatedAt: new Date()
			}
		],
		'sum' // Add up all the component servings
	);
}

/**
 * Resolve the potluck capacity against current network state
 */
function resolvePotluckCapacity() {
	const potluckCapacity = createPotluckCapacity();

	// Create resolution context from current network state
	const context: CapacityResolutionContext = {
		providerShares: mockProviderShares,
		capacities: {}, // We don't own any relevant capacities for this example
		networkCapacities: mockNetworkCapacities,
		networkShares: mockNetworkShares
	};

	// Resolve the composite capacity
	const resolved = globalCompositeCapacityManager.resolveCompositeCapacity(
		potluckCapacity,
		context
	);

	return resolved;
}

/**
 * Demonstrate the composition
 */
export function demonstratePotluckComposition() {
	console.log('=== Community Potluck Composition Example ===\n');

	// Show our shares in network capacities
	console.log('Our shares in network capacities:');
	Object.entries(mockNetworkShares).forEach(([providerId, shares]) => {
		console.log(`\nFrom ${providerId}:`);
		Object.entries(shares).forEach(([capacityId, share]) => {
			const capacity = mockNetworkCapacities[providerId][capacityId];
			const ourQuantity = computeQuantityShare(capacity, share);
			console.log(
				`  ${capacity.emoji} ${capacity.name}: ${share * 100}% = ${ourQuantity} ${capacity.unit}`
			);
		});
	});

	console.log('\n' + '='.repeat(50) + '\n');

	// Resolve the potluck
	const resolved = resolvePotluckCapacity();

	console.log('Potluck Composition Result:');
	console.log(`${resolved.capacity.emoji} ${resolved.capacity.name}`);
	console.log(`Total Available: ${resolved.totalAvailableQuantity} ${resolved.capacity.unit}`);
	console.log(`Composition Feasible: ${resolved.compositionFeasible}`);
	console.log(`Progress: ${resolved.capacity.compositionProgress}%`);

	if (resolved.missingRequirements.length > 0) {
		console.log('\nMissing Requirements:');
		resolved.missingRequirements.forEach((req) => console.log(`  - ${req}`));
	}

	console.log('\nComponent Breakdown:');
	Object.entries(resolved.resolvedComponents).forEach(([slotId, component]) => {
		const capacity = mockNetworkCapacities[component.providerId][component.capacityId];
		console.log(`  ${capacity.emoji} ${capacity.name}:`);
		console.log(
			`    Available: ${component.availableQuantity} ${capacity.unit} (${component.ourShare * 100}% share)`
		);
		console.log(`    Allocated: ${component.allocatedQuantity} ${capacity.unit}`);
		console.log(`    Provider: ${component.providerId}`);
	});

	console.log('\n' + '='.repeat(50));
	console.log('Analysis:');
	console.log("- This potluck offering is automatically composed from Charlie's shares");
	console.log('- The total offering (6 servings) respects mutual-fulfillment constraints');
	console.log('- If mutual recognition changes, the potluck capacity automatically adjusts');
	console.log('- Fallback capacities could be used if primary components become unavailable');

	return resolved;
}

/**
 * Show how changing mutual recognition affects the composition
 */
export function demonstrateDynamicComposition() {
	console.log('\n=== Dynamic Composition Example ===\n');

	// Original composition
	console.log('Original composition:');
	const original = resolvePotluckCapacity();
	console.log(`Total: ${original.totalAvailableQuantity} servings\n`);

	// Simulate increased mutual recognition
	const improvedContext: CapacityResolutionContext = {
		providerShares: {
			alice: 0.25, // Increased from 15% to 25%
			bob: 0.3 // Increased from 22% to 30%
		},
		capacities: {},
		networkCapacities: mockNetworkCapacities,
		networkShares: {
			alice: {
				'apple-pie-001': 0.25,
				'garden-salad-001': 0.25
			},
			bob: {
				'bbq-ribs-001': 0.3
			}
		}
	};

	const potluckCapacity = createPotluckCapacity();
	const improved = globalCompositeCapacityManager.resolveCompositeCapacity(
		potluckCapacity,
		improvedContext
	);

	console.log('After improved mutual recognition:');
	console.log(`Total: ${improved.totalAvailableQuantity} servings`);
	console.log(
		`Increase: +${improved.totalAvailableQuantity - original.totalAvailableQuantity} servings`
	);

	console.log('\nThis demonstrates how composite capacities automatically scale');
	console.log('with changes in mutual recognition relationships!');
}

// Example usage:
// demonstratePotluckComposition();
// demonstrateDynamicComposition();
