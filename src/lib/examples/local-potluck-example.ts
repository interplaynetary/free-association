import {
	localCompositeManager,
	CompositionHelpers,
	type LocalCompositeCapacity,
	type CompositionResult
} from './local-composite-capacity';
import { get } from 'svelte/store';
import { userNetworkCapacitiesWithShares } from '../state/core.svelte';

// Helper function to get total computed quantity from slot-based structure
function getTotalComputedQuantity(capacity: any): number {
	if (capacity.computed_quantities && Array.isArray(capacity.computed_quantities)) {
		return capacity.computed_quantities.reduce(
			(sum: number, slot: any) => sum + (slot.quantity || 0),
			0
		);
	}
	return 0;
}

/**
 * Example: Local Potluck Composition using Network Capacities
 *
 * This demonstrates how you can compose your own offerings using
 * the computed quantities from your shares in network capacities
 */

export function demonstrateLocalComposition() {
	console.log('=== Local Composition with Network Capacities ===\n');

	// Get current network capacities (your shares in others' capacities)
	const networkCapacities = get(userNetworkCapacitiesWithShares);

	console.log('Available network capacities:');
	Object.entries(networkCapacities).forEach(([capacityId, capacity]) => {
		console.log(
			`  ${capacity.emoji || '🎁'} ${capacity.name}: ${getTotalComputedQuantity(capacity)} ${capacity.unit} (from ${capacity.provider_id})`
		);
	});

	console.log('\n' + '='.repeat(50) + '\n');

	// Create a potluck using network capacities
	const potluckComponents = Object.entries(networkCapacities)
		.filter(([_, capacity]) => getTotalComputedQuantity(capacity) > 0)
		.map(([capacityId, capacity]) => ({
			capacityId,
			providerId: capacity.provider_id,
			quantity: Math.min(getTotalComputedQuantity(capacity), 2), // Use up to 2 units of each
			displayName: `${capacity.emoji || '🎁'} ${capacity.name}`
		}));

	if (potluckComponents.length === 0) {
		console.log('No network capacities available for composition');
		return;
	}

	// Create the composite capacity
	const potluck = CompositionHelpers.createPotluck(
		'Community Potluck from Network Shares',
		potluckComponents
	);

	console.log('Created composite potluck:');
	console.log(`${potluck.emoji} ${potluck.name}`);
	console.log(`Components required: ${potluck.components.length}`);

	// Resolve it against current state
	const result = localCompositeManager.resolveComposition(potluck);

	console.log('\nComposition result:');
	console.log(`Feasible: ${result.isComposable}`);
	console.log(`Total available: ${result.totalAvailable} ${potluck.unit}`);

	if (result.issues.length > 0) {
		console.log('\nIssues:');
		result.issues.forEach((issue) => console.log(`  - ${issue}`));
	}

	console.log('\nComponent breakdown:');
	result.resolvedComponents.forEach((comp) => {
		const status = comp.sufficient ? '✅' : '❌';
		console.log(
			`  ${status} ${comp.capacity.name}: need ${comp.required}, have ${comp.available} (from ${comp.providerId})`
		);
	});

	return result;
}

/**
 * Example of how this integrates with your reactive system
 */
export function createReactiveComposition() {
	console.log('\n=== Reactive Composition Example ===\n');

	// This would automatically update when network capacities change
	const potluck = CompositionHelpers.createPotluck('Auto-updating Potluck', [
		// These reference actual network capacity IDs
		{ capacityId: 'some-pie-capacity', quantity: 2, displayName: '🥧 Pie Share' },
		{ capacityId: 'some-salad-capacity', quantity: 1, displayName: '🥗 Salad Share' }
	]);

	// In a real Svelte component, you could do:
	// $: result = localCompositeManager.resolveComposition(potluck);
	// And the composition would automatically update when network shares change

	console.log('Created reactive potluck that will update with network changes');
	console.log(`${potluck.emoji} ${potluck.name}`);

	return potluck;
}

/**
 * Show how the composition paradigm works with divisibility
 */
export function explainCompositionParadigm() {
	console.log('\n=== Composition + Divisibility Paradigm ===\n');

	console.log('How it works:');
	console.log("1. Network layer: You subscribe to shares in others' capacities");
	console.log('2. Computation layer: Your computed_quantities respect divisibility constraints');
	console.log('3. Composition layer: You compose these computed quantities into offerings');
	console.log('4. Reactivity: Compositions auto-update when mutual recognition changes');

	console.log('\nExample flow:');
	console.log('Alice has 12 🥧 pie slices, max_percentage_div: 0.8 (keeps 20% for herself)');
	console.log('Your mutual recognition with Alice: 15%');
	console.log(
		'Your computed_quantities: Math.floor(12 * 0.15) = 1 slice per slot (respects natural divisibility)'
	);
	console.log('Your potluck composition: Requires 2 slices, you have 1 → NOT FEASIBLE');
	console.log('');
	console.log('If mutual recognition improves to 25%:');
	console.log('Your new computed_quantities: Math.floor(12 * 0.25) = 3 slices per slot');
	console.log('Your potluck composition: Requires 2 slices, you have 3 → NOW FEASIBLE');
	console.log('Total potluck capacity automatically increases!');

	console.log('\nKey insight: Your capacity to offer composite experiences');
	console.log('depends directly on your network relationships and changes dynamically!');
}

// Example usage:
// demonstrateLocalComposition();
// createReactiveComposition();
// explainCompositionParadigm();
