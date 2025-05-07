/**
 * Centralized Free Association System
 * Translated from Haskell to TypeScript
 *
 * This is a functional, immutable implementation of a fulfillment and contribution
 * calculation system for network associations.
 */

// Note: We don't re-export everything here to avoid circular dependencies.
// The main exports are handled by src/lib/centralized.ts

// Example usage
import { createExampleForest } from './forest';
import { mutualFulfillment } from './mutual';

// Run a simple demonstration
export function demonstrateFulfillment() {
	const { forest, ci } = createExampleForest();

	const alice = forest.get('alice');
	const bob = forest.get('bob');
	const charlie = forest.get('charlie');

	if (!alice || !bob || !charlie) {
		console.error('Could not find one or more nodes in the forest');
		return;
	}

	// Print fulfillment values
	console.log('Mutual Fulfillment Values:');
	console.log(`Alice <-> Bob: ${mutualFulfillment(ci, alice, bob)}`);
	console.log(`Alice <-> Charlie: ${mutualFulfillment(ci, alice, charlie)}`);
	console.log(`Bob <-> Charlie: ${mutualFulfillment(ci, bob, charlie)}`);
}
