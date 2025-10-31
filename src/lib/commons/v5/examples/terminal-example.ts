/**
 * Terminal Example - Using Free Association Protocol v5 in a CLI/Terminal Environment
 * 
 * This demonstrates how to use the pure `free.ts` API without Svelte.
 * Perfect for:
 * - CLI tools
 * - Server-side processing
 * - Tests
 * - Non-browser environments
 */

import {
	createInitialState,
	buildSystemState,
	computeAllocations,
	computeMutualRecognition,
	computeDampingFactors,
	updateOverAllocationHistory,
	applyNeedUpdateLaw,
	type SystemStateSnapshot,
	type AllocationResult
} from '../free';

import type {
	Commitment,
	NeedSlot,
	AvailabilitySlot,
	GlobalRecognitionWeights
} from '../schemas';

// ═══════════════════════════════════════════════════════════════════
// EXAMPLE SCENARIO: 3 people, food and tutoring needs
// ═══════════════════════════════════════════════════════════════════

/**
 * Alice: Has food, needs tutoring
 */
const alicePubKey = 'alice-pub-key';
const aliceCommitment: Commitment = {
	capacity_slots: [
		{
			id: 'alice-food-1',
			need_type_id: 'food',
			quantity: 50,
			availability_window: {
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	need_slots: [
		{
			id: 'alice-tutoring-1',
			need_type_id: 'tutoring',
			quantity: 10,
			availability_window: {
				time_ranges: [{ start_time: '18:00', end_time: '20:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	global_recognition_weights: {
		'bob-pub-key': 0.6,
		'carol-pub-key': 0.4
	}
};

/**
 * Bob: Has tutoring, needs food
 */
const bobPubKey = 'bob-pub-key';
const bobCommitment: Commitment = {
	capacity_slots: [
		{
			id: 'bob-tutoring-1',
			need_type_id: 'tutoring',
			quantity: 20,
			availability_window: {
				time_ranges: [{ start_time: '18:00', end_time: '21:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	need_slots: [
		{
			id: 'bob-food-1',
			need_type_id: 'food',
			quantity: 30,
			availability_window: {
				time_ranges: [{ start_time: '09:00', end_time: '17:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	global_recognition_weights: {
		'alice-pub-key': 0.7,
		'carol-pub-key': 0.3
	}
};

/**
 * Carol: Has both, needs a little of each
 */
const carolPubKey = 'carol-pub-key';
const carolCommitment: Commitment = {
	capacity_slots: [
		{
			id: 'carol-food-1',
			need_type_id: 'food',
			quantity: 40,
			availability_window: {
				time_ranges: [{ start_time: '10:00', end_time: '16:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		},
		{
			id: 'carol-tutoring-1',
			need_type_id: 'tutoring',
			quantity: 15,
			availability_window: {
				time_ranges: [{ start_time: '19:00', end_time: '21:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	need_slots: [
		{
			id: 'carol-food-2',
			need_type_id: 'food',
			quantity: 10,
			availability_window: {
				time_ranges: [{ start_time: '12:00', end_time: '14:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		},
		{
			id: 'carol-tutoring-2',
			need_type_id: 'tutoring',
			quantity: 5,
			availability_window: {
				time_ranges: [{ start_time: '18:00', end_time: '19:00' }]
			},
			location: { city: 'San Francisco', country: 'USA' }
		}
	],
	global_recognition_weights: {
		'alice-pub-key': 0.5,
		'bob-pub-key': 0.5
	}
};

// ═══════════════════════════════════════════════════════════════════
// PROTOCOL EXECUTION
// ═══════════════════════════════════════════════════════════════════

/**
 * Run one iteration of the protocol from Alice's perspective
 */
function runAliceIteration() {
	console.log('\n════════════════════════════════════════════════════════');
	console.log('Running Allocation from Alice\'s Perspective');
	console.log('════════════════════════════════════════════════════════\n');
	
	// 1. Build system state from commitments
	const allCommitments: Record<string, Commitment> = {
		[alicePubKey]: aliceCommitment,
		[bobPubKey]: bobCommitment,
		[carolPubKey]: carolCommitment
	};
	
	const systemState = buildSystemState(allCommitments);
	console.log('System State:');
	console.log('  People with needs:', Object.keys(systemState.needsByPersonAndType).length);
	console.log('  People with capacity:', Object.keys(systemState.capacityByPersonAndType).length);
	console.log('  Iteration:', systemState.iteration);
	
	// 2. Compute mutual recognition
	const othersRecognition: Record<string, GlobalRecognitionWeights> = {
		[bobPubKey]: bobCommitment.global_recognition_weights || {},
		[carolPubKey]: carolCommitment.global_recognition_weights || {}
	};
	
	const myRecognition = aliceCommitment.global_recognition_weights || {};
	const mutualRecognition = computeMutualRecognition(myRecognition, othersRecognition, alicePubKey);
	
	console.log('\nMutual Recognition:');
	for (const [person, value] of Object.entries(mutualRecognition)) {
		const name = person === bobPubKey ? 'Bob' : 'Carol';
		console.log(`  Alice ⟷ ${name}: ${(value * 100).toFixed(1)}%`);
	}
	
	// 3. Compute allocations for Alice's capacity
	const myCapacitySlots = aliceCommitment.capacity_slots || [];
	const result = computeAllocations(
		alicePubKey,
		myCapacitySlots,
		myRecognition,
		mutualRecognition,
		allCommitments,
		systemState,
		null // No previous state (first iteration)
	);
	
	console.log('\nAllocations:');
	console.log(`  Total allocations: ${result.allocations.length}`);
	
	for (const allocation of result.allocations) {
		const recipient = allocation.recipient_pubkey === bobPubKey ? 'Bob' : 'Carol';
		console.log(`  → ${recipient}: ${allocation.quantity.toFixed(2)} ${allocation.need_type_id} (${allocation.tier})`);
	}
	
	console.log('\nSlot Denominators:');
	for (const [slotId, denom] of Object.entries(result.slotDenominators)) {
		console.log(`  ${slotId}:`);
		console.log(`    Mutual tier: ${denom.mutual.toFixed(2)}`);
		console.log(`    Non-mutual tier: ${denom.nonMutual.toFixed(2)}`);
	}
	
	console.log('\nConvergence Metrics:');
	console.log(`  Total need magnitude: ${result.convergence.totalNeedMagnitude.toFixed(2)}`);
	console.log(`  Contraction rate: ${result.convergence.contractionRate.toFixed(2)}`);
	if (result.convergence.percentNeedReduction !== undefined) {
		console.log(`  Need reduction: ${result.convergence.percentNeedReduction.toFixed(1)}% (allocation progress)`);
	}
	console.log(`  People satisfied: ${result.convergence.percentNeedsMet.toFixed(1)}% (fully met)`);
	console.log(`  Universal satisfaction: ${result.convergence.universalSatisfaction}`);
	console.log(`  Iterations to convergence: ${result.convergence.iterationsToConvergence ?? 'unknown'}`);
	
	if (result.convergence.maxPersonNeed !== undefined) {
		console.log(`  Max person need: ${result.convergence.maxPersonNeed.toFixed(2)}`);
	}
	if (result.convergence.needVariance !== undefined) {
		console.log(`  Need variance: ${result.convergence.needVariance.toFixed(2)}`);
	}
	
	return result;
}

/**
 * Interactive step-by-step iteration mode
 * Press Enter to advance to the next iteration
 */
async function runStepByStepIteration() {
	const readline = require('readline');
	const rl = readline.createInterface({
		input: process.stdin,
		output: process.stdout
	});
	
	const waitForEnter = (): Promise<void> => {
		return new Promise((resolve) => {
			rl.question('\n👉 Press ENTER for next iteration (or Ctrl+C to quit)... ', () => {
				resolve();
			});
		});
	};
	
	console.log('\n╔════════════════════════════════════════════════════════╗');
	console.log('║     STEP-BY-STEP CONVERGENCE VISUALIZATION            ║');
	console.log('╚════════════════════════════════════════════════════════╝');
	console.log('\nWatch how the algorithm converges iteration by iteration!\n');
	
	let overAllocationHistory: Record<string, number[]> = {};
	let previousState: SystemStateSnapshot | null = null;
	let iteration = 0;
	const maxIterations = 50; // Safety limit
	
	// Track all allocations made so we can update needs
	const allAllocations: Record<string, Record<string, number>> = {
		[alicePubKey]: {},
		[bobPubKey]: {},
		[carolPubKey]: {}
	};
	
	while (iteration < maxIterations) {
		console.log('\n════════════════════════════════════════════════════════');
		console.log(`ITERATION ${iteration}`);
		console.log('════════════════════════════════════════════════════════');
		
		// Build system state with updated commitments (needs reduced by received allocations)
		const updatedCommitments: Record<string, Commitment> = {};
		
		for (const [pubKey, commitment] of Object.entries({
			[alicePubKey]: aliceCommitment,
			[bobPubKey]: bobCommitment,
			[carolPubKey]: carolCommitment
		})) {
			// Update need slots based on received allocations
			const updatedNeedSlots = (commitment.need_slots || []).map(slot => {
				const received = allAllocations[pubKey][slot.need_type_id] || 0;
				return {
					...slot,
					quantity: Math.max(0, slot.quantity - received)
				};
			});
			
			updatedCommitments[pubKey] = {
				...commitment,
				need_slots: updatedNeedSlots
			};
		}
		
		const systemState = buildSystemState(updatedCommitments, previousState || undefined);
		
		// Show current needs before allocation
		console.log('\n📊 Current Needs:');
		for (const [person, needsByType] of Object.entries(systemState.needsByPersonAndType)) {
			const name = person === alicePubKey ? 'Alice' : person === bobPubKey ? 'Bob' : 'Carol';
			const totalNeed = Math.sqrt(
				Object.values(needsByType).reduce((sum, need) => sum + need ** 2, 0)
			);
			console.log(`  ${name}: ${totalNeed.toFixed(2)} (magnitude)`);
			for (const [type, amount] of Object.entries(needsByType)) {
				if (amount > 0.01) {
					console.log(`    - ${type}: ${amount.toFixed(2)}`);
				}
			}
		}
		
		// Compute allocations from ALL participants' perspectives
		const allResults: any[] = [];
		
		for (const [providerPubKey, providerCommitment] of Object.entries(updatedCommitments)) {
			const providerCapacity = providerCommitment.capacity_slots || [];
			if (providerCapacity.length === 0) continue; // Skip if no capacity
			
			// Compute mutual recognition from this provider's perspective
			const myRecognition = providerCommitment.global_recognition_weights || {};
			const othersRecognition: Record<string, GlobalRecognitionWeights> = {};
			for (const [otherPubKey, otherCommitment] of Object.entries(updatedCommitments)) {
				if (otherPubKey !== providerPubKey) {
					othersRecognition[otherPubKey] = otherCommitment.global_recognition_weights || {};
				}
			}
			const mutualRecognition = computeMutualRecognition(myRecognition, othersRecognition, providerPubKey);
			
			// Compute allocations from this provider
			const result = computeAllocations(
				providerPubKey,
				providerCapacity,
				myRecognition,
				mutualRecognition,
				updatedCommitments,
				systemState,
				previousState
			);
			
			allResults.push({ provider: providerPubKey, result });
			
			// Track allocations for updating needs next iteration
			for (const allocation of result.allocations) {
				const recipientId = allocation.recipient_pubkey;
				const typeId = allocation.need_type_id;
				allAllocations[recipientId][typeId] = 
					(allAllocations[recipientId][typeId] || 0) + allocation.quantity;
			}
		}
		
		// Combine all allocations for display
		const totalAllocations = allResults.flatMap(r => r.result.allocations);
		
		// Show allocations grouped by provider
		console.log('\n💰 Allocations Made:');
		if (totalAllocations.length === 0) {
			console.log('  (none - everyone satisfied!)');
		} else {
			for (const { provider, result } of allResults) {
				if (result.allocations.length > 0) {
					const providerName = provider === alicePubKey ? 'Alice' : provider === bobPubKey ? 'Bob' : 'Carol';
					console.log(`  From ${providerName}:`);
					for (const allocation of result.allocations) {
						const recipientName = allocation.recipient_pubkey === alicePubKey ? 'Alice' : 
											   allocation.recipient_pubkey === bobPubKey ? 'Bob' : 'Carol';
						console.log(`    → ${recipientName}: ${allocation.quantity.toFixed(2)} ${allocation.need_type_id} (${allocation.tier})`);
					}
				}
			}
		}
		
		// Use the last result's convergence (they should all see the same system state)
		const convergence = allResults.length > 0 ? allResults[0].result.convergence : null;
		
		if (convergence) {
			// Show convergence metrics
			console.log('\n📈 Convergence Status:');
			console.log(`  Need Magnitude: ${convergence.totalNeedMagnitude.toFixed(2)}`);
			if (previousState) {
				const prevMagnitude = convergence.previousNeedMagnitude;
				const reduction = prevMagnitude - convergence.totalNeedMagnitude;
				console.log(`  Reduction: ${reduction.toFixed(2)} (${convergence.percentNeedReduction?.toFixed(1)}%)`);
			}
			console.log(`  Contraction Rate: ${convergence.contractionRate.toFixed(2)}`);
			console.log(`  People Satisfied: ${convergence.percentNeedsMet.toFixed(1)}%`);
			
			if (convergence.universalSatisfaction) {
				console.log('\n🎉 ════════════════════════════════════════════════════════');
				console.log('   CONVERGENCE ACHIEVED!');
				console.log('   All needs satisfied after', iteration + 1, 'iterations');
				console.log('════════════════════════════════════════════════════════\n');
				break;
			}
			
			if (convergence.isConverged) {
				console.log('\n✓ Converged (needs < 0.001)');
				break;
			}
		}
		
		// Check if we should continue
		if (totalAllocations.length === 0 && iteration > 0) {
			// No allocations and we've run at least one iteration
			// Check if there are still unmet needs
			let hasUnmetNeeds = false;
			for (const needsByType of Object.values(systemState.needsByPersonAndType)) {
				for (const need of Object.values(needsByType)) {
					if (need > 0.01) {
						hasUnmetNeeds = true;
						break;
					}
				}
				if (hasUnmetNeeds) break;
			}
			
			if (!hasUnmetNeeds) {
				console.log('\n🎉 ════════════════════════════════════════════════════════');
				console.log('   CONVERGENCE ACHIEVED!');
				console.log('   All needs satisfied after', iteration + 1, 'iterations');
				console.log('════════════════════════════════════════════════════════\n');
				break;
			}
		}
		
		// Wait for user input before next iteration
		await waitForEnter();
		
		previousState = systemState;
		iteration++;
	}
	
	rl.close();
}

/**
 * Simulate multiple iterations with damping
 */
function runMultipleIterations(maxIterations: number = 5) {
	console.log('\n════════════════════════════════════════════════════════');
	console.log('Running Multiple Iterations with Damping');
	console.log('════════════════════════════════════════════════════════\n');
	
	let overAllocationHistory: Record<string, number[]> = {};
	let previousState: SystemStateSnapshot | null = null;
	
	for (let i = 0; i < maxIterations; i++) {
		console.log(`\n─────────────────────────────────────────────────────────`);
		console.log(`ITERATION ${i + 1}`);
		console.log(`─────────────────────────────────────────────────────────`);
		
		// Compute damping factors
		const dampingFactors = computeDampingFactors(overAllocationHistory);
		console.log('\nDamping Factors:');
		for (const [typeId, factor] of Object.entries(dampingFactors)) {
			console.log(`  ${typeId}: ${factor.toFixed(2)}`);
		}
		
		// Build current commitments (would normally update with damping)
		const allCommitments: Record<string, Commitment> = {
			[alicePubKey]: aliceCommitment,
			[bobPubKey]: bobCommitment,
			[carolPubKey]: carolCommitment
		};
		
		const systemState = buildSystemState(allCommitments, previousState || undefined);
		
		// Compute allocations
		const myRecognition = aliceCommitment.global_recognition_weights || {};
		const othersRecognition: Record<string, GlobalRecognitionWeights> = {
			[bobPubKey]: bobCommitment.global_recognition_weights || {},
			[carolPubKey]: carolCommitment.global_recognition_weights || {}
		};
		const mutualRecognition = computeMutualRecognition(myRecognition, othersRecognition, alicePubKey);
		
		const result = computeAllocations(
			alicePubKey,
			aliceCommitment.capacity_slots || [],
			myRecognition,
			mutualRecognition,
			allCommitments,
			systemState,
			previousState
		);
		
		console.log(`\nAllocated ${result.allocations.length} slots`);
		console.log(`Need magnitude: ${result.convergence.totalNeedMagnitude.toFixed(2)}`);
		
		// Update damping history (simulate receiving allocations)
		const receivedByType: Record<string, number> = {};
		for (const allocation of result.allocations) {
			receivedByType[allocation.need_type_id] = 
				(receivedByType[allocation.need_type_id] || 0) + allocation.quantity;
		}
		
		const currentNeeds: Record<string, number> = {
			food: 0,
			tutoring: 10 // Alice's need
		};
		
		overAllocationHistory = updateOverAllocationHistory(
			overAllocationHistory,
			receivedByType,
			currentNeeds
		);
		
		previousState = systemState;
		
		// Check convergence
		if (result.convergence.universalSatisfaction) {
			console.log('\n✓ Universal satisfaction achieved!');
			break;
		}
	}
}

// ═══════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════

if (import.meta.url === `file://${process.argv[1]}`) {
	// Run single iteration
	runAliceIteration();
	
	// Run multiple iterations
	runMultipleIterations(5);
}

export {
	runAliceIteration,
	runMultipleIterations,
	runStepByStepIteration
};

