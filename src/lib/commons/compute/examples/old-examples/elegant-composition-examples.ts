/**
 * Examples of the elegant composition target schema in action
 */

import type { UserSlotCompositionData } from '$lib/commons/v5/schemas';
import { parseCompositionTarget } from '$lib/validation';
import { createCollectiveTarget, isSelfConsumption } from '$lib/state/users.svelte';

// Example user slot composition data showing all target types
export const exampleCompositions: UserSlotCompositionData = {
	// Alice's cooking capacity, Tuesday 2pm slot
	'alice-cooking-capacity': {
		'tuesday-2pm-slot': {
			// 1. SELF-CONSUMPTION: Bob wants Alice's cooking for himself
			'04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b': {
				consumption: 2.0 // Bob's pubkey -> 2 units for self
			},

			// 2. INDIVIDUAL GIFTING: Charlie wants Alice's cooking to give to Diana
			'05d74a3b9f1e8c6a4b7c9e2f5a8d1b4c7e9f2a5d7b9c1e4f6a8d2b5c7e9f3a6d': {
				gift: 1.0 // Diana's pubkey -> 1 unit as gift
			},

			// 3. COLLECTIVE SHARING: Study group wants Alice's cooking to share
			'collective:04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b,05d74a3b9f1e8c6a4b7c9e2f5a8d1b4c7e9f2a5d7b9c1e4f6a8d2b5c7e9f3a6d':
				{
					'group-share': 3.0 // Multiple pubkeys -> 3 units for collective
				},

			// 4. CAPACITY COMPOSITION: Traditional slot-to-slot composition
			'my-dinner-party-capacity': {
				'friday-evening-slot': 2.0 // Existing behavior unchanged
			}
		}
	}
};

// Example usage of utility functions
export function demonstrateTargetParsing() {
	console.log('=== COMPOSITION TARGET PARSING EXAMPLES ===\n');

	// Parse different target types
	const targets = [
		'04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b', // Individual
		'collective:04c63b...,05d74a...', // Collective (shortened for display)
		'my-dinner-party-capacity' // Capacity
	];

	targets.forEach((targetId) => {
		const parsed = parseCompositionTarget(targetId);
		console.log(`Target: ${targetId.length > 20 ? targetId.slice(0, 20) + '...' : targetId}`);
		console.log(`  Type: ${parsed.type}`);
		console.log(`  Recipients: ${parsed.recipients.length > 0 ? parsed.recipients.length : 'N/A'}`);
		console.log('');
	});
}

// Example of creating collective targets
export function demonstrateCollectiveCreation() {
	console.log('=== COLLECTIVE TARGET CREATION ===\n');

	const studyGroupPubkeys = [
		'04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b',
		'05d74a3b9f1e8c6a4b7c9e2f5a8d1b4c7e9f2a5d7b9c1e4f6a8d2b5c7e9f3a6d',
		'06e85b4c1f2e9d8a5b8c1e3f6a9d2b5c8e1f3a6d8b1c4e7f9a2d5b8c1e4f7a9d'
	];

	const collectiveTarget = createCollectiveTarget(studyGroupPubkeys);
	console.log(`Created collective target: ${collectiveTarget.slice(0, 30)}...`);

	const parsed = parseCompositionTarget(collectiveTarget);
	console.log(`Parsed collective has ${parsed.recipients.length} members`);
}

// Example of self-consumption detection
export function demonstrateSelfConsumption() {
	console.log('=== SELF-CONSUMPTION DETECTION ===\n');

	const myPubkey = '04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b';
	const friendPubkey = '05d74a3b9f1e8c6a4b7c9e2f5a8d1b4c7e9f2a5d7b9c1e4f6a8d2b5c7e9f3a6d';

	console.log(`My pubkey targeting myself: ${isSelfConsumption(myPubkey, myPubkey)}`); // true
	console.log(`Friend's pubkey targeting myself: ${isSelfConsumption(friendPubkey, myPubkey)}`); // false
	console.log(`Capacity ID targeting myself: ${isSelfConsumption('some-capacity-id', myPubkey)}`); // false
}

// Example showing backward compatibility
export function demonstrateBackwardCompatibility() {
	console.log('=== BACKWARD COMPATIBILITY ===\n');

	// Old capacity-to-capacity compositions work unchanged
	const traditionalComposition: UserSlotCompositionData = {
		'source-capacity': {
			'source-slot': {
				'target-capacity': {
					'target-slot': 2.5
				}
			}
		}
	};

	console.log('Traditional capacity-to-capacity composition still works:');
	console.log(JSON.stringify(traditionalComposition, null, 2));
}

/**
 * Example 7: Enhanced DropDown with Collective Creation
 * Demonstrates how to use the enhanced dropdown for creating collectives
 */
export function example7_EnhancedDropdownUsage() {
	// Example of how to use the enhanced dropdown component
	const dropdownProps = {
		title: 'Select Contributors',
		searchPlaceholder: 'Search people...',
		position: { x: 100, y: 100 },
		width: 320,
		maxHeight: 400,
		allowCreateContact: true,
		allowCreateCollective: true, // 🎯 NEW: Enable collective creation
		collectiveMode: false, // Can be toggled to enable multi-select

		// Handlers for collective creation
		createCollective: async (detail: { name: string; memberIds: string[] }) => {
			console.log('Creating collective:', detail.name);
			console.log('Members:', detail.memberIds);

			// Create the collective target identifier
			const collectiveTargetId = createCollectiveTarget(detail.memberIds);
			console.log('Generated collective target:', collectiveTargetId);

			// Example: collective:04c63b1a...,05d74c2b...,06e85d3c...
			return {
				id: collectiveTargetId,
				name: detail.name,
				metadata: {
					type: 'collective',
					memberCount: detail.memberIds.length,
					members: detail.memberIds
				}
			};
		}
	};

	return dropdownProps;
}

/**
 * Example 8: Collective Target in Composition
 * Shows how collective targets work in slot composition
 */
export function example8_CollectiveComposition(): UserSlotCompositionData {
	// Three people working together
	const alice = '04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b';
	const bob = '05d74c2b3f9e8d6a4b7c9e2f3a5d7b9c8e6f8a3d5b7c9e1f4a6d8b0c2e5f7a9d';
	const charlie = '06e85d3c4f0e9d7a5b8c0e3f4a6d8b0c9e7f9a4d6b8c0e2f5a7d9b1c3e6f8a0d';

	// Create collective target
	const teamCollective = createCollectiveTarget([alice, bob, charlie]);

	return {
		project_workspace_456: {
			slot_friday_2pm: {
				[teamCollective]: {
					// Compose workspace time into the team collective
					meeting_room_789: 3 // 3 hours for the team meeting
				}
			}
		}
	};
}

/**
 * Example 9: Mixed Individual and Collective Composition
 * Shows complex composition with both individual and collective targets
 */
export function example9_MixedComposition(): UserSlotCompositionData {
	const alice = '04c63b1a2f8e9d7a3b5c8e1f2a4d6b9c8e5f7a2d4b6c9e8f1a3d5b7c9e2f4a6d8b';
	const bob = '05d74c2b3f9e8d6a4b7c9e2f3a5d7b9c8e6f8a3d5b7c9e1f4a6d8b0c2e5f7a9d';
	const designTeam = createCollectiveTarget([alice, bob]);

	return {
		mentorship_capacity_789: {
			slot_wednesday_10am: {
				// Individual mentoring for Alice (combined targets)
				[alice]: {
					personal_development_slot: 1,
					reflection_time_slot: 0.5 // Self-consumption
				},
				// Team mentoring for the design collective
				[designTeam]: {
					team_workshop_slot: 2
				}
			}
		}
	};
}

// Run all demonstrations
if (typeof window === 'undefined') {
	// Only run in Node.js context
	demonstrateTargetParsing();
	demonstrateCollectiveCreation();
	demonstrateSelfConsumption();
	demonstrateBackwardCompatibility();
}
