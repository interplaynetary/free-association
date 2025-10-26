<script lang="ts">
	import { onMount } from 'svelte';
	import Visualization from '$lib/commons/components/Visualization.svelte';
	import {
		myCommitmentStore,
		myAllocationStateStore,
		myRecognitionWeightsStore,
		networkCommitments,
		networkAllocationStates,
		networkRecognitionWeights,
		initializeAllocationStores
	} from '$lib/commons/stores.svelte';
	import { holsterUserPub } from '$lib/state/holster.svelte';
	import type { Commitment, TwoTierAllocationState } from '$lib/commons/schemas';
	
	// UI state
	let selectedResourceType = $state<string | undefined>(undefined);
	let availableResourceTypes = $state<string[]>([]);
	
	const TEST_USER_PUBKEY = 'test-user-pub-key-12345';
	
	// Initialize mock data for testing
	function setupMockData() {
		// Set a mock public key via holster store
		holsterUserPub.set(TEST_USER_PUBKEY);
		
		// Create my commitment with multiple resource types
		const myCommitment: Commitment = {
			capacity_slots: [
				{
					id: 'tutoring-slot-1',
					name: 'Math Tutoring',
					quantity: 100,
					resource_type: 'tutoring',
					emoji: '📐',
					unit: 'hours'
				},
				{
					id: 'housing-slot-1',
					name: 'Guest Room',
					quantity: 50,
					resource_type: 'housing',
					emoji: '🏠',
					unit: 'nights'
				},
				{
					id: 'money-slot-1',
					name: 'Financial Support',
					quantity: 200,
					resource_type: 'money',
					emoji: '💰',
					unit: 'dollars'
				}
			],
			need_slots: [
				{
					id: 'need-childcare-1',
					name: 'Childcare Hours',
					quantity: 30,
					resource_type: 'childcare',
					emoji: '👶',
					unit: 'hours'
				}
			],
			mr_values: {
				'peer-alice': 0.8,
				'peer-bob': 0.6
			},
			recognition_weights: {
				'peer-alice': 0.5,
				'peer-bob': 0.3,
				'peer-charlie': 0.2
			},
			timestamp: Date.now()
		};
		
		myCommitmentStore.set(myCommitment);
		
		// Create my allocation state
		const myAllocationState: TwoTierAllocationState = {
			slot_denominators: {
				'tutoring-slot-1': { mutual: 120, nonMutual: 50 },
				'housing-slot-1': { mutual: 60, nonMutual: 20 },
				'money-slot-1': { mutual: 250, nonMutual: 100 }
			},
			slot_allocations: [
				{
					availability_slot_id: 'tutoring-slot-1',
					recipient_pubkey: 'peer-alice',
					quantity: 60,
					tier: 'mutual',
					time_compatible: true,
					location_compatible: true
				},
				{
					availability_slot_id: 'tutoring-slot-1',
					recipient_pubkey: 'peer-bob',
					quantity: 40,
					tier: 'mutual',
					time_compatible: true,
					location_compatible: true
				},
				{
					availability_slot_id: 'housing-slot-1',
					recipient_pubkey: 'peer-alice',
					quantity: 30,
					tier: 'mutual',
					time_compatible: true,
					location_compatible: true
				},
				{
					availability_slot_id: 'housing-slot-1',
					recipient_pubkey: 'peer-charlie',
					quantity: 20,
					tier: 'non-mutual',
					time_compatible: true,
					location_compatible: true
				},
				{
					availability_slot_id: 'money-slot-1',
					recipient_pubkey: 'peer-bob',
					quantity: 100,
					tier: 'mutual',
					time_compatible: true,
					location_compatible: true
				},
				{
					availability_slot_id: 'money-slot-1',
					recipient_pubkey: 'peer-charlie',
					quantity: 100,
					tier: 'non-mutual',
					time_compatible: true,
					location_compatible: true
				}
			],
			recipient_totals: {
				'peer-alice': 90,
				'peer-bob': 140,
				'peer-charlie': 120
			},
			timestamp: Date.now()
		};
		
		myAllocationStateStore.set(myAllocationState);
		
		// Set recognition weights
		myRecognitionWeightsStore.set({
			'peer-alice': 0.5,
			'peer-bob': 0.3,
			'peer-charlie': 0.2
		});
		
		// Add network participants
		// Alice - provides childcare, needs tutoring
		const aliceCommitment: Commitment = {
			capacity_slots: [
				{
					id: 'alice-childcare-1',
					name: 'After-school Care',
					quantity: 80,
					resource_type: 'childcare',
					emoji: '👶',
					unit: 'hours'
				}
			],
			need_slots: [
				{
					id: 'alice-need-tutoring-1',
					name: 'Math Tutoring',
					quantity: 60,
					resource_type: 'tutoring',
					emoji: '📐',
					unit: 'hours'
				}
			],
			mr_values: {
				[TEST_USER_PUBKEY]: 0.8,
				'peer-bob': 0.4
			},
			recognition_weights: {
				[TEST_USER_PUBKEY]: 0.6,
				'peer-bob': 0.4
			},
			timestamp: Date.now()
		};
		
		const aliceAllocationState: TwoTierAllocationState = {
			slot_denominators: {
				'alice-childcare-1': { mutual: 40, nonMutual: 10 }
			},
			slot_allocations: [
				{
					availability_slot_id: 'alice-childcare-1',
					recipient_pubkey: TEST_USER_PUBKEY,
					quantity: 30,
					tier: 'mutual',
					time_compatible: true,
					location_compatible: true
				}
			],
			recipient_totals: {
				[TEST_USER_PUBKEY]: 30
			},
			timestamp: Date.now()
		};
		
		networkCommitments.set('peer-alice', aliceCommitment);
		networkAllocationStates.set('peer-alice', aliceAllocationState);
		
		// Bob - provides housing, needs money
		const bobCommitment: Commitment = {
			capacity_slots: [
				{
					id: 'bob-housing-1',
					name: 'Spare Room',
					quantity: 40,
					resource_type: 'housing',
					emoji: '🏠',
					unit: 'nights'
				}
			],
			need_slots: [
				{
					id: 'bob-need-money-1',
					name: 'Financial Help',
					quantity: 100,
					resource_type: 'money',
					emoji: '💰',
					unit: 'dollars'
				}
			],
			mr_values: {
				[TEST_USER_PUBKEY]: 0.6
			},
			recognition_weights: {
				[TEST_USER_PUBKEY]: 0.7
			},
			timestamp: Date.now()
		};
		
		const bobAllocationState: TwoTierAllocationState = {
			slot_denominators: {
				'bob-housing-1': { mutual: 30, nonMutual: 15 }
			},
			slot_allocations: [],
			recipient_totals: {},
			timestamp: Date.now()
		};
		
		networkCommitments.set('peer-bob', bobCommitment);
		networkAllocationStates.set('peer-bob', bobAllocationState);
		
		// Charlie - needs housing and tutoring, no capacity
		const charlieCommitment: Commitment = {
			need_slots: [
				{
					id: 'charlie-need-housing-1',
					name: 'Housing',
					quantity: 20,
					resource_type: 'housing',
					emoji: '🏠',
					unit: 'nights'
				},
				{
					id: 'charlie-need-tutoring-1',
					name: 'Tutoring',
					quantity: 40,
					resource_type: 'tutoring',
					emoji: '📐',
					unit: 'hours'
				}
			],
			mr_values: {},
			recognition_weights: {},
			timestamp: Date.now()
		};
		
		networkCommitments.set('peer-charlie', charlieCommitment);
		
		// Extract available resource types
		const types = new Set<string>();
		
		// From my commitment
		myCommitment.capacity_slots?.forEach(slot => {
			if (slot.resource_type) types.add(slot.resource_type);
		});
		myCommitment.need_slots?.forEach(slot => {
			if (slot.resource_type) types.add(slot.resource_type);
		});
		
		// From network commitments
		for (const [_, commitment] of networkCommitments) {
			commitment.capacity_slots?.forEach(slot => {
				if (slot.resource_type) types.add(slot.resource_type);
			});
			commitment.need_slots?.forEach(slot => {
				if (slot.resource_type) types.add(slot.resource_type);
			});
		}
		
		availableResourceTypes = Array.from(types).sort();
		
		console.log('[VIZ-TEST] Mock data setup complete', {
			myPubKey: TEST_USER_PUBKEY,
			resourceTypes: availableResourceTypes
		});
	}
	
	onMount(() => {
		setupMockData();
	});
</script>

<div class="test-container">
	<div class="controls-panel">
		<h2>Visualization Test Controls</h2>
		
		<div class="control-section">
			<label for="resource-type">Resource Type Filter:</label>
			<select 
				id="resource-type" 
				bind:value={selectedResourceType}
			>
				<option value={undefined}>All Types</option>
				{#each availableResourceTypes as type}
					<option value={type}>{type}</option>
				{/each}
			</select>
		</div>
		
		<div class="info-section">
			<h3>Mock Data:</h3>
			<ul>
				<li><strong>Me:</strong> Provides tutoring (100), housing (50), money (200) • Needs childcare (30)</li>
				<li><strong>Alice:</strong> Provides childcare (80) • Needs tutoring (60)</li>
				<li><strong>Bob:</strong> Provides housing (40) • Needs money (100)</li>
				<li><strong>Charlie:</strong> No capacity • Needs housing (20), tutoring (40)</li>
			</ul>
			
			<h3>Mutual Recognition:</h3>
			<ul>
				<li>Me ↔ Alice: 0.8</li>
				<li>Me ↔ Bob: 0.6</li>
			</ul>
		</div>
	</div>
	
	<div class="viz-section">
		<Visualization 
			width={900} 
			height={700} 
			resourceType={selectedResourceType}
		/>
	</div>
</div>

<style>
	.test-container {
		display: flex;
		flex-direction: column;
		gap: 2rem;
		padding: 2rem;
		background: #0f172a;
		min-height: 100vh;
		color: white;
	}
	
	.controls-panel {
		background: rgba(30, 41, 59, 0.5);
		border: 1px solid #475569;
		border-radius: 0.75rem;
		padding: 1.5rem;
	}
	
	.controls-panel h2 {
		margin-bottom: 1rem;
		color: #f1f5f9;
		font-size: 1.5rem;
	}
	
	.controls-panel h3 {
		margin-top: 1rem;
		margin-bottom: 0.5rem;
		color: #cbd5e1;
		font-size: 1.125rem;
	}
	
	.control-section {
		margin-bottom: 1rem;
	}
	
	.control-section label {
		display: block;
		margin-bottom: 0.5rem;
		color: #cbd5e1;
		font-weight: 600;
	}
	
	.control-section select {
		width: 100%;
		max-width: 300px;
		padding: 0.5rem;
		background: #1e293b;
		color: white;
		border: 1px solid #475569;
		border-radius: 0.375rem;
		font-size: 1rem;
	}
	
	.control-section select:focus {
		outline: none;
		border-color: #60a5fa;
	}
	
	.info-section ul {
		list-style-position: inside;
		color: #94a3b8;
		font-size: 0.875rem;
	}
	
	.info-section li {
		margin-bottom: 0.25rem;
	}
	
	.info-section strong {
		color: #e2e8f0;
	}
	
	.viz-section {
		flex: 1;
	}
</style>

