<script lang="ts">
	import { onMount } from 'svelte';

	interface CapacityShare {
		id: string;
		name: string;
		quantity: number;
		unit: string;
		depth: number;
		provider: string;
	}

	let capacityShares = $state<CapacityShare[]>([createCapacityShare()]);

	// Create a new capacity share row
	function createCapacityShare(): CapacityShare {
		return {
			id: crypto.randomUUID(),
			name: '',
			quantity: 0,
			unit: '',
			depth: 2,
			provider: 'Unknown'
		};
	}

	// Green color scale for depth
	const colors = ['#22c55e', '#86efac', '#dcfce7'];

	function getDepthColor(depth: number): string {
		if (depth <= 0) return '#f3f4f6';
		return depth <= colors.length ? colors[depth - 1] : colors[colors.length - 1];
	}

	function handleProviderClick(provider: string) {
		// This function will be implemented later to navigate to the provider
		console.log(`Navigating to provider: ${provider}`);
		// Could display a toast or navigation action here
	}

	onMount(() => {
		// Initialize with 25 demo capacity shares
		const demoShares: CapacityShare[] = [
			{
				id: crypto.randomUUID(),
				name: 'Potable Water',
				quantity: 50,
				unit: 'gallons',
				depth: 3,
				provider: 'Jane Doe'
			},
			{
				id: crypto.randomUUID(),
				name: 'Rice',
				quantity: 20,
				unit: 'lbs',
				depth: 3,
				provider: 'Community Pantry'
			},
			{
				id: crypto.randomUUID(),
				name: 'Beans',
				quantity: 15,
				unit: 'lbs',
				depth: 3,
				provider: 'Community Pantry'
			},
			{
				id: crypto.randomUUID(),
				name: 'First Aid Kit',
				quantity: 2,
				unit: 'kits',
				depth: 3,
				provider: 'Medical Group'
			},
			{
				id: crypto.randomUUID(),
				name: 'Solar Panel',
				quantity: 3,
				unit: 'panels',
				depth: 3,
				provider: 'Green Energy Co-op'
			},
			{
				id: crypto.randomUUID(),
				name: 'Garden Seeds',
				quantity: 25,
				unit: 'packets',
				depth: 3,
				provider: 'Seed Exchange'
			},
			{
				id: crypto.randomUUID(),
				name: 'Cooking Capacity',
				quantity: 4,
				unit: 'meals/day',
				depth: 3,
				provider: 'Community Kitchen'
			},
			{
				id: crypto.randomUUID(),
				name: 'Spare Blankets',
				quantity: 8,
				unit: 'blankets',
				depth: 3,
				provider: 'Shelter Group'
			},
			{
				id: crypto.randomUUID(),
				name: 'Extra Shelter Space',
				quantity: 2,
				unit: 'people',
				depth: 3,
				provider: 'Housing Collective'
			},
			{
				id: crypto.randomUUID(),
				name: 'Water Filtration',
				quantity: 1,
				unit: 'system',
				depth: 3,
				provider: 'Clean Water Project'
			},
			{
				id: crypto.randomUUID(),
				name: 'Board Games',
				quantity: 6,
				unit: 'games',
				depth: 1,
				provider: 'Recreation Circle'
			},
			{
				id: crypto.randomUUID(),
				name: 'Musical Instruments',
				quantity: 3,
				unit: 'instruments',
				depth: 2,
				provider: 'Arts Collective'
			},
			{
				id: crypto.randomUUID(),
				name: 'Storytelling Capacity',
				quantity: 100,
				unit: 'stories',
				depth: 2,
				provider: 'John Smith'
			},
			{
				id: crypto.randomUUID(),
				name: 'Candles',
				quantity: 24,
				unit: 'candles',
				depth: 3,
				provider: 'Craft Co-op'
			},
			{
				id: crypto.randomUUID(),
				name: 'Honey',
				quantity: 5,
				unit: 'jars',
				depth: 3,
				provider: 'Local Beekeeper'
			},
			{
				id: crypto.randomUUID(),
				name: 'Childcare Capacity',
				quantity: 10,
				unit: 'hours/week',
				depth: 3,
				provider: 'Parents Collective'
			},
			{
				id: crypto.randomUUID(),
				name: 'Herbal Medicine',
				quantity: 8,
				unit: 'varieties',
				depth: 3,
				provider: 'Herbalist Network'
			},
			{
				id: crypto.randomUUID(),
				name: 'Yoga Instruction',
				quantity: 3,
				unit: 'sessions/week',
				depth: 1,
				provider: 'Wellness Circle'
			},
			{
				id: crypto.randomUUID(),
				name: 'Canned Vegetables',
				quantity: 36,
				unit: 'cans',
				depth: 3,
				provider: 'Urban Farm'
			},
			{
				id: crypto.randomUUID(),
				name: 'Emergency Radio',
				quantity: 1,
				unit: 'radio',
				depth: 3,
				provider: 'Communications Team'
			},
			{
				id: crypto.randomUUID(),
				name: 'Hand Tools',
				quantity: 12,
				unit: 'tools',
				depth: 3,
				provider: 'Tool Library'
			},
			{
				id: crypto.randomUUID(),
				name: 'Comedy Relief',
				quantity: 42,
				unit: 'jokes',
				depth: 1,
				provider: 'Entertainment Group'
			},
			{
				id: crypto.randomUUID(),
				name: 'Extra Bicycles',
				quantity: 2,
				unit: 'bikes',
				depth: 3,
				provider: 'Bike Collective'
			},
			{
				id: crypto.randomUUID(),
				name: 'Pasta',
				quantity: 10,
				unit: 'lbs',
				depth: 3,
				provider: 'Food Pantry'
			},
			{
				id: crypto.randomUUID(),
				name: 'Meditation Space',
				quantity: 1,
				unit: 'space',
				depth: 2,
				provider: 'Mindfulness Center'
			}
		];

		capacityShares = demoShares;
	});
</script>

<div class="shares-list grid grid-cols-1 gap-3 p-2 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4">
	{#each capacityShares as share (share.id)}
		<div
			class="capacity-share flex items-center justify-between rounded p-2 shadow-sm"
			style="background-color: {getDepthColor(share.depth)}; border: 1px solid #e5e7eb;"
		>
			<div class="flex min-w-0 flex-1 flex-col pr-2">
				<span class="share-value name overflow-hidden font-medium text-ellipsis whitespace-nowrap"
					>{share.name}</span
				>
				<span class="share-value qty text-sm">{share.quantity} {share.unit}</span>
			</div>
			<button
				type="button"
				class="provider-btn rounded-md px-2 py-1 text-xs whitespace-nowrap"
				onclick={() => handleProviderClick(share.provider)}
			>
				{share.provider}
			</button>
		</div>
	{/each}
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.share-value {
		min-width: 0;
		line-height: 1.2;
	}

	.provider-btn {
		background: rgba(255, 255, 255, 0.5);
		color: #4b5563;
		border: none;
		transition:
			background 0.2s,
			color 0.2s;
		cursor: pointer;
		flex-shrink: 0;
	}

	.provider-btn:hover {
		background: rgba(255, 255, 255, 0.8);
		color: #1f2937;
	}
</style>
