<script lang="ts">
	import { onMount } from 'svelte';

	interface CapacityEntry {
		id: string;
		name: string;
		quantity: number;
		unit: string;
		depth: number;
	}

	let capacityEntries = $state<CapacityEntry[]>([createCapacityEntry()]);

	// Create a new capacity entry row
	function createCapacityEntry(): CapacityEntry {
		return {
			id: crypto.randomUUID(),
			name: '',
			quantity: 0,
			unit: '',
			depth: 3
		};
	}

	// Green color scale for depth
	const colors = ['#dcfce7', '#86efac', '#22c55e', '#15803d', '#14532d'];

	function setEntryDepth(entries: CapacityEntry[], entryId: string, newDepth: number) {
		const idx = entries.findIndex((e) => e.id === entryId);
		if (idx !== -1) {
			entries[idx] = { ...entries[idx], depth: newDepth };
		}
	}

	function addCapacityRow() {
		capacityEntries = [...capacityEntries, createCapacityEntry()];
	}

	function removeCapacityRow(entryId: string) {
		if (capacityEntries.length === 1) return; // Always keep at least one row
		capacityEntries = capacityEntries.filter((e) => e.id !== entryId);
	}

	onMount(() => {
		// Optionally initialize demo data here
	});
</script>

<!-- <SixDegrees /> -->

<div class="inventory-list grid grid-cols-1 gap-3 p-2 md:grid-cols-2 lg:grid-cols-3">
	{#each capacityEntries as entry, i (entry.id)}
		<div class="capacity-row flex items-center gap-2 rounded bg-white p-2 shadow-sm">
			<input
				type="text"
				class="capacity-input name min-w-0 flex-1"
				bind:value={entry.name}
				placeholder="Name"
			/>
			<input
				type="number"
				class="capacity-input qty w-16 text-right"
				min="0"
				step="0.01"
				bind:value={entry.quantity}
				placeholder="Qty"
			/>
			<input
				type="text"
				class="capacity-input unit w-20"
				bind:value={entry.unit}
				placeholder="Unit"
			/>
			<div class="depth-bar ml-2 flex gap-1">
				{#each Array(5) as _, j}
					<span
						class="depth-dot {j < entry.depth ? 'active' : ''}"
						style="background: {j < entry.depth ? colors[j] : '#e5e7eb'}"
						onclick={() => {
							setEntryDepth(capacityEntries, entry.id, j + 1);
						}}
					></span>
				{/each}
			</div>
			<button
				type="button"
				class="remove-btn ml-2"
				onclick={() => removeCapacityRow(entry.id)}
				disabled={capacityEntries.length === 1}>×</button
			>
		</div>
	{/each}
	<button type="button" class="add-btn mx-auto my-2 h-10 w-10" onclick={addCapacityRow}>+</button>
</div>

<style>
	:global(body) {
		font-family: 'Inter', system-ui, sans-serif;
		background: #f7fafc;
	}

	.capacity-input {
		font-size: 1rem;
		padding: 6px 8px;
		border: none;
		border-bottom: 1.5px solid #e5e7eb;
		background: transparent;
		outline: none;
		transition: border-color 0.2s;
		min-width: 0;
	}
	.capacity-input::placeholder {
		color: #cbd5e1;
	}
	.capacity-input:focus {
		border-bottom: 1px solid #3b82f6; /* Keep focus color */
	}

	.depth-dot {
		width: 15px; /* Adjusted size */
		height: 15px;
		border-radius: 50%;
		background: #e5e7eb;
		display: inline-block;
		margin-right: 0;
		transition:
			background 0.2s,
			border 0.2s,
			box-shadow 0.2s;
		cursor: pointer;
		border: 1px solid #e5e7eb; /* Thinner border */
	}
	.depth-dot.active {
		border: 1px solid #16a34a; /* Green active border */
		box-shadow: 0 0 0 2px #bbf7d0; /* Lighter green glow */
	}
	.depth-dot:hover {
		border: 1px solid #15803d; /* Darker green hover */
	}

	.add-btn {
		background: #f0fdf4; /* Light green */
		color: #16a34a; /* Green */
		border: none;
		border-radius: 50%;
		font-size: 1.3em; /* Adjusted */
		font-weight: 500;
		cursor: pointer;
		transition:
			background 0.2s,
			color 0.2s;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
		display: flex;
		align-items: center;
		justify-content: center;
	}
	.add-btn:hover {
		background: #dcfce7; /* Slightly darker green */
		color: #15803d;
	}

	.remove-btn {
		background: none;
		border: none;
		color: #cbd5e1; /* Subtle remove color */
		font-size: 1.3em; /* Larger click target */
		cursor: pointer;
		padding: 0 4px;
		line-height: 1;
		border-radius: 50%;
		transition:
			background 0.2s,
			color 0.2s;
		display: flex;
		align-items: center;
		justify-content: center;
		width: 24px;
		height: 24px;
	}
	.remove-btn:disabled {
		color: #e5e7eb;
		cursor: not-allowed;
	}
	.remove-btn:hover:not(:disabled) {
		background: #fef2f2;
		color: #ef4444; /* Red on hover */
	}
</style>
