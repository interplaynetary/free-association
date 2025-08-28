<script lang="ts">
	import { unconferenceState } from '$lib/state/unconference-reactive.svelte';
	import GridCell3 from './GridCell3.svelte';

	let { boardId } = $props<{ boardId: string }>();

	let gridElement: HTMLDivElement;
	let isDraggingOver = $state(false);

	function handleDragOver(e: DragEvent) {
		e.preventDefault();
		isDraggingOver = true;
	}

	function handleDragLeave(e: DragEvent) {
		if (e.target === gridElement) {
			isDraggingOver = false;
		}
	}

	function handleDrop(e: DragEvent) {
		e.preventDefault();
		isDraggingOver = false;
	}
</script>

<div class="grid-container">
	<div class="grid-header">
		<h2>{unconferenceState.board.title}</h2>
		<div class="grid-controls">
			<button class="btn-settings" onclick={() => console.log('Settings')}>
				⚙️ Settings
			</button>
		</div>
	</div>

	<div 
		class="grid-wrapper"
		bind:this={gridElement}
		ondragover={handleDragOver}
		ondragleave={handleDragLeave}
		ondrop={handleDrop}
		class:dragging-over={isDraggingOver}
	>
		<div class="grid" style="--room-count: {unconferenceState.sortedRooms.length}">
			<!-- Time header row -->
			<div class="grid-corner"></div>
			{#each unconferenceState.sortedRooms as room}
				<div class="room-header">
					<div class="room-name">{room.name}</div>
					{#if room.capacity}
						<div class="room-capacity">Max: {room.capacity}</div>
					{/if}
				</div>
			{/each}

			<!-- Grid rows -->
			{#each unconferenceState.timeSlots as timeSlot, slotIndex}
				<div class="time-label">
					{unconferenceState.formatTime(timeSlot)}
				</div>
				
				{#each unconferenceState.sortedRooms as room}
					{@const placement = unconferenceState.getPlacementAt(room.id, slotIndex)}
					{@const isFirstCell = !placement || placement.slotIndex === slotIndex}
					
					<GridCell3
						{room}
						{slotIndex}
						{placement}
						showContent={isFirstCell}
					/>
				{/each}
			{/each}
		</div>
	</div>
</div>

<style>
	.grid-container {
		height: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
	}

	.grid-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 1rem;
		border-bottom: 1px solid #e0e0e0;
		background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
	}

	.grid-header h2 {
		margin: 0;
		font-size: 1.5rem;
		font-weight: 600;
		color: #333;
	}

	.grid-controls {
		display: flex;
		gap: 0.5rem;
	}

	.btn-settings {
		padding: 0.5rem 1rem;
		background: white;
		border: 1px solid #ddd;
		border-radius: 0.25rem;
		cursor: pointer;
		font-size: 0.875rem;
		transition: all 0.2s;
	}

	.btn-settings:hover {
		background: #f8f9fa;
		border-color: #999;
	}

	.grid-wrapper {
		flex: 1;
		overflow: auto;
		padding: 1rem;
		position: relative;
		transition: background-color 0.2s;
	}

	.grid-wrapper.dragging-over {
		background-color: #f0f8ff;
	}

	.grid {
		display: grid;
		grid-template-columns: 100px repeat(var(--room-count, 5), minmax(150px, 1fr));
		gap: 1px;
		background: #e0e0e0;
		border: 1px solid #e0e0e0;
		min-width: max-content;
	}

	.grid-corner {
		background: #f5f5f5;
		border-right: 2px solid #ccc;
		border-bottom: 2px solid #ccc;
	}

	.room-header {
		background: linear-gradient(180deg, #4a90e2 0%, #357abd 100%);
		color: white;
		padding: 0.75rem;
		font-weight: 600;
		text-align: center;
		border-bottom: 2px solid #2968a3;
		position: sticky;
		top: 0;
		z-index: 10;
	}

	.room-name {
		font-size: 0.95rem;
		margin-bottom: 0.25rem;
	}

	.room-capacity {
		font-size: 0.75rem;
		opacity: 0.9;
		font-weight: 400;
	}

	.time-label {
		background: linear-gradient(90deg, #f5f5f5 0%, #ebebeb 100%);
		padding: 0.5rem;
		font-size: 0.875rem;
		font-weight: 500;
		color: #555;
		border-right: 2px solid #ccc;
		display: flex;
		align-items: center;
		justify-content: center;
		white-space: nowrap;
	}

	@media (max-width: 768px) {
		.grid {
			font-size: 0.875rem;
		}

		.time-label {
			font-size: 0.75rem;
			padding: 0.25rem;
		}

		.room-header {
			padding: 0.5rem;
		}
	}
</style>