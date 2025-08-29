<script lang="ts">
	import { unconferenceState } from '$lib/state/unconference/unconference.svelte';
	import StickyNote from './StickyNote.svelte';

	interface Room {
		id: string;
		name: string;
	}

	interface Placement {
		noteId: string;
		roomId: string;
		slotIndex: number;
		spanSlots: number;
	}

	let { 
		room, 
		slotIndex, 
		placement = null,
		showContent = true
	} = $props<{
		room: Room;
		slotIndex: number;
		placement: Placement | null;
		showContent: boolean;
	}>();

	let isDragOver = $state(false);
	let isValidDrop = $state(false);

	function handleDragOver(e: DragEvent) {
		e.preventDefault();
		e.stopPropagation();
		
		if (unconferenceState.draggedNote) {
			isDragOver = true;
			isValidDrop = !unconferenceState.hasConflict(
				room.id, 
				slotIndex, 
				1, 
				unconferenceState.draggedNote.id
			);
		}
	}

	function handleDragLeave(e: DragEvent) {
		isDragOver = false;
		isValidDrop = false;
	}

	function handleDrop(e: DragEvent) {
		e.preventDefault();
		e.stopPropagation();
		
		const noteId = e.dataTransfer?.getData('noteId');
		if (noteId) {
			const success = unconferenceState.placeNote(noteId, room.id, slotIndex, 1);
			if (!success) {
				console.warn('Failed to place note - conflict detected');
			}
		}
		
		isDragOver = false;
		isValidDrop = false;
		unconferenceState.stopDragging();
	}

	const note = $derived(placement ? unconferenceState.notes.get(placement.noteId) : null);
	const spanSlots = $derived(placement?.spanSlots || 1);
</script>

<div 
	class="grid-cell"
	class:occupied={placement !== null}
	class:drag-over={isDragOver}
	class:valid-drop={isValidDrop}
	class:invalid-drop={isDragOver && !isValidDrop}
	class:spanned={placement && !showContent}
	ondragover={handleDragOver}
	ondragleave={handleDragLeave}
	ondrop={handleDrop}
	style:grid-row-end={showContent && spanSlots > 1 ? `span ${spanSlots}` : undefined}
>
	{#if placement && note && showContent}
		<StickyNote 
			{note}
			{placement}
			onRemove={() => {
				const placementId = `${placement.noteId}_${placement.roomId}_${placement.slotIndex}`;
				unconferenceState.removePlacement(placementId);
			}}
		/>
	{/if}
</div>

<style>
	.grid-cell {
		background: white;
		min-height: 60px;
		position: relative;
		transition: all 0.2s ease;
		border: 1px solid transparent;
	}

	.grid-cell.spanned {
		display: none;
	}

	.grid-cell.occupied {
		background: #fafafa;
	}

	.grid-cell.drag-over {
		background: #e8f4f8;
		border-color: #4a90e2;
		transform: scale(1.02);
		z-index: 5;
	}

	.grid-cell.valid-drop {
		background: #d4f4dd;
		border-color: #52c41a;
		box-shadow: 0 0 0 2px rgba(82, 196, 26, 0.2);
	}

	.grid-cell.invalid-drop {
		background: #fff1f0;
		border-color: #ff4d4f;
		box-shadow: 0 0 0 2px rgba(255, 77, 79, 0.2);
		cursor: not-allowed;
	}

	.grid-cell:hover:not(.occupied) {
		background: #f8f9fa;
		border-color: #ddd;
	}
</style>