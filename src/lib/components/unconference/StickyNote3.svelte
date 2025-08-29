<script lang="ts">
	import { unconferenceState } from '$lib/state/unconference/unconference-reactive.svelte';
	import type { Note, Placement } from '$lib/schema/unconference';

	let { 
		note, 
		placement = null,
		onRemove = () => {},
		isInSidebar = false
	} = $props<{
		note: Note;
		placement?: Placement | null;
		onRemove?: () => void;
		isInSidebar?: boolean;
	}>();

	let isEditing = $state(false);
	let editContent = $state(note.content);
	let isDragging = $state(false);

	function handleDragStart(e: DragEvent) {
		if (!e.dataTransfer) return;
		
		e.dataTransfer.effectAllowed = 'move';
		e.dataTransfer.setData('noteId', note.id);
		isDragging = true;
		
		// Create a drag image
		const dragImage = document.createElement('div');
		dragImage.style.cssText = `
			position: absolute;
			top: -1000px;
			padding: 0.5rem;
			background: ${note.color};
			border-radius: 0.25rem;
			box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
			transform: rotate(-2deg);
			max-width: 200px;
			opacity: 0.9;
		`;
		dragImage.textContent = note.content.substring(0, 50);
		document.body.appendChild(dragImage);
		e.dataTransfer.setDragImage(dragImage, 50, 20);
		
		setTimeout(() => {
			document.body.removeChild(dragImage);
		}, 0);
	}

	function handleDragEnd(e: DragEvent) {
		isDragging = false;
	}

	function startEditing() {
		if (!isInSidebar) {
			isEditing = true;
			editContent = note.content;
		}
	}

	function saveEdit() {
		if (editContent.trim() && editContent !== note.content) {
			unconferenceState.updateNote(note.id, editContent.trim());
		}
		isEditing = false;
	}

	function cancelEdit() {
		editContent = note.content;
		isEditing = false;
	}

	function handleKeyDown(e: KeyboardEvent) {
		if (e.key === 'Enter' && !e.shiftKey) {
			e.preventDefault();
			saveEdit();
		} else if (e.key === 'Escape') {
			cancelEdit();
		}
	}

	function handleDelete() {
		if (confirm('Delete this note?')) {
			unconferenceState.deleteNote(note.id);
		}
	}

	const spanHeight = $derived(placement ? `span ${placement.spanSlots}` : undefined);
</script>

<div 
	class="sticky-note"
	class:editing={isEditing}
	class:dragging={isDragging}
	class:sidebar={isInSidebar}
	style:background-color={note.color}
	style:grid-row-end={spanHeight}
	draggable={!isEditing}
	ondragstart={handleDragStart}
	ondragend={handleDragEnd}
	ondblclick={startEditing}
>
	{#if isEditing}
		<textarea
			bind:value={editContent}
			onblur={saveEdit}
			onkeydown={handleKeyDown}
			class="edit-textarea"
		/>
	{:else}
		<div class="note-content">
			{note.content}
		</div>
	{/if}
	
	<div class="note-footer">
		{#if note.authorName}
			<span class="note-author">{note.authorName}</span>
		{/if}
		<div class="note-actions">
			{#if !isInSidebar && placement}
				<button 
					class="btn-action"
					onclick={onRemove}
					title="Remove from grid"
				>
					📌
				</button>
			{/if}
			<button 
				class="btn-action"
				onclick={handleDelete}
				title="Delete note"
			>
				🗑️
			</button>
		</div>
	</div>
</div>

<style>
	.sticky-note {
		padding: 0.5rem;
		border-radius: 0.25rem;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
		cursor: move;
		transition: all 0.2s ease;
		height: 100%;
		display: flex;
		flex-direction: column;
		position: relative;
		transform: rotate(-1deg);
		font-family: 'Comic Sans MS', cursive, sans-serif;
	}

	.sticky-note.sidebar {
		margin: 0.5rem;
		min-height: 80px;
		transform: rotate(0deg);
	}

	.sticky-note:hover {
		transform: rotate(0deg) scale(1.02);
		box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
		z-index: 10;
	}

	.sticky-note.dragging {
		opacity: 0.5;
		cursor: grabbing;
	}

	.sticky-note.editing {
		cursor: text;
		transform: rotate(0deg) scale(1.05);
		z-index: 20;
	}

	.note-content {
		flex: 1;
		font-size: 0.875rem;
		line-height: 1.4;
		color: #333;
		word-wrap: break-word;
		overflow: hidden;
		display: -webkit-box;
		-webkit-line-clamp: 4;
		-webkit-box-orient: vertical;
		margin-bottom: 0.5rem;
	}

	.sidebar .note-content {
		-webkit-line-clamp: 3;
	}

	.edit-textarea {
		flex: 1;
		width: 100%;
		background: rgba(255, 255, 255, 0.9);
		border: 1px solid rgba(0, 0, 0, 0.1);
		border-radius: 0.25rem;
		padding: 0.25rem;
		font-size: 0.875rem;
		font-family: inherit;
		resize: none;
		outline: none;
	}

	.note-footer {
		display: flex;
		justify-content: space-between;
		align-items: center;
		font-size: 0.7rem;
		opacity: 0.8;
		margin-top: auto;
	}

	.note-author {
		color: #555;
		font-style: italic;
		overflow: hidden;
		text-overflow: ellipsis;
		white-space: nowrap;
		max-width: 100px;
	}

	.note-actions {
		display: flex;
		gap: 0.25rem;
		opacity: 0;
		transition: opacity 0.2s;
	}

	.sticky-note:hover .note-actions {
		opacity: 1;
	}

	.btn-action {
		background: transparent;
		border: none;
		cursor: pointer;
		font-size: 0.875rem;
		padding: 0.125rem;
		transition: transform 0.2s;
		filter: grayscale(0.3);
	}

	.btn-action:hover {
		transform: scale(1.2);
		filter: grayscale(0);
	}
</style>