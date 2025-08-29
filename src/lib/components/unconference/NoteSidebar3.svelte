<script lang="ts">
	import { unconferenceState } from '$lib/state/unconference/unconference-reactive.svelte';
	import StickyNote3 from './StickyNote3.svelte';
	import NoteEditor from './NoteEditor.svelte';

	let showEditor = $state(false);
	let searchQuery = $state('');

	// Filter unplaced notes by search query
	const filteredNotes = $derived.by(() => {
		const notes = unconferenceState.unplacedNotes;
		if (!searchQuery) return notes;
		
		const query = searchQuery.toLowerCase();
		return notes.filter(note =>
			note.content.toLowerCase().includes(query) ||
			(note.authorName && note.authorName.toLowerCase().includes(query))
		);
	});

	function handleSaveNote(content: string, color: string) {
		unconferenceState.createNote(content, color);
		showEditor = false;
	}
</script>

<div class="sidebar-container">
	<div class="sidebar-header">
		<h3>Session Ideas</h3>
		<div class="note-stats">
			{unconferenceState.placedNotesCount} scheduled / {unconferenceState.totalNotesCount} total
		</div>
	</div>

	<div class="sidebar-controls">
		<button 
			class="btn-add-note"
			onclick={() => showEditor = true}
		>
			➕ Add Session Idea
		</button>
		
		<input 
			type="search"
			bind:value={searchQuery}
			placeholder="Search notes..."
			class="search-input"
		/>
	</div>

	<div class="notes-list">
		{#if filteredNotes.length === 0}
			<div class="empty-state">
				{#if searchQuery}
					<p>No notes match your search.</p>
				{:else if unconferenceState.totalNotesCount === 0}
					<p>No session ideas yet.</p>
					<p>Click "Add Session Idea" to create one!</p>
				{:else}
					<p>All notes are scheduled!</p>
				{/if}
			</div>
		{:else}
			<div class="notes-info">
				Drag notes to the schedule grid →
			</div>
			{#each filteredNotes as note (note.id)}
				<StickyNote3
					{note}
					isInSidebar={true}
				/>
			{/each}
		{/if}
	</div>
</div>

{#if showEditor}
	<NoteEditor 
		onClose={() => showEditor = false}
		onSave={handleSaveNote}
	/>
{/if}

<style>
	.sidebar-container {
		height: 100%;
		display: flex;
		flex-direction: column;
	}

	.sidebar-header {
		padding: 1rem;
		border-bottom: 1px solid #e0e0e0;
		background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
	}

	.sidebar-header h3 {
		margin: 0 0 0.25rem 0;
		font-size: 1.125rem;
		color: #333;
	}

	.note-stats {
		font-size: 0.875rem;
		color: #666;
	}

	.sidebar-controls {
		padding: 1rem;
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
		border-bottom: 1px solid #e0e0e0;
	}

	.btn-add-note {
		padding: 0.75rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border: none;
		border-radius: 0.375rem;
		font-size: 0.95rem;
		font-weight: 500;
		cursor: pointer;
		transition: all 0.2s;
		box-shadow: 0 2px 4px rgba(102, 126, 234, 0.2);
	}

	.btn-add-note:hover {
		transform: translateY(-1px);
		box-shadow: 0 4px 8px rgba(102, 126, 234, 0.3);
	}

	.btn-add-note:active {
		transform: translateY(0);
	}

	.search-input {
		padding: 0.5rem;
		border: 1px solid #ddd;
		border-radius: 0.375rem;
		font-size: 0.875rem;
		transition: border-color 0.2s;
	}

	.search-input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
	}

	.notes-list {
		flex: 1;
		overflow-y: auto;
		padding: 0.5rem;
		background: #fafafa;
	}

	.notes-info {
		padding: 0.5rem;
		margin-bottom: 0.5rem;
		background: #e8f4f8;
		border-radius: 0.25rem;
		font-size: 0.8rem;
		color: #4a90e2;
		text-align: center;
	}

	.empty-state {
		padding: 2rem 1rem;
		text-align: center;
		color: #999;
		font-size: 0.875rem;
	}

	.empty-state p {
		margin: 0.25rem 0;
	}

	.notes-list::-webkit-scrollbar {
		width: 6px;
	}

	.notes-list::-webkit-scrollbar-track {
		background: #f1f1f1;
		border-radius: 3px;
	}

	.notes-list::-webkit-scrollbar-thumb {
		background: #888;
		border-radius: 3px;
	}

	.notes-list::-webkit-scrollbar-thumb:hover {
		background: #555;
	}
</style>