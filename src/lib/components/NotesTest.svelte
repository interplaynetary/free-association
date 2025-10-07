<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { user } from '$lib/state/holster.svelte';
	import {
		notes,
		notesArray,
		isLoadingNotes,
		initializeNotes,
		cleanupNotes,
		createNote,
		updateNote,
		deleteNote
	} from '$lib/state/notes.svelte';

	let newNoteTitle = '';
	let newNoteContent = '';
	let editingNoteId: string | null = null;
	let editTitle = '';
	let editContent = '';

	onMount(() => {
		// Initialize if user is already logged in (via recall or active session)
		if (user.is) {
			initializeNotes();
		} else {
			// Try to recall user from stored credentials
			user.recall();
			// Give recall a moment to complete, then check again
			setTimeout(() => {
				if (user.is) {
					initializeNotes();
				}
			}, 100);
		}
	});

	onDestroy(() => {
		cleanupNotes();
	});

	async function handleCreateNote() {
		if (!newNoteTitle.trim()) return;

		try {
			await createNote(newNoteTitle, newNoteContent);
			newNoteTitle = '';
			newNoteContent = '';
		} catch (err) {
			console.error('[NOTES-UI] Create failed:', err);
			alert('Failed to create note: ' + err);
		}
	}

	function startEdit(noteId: string, title: string, content: string) {
		editingNoteId = noteId;
		editTitle = title;
		editContent = content;
	}

	async function handleUpdateNote() {
		if (!editingNoteId) return;

		try {
			await updateNote(editingNoteId, {
				title: editTitle,
				content: editContent
			});
			editingNoteId = null;
			editTitle = '';
			editContent = '';
		} catch (err) {
			console.error('[NOTES-UI] Update failed:', err);
			alert('Failed to update note: ' + err);
		}
	}

	function cancelEdit() {
		editingNoteId = null;
		editTitle = '';
		editContent = '';
	}

	async function handleDeleteNote(noteId: string) {
		if (!confirm('Delete this note?')) return;

		try {
			await deleteNote(noteId);
		} catch (err) {
			console.error('[NOTES-UI] Delete failed:', err);
			alert('Failed to delete note: ' + err);
		}
	}

	function handleInitialize() {
		initializeNotes();
	}
</script>

<div class="notes-test">
	<h2>Notes Test (Holster)</h2>

	{#if !user.is}
		<p class="warning">Please log in to use notes</p>
	{:else if $isLoadingNotes}
		<p>Loading notes...</p>
	{:else}
		<!-- Create new note -->
		<div class="create-note">
			<h3>Create Note</h3>
			<input
				type="text"
				placeholder="Title"
				bind:value={newNoteTitle}
				class="input"
			/>
			<textarea
				placeholder="Content"
				bind:value={newNoteContent}
				class="textarea"
				rows="4"
			></textarea>
			<button onclick={handleCreateNote} class="btn btn-primary">
				Create Note
			</button>
			<button onclick={handleInitialize} class="btn btn-secondary">
				Re-initialize
			</button>
		</div>

		<!-- Notes list -->
		<div class="notes-list">
			<h3>Your Notes ({$notesArray.length})</h3>

			{#if $notesArray.length === 0}
				<p class="empty">No notes yet. Create one above!</p>
			{/if}

			{#each $notesArray as note (note.id)}
				<div class="note-card">
					{#if editingNoteId === note.id}
						<!-- Edit mode -->
						<input
							type="text"
							bind:value={editTitle}
							class="input"
						/>
						<textarea
							bind:value={editContent}
							class="textarea"
							rows="4"
						></textarea>
						<div class="note-actions">
							<button onclick={handleUpdateNote} class="btn btn-primary">
								Save
							</button>
							<button onclick={cancelEdit} class="btn btn-secondary">
								Cancel
							</button>
						</div>
					{:else}
						<!-- View mode -->
						<h4>{note.title}</h4>
						<p>{note.content}</p>
						<div class="note-meta">
							<small>ID: {note.id}</small>
							{#if note._updatedAt}
								<small>Updated: {new Date(note._updatedAt).toLocaleString()}</small>
							{/if}
						</div>
						<div class="note-actions">
							<button
								onclick={() => startEdit(note.id, note.title, note.content)}
								class="btn btn-secondary"
							>
								Edit
							</button>
							<button
								onclick={() => handleDeleteNote(note.id)}
								class="btn btn-danger"
							>
								Delete
							</button>
						</div>
					{/if}
				</div>
			{/each}
		</div>
	{/if}
</div>

<style>
	.notes-test {
		padding: 20px;
		max-width: 800px;
		margin: 0 auto;
	}

	h2 {
		margin-bottom: 20px;
		color: #333;
	}

	h3 {
		margin-bottom: 15px;
		color: #555;
	}

	.warning {
		color: #d97706;
		padding: 10px;
		background: #fef3c7;
		border-radius: 4px;
	}

	.create-note {
		background: #f9fafb;
		padding: 20px;
		border-radius: 8px;
		margin-bottom: 30px;
	}

	.input {
		width: 100%;
		padding: 8px 12px;
		margin-bottom: 10px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 14px;
	}

	.textarea {
		width: 100%;
		padding: 8px 12px;
		margin-bottom: 10px;
		border: 1px solid #d1d5db;
		border-radius: 4px;
		font-size: 14px;
		font-family: inherit;
		resize: vertical;
	}

	.btn {
		padding: 8px 16px;
		border: none;
		border-radius: 4px;
		font-size: 14px;
		cursor: pointer;
		margin-right: 8px;
	}

	.btn-primary {
		background: #3b82f6;
		color: white;
	}

	.btn-primary:hover {
		background: #2563eb;
	}

	.btn-secondary {
		background: #6b7280;
		color: white;
	}

	.btn-secondary:hover {
		background: #4b5563;
	}

	.btn-danger {
		background: #ef4444;
		color: white;
	}

	.btn-danger:hover {
		background: #dc2626;
	}

	.notes-list {
		margin-top: 30px;
	}

	.empty {
		color: #9ca3af;
		font-style: italic;
	}

	.note-card {
		background: white;
		border: 1px solid #e5e7eb;
		border-radius: 8px;
		padding: 16px;
		margin-bottom: 16px;
	}

	.note-card h4 {
		margin: 0 0 8px 0;
		color: #1f2937;
	}

	.note-card p {
		margin: 0 0 12px 0;
		color: #4b5563;
		white-space: pre-wrap;
	}

	.note-meta {
		display: flex;
		gap: 16px;
		margin-bottom: 12px;
	}

	.note-meta small {
		color: #9ca3af;
		font-size: 12px;
	}

	.note-actions {
		display: flex;
		gap: 8px;
	}
</style>
