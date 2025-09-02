<script lang="ts">
	import UnconferenceGrid3 from '$lib/components/unconference/UnconferenceGrid3.svelte';
	import NoteSidebar3 from '$lib/components/unconference/NoteSidebar3.svelte';
	import { unconferenceState } from '$lib/state/unconference/unconference-reactive.svelte';
	import { onMount, onDestroy } from 'svelte';
	import { page } from '$app/stores';

	let boardId = $state('');

	onMount(() => {
		const urlBoardId = $page.url.searchParams.get('board') || 'default';
		boardId = urlBoardId;
		unconferenceState.initializeBoard(boardId);
	});

	onDestroy(() => {
		unconferenceState.cleanup();
	});
</script>

<div class="unconference-container">
	<div class="unconference-sidebar">
		<NoteSidebar3 />
	</div>
	<div class="unconference-main">
		<UnconferenceGrid3 {boardId} />
	</div>
</div>

<style>
	.unconference-container {
		display: flex;
		height: calc(100vh - 76px);
		width: 100%;
		gap: 1rem;
		padding: 1rem;
		background: #f8f9fa;
	}

	.unconference-sidebar {
		width: 300px;
		background: white;
		border-radius: 0.5rem;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
		overflow-y: auto;
		flex-shrink: 0;
	}

	.unconference-main {
		flex: 1;
		background: white;
		border-radius: 0.5rem;
		box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
		overflow: auto;
		position: relative;
	}

	@media (max-width: 768px) {
		.unconference-container {
			flex-direction: column;
			height: auto;
			min-height: calc(100vh - 76px);
		}

		.unconference-sidebar {
			width: 100%;
			max-height: 300px;
		}
	}
</style>
