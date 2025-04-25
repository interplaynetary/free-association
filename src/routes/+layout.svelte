<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import '../app.css';
	import { onMount, setContext } from 'svelte';
	import { createRec } from '../stores/rec.svelte';
	import type { RecognitionStore } from '../types/types';
	import { goto } from '$app/navigation';
	import Parent from '$lib/components/Parent.svelte';

	// Layout data from +layout.ts using Svelte 5 runes
	let { data } = $props<{
		data: {
			navigation: { id: string; path: string; label: string }[];
			currentPath: string;
			isNode: boolean;
			isInventory: boolean;
			isLogin: boolean;
			isCharts: boolean;
		}
	}>();

	// Path to the recognition data
	let path: string[] = ["recognition"];

	// Store reference - make this reactive using Svelte 5 state
	let recStore: RecognitionStore | undefined = $state(undefined);

	// State
	let loading = $state(true);
	let error = $state<string | null>(null);
	let viewportWidth = $state(window.innerWidth);
	let viewportHeight = $state(window.innerHeight);
	let deleteMode = $state(false);

	// Panel function handlers
	function handleInventoryClick() {
		goto(data.isInventory ? '/' : '/inventory');
	}

	function handlePeerClick() {
		goto(data.isLogin ? '/' : '/login');
	}

	function handleChartsClick() {
		goto(data.isCharts ? '/' : '/charts');
	}

	// Toast notification state
	let toast = $state({
		visible: false,
		message: "",
		type: "info" as "info" | "success" | "warning" | "error",
		timeoutId: null as number | null,
	});

	// Show toast message
	function showToast(
		message: string,
		type: "info" | "success" | "warning" | "error" = "info",
	) {
		// Clear any existing toast timeout
		if (toast.timeoutId) {
			clearTimeout(toast.timeoutId);
		}

		// Show new toast
		toast = {
			visible: true,
			message,
			type,
			timeoutId: window.setTimeout(() => {
				toast.visible = false;
			}, 3000) as unknown as number,
		};
	}

	// Navigation functions
	function toggleDeleteMode() {
		deleteMode = !deleteMode;
		showToast(
			deleteMode 
				? "Delete mode activated. Click a node to delete it." 
				: "Delete mode deactivated.",
			deleteMode ? "warning" : "info"
		);
	}

	function handleAddNode() {
		showToast("Add node - handled by Parent component", "info");
		// For non-node pages, navigate to node page first
		if (!data.isNode) {
			goto('/');
		}
	}

	function zoomOutToParent() {
		showToast("This functionality is handled by the Parent component", "info");
	}

	function navigateToPathIndex(index: number) {
		showToast(`Navigate to path index ${index} - handled by Parent component`, "info");
	}

	// Initialize on mount
	onMount(async () => {
		try {
			loading = true;

			// Create the recognition store directly
			recStore = createRec(path);

			loading = false;
		} catch (err) {
			console.error("Error initializing:", err);
			error = err instanceof Error ? err.message : "Unknown error";
			loading = false;
		}
	});

	// Make the global store available to all children when it's ready
	$effect(() => {
		if (recStore) {
			console.log('Setting recStore in context:', recStore);
			setContext('recStore', recStore);
		}
	});

	// Handle window resize
	function handleResize() {
		viewportWidth = window.innerWidth;
		viewportHeight = window.innerHeight;
	}

	// Set up the panels context
	setContext("panels", {
		get activePanel() {
			if (data.isInventory) return 'inventory';
			if (data.isLogin) return 'login';
			if (data.isCharts) return 'charts';
			return 'node';
		},
		setPanel: (panel: string) => {
			goto(panel === 'node' ? '/' : `/${panel}`);
		},
		handleInventoryClick,
		handlePeerClick,
		handleChartsClick,
	});

	// Set up the toast context
	setContext("toast", {
		showToast,
	});

	// Set up navigation context for Header outside of Parent
	setContext("navigation", {
		deleteMode,
		toggleDeleteMode,
		handleAddNode,
		zoomOutToParent,
		navigateToPathIndex,
	});
</script>

<svelte:window on:resize={handleResize} />

<main>
	<div class="container">
		{#if loading}
			<div class="loading">Loading recognition data...</div>
		{:else if error}
			<div class="error">{error}</div>
		{:else if recStore}
			<div class="app-layout">
				{#if data.isNode}
					<!-- For node page, use Parent with its own header -->
					<Parent store={recStore} />
				{:else}
					<!-- For other pages, show header and slot content -->
					<div class="app-header">
						<Header store={recStore} />
					</div>
					<div class="app-content">
						<slot store={recStore} />
					</div>
				{/if}
			</div>
		{:else}
			<div class="empty">No recognition data available</div>
		{/if}
	</div>

	<!-- Toast notification -->
	{#if toast.visible}
		<div class="toast-container">
			<div class="toast toast-{toast.type}">
				{toast.message}
			</div>
		</div>
	{/if}
</main>

<!-- CSS moved to global app.css -->
