<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import '../app.css';
	import { setContext } from 'svelte';

	// Provide the panels context that Header is trying to access
	setContext("panels", {
		handleInventoryClick: () => console.log("Inventory clicked"),
		handlePeerClick: () => console.log("Peer clicked"),
		handleChartsClick: () => console.log("Charts clicked")
	});

	let { children } = $props();
</script>

<div class="app">
	<Header />

	{@render children()}
</div>
