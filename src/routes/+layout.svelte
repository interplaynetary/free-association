<script lang="ts">
	import Header from '$lib/components/Header.svelte';
	import '../app.css';
	import type { LayoutProps } from './$types';
	import { globalState } from '$lib/global.svelte';
	// Layout data from +layout.ts using Svelte 5 runes
	let { children }: LayoutProps = $props();

	let toast = $derived(globalState.toast);

	// State
	let viewportWidth = $state(window.innerWidth);
	let viewportHeight = $state(window.innerHeight);

	// Handle window resize
	function handleResize() {
		viewportWidth = window.innerWidth;
		viewportHeight = window.innerHeight;
	}
</script>

<svelte:window on:resize={handleResize} />

<main>
	<div class="app-header">
		<Header />
	</div>
	<div class="app-content">
		{@render children()}
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
