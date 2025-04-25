<script lang="ts">
	import { onMount, getContext } from 'svelte';
	import { goto } from '$app/navigation';
	import type { RecognitionStore } from '../../types/types';
	
	let LoginComponent: any;
	
	// Get data from props 
	let { data } = $props<{
		data: Record<string, any>;
	}>();
	
	// Get store from context
	const recStore = getContext<RecognitionStore>('recStore');
	
	// Debugging to help diagnose issues
	console.log('Login page - recStore from context:', recStore);

	onMount(async () => {
		try {
			console.log('Loading Login component...');
			const module = await import('$lib/components/LogIn.svelte');
			LoginComponent = module.default;
			console.log('Login component loaded');
		} catch (error) {
			console.error('Error loading Login component:', error);
		}
	});
	
	function handleClose() {
		goto('/');
	}
	
	function handleAuthChange(e: CustomEvent) {
		console.log("Auth changed:", e.detail);
	}
</script>

<div class="login-wrapper panel-wrapper">
	{#if LoginComponent}
		<svelte:component 
			this={LoginComponent} 
			onclose={handleClose}
			onauthchange={handleAuthChange}
		/>
	{:else}
		<div class="loading">
			Loading login... 
			{#if !recStore}
				<div class="error mt-10">(Store is not available)</div>
			{/if}
		</div>
	{/if}
</div>

<!-- CSS moved to global app.css --> 