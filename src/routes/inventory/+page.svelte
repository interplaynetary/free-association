<script lang="ts">
	import Capacities from '$lib/components/Capacities.svelte';
	import Map from '$lib/components/Map.svelte';
	import Shares from '$lib/components/Shares.svelte';
</script>

<div class="mt-8">
	<Map />
	<h2 class="text-center text-2xl font-bold">Your Capacities</h2>
	<Capacities />

	<h2 class="text-center text-2xl font-bold">Your Shares</h2>
	<Shares />
</div>
