<script lang="ts">
	import Capacities from '$lib/components/Capacities.svelte';
	import Map from '$lib/components/Map.svelte';
	import Shares from '$lib/components/Shares.svelte';
</script>

<div>
	<h2 class="mt-4 mb-4 text-center text-2xl font-bold">Your Capacities</h2>
	<Capacities />

	<h2 class="mt-4 mb-4 text-center text-2xl font-bold">Your Shares</h2>
	<Shares />
	<div class="mt-4"></div>
	<Map />
</div>
