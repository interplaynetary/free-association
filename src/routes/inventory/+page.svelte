<script lang="ts">
	import NestedPieExample from '$lib/components/examples/NestedPieExample.svelte';
	import Inventory from '$lib/components/Inventory.svelte';
	import Query from '$lib/components/maps/Query.svelte';
	import Shares from '$lib/components/Shares.svelte';
</script>

<div class="mb-10 flex justify-between">
	<div class="flex-1">
		<Query />
	</div>
</div>
<div>
	<h2 class="mt-10 mb-4 text-center text-2xl font-bold">Your Capacities</h2>
	<Inventory />

	<h2 class="mt-10 mb-4 text-center text-2xl font-bold">Your Shares</h2>
	<Shares />
</div>
