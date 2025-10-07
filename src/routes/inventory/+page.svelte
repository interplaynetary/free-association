<script lang="ts">
	import Capacities from '$lib/components/Capacities.svelte';
	import Map from '$lib/components/Map.svelte';
	import Shares from '$lib/components/Shares.svelte';
	import LiveLocationToggle from '$lib/components/LiveLocationToggle.svelte';
	import LiveLocationPrivacy from '$lib/components/LiveLocationPrivacy.svelte';
</script>

<div class="mt-8">
	<div class="max-w-4xl mx-auto px-4 mb-6">
		<LiveLocationToggle />
		<LiveLocationPrivacy />
	</div>

	<Map />
	<h2 class="text-center text-2xl font-bold">Capacities</h2>
	<Capacities />

	<h2 class="text-center text-2xl font-bold">Shares</h2>
	<Shares />
</div>
