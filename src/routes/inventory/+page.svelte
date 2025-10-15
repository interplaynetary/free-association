<script lang="ts">
	import Capacities from '$lib/components/Capacities.svelte';
	import Map from '$lib/components/Map.svelte';
	import Shares from '$lib/components/Shares.svelte';
	import { t } from '$lib/translations';
</script>

<div class="mt-8">
	<Map />
	<h2 class="text-center text-2xl font-bold">{$t('home.capacities')}</h2>
	<Capacities />

	<h2 class="text-center text-2xl font-bold">{$t('home.shares')}</h2>
	<Shares />
</div>
