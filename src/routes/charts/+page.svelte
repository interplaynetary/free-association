<script lang="ts">

	import StackedBar from '$lib/components/StackedBar.svelte';

	// Sample data for charts
	const chartData = [
		{ category: 'Recognition', peer: 'Self', value: 25 },
		{ category: 'Recognition', peer: 'Peer', value: 15 },
		{ category: 'Contributions', peer: 'Self', value: 20 },
		{ category: 'Contributions', peer: 'Peer', value: 10 },
		{ category: 'Innovation', peer: 'Self', value: 15 },
		{ category: 'Innovation', peer: 'Peer', value: 30 }
	];
</script>

<StackedBar data={chartData} width={800} />
