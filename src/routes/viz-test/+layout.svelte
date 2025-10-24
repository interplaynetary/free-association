<script lang="ts">
	import type { LayoutProps } from './$types';
	import '../../app.css';
	
	let { children }: LayoutProps = $props();
</script>

<main>
	{@render children()}
</main>

<style>
	main {
		width: 100vw;
		height: 100vh;
		overflow: auto;
		background: #0f172a;
	}
</style>

