<script lang="ts">
	import { onMount } from 'svelte';
	import { marked } from 'marked';

	const privacyContent = `# Free Association (experimental beta)

[playnet.lol](https://playnet.lol)

[t.me/interplaynetary](https://t.me/interplaynetary)`;

	let htmlContent = '';

	onMount(() => {
		htmlContent = marked(privacyContent) as string;
	});
</script>

<div class="privacy-container">
	{@html htmlContent}
</div>

<style>
	.privacy-container {
		max-width: 800px;
		margin: 0 auto;
		padding: 2rem;
		font-family:
			system-ui,
			-apple-system,
			sans-serif;
		line-height: 1.6;
	}

	:global(.privacy-container h1) {
		font-size: 2.5rem;
		margin-bottom: 1.5rem;
		color: #333;
	}

	:global(.privacy-container a) {
		color: #0066cc;
		text-decoration: none;
	}

	:global(.privacy-container a:hover) {
		text-decoration: underline;
	}
</style>
