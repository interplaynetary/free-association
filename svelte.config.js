import { mdsvex } from 'mdsvex';
import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: adapter({
			fallback: 'index.html'
		}),
		// Configure base path for GitHub Pages deployment
		paths: {
			base: process.env.BASE_PATH || ''
		}
	},
	extensions: ['.svelte', '.svx']
};

export default config;
