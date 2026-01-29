import { mdsvex } from 'mdsvex';
import adapter from 'svelte-adapter-bun';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: adapter({
			out: 'build',
			serveAssets: true,
			precompress: true,
			envPrefix: ''
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH || ''
		}
	},
	extensions: ['.svelte', '.svx'],
	vitePlugin: {
		// Ensure .svelte.ts files are properly handled
		inspector: false
	}
};

export default config;
