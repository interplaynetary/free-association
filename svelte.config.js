import { mdsvex } from 'mdsvex';
import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: adapter({
			fallback: 'index.html'
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH || ''
		},
		serviceWorker: {
			register: false // Using vite-plugin-pwa instead
		}
	},
	extensions: ['.svelte', '.svx']
};

export default config;
