import { mdsvex } from 'mdsvex';
import adapter from '@sveltejs/adapter-node';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: adapter({
			// Options for Node.js deployment
			out: 'build-server',
			precompress: true,
			envPrefix: ''
		}),
		// Server-side paths (no base path needed for DigitalOcean)
		paths: {
			base: ''
		},
		serviceWorker: {
			register: false // Service worker not needed on server
		}
	},
	extensions: ['.svelte', '.svx']
};

export default config;

