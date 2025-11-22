import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: [vitePreprocess()],
	kit: {
		adapter: adapter({
			fallback: 'index.html'
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH || ''
		},
		serviceWorker: {
			register: false
		},
		files: {
			serviceWorker: 'src/service-worker.ts'
		}
	},
	extensions: ['.svelte']
};

export default config;
