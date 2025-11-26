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
		// Service worker completely disabled
		// serviceWorker: {
		// 	register: false
		// },
		// files: {
		// 	serviceWorker: 'src/service-worker.ts'
		// }
	},
	extensions: ['.svelte', '.svx'],
	vitePlugin: {
		// Ensure .svelte.ts files are properly handled
		inspector: false
	}
};

export default config;
