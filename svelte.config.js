import { mdsvex } from 'mdsvex';
import adapter from '@sveltejs/adapter-static';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: adapter({
			// GitHub Pages configuration
			pages: 'build',
			assets: 'build',
			fallback: '404.html',
			precompress: false,
			strict: true
		}),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH || '',
			relative: false
		}
		// Zero-config PWA: Let @vite-pwa/sveltekit handle service worker automatically
	},
	extensions: ['.svelte', '.svx'],
	alias: {
		'@/*': './path/to/lib/*'
	}
};

export default config;
