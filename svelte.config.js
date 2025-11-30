import { mdsvex } from 'mdsvex';
import adapterStatic from '@sveltejs/adapter-static';
import adapterNode from '@sveltejs/adapter-node';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';
import process from 'node:process';

// Use Node adapter for server builds, static adapter otherwise
const isServerBuild = process.env.BUILD_TARGET === 'server';

const config = {
	preprocess: [vitePreprocess(), mdsvex()],
	kit: {
		adapter: isServerBuild
			? adapterNode({
					out: 'build-server',
					precompress: true,
					envPrefix: ''
			  })
			: adapterStatic({
					fallback: 'index.html'
			  }),
		paths: {
			base: process.argv.includes('dev') ? '' : process.env.BASE_PATH || ''
		},
		// Disable service worker registration for server builds
		// vite-pwa handles service worker generation
		serviceWorker: {
			register: false
		}
	},
	extensions: ['.svelte', '.svx'],
	vitePlugin: {
		// Ensure .svelte.ts files are properly handled
		inspector: false
	}
};

export default config;
