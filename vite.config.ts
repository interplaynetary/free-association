import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';
import { configDefaults } from 'vitest/config';
import { VitePWA } from 'vite-plugin-pwa';

// https://vite.dev/config/
export default defineConfig({
	plugins: [
		tailwindcss(),
		sveltekit(),
		VitePWA({
			strategies: 'injectManifest',
			srcDir: 'src',
			filename: 'service-worker.ts',
			injectManifest: {
				globPatterns: ['**/*.{js,css,html,ico,png,svg,webp,woff,woff2}']
			},
			manifest: false, // We'll use the existing manifest.json
			devOptions: {
				enabled: false // Disable in dev to avoid caching issues with HMR
			},
			workbox: {
				globPatterns: ['**/*.{js,css,html,ico,png,svg,webp,woff,woff2}'],
				cleanupOutdatedCaches: true,
				clientsClaim: true,
				skipWaiting: true
			}
		})
	],
	define: {
		'process.env.NODE_ENV': process.env.NODE_ENV === 'production' ? '"production"' : '"development"'
	},
	// Support top-level await for Holster
	build: {
		target: 'esnext'
	},
	esbuild: {
		target: 'esnext'
	},
	// SSR configuration for server-only packages
	ssr: {
		noExternal: []
	},
	server: {
		watch: {
			ignored: [
				'**/store/**',
				'**/store',
				'store/**',
				'store',
				'**/radata/**',
				'**/radata',
				'radata/**',
				'radata',
				'radata-*',
				'**/radata-*',
				'store-*',
				'*.md',
				'*.txt'
			]
		}
	},
	// Service worker configuration
	worker: {
		format: 'es'
	},
	// Vitest configuration
	test: {
		globals: true, // Enable globals (describe, it, expect)
		environment: 'jsdom', // Use jsdom for DOM/Svelte support
		include: ['**/*.test.ts'],
		exclude: [...configDefaults.exclude],
		coverage: {
			provider: 'v8',
			reporter: ['text', 'json', 'html'],
			include: ['src/**/*.{ts,js,svelte.ts}'],
			exclude: ['src/**/*.test.ts', 'src/**/*.spec.ts']
		}
	}
});
