import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';
import { configDefaults } from 'vitest/config';
import { SvelteKitPWA } from '@vite-pwa/sveltekit';
import devtoolsJson from 'vite-plugin-devtools-json';
import process from 'node:process';

// https://vite.dev/config/
export default defineConfig({
	plugins: [
		devtoolsJson(),
		sveltekit(),
		SvelteKitPWA({
			srcDir: 'src',
			strategies: 'injectManifest',
			filename: 'service-worker.ts',
			scope: '/',
			selfDestroying: process.env.SELF_DESTROYING_SW === 'true',
			includeAssets: ['favicon.png', 'robots.txt'],
			manifest: {
				name: 'Playnet',
				short_name: 'Playnet',
				description: 'Free association network platform',
				theme_color: '#000000',
				background_color: '#ffffff',
				display: 'standalone',
				scope: '/',
				start_url: '/',
				orientation: 'any',
				// Vite PWA docs pattern for icons
				icons: [
					{
						src: '/favicon.png',
						sizes: '192x192',
						type: 'image/png',
						purpose: 'any maskable'
					},
					{
						src: '/favicon.png',
						sizes: '512x512',
						type: 'image/png',
						purpose: 'any maskable'
					}
				],
				// Categories help app stores categorize your PWA
				categories: ['productivity', 'social', 'collaboration'],
				shortcuts: [
					{
						name: 'Recognition Tree',
						short_name: 'Tree',
						description: 'View your recognition tree',
						url: '/',
						icons: [{ src: '/favicon.png', sizes: '192x192' }]
					},
					{
						name: 'Collective View',
						short_name: 'Collective',
						description: 'View collective allocations',
						url: '/collective',
						icons: [{ src: '/favicon.png', sizes: '192x192' }]
					},
					{
						name: 'Map View',
						short_name: 'Map',
						description: 'View on map',
						url: '/map',
						icons: [{ src: '/favicon.png', sizes: '192x192' }]
					}
				],
				share_target: {
					action: '/share',
					method: 'POST',
					enctype: 'multipart/form-data',
					params: {
						title: 'title',
						text: 'text',
						url: 'url'
					}
				}
			},
			injectManifest: {
				globPatterns: ['client/**/*.{js,css,ico,png,svg,webp,woff,woff2}', 'prerendered/**/*.{html,json}']
			},
			workbox: {
				globPatterns: ['client/**/*.{js,css,ico,png,svg,webp,woff,woff2}', 'prerendered/**/*.{html,json}']
			},
			devOptions: {
				enabled: false,
				suppressWarnings: process.env.SUPPRESS_WARNING === 'true',
				type: 'module'
			},
			kit: {
				includeVersionFile: true,
				adapterFallback: 'index.html'
			},
			injectRegister: false
		})
	],
	define: {
		'process.env.NODE_ENV': process.env.NODE_ENV === 'production' ? '"production"' : '"development"'
	},
	// Support top-level await for Holster
	optimizeDeps: {
		esbuildOptions: {
			target: 'esnext'
		}
	},
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
