import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';
import { configDefaults } from 'vitest/config';
import { VitePWA } from 'vite-plugin-pwa';
import devtoolsJson from 'vite-plugin-devtools-json';

// https://vite.dev/config/
export default defineConfig({
	plugins: [
		devtoolsJson(),
		sveltekit(),
		VitePWA({
			strategies: 'injectManifest',
			srcDir: 'src',
			filename: 'service-worker.ts',
			injectManifest: {
				globPatterns: ['**/*.{js,css,html,ico,png,svg,webp,woff,woff2}'],
				globIgnores: ['**/node_modules/**/*'],
				// Vite PWA docs recommend specifying manifest injection point
				injectionPoint: undefined
			},
			// Include specific assets (vite-pwa docs pattern)
			includeAssets: ['favicon.png', 'robots.txt'],
			// Register type for cleaner auto-update behavior
			registerType: 'autoUpdate',
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
			// Dev options - vite-pwa docs pattern for development
			devOptions: {
				enabled: false,
				type: 'module',
				// Suppress warnings during development
				suppressWarnings: true
			},
			// Manual registration for more control (vite-pwa docs pattern)
			injectRegister: false,
			// Vite PWA docs recommend setting this for SvelteKit
			selfDestroying: false
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
