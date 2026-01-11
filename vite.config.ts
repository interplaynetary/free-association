import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, configDefaults } from 'vitest/config';
import process from 'node:process';
import devtoolsJson from 'vite-plugin-devtools-json';



// https://vite.dev/config/
export default defineConfig({
	plugins: [
		devtoolsJson() as any,
		sveltekit(),
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
		target: 'esnext',
		sourcemap: true
	},
	esbuild: {
		target: 'esnext'
	},
	// SSR configuration for server-only packages
	ssr: {
		noExternal: []
	},
	server: {
		fs: {
			allow: [
				// Search up for workspace root
				'..',
				// Explicitly allow packages directory
				'packages'
			]
		},
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
})
