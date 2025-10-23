import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';
import { configDefaults } from 'vitest/config';

// GUN module exclusion function for text-encoding
const moduleExclude = (match: string): Plugin => {
	const m = (id: string): boolean => id.indexOf(match) > -1;
	return {
		name: `exclude-${match}`,
		resolveId(id: string) {
			if (m(id)) return id;
		},
		load(id: string) {
			if (m(id)) return `export default {}`;
		}
	};
};

// https://vite.dev/config/
export default defineConfig({
	plugins: [tailwindcss(), sveltekit(), moduleExclude('text-encoding')],
	// Required for Gun in service worker
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
	resolve: {
		conditions: ['node']
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
		format: 'es',
		plugins: () => [tailwindcss(), moduleExclude('text-encoding')]
	},
	optimizeDeps: {
		include: [
			'gun',
			//'gun/gun',
			'gun/sea',
			'gun/sea.js',
			'gun/axe',
			'gun/lib/then',
			'gun/lib/webrtc',
			'gun/lib/radix',
			'gun/lib/radisk',
			'gun/lib/store',
			'gun/lib/rindexed',
			'gun/lib/yson.js'
		],
		esbuildOptions: {
			target: 'esnext'
		}
	},
	// Vitest configuration
	test: {
		globals: false,
		environment: 'node',
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
