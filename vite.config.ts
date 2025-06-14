import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';
import devtoolsJson from 'vite-plugin-devtools-json';
import { SvelteKitPWA } from '@vite-pwa/sveltekit';

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
	plugins: [
		tailwindcss(),
		sveltekit(),
		moduleExclude('text-encoding'),
		devtoolsJson(),
		SvelteKitPWA({
			strategies: 'injectManifest',
			srcDir: 'src',
			filename: 'sw.ts',
			registerType: 'autoUpdate',
			injectManifest: {
				globPatterns: ['**/*.{js,css,html,ico,png,svg,webp,woff,woff2}'],
				swSrc: 'src/sw.ts',
				swDest: 'service-worker.js'
			},
			manifest: {
				name: 'Free Association',
				short_name: 'FreeAssoc',
				description: 'Free Association Network',
				theme_color: '#ffffff',
				background_color: '#ffffff',
				display: 'standalone',
				scope: '/',
				start_url: '/',
				icons: [
					{
						src: '/favicon.png',
						sizes: '32x32',
						type: 'image/png'
					}
				]
			},
			workbox: {
				globPatterns: ['**/*.{js,css,html,ico,png,svg,webp,woff,woff2}']
			},
			devOptions: {
				enabled: true,
				type: 'module'
			}
		})
	],
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
		]
	}
});
