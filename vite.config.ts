import tailwindcss from '@tailwindcss/vite';
import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig, type Plugin } from 'vite';

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
				'store-*'
			]
		}
	},
	optimizeDeps: {
		include: [
			'gun',
			'gun/gun',
			'gun/sea',
			'gun/sea.js',
			'gun/lib/then',
			'gun/lib/webrtc',
			'gun/lib/radix',
			'gun/lib/radisk',
			'gun/lib/store',
			'gun/lib/rindexed'
		]
	}
});
