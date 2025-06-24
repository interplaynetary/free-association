import { writeFileSync } from 'fs';
import { resolve } from 'path';

const basePath = process.env.BASE_PATH || '';

const manifest = {
	name: 'Playnet',
	short_name: 'Playnet',
	description: 'Free association network platform',
	start_url: basePath ? `${basePath}/` : '/',
	scope: basePath ? `${basePath}/` : '/',
	display: 'standalone',
	background_color: '#ffffff',
	theme_color: '#000000',
	icons: [
		{
			src: `${basePath}/favicon.png`,
			sizes: '192x192',
			type: 'image/png'
		}
	]
};

// Write to both static (for dev) and build (for production)
const staticPath = resolve('static/manifest.json');
const buildPath = resolve('build/manifest.json');

try {
	writeFileSync(staticPath, JSON.stringify(manifest, null, 2));
	console.log('✓ Generated manifest.json for static directory');
} catch (err) {
	console.log('Note: Could not write to static directory (this is OK during build)');
}

try {
	writeFileSync(buildPath, JSON.stringify(manifest, null, 2));
	console.log('✓ Generated manifest.json for build directory');
} catch (err) {
	console.log('Note: Could not write to build directory (this is OK during dev)');
}
