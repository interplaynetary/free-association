import { writeFileSync, mkdirSync } from 'fs';
import { join } from 'path';

const basePath = process.env.BASE_PATH || '';

const manifest = {
	name: 'Playnet',
	short_name: 'Playnet',
	description: 'Free association network platform',
	start_url: `${basePath}/`,
	scope: `${basePath}/`,
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

// Ensure build directory exists
mkdirSync('build', { recursive: true });

// Write manifest to build directory
writeFileSync('build/manifest.json', JSON.stringify(manifest, null, 2));

console.log(`Generated manifest.json with base path: ${basePath || '/'}`); 