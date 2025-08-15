#!/usr/bin/env node

import fs from 'fs';
import path from 'path';

// Get base path from environment or default to empty
const basePath = process.env.BASE_PATH || '';
const isDev = process.argv.includes('--dev');

// Base manifest configuration
const manifest = {
	name: "Playnet",
	short_name: "Playnet",
	description: "Free association network platform",
	start_url: isDev ? "/" : `${basePath}/`,
	scope: isDev ? "/" : `${basePath}/`,
	display: "standalone",
	orientation: "portrait-primary",
	background_color: "#ffffff",
	theme_color: "#000000",
	lang: "en",
	dir: "ltr",
	icons: [
		{
			src: isDev ? "/favicon.png" : `${basePath}/favicon.png`,
			sizes: "192x192",
			type: "image/png",
			purpose: "any maskable"
		},
		{
			src: isDev ? "/favicon.png" : `${basePath}/favicon.png`,
			sizes: "512x512",
			type: "image/png",
			purpose: "any maskable"
		}
	],
	categories: ["productivity", "social", "collaboration"],
	screenshots: []
};

// Write manifest to static directory
const manifestPath = path.join(process.cwd(), 'static', 'manifest.json');
fs.writeFileSync(manifestPath, JSON.stringify(manifest, null, 2));

console.log(`Generated manifest.json with base path: "${basePath}"`);
console.log(`Start URL: ${manifest.start_url}`);
console.log(`Scope: ${manifest.scope}`);
