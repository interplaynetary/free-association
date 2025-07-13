import * as d3 from 'd3';
import { resolveToPublicKey } from '$lib/state/users.svelte';

const nameColors = new Map();

const colorScale = d3
	.scaleOrdinal()
	.range([...d3.schemePastel1, ...d3.schemePastel2, ...d3.schemeSet2, ...d3.schemeSet3]);

/**
 * Color palette for user identifiers
 */
const COLOR_PALETTE = [
	'#1f77b4',
	'#ff7f0e',
	'#2ca02c',
	'#d62728',
	'#9467bd',
	'#8c564b',
	'#e377c2',
	'#7f7f7f',
	'#bcbd22',
	'#17becf',
	'#aec7e8',
	'#ffbb78',
	'#98df8a',
	'#ff9896',
	'#c5b0d5',
	'#c49c94',
	'#f7b6d2',
	'#c7c7c7',
	'#dbdb8d',
	'#9edae5'
];

/**
 * Hash function to get a numeric hash from a string
 * @param str The string to hash
 * @returns A numeric hash
 */
function hashString(str: string): number {
	let hash = 0;
	if (str.length === 0) return hash;

	for (let i = 0; i < str.length; i++) {
		const char = str.charCodeAt(i);
		hash = (hash << 5) - hash + char;
		hash = hash & hash; // Convert to 32bit integer
	}

	// Ensure positive return value
	return Math.abs(hash);
}

/**
 * Brightness level for a color (0-255)
 * Higher value means brighter color
 * @param hex Hex color code
 * @returns Brightness value
 */
function getBrightness(hex: string): number {
	// Remove # if present
	const color = hex.startsWith('#') ? hex.slice(1) : hex;

	// Parse r, g, b values
	const r = parseInt(color.substr(0, 2), 16);
	const g = parseInt(color.substr(2, 2), 16);
	const b = parseInt(color.substr(4, 2), 16);

	// Perceived brightness formula (gives more weight to green)
	return r * 0.299 + g * 0.587 + b * 0.114;
}

/**
 * Get a color for a user ID that's consistent across the application
 * Prioritizes using the public key for colors when available
 * @param userId User ID to get color for
 * @returns Hex color code
 */
export function getColorForUserId(userId: string): string {
	if (!userId) return '#cccccc';

	// Resolve to public key if possible to ensure consistent colors
	// for the same person regardless of contact ID vs public key
	const resolvedKey = resolveToPublicKey(userId);
	const colorKey = resolvedKey || userId;

	// Get a consistent index from the resolved key
	const index = hashString(colorKey) % COLOR_PALETTE.length;
	return COLOR_PALETTE[index];
}

/**
 * Get text color (black or white) that contrasts with background
 * @param backgroundColor Background color in hex
 * @returns '#ffffff' for dark backgrounds, '#000000' for light
 */
export function getContrastTextColor(backgroundColor: string): string {
	// Get brightness value (0-255)
	const brightness = getBrightness(backgroundColor);

	// Use white text for dark backgrounds, black for light
	return brightness > 128 ? '#000000' : '#ffffff';
}

/**
 * Apply alpha transparency to a hex color
 * @param hex Hex color code
 * @param alpha Alpha value (0-1)
 * @returns Rgba color string
 */
export function hexToRgba(hex: string, alpha: number): string {
	// Remove # if present
	const color = hex.startsWith('#') ? hex.slice(1) : hex;

	// Parse r, g, b values
	const r = parseInt(color.substr(0, 2), 16);
	const g = parseInt(color.substr(2, 2), 16);
	const b = parseInt(color.substr(4, 2), 16);

	// Return rgba
	return `rgba(${r}, ${g}, ${b}, ${alpha})`;
}

export function getColorForName(name: string) {
	if (!nameColors.has(name)) {
		nameColors.set(name, colorScale(name));
	}
	return nameColors.get(name);
}

// Helper function to get color based on name
export function getColorForNameHash(name: string): string {
	if (!name) return '#64748b'; // Default slate color

	// Simple hash function for consistent colors
	let hash = 0;
	for (let i = 0; i < name.length; i++) {
		hash = name.charCodeAt(i) + ((hash << 5) - hash);
	}

	// Generate colors in a pleasant range
	const h = Math.abs(hash) % 360;
	const s = 55 + (Math.abs(hash) % 15); // 55-70% saturation - softer but still vibrant
	const l = 70 + (Math.abs(hash) % 10); // 70-80% lightness - brighter, softer tones

	return `hsl(${h}, ${s}%, ${l}%)`;
}
