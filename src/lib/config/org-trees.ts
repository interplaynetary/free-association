/**
 * Organization Tree Configuration System
 * 
 * Maps organization slugs to custom tree configurations for tailored interfaces.
 * Each organization gets a pre-populated tree structure loaded from JSON config.
 */

import type { RootNode } from '$lib/protocol/schemas';
import { RootNodeSchema } from '$lib/protocol/schemas';
import orgTreesConfig from './org-trees.json';

/**
 * Organization Tree Configuration Entry
 */
export interface OrgTreeConfig {
	slug: string;
	name: string;
	description: string;
	tree: RootNode;
}

/**
 * Type-safe mapping of organization slugs to tree configs
 */
export type OrgTreesMap = Record<string, OrgTreeConfig>;

/**
 * Get all available organization slugs
 */
export function getAvailableOrgs(): string[] {
	return Object.keys(orgTreesConfig);
}

/**
 * Get organization tree configuration by slug
 * 
 * @param slug - Organization identifier (e.g., 'unicef', 'world-bank')
 * @returns RootNode tree or null if not found
 */
export function getOrgTree(slug: string): RootNode | null {
	const config = orgTreesConfig[slug as keyof typeof orgTreesConfig];
	
	if (!config) {
		console.warn(`[ORG-TREES] No configuration found for slug: ${slug}`);
		return null;
	}
	
	try {
		// Validate the tree structure
		const validated = RootNodeSchema.parse(config.tree);
		console.log(`[ORG-TREES] Loaded tree for ${slug}:`, config.name);
		return validated;
	} catch (error) {
		console.error(`[ORG-TREES] Invalid tree structure for ${slug}:`, error);
		return null;
	}
}

/**
 * Get organization metadata without the full tree
 */
export function getOrgMetadata(slug: string): { name: string; description: string } | null {
	const config = orgTreesConfig[slug as keyof typeof orgTreesConfig];
	
	if (!config) {
		return null;
	}
	
	return {
		name: config.name,
		description: config.description
	};
}

/**
 * Check if an organization slug exists
 */
export function isValidOrgSlug(slug: string): boolean {
	return slug in orgTreesConfig;
}

