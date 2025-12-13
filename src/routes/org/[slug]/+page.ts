import { error } from '@sveltejs/kit';
import { getOrgTree, getOrgMetadata } from '$lib/config/org-trees';
import type { RootNode, Contributor } from '@playnet/free-association/schemas';
import { get } from 'svelte/store';

// Client-side only rendering (same as main app)
export const prerender = false;
export const ssr = false;
export const csr = true;

export interface PageData {
	tree: RootNode | null; // null for user trees (loaded from network)
	slug: string;
	orgName: string;
	orgDescription: string;
	monthlyBudget?: number;
	recognizes: Contributor[];
	isUserTree?: boolean; // Flag to indicate this is a user's public tree
	userPubkey?: string; // The user's pubkey if this is a user tree
}

/**
 * Detect if a slug is a pubkey (long base64url string) vs an org slug
 */
function isPubkeySlug(slug: string): boolean {
	// First check if it's a known org slug
	const tree = getOrgTree(slug);
	if (tree) return false; // It's an org, not a pubkey

	// Pubkeys are typically 64+ character base64url strings
	// Base64url uses: A-Z, a-z, 0-9, -, _, .
	// Org slugs are short kebab-case strings like "unicef" or "world-bank"
	return slug.length > 40 && /^[A-Za-z0-9\-_.]+$/.test(slug);
}

/** @type {import('./$types').PageLoad} */
export const load = async ({ params }): Promise<PageData> => {
	const { slug } = params;

	console.log('[ORG-ROUTE] Loading for slug:', slug);

	// Check if this is a pubkey (user tree) or org slug
	if (isPubkeySlug(slug)) {
		console.log('[ORG-ROUTE] Detected pubkey slug - loading user tree');

		// For user trees, we'll load the tree from the network in the component
		// using subscribeToRecognitionTree()
		// Here we just return metadata to indicate it's a user tree

		// Try to get alias from public trees list
		let alias = slug.slice(0, 20) + '...'; // Default to truncated pubkey

		// Import public trees store dynamically (client-side only)
		if (typeof window !== 'undefined') {
			try {
				const { publicTrees } = await import('$lib/network/public-trees.svelte');
				const trees = get(publicTrees);
				const treeEntry = trees[slug];
				if (treeEntry) {
					alias = treeEntry.alias;
					console.log('[ORG-ROUTE] Found alias in public trees:', alias);
				}
			} catch (err) {
				console.warn('[ORG-ROUTE] Could not load public trees:', err);
			}
		}

		return {
			tree: null, // Will be loaded from network
			slug,
			orgName: alias,
			orgDescription: `Public recognition tree for ${alias}`,
			recognizes: [],
			isUserTree: true,
			userPubkey: slug
		};
	}

	// Original org tree loading logic
	console.log('[ORG-ROUTE] Loading organization tree for:', slug);

	// Get tree configuration
	const tree = getOrgTree(slug);

	if (!tree) {
		console.error('[ORG-ROUTE] Organization not found:', slug);
		throw error(404, {
			message: `Organization "${slug}" not found. Available organizations can be found at the root page.`
		});
	}

	// Get metadata
	const metadata = getOrgMetadata(slug);

	console.log('[ORG-ROUTE] Successfully loaded tree for:', metadata?.name || slug);

	return {
		tree,
		slug,
		orgName: metadata?.name || slug,
		orgDescription: metadata?.description || '',
		monthlyBudget: metadata?.monthly_budget,
		recognizes: metadata?.recognizes || [],
		isUserTree: false
	};
};
