import { error } from '@sveltejs/kit';
import { getOrgTree, getOrgMetadata } from '$lib/config/org-trees';
import type { RootNode, Contributor } from '../../../../packages/protocol/src/schemas';

// Client-side only rendering (same as main app)
export const prerender = false;
export const ssr = false;
export const csr = true;

export interface PageData {
	tree: RootNode;
	slug: string;
	orgName: string;
	orgDescription: string;
	monthlyBudget?: number;
	recognizes: Contributor[];
}

/** @type {import('./$types').PageLoad} */
export const load = async ({ params }): Promise<PageData> => {
	const { slug } = params;

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
		recognizes: metadata?.recognizes || []
	};
};

