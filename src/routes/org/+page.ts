import { DEMO_ORGANIZATIONS, getOrgTreesMap } from '$lib/config/org-trees';
import type { PageLoad } from './$types';

export const load: PageLoad = async () => {
	const orgTreesMap = getOrgTreesMap();
	
	// Combine organization metadata with config data
	const organizations = Object.entries(DEMO_ORGANIZATIONS).map(([orgId, org]) => {
		const slug = orgId.replace('org_demo_', '');
		const config = orgTreesMap[slug];
		
		return {
			slug,
			orgId,
			name: org.names.en,
			emoji: org.emoji,
			description: org.description,
			monthlyBudget: config?.monthly_budget || 0,
			recognizes: config?.recognizes?.length || 0,
			priorities: config?.tree?.children?.length || 0
		};
	});
	
	// Sort by budget (largest first)
	organizations.sort((a, b) => b.monthlyBudget - a.monthlyBudget);
	
	return {
		organizations
	};
};

