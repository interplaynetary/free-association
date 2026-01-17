import { addChild, findNodeById, addContributors } from '@playnet/free-association/tree';
import type { RootNode } from '../protocol/schemas';
import { DEMO_ORGANIZATIONS } from '$lib/config/org-trees';

/**
 * Official UN Sustainable Development Goals colors
 * Source: https://www.un.org/sustainabledevelopment/
 */
export const SDG_COLORS: Record<string, string> = {
	// Individual SDG colors (official UN colors)
	'sdg-1': '#e5243b', // No Poverty - Red
	'poverty-finance': '#e5243b',
	'poverty-housing': '#e5243b',
	'poverty-employment': '#e5243b',

	'sdg-2': '#dda63a', // Zero Hunger - Gold
	'hunger-food': '#dda63a',
	'hunger-farming': '#dda63a',
	'hunger-nutrition': '#dda63a',

	'sdg-3': '#4c9f38', // Good Health - Green
	'health-medical': '#4c9f38',
	'health-mental': '#4c9f38',
	'health-prevention': '#4c9f38',
	'health-wellness': '#4c9f38',

	'sdg-4': '#c5192d', // Quality Education - Dark Red
	'edu-primary': '#c5192d',
	'edu-vocational': '#c5192d',
	'edu-digital': '#c5192d',
	'edu-lifelong': '#c5192d',

	'sdg-5': '#ff3a21', // Gender Equality - Orange Red
	'gender-empowerment': '#ff3a21',
	'gender-rights': '#ff3a21',
	'gender-violence': '#ff3a21',

	'sdg-6': '#26bde2', // Clean Water - Cyan
	'water-access': '#26bde2',
	'water-sanitation': '#26bde2',
	'water-treatment': '#26bde2',

	'sdg-7': '#fcc30b', // Affordable Energy - Yellow
	'energy-solar': '#fcc30b',
	'energy-wind': '#fcc30b',
	'energy-efficiency': '#fcc30b',
	'energy-access': '#fcc30b',

	'sdg-8': '#a21942', // Decent Work - Burgundy
	'work-jobs': '#a21942',
	'work-entrepreneurship': '#a21942',
	'work-rights': '#a21942',
	'work-economy': '#a21942',

	'sdg-9': '#fd6925', // Innovation - Orange
	'innovation-tech': '#fd6925',
	'innovation-infrastructure': '#fd6925',
	'innovation-research': '#fd6925',

	'sdg-10': '#dd1367', // Reduced Inequalities - Magenta
	'inequality-income': '#dd1367',
	'inequality-social': '#dd1367',
	'inequality-access': '#dd1367',

	'sdg-11': '#fd9d24', // Sustainable Cities - Orange
	'cities-transport': '#fd9d24',
	'cities-housing': '#fd9d24',
	'cities-spaces': '#fd9d24',
	'cities-waste': '#fd9d24',

	'sdg-12': '#bf8b2e', // Responsible Consumption - Brown
	'consumption-circular': '#bf8b2e',
	'consumption-waste': '#bf8b2e',
	'consumption-sustainable': '#bf8b2e',

	'sdg-13': '#3f7e44', // Climate Action - Green
	'climate-mitigation': '#3f7e44',
	'climate-adaptation': '#3f7e44',
	'climate-education': '#3f7e44',

	'sdg-14': '#0a97d9', // Life Below Water - Blue
	'ocean-conservation': '#0a97d9',
	'ocean-fishing': '#0a97d9',
	'ocean-pollution': '#0a97d9',

	'sdg-15': '#56c02b', // Life on Land - Light Green
	'land-forests': '#56c02b',
	'land-biodiversity': '#56c02b',
	'land-restoration': '#56c02b',

	'sdg-16': '#00689d', // Peace & Justice - Dark Blue
	'peace-institutions': '#00689d',
	'peace-justice': '#00689d',
	'peace-conflict': '#00689d',
	'peace-transparency': '#00689d',

	'sdg-17': '#19486a', // Partnerships - Navy
	'partnership-local': '#19486a',
	'partnership-global': '#19486a',
	'partnership-tech': '#19486a',
	'partnership-finance': '#19486a'
};

/**
 * Get the SDG color for a node ID, or null if not an SDG node
 */
export function getSDGColor(nodeId: string): string | null {
	return SDG_COLORS[nodeId] || null;
}

/**
 * Check if a node ID is part of the SDG tree structure
 */
export function isSDGNode(nodeId: string): boolean {
	return nodeId in SDG_COLORS;
}

/**
 * Populate tree with all 17 Sustainable Development Goals
 * All SDGs are shown at the root level with emojis matching official SDG themes
 */
export function populateSDGTree(rootNode: RootNode): RootNode {
	console.log('[SDG] Populating tree with all 17 SDGs at root level...');

	// Helper to safely add SDG nodes only if they don't exist
	const safeAddChild = (parent: any, id: string, name: string, fulfillment?: number) => {
		const existing = findNodeById(rootNode, id);
		if (existing) {
			console.log(`[SDG] Node ${id} already exists, skipping.`);
			return existing;
		}
		return addChild(parent, id, name, fulfillment ?? 0) as any;
	};

	// Add all 17 SDGs directly to root with appropriate emojis
	safeAddChild(rootNode, 'sdg-1', '🚫💰 No Poverty', 6);
	safeAddChild(rootNode, 'sdg-2', '🌾 Zero Hunger', 6);
	safeAddChild(rootNode, 'sdg-3', '❤️‍🩹 Good Health', 6);
	safeAddChild(rootNode, 'sdg-4', '📚 Quality Education', 6);
	safeAddChild(rootNode, 'sdg-5', '⚖️ Gender Equality', 6);
	safeAddChild(rootNode, 'sdg-6', '💧 Clean Water', 6);
	safeAddChild(rootNode, 'sdg-7', '⚡ Clean Energy', 6);
	safeAddChild(rootNode, 'sdg-8', '💼 Decent Work', 6);
	safeAddChild(rootNode, 'sdg-9', '🏭 Innovation', 6);
	safeAddChild(rootNode, 'sdg-10', '📊 Reduced Inequality', 6);
	safeAddChild(rootNode, 'sdg-11', '🏙️ Sustainable Cities', 6);
	safeAddChild(rootNode, 'sdg-12', '♻️ Responsible Consumption', 6);
	safeAddChild(rootNode, 'sdg-13', '🌍 Climate Action', 6);
	safeAddChild(rootNode, 'sdg-14', '🌊 Life Below Water', 6);
	safeAddChild(rootNode, 'sdg-15', '🌳 Life on Land', 5);
	safeAddChild(rootNode, 'sdg-16', '🕊️ Peace & Justice', 5);
	safeAddChild(rootNode, 'sdg-17', '🤝 Partnerships', 6);

	// SDG 1: No Poverty
	const sdg1 = findNodeById(rootNode, 'sdg-1');
	if (sdg1) {
		safeAddChild(sdg1, 'poverty-finance', 'Financial Inclusion', 30);
		safeAddChild(sdg1, 'poverty-housing', 'Affordable Housing', 30);
		safeAddChild(sdg1, 'poverty-employment', 'Employment Programs', 40);
	}

	// SDG 2: Zero Hunger
	const sdg2 = findNodeById(rootNode, 'sdg-2');
	if (sdg2) {
		safeAddChild(sdg2, 'hunger-food', 'Food Distribution', 35);
		safeAddChild(sdg2, 'hunger-farming', 'Sustainable Farming', 35);
		safeAddChild(sdg2, 'hunger-nutrition', 'Nutrition Education', 30);
	}

	// SDG 3: Good Health
	const sdg3 = findNodeById(rootNode, 'sdg-3');
	if (sdg3) {
		safeAddChild(sdg3, 'health-medical', 'Medical Services', 30);
		safeAddChild(sdg3, 'health-mental', 'Mental Health', 25);
		safeAddChild(sdg3, 'health-prevention', 'Disease Prevention', 25);
		safeAddChild(sdg3, 'health-wellness', 'Community Wellness', 20);
	}

	// SDG 4: Quality Education
	const sdg4 = findNodeById(rootNode, 'sdg-4');
	if (sdg4) {
		safeAddChild(sdg4, 'edu-primary', 'Primary Education', 25);
		safeAddChild(sdg4, 'edu-vocational', 'Vocational Training', 25);
		safeAddChild(sdg4, 'edu-digital', 'Digital Literacy', 25);
		safeAddChild(sdg4, 'edu-lifelong', 'Lifelong Learning', 25);
	}

	// SDG 5: Gender Equality
	const sdg5 = findNodeById(rootNode, 'sdg-5');
	if (sdg5) {
		safeAddChild(sdg5, 'gender-empowerment', 'Women Empowerment', 40);
		safeAddChild(sdg5, 'gender-rights', 'Equal Rights', 30);
		safeAddChild(sdg5, 'gender-violence', 'Violence Prevention', 30);
	}

	// SDG 6: Clean Water
	const sdg6 = findNodeById(rootNode, 'sdg-6');
	if (sdg6) {
		safeAddChild(sdg6, 'water-access', 'Water Access', 40);
		safeAddChild(sdg6, 'water-sanitation', 'Sanitation', 35);
		safeAddChild(sdg6, 'water-treatment', 'Water Treatment', 25);
	}

	// SDG 7: Clean Energy
	const sdg7 = findNodeById(rootNode, 'sdg-7');
	if (sdg7) {
		safeAddChild(sdg7, 'energy-solar', 'Solar Power', 30);
		safeAddChild(sdg7, 'energy-wind', 'Wind Power', 25);
		safeAddChild(sdg7, 'energy-efficiency', 'Energy Efficiency', 25);
		safeAddChild(sdg7, 'energy-access', 'Energy Access', 20);
	}

	// SDG 8: Decent Work
	const sdg8 = findNodeById(rootNode, 'sdg-8');
	if (sdg8) {
		safeAddChild(sdg8, 'work-jobs', 'Job Creation', 30);
		safeAddChild(sdg8, 'work-entrepreneurship', 'Entrepreneurship', 30);
		safeAddChild(sdg8, 'work-rights', 'Labor Rights', 20);
		safeAddChild(sdg8, 'work-economy', 'Local Economy', 20);
	}

	// SDG 9: Innovation
	const sdg9 = findNodeById(rootNode, 'sdg-9');
	if (sdg9) {
		safeAddChild(sdg9, 'innovation-tech', 'Technology Development', 35);
		safeAddChild(sdg9, 'innovation-infrastructure', 'Infrastructure', 35);
		safeAddChild(sdg9, 'innovation-research', 'Research & Development', 30);
	}

	// SDG 10: Reduced Inequality
	const sdg10 = findNodeById(rootNode, 'sdg-10');
	if (sdg10) {
		safeAddChild(sdg10, 'inequality-income', 'Income Equality', 35);
		safeAddChild(sdg10, 'inequality-social', 'Social Inclusion', 35);
		safeAddChild(sdg10, 'inequality-access', 'Equal Access', 30);
	}

	// SDG 11: Sustainable Cities
	const sdg11 = findNodeById(rootNode, 'sdg-11');
	if (sdg11) {
		safeAddChild(sdg11, 'cities-transport', 'Public Transport', 30);
		safeAddChild(sdg11, 'cities-housing', 'Sustainable Housing', 25);
		safeAddChild(sdg11, 'cities-spaces', 'Green Spaces', 25);
		safeAddChild(sdg11, 'cities-waste', 'Waste Management', 20);
	}

	// SDG 12: Responsible Consumption
	const sdg12 = findNodeById(rootNode, 'sdg-12');
	if (sdg12) {
		safeAddChild(sdg12, 'consumption-circular', 'Circular Economy', 30);
		safeAddChild(sdg12, 'consumption-waste', 'Waste Reduction', 30);
		safeAddChild(sdg12, 'consumption-sustainable', 'Sustainable Products', 40);
	}

	// SDG 13: Climate Action
	const sdg13 = findNodeById(rootNode, 'sdg-13');
	if (sdg13) {
		safeAddChild(sdg13, 'climate-mitigation', 'Emissions Reduction', 35);
		safeAddChild(sdg13, 'climate-adaptation', 'Climate Adaptation', 35);
		safeAddChild(sdg13, 'climate-education', 'Climate Education', 30);
	}

	// SDG 14: Life Below Water
	const sdg14 = findNodeById(rootNode, 'sdg-14');
	if (sdg14) {
		safeAddChild(sdg14, 'ocean-conservation', 'Marine Conservation', 35);
		safeAddChild(sdg14, 'ocean-fishing', 'Sustainable Fishing', 35);
		safeAddChild(sdg14, 'ocean-pollution', 'Ocean Cleanup', 30);
	}

	// SDG 15: Life on Land
	const sdg15 = findNodeById(rootNode, 'sdg-15');
	if (sdg15) {
		safeAddChild(sdg15, 'land-forests', 'Forest Conservation', 35);
		safeAddChild(sdg15, 'land-biodiversity', 'Biodiversity Protection', 35);
		safeAddChild(sdg15, 'land-restoration', 'Land Restoration', 30);
	}

	// SDG 16: Peace & Justice
	const sdg16 = findNodeById(rootNode, 'sdg-16');
	if (sdg16) {
		safeAddChild(sdg16, 'peace-institutions', 'Strong Institutions', 30);
		safeAddChild(sdg16, 'peace-justice', 'Access to Justice', 30);
		safeAddChild(sdg16, 'peace-conflict', 'Conflict Resolution', 20);
		safeAddChild(sdg16, 'peace-transparency', 'Transparency', 20);
	}

	// SDG 17: Partnerships
	const sdg17 = findNodeById(rootNode, 'sdg-17');
	if (sdg17) {
		safeAddChild(sdg17, 'partnership-local', 'Local Partnerships', 30);
		safeAddChild(sdg17, 'partnership-global', 'Global Cooperation', 30);
		safeAddChild(sdg17, 'partnership-tech', 'Technology Sharing', 20);
		safeAddChild(sdg17, 'partnership-finance', 'Resource Mobilization', 20);
	}

	console.log('[SDG] Tree populated with all 17 SDGs at root level');

	console.log('[SDG] Injecting recognized organizations...');

	// Map of specific sub-goal IDs to relevant Demo Org IDs for deepest-level recognition
	const recognitionMap: Record<string, string[]> = {
		// SDG 1: No Poverty
		'poverty-employment': ['org_demo_undp'],
		'poverty-finance': ['org_demo_oxfam'],

		// SDG 2: Zero Hunger
		'hunger-food': ['org_demo_wfp'],
		'hunger-farming': ['org_demo_fao'],

		// SDG 3: Good Health
		'health-medical': ['org_demo_who', 'org_demo_redcross'],
		'health-prevention': ['org_demo_unicef'],

		// SDG 4: Quality Education
		'edu-primary': ['org_demo_unicef'],
		'edu-vocational': ['org_demo_tsinghua'],

		// SDG 5: Gender Equality
		'gender-rights': ['org_demo_un_women'],

		// SDG 6: Clean Water
		'water-access': ['org_demo_siwi'],
		'water-sanitation': ['org_demo_unicef'],

		// SDG 7: Clean Energy
		'energy-access': ['org_demo_irena', 'org_demo_isa'],

		// SDG 10: Reduced Inequalities
		'inequality-social': ['org_demo_unhcr', 'org_demo_oxfam'],

		// SDG 13: Climate Action
		'climate-mitigation': ['org_demo_unep', 'org_demo_unfccc'],
		'climate-adaptation': ['org_demo_ipcc'],
		'climate-education': ['org_demo_greenpeace'],

		// SDG 14: Life Below Water
		'ocean-conservation': ['org_demo_wwf', 'org_demo_oceandori'],

		// SDG 15: Life on Land
		'land-biodiversity': ['org_demo_wwf', 'org_demo_conservationinternational'],
		'land-forests': ['org_demo_natureconservancy'],

		// SDG 16: Peace & Justice
		'peace-conflict': ['org_demo_unhcr'],
		'peace-justice': ['org_demo_amnesty'],

		// SDG 17: Partnerships
		'partnership-global': ['org_demo_un', 'org_demo_worldbank']
	};

	// Inject contributors
	for (const [nodeId, orgIds] of Object.entries(recognitionMap)) {
		const node = findNodeById(rootNode, nodeId);
		if (node) {
			const validOrgIds: string[] = [];
			for (const orgId of orgIds) {
				// Only add if we have this demo org config (prevents errors for missing keys)
				if (orgId in DEMO_ORGANIZATIONS) {
					validOrgIds.push(orgId);
				}
			}

			if (validOrgIds.length > 0) {
				try {
					// Use protocol function to add contributors
					// Checks for existing contributors are handled inside or by passing current list
					const n = node as any;
					const currentContributors = (n.contributors || []).map((c: any) => c.id);
					const currentAntiContributors = (n.anti_contributors || []).map((c: any) => c.id);

					// Merge and deduplicate
					const startLength = currentContributors.length;
					for (const id of validOrgIds) {
						if (!currentContributors.includes(id)) {
							currentContributors.push(id);
						}
					}

					if (currentContributors.length > startLength) {
						addContributors(n, currentContributors, currentAntiContributors);
						// console.log(`[SDG] Added ${validOrgIds.join(', ')} to ${nodeId}`);
					}
				} catch (e) {
					console.warn(`[SDG] Failed to add contributors to ${nodeId}`, e);
				}
			}
		}
	}

	return rootNode;
}

/**
 * SDG metadata for display and reference
 */
export const SDG_METADATA = {
	1: { name: 'No Poverty', icon: '🚫', color: '#e5243b' },
	2: { name: 'Zero Hunger', icon: '🌾', color: '#dda63a' },
	3: { name: 'Good Health and Well-being', icon: '❤️', color: '#4c9f38' },
	4: { name: 'Quality Education', icon: '📚', color: '#c5192d' },
	5: { name: 'Gender Equality', icon: '⚖️', color: '#ff3a21' },
	6: { name: 'Clean Water and Sanitation', icon: '💧', color: '#26bde2' },
	7: { name: 'Affordable and Clean Energy', icon: '⚡', color: '#fcc30b' },
	8: { name: 'Decent Work and Economic Growth', icon: '💼', color: '#a21942' },
	9: { name: 'Industry, Innovation and Infrastructure', icon: '🏭', color: '#fd6925' },
	10: { name: 'Reduced Inequalities', icon: '📊', color: '#dd1367' },
	11: { name: 'Sustainable Cities and Communities', icon: '🏙️', color: '#fd9d24' },
	12: { name: 'Responsible Consumption and Production', icon: '♻️', color: '#bf8b2e' },
	13: { name: 'Climate Action', icon: '🌍', color: '#3f7e44' },
	14: { name: 'Life Below Water', icon: '🌊', color: '#0a97d9' },
	15: { name: 'Life on Land', icon: '🌳', color: '#56c02b' },
	16: { name: 'Peace, Justice and Strong Institutions', icon: '🕊️', color: '#00689d' },
	17: { name: 'Partnerships for the Goals', icon: '🤝', color: '#19486a' }
};
