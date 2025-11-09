/**
 * Organization Tree Configuration System
 * 
 * Maps organization slugs to custom tree configurations for tailored interfaces.
 * Each organization gets a pre-populated tree structure loaded from JSON config.
 */

import type { RootNode, Organization, Contributor } from '$lib/protocol/schemas';
import { RootNodeSchema } from '$lib/protocol/schemas';
import orgTreesConfig from './org-trees.json';

/**
 * Organization Tree Configuration Entry
 */
export interface OrgTreeConfig {
	slug: string;
	name: string;
	description: string;
	monthly_budget?: number;
	tree: RootNode;
	recognizes?: Contributor[]; // Optional: org contributors to inject
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
 * Get the full organization trees map
 */
export function getOrgTreesMap(): OrgTreesMap {
	return orgTreesConfig as OrgTreesMap;
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
export function getOrgMetadata(slug: string): { name: string; description: string; monthly_budget?: number; recognizes?: Contributor[] } | null {
	const config = orgTreesConfig[slug as keyof typeof orgTreesConfig];
	
	if (!config) {
		return null;
	}
	
	return {
		name: config.name,
		description: config.description,
		monthly_budget: config.monthly_budget,
		recognizes: config.recognizes
	};
}

/**
 * Format a number with K/M/B notation
 * @param num - Number to format
 * @returns Formatted string (e.g., "27.3B", "1.5M", "500K")
 */
export function formatBudget(num: number): string {
	if (num >= 1_000_000_000) {
		return (num / 1_000_000_000).toFixed(1) + 'B';
	} else if (num >= 1_000_000) {
		return (num / 1_000_000).toFixed(1) + 'M';
	} else if (num >= 1_000) {
		return (num / 1_000).toFixed(1) + 'K';
	}
	return num.toString();
}

/**
 * Check if an organization slug exists
 */
export function isValidOrgSlug(slug: string): boolean {
	return slug in orgTreesConfig;
}

// ═══════════════════════════════════════════════════════════════════
// DEMO ORGANIZATIONS (Bootstrap for Showcase)
// ═══════════════════════════════════════════════════════════════════

/**
 * Demo org IDs (COP30 Climate Organizations - predictable format for config file)
 */
export const DEMO_ORG_IDS = {
	// UN Agencies
	unep: 'org_demo_unep',
	undp: 'org_demo_undp',
	unicef: 'org_demo_unicef',
	unhcr: 'org_demo_unhcr',
	wfp: 'org_demo_wfp',
	who: 'org_demo_who',
	fao: 'org_demo_fao',
	unhabitat: 'org_demo_unhabitat',
	// Climate Science
	ipcc: 'org_demo_ipcc',
	// Conservation NGOs
	greenpeace: 'org_demo_greenpeace',
	wwf: 'org_demo_wwf',
	natureconservancy: 'org_demo_natureconservancy',
	conservationinternational: 'org_demo_conservationinternational',
	oxfam: 'org_demo_oxfam',
	redcross: 'org_demo_redcross',
	threefiveozero: 'org_demo_threefiveozero',
	// Climate Finance
	greenclimatefund: 'org_demo_greenclimatefund',
	worldbank: 'org_demo_worldbank',
	imf: 'org_demo_imf',
	asiandevbank: 'org_demo_asiandevbank',
	africandevbank: 'org_demo_africandevbank',
	// Philanthropic Foundations
	bezosearthfund: 'org_demo_bezosearthfund',
	gatesfoundation: 'org_demo_gatesfoundation',
	rockefellerfoundation: 'org_demo_rockefellerfoundation',
	bloombergphilanthropies: 'org_demo_bloombergphilanthropies',
	fordfoundation: 'org_demo_fordfoundation',
	climateworks: 'org_demo_climateworks',
	// Regional & Political Bodies
	europeanunion: 'org_demo_europeanunion',
	africanunion: 'org_demo_africanunion',
	aosis: 'org_demo_aosis',
	c40cities: 'org_demo_c40cities',
	// Climate Coalitions
	climateactionnetwork: 'org_demo_climateactionnetwork',
	wemeanbus: 'org_demo_wemeanbus'
} as const;

/**
 * Demo organizations (COP30 participants - using real Organization schema)
 * These are pre-registered organizations for demo showcase.
 * In production, organizations would come from Holster network.
 */
export const DEMO_ORGANIZATIONS: Record<string, Organization> = {
	// ═══════════════════════════════════════════════════════════════════
	// UN AGENCIES
	// ═══════════════════════════════════════════════════════════════════
	org_demo_unep: {
		org_id: 'org_demo_unep',
		names: { en: 'UNEP', es: 'PNUMA', fr: 'PNUE' },
		emoji: '🌍',
		description: 'UN Environment Programme - Global Environmental Authority',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_undp: {
		org_id: 'org_demo_undp',
		names: { en: 'UNDP', es: 'PNUD', fr: 'PNUD' },
		emoji: '🌱',
		description: 'UN Development Programme - Sustainable Development',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_unicef: {
		org_id: 'org_demo_unicef',
		names: { en: 'UNICEF', es: 'UNICEF', fr: 'UNICEF' },
		emoji: '🧒',
		description: 'UN Children\'s Fund - Climate-Affected Children',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_unhcr: {
		org_id: 'org_demo_unhcr',
		names: { en: 'UNHCR', es: 'ACNUR', fr: 'HCR' },
		emoji: '🏕️',
		description: 'UN Refugee Agency - Climate Displacement',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wfp: {
		org_id: 'org_demo_wfp',
		names: { en: 'WFP', es: 'PMA', fr: 'PAM' },
		emoji: '🌾',
		description: 'World Food Programme - Climate & Food Security',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_who: {
		org_id: 'org_demo_who',
		names: { en: 'WHO', es: 'OMS', fr: 'OMS' },
		emoji: '🏥',
		description: 'World Health Organization - Climate Health',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_fao: {
		org_id: 'org_demo_fao',
		names: { en: 'FAO', es: 'FAO', fr: 'FAO' },
		emoji: '🚜',
		description: 'Food & Agriculture Organization - Climate Agriculture',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_unhabitat: {
		org_id: 'org_demo_unhabitat',
		names: { en: 'UN-Habitat', es: 'ONU-Hábitat', fr: 'ONU-Habitat' },
		emoji: '🏙️',
		description: 'UN Human Settlements - Sustainable Cities',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// CLIMATE SCIENCE
	// ═══════════════════════════════════════════════════════════════════
	org_demo_ipcc: {
		org_id: 'org_demo_ipcc',
		names: { en: 'IPCC', es: 'IPCC', fr: 'GIEC' },
		emoji: '📊',
		description: 'Intergovernmental Panel on Climate Change',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// CONSERVATION & ENVIRONMENTAL NGOs
	// ═══════════════════════════════════════════════════════════════════
	org_demo_greenpeace: {
		org_id: 'org_demo_greenpeace',
		names: { en: 'Greenpeace', es: 'Greenpeace', fr: 'Greenpeace' },
		emoji: '🌊',
		description: 'Climate Activism & Environmental Justice',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wwf: {
		org_id: 'org_demo_wwf',
		names: { en: 'WWF', es: 'WWF', fr: 'WWF' },
		emoji: '🐼',
		description: 'World Wildlife Fund - Conservation',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_natureconservancy: {
		org_id: 'org_demo_natureconservancy',
		names: { en: 'Nature Conservancy', es: 'Conservación de la Naturaleza', fr: 'Nature Conservancy' },
		emoji: '🌳',
		description: 'Nature-Based Climate Solutions',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_conservationinternational: {
		org_id: 'org_demo_conservationinternational',
		names: { en: 'Conservation International', es: 'Conservación Internacional', fr: 'Conservation International' },
		emoji: '🦜',
		description: 'Biodiversity & Climate Solutions',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_oxfam: {
		org_id: 'org_demo_oxfam',
		names: { en: 'Oxfam', es: 'Oxfam', fr: 'Oxfam' },
		emoji: '⚖️',
		description: 'Climate Justice & Equity',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_redcross: {
		org_id: 'org_demo_redcross',
		names: { en: 'Red Cross', es: 'Cruz Roja', fr: 'Croix-Rouge' },
		emoji: '❤️',
		description: 'Humanitarian Climate Response',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_threefiveozero: {
		org_id: 'org_demo_threefiveozero',
		names: { en: '350.org', es: '350.org', fr: '350.org' },
		emoji: '✊',
		description: 'Fossil Fuel Divestment & Climate Action',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// CLIMATE FINANCE INSTITUTIONS
	// ═══════════════════════════════════════════════════════════════════
	org_demo_greenclimatefund: {
		org_id: 'org_demo_greenclimatefund',
		names: { en: 'Green Climate Fund', es: 'Fondo Verde del Clima', fr: 'Fonds vert pour le climat' },
		emoji: '💚',
		description: 'Climate Finance Mobilization',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_worldbank: {
		org_id: 'org_demo_worldbank',
		names: { en: 'World Bank', es: 'Banco Mundial', fr: 'Banque Mondiale' },
		emoji: '🏦',
		description: 'Climate Finance & Development',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_imf: {
		org_id: 'org_demo_imf',
		names: { en: 'IMF', es: 'FMI', fr: 'FMI' },
		emoji: '💰',
		description: 'International Monetary Fund - Climate Economics',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_asiandevbank: {
		org_id: 'org_demo_asiandevbank',
		names: { en: 'Asian Development Bank', es: 'Banco Asiático de Desarrollo', fr: 'Banque Asiatique de Développement' },
		emoji: '🏯',
		description: 'Climate Finance Asia-Pacific',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_africandevbank: {
		org_id: 'org_demo_africandevbank',
		names: { en: 'African Development Bank', es: 'Banco Africano de Desarrollo', fr: 'Banque Africaine de Développement' },
		emoji: '🦁',
		description: 'Climate Finance Africa',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// PHILANTHROPIC FOUNDATIONS
	// ═══════════════════════════════════════════════════════════════════
	org_demo_bezosearthfund: {
		org_id: 'org_demo_bezosearthfund',
		names: { en: 'Bezos Earth Fund', es: 'Fondo de la Tierra Bezos', fr: 'Fonds Terre Bezos' },
		emoji: '🚀',
		description: 'Large-Scale Climate Solutions',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_gatesfoundation: {
		org_id: 'org_demo_gatesfoundation',
		names: { en: 'Gates Foundation', es: 'Fundación Gates', fr: 'Fondation Gates' },
		emoji: '🔬',
		description: 'Climate Innovation & Agriculture',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_rockefellerfoundation: {
		org_id: 'org_demo_rockefellerfoundation',
		names: { en: 'Rockefeller Foundation', es: 'Fundación Rockefeller', fr: 'Fondation Rockefeller' },
		emoji: '🏛️',
		description: 'Climate Resilience',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_bloombergphilanthropies: {
		org_id: 'org_demo_bloombergphilanthropies',
		names: { en: 'Bloomberg Philanthropies', es: 'Filantropías Bloomberg', fr: 'Philanthropies Bloomberg' },
		emoji: '🌆',
		description: 'Cities & Climate Action',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_fordfoundation: {
		org_id: 'org_demo_fordfoundation',
		names: { en: 'Ford Foundation', es: 'Fundación Ford', fr: 'Fondation Ford' },
		emoji: '✊🏾',
		description: 'Climate Justice & Equity',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_climateworks: {
		org_id: 'org_demo_climateworks',
		names: { en: 'ClimateWorks Foundation', es: 'Fundación ClimateWorks', fr: 'Fondation ClimateWorks' },
		emoji: '🎯',
		description: 'Climate Policy & Solutions',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// REGIONAL & POLITICAL BODIES
	// ═══════════════════════════════════════════════════════════════════
	org_demo_europeanunion: {
		org_id: 'org_demo_europeanunion',
		names: { en: 'European Union', es: 'Unión Europea', fr: 'Union Européenne' },
		emoji: '🇪🇺',
		description: 'Climate Policy & Finance Leader',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_africanunion: {
		org_id: 'org_demo_africanunion',
		names: { en: 'African Union', es: 'Unión Africana', fr: 'Union Africaine' },
		emoji: '🌍',
		description: 'Continental Climate Coordination',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_aosis: {
		org_id: 'org_demo_aosis',
		names: { en: 'AOSIS', es: 'AOSIS', fr: 'AOSIS' },
		emoji: '🏝️',
		description: 'Alliance of Small Island States',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_c40cities: {
		org_id: 'org_demo_c40cities',
		names: { en: 'C40 Cities', es: 'Ciudades C40', fr: 'Villes C40' },
		emoji: '🏙️',
		description: 'Urban Climate Action Network',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	// ═══════════════════════════════════════════════════════════════════
	// CLIMATE COALITIONS
	// ═══════════════════════════════════════════════════════════════════
	org_demo_climateactionnetwork: {
		org_id: 'org_demo_climateactionnetwork',
		names: { en: 'Climate Action Network', es: 'Red de Acción Climática', fr: 'Réseau Action Climat' },
		emoji: '🌐',
		description: 'NGO Climate Coordination',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wemeanbus: {
		org_id: 'org_demo_wemeanbus',
		names: { en: 'We Mean Business', es: 'Hablamos en Serio', fr: 'Nous Sommes Sérieux' },
		emoji: '💼',
		description: 'Corporate Climate Action Coalition',
		created_at: 1704067200000,
		updated_at: 1704067200000
	}
};

