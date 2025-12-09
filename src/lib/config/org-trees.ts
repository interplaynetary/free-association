/**
 * Organization Tree Configuration System
 * 
 * Maps organization slugs to custom tree configurations for tailored interfaces.
 * Each organization gets a pre-populated tree structure loaded from JSON config.
 */

import type { RootNode, Organization, Contributor } from '../../../packages/protocol/src/schemas';
import { RootNodeSchema } from '../../../packages/protocol/src/schemas';
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
		monthly_budget: (config as any).monthly_budget,
		recognizes: (config as any).recognizes
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
	wemeanbus: 'org_demo_wemeanbus',
	// COP30 Additional Organizations
	ilo: 'org_demo_ilo',
	iaea: 'org_demo_iaea',
	irena: 'org_demo_irena',
	iso: 'org_demo_iso',
	iom: 'org_demo_iom',
	wmo: 'org_demo_wmo',
	unfccc: 'org_demo_unfccc',
	nep: 'org_demo_nep',
	iclei: 'org_demo_iclei',
	cdri: 'org_demo_cdri',
	icc_chamber: 'org_demo_icc_chamber',
	siwi: 'org_demo_siwi',
	isa: 'org_demo_isa',
	intosai: 'org_demo_intosai',
	ramsar: 'org_demo_ramsar',
	ndc_partnership: 'org_demo_ndc_partnership',
	cfrn: 'org_demo_cfrn',
	wwea: 'org_demo_wwea',
	climate_registry: 'org_demo_climate_registry',
	wgeo: 'org_demo_wgeo',
	ibam: 'org_demo_ibam',
	yle: 'org_demo_yle',
	iync: 'org_demo_iync',
	ycla: 'org_demo_ycla',
	monterrey: 'org_demo_monterrey',
	children_youth_pavilion: 'org_demo_children_youth_pavilion',
	iaai: 'org_demo_iaai',
	yilaa: 'org_demo_yilaa',
	wna: 'org_demo_wna',
	sdce: 'org_demo_sdce',
	climate_live: 'org_demo_climate_live',
	eac: 'org_demo_eac',
	ethiopia: 'org_demo_ethiopia',
	namibia: 'org_demo_namibia',
	tanzania: 'org_demo_tanzania',
	sweden: 'org_demo_sweden',
	deval: 'org_demo_deval',
	tsinghua: 'org_demo_tsinghua',
	oif: 'org_demo_oif',
	climate_funds: 'org_demo_climate_funds',
	ens: 'org_demo_ens',
	finland: 'org_demo_finland',
	france: 'org_demo_france',
	spain: 'org_demo_spain',
	cuba: 'org_demo_cuba',
	ukraine: 'org_demo_ukraine',
	liberia: 'org_demo_liberia',
	sierra_leone: 'org_demo_sierra_leone',
	cote_ivoire: 'org_demo_cote_ivoire',
	mali: 'org_demo_mali',
	afdb: 'org_demo_afdb',
	malawi: 'org_demo_malawi',
	djibouti: 'org_demo_djibouti',
	congo_drc: 'org_demo_congo_drc',
	peru: 'org_demo_peru',
	can: 'org_demo_can',
	senegal: 'org_demo_senegal',
	ldc_group: 'org_demo_ldc_group',
	chad: 'org_demo_chad',
	rwanda: 'org_demo_rwanda',
	mongolia: 'org_demo_mongolia',
	gabon: 'org_demo_gabon',
	cni: 'org_demo_cni',
	oman: 'org_demo_oman',
	nigeria: 'org_demo_nigeria',
	qatar: 'org_demo_qatar',
	denmark: 'org_demo_denmark',
	iadb: 'org_demo_iadb',
	angola: 'org_demo_angola',
	chile: 'org_demo_chile',
	pakistan: 'org_demo_pakistan',
	luxembourg: 'org_demo_luxembourg',
	morocco: 'org_demo_morocco',
	colombia: 'org_demo_colombia',
	el_salvador: 'org_demo_el_salvador',
	iceland: 'org_demo_iceland',
	guinea: 'org_demo_guinea',
	uruguay: 'org_demo_uruguay',
	mauritania: 'org_demo_mauritania',
	india: 'org_demo_india',
	italy: 'org_demo_italy',
	brazil: 'org_demo_brazil',
	portugal: 'org_demo_portugal',
	china: 'org_demo_china',
	uk: 'org_demo_uk',
	malaysia: 'org_demo_malaysia',
	indonesia: 'org_demo_indonesia',
	azerbaijan: 'org_demo_azerbaijan',
	australia: 'org_demo_australia',
	turkiye: 'org_demo_turkiye',
	south_korea: 'org_demo_south_korea',
	bangladesh: 'org_demo_bangladesh',
	saudi_arabia: 'org_demo_saudi_arabia',
	singapore: 'org_demo_singapore',
	thailand: 'org_demo_thailand',
	germany: 'org_demo_germany',
	venezuela: 'org_demo_venezuela',
	uganda: 'org_demo_uganda',
	unica: 'org_demo_unica',
	caf: 'org_demo_caf',
	ipam: 'org_demo_ipam',
	sitawi: 'org_demo_sitawi',
	arapyau: 'org_demo_arapyau',
	multilateral_banks: 'org_demo_multilateral_banks',
	kfw: 'org_demo_kfw',
	open_society: 'org_demo_open_society',
	indigenous_pavilion: 'org_demo_indigenous_pavilion',
	oceandori: 'org_demo_oceandori',
	wipo: 'org_demo_wipo',
	norroway_org: 'org_demo_norroway_org'
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
	},
	// ═══════════════════════════════════════════════════════════════════
	// COP30 ADDITIONAL ORGANIZATIONS
	// ═══════════════════════════════════════════════════════════════════
	org_demo_ilo: {
		org_id: 'org_demo_ilo',
		names: { en: 'International Labour Organization' },
		emoji: '🌍',
		description: 'Just transition and green jobs for climate economy',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iaea: {
		org_id: 'org_demo_iaea',
		names: { en: 'International Atomic Energy Agency' },
		emoji: '⚛️',
		description: 'Nuclear energy for clean power transition',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_irena: {
		org_id: 'org_demo_irena',
		names: { en: 'International Renewable Energy Agency' },
		emoji: '☀️',
		description: 'Leading global renewable energy transition',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iso: {
		org_id: 'org_demo_iso',
		names: { en: 'International Organization for Standardization' },
		emoji: '📊',
		description: 'Climate and sustainability standards',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iom: {
		org_id: 'org_demo_iom',
		names: { en: 'International Organization for Migration' },
		emoji: '🚶',
		description: 'Managing climate-induced migration',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wmo: {
		org_id: 'org_demo_wmo',
		names: { en: 'World Meteorological Organization' },
		emoji: '🌦️',
		description: 'Climate monitoring and early warning',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_unfccc: {
		org_id: 'org_demo_unfccc',
		names: { en: 'United Nations Climate Change (UNFCCC)' },
		emoji: '🌐',
		description: 'Coordinating global climate action',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_nep: {
		org_id: 'org_demo_nep',
		names: { en: 'Negative Emissions Platform' },
		emoji: '🔄',
		description: 'Carbon removal technologies',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iclei: {
		org_id: 'org_demo_iclei',
		names: { en: 'ICLEI-Local Governments for Sustainability' },
		emoji: '🏙️',
		description: 'Cities committed to sustainability',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_cdri: {
		org_id: 'org_demo_cdri',
		names: { en: 'Coalition for Disaster Resilient Infrastructure' },
		emoji: '🏗️',
		description: 'Climate-resilient infrastructure',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_icc_chamber: {
		org_id: 'org_demo_icc_chamber',
		names: { en: 'International Chamber of Commerce' },
		emoji: '💼',
		description: 'Business climate leadership',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_siwi: {
		org_id: 'org_demo_siwi',
		names: { en: 'Stockholm International Water Institute' },
		emoji: '💧',
		description: 'Water and climate resilience',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_isa: {
		org_id: 'org_demo_isa',
		names: { en: 'International Solar Alliance' },
		emoji: '☀️',
		description: 'Solar energy deployment',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_intosai: {
		org_id: 'org_demo_intosai',
		names: { en: 'International Organization of Supreme Audit Institutions' },
		emoji: '📋',
		description: 'Climate finance auditing',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_ramsar: {
		org_id: 'org_demo_ramsar',
		names: { en: 'Ramsar Convention Secretariat' },
		emoji: '🦆',
		description: 'Wetlands conservation',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_ndc_partnership: {
		org_id: 'org_demo_ndc_partnership',
		names: { en: 'NDC Partnership' },
		emoji: '🤝',
		description: 'Supporting Paris commitments',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_cfrn: {
		org_id: 'org_demo_cfrn',
		names: { en: 'Coalition for Rainforest Nations' },
		emoji: '🌳',
		description: 'REDD+ and rainforest conservation',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wwea: {
		org_id: 'org_demo_wwea',
		names: { en: 'World Wind Energy Association' },
		emoji: '💨',
		description: 'Wind energy worldwide',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_climate_registry: {
		org_id: 'org_demo_climate_registry',
		names: { en: 'The Climate Registry' },
		emoji: '📊',
		description: 'GHG measurement and reporting',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wgeo: {
		org_id: 'org_demo_wgeo',
		names: { en: 'World Green Economy Organization' },
		emoji: '♻️',
		description: 'Green economy transformation',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_ibam: {
		org_id: 'org_demo_ibam',
		names: { en: 'Institute of Environmental Well (IBAM)' },
		emoji: '🔬',
		description: 'Environmental research and sustainable development',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_yle: {
		org_id: 'org_demo_yle',
		names: { en: 'Youth Love Egypt Foundation' },
		emoji: '🇪🇬',
		description: 'Youth climate action and empowerment in Egypt',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iync: {
		org_id: 'org_demo_iync',
		names: { en: 'International Youth Nuclear Congress' },
		emoji: '⚛️',
		description: 'Young nuclear professionals for clean energy',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_ycla: {
		org_id: 'org_demo_ycla',
		names: { en: 'Youth Climate Leader Association' },
		emoji: '👥',
		description: 'Youth climate leadership development',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_monterrey: {
		org_id: 'org_demo_monterrey',
		names: { en: 'Tecnológico de Monterrey' },
		emoji: '🎓',
		description: 'Technology and innovation for sustainability',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_children_youth_pavilion: {
		org_id: 'org_demo_children_youth_pavilion',
		names: { en: 'Children and Youth Pavilion' },
		emoji: '🧒',
		description: 'Child and youth rights in climate action',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_iaai: {
		org_id: 'org_demo_iaai',
		names: { en: 'International Association for Innovation to Global Changes' },
		emoji: '💡',
		description: 'Innovation for climate adaptation',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_yilaa: {
		org_id: 'org_demo_yilaa',
		names: { en: 'Youth Initiative for Land in Africa' },
		emoji: '🌱',
		description: 'Youth-led land restoration in Africa',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_wna: {
		org_id: 'org_demo_wna',
		names: { en: 'World Nuclear Association' },
		emoji: '⚛️',
		description: 'Nuclear industry for clean energy',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_sdce: {
		org_id: 'org_demo_sdce',
		names: { en: 'Society for Development and Community Empowerment' },
		emoji: '🤝',
		description: 'Community-led development',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_climate_live: {
		org_id: 'org_demo_climate_live',
		names: { en: 'Climate Live' },
		emoji: '🎵',
		description: 'Global climate music and arts movement',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_eac: {
		org_id: 'org_demo_eac',
		names: { en: 'East African Community' },
		emoji: '🌍',
		description: 'Regional climate cooperation in East Africa',
		created_at: 1704067200000,
		updated_at: 1704067200000
	},
	org_demo_ethiopia: { org_id: 'org_demo_ethiopia', names: { en: 'Ethiopia' }, emoji: '🇪🇹', description: 'Ethiopian climate action and green development', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_namibia: { org_id: 'org_demo_namibia', names: { en: 'Namibia' }, emoji: '🇳🇦', description: 'Namibian climate resilience and conservation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_tanzania: { org_id: 'org_demo_tanzania', names: { en: 'Tanzania' }, emoji: '🇹🇿', description: 'Tanzanian sustainable development', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_sweden: { org_id: 'org_demo_sweden', names: { en: 'Sweden' }, emoji: '🇸🇪', description: 'Swedish climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_deval: { org_id: 'org_demo_deval', names: { en: 'German Institute for Development Evaluation' }, emoji: '📊', description: 'Evaluating development and climate programs', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_tsinghua: { org_id: 'org_demo_tsinghua', names: { en: 'Tsinghua University Global Climate Governance' }, emoji: '🎓', description: 'Climate research and policy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_oif: { org_id: 'org_demo_oif', names: { en: 'Organisation Internationale de la Francophonie' }, emoji: '🗣️', description: 'Francophone climate cooperation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_climate_funds: { org_id: 'org_demo_climate_funds', names: { en: 'The Climate Funds Pavilion' }, emoji: '💰', description: 'Climate finance coordination', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_ens: { org_id: 'org_demo_ens', names: { en: 'European Nuclear Society' }, emoji: '⚛️', description: 'Nuclear technology for Europe', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_finland: { org_id: 'org_demo_finland', names: { en: 'Finland' }, emoji: '🇫🇮', description: 'Finnish climate ambition', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_france: { org_id: 'org_demo_france', names: { en: 'France' }, emoji: '🇫🇷', description: 'French climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_spain: { org_id: 'org_demo_spain', names: { en: 'Spain' }, emoji: '🇪🇸', description: 'Spanish climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_cuba: { org_id: 'org_demo_cuba', names: { en: 'Cuba' }, emoji: '🇨🇺', description: 'Cuban climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_ukraine: { org_id: 'org_demo_ukraine', names: { en: 'Ukraine' }, emoji: '🇺🇦', description: 'Ukrainian green reconstruction', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_liberia: { org_id: 'org_demo_liberia', names: { en: 'Liberia' }, emoji: '🇱🇷', description: 'Liberian forest conservation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_sierra_leone: { org_id: 'org_demo_sierra_leone', names: { en: 'Sierra Leone' }, emoji: '🇸🇱', description: 'Sierra Leone climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_cote_ivoire: { org_id: 'org_demo_cote_ivoire', names: { en: 'Côte d\'Ivoire' }, emoji: '🇨🇮', description: 'Ivorian climate and development', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_mali: { org_id: 'org_demo_mali', names: { en: 'Mali' }, emoji: '🇲🇱', description: 'Mali climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_afdb: { org_id: 'org_demo_afdb', names: { en: 'African Development Bank' }, emoji: '🏦', description: 'Africa climate finance and development', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_malawi: { org_id: 'org_demo_malawi', names: { en: 'Malawi' }, emoji: '🇲🇼', description: 'Malawian climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_djibouti: { org_id: 'org_demo_djibouti', names: { en: 'Djibouti' }, emoji: '🇩🇯', description: 'Djibouti climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_congo_drc: { org_id: 'org_demo_congo_drc', names: { en: 'Democratic Republic of Congo' }, emoji: '🇨🇩', description: 'DRC forest conservation and climate', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_peru: { org_id: 'org_demo_peru', names: { en: 'Peru' }, emoji: '🇵🇪', description: 'Peruvian climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_can: { org_id: 'org_demo_can', names: { en: 'Climate Action Network International' }, emoji: '🌐', description: 'Global NGO climate coalition', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_senegal: { org_id: 'org_demo_senegal', names: { en: 'Senegal' }, emoji: '🇸🇳', description: 'Senegalese green growth', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_ldc_group: { org_id: 'org_demo_ldc_group', names: { en: 'LDC Group' }, emoji: '🌍', description: 'Least Developed Countries climate coalition', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_chad: { org_id: 'org_demo_chad', names: { en: 'Chad' }, emoji: '🇹🇩', description: 'Chad climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_rwanda: { org_id: 'org_demo_rwanda', names: { en: 'Rwanda' }, emoji: '🇷🇼', description: 'Rwandan green development', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_mongolia: { org_id: 'org_demo_mongolia', names: { en: 'Mongolia' }, emoji: '🇲🇳', description: 'Mongolian climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_gabon: { org_id: 'org_demo_gabon', names: { en: 'Gabon' }, emoji: '🇬🇦', description: 'Gabonese forest conservation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_cni: { org_id: 'org_demo_cni', names: { en: 'Brazilian National Confederation of Industry' }, emoji: '🏭', description: 'Brazilian industry decarbonization', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_oman: { org_id: 'org_demo_oman', names: { en: 'Oman' }, emoji: '🇴🇲', description: 'Omani green hydrogen and renewable energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_nigeria: { org_id: 'org_demo_nigeria', names: { en: 'Nigeria' }, emoji: '🇳🇬', description: 'Nigerian climate action and energy transition', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_qatar: { org_id: 'org_demo_qatar', names: { en: 'Qatar' }, emoji: '🇶🇦', description: 'Qatari climate and sustainability', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_denmark: { org_id: 'org_demo_denmark', names: { en: 'Denmark' }, emoji: '🇩🇰', description: 'Danish climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_iadb: { org_id: 'org_demo_iadb', names: { en: 'Inter-American Development Bank' }, emoji: '🏦', description: 'Latin America climate finance', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_angola: { org_id: 'org_demo_angola', names: { en: 'Angola' }, emoji: '🇦🇴', description: 'Angolan climate and energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_chile: { org_id: 'org_demo_chile', names: { en: 'Chile' }, emoji: '🇨🇱', description: 'Chilean climate ambition', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_pakistan: { org_id: 'org_demo_pakistan', names: { en: 'Pakistan' }, emoji: '🇵🇰', description: 'Pakistani climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_luxembourg: { org_id: 'org_demo_luxembourg', names: { en: 'Luxembourg' }, emoji: '🇱🇺', description: 'Luxembourg climate finance', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_morocco: { org_id: 'org_demo_morocco', names: { en: 'Morocco' }, emoji: '🇲🇦', description: 'Moroccan renewable energy leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_colombia: { org_id: 'org_demo_colombia', names: { en: 'Colombia' }, emoji: '🇨🇴', description: 'Colombian biodiversity and climate', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_el_salvador: { org_id: 'org_demo_el_salvador', names: { en: 'El Salvador' }, emoji: '🇸🇻', description: 'El Salvador climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_iceland: { org_id: 'org_demo_iceland', names: { en: 'Iceland' }, emoji: '🇮🇸', description: 'Icelandic renewable energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_guinea: { org_id: 'org_demo_guinea', names: { en: 'Guinea' }, emoji: '🇬🇳', description: 'Guinean climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_uruguay: { org_id: 'org_demo_uruguay', names: { en: 'Uruguay' }, emoji: '🇺🇾', description: 'Uruguayan renewable leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_mauritania: { org_id: 'org_demo_mauritania', names: { en: 'Mauritania' }, emoji: '🇲🇷', description: 'Mauritanian climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_india: { org_id: 'org_demo_india', names: { en: 'India' }, emoji: '🇮🇳', description: 'Indian climate action and renewable energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_italy: { org_id: 'org_demo_italy', names: { en: 'Italy' }, emoji: '🇮🇹', description: 'Italian climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_brazil: { org_id: 'org_demo_brazil', names: { en: 'Brazil' }, emoji: '🇧🇷', description: 'Brazilian climate and Amazon protection', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_portugal: { org_id: 'org_demo_portugal', names: { en: 'Portugal' }, emoji: '🇵🇹', description: 'Portuguese climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_china: { org_id: 'org_demo_china', names: { en: 'China' }, emoji: '🇨🇳', description: 'Chinese climate action and renewable energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_uk: { org_id: 'org_demo_uk', names: { en: 'United Kingdom' }, emoji: '🇬🇧', description: 'UK climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_malaysia: { org_id: 'org_demo_malaysia', names: { en: 'Malaysia' }, emoji: '🇲🇾', description: 'Malaysian climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_indonesia: { org_id: 'org_demo_indonesia', names: { en: 'Indonesia' }, emoji: '🇮🇩', description: 'Indonesian climate and forest action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_azerbaijan: { org_id: 'org_demo_azerbaijan', names: { en: 'Azerbaijan' }, emoji: '🇦🇿', description: 'Azerbaijan climate and energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_australia: { org_id: 'org_demo_australia', names: { en: 'Australia' }, emoji: '🇦🇺', description: 'Australian climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_turkiye: { org_id: 'org_demo_turkiye', names: { en: 'Türkiye' }, emoji: '🇹🇷', description: 'Turkish climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_south_korea: { org_id: 'org_demo_south_korea', names: { en: 'Republic of Korea' }, emoji: '🇰🇷', description: 'Korean green growth', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_bangladesh: { org_id: 'org_demo_bangladesh', names: { en: 'Bangladesh' }, emoji: '🇧🇩', description: 'Bangladeshi climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_saudi_arabia: { org_id: 'org_demo_saudi_arabia', names: { en: 'Saudi Arabia' }, emoji: '🇸🇦', description: 'Saudi climate and green energy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_singapore: { org_id: 'org_demo_singapore', names: { en: 'Singapore' }, emoji: '🇸🇬', description: 'Singapore climate innovation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_thailand: { org_id: 'org_demo_thailand', names: { en: 'Thailand' }, emoji: '🇹🇭', description: 'Thai climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_germany: { org_id: 'org_demo_germany', names: { en: 'Germany' }, emoji: '🇩🇪', description: 'German Energiewende and climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_venezuela: { org_id: 'org_demo_venezuela', names: { en: 'Venezuela' }, emoji: '🇻🇪', description: 'Venezuelan climate action', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_uganda: { org_id: 'org_demo_uganda', names: { en: 'Uganda' }, emoji: '🇺🇬', description: 'Ugandan climate resilience', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_unica: { org_id: 'org_demo_unica', names: { en: 'UNICA (Brazilian Sugarcane Industry Association)' }, emoji: '🌾', description: 'Sustainable biofuels and bioeconomy', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_caf: { org_id: 'org_demo_caf', names: { en: 'CAF Development Bank of Latin America' }, emoji: '🏦', description: 'Latin American development and climate', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_ipam: { org_id: 'org_demo_ipam', names: { en: 'IPAM-Amazon Environmental Research' }, emoji: '🔬', description: 'Amazon research and conservation', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_sitawi: { org_id: 'org_demo_sitawi', names: { en: 'SITAWI-BEG' }, emoji: '💰', description: 'Impact finance for sustainability in Brazil', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_arapyau: { org_id: 'org_demo_arapyau', names: { en: 'Instituto Arapyaú' }, emoji: '🌱', description: 'Sustainable development and innovation in Brazil', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_multilateral_banks: { org_id: 'org_demo_multilateral_banks', names: { en: 'Multilateral Banks Pavilion' }, emoji: '🏛️', description: 'Coordinated multilateral development finance', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_kfw: { org_id: 'org_demo_kfw', names: { en: 'KFW (IDFC)' }, emoji: '🏦', description: 'German development finance', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_open_society: { org_id: 'org_demo_open_society', names: { en: 'Open Society Institute' }, emoji: '⚖️', description: 'Climate justice and governance', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_indigenous_pavilion: { org_id: 'org_demo_indigenous_pavilion', names: { en: 'Indigenous Peoples Pavilion' }, emoji: '🌿', description: 'Indigenous climate leadership', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_oceandori: { org_id: 'org_demo_oceandori', names: { en: 'Ocean (DORI-SPP)' }, emoji: '🌊', description: 'Ocean climate and marine protection', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_wipo: { org_id: 'org_demo_wipo', names: { en: 'World Indigenous Peoples Organization' }, emoji: '🌍', description: 'Indigenous climate action coordination', created_at: 1704067200000, updated_at: 1704067200000 },
	org_demo_norroway_org: { org_id: 'org_demo_norroway_org', names: { en: 'Norway Organization' }, emoji: '🇳🇴', description: 'Norwegian climate and forest finance', created_at: 1704067200000, updated_at: 1704067200000 }
};

