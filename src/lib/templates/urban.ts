import { addChild, findNodeById } from '@playnet/free-association/tree';
import type { RootNode } from '@playnet/free-association/schemas';

/**
 * Urban Contexts color palette
 * Inspired by urban planning and city systems
 */
export const URBAN_COLORS: Record<string, string> = {
    // Housing & Shelter
    'urban-housing': '#e67e22', // Orange
    'housing-affordable': '#e67e22',
    'housing-social': '#e67e22',
    'housing-emergency': '#e67e22',
    'housing-cooperative': '#e67e22',

    // Mobility & Transport
    'urban-mobility': '#3498db', // Blue
    'mobility-public': '#3498db',
    'mobility-active': '#3498db',
    'mobility-accessibility': '#3498db',
    'mobility-freight': '#3498db',

    // Public Space
    'urban-public-space': '#27ae60', // Green
    'space-parks': '#27ae60',
    'space-plazas': '#27ae60',
    'space-streets': '#27ae60',
    'space-community': '#27ae60',

    // Infrastructure
    'urban-infrastructure': '#95a5a6', // Gray
    'infra-water': '#95a5a6',
    'infra-energy': '#95a5a6',
    'infra-waste': '#95a5a6',
    'infra-digital': '#95a5a6',

    // Economic Systems
    'urban-economy': '#f39c12', // Gold
    'economy-local': '#f39c12',
    'economy-markets': '#f39c12',
    'economy-innovation': '#f39c12',
    'economy-circular': '#f39c12',

    // Social Services
    'urban-services': '#9b59b6', // Purple
    'services-health': '#9b59b6',
    'services-education': '#9b59b6',
    'services-culture': '#9b59b6',
    'services-safety': '#9b59b6',

    // Governance
    'urban-governance': '#34495e', // Dark Blue
    'governance-participation': '#34495e',
    'governance-planning': '#34495e',
    'governance-transparency': '#34495e',
    'governance-equity': '#34495e',

    // Environment & Ecology
    'urban-environment': '#16a085', // Teal
    'env-biodiversity': '#16a085',
    'env-climate': '#16a085',
    'env-air-quality': '#16a085',
    'env-green-infra': '#16a085',

    // Food Systems
    'urban-food': '#c0392b', // Red
    'food-production': '#c0392b',
    'food-distribution': '#c0392b',
    'food-access': '#c0392b',
    'food-waste': '#c0392b',

    // Culture & Identity
    'urban-culture': '#e91e63', // Pink
    'culture-heritage': '#e91e63',
    'culture-arts': '#e91e63',
    'culture-diversity': '#e91e63',
    'culture-placemaking': '#e91e63'
};

/**
 * Get the Urban color for a node ID, or null if not an Urban node
 */
export function getUrbanColor(nodeId: string): string | null {
    return URBAN_COLORS[nodeId] || null;
}

/**
 * Check if a node ID is part of the Urban tree structure
 */
export function isUrbanNode(nodeId: string): boolean {
    return nodeId in URBAN_COLORS;
}

/**
 * Populate tree with Urban Contexts - comprehensive urban systems framework
 * Covers key aspects of urban life, infrastructure, and governance
 */
export function populateUrbanTree(rootNode: RootNode): RootNode {
    console.log('[URBAN] Populating tree with Urban Contexts...');

    // Add all 10 urban context categories to root
    addChild(rootNode, 'urban-housing', '🏘️ Housing & Shelter', 6);
    addChild(rootNode, 'urban-mobility', '🚇 Mobility & Transport', 6);
    addChild(rootNode, 'urban-public-space', '🌳 Public Space', 6);
    addChild(rootNode, 'urban-infrastructure', '🏗️ Infrastructure', 6);
    addChild(rootNode, 'urban-economy', '💼 Economic Systems', 6);
    addChild(rootNode, 'urban-services', '🏥 Social Services', 6);
    addChild(rootNode, 'urban-governance', '🏛️ Governance', 5);
    addChild(rootNode, 'urban-environment', '🌿 Environment & Ecology', 6);
    addChild(rootNode, 'urban-food', '🌾 Food Systems', 5);
    addChild(rootNode, 'urban-culture', '🎭 Culture & Identity', 5);

    // Housing & Shelter
    const housing = findNodeById(rootNode, 'urban-housing');
    if (housing) {
        addChild(housing, 'housing-affordable', 'Affordable Housing', 30);
        addChild(housing, 'housing-social', 'Social Housing', 25);
        addChild(housing, 'housing-emergency', 'Emergency Shelter', 25);
        addChild(housing, 'housing-cooperative', 'Housing Cooperatives', 20);
    }

    // Mobility & Transport
    const mobility = findNodeById(rootNode, 'urban-mobility');
    if (mobility) {
        addChild(mobility, 'mobility-public', 'Public Transit', 30);
        addChild(mobility, 'mobility-active', 'Walking & Cycling', 30);
        addChild(mobility, 'mobility-accessibility', 'Accessible Transport', 25);
        addChild(mobility, 'mobility-freight', 'Urban Freight', 15);
    }

    // Public Space
    const publicSpace = findNodeById(rootNode, 'urban-public-space');
    if (publicSpace) {
        addChild(publicSpace, 'space-parks', 'Parks & Gardens', 30);
        addChild(publicSpace, 'space-plazas', 'Plazas & Squares', 25);
        addChild(publicSpace, 'space-streets', 'Streets & Sidewalks', 25);
        addChild(publicSpace, 'space-community', 'Community Spaces', 20);
    }

    // Infrastructure
    const infrastructure = findNodeById(rootNode, 'urban-infrastructure');
    if (infrastructure) {
        addChild(infrastructure, 'infra-water', 'Water & Sanitation', 30);
        addChild(infrastructure, 'infra-energy', 'Energy Systems', 25);
        addChild(infrastructure, 'infra-waste', 'Waste Management', 25);
        addChild(infrastructure, 'infra-digital', 'Digital Infrastructure', 20);
    }

    // Economic Systems
    const economy = findNodeById(rootNode, 'urban-economy');
    if (economy) {
        addChild(economy, 'economy-local', 'Local Business', 30);
        addChild(economy, 'economy-markets', 'Public Markets', 25);
        addChild(economy, 'economy-innovation', 'Innovation Hubs', 25);
        addChild(economy, 'economy-circular', 'Circular Economy', 20);
    }

    // Social Services
    const services = findNodeById(rootNode, 'urban-services');
    if (services) {
        addChild(services, 'services-health', 'Healthcare Access', 30);
        addChild(services, 'services-education', 'Education & Learning', 30);
        addChild(services, 'services-culture', 'Cultural Services', 20);
        addChild(services, 'services-safety', 'Public Safety', 20);
    }

    // Governance
    const governance = findNodeById(rootNode, 'urban-governance');
    if (governance) {
        addChild(governance, 'governance-participation', 'Civic Participation', 30);
        addChild(governance, 'governance-planning', 'Urban Planning', 30);
        addChild(governance, 'governance-transparency', 'Transparency', 20);
        addChild(governance, 'governance-equity', 'Equity & Justice', 20);
    }

    // Environment & Ecology
    const environment = findNodeById(rootNode, 'urban-environment');
    if (environment) {
        addChild(environment, 'env-biodiversity', 'Urban Biodiversity', 25);
        addChild(environment, 'env-climate', 'Climate Resilience', 30);
        addChild(environment, 'env-air-quality', 'Air Quality', 25);
        addChild(environment, 'env-green-infra', 'Green Infrastructure', 20);
    }

    // Food Systems
    const food = findNodeById(rootNode, 'urban-food');
    if (food) {
        addChild(food, 'food-production', 'Urban Agriculture', 25);
        addChild(food, 'food-distribution', 'Food Distribution', 25);
        addChild(food, 'food-access', 'Food Access & Security', 30);
        addChild(food, 'food-waste', 'Food Waste Reduction', 20);
    }

    // Culture & Identity
    const culture = findNodeById(rootNode, 'urban-culture');
    if (culture) {
        addChild(culture, 'culture-heritage', 'Cultural Heritage', 25);
        addChild(culture, 'culture-arts', 'Arts & Expression', 25);
        addChild(culture, 'culture-diversity', 'Cultural Diversity', 25);
        addChild(culture, 'culture-placemaking', 'Placemaking', 25);
    }

    console.log('[URBAN] Tree populated with 10 urban context categories');
    return rootNode;
}

/**
 * Urban Contexts metadata for display and reference
 */
export const URBAN_METADATA = {
    housing: { name: 'Housing & Shelter', icon: '🏘️', color: '#e67e22' },
    mobility: { name: 'Mobility & Transport', icon: '🚇', color: '#3498db' },
    publicSpace: { name: 'Public Space', icon: '🌳', color: '#27ae60' },
    infrastructure: { name: 'Infrastructure', icon: '🏗️', color: '#95a5a6' },
    economy: { name: 'Economic Systems', icon: '💼', color: '#f39c12' },
    services: { name: 'Social Services', icon: '🏥', color: '#9b59b6' },
    governance: { name: 'Governance', icon: '🏛️', color: '#34495e' },
    environment: { name: 'Environment & Ecology', icon: '🌿', color: '#16a085' },
    food: { name: 'Food Systems', icon: '🌾', color: '#c0392b' },
    culture: { name: 'Culture & Identity', icon: '🎭', color: '#e91e63' }
};
