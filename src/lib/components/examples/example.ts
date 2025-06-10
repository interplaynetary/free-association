import { addChild } from '$lib/protocol';
import type { RootNode, Node } from '$lib/schema';
import { userTree } from '$lib/state.svelte';
import { get } from 'svelte/store';

// Export the initialization function that populates an existing root node
export function populateWithExampleData(rootNode: RootNode): RootNode {
	// 1. Material Needs & Dependencies
	addChild(rootNode, 'space', 'Space & Environment', 25);
	const space = rootNode.children.find((child: Node) => child.id === 'space')!;

	// Space & Environment children
	addChild(space, 'indoorSpace', 'Indoor/Outdoor Space', 15);
	addChild(space, 'seating', 'Comfortable Seating', 15);
	addChild(space, 'lighting', 'Lighting', 12);
	addChild(space, 'temperature', 'Temperature Control', 12);
	addChild(space, 'bathroom', 'Bathroom Access', 12);
	addChild(space, 'waterAccess', 'Water Access', 12);
	addChild(space, 'cleaning', 'Cleaning Supplies', 11);
	addChild(space, 'waste', 'Trash/Recycling', 11);

	const indoorSpace = space.children.find((child: Node) => child.id === 'indoorSpace')!;

	addChild(indoorSpace, 'hosts', 'Space Providers/Hosts', 14);
	addChild(indoorSpace, 'infrastructure', 'Infra(de)structure & Bottom-secret Spaces', 25);

	const infrastructure = indoorSpace.children.find((child: Node) => child.id === 'infrastructure')!;

	// Main Categories with their percentage weights
	addChild(rootNode, 'subverse', 'Subverse 🌌', 25);
	const subverse = rootNode.children.find((child: Node) => child.id === 'subverse')!;

	addChild(subverse, 'magicalTech', 'Magical Technologies & Systems', 25);
	addChild(subverse, 'substances', 'Transformative Substances', 20);
	addChild(subverse, 'realityHacking', 'Reality Hacking & Manifestation', 20);
	addChild(subverse, 'loreSystem', 'Lore & Knowledge Systems', 10);

	const magicalTech = subverse.children.find((child: Node) => child.id === 'magicalTech')!;
	const substances = subverse.children.find((child: Node) => child.id === 'substances')!;
	const realityHacking = subverse.children.find((child: Node) => child.id === 'realityHacking')!;
	const loreSystem = subverse.children.find((child: Node) => child.id === 'loreSystem')!;

	addChild(rootNode, 'money', '💸', 10);
	addChild(rootNode, 'freeAssociation', 'Free Association', 10);
	addChild(rootNode, 'automation', 'Automation of Useful & Repetitive Tasks', 10);
	addChild(rootNode, 'commons', 'Socialization of Land & Means of Production', 10);
	addChild(rootNode, 'property', 'Maintaining Personal Property Relations', 10);

	const money = rootNode.children.find((child: Node) => child.id === 'money')!;
	const freeAssociation = rootNode.children.find((child: Node) => child.id === 'freeAssociation')!;
	const property = rootNode.children.find((child: Node) => child.id === 'property')!;

	addChild(property, 'securingLaptop', 'Securing my Laptop', 10);
	addChild(property, 'securingBackpack', 'Securing my Backpack', 10);

	addChild(money, 'openCollective', 'Playnet Open Collective', 10);
	addChild(money, 'personalDonations', 'Personal Donations', 10);

	addChild(freeAssociation, 'development', 'Development', 10);
	addChild(freeAssociation, 'communications', 'Communications', 10);

	// Underground Networks contributions - these will have contributors
	addChild(infrastructure, 'secretStairs', 'Secret stairways', 10);
	addChild(infrastructure, 'wellsTunnels', 'Wells & tunnels', 8);
	addChild(infrastructure, 'basements', 'Basements', 7);
	addChild(infrastructure, 'subLakes', 'Subterranean lakes', 6);

	// Hidden Passages contributions
	addChild(infrastructure, 'alleysRoots', 'Alleys & Roots', 9);
	addChild(infrastructure, 'trapDoors', 'Trap-doors', 8);
	addChild(infrastructure, 'warpZones', 'Warp zones', 7);

	// Alternative Venues contributions
	addChild(infrastructure, 'driveIns', 'Liberation Drive-ins', 8);
	addChild(infrastructure, 'trainYards', 'Train-yards', 7);
	addChild(infrastructure, 'fleaMarkets', 'Flea Markets', 6);
	addChild(infrastructure, 'roofs', 'Roofs', 5);
	addChild(infrastructure, 'ufoLanding', '*ufo* Landing Pads', 5);

	// Reality Manipulation contributions
	addChild(magicalTech, 'cloudBusting', 'Cloud Busting', 9);
	addChild(magicalTech, 'cameraAbatement', 'Camera Abatement', 8);
	addChild(magicalTech, 'systemOfframp', 'System (off/out)-ramp Drive-thrus', 7);

	// Mystical Operations contributions
	addChild(magicalTech, 'bioSlimes', 'Bioluminescent Slimes', 8);
	addChild(magicalTech, 'butterflyWings', 'Butterfly-wing-iridescence Materiality', 8);
	addChild(magicalTech, 'eventHorizons', 'Event Horizons & Vanishing Points', 7);

	// Sacred Knowledge contributions
	addChild(magicalTech, 'libraryStacks', 'Library Stacks', 8);
	addChild(magicalTech, 'saunaLore', 'Pagan Sauna Lore', 7);
	addChild(magicalTech, 'ritualSpaces', 'Ritual Spaces', 6);
	addChild(magicalTech, 'candyWisdom', 'Candy Store Wisdom', 6);

	// Magical Materials contributions
	addChild(substances, 'pixieDust', 'Pixie Dust & Silly Powders', 9);
	addChild(substances, 'oozeSlimes', 'Oozes & slimes', 8);
	addChild(substances, 'potionsBalms', 'Potions & Balms', 7);

	// Alchemical Mixtures contributions
	addChild(substances, 'veganWaters', 'Vegan & Non-vegan Waters', 8);
	addChild(substances, 'mistsSprays', 'Mists & Sprays', 7);
	addChild(substances, 'lozengesBonbons', 'Lozenges & Bonbons', 6);

	// Special Effects contributions
	addChild(substances, 'pheromones', 'Pheromonal Inflection Points', 8);
	addChild(substances, 'darkMatter', 'Dark Matter Manipulation', 7);
	addChild(substances, 'globulation', 'Nebulatory Coagular Globulation', 6);

	// Reality Scripts contributions
	addChild(realityHacking, 'trueFakes', 'TrueFakes & FakeUntruths', 9);
	addChild(realityHacking, 'memeDrives', 'MemeDrives & GeneEngines', 8);
	addChild(realityHacking, 'cosmicBabble', 'Cosmic Psychobabble', 7);

	// Dimensional Engineering contributions
	addChild(realityHacking, 'dimPortals', 'Interdimensional portals', 8);
	addChild(realityHacking, 'infinityPools', 'Infinity Pools', 7);
	addChild(realityHacking, 'deprivationTanks', 'Sensory Deprivation Tankage', 6);

	// Mathematical Magic contributions
	addChild(realityHacking, 'girlMath', 'GirlMath', 8);
	addChild(realityHacking, 'moonMath', 'Moonlight Mathematicians', 7);
	addChild(realityHacking, 'angelicNums', 'Angelic Numbers', 6);

	// Narrative Crafting contributions
	addChild(loreSystem, 'comicBooks', 'Comic Books & Stories', 8);
	addChild(loreSystem, 'poemsAndMaps', 'Poems & Maps', 7);

	// Wisdom Keepers contributions
	addChild(loreSystem, 'priestesses', 'Interdimensional Priestesses', 8);
	addChild(loreSystem, 'crimeLords', 'Alien Crime Lords', 7);
	addChild(loreSystem, 'ballerinas', 'Sci-fi Ballerinas', 6);

	console.log('Populated root node with example data:', rootNode);
	return rootNode;
}

// Expose to window for debugging
if (typeof window !== 'undefined') {
	// Expose the original function
	(window as any).populateWithExampleData = populateWithExampleData;

	// Add a wrapper that uses current userTree if available
	(window as any).populateCurrentTreeWithExampleData = () => {
		const currentTree = get(userTree);
		if (!currentTree) {
			console.error('[DEBUG] No userTree available to populate with example data');
			return null;
		}
		console.log('[DEBUG] Populating current userTree with example data');
		const populatedTree = populateWithExampleData(currentTree);
		userTree.set(populatedTree);
		return populatedTree;
	};

	console.log(
		'[DEBUG] populateWithExampleData and populateCurrentTreeWithExampleData functions exposed to window'
	);
}
