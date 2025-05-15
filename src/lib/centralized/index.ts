// Export centralized module with Gun persistence

// Export from tree.ts
export {
	Tree,
	enterChild,
	exitToParent,
	goToRoot,
	getAllDescendants,
	descendants,
	children,
	followPath,
	getCurrentPath
} from './tree';

// Export from types.ts
export type {
	Node,
	RootNode,
	NonRootNode,
	TreeZipper,
	Ctx,
	NavigationPath,
	ShareMap,
	Forest,
	Capacity,
	CapacityInventory,
	CapacityShare
} from './types';

// Export from calculations.ts
export {
	totalChildPoints,
	weight,
	shareOfParent,
	isContribution,
	hasDirectContributionChild,
	hasNonContributionChild,
	contributionChildrenWeight,
	childrenFulfillment,
	contributionChildrenFulfillment,
	nonContributionChildrenFulfillment,
	getContributorInstances,
	getContributorNode,
	fulfilled,
	desire,
	shareOfGeneralFulfillment,
	sharesOfGeneralFulfillmentMap,
	providerShares,
	mutualFulfillment,
	receiverShareFrom
} from './calculations';

// Export from utils.ts
export { normalizeShareMap } from './utils';
