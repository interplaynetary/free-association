/**
 * Attribute Recognition System - Public API
 * 
 * Exports all public functions and types for the attribute recognition system.
 */

// Core recognition functions (pure)
export {
	recognizeAttribute,
	updateAttributeInCollection,
	getAttributeFromCollection,
	removeAttributeFromCollection,
	getAllAttributesForEntity,
	getEntitiesWithAttribute,
	subscribeToAttribute,
	unsubscribeFromAttribute,
	getSubscriptionSource,
	getAllSubscriptionsForEntity,
	resolveEntityId,
	addEntityIdMapping,
	removeEntityIdMapping,
	mergeAttributeCollections
} from './attribute-recognition';

// Svelte stores & resolution (reactive)
export {
	myAttributeRecognitions,
	myAttributeSubscriptions,
	myEntityIdMappings,
	resolveAttribute,
	getAttribute,
	createAttributeStore,
	createResolutionStore,
	subscribeToAttributeRecognitions,
	unsubscribeFromAttributeRecognitions,
	enableAutoAttributeSync,
	getSubscribedPubkeys,
	type ResolutionResult
} from './attribute-recognition.svelte';

// Type-specific helpers
export {
	parseMembershipAttribute,
	isMembershipAttribute,
	createMembershipAttribute,
	parseCapacityAttribute,
	isCapacityAttribute,
	parseNeedAttribute,
	isNeedAttribute,
	parseSkillAttribute,
	isSkillAttribute,
	parseLocationAttribute,
	isLocationAttribute,
	detectAttributeType,
	parseAttributeValue,
	validateAttributeValue,
	extractNeedType,
	extractSkillName,
	createCapacityAttributeName,
	createNeedAttributeName,
	createSkillAttributeName
} from './attribute-types';

// Re-export types
export type {
	AttributeValue,
	AttributeRecognitionsCollection,
	AttributeSubscriptions,
	EntityIdMappings
} from '$lib/protocol/schemas';

export type { SkillValue, LocationValue } from './attribute-types';

