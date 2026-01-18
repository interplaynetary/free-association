/**
 * Need Type mappings and utilities
 * 
 * Maps type_id values to human-readable labels with emojis
 */

export interface ResourceType {
	id: string;
	label: string;
	emoji: string;
	description?: string;
}

export const types: ResourceType[] = [
	{
		id: 'general',
		label: 'General',
		emoji: '🚩',
		description: 'General resources and services'
	},
	{
		id: 'food',
		label: 'Food',
		emoji: '🍎',
		description: 'Food, groceries, meals'
	},
	{
		id: 'housing',
		label: 'Housing',
		emoji: '🏠',
		description: 'Shelter, accommodation, housing'
	},
	{
		id: 'healthcare',
		label: 'Healthcare',
		emoji: '🚑',
		description: 'Medical care, health services'
	},
	{
		id: 'education',
		label: 'Education',
		emoji: '🎓',
		description: 'Learning, training, education'
	},
	{
		id: 'transportation',
		label: 'Transportation',
		emoji: '🚌',
		description: 'Travel, commute, transportation'
	},
	{
		id: 'childcare',
		label: 'Childcare',
		emoji: '👶',
		description: 'Childcare, babysitting'
	},
	{
		id: 'money',
		label: 'Money',
		emoji: '💰',
		description: 'Financial resources, currency, funds'
	},
	{
		id: 'other',
		label: 'Other',
		emoji: '📦',
		description: 'Other resources and services'
	}
];

// Create a map for quick lookups
const type_MAP = new Map(types.map(type => [type.id, type]));

/**
 * Get friendly label for a type_id
 * @param resourceTypeId - The type_id (e.g., 'type_food')
 * @returns Friendly label (e.g., 'Food') or the original id if not found
 */
export function getResourceTypeLabel(resourceTypeId: string): string {
	return type_MAP.get(resourceTypeId)?.label || resourceTypeId;
}

/**
 * Get emoji for a type_id
 * @param resourceTypeId - The type_id (e.g., 'type_food')
 * @returns Emoji (e.g., '🍎') or empty string if not found
 */
export function getResourceTypeEmoji(resourceTypeId: string): string {
	return type_MAP.get(resourceTypeId)?.emoji || '';
}

/**
 * Get full ResourceType object for a type_id
 * @param resourceTypeId - The type_id (e.g., 'type_food')
 * @returns ResourceType object or undefined if not found
 */
export function getResourceType(resourceTypeId: string): ResourceType | undefined {
	return type_MAP.get(resourceTypeId);
}

/**
 * Get formatted display name
 * @param resourceTypeId - The type_id (e.g., 'food')
 * @returns Label (e.g., 'Food') or the original id if not found
 */
export function formatResourceType(resourceTypeId: string): string {
	const type = type_MAP.get(resourceTypeId);
	if (!type) return resourceTypeId;
	return type.label;
}

