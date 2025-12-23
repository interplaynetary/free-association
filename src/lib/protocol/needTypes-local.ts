/**
 * Need Type mappings and utilities
 * 
 * Maps need_type_id values to human-readable labels with emojis
 */

export interface NeedType {
	id: string;
	label: string;
	emoji: string;
	description?: string;
}

export const NEED_TYPES: NeedType[] = [
	{ 
		id: 'general', 
		label: 'General', 
		emoji: '',
		description: 'General resources and services'
	},
	{ 
		id: 'food', 
		label: 'Food', 
		emoji: '',
		description: 'Food, groceries, meals'
	},
	{ 
		id: 'housing', 
		label: 'Housing', 
		emoji: '',
		description: 'Shelter, accommodation, housing'
	},
	{ 
		id: 'healthcare', 
		label: 'Healthcare', 
		emoji: '',
		description: 'Medical care, health services'
	},
	{ 
		id: 'education', 
		label: 'Education', 
		emoji: '',
		description: 'Learning, training, education'
	},
	{ 
		id: 'transportation', 
		label: 'Transportation', 
		emoji: '',
		description: 'Travel, commute, transportation'
	},
	{ 
		id: 'childcare', 
		label: 'Childcare', 
		emoji: '',
		description: 'Childcare, babysitting'
	},
	{ 
		id: 'money', 
		label: 'Money', 
		emoji: '',
		description: 'Financial resources, currency, funds'
	},
	{ 
		id: 'other', 
		label: 'Other', 
		emoji: '',
		description: 'Other resources and services'
	}
];

// Create a map for quick lookups
const NEED_TYPE_MAP = new Map(NEED_TYPES.map(type => [type.id, type]));

/**
 * Get friendly label for a need_type_id
 * @param needTypeId - The need_type_id (e.g., 'need_type_food')
 * @returns Friendly label (e.g., 'Food') or the original id if not found
 */
export function getNeedTypeLabel(needTypeId: string): string {
	return NEED_TYPE_MAP.get(needTypeId)?.label || needTypeId;
}

/**
 * Get emoji for a need_type_id
 * @param needTypeId - The need_type_id (e.g., 'need_type_food')
 * @returns Emoji (e.g., '🍎') or empty string if not found
 */
export function getNeedTypeEmoji(needTypeId: string): string {
	return NEED_TYPE_MAP.get(needTypeId)?.emoji || '';
}

/**
 * Get full NeedType object for a need_type_id
 * @param needTypeId - The need_type_id (e.g., 'need_type_food')
 * @returns NeedType object or undefined if not found
 */
export function getNeedType(needTypeId: string): NeedType | undefined {
	return NEED_TYPE_MAP.get(needTypeId);
}

/**
 * Get formatted display name
 * @param needTypeId - The need_type_id (e.g., 'food')
 * @returns Label (e.g., 'Food') or the original id if not found
 */
export function formatNeedType(needTypeId: string): string {
	const type = NEED_TYPE_MAP.get(needTypeId);
	if (!type) return needTypeId;
	return type.label;
}

