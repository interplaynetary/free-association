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
	category?: string;
}

export const types: ResourceType[] = [
	// 1. Space
	{ id: 'space_hosting_short', label: 'Short-term hosting', emoji: '🛋️', description: 'Hosting for a few nights', category: 'Space' },
	{ id: 'space_daytime', label: 'Daytime space', emoji: '☀️', description: 'Space available during the day', category: 'Space' },
	{ id: 'space_sleeping', label: 'Sleeping space', emoji: '🛌', description: 'A place to sleep', category: 'Space' },
	{ id: 'space_work', label: 'Work space', emoji: '🖥️', description: 'Desk, studio, or office space', category: 'Space' },
	{ id: 'space_creative', label: 'Creative space', emoji: '🎨', description: 'Studio for music, art, making', category: 'Space' },
	{ id: 'space_event', label: 'Event space', emoji: '🎉', description: 'Gathering or event hosting', category: 'Space' },
	{ id: 'space_storage', label: 'Storage space', emoji: '📦', description: 'Store items securely', category: 'Space' },
	{ id: 'space_outdoor', label: 'Outdoor space', emoji: '🌳', description: 'Yard, land, or open air', category: 'Space' },
	{ id: 'space_garden', label: 'Garden space', emoji: '🌻', description: 'Growing space or garden', category: 'Space' },
	{ id: 'space_parking', label: 'Parking space', emoji: '🅿️', description: 'Vehicle parking', category: 'Space' },
	{ id: 'space_emergency', label: 'Emergency shelter', emoji: '🆘', description: 'Urgent shelter needs', category: 'Space' },

	// 2. Food
	{ id: 'food_home_cooked', label: 'Home-cooked meals', emoji: '🍲', description: 'Prepared meals to share', category: 'Food' },
	{ id: 'food_shared_cooking', label: 'Shared cooking', emoji: '🍳', description: 'Cooking together', category: 'Food' },
	{ id: 'food_hosting', label: 'Meal hosting', emoji: '🍽️', description: 'Hosting a dinner or lunch', category: 'Food' },
	{ id: 'food_groceries', label: 'Ingredients / groceries', emoji: '🥕', description: 'Raw ingredients and supplies', category: 'Food' },
	{ id: 'food_produce', label: 'Produce / harvest', emoji: '🌽', description: 'Fresh garden produce', category: 'Food' },
	{ id: 'food_preserved', label: 'Preserved food', emoji: '🥫', description: 'Canned, dried, or pickled goods', category: 'Food' },
	{ id: 'food_baked', label: 'Baked goods', emoji: '🥖', description: 'Bread, pastries, cookies', category: 'Food' },
	{ id: 'food_leftovers', label: 'Leftovers / surplus', emoji: '🥡', description: 'Extra food to share', category: 'Food' },
	{ id: 'food_rescue', label: 'Food rescue', emoji: '♻️', description: 'Rescued food distribution', category: 'Food' },
	{ id: 'food_foraging', label: 'Foraging', emoji: '🍄', description: 'Wild food gathering', category: 'Food' },
	{ id: 'food_community', label: 'Community meals', emoji: '🥘', description: 'Large scale community dining', category: 'Food' },

	// 3. Skills
	{ id: 'skill_teaching', label: 'Teaching', emoji: '👨‍🏫', description: 'Formal or informal instruction', category: 'Skills' },
	{ id: 'skill_tutoring', label: 'Tutoring', emoji: '📝', description: 'Subject-specific help', category: 'Skills' },
	{ id: 'skill_mentorship', label: 'Mentorship', emoji: '🤝', description: 'Guidance and advice', category: 'Skills' },
	{ id: 'skill_coaching', label: 'Coaching', emoji: '📣', description: 'Performance or life coaching', category: 'Skills' },
	{ id: 'skill_language', label: 'Language exchange', emoji: '🗣️', description: 'Practice and learn languages', category: 'Skills' },
	{ id: 'skill_repair', label: 'Repair / fixing', emoji: '🔧', description: 'Fixing broken items', category: 'Skills' },
	{ id: 'skill_building', label: 'Building / making', emoji: '🔨', description: 'Construction and fabrication', category: 'Skills' },
	{ id: 'skill_creative', label: 'Creative skills', emoji: '🎭', description: 'Artistic and creative expertise', category: 'Skills' },
	{ id: 'skill_technical', label: 'Technical skills', emoji: '💻', description: 'IT, coding, electronics', category: 'Skills' },
	{ id: 'skill_admin', label: 'Administrative help', emoji: '🗂️', description: 'Organization, paperwork, logistics', category: 'Skills' },
	{ id: 'skill_life', label: 'Life skills', emoji: '🧠', description: 'Cooking, budgeting, adulting', category: 'Skills' },
	{ id: 'skill_peer', label: 'Peer learning', emoji: '👥', description: 'Learning together', category: 'Skills' },

	// 4. Tools & Objects
	{ id: 'tool_hand', label: 'Hand tools', emoji: '⚒️', description: 'Manual tools', category: 'Tools & Objects' },
	{ id: 'tool_power', label: 'Power tools', emoji: '🔌', description: 'Electric or battery tools', category: 'Tools & Objects' },
	{ id: 'tool_kitchen', label: 'Kitchen equipment', emoji: '🥣', description: 'Appliances and cookware', category: 'Tools & Objects' },
	{ id: 'tool_electronics', label: 'Electronics', emoji: '📱', description: 'Gadgets and devices', category: 'Tools & Objects' },
	{ id: 'tool_computer', label: 'Computers / accessories', emoji: '⌨️', description: 'Laptops, monitors, peripherals', category: 'Tools & Objects' },
	{ id: 'tool_music', label: 'Musical instruments', emoji: '🎸', description: 'Instruments and audio gear', category: 'Tools & Objects' },
	{ id: 'tool_sports', label: 'Sports equipment', emoji: '⚽', description: 'Gear for sports and activities', category: 'Tools & Objects' },
	{ id: 'tool_camping', label: 'Camping / outdoor gear', emoji: '⛺', description: 'Tents, bags, hiking gear', category: 'Tools & Objects' },
	{ id: 'tool_event', label: 'Event equipment', emoji: '🎪', description: 'Chairs, tables, PA systems', category: 'Tools & Objects' },
	{ id: 'tool_baby', label: 'Baby / child items', emoji: '🧸', description: 'Clothes, toys, gear', category: 'Tools & Objects' },
	{ id: 'tool_medical', label: 'Medical / accessibility', emoji: '🦽', description: 'Aids and medical devices', category: 'Tools & Objects' },
	{ id: 'tool_art', label: 'Art / craft supplies', emoji: '🖍️', description: 'Materials for creation', category: 'Tools & Objects' },

	// 5. Mobility
	{ id: 'mob_rideshare', label: 'Ridesharing', emoji: '🚗', description: 'Sharing a ride', category: 'Mobility' },
	{ id: 'mob_vehicle', label: 'Vehicle borrowing', emoji: '🔑', description: 'Borrowing a car or truck', category: 'Mobility' },
	{ id: 'mob_bike', label: 'Bike sharing', emoji: '🚲', description: 'Bicycles and gear', category: 'Mobility' },
	{ id: 'mob_cargo', label: 'Cargo transport', emoji: '🚛', description: 'Moving large items', category: 'Mobility' },
	{ id: 'mob_moving', label: 'Moving help', emoji: '📦', description: 'Help with relocating', category: 'Mobility' },
	{ id: 'mob_trip', label: 'Trip coordination', emoji: '🗺️', description: 'Planning travel together', category: 'Mobility' },
	{ id: 'mob_travel_buddy', label: 'Accompanied travel', emoji: '👯', description: 'Company during travel', category: 'Mobility' },
	{ id: 'mob_accessibility', label: 'Accessibility transport', emoji: '🚐', description: 'Specialized transport help', category: 'Mobility' },

	// 6. Time & Care
	{ id: 'care_child', label: 'Childcare', emoji: '🍼', description: 'Looking after children', category: 'Time & Care' },
	{ id: 'care_elder', label: 'Elder support', emoji: '👵', description: 'Assisting elders', category: 'Time & Care' },
	{ id: 'care_pet', label: 'Pet care', emoji: '🐕', description: 'Walking, feeding, sitting', category: 'Time & Care' },
	{ id: 'care_plant', label: 'Plant care', emoji: '🪴', description: 'Watering and tending plants', category: 'Time & Care' },
	{ id: 'care_house', label: 'House sitting', emoji: '🏠', description: 'Watching home while away', category: 'Time & Care' },
	{ id: 'care_emotional', label: 'Emotional support', emoji: '❤️', description: 'Being there for someone', category: 'Time & Care' },
	{ id: 'care_listening', label: 'Listening / presence', emoji: '👂', description: 'Active listening', category: 'Time & Care' },
	{ id: 'care_company', label: 'Companionship', emoji: '☕', description: 'Spending time together', category: 'Time & Care' },
	{ id: 'care_checkin', label: 'Check-ins', emoji: '👋', description: 'Regular safety/wellness checks', category: 'Time & Care' },
	{ id: 'care_errand', label: 'Errand help', emoji: '🛍️', description: 'Shopping, post office, etc.', category: 'Time & Care' },

	// 7. Nature & Growing
	{ id: 'grow_help', label: 'Gardening help', emoji: '👩‍🌾', description: 'Weeding, planting, watering', category: 'Nature & Growing' },
	{ id: 'grow_seed', label: 'Seed sharing', emoji: '🌰', description: 'Swapping seeds', category: 'Nature & Growing' },
	{ id: 'grow_compost', label: 'Compost sharing', emoji: '🍂', description: 'Contributing or taking compost', category: 'Nature & Growing' },
	{ id: 'grow_tools', label: 'Garden tools', emoji: '🧤', description: 'Specific gardening equipment', category: 'Nature & Growing' },
	{ id: 'grow_urban', label: 'Urban farming', emoji: '🏙️', description: 'City growing projects', category: 'Nature & Growing' },
	{ id: 'grow_steward', label: 'Land stewardship', emoji: '🏞️', description: 'Caring for the land', category: 'Nature & Growing' },
	{ id: 'grow_tree', label: 'Tree planting', emoji: '🌲', description: 'Planting and caring for trees', category: 'Nature & Growing' },
	{ id: 'grow_env', label: 'Environmental projects', emoji: '🌍', description: 'Restoration and cleanup', category: 'Nature & Growing' },
	{ id: 'grow_harvest', label: 'Harvest sharing', emoji: '🧺', description: 'Sharing the bounty', category: 'Nature & Growing' },

	// 8. Creativity & Culture
	{ id: 'cult_jam', label: 'Music jams', emoji: '🎵', description: 'Playing music together', category: 'Creativity & Culture' },
	{ id: 'cult_art', label: 'Art collaboration', emoji: '🖌️', description: 'Creating art together', category: 'Creativity & Culture' },
	{ id: 'cult_write', label: 'Writing groups', emoji: '✍️', description: 'Writing and feedback', category: 'Creativity & Culture' },
	{ id: 'cult_perform', label: 'Performance practice', emoji: '🩰', description: 'Rehearsal and performance', category: 'Creativity & Culture' },
	{ id: 'cult_craft', label: 'Craft circles', emoji: '🧶', description: 'Knitting, sewing, making', category: 'Creativity & Culture' },
	{ id: 'cult_story', label: 'Storytelling', emoji: '📖', description: 'Sharing stories', category: 'Creativity & Culture' },
	{ id: 'cult_exchange', label: 'Cultural exchange', emoji: '🌏', description: 'Sharing traditions and culture', category: 'Creativity & Culture' },
	{ id: 'cult_skill', label: 'Skill circles', emoji: '⭕', description: 'Group skill sharing', category: 'Creativity & Culture' },
	{ id: 'cult_mentor', label: 'Creative mentorship', emoji: '🧚', description: 'Guiding creative growth', category: 'Creativity & Culture' },

	// 9. Knowledge & Navigation
	{ id: 'know_local', label: 'Local knowledge', emoji: '📍', description: 'Tips on the neighborhood', category: 'Knowledge & Navigation' },
	{ id: 'know_admin', label: 'Bureaucracy navigation', emoji: '🏢', description: 'Help with forms and systems', category: 'Knowledge & Navigation' },
	{ id: 'know_legal', label: 'Legal literacy', emoji: '⚖️', description: 'Understanding rights/laws', category: 'Knowledge & Navigation' },
	{ id: 'know_health', label: 'Health system nav', emoji: '🏥', description: 'Navigating healthcare', category: 'Knowledge & Navigation' },
	{ id: 'know_academic', label: 'Academic help', emoji: '🎓', description: 'Studies and research', category: 'Knowledge & Navigation' },
	{ id: 'know_research', label: 'Research assistance', emoji: '🔎', description: 'Finding information', category: 'Knowledge & Navigation' },
	{ id: 'know_study', label: 'Study groups', emoji: '📚', description: 'Learning together', category: 'Knowledge & Navigation' },
	{ id: 'know_map', label: 'Resource mapping', emoji: '🗺️', description: 'Mapping community assets', category: 'Knowledge & Navigation' },

	// 10. Projects & Mutual Aid
	{ id: 'mut_project', label: 'Community projects', emoji: '🏗️', description: 'Building things together', category: 'Projects & Mutual Aid' },
	{ id: 'mut_coord', label: 'Mutual aid coord', emoji: '🔄', description: 'Organizing support systems', category: 'Projects & Mutual Aid' },
	{ id: 'mut_emerg', label: 'Emergency response', emoji: '🚨', description: 'Acting in crisis', category: 'Projects & Mutual Aid' },
	{ id: 'mut_solve', label: 'Collective solving', emoji: '🧩', description: 'Fixing problems together', category: 'Projects & Mutual Aid' },
	{ id: 'mut_pool', label: 'Skill pooling', emoji: '🏊', description: 'Combining talents', category: 'Projects & Mutual Aid' },
	{ id: 'mut_neighbor', label: 'Neighborhood support', emoji: '🏘️', description: 'Helping neighbors', category: 'Projects & Mutual Aid' },
	{ id: 'mut_popup', label: 'Pop-up initiatives', emoji: '🎪', description: 'Temporary actions', category: 'Projects & Mutual Aid' },
	{ id: 'mut_event', label: 'Event organizing', emoji: '🗓️', description: 'Planning gatherings', category: 'Projects & Mutual Aid' },

	// Legacy / Fallbacks
	{
		id: 'general',
		label: 'General',
		emoji: '🚩',
		description: 'General resources and services',
		category: 'Other'
	},
	{
		id: 'money',
		label: 'Money',
		emoji: '💰',
		description: 'Financial resources, currency, funds',
		category: 'Other'
	},
	{
		id: 'other',
		label: 'Other',
		emoji: '📦',
		description: 'Other resources and services',
		category: 'Other'
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
