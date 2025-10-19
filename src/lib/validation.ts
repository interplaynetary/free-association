/**
 * Validation utilities for the Free Association Protocol
 */
import { z } from 'zod';
import {
	RootNodeSchema,
	NodeSchema,
	CapacitySchema,
	CapacitiesCollectionSchema,
	ShareMapSchema,
	RecognitionCacheSchema,
	UserCompositionSchema,
	NetworkCompositionSchema,
	UserSlotCompositionSchema,
	NetworkSlotCompositionSchema,
	ProviderAllocationStateDataSchema,
	NetworkAllocationStatesSchema,
	ContactsCollectionSchema,
	ChatReadStatesSchema,
	CompositionTargetSchema
} from '$lib/schema';

/**
 * Generic parser options for customizing validation behavior
 */
interface ParseOptions<T> {
	/** Schema to validate against */
	schema: z.ZodType<T>;
	/** Default value to return on validation failure */
	defaultValue: T;
	/** Function name for logging context */
	functionName: string;
	/** Whether to enable detailed logging */
	enableLogging?: boolean;
	/** Custom preprocessing function to transform data before validation */
	preprocess?: (data: unknown) => unknown;
}

/**
 * Filter out Gun.js metadata properties from an object
 * @param data Object to filter
 * @returns Filtered object without Gun.js metadata
 */
function filterGunMetadata(data: unknown): unknown {
	if (!data || typeof data !== 'object' || Array.isArray(data)) {
		return data;
	}

	const filteredData: Record<string, any> = {};
	for (const [key, value] of Object.entries(data)) {
		// Filter out Gun.js metadata properties (keys starting with "_")
		if (!key.startsWith('_')) {
			filteredData[key] = value;
		}
	}
	return filteredData;
}

// DELETED: parseTimestampedData - No longer needed since we use Gun's native timestamps
// All data is now stored unwrapped, and Gun tracks timestamps via GUN.state.is()

/**
 * Generic data parser that handles the common validation pattern
 * @param data Raw data to parse and validate
 * @param options Parsing options including schema and defaults
 * @returns Validated data or default value if validation fails
 */
function parseData<T>(data: unknown, options: ParseOptions<T>): T {
	const { schema, defaultValue, functionName, enableLogging = false, preprocess } = options;

	try {
		// 1. Handle string/object input
		let parsedData;
		if (typeof data === 'string') {
			try {
				parsedData = JSON.parse(data);
			} catch (error) {
				if (enableLogging) {
					console.error(`[VALIDATION] Error parsing ${functionName} JSON:`, error);
				}
				return defaultValue;
			}
		} else {
			parsedData = data;
		}

		// 2. Filter Gun.js metadata
		parsedData = filterGunMetadata(parsedData);

		// 3. Apply custom preprocessing if provided
		if (preprocess) {
			parsedData = preprocess(parsedData);
		}

		if (enableLogging) {
			console.log(`[VALIDATION] Pre-validation ${functionName} data:`, parsedData);
		}

		// 4. Apply Zod schema validation
		const result = schema.safeParse(parsedData);

		if (result.success) {
			if (enableLogging) {
				console.log(`[VALIDATION] ${functionName} validation successful`);
			}
			return result.data;
		} else {
			if (enableLogging) {
				console.error(`[VALIDATION] ${functionName} validation failed. Data:`, parsedData);
				console.error(`[VALIDATION] Validation errors:`, result.error.issues);
			}
			return defaultValue;
		}
	} catch (err) {
		if (enableLogging) {
			console.error(`[VALIDATION] Error during ${functionName} validation:`, err);
		}
		return defaultValue;
	}
}

/**
 * Parse and validate a tree from Gun database or other source
 * @param treeData Raw tree data (either string or object)
 * @returns Validated RootNode or null if validation fails
 */
export function parseTree(treeData: unknown) {
	return parseData(treeData, {
		schema: RootNodeSchema,
		defaultValue: null,
		functionName: 'tree'
	});
}

/**
 * Migrates old capacity structure to new slot-based structure
 */
function migrateCapacityToSlotBased(capacity: any): any {
	// If it already has availability_slots, it's already in the new format
	if (capacity.availability_slots) {
		console.log(
			`[MIGRATION] 🚨 DEBUG: Capacity ${capacity.id} already has availability_slots, checking existing slot location data:`,
			{
				slots: capacity.availability_slots.map((slot: any) => ({
					id: slot.id,
					location_type: slot.location_type,
					coordinates: { lat: slot.latitude, lng: slot.longitude },
					address: {
						street: slot.street_address,
						city: slot.city,
						state: slot.state_province,
						postal: slot.postal_code,
						country: slot.country
					}
				}))
			}
		);
		return capacity;
	}

	console.log(`[MIGRATION] 🚨 DEBUG: Starting migration for capacity ${capacity.id}:`, {
		name: capacity.name,
		old_location_type: capacity.location_type,
		old_coordinates: { lat: capacity.latitude, lng: capacity.longitude },
		old_address: {
			street: capacity.street_address,
			city: capacity.city,
			state: capacity.state_province,
			postal: capacity.postal_code,
			country: capacity.country
		},
		has_quantity: typeof capacity.quantity !== 'undefined',
		has_start_date: !!capacity.start_date
	});

	// If it has the old structure properties, migrate them to a slot
	if (typeof capacity.quantity !== 'undefined' || capacity.location_type || capacity.start_date) {
		const slotId = `migrated-slot-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

		// Create a slot from the old capacity properties
		const migratedSlot: any = {
			id: slotId,
			quantity: capacity.quantity || 1
		};

		// Migrate location properties
		if (capacity.location_type) migratedSlot.location_type = capacity.location_type;
		if (capacity.longitude) migratedSlot.longitude = capacity.longitude;
		if (capacity.latitude) migratedSlot.latitude = capacity.latitude;
		if (capacity.street_address) migratedSlot.street_address = capacity.street_address;
		if (capacity.city) migratedSlot.city = capacity.city;
		if (capacity.state_province) migratedSlot.state_province = capacity.state_province;
		if (capacity.postal_code) migratedSlot.postal_code = capacity.postal_code;
		if (capacity.country) migratedSlot.country = capacity.country;

		console.log(`[MIGRATION] 🚨 DEBUG: Created migrated slot for ${capacity.id}:`, {
			slot_id: slotId,
			migrated_location_type: migratedSlot.location_type,
			migrated_coordinates: { lat: migratedSlot.latitude, lng: migratedSlot.longitude },
			migrated_address: {
				street: migratedSlot.street_address,
				city: migratedSlot.city,
				state: migratedSlot.state_province,
				postal: migratedSlot.postal_code,
				country: migratedSlot.country
			}
		});

		// Migrate time properties
		if (capacity.all_day !== undefined) migratedSlot.all_day = capacity.all_day;
		if (capacity.start_date) migratedSlot.start_date = capacity.start_date;
		if (capacity.start_time) migratedSlot.start_time = capacity.start_time;
		if (capacity.end_date) migratedSlot.end_date = capacity.end_date;
		if (capacity.end_time) migratedSlot.end_time = capacity.end_time;
		if (capacity.time_zone) migratedSlot.time_zone = capacity.time_zone;
		if (capacity.recurrence) migratedSlot.recurrence = capacity.recurrence;
		if (capacity.custom_recurrence_repeat_every)
			migratedSlot.custom_recurrence_repeat_every = capacity.custom_recurrence_repeat_every;
		if (capacity.custom_recurrence_repeat_unit)
			migratedSlot.custom_recurrence_repeat_unit = capacity.custom_recurrence_repeat_unit;
		if (capacity.custom_recurrence_end_type)
			migratedSlot.custom_recurrence_end_type = capacity.custom_recurrence_end_type;
		if (capacity.custom_recurrence_end_value)
			migratedSlot.custom_recurrence_end_value = capacity.custom_recurrence_end_value;

		// Create the new capacity structure
		const migratedCapacity: any = {
			id: capacity.id,
			name: capacity.name || '',
			emoji: capacity.emoji || '',
			unit: capacity.unit || '',
			description: capacity.description || '',
			max_natural_div: capacity.max_natural_div,
			max_percentage_div: capacity.max_percentage_div,
			hidden_until_request_accepted: capacity.hidden_until_request_accepted,
			owner_id: capacity.owner_id,
			filter_rule: capacity.filter_rule,
			availability_slots: [migratedSlot],
			// Migrate members field
			members: capacity.members || (capacity.owner_id ? [capacity.owner_id] : [])
		};

		// Handle different capacity types
		if (capacity.recipient_shares) {
			migratedCapacity.recipient_shares = capacity.recipient_shares;
		}
		if (capacity.share_percentage !== undefined) {
			migratedCapacity.share_percentage = capacity.share_percentage;
			migratedCapacity.provider_id = capacity.provider_id;
			// Migrate computed_quantity to computed_quantities
			if (capacity.computed_quantity !== undefined) {
				migratedCapacity.computed_quantities = [
					{
						slot_id: slotId,
						quantity: capacity.computed_quantity
					}
				];
			}
		}

		console.log(`[MIGRATION] 🚨 DEBUG: Final migrated capacity ${capacity.id}:`, {
			name: migratedCapacity.name,
			slots_count: migratedCapacity.availability_slots.length,
			first_slot: {
				id: migratedCapacity.availability_slots[0].id,
				location_type: migratedCapacity.availability_slots[0].location_type,
				coordinates: {
					lat: migratedCapacity.availability_slots[0].latitude,
					lng: migratedCapacity.availability_slots[0].longitude
				},
				address: {
					street: migratedCapacity.availability_slots[0].street_address,
					city: migratedCapacity.availability_slots[0].city,
					state: migratedCapacity.availability_slots[0].state_province,
					postal: migratedCapacity.availability_slots[0].postal_code,
					country: migratedCapacity.availability_slots[0].country
				}
			}
		});

		console.log(`[MIGRATION] Migrated old capacity ${capacity.id} to slot-based structure`);
		return migratedCapacity;
	}

	console.log(
		`[MIGRATION] 🚨 DEBUG: No migration needed for capacity ${capacity.id}, returning as-is`
	);
	// Return as-is if no migration is needed
	return capacity;
}

/**
 * Parse and validate capacities data
 * @param capacitiesData Raw capacities data
 * @returns Validated CapacitiesCollection or empty object if validation fails
 */
export function parseCapacities(capacitiesData: unknown) {
	return parseData(capacitiesData, {
		schema: CapacitiesCollectionSchema,
		defaultValue: {},
		functionName: 'capacities',
		enableLogging: true
	});
}

/**
 * Parse and validate a share map
 * @param shareMapData Raw share map data
 * @returns Validated ShareMap or empty object if validation fails
 */
export function parseShareMap(shareMapData: unknown) {
	return parseData(shareMapData, {
		schema: ShareMapSchema,
		defaultValue: {},
		functionName: 'share map',
		enableLogging: true
	});
}

/**
 * Parse and validate recognition cache data
 * @param cacheData Raw recognition cache data
 * @returns Validated RecognitionCache or empty object if validation fails
 */
export function parseRecognitionCache(cacheData: unknown) {
	return parseData(cacheData, {
		schema: RecognitionCacheSchema,
		defaultValue: {},
		functionName: 'recognition cache'
	});
}

/**
 * Validate a single node
 * @param node Node to validate
 * @returns Boolean indicating if node is valid
 */
export function isValidNode(node: unknown): boolean {
	const result = NodeSchema.safeParse(node);
	return result.success;
}

/**
 * Validate a capacity
 * @param capacity Capacity to validate
 * @returns Boolean indicating if capacity is valid
 */
export function isValidCapacity(capacity: unknown): boolean {
	const result = CapacitySchema.safeParse(capacity);
	return result.success;
}

/**
 * Get validation errors for debugging
 * @param schema Zod schema to use
 * @param data Data to validate
 * @returns Error message or null if valid
 */
export function getValidationErrors(schema: z.ZodType, data: unknown): string | null {
	const result = schema.safeParse(data);
	if (!result.success) {
		return JSON.stringify(result.error, null, 2);
	}
	return null;
}

/**
 * Parse and validate user composition data (desiredComposeFrom/desiredComposeInto)
 * @param compositionData Raw composition data
 * @returns Validated UserComposition or empty object if validation fails
 */
export function parseUserComposition(compositionData: unknown) {
	return parseData(compositionData, {
		schema: UserCompositionSchema,
		defaultValue: {},
		functionName: 'user composition',
		enableLogging: true
	});
}

/**
 * Parse and validate network composition data (from contributors)
 * @param compositionData Raw network composition data
 * @returns Validated NetworkComposition or empty object if validation fails
 */
export function parseNetworkComposition(compositionData: unknown) {
	return parseData(compositionData, {
		schema: NetworkCompositionSchema,
		defaultValue: {},
		functionName: 'network composition',
		enableLogging: true
	});
}

/**
 * Parse and validate user slot composition data
 * @param slotCompositionData Raw user slot composition data
 * @returns Validated UserSlotComposition or empty object if validation fails
 */
export function parseUserSlotComposition(slotCompositionData: unknown) {
	console.log('[COMPOSE] [VALIDATION] Parsing user slot composition data...');

	const result = parseData(slotCompositionData, {
		schema: UserSlotCompositionSchema,
		defaultValue: {},
		functionName: 'user slot composition',
		enableLogging: true
	});

	console.log('[COMPOSE] [VALIDATION] Parsed result (unwrapped):', result);

	// Additional validation for composition targets (post-parsing validation)
	if (result) {
		try {
			validateCompositionTargets(result);
			console.log('[COMPOSE] [VALIDATION] ✅ Composition targets validated successfully');
		} catch (error) {
			console.warn('[VALIDATION] Invalid composition targets in slot composition data:', error);
			console.warn('[COMPOSE] [VALIDATION] ❌ Composition target validation failed:', error);
			// Return empty object instead of failing completely
			return {};
		}
	}

	console.log('[COMPOSE] [VALIDATION] Final validated result:', result);
	return result;
}

/**
 * Validate composition targets in slot composition data
 * @param data Slot composition data to validate
 */
function validateCompositionTargets(data: any): void {
	Object.values(data).forEach((sourceSlots: any) => {
		Object.values(sourceSlots).forEach((targetCompositions: any) => {
			Object.keys(targetCompositions).forEach((targetId) => {
				// Validate each composition target
				const validation = CompositionTargetSchema.safeParse(targetId);
				if (!validation.success) {
					throw new Error(`Invalid composition target: ${targetId}`);
				}
			});
		});
	});
}

/**
 * Parse and validate network slot composition data
 * @param slotCompositionData Raw network slot composition data
 * @returns Validated NetworkSlotComposition or empty object if validation fails
 */
export function parseNetworkSlotComposition(slotCompositionData: unknown) {
	return parseData(slotCompositionData, {
		schema: NetworkSlotCompositionSchema,
		defaultValue: {},
		functionName: 'network slot composition',
		enableLogging: true
	});
}

/**
 * Parse and validate provider allocation state data
 * @param allocationData Raw provider allocation state data
 * @returns Validated ProviderAllocationStateData or empty object if validation fails
 */
export function parseProviderAllocationStateData(allocationData: unknown) {
	return parseData(allocationData, {
		schema: ProviderAllocationStateDataSchema,
		defaultValue: {},
		functionName: 'provider allocation state data',
		enableLogging: true
	});
}

/**
 * Parse and validate network allocation states data
 * @param networkAllocationData Raw network allocation states data
 * @returns Validated NetworkAllocationStates or empty object if validation fails
 */
export function parseNetworkAllocationStates(networkAllocationData: unknown) {
	return parseData(networkAllocationData, {
		schema: NetworkAllocationStatesSchema,
		defaultValue: {},
		functionName: 'network allocation states',
		enableLogging: true
	});
}

/**
 * Parse and validate contacts data
 * @param contactsData Raw contacts data
 * @returns Validated ContactsCollection or empty object if validation fails
 */
export function parseContacts(contactsData: unknown) {
	return parseData(contactsData, {
		schema: ContactsCollectionSchema,
		defaultValue: {},
		functionName: 'contacts',
		enableLogging: true
	});
}

// DELETED: parseCapacityShares - Replaced by efficient provider-centric algorithm
// DELETED: parseCapacitySlotQuantities - Replaced by efficient provider-centric algorithm

/**
 * Parse and validate chat read states data
 * @param readStatesData Raw chat read states data
 * @returns Validated ChatReadStates or empty object if validation fails
 */
export function parseChatReadStates(readStatesData: unknown) {
	return parseData(readStatesData, {
		schema: ChatReadStatesSchema,
		defaultValue: {},
		functionName: 'chat read states',
		enableLogging: true
	});
}

// ===== COMPOSITION TARGET VALIDATION =====

/**
 * Validate if a string is a valid pubkey format (64 hex characters)
 */
export function isValidPubkey(pubkey: string): boolean {
	return /^[0-9a-fA-F]{64}$/.test(pubkey);
}

/**
 * Validate if a string is a valid collective target format
 */
export function isValidCollectiveTarget(target: string): boolean {
	if (!target.startsWith('collective:')) return false;
	const pubkeysStr = target.slice(11);
	const pubkeys = pubkeysStr.split(',');
	return pubkeys.length > 0 && pubkeys.every((pk) => isValidPubkey(pk));
}

/**
 * Parse and analyze a composition target identifier
 */
export function parseCompositionTarget(targetId: string): {
	type: 'capacity' | 'individual' | 'collective';
	recipients: string[];
	originalId: string;
} {
	// Check for collective format first
	if (targetId.startsWith('collective:')) {
		const pubkeysStr = targetId.slice(11); // Remove "collective:" prefix
		const pubkeys = pubkeysStr.split(',').filter((pk) => isValidPubkey(pk));
		return {
			type: 'collective',
			recipients: pubkeys,
			originalId: targetId
		};
	}

	// Check for individual pubkey
	if (isValidPubkey(targetId)) {
		return {
			type: 'individual',
			recipients: [targetId],
			originalId: targetId
		};
	}

	// Default to capacity ID
	return {
		type: 'capacity',
		recipients: [],
		originalId: targetId
	};
}
