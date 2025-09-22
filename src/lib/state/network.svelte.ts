import { get } from 'svelte/store';
import { gun, user, userPub, usersList } from './gun.svelte';
import {
	userPubKeys,
	userAliasesCache,
	resolveToPublicKey,
	userContacts,
	isLoadingContacts
} from '$lib/state/users.svelte';
import {
	contributors,
	mutualContributors,
	recognitionCache,
	networkCapacities,
	userTree,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities,
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto,
	networkDesiredSlotComposeFrom,
	networkDesiredSlotComposeInto,
	networkAllocationStates,
	updateStoreWithTimestamp,
	updateStoreWithFreshTimestamp,
	userCapacitiesTimestamp,
	userSogfTimestamp,
	userDesiredSlotComposeFromTimestamp,
	userDesiredSlotComposeIntoTimestamp,
	userContactsTimestamp,
	chatReadStatesTimestamp
} from '$lib/state/core.svelte';
import { chatReadStates, isLoadingChatReadStates, setChatReadStates } from '$lib/state/chat.svelte';
import { collectiveMembers, collectiveForest } from '$lib/collective.svelte';
import type {
	NetworkSlotComposition,
	CapacitiesCollection,
	ContactsCollection,
	UserSlotComposition,
	ChatReadStates,
	NetworkAllocationStates
} from '$lib/schema';
import { recalculateFromTree } from './calculations.svelte';
import {
	parseCapacities,
	parseTree,
	parseUserSlotComposition,
	parseShareMap,
	parseContacts,
	parseChatReadStates,
	parseProviderAllocationStateData
} from '$lib/validation';
import { debounce } from '$lib/utils/debounce';
import { derived } from 'svelte/store';
import { GunSubscriptionStream, StreamSubscriptionManager } from './streams';

// Create stream managers
const sogfStreamManager = new StreamSubscriptionManager('SOGF');
const mutualStreamManager = new StreamSubscriptionManager('MUTUAL');
const ownDataStreamManager = new StreamSubscriptionManager('OWN_DATA');
const chatStreamManager = new StreamSubscriptionManager('CHAT');

// Note: Tree subscriptions are now handled via mutualContributorStreamConfigs
// No separate collective stream system needed

/**
 * Higher-order function that wraps stream creation with authentication
 */
function withAuthentication<T extends any[]>(
	fn: (userId: string, ...args: T) => Promise<void>
): (...args: T) => Promise<void> {
	return async (...args: T) => {
		let ourId: string;
		try {
			ourId = get(userPub);
			if (!ourId) {
				//console.log('[NETWORK] Cannot create stream - not authenticated');
				return;
			}
		} catch (error) {
			//console.log('[NETWORK] Cannot create stream - userPub not initialized');
			return;
		}

		return fn(ourId, ...args);
	};
}

/**
 * Generic data processor that handles validation, comparison, and store updates with timestamp-based freshness detection
 */
function createDataProcessor<T>(config: {
	dataType: string;
	validator?: (data: any) => T | null;
	getCurrentData: () => T | null | unknown; // Allow unknown to handle validator default types
	updateStore: (data: T) => void;
	loadingFlag?: { set: (value: boolean) => void };
	onUpdate?: () => void;
	emptyValue?: T;
	enableTimestampComparison?: boolean;
}) {
	return (rawData: any) => {
		const {
			dataType,
			validator,
			getCurrentData,
			updateStore,
			loadingFlag,
			onUpdate,
			emptyValue,
			enableTimestampComparison
		} = config;

		if (!rawData) {
			//console.log(`[NETWORK] No ${dataType} data found`);
			// Don't update store with empty values for timestamped data
			// Let the validators handle empty cases with their defaultValue
			loadingFlag?.set(false);
			return;
		}

		//console.log(`[NETWORK] Received ${dataType} update from stream`);
		loadingFlag?.set(true);

		try {
			let processedData = rawData;

			// Apply validator (which handles both parsing and validation)
			if (validator) {
				processedData = validator(rawData);
				if (!processedData) {
					console.error(`[NETWORK] Failed to validate ${dataType} data`);
					loadingFlag?.set(false);
					return;
				}

				// Validators now return proper empty timestamped objects on failure
				// No need to check for null since we always get valid schema-compliant data
			}

			// Get current data for comparison
			const currentData = getCurrentData();

			// SMART TIMESTAMP-BASED FRESHNESS CHECK (for timestamped collections)
			if (enableTimestampComparison && processedData && currentData) {
				try {
					// Type-safe timestamp extraction from new metadata structure
					const hasIncomingMetadata =
						processedData &&
						typeof processedData === 'object' &&
						'metadata' in processedData &&
						processedData.metadata &&
						typeof processedData.metadata === 'object' &&
						'updated_at' in processedData.metadata;

					const hasCurrentMetadata =
						currentData &&
						typeof currentData === 'object' &&
						'metadata' in currentData &&
						currentData.metadata &&
						typeof currentData.metadata === 'object' &&
						'updated_at' in currentData.metadata;

					if (hasIncomingMetadata && hasCurrentMetadata) {
						const incomingTimestamp = (processedData as { metadata: { updated_at: string } })
							.metadata.updated_at;
						const currentTimestamp = (currentData as { metadata: { updated_at: string } }).metadata
							.updated_at;

						const incomingTime = new Date(incomingTimestamp).getTime();
						const currentTime = new Date(currentTimestamp).getTime();

						// SMART TIMESTAMPING: Only apply timestamp validation if incoming timestamp is reliable
						const isReliableTimestamp =
							incomingTime > new Date('1970-01-02T00:00:00.000Z').getTime();

						if (isReliableTimestamp && incomingTime <= currentTime) {
							console.log(
								`[NETWORK] Incoming ${dataType} is older/same (${incomingTimestamp}) than current (${currentTimestamp}), ignoring update`
							);
							loadingFlag?.set(false);
							return;
						}

						if (isReliableTimestamp) {
							console.log(
								`[NETWORK] Incoming ${dataType} is newer (${incomingTimestamp}) than current (${currentTimestamp}), accepting update`
							);
						} else {
							console.log(
								`[NETWORK] Incoming ${dataType} has unreliable timestamp (${incomingTimestamp}), falling back to value comparison`
							);
						}
					}
				} catch (timestampError) {
					console.warn(`[NETWORK] Error comparing timestamps for ${dataType}:`, timestampError);
					// Fall through to regular comparison if timestamp comparison fails
				}
			}

			// Fallback: JSON-based change detection
			if (currentData && JSON.stringify(currentData) === JSON.stringify(processedData)) {
				//console.log(`[NETWORK] Incoming ${dataType} matches current ${dataType}, ignoring update`);
				loadingFlag?.set(false);
				return;
			}

			//console.log(`[NETWORK] ${dataType} data changed, updating local store`);
			// Final type safety check before updating store
			if (processedData && typeof processedData === 'object') {
				// At this point, processedData has been validated by the validator and passed timestamp checks
				// The type assertion is safe because the validator guarantees the correct type
				updateStore(processedData as T);
				onUpdate?.();
			} else {
				console.warn(`[NETWORK] Invalid processed data for ${dataType}, skipping store update`);
			}
		} catch (error) {
			console.error(`[NETWORK] Error processing ${dataType}:`, error);
		} finally {
			loadingFlag?.set(false);
		}
	};
}

/**
 * Stream configuration interface
 */
interface StreamConfig<T> {
	type: string;
	streamManager: StreamSubscriptionManager;
	getGunPath: (userId: string, contributorId?: string) => any;
	processor: (data: any) => void;
	errorHandler: (error: any) => void;
}

/**
 * Generic stream factory function
 */
async function createStream<T>(
	config: StreamConfig<T>,
	userId: string,
	contributorId?: string
): Promise<void> {
	const { type, streamManager, getGunPath, processor, errorHandler } = config;
	const targetId = contributorId || userId;

	//console.log(`[NETWORK] Creating ${type} stream for: ${targetId}`);

	await streamManager.createStream(
		targetId,
		() => getGunPath(userId, contributorId),
		type,
		processor,
		errorHandler
	);
}

/**
 * Stream configurations for own data
 */
const ownDataStreamConfigs = {
	tree: {
		type: 'tree',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('tree'),
		processor: createDataProcessor({
			dataType: 'tree',
			validator: parseTree,
			getCurrentData: () => get(userTree),
			updateStore: (data) => userTree.set(data),
			loadingFlag: isLoadingTree,
			onUpdate: () => recalculateFromTree()
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own tree stream:', error);
			isLoadingTree.set(false);
		}
	},
	capacities: {
		type: 'capacities',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('capacities'),
		processor: createDataProcessor({
			dataType: 'capacities',
			enableTimestampComparison: true,
			validator: (rawData) => {
				console.log('[NETWORK] 🚨 DEBUG: Raw capacities data received from Gun:', rawData);

				// Check if we have any location data in the raw data
				if (rawData && typeof rawData === 'object') {
					let parsedData;
					try {
						parsedData = typeof rawData === 'string' ? JSON.parse(rawData) : rawData;
					} catch (e) {
						console.log('[NETWORK] 🚨 DEBUG: Failed to parse raw data');
						return parseCapacities(rawData);
					}

					console.log('[NETWORK] 🚨 DEBUG: Parsed capacities data before validation:');
					Object.entries(parsedData || {}).forEach(([capacityId, capacity]: [string, any]) => {
						if (capacity && capacity.availability_slots) {
							capacity.availability_slots.forEach((slot: any, slotIndex: number) => {
								const hasLocationData =
									slot.location_type === 'Specific' ||
									slot.latitude !== undefined ||
									slot.longitude !== undefined ||
									slot.street_address ||
									slot.city ||
									slot.state_province ||
									slot.postal_code ||
									slot.country;

								if (hasLocationData) {
									console.log(
										`[NETWORK] 🚨 DEBUG: RAW - Capacity ${capacityId} slot ${slotIndex} HAS location data:`,
										{
											slot_id: slot.id,
											location_type: slot.location_type,
											coordinates: { lat: slot.latitude, lng: slot.longitude },
											address: {
												street: slot.street_address,
												city: slot.city,
												state: slot.state_province,
												postal: slot.postal_code,
												country: slot.country
											}
										}
									);
								}
							});
						}
					});
				}

				const validatedData = parseCapacities(rawData);

				// Check what happened after validation
				console.log('[NETWORK] 🚨 DEBUG: Post-validation capacities data:');
				Object.entries(validatedData || {}).forEach(([capacityId, capacity]: [string, any]) => {
					if (capacity && capacity.availability_slots) {
						capacity.availability_slots.forEach((slot: any, slotIndex: number) => {
							const hasLocationData =
								slot.location_type === 'Specific' ||
								slot.latitude !== undefined ||
								slot.longitude !== undefined ||
								slot.street_address ||
								slot.city ||
								slot.state_province ||
								slot.postal_code ||
								slot.country;

							if (hasLocationData) {
								console.log(
									`[NETWORK] 🚨 DEBUG: VALIDATED - Capacity ${capacityId} slot ${slotIndex} HAS location data:`,
									{
										slot_id: slot.id,
										location_type: slot.location_type,
										coordinates: { lat: slot.latitude, lng: slot.longitude },
										address: {
											street: slot.street_address,
											city: slot.city,
											state: slot.state_province,
											postal: slot.postal_code,
											country: slot.country
										}
									}
								);
							} else {
								console.log(
									`[NETWORK] 🚨 DEBUG: VALIDATED - Capacity ${capacityId} slot ${slotIndex} has NO location data`
								);
							}
						});
					}
				});

				return validatedData;
			},
			getCurrentData: () => {
				// Create timestamped structure for comparison
				const flatData = get(userCapacities);
				const timestamp = get(userCapacitiesTimestamp);
				if (!flatData || !timestamp) return null;
				return {
					metadata: {
						created_at: timestamp,
						updated_at: timestamp
					},
					data: flatData
				};
			},
			updateStore: (timestampedData) => {
				// Use helper to update both flat data and timestamp
				updateStoreWithTimestamp(userCapacities, userCapacitiesTimestamp, timestampedData);
			},
			loadingFlag: isLoadingCapacities
		}),
		errorHandler: (error: any) => {
			//console.error('[NETWORK] Error in own capacities stream:', error);
			isLoadingCapacities.set(false);
		}
	},
	contacts: {
		type: 'contacts',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('contacts'),
		processor: createDataProcessor({
			dataType: 'contacts',
			enableTimestampComparison: true,
			validator: parseContacts,
			getCurrentData: () => {
				// Create timestamped structure for comparison
				const flatData = get(userContacts);
				const timestamp = get(userContactsTimestamp);
				if (!flatData || !timestamp) return null;
				return {
					metadata: {
						created_at: timestamp,
						updated_at: timestamp
					},
					data: flatData
				};
			},
			updateStore: (timestampedData) => {
				// Use helper to update both flat data and timestamp
				updateStoreWithTimestamp(userContacts, userContactsTimestamp, timestampedData);
			},
			loadingFlag: isLoadingContacts
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own contacts stream:', error);
			isLoadingContacts.set(false);
		}
	},
	desiredComposeFrom: {
		type: 'desiredSlotComposeFrom',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('desiredSlotComposeFrom'),
		processor: (rawData: any) => {
			console.log('[COMPOSE] [NETWORK-OWN] Loading own compose-from data from Gun...');
			console.log('[COMPOSE] [NETWORK-OWN] Raw own compose-from data:', rawData);

			const dataProcessor = createDataProcessor({
				dataType: 'desiredSlotComposeFrom',
				enableTimestampComparison: true,
				validator: parseUserSlotComposition,
				getCurrentData: () => {
					const flatData = get(userDesiredSlotComposeFrom);
					const timestamp = get(userDesiredSlotComposeFromTimestamp);
					if (!flatData || !timestamp) return null;
					return {
						metadata: {
							created_at: timestamp,
							updated_at: timestamp
						},
						data: flatData
					};
				},
				updateStore: (timestampedData) => {
					console.log(
						'[COMPOSE] [NETWORK-OWN] ✅ Updating own compose-from store:',
						timestampedData
					);
					updateStoreWithTimestamp(
						userDesiredSlotComposeFrom,
						userDesiredSlotComposeFromTimestamp,
						timestampedData
					);
				}
			});

			return dataProcessor(rawData);
		},
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own desiredSlotComposeFrom stream:', error);
			console.error('[COMPOSE] [NETWORK-OWN] ❌ Error loading own compose-from:', error);
		}
	},
	desiredComposeInto: {
		type: 'desiredSlotComposeInto',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('desiredSlotComposeInto'),
		processor: (rawData: any) => {
			console.log('[COMPOSE] [NETWORK-OWN] Loading own compose-into data from Gun...');
			console.log('[COMPOSE] [NETWORK-OWN] Raw own compose-into data:', rawData);

			const dataProcessor = createDataProcessor({
				dataType: 'desiredSlotComposeInto',
				enableTimestampComparison: true,
				validator: parseUserSlotComposition,
				getCurrentData: () => {
					const flatData = get(userDesiredSlotComposeInto);
					const timestamp = get(userDesiredSlotComposeIntoTimestamp);
					if (!flatData || !timestamp) return null;
					return {
						metadata: {
							created_at: timestamp,
							updated_at: timestamp
						},
						data: flatData
					};
				},
				updateStore: (timestampedData) => {
					console.log(
						'[COMPOSE] [NETWORK-OWN] ✅ Updating own compose-into store:',
						timestampedData
					);
					updateStoreWithTimestamp(
						userDesiredSlotComposeInto,
						userDesiredSlotComposeIntoTimestamp,
						timestampedData
					);
				}
			});

			return dataProcessor(rawData);
		},
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own desiredSlotComposeInto stream:', error);
			console.error('[COMPOSE] [NETWORK-OWN] ❌ Error loading own compose-into:', error);
		}
	},
	// DELETED: desiredSlotClaims stream - Replaced by unified compose-from model
	// Slot claims are now handled as compose-from-self in the composition streams
	chatReadStates: {
		type: 'chatReadStates',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('chatReadStates'),
		processor: createDataProcessor({
			dataType: 'chatReadStates',
			enableTimestampComparison: true,
			validator: parseChatReadStates,
			getCurrentData: () => {
				// Create timestamped structure for comparison
				const flatData = get(chatReadStates);
				const timestamp = get(chatReadStatesTimestamp);
				if (!flatData || !timestamp) return null;
				return {
					metadata: {
						created_at: timestamp,
						updated_at: timestamp
					},
					data: flatData
				};
			},
			updateStore: (timestampedData) => {
				// Use helper to update both flat data and timestamp
				updateStoreWithTimestamp(chatReadStates, chatReadStatesTimestamp, timestampedData);
			},
			loadingFlag: isLoadingChatReadStates
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own chatReadStates stream:', error);
			isLoadingChatReadStates.set(false);
		}
	}
};

/**
 * Stream configurations for chat data
 */
const chatStreamConfigs = {
	messages: {
		type: 'messages',
		streamManager: chatStreamManager,
		getGunPath: (userId: string, chatId: string) => gun.get(chatId).map(),
		processor: (chatId: string) => (data: any) => {
			if (!data) return;

			// Handle Gun's map data structure
			const handleMessage = async (messageData: any, key: string) => {
				try {
					const SEA = await import('gun/sea');
					const encryptionKey = '#foo';

					// Properly await all Gun chain resolutions
					const who = await gun.user(messageData).get('alias');
					const whopub = await gun.user(messageData).get('pub');

					const what = (await SEA.default.decrypt(messageData.what, encryptionKey)) + '';
					// @ts-ignore - GUN.state.is typing issue
					const when = GUN.state.is(messageData, 'what');

					const message = {
						who: typeof who === 'string' ? who : 'Anonymous',
						what,
						when,
						whopub: typeof whopub === 'string' ? whopub : ''
					};

					if (message.what && message.when) {
						// Import chat functions dynamically to avoid circular imports
						const { getChatMessages } = await import('$lib/state/chat.svelte');
						const chatStore = getChatMessages(chatId);
						const currentMessages = get(chatStore);

						// Check if message already exists to prevent duplicates
						const messageExists = currentMessages.some(
							(m: any) => m.when === message.when && m.what === message.what
						);

						if (!messageExists) {
							// Add message and keep sorted
							const updatedMessages = [...currentMessages, message].sort((a, b) => a.when - b.when);
							chatStore.set(updatedMessages);
							//console.log(`[NETWORK-CHAT] New message in ${chatId}:`, message.what);
						}
					}
				} catch (error) {
					console.error(`[NETWORK-CHAT] Error processing message in ${chatId}:`, error);
				}
			};

			// Gun's map() provides individual items, so we handle each one
			void handleMessage(data, '');
		},
		errorHandler: (chatId: string) => (error: any) => {
			console.error(`[NETWORK-CHAT] Error in messages stream for ${chatId}:`, error);
		}
	}
};

/**
 * Stream configurations for contributor data
 */
const contributorStreamConfigs = {
	sogf: {
		type: 'sogf',
		streamManager: sogfStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) {
				/*console.warn(
					`[NETWORK] Cannot create SOGF stream for ${contributorId}: no public key available`
				);*/
				return null;
			}
			return gun.user(pubKey).get('sogf');
		},
		processor: (contributorId: string) => (sogfData: any) => {
			if (!sogfData) return;

			console.log(`[NETWORK] Received SOGF update from stream for ${contributorId}`);

			// Validate SOGF data using parseShareMap
			const validatedSogfData = parseShareMap(sogfData);
			if (
				!validatedSogfData ||
				Object.keys(validatedSogfData.data || validatedSogfData).length === 0
			) {
				console.warn(`[NETWORK] Invalid SOGF data from ${contributorId}`);
				return;
			}

			let ourId: string;
			try {
				ourId = get(userPub);
				if (!ourId) return;
			} catch (error) {
				console.log(`[NETWORK] Cannot get userPub in SOGF stream for ${contributorId}`);
				return;
			}

			// Extract the share - handle both flat and timestamped formats
			const shareData = validatedSogfData.data || validatedSogfData;
			const theirShare = shareData[ourId] || 0;

			// Get current recognition cache entry
			const currentCache = get(recognitionCache);
			const existingEntry = currentCache[contributorId];

			// SMART TIMESTAMPING: Apply timestamp validation intelligently
			const shouldUpdate = shouldAcceptSOGFUpdate(
				existingEntry,
				theirShare,
				validatedSogfData.metadata?.updated_at
			);

			if (shouldUpdate) {
				console.log(
					`[NETWORK] Accepting SOGF update from ${contributorId}: ${theirShare.toFixed(4)}`
				);
				updateTheirShareFromNetwork(contributorId, theirShare);
			} else {
				console.log(
					`[NETWORK] Rejecting SOGF update from ${contributorId} (timestamp/value check failed)`
				);
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(`[NETWORK] Error in SOGF stream for ${contributorId}:`, error);
		}
	}
};

/**
 * Stream configurations for mutual contributor data
 */
const mutualContributorStreamConfigs = {
	capacities: {
		type: 'capacities',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('capacities');
		},
		processor: (contributorId: string) =>
			createDataProcessor({
				dataType: 'capacities',
				enableTimestampComparison: true,
				validator: parseCapacities,
				getCurrentData: () => get(networkCapacities)[contributorId] || null,
				updateStore: (data) => {
					if (data) {
						// Extract flat data from timestamped structure for network store
						const flatData = data.data || {};
						networkCapacities.update((current) => ({
							...current,
							[contributorId]: flatData
						}));
					} else {
						networkCapacities.update((current) => {
							const { [contributorId]: _, ...rest } = current;
							return rest;
						});
					}
				}
			}),
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(`[NETWORK] Error in capacities stream for ${contributorId}:`, error);
		}
	},
	// DELETED: capacityShares stream - Replaced by efficient provider-centric algorithm
	// DELETED: capacitySlotQuantities stream - Replaced by efficient provider-centric algorithm
	// DELETED: desiredSlotClaims stream - Replaced by unified compose-from model
	// Slot claims are now handled as compose-from-self in the composition streams
	allocationStates: {
		type: 'allocationStates',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('allocationStates');
		},
		processor: (contributorId: string) => (allocationData: any) => {
			if (!allocationData) {
				//console.log(`[NETWORK] No allocation states from provider ${contributorId}`);
				networkAllocationStates.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(`[NETWORK] Received allocation states update from provider ${contributorId}`);

			// Validate the allocation states data using proper schema validation
			const validatedAllocationStates = parseProviderAllocationStateData(allocationData);

			if (!validatedAllocationStates) {
				console.warn(`[NETWORK] Invalid allocation states data from provider ${contributorId}`);
				return;
			}

			const currentNetworkAllocations = get(networkAllocationStates)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedAllocationStates) === JSON.stringify(currentNetworkAllocations);

			if (!isUnchanged) {
				console.log(
					`[NETWORK] Received new allocation states from provider ${contributorId}:`,
					Object.keys(validatedAllocationStates || {}).length,
					'capacities'
				);

				// Store the validated allocation states for this provider
				networkAllocationStates.update((current) => ({
					...current,
					[contributorId]: (validatedAllocationStates as any) || {}
				}));
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(`[NETWORK] Error in allocation states stream for ${contributorId}:`, error);
		}
	},
	desiredSlotComposeFrom: {
		type: 'desiredSlotComposeFrom',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('desiredSlotComposeFrom');
		},
		processor: (contributorId: string) => (composeFromData: any) => {
			console.log(`[COMPOSE] [NETWORK] Processing compose-from data from ${contributorId}...`);
			console.log(`[COMPOSE] [NETWORK] Raw compose-from data:`, composeFromData);

			if (!composeFromData) {
				console.log(
					`[COMPOSE] [NETWORK] No desired slot compose-from from contributor ${contributorId}`
				);
				networkDesiredSlotComposeFrom.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(
				`[COMPOSE] [NETWORK] Received desired slot compose-from update from stream for ${contributorId}`
			);

			const validatedComposeFrom = parseUserSlotComposition(composeFromData);
			console.log(`[COMPOSE] [NETWORK] Validated compose-from:`, validatedComposeFrom);

			const currentNetworkComposeFrom = get(networkDesiredSlotComposeFrom)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeFrom) === JSON.stringify(currentNetworkComposeFrom);

			if (!isUnchanged) {
				// Extract flat data from timestamped structure for network store
				const flatComposeFrom = validatedComposeFrom.data || {};
				console.log(
					`[COMPOSE] [NETWORK] ✅ Updating network compose-from for ${contributorId}:`,
					flatComposeFrom
				);
				networkDesiredSlotComposeFrom.update((current) => ({
					...current,
					[contributorId]: flatComposeFrom
				}));
			} else {
				console.log(`[COMPOSE] [NETWORK] No changes in compose-from data for ${contributorId}`);
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(
				`[NETWORK] Error in desired slot compose-from stream for ${contributorId}:`,
				error
			);
		}
	},
	desiredSlotComposeInto: {
		type: 'desiredSlotComposeInto',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('desiredSlotComposeInto');
		},
		processor: (contributorId: string) => (composeIntoData: any) => {
			console.log(`[COMPOSE] [NETWORK] Processing compose-into data from ${contributorId}...`);
			console.log(`[COMPOSE] [NETWORK] Raw compose-into data:`, composeIntoData);

			if (!composeIntoData) {
				console.log(
					`[COMPOSE] [NETWORK] No desired slot compose-into from contributor ${contributorId}`
				);
				networkDesiredSlotComposeInto.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(
				`[COMPOSE] [NETWORK] Received desired slot compose-into update from stream for ${contributorId}`
			);

			const validatedComposeInto = parseUserSlotComposition(composeIntoData);
			console.log(`[COMPOSE] [NETWORK] Validated compose-into:`, validatedComposeInto);

			const currentNetworkComposeInto = get(networkDesiredSlotComposeInto)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeInto) === JSON.stringify(currentNetworkComposeInto);

			if (!isUnchanged) {
				// Extract flat data from timestamped structure for network store
				const flatComposeInto = validatedComposeInto.data || {};
				console.log(
					`[COMPOSE] [NETWORK] ✅ Updating network compose-into for ${contributorId}:`,
					flatComposeInto
				);
				networkDesiredSlotComposeInto.update((current) => ({
					...current,
					[contributorId]: flatComposeInto
				}));
			} else {
				console.log(`[COMPOSE] [NETWORK] No changes in compose-into data for ${contributorId}`);
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(
				`[NETWORK] Error in desired slot compose-into stream for ${contributorId}:`,
				error
			);
		}
	},
	tree: {
		type: 'tree',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) {
				console.log(
					`[NETWORK] Could not resolve ${contributorId} for tree stream, assuming it's already a public key`
				);
				return gun.user(contributorId).get('tree');
			}
			console.log(
				`[NETWORK] Creating tree stream for mutual contributor ${contributorId} with public key ${pubKey.substring(0, 20)}...`
			);
			return gun.user(pubKey).get('tree');
		},
		processor: (contributorId: string) => (treeData: any) => {
			if (!treeData) {
				console.log(`[NETWORK] No tree data received for mutual contributor ${contributorId}`);
				// Remove from collective forest
				collectiveForest.update((forest) => {
					const newForest = new Map(forest);
					newForest.delete(contributorId);
					console.log(
						`[NETWORK] Removed tree for contributor ${contributorId}. Forest now has ${newForest.size} trees`
					);
					return newForest;
				});
				return;
			}

			console.log(`[NETWORK] Received tree data for mutual contributor ${contributorId}`);

			// Validate the tree data
			const validatedTree = parseTree(treeData);
			if (!validatedTree) {
				console.warn(`[NETWORK] Invalid tree data from mutual contributor ${contributorId}`);
				return;
			}

			// Update the collective forest
			collectiveForest.update((forest) => {
				const newForest = new Map(forest);
				newForest.set(contributorId, validatedTree);
				console.log(
					`[NETWORK] Updated forest with tree for mutual contributor ${contributorId}. Forest now has ${newForest.size} trees`
				);
				return newForest;
			});
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(
				`[NETWORK] Error in tree stream for mutual contributor ${contributorId}:`,
				error
			);
		}
	}
};

/**
 * Elegant stream creation functions using configurations
 */
const createOwnTreeStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.tree, userId);
});

const createOwnCapacitiesStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.capacities, userId);
});

const createOwnDesiredSlotComposeFromStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.desiredComposeFrom, userId);
});

const createOwnDesiredSlotComposeIntoStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.desiredComposeInto, userId);
});

// DELETED: createOwnDesiredSlotClaimsStream - Replaced by unified compose-from model

const createOwnContactsStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.contacts, userId);
});

const createOwnChatReadStatesStream = withAuthentication(async (userId: string) => {
	await createStream(ownDataStreamConfigs.chatReadStates, userId);
});

const createChatMessagesStream = async (chatId: string) => {
	const config = chatStreamConfigs.messages;
	await config.streamManager.createStream(
		chatId,
		() => config.getGunPath('', chatId),
		config.type,
		config.processor(chatId),
		config.errorHandler(chatId)
	);
};

const createContributorSOGFStream = async (contributorId: string) => {
	console.log(`[STREAM-DEBUG] Creating SOGF stream for contributor: ${contributorId}`);

	const config = contributorStreamConfigs.sogf;
	const pubKey = resolveToPublicKey(contributorId);
	if (!pubKey) {
		/*console.warn(
			`[NETWORK] Cannot create SOGF stream for ${contributorId}: no public key available`
		);*/
		return;
	}

	console.log(`[STREAM-DEBUG] Resolved ${contributorId} to ${pubKey} for SOGF stream`);

	// Use resolved public key as stream key to prevent duplicates
	await config.streamManager.createStream(
		pubKey,
		() => config.getGunPath('', contributorId),
		config.type,
		config.processor(pubKey),
		config.errorHandler(pubKey)
	);

	//console.log(`[STREAM-DEBUG] Created SOGF stream with key: ${pubKey}`);
};

const createMutualContributorStreams = withAuthentication(
	async (userId: string, contributorId: string) => {
		console.log(`[NETWORK] Creating all mutual contributor streams for: ${contributorId}`);

		const pubKey = resolveToPublicKey(contributorId);
		if (!pubKey) {
			/*console.warn(
				`[NETWORK] Cannot create mutual contributor streams for ${contributorId}: no public key available`
			);*/
			return;
		}

		// Create all mutual contributor streams
		const streamTypes = [
			'capacities',
			'allocationStates',
			'desiredSlotComposeFrom',
			'desiredSlotComposeInto',
			'tree'
		] as const;

		for (const streamType of streamTypes) {
			const config = mutualContributorStreamConfigs[streamType];
			const gunPath = config.getGunPath(userId, contributorId);

			if (gunPath) {
				// Use resolved public key as stream key to prevent duplicates
				await config.streamManager.createStream(
					pubKey,
					() => gunPath,
					streamType,
					config.processor(pubKey),
					config.errorHandler(pubKey)
				);
			}
		}
	}
);

/**
 * Initialize all user data streams
 */
export async function initializeUserDataStreams(): Promise<void> {
	try {
		if (!userPub || !get(userPub)) {
			console.log('[NETWORK] Cannot initialize streams - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot initialize streams - userPub not initialized');
		return;
	}

	console.log('[NETWORK] Initializing all user data streams');

	try {
		// Stop existing streams first
		ownDataStreamManager.stopAllStreams();
		chatStreamManager.stopAllStreams();

		// Create new streams
		await createOwnTreeStream();
		await createOwnCapacitiesStream();
		await createOwnContactsStream();
		await createOwnDesiredSlotComposeFromStream();
		await createOwnDesiredSlotComposeIntoStream();
		await createOwnChatReadStatesStream();

		// Note: setupUsersListSubscription is now called early in gun.svelte.ts
		// No need to call it again here

		// Note: Tree subscriptions are now handled automatically via mutual contributor streams
		// No separate initialization needed

		console.log('[NETWORK] User data streams initialized successfully');
	} catch (error) {
		console.error('[NETWORK] Error initializing user data streams:', error);
	}
}

/**
 * Smart SOGF update validation that prevents conflicts while allowing offline operation
 * @param existingEntry Current recognition cache entry (if any)
 * @param newTheirShare New share value from network
 * @param incomingTimestamp Timestamp from incoming SOGF data (if available)
 * @returns true if update should be accepted
 */
function shouldAcceptSOGFUpdate(
	existingEntry: any,
	newTheirShare: number,
	incomingTimestamp?: string
): boolean {
	// RULE 1: Always accept if no existing entry (initial load from Gun)
	if (!existingEntry) {
		console.log('[NETWORK] No existing entry - accepting initial SOGF data');
		return true;
	}

	// RULE 2: Accept if value has changed (regardless of timestamp)
	if (existingEntry.theirShare !== newTheirShare) {
		console.log(
			`[NETWORK] Value changed (${existingEntry.theirShare} → ${newTheirShare}) - accepting update`
		);
		return true;
	}

	// RULE 3: If value is unchanged, use timestamp validation (if reliable)
	if (incomingTimestamp && existingEntry.timestamp) {
		const incomingTime = new Date(incomingTimestamp).getTime();
		const existingTime = existingEntry.timestamp;

		// Only apply timestamp validation if incoming timestamp is reliable (not epoch)
		const isReliableTimestamp = incomingTime > new Date('1970-01-02T00:00:00.000Z').getTime();

		if (isReliableTimestamp) {
			if (incomingTime > existingTime) {
				console.log(
					`[NETWORK] Newer timestamp (${new Date(incomingTime).toISOString()}) - accepting update`
				);
				return true;
			} else {
				console.log(
					`[NETWORK] Older/same timestamp (${new Date(incomingTime).toISOString()}) - rejecting update`
				);
				return false;
			}
		}
	}

	// RULE 4: If no reliable timestamp comparison possible, reject duplicates
	console.log('[NETWORK] Same value, no reliable timestamp - rejecting duplicate');
	return false;
}

/**
 * Update the recognition cache with a contributor's share for us from network
 * Uses smart timestamp comparison with fallback to value-based deduplication
 * @param contributorId The ID of the contributor (could be contact ID or public key)
 * @param theirShare Share they assign to us in their SOGF
 */
export function updateTheirShareFromNetwork(contributorId: string, theirShare: number) {
	console.log(`[NETWORK] Received share from ${contributorId}: ${theirShare.toFixed(4)}`);

	// CRITICAL: Always resolve to public key for unified cache storage
	// This ensures consistency with our calculation layer and prevents feedback loops
	const resolvedContributorId = resolveToPublicKey(contributorId) || contributorId;

	if (resolvedContributorId !== contributorId) {
		console.log(`[NETWORK] Resolved ${contributorId} to ${resolvedContributorId}`);
	}

	// Get current cache entry using the resolved ID (consistent with calculation layer)
	const cache = get(recognitionCache);
	const existing = cache[resolvedContributorId];

	// Update the cache immediately with new theirShare using resolved public key
	recognitionCache.update((cache) => {
		if (existing) {
			// Update only theirShare in existing entry
			console.log(
				`[NETWORK] ✅ Updating existing entry for ${resolvedContributorId}: theirShare=${theirShare.toFixed(4)}`
			);
			cache[resolvedContributorId] = {
				...cache[resolvedContributorId],
				theirShare,
				timestamp: Date.now()
			};
		} else {
			// Create new entry with default ourShare of 0, using resolved public key
			console.log(
				`[NETWORK] ✅ Creating new entry for ${resolvedContributorId}: ourShare=0, theirShare=${theirShare.toFixed(4)}`
			);
			cache[resolvedContributorId] = {
				ourShare: 0, // We don't know our share yet
				theirShare,
				timestamp: Date.now()
			};
		}

		return cache;
	});
}

// Track if usersList subscription is active
let usersListSubscriptionActive = false;

// Centralized reactive subscription to usersList
export function setupUsersListSubscription() {
	if (typeof window === 'undefined') return; // Only run in browser
	if (usersListSubscriptionActive) {
		console.log('[USERS] usersList subscription already active, skipping setup');
		return;
	}

	console.log('[USERS] Setting up centralized usersList subscription');
	usersListSubscriptionActive = true;

	// Track current users to detect additions/removals
	const currentUsers = new Map<string, any>();

	// Helper function to update userPubKeys store whenever currentUsers changes
	function updateUserPubKeysStore() {
		const allUserIds = Array.from(currentUsers.keys());
		// console.log(`[USERS] Updating userIds store with ${allUserIds.length} users`);
		userPubKeys.set(allUserIds);
	}

	// Subscribe to all changes in the usersList
	usersList.map().on((userData: any, pubKey: string) => {
		console.log(`[USERS] User update received: ${pubKey}`, userData);

		if (!pubKey || pubKey === '_') return; // Skip invalid keys

		let collectionChanged = false;

		if (userData === null || userData === undefined) {
			// User was removed from usersList (they went offline or left the shared space)
			//console.log(`[USERS] User removed from usersList: ${userId}`);
			if (currentUsers.has(pubKey)) {
				currentUsers.delete(pubKey);
				collectionChanged = true;
			}

			// Note: We intentionally don't remove from userNamesCache here
			// because the cache serves a different purpose (performance optimization)
			// and this user might still be referenced in our tree as a contributor
		} else {
			// User was added or updated
			const wasNew = !currentUsers.has(pubKey);
			console.log(`[USERS] User ${wasNew ? 'added' : 'updated'}: ${pubKey}`, userData);
			currentUsers.set(pubKey, userData);
			if (wasNew) {
				collectionChanged = true;
			}

			// Get the user's alias from usersList (stored as 'alias' field, not 'name')
			const userAlias = userData.alias || userData.name; // Try alias first, fallback to name for compatibility
			if (userAlias && typeof userAlias === 'string') {
				// Update userAliasesCache with the alias from usersList
				console.log(`[USERS] Updating alias cache for ${pubKey}: ${userAlias}`);
				userAliasesCache.update((cache) => ({
					...cache,
					[pubKey]: userAlias
				}));
			} else {
				// Try to get alias from user's protected space using Gun's user system
				console.log(`[USERS] No alias in usersList for ${pubKey}, trying protected space...`);
				gun
					.user(pubKey) // Use Gun's user system
					.get('alias')
					.once((alias: any) => {
						if (alias && typeof alias === 'string') {
							console.log(`[USERS] Got alias from protected space for ${pubKey}: ${alias}`);
							userAliasesCache.update((cache) => ({
								...cache,
								[pubKey]: alias
							}));
						} else {
							console.log(`[USERS] No alias found anywhere for ${pubKey}, using truncated pubkey`);
							// Fallback to truncated pubkey
							const fallbackName = pubKey.substring(0, 8) + '...';
							userAliasesCache.update((cache) => ({
								...cache,
								[pubKey]: fallbackName
							}));
						}
					});
			}
		}

		// Only update userPubKeys store if the collection actually changed
		// This prevents unnecessary updates when just user data changes
		if (collectionChanged) {
			console.log(
				`[USERS] Collection changed, updating userPubKeys store. Total users: ${currentUsers.size}`
			);
			updateUserPubKeysStore();
		}
	});
}

// Derived store for all chat IDs we need to subscribe to
const chatIdsToSubscribe = derived(
	[userCapacities, networkCapacities],
	([$userCapacities, $networkCapacities]) => {
		const chatIds = new Set<string>();

		// Add all user capacity IDs (these are used as chat IDs)
		if ($userCapacities) {
			Object.keys($userCapacities).forEach((capacityId) => {
				chatIds.add(capacityId);
			});
		}

		// Add all network capacity IDs from mutual contributors
		if ($networkCapacities) {
			Object.values($networkCapacities).forEach((contributorCapacities) => {
				if (contributorCapacities) {
					Object.keys(contributorCapacities).forEach((capacityId) => {
						chatIds.add(capacityId);
					});
				}
			});
		}

		return Array.from(chatIds);
	}
);

// Watch for changes to chat IDs and subscribe to their message streams
const debouncedUpdateChatSubscriptions = debounce((chatIds: string[]) => {
	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			//console.log('[NETWORK] Cannot subscribe to chats - not authenticated');
			return;
		}
	} catch (error) {
		//console.log('[NETWORK] Cannot subscribe to chats - userPub not initialized');
		return;
	}

	//console.log(`[NETWORK] Updating chat subscriptions for ${chatIds.length} chats`);
	chatStreamManager.updateSubscriptions(chatIds, createChatMessagesStream);
}, 100);

chatIdsToSubscribe.subscribe(debouncedUpdateChatSubscriptions);

// Watch for changes to contributors and subscribe to get their SOGF data
const debouncedUpdateSOGFSubscriptions = debounce((allContributors: string[]) => {
	/*console.log(
		'[STREAM-DEBUG] debouncedUpdateSOGFSubscriptions called with contributors:',
		allContributors
	);*/

	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			//console.log('[NETWORK] Cannot subscribe to contributors - not authenticated');
			return;
		}
	} catch (error) {
		//console.log('[NETWORK] Cannot subscribe to contributors - userPub not initialized');
		return;
	}

	/*console.log(
		'[STREAM-DEBUG] About to update SOGF subscriptions for',
		allContributors.length,
		'contributors'
	);*/
	sogfStreamManager.updateSubscriptions(allContributors, createContributorSOGFStream);
}, 100);

contributors.subscribe(debouncedUpdateSOGFSubscriptions);

// Watch for changes to mutual contributors and subscribe to get their capacity data and shares
const debouncedUpdateMutualSubscriptions = debounce((currentMutualContributors: string[]) => {
	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			//console.log('[NETWORK] Cannot subscribe to mutual contributors - not authenticated');
			return;
		}
	} catch (error) {
		//console.log('[NETWORK] Cannot subscribe to mutual contributors - userPub not initialized');
		return;
	}

	if (!currentMutualContributors.length) {
		// Clear all network stores
		networkCapacities.set({});
		networkAllocationStates.set({});
		networkDesiredSlotComposeFrom.set({});
		networkDesiredSlotComposeInto.set({});
		mutualStreamManager.stopAllStreams();
		return;
	}

	// Define network stores for cleanup
	const networkStores = [
		{ store: networkCapacities, name: 'networkCapacities' },
		{ store: networkAllocationStates, name: 'networkAllocationStates' },
		{ store: networkDesiredSlotComposeFrom, name: 'networkDesiredComposeFrom' },
		{ store: networkDesiredSlotComposeInto, name: 'networkDesiredComposeInto' }
	];

	mutualStreamManager.updateSubscriptions(
		currentMutualContributors,
		createMutualContributorStreams
	);
}, 100);

mutualContributors.subscribe(debouncedUpdateMutualSubscriptions);

// Note: Tree subscriptions are now handled via the mutual contributor stream system
// No separate collective subscription needed

// Debug: Check if tree subscriptions are working via mutual contributor streams
mutualContributors.subscribe((currentMutualContributors) => {
	console.log(
		`[NETWORK] Mutual contributors updated: ${currentMutualContributors.length} contributors`,
		currentMutualContributors.map((id) => id.substring(0, 20) + '...')
	);

	// Debug: Check if tree subscriptions are working after delay
	setTimeout(() => {
		const currentForest = get(collectiveForest);
		console.log(
			`[NETWORK] Forest check after 3 seconds: size=${currentForest.size}, keys=[${Array.from(
				currentForest.keys()
			)
				.map((k) => k.substring(0, 20) + '...')
				.join(', ')}]`
		);
	}, 3000);
});
