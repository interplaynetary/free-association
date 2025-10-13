import { get } from 'svelte/store';
import { gun, user, userPub, usersList } from './gun.svelte';
import {
	userPubKeys,
	userAliasesCache,
	resolveToPublicKey,
	userContacts,
	isLoadingContacts,
	initializeContacts
} from '$lib/state/users.svelte';
import { USE_HOLSTER_CONTACTS, USE_HOLSTER_CAPACITIES, USE_HOLSTER_TREE, USE_HOLSTER_RECOGNITION, USE_HOLSTER_CHAT } from '$lib/config';
import { initializeHolsterCapacities } from './capacities-holster.svelte';
import { initializeHolsterTree } from './tree-holster.svelte';
import { initializeHolsterSogf } from './recognition-holster.svelte';
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
	networkAllocationStates
} from '$lib/state/core.svelte';
import { getGunTimestamp, compareGunTimestamps, isReliableGunTimestamp } from '$lib/utils/gunTimestamp';
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
 * Track the most recent network timestamps for each data type
 * Used by persistence layer to prevent overwriting newer network data
 */
export const lastNetworkTimestamps = $state({
	tree: null as number | null,
	capacities: null as number | null,
	contacts: null as number | null,
	desiredSlotComposeFrom: null as number | null,
	desiredSlotComposeInto: null as number | null,
	chatReadStates: null as number | null,
	sogf: null as number | null
});

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
 * Generic data processor that handles validation, comparison, and store updates using Gun's native timestamps
 */
function createDataProcessor<T>(config: {
	dataType: string;
	validator?: (data: any) => T | null;
	getCurrentData: () => T | null | unknown;
	updateStore: (data: T) => void;
	loadingFlag?: { set: (value: boolean) => void };
	onUpdate?: () => void;
	emptyValue?: T;
	enableTimestampComparison?: boolean;
	gunTimestampField?: string; // Field name to extract Gun timestamp from (defaults to most recent)
	timestampKey?: keyof typeof lastNetworkTimestamps; // Key to store timestamp in export object
}) {
	// Store the most recent Gun node reference for timestamp extraction
	let lastGunNode: any = null;
	let lastTimestamp: number | null = null;

	return (rawData: any) => {
		const {
			dataType,
			validator,
			getCurrentData,
			updateStore,
			loadingFlag,
			onUpdate,
			emptyValue,
			enableTimestampComparison,
			gunTimestampField,
			timestampKey
		} = config;

		if (!rawData) {
			//console.log(`[NETWORK] No ${dataType} data found`);
			loadingFlag?.set(false);
			return;
		}

		//console.log(`[NETWORK] Received ${dataType} update from stream`);
		loadingFlag?.set(true);

		try {
			// Store raw Gun node for timestamp extraction
			lastGunNode = rawData;

			let processedData = rawData;

			// Apply validator (which handles both parsing and validation)
			if (validator) {
				processedData = validator(rawData);
				if (!processedData) {
					console.error(`[NETWORK] Failed to validate ${dataType} data`);
					loadingFlag?.set(false);
					return;
				}
			}

			// Get current data for comparison
			const currentData = getCurrentData();

			// SMART TIMESTAMP-BASED FRESHNESS CHECK using Gun's native timestamps
			if (enableTimestampComparison) {
				try {
					// Extract Gun timestamp from raw node
					const incomingTimestamp = gunTimestampField
						? getGunTimestamp(lastGunNode, gunTimestampField)
						: getGunTimestamp(lastGunNode, '_'); // Default to root node timestamp

					if (incomingTimestamp !== null) {
						// Compare with last known timestamp
						if (lastTimestamp !== null) {
							const comparison = compareGunTimestamps(incomingTimestamp, lastTimestamp);

							if (comparison <= 0 && isReliableGunTimestamp(incomingTimestamp)) {
								console.log(
									`[NETWORK] Incoming ${dataType} is older/same (Gun ts: ${incomingTimestamp}) than current (${lastTimestamp}), ignoring update`
								);
								loadingFlag?.set(false);
								return;
							}

							if (isReliableGunTimestamp(incomingTimestamp)) {
								console.log(
									`[NETWORK] Incoming ${dataType} is newer (Gun ts: ${incomingTimestamp}) than current (${lastTimestamp}), accepting update`
								);
								lastTimestamp = incomingTimestamp;
							} else {
								console.log(
									`[NETWORK] Incoming ${dataType} has unreliable Gun timestamp (${incomingTimestamp}), falling back to value comparison`
								);
							}
						} else {
							// First timestamp - store it
							lastTimestamp = incomingTimestamp;
							console.log(`[NETWORK] First ${dataType} Gun timestamp recorded: ${incomingTimestamp}`);
						}

						// Export timestamp for persistence layer
						if (timestampKey && isReliableGunTimestamp(incomingTimestamp)) {
							lastNetworkTimestamps[timestampKey] = incomingTimestamp;
						}
					}
				} catch (timestampError) {
					console.warn(`[NETWORK] Error extracting Gun timestamps for ${dataType}:`, timestampError);
					// Fall through to regular comparison if timestamp extraction fails
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
			timestampKey: 'tree',
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
			gunTimestampField: 'capacities', // Track timestamp of capacities field
			timestampKey: 'capacities',
			validator: parseCapacities,
			getCurrentData: () => get(userCapacities),
			updateStore: (data) => userCapacities.set(data),
			loadingFlag: isLoadingCapacities
		}),
		errorHandler: (error: any) => {
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
			gunTimestampField: 'contacts',
			timestampKey: 'contacts',
			validator: parseContacts,
			getCurrentData: () => get(userContacts),
			updateStore: (data) => userContacts.set(data),
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
		processor: createDataProcessor({
			dataType: 'desiredSlotComposeFrom',
			enableTimestampComparison: true,
			gunTimestampField: 'desiredSlotComposeFrom',
			timestampKey: 'desiredSlotComposeFrom',
			validator: parseUserSlotComposition,
			getCurrentData: () => get(userDesiredSlotComposeFrom),
			updateStore: (data) => {
				console.log('[COMPOSE] [NETWORK-OWN] ✅ Updating own compose-from store:', data);
				userDesiredSlotComposeFrom.set(data);
			}
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own desiredSlotComposeFrom stream:', error);
			console.error('[COMPOSE] [NETWORK-OWN] ❌ Error loading own compose-from:', error);
		}
	},
	desiredComposeInto: {
		type: 'desiredSlotComposeInto',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('desiredSlotComposeInto'),
		processor: createDataProcessor({
			dataType: 'desiredSlotComposeInto',
			enableTimestampComparison: true,
			gunTimestampField: 'desiredSlotComposeInto',
			timestampKey: 'desiredSlotComposeInto',
			validator: parseUserSlotComposition,
			getCurrentData: () => get(userDesiredSlotComposeInto),
			updateStore: (data) => {
				console.log('[COMPOSE] [NETWORK-OWN] ✅ Updating own compose-into store:', data);
				userDesiredSlotComposeInto.set(data);
			}
		}),
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
			gunTimestampField: 'chatReadStates',
			timestampKey: 'chatReadStates',
			validator: parseChatReadStates,
			getCurrentData: () => get(chatReadStates),
			updateStore: (data) => chatReadStates.set(data),
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

			// Validate SOGF data using parseShareMap (now returns unwrapped data)
			const validatedSogfData = parseShareMap(sogfData);
			if (!validatedSogfData || Object.keys(validatedSogfData).length === 0) {
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

			// Extract the share (now direct access - no .data wrapper)
			const theirShare = validatedSogfData[ourId] || 0;

			// Get current recognition cache entry
			const currentCache = get(recognitionCache);
			const existingEntry = currentCache[contributorId];

			// Extract Gun timestamp for SOGF update
			const gunTimestamp = getGunTimestamp(sogfData, 'sogf');

			// SMART TIMESTAMPING: Apply timestamp validation using Gun timestamps
			const shouldUpdate = shouldAcceptSOGFUpdate(
				existingEntry,
				theirShare,
				gunTimestamp
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
				gunTimestampField: 'capacities',
				validator: parseCapacities,
				getCurrentData: () => get(networkCapacities)[contributorId] || null,
				updateStore: (data) => {
					if (data) {
						// Store unwrapped data directly
						networkCapacities.update((current) => ({
							...current,
							[contributorId]: data
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

			if (!composeFromData) {
				console.log(
					`[COMPOSE] [NETWORK] No desired slot compose-from from contributor ${contributorId}`
				);
				networkDesiredSlotComposeFrom.update((current) => {
					const { [contributorId]: _, ...rest} = current;
					return rest;
				});
				return;
			}

			const validatedComposeFrom = parseUserSlotComposition(composeFromData);
			console.log(`[COMPOSE] [NETWORK] Validated compose-from (unwrapped):`, validatedComposeFrom);

			const currentNetworkComposeFrom = get(networkDesiredSlotComposeFrom)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeFrom) === JSON.stringify(currentNetworkComposeFrom);

			if (!isUnchanged) {
				console.log(
					`[COMPOSE] [NETWORK] ✅ Updating network compose-from for ${contributorId}:`,
					validatedComposeFrom
				);
				networkDesiredSlotComposeFrom.update((current) => ({
					...current,
					[contributorId]: validatedComposeFrom
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

			const validatedComposeInto = parseUserSlotComposition(composeIntoData);
			console.log(`[COMPOSE] [NETWORK] Validated compose-into (unwrapped):`, validatedComposeInto);

			const currentNetworkComposeInto = get(networkDesiredSlotComposeInto)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeInto) === JSON.stringify(currentNetworkComposeInto);

			if (!isUnchanged) {
				console.log(
					`[COMPOSE] [NETWORK] ✅ Updating network compose-into for ${contributorId}:`,
					validatedComposeInto
				);
				networkDesiredSlotComposeInto.update((current) => ({
					...current,
					[contributorId]: validatedComposeInto
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
	// When using Holster, initialize Holster tree instead of Gun stream
	if (USE_HOLSTER_TREE) {
		console.log('[NETWORK] Using Holster for tree - skipping Gun stream');
		initializeHolsterTree();
		return;
	}

	// Gun implementation
	await createStream(ownDataStreamConfigs.tree, userId);
});

const createOwnCapacitiesStream = withAuthentication(async (userId: string) => {
	// When using Holster, initialize Holster capacities instead of Gun stream
	if (USE_HOLSTER_CAPACITIES) {
		console.log('[NETWORK] Using Holster for capacities - skipping Gun stream');
		initializeHolsterCapacities();
		return;
	}

	// Gun implementation
	await createStream(ownDataStreamConfigs.capacities, userId);
});

const createOwnDesiredSlotComposeFromStream = withAuthentication(async (userId: string) => {
	// When using Holster, initialize Holster compose instead of Gun stream
	const { USE_HOLSTER_COMPOSE } = await import('$lib/config');
	if (USE_HOLSTER_COMPOSE) {
		console.log('[NETWORK] Using Holster for compose-from - skipping Gun stream');
		const { initializeHolsterComposeFrom } = await import('./compose-holster.svelte');
		initializeHolsterComposeFrom();
		return;
	}

	// Gun implementation
	await createStream(ownDataStreamConfigs.desiredComposeFrom, userId);
});

const createOwnDesiredSlotComposeIntoStream = withAuthentication(async (userId: string) => {
	// When using Holster, initialize Holster compose instead of Gun stream
	const { USE_HOLSTER_COMPOSE } = await import('$lib/config');
	if (USE_HOLSTER_COMPOSE) {
		console.log('[NETWORK] Using Holster for compose-into - skipping Gun stream');
		const { initializeHolsterComposeInto } = await import('./compose-holster.svelte');
		initializeHolsterComposeInto();
		return;
	}

	// Gun implementation
	await createStream(ownDataStreamConfigs.desiredComposeInto, userId);
});

// DELETED: createOwnDesiredSlotClaimsStream - Replaced by unified compose-from model

const createOwnContactsStream = withAuthentication(async (userId: string) => {
	// When using Holster, initialize Holster contacts instead of Gun stream
	if (USE_HOLSTER_CONTACTS) {
		console.log('[NETWORK] Using Holster for contacts - skipping Gun stream');
		initializeContacts();
		return;
	}

	// Gun implementation
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

	// Skip Gun SOGF stream for ourselves if using Holster for our own SOGF
	if (USE_HOLSTER_RECOGNITION) {
		const ourPubKey = get(userPub);
		if (pubKey === ourPubKey) {
			console.log('[STREAM-DEBUG] Skipping Gun SOGF stream for self - using Holster');
			return;
		}
	}

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

		// Check if Holster compose is enabled to skip Gun compose streams
		const { USE_HOLSTER_COMPOSE } = await import('$lib/config');

		// Create all mutual contributor streams
		let streamTypes = [
			'capacities',
			'allocationStates',
			'desiredSlotComposeFrom',
			'desiredSlotComposeInto',
			'tree'
		] as const;

		// Skip compose streams if using Holster
		if (USE_HOLSTER_COMPOSE) {
			streamTypes = streamTypes.filter(
				(type) => type !== 'desiredSlotComposeFrom' && type !== 'desiredSlotComposeInto'
			) as any;
			console.log(`[NETWORK] Skipping Gun compose streams for ${contributorId} - using Holster`);
		}

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
		const currentUserPub = get(userPub);

		if (!userPub || !currentUserPub) {
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

		// Initialize Holster SOGF if enabled
		// Note: SOGF is computed from our tree, so we only persist it
		// We still subscribe to contributors' Gun SOGF streams regardless
		if (USE_HOLSTER_RECOGNITION) {
			console.log('[NETWORK] Using Holster for SOGF - initializing');
			initializeHolsterSogf();
		}

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
 * Initialize all Holster user data streams
 * Called after Holster authentication succeeds
 * This is completely independent from Gun initialization
 */
export async function initializeHolsterDataStreams(): Promise<void> {
	// Import holsterUser to check authentication
	const { holsterUser } = await import('./holster.svelte');

	if (!holsterUser.is) {
		console.log('[NETWORK-HOLSTER] Cannot initialize - not authenticated');
		return;
	}

	console.log('[NETWORK-HOLSTER] Initializing all Holster data streams');

	try {
		// Initialize all Holster modules that have been migrated
		const { initializeHolsterContacts } = await import('./contacts-holster.svelte');
		const { initializeHolsterCapacities } = await import('./capacities-holster.svelte');
		const { initializeHolsterTree } = await import('./tree-holster.svelte');
		const { initializeHolsterSogf } = await import('./recognition-holster.svelte');

		// Initialize each module
		initializeHolsterContacts();
		initializeHolsterCapacities();
		initializeHolsterTree();
		initializeHolsterSogf();

		// Chat initialization is handled by getChatMessages() calling subscribeToHolsterChat()
		// No explicit initialization needed here

		console.log('[NETWORK-HOLSTER] All Holster data streams initialized successfully');
	} catch (error) {
		console.error('[NETWORK-HOLSTER] Error initializing Holster data streams:', error);
	}
}

/**
 * Smart SOGF update validation using Gun's native timestamps
 * @param existingEntry Current recognition cache entry (if any)
 * @param newTheirShare New share value from network
 * @param gunTimestamp Gun timestamp from incoming SOGF data (milliseconds, if available)
 * @returns true if update should be accepted
 */
function shouldAcceptSOGFUpdate(
	existingEntry: any,
	newTheirShare: number,
	gunTimestamp: number | null
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

	// RULE 3: If value is unchanged, use Gun timestamp validation (if available and reliable)
	if (gunTimestamp !== null && existingEntry.timestamp) {
		const existingTime = existingEntry.timestamp;

		// Only apply timestamp validation if Gun timestamp is reliable
		if (isReliableGunTimestamp(gunTimestamp)) {
			const comparison = compareGunTimestamps(gunTimestamp, existingTime);

			if (comparison > 0) {
				console.log(
					`[NETWORK] Newer Gun timestamp (${gunTimestamp}) - accepting update`
				);
				return true;
			} else {
				console.log(
					`[NETWORK] Older/same Gun timestamp (${gunTimestamp}) - rejecting update`
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
	if (typeof window === 'undefined') {
		console.log('[USERS-DEBUG] Skipping usersList subscription - not in browser');
		return; // Only run in browser
	}
	if (usersListSubscriptionActive) {
		console.log('[USERS] usersList subscription already active, skipping setup');
		return;
	}

	console.log('[USERS] Setting up centralized usersList subscription');
	console.log('[USERS-DEBUG] usersList reference:', usersList);
	usersListSubscriptionActive = true;

	// Test: Expose a function to manually trigger a test update
	if (typeof window !== 'undefined') {
		(window as any).testUsersListUpdate = () => {
			console.log('[USERS-TEST] Manually triggering test update...');
			usersList.get('test-user-' + Date.now()).put({
				alias: 'TestUser' + Date.now(),
				lastSeen: Date.now()
			});
		};
		console.log('[USERS-TEST] Run window.testUsersListUpdate() to test subscription');
	}

	// Track current users to detect additions/removals
	const currentUsers = new Map<string, any>();

	// Helper function to update userPubKeys store whenever currentUsers changes
	function updateUserPubKeysStore() {
		const allUserIds = Array.from(currentUsers.keys());
		console.log(`[USERS-DEBUG] Updating userPubKeys store with ${allUserIds.length} users:`, 
			allUserIds.map(id => id.slice(0, 20) + '...'));
		userPubKeys.set(allUserIds);
	}

	// Subscribe to all changes in the usersList
	console.log('[USERS-DEBUG] Setting up usersList.map().on subscription');
	
	// Debug: Try to read current usersList data
	setTimeout(() => {
		console.log('[USERS-DEBUG] Attempting to read current usersList data...');
		usersList.map().once((userData: any, pubKey: string) => {
			if (pubKey && pubKey !== '_' && userData) {
				console.log('[USERS-DEBUG] Found existing user in usersList:', {
					pubKey: pubKey.slice(0, 20) + '...',
					userData
				});
			}
		});
	}, 2000);

	usersList.map().on((userData: any, pubKey: string) => {
		console.log(`[USERS-DEBUG] Raw usersList update:`, {
			pubKey: pubKey?.slice(0, 20) + '...',
			userData,
			isNull: userData === null,
			isUndefined: userData === undefined,
			currentUsersSize: currentUsers.size
		});

		if (!pubKey || pubKey === '_') {
			console.log(`[USERS-DEBUG] Skipping invalid pubKey: ${pubKey}`);
			return; // Skip invalid keys
		}

		let collectionChanged = false;

		if (userData === null || userData === undefined) {
			// User was removed from usersList (they went offline or left the shared space)
			console.log(`[USERS-DEBUG] User removed from usersList: ${pubKey?.slice(0, 20) + '...'}`);
			if (currentUsers.has(pubKey)) {
				currentUsers.delete(pubKey);
				collectionChanged = true;
				console.log(`[USERS-DEBUG] Removed user from currentUsers map. New size: ${currentUsers.size}`);
			}

			// Note: We intentionally don't remove from userNamesCache here
			// because the cache serves a different purpose (performance optimization)
			// and this user might still be referenced in our tree as a contributor
		} else {
			// User was added or updated
			const wasNew = !currentUsers.has(pubKey);
			console.log(`[USERS-DEBUG] User ${wasNew ? 'added' : 'updated'}: ${pubKey?.slice(0, 20) + '...'}`, userData);
			currentUsers.set(pubKey, userData);
			if (wasNew) {
				collectionChanged = true;
				console.log(`[USERS-DEBUG] New user added to currentUsers map. New size: ${currentUsers.size}`);
			}

			// Get the user's alias from usersList (stored as 'alias' field, not 'name')
			const userAlias = userData.alias || userData.name; // Try alias first, fallback to name for compatibility
			if (userAlias && typeof userAlias === 'string') {
				// Update userAliasesCache with the alias from usersList
				console.log(`[USERS-DEBUG] Updating alias cache for ${pubKey?.slice(0, 20) + '...'}: ${userAlias}`);
				userAliasesCache.update((cache) => ({
					...cache,
					[pubKey]: userAlias
				}));
			} else {
				// Try to get alias from user's protected space using Gun's user system
				console.log(`[USERS-DEBUG] No alias in usersList for ${pubKey?.slice(0, 20) + '...'}, trying protected space...`);
				gun
					.user(pubKey) // Use Gun's user system
					.get('alias')
					.once((alias: any) => {
						if (alias && typeof alias === 'string') {
							console.log(`[USERS-DEBUG] Got alias from protected space for ${pubKey?.slice(0, 20) + '...'}: ${alias}`);
							userAliasesCache.update((cache) => ({
								...cache,
								[pubKey]: alias
							}));
						} else {
							console.log(`[USERS-DEBUG] No alias found anywhere for ${pubKey?.slice(0, 20) + '...'}, using truncated pubkey`);
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
				`[USERS-DEBUG] Collection changed, updating userPubKeys store. Total users: ${currentUsers.size}`
			);
			updateUserPubKeysStore();
		} else {
			console.log(`[USERS-DEBUG] No collection change, skipping userPubKeys store update`);
		}
	});

	// Debug: Log userPubKeys store changes
	userPubKeys.subscribe((pubKeys) => {
		console.log('[USERS-DEBUG] userPubKeys store updated:', {
			count: pubKeys.length,
			pubKeys: pubKeys.map(pk => pk.slice(0, 20) + '...')
		});
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
	// Skip Gun chat streams when using Holster
	if (USE_HOLSTER_CHAT) {
		console.log('[NETWORK] Using Holster for chat - skipping Gun chat streams');
		chatStreamManager.stopAllStreams();
		// Note: Holster chat subscriptions are managed by chat-holster.svelte.ts
		// via getChatMessages() which calls subscribeToHolsterChat()
		return;
	}

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

// Track active Holster SOGF subscriptions
const activeHolsterSogfSubs = new Set<string>();

// Track active Holster compose subscriptions for mutual contributors
const activeHolsterComposeFromSubs = new Set<string>();
const activeHolsterComposeIntoSubs = new Set<string>();

// Watch for changes to contributors and subscribe to get their SOGF data
const debouncedUpdateSOGFSubscriptions = debounce((allContributors: string[]) => {
	/*console.log(
		'[STREAM-DEBUG] debouncedUpdateSOGFSubscriptions called with contributors:',
		allContributors
	);*/

	// Use Holster SOGF streams when Holster recognition is enabled
	if (USE_HOLSTER_RECOGNITION) {
		console.log('[SOGF-STREAMS] Using Holster for recognition - setting up Holster SOGF streams');
		sogfStreamManager.stopAllStreams(); // Stop any Gun streams

		// Get contributors with resolved public keys (filter out null and undefined)
		const contributorsWithPubKeys = allContributors
			.map(id => resolveToPublicKey(id))
			.filter((pubKey): pubKey is string => pubKey !== null && pubKey !== undefined);

		// Get our own pubkey to skip ourselves
		const ourPubKey = get(userPub);

		// Remove subscriptions for contributors no longer in the list
		activeHolsterSogfSubs.forEach(pubKey => {
			if (!contributorsWithPubKeys.includes(pubKey)) {
				console.log(`[SOGF-HOLSTER] Removing subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterSogfSubs.delete(pubKey);
				// Note: Holster doesn't have a clean way to unsubscribe, but removing from tracking set is enough
			}
		});

		// Add subscriptions for new contributors
		contributorsWithPubKeys.forEach(pubKey => {
			// Skip ourselves
			if (pubKey === ourPubKey) {
				console.log('[SOGF-HOLSTER] Skipping subscription to own SOGF');
				return;
			}

			if (!activeHolsterSogfSubs.has(pubKey)) {
				console.log(`[SOGF-HOLSTER] Adding subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterSogfSubs.add(pubKey);

				// Import the subscription function dynamically to avoid circular deps
				import('./recognition-holster.svelte').then(({ subscribeToContributorHolsterSogf }) => {
					subscribeToContributorHolsterSogf(pubKey, (theirShare, timestamp) => {
						// Update recognition cache
						updateTheirShareFromNetwork(pubKey, theirShare);
					});
				});
			}
		});

		console.log(`[SOGF-HOLSTER] Active Holster SOGF subscriptions: ${activeHolsterSogfSubs.size}`);
		return;
	}

	// Gun implementation (legacy)
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
const debouncedUpdateMutualSubscriptions = debounce(async (currentMutualContributors: string[]) => {
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

		// Clear Holster compose tracking sets
		activeHolsterComposeFromSubs.clear();
		activeHolsterComposeIntoSubs.clear();
		return;
	}

	// Define network stores for cleanup
	const networkStores = [
		{ store: networkCapacities, name: 'networkCapacities' },
		{ store: networkAllocationStates, name: 'networkAllocationStates' },
		{ store: networkDesiredSlotComposeFrom, name: 'networkDesiredComposeFrom' },
		{ store: networkDesiredSlotComposeInto, name: 'networkDesiredComposeInto' }
	];

	// Check if Holster compose is enabled
	const { USE_HOLSTER_COMPOSE } = await import('$lib/config');

	if (USE_HOLSTER_COMPOSE) {
		console.log('[COMPOSE-HOLSTER] Using Holster for compose - setting up Holster compose streams for mutual contributors');

		// Get contributors with resolved public keys
		const contributorsWithPubKeys = currentMutualContributors
			.map(id => resolveToPublicKey(id))
			.filter((pubKey): pubKey is string => pubKey !== null && pubKey !== undefined);

		const ourPubKey = get(userPub);

		// === COMPOSE-FROM subscriptions ===
		// Remove subscriptions for contributors no longer in the list
		activeHolsterComposeFromSubs.forEach(pubKey => {
			if (!contributorsWithPubKeys.includes(pubKey)) {
				console.log(`[COMPOSE-FROM-HOLSTER] Removing subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterComposeFromSubs.delete(pubKey);
				// Clear from network store
				networkDesiredSlotComposeFrom.update((current) => {
					const { [pubKey]: _, ...rest } = current;
					return rest;
				});
			}
		});

		// Add subscriptions for new contributors
		contributorsWithPubKeys.forEach(pubKey => {
			// Skip ourselves
			if (pubKey === ourPubKey) {
				return;
			}

			if (!activeHolsterComposeFromSubs.has(pubKey)) {
				console.log(`[COMPOSE-FROM-HOLSTER] Adding subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterComposeFromSubs.add(pubKey);

				// Import and subscribe
				import('./compose-holster.svelte').then(({ subscribeToContributorHolsterComposeFrom }) => {
					subscribeToContributorHolsterComposeFrom(pubKey, (composeFrom) => {
						// Update network store
						networkDesiredSlotComposeFrom.update((current) => ({
							...current,
							[pubKey]: composeFrom
						}));
					});
				});
			}
		});

		// === COMPOSE-INTO subscriptions ===
		// Remove subscriptions for contributors no longer in the list
		activeHolsterComposeIntoSubs.forEach(pubKey => {
			if (!contributorsWithPubKeys.includes(pubKey)) {
				console.log(`[COMPOSE-INTO-HOLSTER] Removing subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterComposeIntoSubs.delete(pubKey);
				// Clear from network store
				networkDesiredSlotComposeInto.update((current) => {
					const { [pubKey]: _, ...rest } = current;
					return rest;
				});
			}
		});

		// Add subscriptions for new contributors
		contributorsWithPubKeys.forEach(pubKey => {
			// Skip ourselves
			if (pubKey === ourPubKey) {
				return;
			}

			if (!activeHolsterComposeIntoSubs.has(pubKey)) {
				console.log(`[COMPOSE-INTO-HOLSTER] Adding subscription for ${pubKey.slice(0, 20)}...`);
				activeHolsterComposeIntoSubs.add(pubKey);

				// Import and subscribe
				import('./compose-holster.svelte').then(({ subscribeToContributorHolsterComposeInto }) => {
					subscribeToContributorHolsterComposeInto(pubKey, (composeInto) => {
						// Update network store
						networkDesiredSlotComposeInto.update((current) => ({
							...current,
							[pubKey]: composeInto
						}));
					});
				});
			}
		});

		console.log(`[COMPOSE-HOLSTER] Active Holster compose subscriptions: from=${activeHolsterComposeFromSubs.size}, into=${activeHolsterComposeIntoSubs.size}`);

		// Note: Still create Gun streams for other data (capacities, allocationStates, tree)
		// but skip compose-from/into streams
		mutualStreamManager.updateSubscriptions(
			currentMutualContributors,
			createMutualContributorStreams
		);
		return;
	}

	// Gun implementation (legacy) - create all streams including compose
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

// ============================================================================
// Development Test Utilities
// ============================================================================

if (import.meta.env.DEV && typeof window !== 'undefined') {
	(window as any).testNetworkInit = {
		// Test Gun initialization
		testGunInit: async () => {
			console.log('[TEST] Testing Gun initialization...');
			console.log('[TEST] Current userPub:', get(userPub) || '(not authenticated)');
			await initializeUserDataStreams();
			console.log('[TEST] Gun initialization complete');
		},

		// Test Holster initialization
		testHolsterInit: async () => {
			console.log('[TEST] Testing Holster initialization...');
			const { holsterUser } = await import('./holster.svelte');
			console.log('[TEST] Holster authenticated:', holsterUser.is?.username || '(not authenticated)');
			await initializeHolsterDataStreams();
			console.log('[TEST] Holster initialization complete');
		}
	};

	console.log('[TEST] Network initialization test utilities loaded');
	console.log('[TEST] Run window.testNetworkInit.testGunInit() to test Gun initialization');
	console.log('[TEST] Run window.testNetworkInit.testHolsterInit() to test Holster initialization');
}
