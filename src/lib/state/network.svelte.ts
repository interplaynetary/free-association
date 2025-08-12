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
	networkCapacityShares,
	networkCapacitySlotQuantities,
	userTree,
	userCapacities,
	isLoadingTree,
	isLoadingCapacities,
	userDesiredSlotComposeFrom,
	userDesiredSlotComposeInto,
	networkDesiredSlotComposeFrom,
	networkDesiredSlotComposeInto,
	userNetworkCapacitiesWithShares
} from '$lib/state/core.svelte';
import { chatReadStates, isLoadingChatReadStates, setChatReadStates } from '$lib/state/chat.svelte';
import { collectiveMembers, collectiveForest } from '$lib/collective.svelte';
import type { NetworkSlotComposition } from '$lib/schema';
import { recalculateFromTree } from './calculations.svelte';
import {
	parseCapacities,
	parseTree,
	parseUserSlotComposition,
	parseShareMap,
	parseContacts,
	parseCapacityShares,
	parseCapacitySlotQuantities,
	parseChatReadStates
} from '$lib/validators/validation';
import { debounce } from '$lib/utils/debounce';
import { derived } from 'svelte/store';

/**
 * Enhanced Gun subscription wrapper using ReadableStream for proper lifecycle management
 */
class GunSubscriptionStream<T> {
	private reader: ReadableStreamDefaultReader<T> | null = null;
	private stream: ReadableStream<T> | null = null;
	private isActive = false;
	private gunRef: any;
	private streamId: string;
	private hasReceivedData = false;

	constructor(
		private gunPath: () => any,
		private streamType: string,
		private onData: (data: T) => void,
		private onError?: (error: any) => void
	) {
		this.streamId = `${streamType}_${Math.random().toString(36).substr(2, 9)}`;
	}

	/**
	 * Start the subscription stream
	 */
	async start(): Promise<void> {
		if (this.isActive) {
			console.warn(`[STREAM] ${this.streamType} stream already active`);
			return;
		}

		try {
			this.isActive = true;
			console.log(`[STREAM] Starting ${this.streamType} stream ${this.streamId}`);

			this.stream = new ReadableStream<T>({
				start: (controller) => {
					try {
						// Get the Gun reference
						this.gunRef = this.gunPath();

						if (!this.gunRef) {
							console.warn(
								`[STREAM] No Gun reference for ${this.streamType} stream ${this.streamId}`
							);
							// Don't complete the stream - keep it open to retry later
							return;
						}

						// Set up the Gun subscription
						this.gunRef.on((data: T) => {
							if (this.isActive) {
								this.hasReceivedData = true;
								if (data !== null && data !== undefined) {
									controller.enqueue(data);
								}
							}
						});

						console.log(`[STREAM] ${this.streamType} stream ${this.streamId} started successfully`);
					} catch (error) {
						console.error(`[STREAM] Error starting ${this.streamType} stream:`, error);
						controller.error(error);
						this.onError?.(error);
					}
				},
				cancel: () => {
					console.log(`[STREAM] Cancelling ${this.streamType} stream ${this.streamId}`);
					this.cleanup();
				}
			});

			// Get the reader and start processing
			this.reader = this.stream.getReader();
			this.processStream();
		} catch (error) {
			console.error(`[STREAM] Failed to start ${this.streamType} stream:`, error);
			this.cleanup();
			this.onError?.(error);
		}
	}

	/**
	 * Process the stream data
	 */
	private async processStream(): Promise<void> {
		if (!this.reader) return;

		try {
			while (this.isActive) {
				const { value, done } = await this.reader.read();

				if (done) {
					console.log(`[STREAM] ${this.streamType} stream ${this.streamId} completed`);
					break;
				}

				if (value && this.isActive) {
					this.onData(value);
				}
			}
		} catch (error) {
			if (this.isActive) {
				console.error(`[STREAM] Error in ${this.streamType} stream:`, error);
				this.onError?.(error);
			}
		} finally {
			// Only cleanup if we're not active anymore (manual stop) or if we had an error
			// Don't cleanup just because no data was received
			if (!this.isActive) {
				this.cleanup();
			}
		}
	}

	/**
	 * Stop the subscription stream
	 */
	stop(): void {
		if (!this.isActive) return;

		console.log(`[STREAM] Stopping ${this.streamType} stream ${this.streamId}`);
		this.isActive = false;
		this.cleanup();
	}

	/**
	 * Clean up resources
	 */
	private cleanup(): void {
		this.isActive = false;

		try {
			// Cancel the reader
			if (this.reader) {
				this.reader.cancel();
				this.reader = null;
			}

			// Clean up Gun subscription
			if (this.gunRef && typeof this.gunRef.off === 'function') {
				this.gunRef.off();
				this.gunRef = null;
			}

			this.stream = null;
			console.log(`[STREAM] Cleaned up ${this.streamType} stream ${this.streamId}`);
		} catch (error) {
			console.error(`[STREAM] Error during cleanup of ${this.streamType} stream:`, error);
		}
	}

	/**
	 * Check if stream is active
	 */
	get active(): boolean {
		return this.isActive;
	}
}

/**
 * Enhanced subscription manager with ReadableStream support and memoization
 */
class StreamSubscriptionManager {
	private activeStreams = new Map<string, GunSubscriptionStream<any>>();
	private subscriptionType: string;
	private lastContributorsList: string[] = [];
	private isUpdating = false;

	constructor(subscriptionType: string) {
		this.subscriptionType = subscriptionType;
	}

	/**
	 * Create a new subscription stream only if it doesn't already exist
	 */
	async createStream<T>(
		contributorId: string,
		gunPath: () => any,
		streamType: string,
		onData: (data: T) => void,
		onError?: (error: any) => void
	): Promise<void> {
		const streamKey = `${contributorId}_${streamType}`;

		// Check if stream already exists and is active
		const existingStream = this.activeStreams.get(streamKey);
		if (existingStream && existingStream.active) {
			/*console.log(
				`[STREAM-MANAGER] Stream ${streamKey} already exists and is active, skipping creation`
			);*/
			return;
		}

		// Only stop existing stream if it exists but is not active
		if (existingStream) {
			//console.log(`[STREAM-MANAGER] Stopping inactive stream for ${streamKey}`);
			existingStream.stop();
			this.activeStreams.delete(streamKey);
		}

		// Create new stream
		const stream = new GunSubscriptionStream(
			gunPath,
			`${this.subscriptionType}_${streamType}`,
			onData,
			onError
		);

		this.activeStreams.set(streamKey, stream);

		try {
			await stream.start();
			//console.log(`[STREAM-MANAGER] Created stream for ${streamKey}`);
		} catch (error) {
			//console.error(`[STREAM-MANAGER] Failed to create stream for ${streamKey}:`, error);
			this.activeStreams.delete(streamKey);
			throw error;
		}
	}

	/**
	 * Stop a specific stream
	 */
	stopStream(contributorId: string, streamType: string): void {
		const streamKey = `${contributorId}_${streamType}`;
		const stream = this.activeStreams.get(streamKey);

		if (stream) {
			//console.log(`[STREAM-MANAGER] Stopping stream for ${streamKey}`);
			stream.stop();
			this.activeStreams.delete(streamKey);
		}
	}

	/**
	 * Stop all streams for a contributor
	 */
	stopContributorStreams(contributorId: string): void {
		const keysToRemove: string[] = [];

		for (const [streamKey, stream] of this.activeStreams.entries()) {
			if (streamKey.startsWith(`${contributorId}_`)) {
				//console.log(`[STREAM-MANAGER] Stopping contributor stream: ${streamKey}`);
				stream.stop();
				keysToRemove.push(streamKey);
			}
		}

		keysToRemove.forEach((key) => this.activeStreams.delete(key));
	}

	/**
	 * Stop all streams
	 */
	stopAllStreams(): void {
		//console.log(`[STREAM-MANAGER] Stopping all ${this.subscriptionType} streams`);

		for (const [streamKey, stream] of this.activeStreams.entries()) {
			stream.stop();
		}

		this.activeStreams.clear();
		this.lastContributorsList = [];
	}

	/**
	 * Check if contributor lists are equal
	 */
	private arraysEqual(a: string[], b: string[]): boolean {
		if (a.length !== b.length) return false;
		const sortedA = [...a].sort();
		const sortedB = [...b].sort();
		return sortedA.every((val, i) => val === sortedB[i]);
	}

	/**
	 * Update subscriptions using delta-based approach with memoization
	 */
	async updateSubscriptions(
		newContributors: string[],
		createStreamFn: (contributorId: string) => Promise<void>
	): Promise<void> {
		// Prevent concurrent updates
		if (this.isUpdating) {
			//console.log(`[STREAM-MANAGER] ${this.subscriptionType} update already in progress, skipping`);
			return;
		}

		// Check if contributors list has actually changed
		if (this.arraysEqual(newContributors, this.lastContributorsList)) {
			/*console.log(
				`[STREAM-MANAGER] ${this.subscriptionType} contributors unchanged, skipping update`
			);*/
			return;
		}

		this.isUpdating = true;

		try {
			if (!newContributors.length) {
				/*console.log(
					`[STREAM-MANAGER] No ${this.subscriptionType} contributors, stopping all streams`
				);*/
				this.stopAllStreams();
				return;
			}

			/*console.log(
				`[STREAM-MANAGER] Updating ${this.subscriptionType} subscriptions for ${newContributors.length} contributors`
			);*/

			// Calculate current contributors from active streams
			const currentContributors = new Set<string>();
			for (const streamKey of this.activeStreams.keys()) {
				const contributorId = streamKey.split('_')[0];
				currentContributors.add(contributorId);
			}

			const newContributorSet = new Set(newContributors);
			const toAdd = newContributors.filter((id) => !currentContributors.has(id));
			const toRemove = Array.from(currentContributors).filter((id) => !newContributorSet.has(id));

			// Remove old streams for contributors no longer in the list
			for (const contributorId of toRemove) {
				//console.log(`[STREAM-MANAGER] Removing streams for contributor: ${contributorId}`);
				this.stopContributorStreams(contributorId);
			}

			// Add new streams for new contributors
			for (const contributorId of toAdd) {
				try {
					//console.log(`[STREAM-MANAGER] Adding streams for contributor: ${contributorId}`);
					await createStreamFn(contributorId);
				} catch (error) {
					/*console.error(
						`[STREAM-MANAGER] Failed to create streams for contributor ${contributorId}:`,
						error
					);*/
				}
			}

			// Update last contributors list
			this.lastContributorsList = [...newContributors];

			/*console.log(
				`[STREAM-MANAGER] ${this.subscriptionType} streams: +${toAdd.length} -${toRemove.length} (total: ${this.activeStreams.size})`
			);*/
		} finally {
			this.isUpdating = false;
		}
	}

	/**
	 * Get stream count for debugging
	 */
	get streamCount(): number {
		return this.activeStreams.size;
	}

	/**
	 * Get active stream keys for debugging
	 */
	get activeStreamKeys(): string[] {
		return Array.from(this.activeStreams.keys());
	}
}

// Create stream managers
const sogfStreamManager = new StreamSubscriptionManager('SOGF');
const mutualStreamManager = new StreamSubscriptionManager('MUTUAL');
const ownDataStreamManager = new StreamSubscriptionManager('OWN_DATA');
const chatStreamManager = new StreamSubscriptionManager('CHAT');

// Stream manager for collective member trees
const collectiveTreeStreamManager = new StreamSubscriptionManager('collective_tree');

// Stream configurations for collective member data
const collectiveMemberStreamConfigs = {
	tree: {
		type: 'tree',
		streamManager: collectiveTreeStreamManager,
		getGunPath: (userId: string, memberId?: string) => {
			if (!memberId) return null;
			const publicKey = resolveToPublicKey(memberId);
			if (!publicKey) {
				console.warn(
					`[NETWORK] Cannot create tree stream for collective member ${memberId}: no public key available`
				);
				return null;
			}
			return gun.user(publicKey).get('tree');
		},
		processor: (memberId: string) => (treeData: any) => {
			if (!treeData) {
				console.log(`[NETWORK] No tree data received for collective member ${memberId}`);
				return;
			}

			console.log(`[NETWORK] Received tree data for collective member ${memberId}`);

			// Validate the tree data
			const validatedTree = parseTree(treeData);
			if (!validatedTree) {
				console.warn(`[NETWORK] Invalid tree data from collective member ${memberId}`);
				return;
			}

			// Update the collective forest
			collectiveForest.update((forest) => {
				const newForest = new Map(forest);
				newForest.set(memberId, validatedTree);
				console.log(
					`[NETWORK] Updated forest with tree for member ${memberId}. Forest now has ${newForest.size} trees`
				);
				return newForest;
			});
		},
		errorHandler: (memberId: string) => (error: any) => {
			console.error(`[NETWORK] Error in collective member tree stream for ${memberId}:`, error);
		}
	}
};

// Function to create streams for a collective member
const createCollectiveMemberStreams = async (member: any) => {
	try {
		const userId = get(userPub);
		if (!userId) {
			console.warn('[NETWORK] Cannot create collective member streams - not authenticated');
			return;
		}

		// Extract member ID from Entity object
		const memberId = typeof member === 'string' ? member : member.id;
		await createStream(collectiveMemberStreamConfigs.tree, userId, memberId);
	} catch (error) {
		const memberId = typeof member === 'string' ? member : member.id;
		console.error(`[NETWORK] Error creating streams for collective member ${memberId}:`, error);
	}
};

// Helper to clean up multiple stores by filtering out removed contributors
function cleanupNetworkStores(
	removedContributors: string[],
	currentContributors: string[],
	stores: Array<{ store: any; name: string }>
) {
	if (removedContributors.length === 0) return;

	/*console.log(
		`[NETWORK] Cleaning up data for removed contributors: ${removedContributors.join(', ')}`
	);*/

	stores.forEach(({ store, name }) => {
		store.update((current: Record<string, any>) => {
			const cleaned: Record<string, any> = {};
			Object.entries(current).forEach(([contributorId, data]) => {
				if (currentContributors.includes(contributorId)) {
					cleaned[contributorId] = data;
				}
			});
			//console.log(`[NETWORK] Cleaned ${name}: kept ${Object.keys(cleaned).length} contributors`);
			return cleaned;
		});
	});
}

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
 * Generic data processor that handles validation, comparison, and store updates
 */
function createDataProcessor<T>(config: {
	dataType: string;
	validator?: (data: any) => T | null;
	getCurrentData: () => T | null;
	updateStore: (data: T) => void;
	loadingFlag?: { set: (value: boolean) => void };
	onUpdate?: () => void;
	emptyValue?: T;
}) {
	return (rawData: any) => {
		const { dataType, validator, getCurrentData, updateStore, loadingFlag, onUpdate, emptyValue } =
			config;

		if (!rawData) {
			//console.log(`[NETWORK] No ${dataType} data found`);
			if (emptyValue !== undefined) {
				updateStore(emptyValue);
			}
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
			}

			// Check if data has changed
			const currentData = getCurrentData();
			if (currentData && JSON.stringify(currentData) === JSON.stringify(processedData)) {
				//console.log(`[NETWORK] Incoming ${dataType} matches current ${dataType}, ignoring update`);
				loadingFlag?.set(false);
				return;
			}

			//console.log(`[NETWORK] ${dataType} data changed, updating local store`);
			updateStore(processedData);
			onUpdate?.();
		} catch (error) {
			//console.error(`[NETWORK] Error processing ${dataType}:`, error);
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
			getCurrentData: () => get(userCapacities),
			updateStore: (data) => userCapacities.set(data),
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
			validator: parseContacts,
			getCurrentData: () => get(userContacts),
			updateStore: (data) => {
				// Update the contacts store
				//console.log('[Network Contacts]', data);
				userContacts.set(data);
			},
			loadingFlag: isLoadingContacts,
			emptyValue: {}
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
			validator: parseUserSlotComposition,
			getCurrentData: () => get(userDesiredSlotComposeFrom),
			updateStore: (data) => userDesiredSlotComposeFrom.set(data),
			emptyValue: {}
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own desiredSlotComposeFrom stream:', error);
		}
	},
	desiredComposeInto: {
		type: 'desiredSlotComposeInto',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('desiredSlotComposeInto'),
		processor: createDataProcessor({
			dataType: 'desiredSlotComposeInto',
			validator: parseUserSlotComposition,
			getCurrentData: () => get(userDesiredSlotComposeInto),
			updateStore: (data) => userDesiredSlotComposeInto.set(data),
			emptyValue: {}
		}),
		errorHandler: (error: any) => {
			console.error('[NETWORK] Error in own desiredSlotComposeInto stream:', error);
		}
	},
	chatReadStates: {
		type: 'chatReadStates',
		streamManager: ownDataStreamManager,
		getGunPath: (userId: string) => user.get('chatReadStates'),
		processor: createDataProcessor({
			dataType: 'chatReadStates',
			validator: parseChatReadStates,
			getCurrentData: () => get(chatReadStates),
			updateStore: (data) => setChatReadStates(data),
			loadingFlag: isLoadingChatReadStates,
			emptyValue: {}
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

			const theirShare = validatedSogfData[ourId] || 0;
			const currentCache = get(recognitionCache);
			const existingEntry = currentCache[contributorId];
			const isUnchanged = existingEntry && existingEntry.theirShare === theirShare;

			if (!isUnchanged) {
				updateTheirShareFromNetwork(contributorId, theirShare);
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
				validator: parseCapacities,
				getCurrentData: () => get(networkCapacities)[contributorId] || {},
				updateStore: (data) => {
					networkCapacities.update((current) => ({
						...current,
						[contributorId]: data
					}));
				}
			}),
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(`[NETWORK] Error in capacities stream for ${contributorId}:`, error);
		}
	},
	capacityShares: {
		type: 'capacityShares',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('capacityShares').get(userId);
		},
		processor: (contributorId: string) => (shares: any) => {
			if (!shares) {
				//console.log(`[NETWORK] No capacity shares from contributor ${contributorId}`);
				networkCapacityShares.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(`[NETWORK] Received capacity shares update from stream for ${contributorId}`);

			// Validate capacity shares using parseCapacityShares
			const validatedShares = parseCapacityShares(shares);
			if (!validatedShares || Object.keys(validatedShares).length === 0) {
				console.warn(`[NETWORK] Invalid capacity shares from ${contributorId}`);
				return;
			}

			const currentNetworkShares = get(networkCapacityShares)[contributorId] || {};
			const isUnchanged = JSON.stringify(validatedShares) === JSON.stringify(currentNetworkShares);

			if (!isUnchanged) {
				console.log(
					`[NETWORK] Received new capacity shares from contributor ${contributorId}:`,
					validatedShares
				);

				networkCapacityShares.update((current) => ({
					...current,
					[contributorId]: validatedShares
				}));
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(`[NETWORK] Error in capacity shares stream for ${contributorId}:`, error);
		}
	},
	capacitySlotQuantities: {
		type: 'capacitySlotQuantities',
		streamManager: mutualStreamManager,
		getGunPath: (userId: string, contributorId: string) => {
			const pubKey = resolveToPublicKey(contributorId);
			if (!pubKey) return null;
			return gun.user(pubKey).get('capacitySlotQuantities').get(userId);
		},
		processor: (contributorId: string) => (slotQuantities: any) => {
			if (!slotQuantities) {
				console.log(`[NETWORK] No capacity slot quantities from contributor ${contributorId}`);
				networkCapacitySlotQuantities.update((current) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(
				`[NETWORK] Received capacity slot quantities update from stream for ${contributorId}`
			);

			// Validate capacity slot quantities using parseCapacitySlotQuantities
			const validatedQuantities = parseCapacitySlotQuantities(slotQuantities);
			if (!validatedQuantities || Object.keys(validatedQuantities).length === 0) {
				console.warn(`[NETWORK] Invalid capacity slot quantities from ${contributorId}`);
				return;
			}

			const currentNetworkQuantities = get(networkCapacitySlotQuantities)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedQuantities) === JSON.stringify(currentNetworkQuantities);

			if (!isUnchanged) {
				console.log(
					`[NETWORK] Received new capacity slot quantities from contributor ${contributorId}:`,
					validatedQuantities
				);

				networkCapacitySlotQuantities.update((current) => ({
					...current,
					[contributorId]: validatedQuantities
				}));
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(
				`[NETWORK] Error in capacity slot quantities stream for ${contributorId}:`,
				error
			);
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
			if (!composeFromData) {
				//console.log(`[NETWORK] No desired slot compose-from from contributor ${contributorId}`);
				networkDesiredSlotComposeFrom.update((current: NetworkSlotComposition) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			/*console.log(
				`[NETWORK] Received desired slot compose-from update from stream for ${contributorId}`
			);*/

			const validatedComposeFrom = parseUserSlotComposition(composeFromData);
			const currentNetworkComposeFrom = get(networkDesiredSlotComposeFrom)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeFrom) === JSON.stringify(currentNetworkComposeFrom);

			if (!isUnchanged) {
				networkDesiredSlotComposeFrom.update((current: NetworkSlotComposition) => ({
					...current,
					[contributorId]: validatedComposeFrom
				}));
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
			if (!composeIntoData) {
				//console.log(`[NETWORK] No desired slot compose-into from contributor ${contributorId}`);
				networkDesiredSlotComposeInto.update((current: NetworkSlotComposition) => {
					const { [contributorId]: _, ...rest } = current;
					return rest;
				});
				return;
			}

			console.log(
				`[NETWORK] Received desired slot compose-into update from stream for ${contributorId}`
			);

			const validatedComposeInto = parseUserSlotComposition(composeIntoData);
			const currentNetworkComposeInto = get(networkDesiredSlotComposeInto)[contributorId] || {};
			const isUnchanged =
				JSON.stringify(validatedComposeInto) === JSON.stringify(currentNetworkComposeInto);

			if (!isUnchanged) {
				networkDesiredSlotComposeInto.update((current: NetworkSlotComposition) => ({
					...current,
					[contributorId]: validatedComposeInto
				}));
			}
		},
		errorHandler: (contributorId: string) => (error: any) => {
			console.error(
				`[NETWORK] Error in desired slot compose-into stream for ${contributorId}:`,
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
			'capacityShares',
			'capacitySlotQuantities',
			'desiredSlotComposeFrom',
			'desiredSlotComposeInto'
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
		collectiveTreeStreamManager.stopAllStreams(); // Stop collective tree streams

		// Create new streams
		await createOwnTreeStream();
		await createOwnCapacitiesStream();
		await createOwnContactsStream();
		await createOwnDesiredSlotComposeFromStream();
		await createOwnDesiredSlotComposeIntoStream();
		await createOwnChatReadStatesStream();

		// Setup users list subscription (still using old approach for now)
		setupUsersListSubscription();

		// Initialize collective members and their tree streams
		await Promise.all(Array.from(get(collectiveMembers)).map(createCollectiveMemberStreams));

		console.log('[NETWORK] User data streams initialized successfully');
	} catch (error) {
		console.error('[NETWORK] Error initializing user data streams:', error);
	}
}

/**
 * Update the recognition cache with a contributor's share for us from network
 * @param contributorId The ID of the contributor (could be contact ID or public key)
 * @param theirShare Share they assign to us in their SOGF
 */
export function updateTheirShareFromNetwork(contributorId: string, theirShare: number) {
	console.log(`[NETWORK] Received share from ${contributorId}: ${theirShare.toFixed(4)}`);

	// CRITICAL: Always resolve to public key for unified cache storage
	// This ensures consistency with our calculation layer and prevents feedback loops
	const resolvedContributorId = resolveToPublicKey(contributorId) || contributorId;

	if (resolvedContributorId !== contributorId) {
		//console.log(`[NETWORK] Resolved ${contributorId} to ${resolvedContributorId}`);
	}

	//console.log(`[NETWORK-DEBUG] Current recognition cache before update:`, get(recognitionCache));

	// Get current cache entry using the resolved ID (consistent with calculation layer)
	const cache = get(recognitionCache);
	const existing = cache[resolvedContributorId];

	//console.log(`[NETWORK] Existing cache entry for ${resolvedContributorId}:`, existing);

	// Update the cache immediately with new theirShare using resolved public key
	recognitionCache.update((cache) => {
		if (existing) {
			// Update only theirShare in existing entry
			//console.log(`[NETWORK] Updating existing entry for ${resolvedContributorId}`);
			cache[resolvedContributorId] = {
				...cache[resolvedContributorId],
				theirShare,
				timestamp: Date.now()
			};
		} else {
			// Create new entry with default ourShare of 0, using resolved public key
			//console.log(`[NETWORK] Creating new entry for ${resolvedContributorId} with ourShare=0`);
			cache[resolvedContributorId] = {
				ourShare: 0, // We don't know our share yet
				theirShare,
				timestamp: Date.now()
			};
		}

		/*console.log(
			`[NETWORK] Updated cache entry for ${resolvedContributorId}:`,
			cache[resolvedContributorId]
		);*/
		return cache;
	});

	// Log the updated cache and force reactivity check
	const updatedCache = get(recognitionCache);

	/*console.log(
		`[NETWORK-DEBUG] Cache after network update from ${resolvedContributorId}:`,
		updatedCache
	);*/
}

// Centralized reactive subscription to usersList
function setupUsersListSubscription() {
	if (typeof window === 'undefined') return; // Only run in browser

	//console.log('[USERS] Setting up centralized usersList subscription');

	// Track current users to detect additions/removals
	const currentUsers = new Map<string, any>();

	// Subscribe to all changes in the usersList
	usersList.map().on((userData: any, pubKey: string) => {
		//console.log(`[USERS] User update: ${userId}`, userData);

		if (!pubKey || pubKey === '_') return; // Skip invalid keys

		if (userData === null || userData === undefined) {
			// User was removed from usersList (they went offline or left the shared space)
			//console.log(`[USERS] User removed from usersList: ${userId}`);
			currentUsers.delete(pubKey);

			// Note: We intentionally don't remove from userNamesCache here
			// because the cache serves a different purpose (performance optimization)
			// and this user might still be referenced in our tree as a contributor
		} else {
			// User was added or updated
			// console.log(`[USERS] User added/updated: ${userId}`, userData);
			currentUsers.set(pubKey, userData);

			// Get the user's alias and update cache
			const userName = userData.name;
			if (userName && typeof userName === 'string') {
				// Update userNamesCache with the name from usersList
				userAliasesCache.update((cache) => ({
					...cache,
					[pubKey]: userName
				}));
			} else {
				// Try to get alias from user's protected space using Gun's user system
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
						}
					});
			}
		}

		// Update userIds store with current user list
		const allUserIds = Array.from(currentUsers.keys());
		// console.log(`[USERS] Updating userIds store with ${allUserIds.length} users`);
		userPubKeys.set(allUserIds);
	});
}

// Derived store for all chat IDs we need to subscribe to
const chatIdsToSubscribe = derived(
	[userCapacities, userNetworkCapacitiesWithShares],
	([$userCapacities, $userNetworkCapacitiesWithShares]) => {
		const chatIds = new Set<string>();

		// Add all user capacity IDs (these are used as chat IDs)
		if ($userCapacities) {
			Object.keys($userCapacities).forEach((capacityId) => {
				chatIds.add(capacityId);
			});
		}

		// Add all network capacity IDs that we have shares in
		if ($userNetworkCapacitiesWithShares) {
			Object.keys($userNetworkCapacitiesWithShares).forEach((capacityId) => {
				chatIds.add(capacityId);
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
		networkCapacityShares.set({});
		networkDesiredSlotComposeFrom.set({});
		networkDesiredSlotComposeInto.set({});
		mutualStreamManager.stopAllStreams();
		return;
	}

	// Define network stores for cleanup
	const networkStores = [
		{ store: networkCapacities, name: 'networkCapacities' },
		{ store: networkCapacityShares, name: 'networkCapacityShares' },
		{ store: networkCapacitySlotQuantities, name: 'networkCapacitySlotQuantities' },
		{ store: networkDesiredSlotComposeFrom, name: 'networkDesiredComposeFrom' },
		{ store: networkDesiredSlotComposeInto, name: 'networkDesiredComposeInto' }
	];

	mutualStreamManager.updateSubscriptions(
		currentMutualContributors,
		createMutualContributorStreams
	);
}, 100);

mutualContributors.subscribe(debouncedUpdateMutualSubscriptions);

// Watch for changes to collective members and subscribe to their trees
const debouncedUpdateCollectiveSubscriptions = debounce((members: Array<any>) => {
	// Only run this if we're authenticated
	try {
		if (!userPub || !get(userPub)) {
			console.log('[NETWORK] Cannot subscribe to collective members - not authenticated');
			return;
		}
	} catch (error) {
		console.log('[NETWORK] Cannot subscribe to collective members - userPub not initialized');
		return;
	}

	if (!members.length) {
		// Clear the forest and stop all streams
		collectiveForest.set(new Map());
		collectiveTreeStreamManager.stopAllStreams();
		return;
	}

	// Update subscriptions using members directly (createCollectiveMemberStreams will extract IDs)
	collectiveTreeStreamManager.updateSubscriptions(
		members.map((m) => (typeof m === 'string' ? m : m.id)),
		(memberId) => createCollectiveMemberStreams(memberId)
	);
}, 100);

// Subscribe to collective members changes
collectiveMembers.subscribe(debouncedUpdateCollectiveSubscriptions);
