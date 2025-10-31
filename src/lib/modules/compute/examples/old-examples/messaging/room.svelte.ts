import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { gun, user, isAuthenticating } from './gun.svelte';

// Types
export interface RoomMessage {
	id: string;
	content: string;
	authorId: string;
	timestamp: number;
	roomId: string;
}

export interface RoomInfo {
	id: string;
	pubKeys: string[];
	hostId: string;
	createdAt: number;
}

export interface MembershipPerspective {
	userId: string;
	roomId: string;
	pubKeys: string[];
	lastUpdated: number;
}

// Stores
export const currentRoom: Writable<RoomInfo | null> = writable(null);
export const roomMessages: Writable<Record<string, RoomMessage[]>> = writable({});
export const myRoomMessages: Writable<Record<string, RoomMessage[]>> = writable({});
export const membershipPerspectives: Writable<
	Record<string, Record<string, MembershipPerspective>>
> = writable({});

// Track active subscriptions to avoid duplicates
const activeSubscriptions = new Set<string>();

/**
 * Derived store that combines messages from all subscribed users for the current room
 */
export const currentRoomChat: Readable<RoomMessage[]> = derived(
	[currentRoom, roomMessages],
	([room, messages]) => {
		if (!room) return [];

		const roomMessagesList = messages[room.id] || [];

		// Sort messages by timestamp
		return roomMessagesList.sort((a, b) => a.timestamp - b.timestamp);
	}
);

/**
 * Get messages for a specific room (flexible approach)
 */
export function getRoomMessages(roomId: string): RoomMessage[] {
	const messages = get(roomMessages);
	const roomMessagesList = messages[roomId] || [];
	return roomMessagesList.sort((a, b) => a.timestamp - b.timestamp);
}

/**
 * Create or host a new room with specified public keys
 * @param roomId Unique identifier for the room
 * @param pubKeys Array of public keys that can participate in the room
 */
export function createRoom(roomId: string, pubKeys: string[]): Promise<void> {
	return new Promise((resolve, reject) => {
		const ourId = user.is?.pub;
		if (!ourId) {
			reject(new Error('Cannot create room - not authenticated'));
			return;
		}

		console.log(`[ROOM] Creating room ${roomId} with ${pubKeys.length} participants`);

		const roomInfo: RoomInfo = {
			id: roomId,
			pubKeys,
			hostId: ourId,
			createdAt: Date.now()
		};

		// Store room info in our user space
		gun
			.get(`~${ourId}`)
			.get('rooms')
			.get(roomId)
			.put(roomInfo, (ack: any) => {
				if (ack.err) {
					console.error(`[ROOM] Error creating room ${roomId}:`, ack.err);
					reject(new Error(ack.err));
				} else {
					console.log(`[ROOM] Successfully created room ${roomId}`);
					currentRoom.set(roomInfo);
					resolve();
				}
			});
	});
}

/**
 * Store our perspective of room membership
 * @param roomId The room ID
 * @param pubKeys Our view of who should be in the room
 */
export function updateMembershipPerspective(roomId: string, pubKeys: string[]): void {
	const ourId = user.is?.pub;
	if (!ourId) {
		console.log('[ROOM] Cannot update membership perspective - not authenticated');
		return;
	}

	const perspective: MembershipPerspective = {
		userId: ourId,
		roomId,
		pubKeys,
		lastUpdated: Date.now()
	};

	console.log(`[ROOM] Updating our membership perspective for room ${roomId}:`, pubKeys);

	// Store our perspective
	gun
		.get(`~${ourId}`)
		.get('roomMembership')
		.get(roomId)
		.put(perspective, (ack: any) => {
			if (ack.err) {
				console.error(`[ROOM] Error updating membership perspective:`, ack.err);
			} else {
				console.log(`[ROOM] Successfully updated membership perspective for room ${roomId}`);
			}
		});

	// Update local store
	membershipPerspectives.update((current) => {
		const roomPerspectives = current[roomId] || {};
		return {
			...current,
			[roomId]: {
				...roomPerspectives,
				[ourId]: perspective
			}
		};
	});
}

/**
 * Subscribe to membership perspectives from multiple users
 * @param roomId The room to track
 * @param userIds Array of user IDs whose perspectives to track
 */
export function subscribeMembershipPerspectives(roomId: string, userIds: string[]): void {
	console.log(
		`[ROOM] Subscribing to membership perspectives for room ${roomId} from ${userIds.length} users`
	);

	userIds.forEach((userId) => {
		const subscriptionKey = `membership:${roomId}:${userId}`;

		if (activeSubscriptions.has(subscriptionKey)) {
			console.log(`[ROOM] Already subscribed to membership perspective from ${userId}`);
			return;
		}

		console.log(`[ROOM] Setting up membership perspective subscription from ${userId}`);
		activeSubscriptions.add(subscriptionKey);

		gun
			.get(`~${userId}`)
			.get('roomMembership')
			.get(roomId)
			.on((perspectiveData: any) => {
				if (!perspectiveData || !perspectiveData.pubKeys) return;

				const perspective: MembershipPerspective = {
					userId: perspectiveData.userId || userId,
					roomId: perspectiveData.roomId || roomId,
					pubKeys: perspectiveData.pubKeys || [],
					lastUpdated: perspectiveData.lastUpdated || Date.now()
				};

				console.log(`[ROOM] Received membership perspective from ${userId}:`, perspective);

				// Update local store
				membershipPerspectives.update((current) => {
					const roomPerspectives = current[roomId] || {};
					return {
						...current,
						[roomId]: {
							...roomPerspectives,
							[userId]: perspective
						}
					};
				});
			});
	});
}

/**
 * Aggregate membership from multiple perspectives using different strategies
 */
export type MembershipStrategy = 'union' | 'intersection' | 'majority' | 'recent';

export function aggregateMembership(
	roomId: string,
	strategy: MembershipStrategy = 'union'
): string[] {
	const perspectives = get(membershipPerspectives)[roomId] || {};
	const perspectivesList = Object.values(perspectives);

	if (perspectivesList.length === 0) return [];

	console.log(
		`[ROOM] Aggregating membership for room ${roomId} using ${strategy} strategy from ${perspectivesList.length} perspectives`
	);

	switch (strategy) {
		case 'union': {
			// Include anyone mentioned by anyone
			const allPubKeys = new Set<string>();
			perspectivesList.forEach((p) => p.pubKeys.forEach((pk) => allPubKeys.add(pk)));
			return Array.from(allPubKeys);
		}

		case 'intersection': {
			// Only include those mentioned by everyone
			if (perspectivesList.length === 1) return perspectivesList[0].pubKeys;

			const first = new Set(perspectivesList[0].pubKeys);
			perspectivesList.slice(1).forEach((p) => {
				const current = new Set(p.pubKeys);
				first.forEach((pk) => {
					if (!current.has(pk)) first.delete(pk);
				});
			});
			return Array.from(first);
		}

		case 'majority': {
			// Include those mentioned by >50% of perspectives
			const counts = new Map<string, number>();
			perspectivesList.forEach((p) => {
				p.pubKeys.forEach((pk) => {
					counts.set(pk, (counts.get(pk) || 0) + 1);
				});
			});

			const threshold = Math.ceil(perspectivesList.length / 2);
			return Array.from(counts.entries())
				.filter(([_, count]) => count >= threshold)
				.map(([pubkey, _]) => pubkey);
		}

		case 'recent': {
			// Use the most recently updated perspective
			const mostRecent = perspectivesList.reduce((latest, current) =>
				current.lastUpdated > latest.lastUpdated ? current : latest
			);
			return mostRecent.pubKeys;
		}

		default:
			return perspectivesList[0]?.pubKeys || [];
	}
}

/**
 * Flexible manifest function - subscribes to messages from specified public keys
 * @param roomId The room identifier (for organization)
 * @param pubKeys Array of public keys to subscribe to for messages
 * @param clearExisting Whether to clear existing subscriptions for this room
 */
export function manifestRoom(
	roomId: string,
	pubKeys: string[],
	clearExisting: boolean = true
): void {
	console.log(`[ROOM] Manifesting room ${roomId} with ${pubKeys.length} participants`);

	if (clearExisting) {
		// Clear existing subscriptions for this room
		activeSubscriptions.forEach((key) => {
			if (key.startsWith(`${roomId}:`)) {
				activeSubscriptions.delete(key);
			}
		});

		// Clear existing messages for this room
		roomMessages.update((current) => {
			const { [roomId]: _, ...rest } = current;
			return rest;
		});
	}

	// Set up subscriptions for each participant
	pubKeys.forEach((pubKey) => {
		const subscriptionKey = `${roomId}:${pubKey}`;

		if (activeSubscriptions.has(subscriptionKey)) {
			console.log(`[ROOM] Already subscribed to ${pubKey} for room ${roomId}`);
			return;
		}

		console.log(`[ROOM] Setting up subscription to ${pubKey} for room ${roomId}`);
		activeSubscriptions.add(subscriptionKey);

		// Subscribe to messages from this user for this room
		gun
			.get(`~${pubKey}`)
			.get('rooms')
			.get(roomId)
			.get('messages')
			.map()
			.on((messageData: any, messageId: string) => {
				if (!messageData) return;

				// Validate message structure
				if (!messageData.content || !messageData.authorId || !messageData.timestamp) {
					console.warn(`[ROOM] Invalid message structure from ${pubKey}:`, messageData);
					return;
				}

				const message: RoomMessage = {
					id: messageId,
					content: messageData.content,
					authorId: messageData.authorId,
					timestamp: messageData.timestamp,
					roomId: messageData.roomId || roomId
				};

				console.log(`[ROOM] Received message from ${pubKey} for room ${roomId}:`, message);

				// Update room messages store
				roomMessages.update((current) => {
					const existingMessages = current[roomId] || [];

					// Check if message already exists (avoid duplicates)
					const messageExists = existingMessages.some((m) => m.id === message.id);
					if (messageExists) {
						return current;
					}

					return {
						...current,
						[roomId]: [...existingMessages, message]
					};
				});
			});
	});
}

/**
 * Manifest room using aggregated membership from multiple perspectives
 * @param roomId The room to manifest
 * @param perspectiveUsers Users whose membership perspectives to consider
 * @param strategy How to aggregate the different perspectives
 */
export function manifestRoomFromPerspectives(
	roomId: string,
	perspectiveUsers: string[],
	strategy: MembershipStrategy = 'union'
): void {
	console.log(
		`[ROOM] Manifesting room ${roomId} from ${perspectiveUsers.length} user perspectives using ${strategy} strategy`
	);

	// First subscribe to all the membership perspectives
	subscribeMembershipPerspectives(roomId, perspectiveUsers);

	// Wait a moment for perspectives to load, then aggregate and manifest
	setTimeout(() => {
		const aggregatedPubKeys = aggregateMembership(roomId, strategy);
		console.log(
			`[ROOM] Aggregated ${aggregatedPubKeys.length} members for room ${roomId}:`,
			aggregatedPubKeys
		);

		if (aggregatedPubKeys.length > 0) {
			manifestRoom(roomId, aggregatedPubKeys);
		}
	}, 1000); // Give perspectives time to load

	// Set up reactive updates when perspectives change
	membershipPerspectives.subscribe((allPerspectives) => {
		const roomPerspectives = allPerspectives[roomId];
		if (roomPerspectives) {
			const newAggregatedPubKeys = aggregateMembership(roomId, strategy);
			const currentMessages = get(roomMessages)[roomId] || [];

			// Only re-manifest if membership actually changed
			const existingPubKeys = [...new Set(currentMessages.map((m) => m.authorId))];
			const hasChanged =
				JSON.stringify(newAggregatedPubKeys.sort()) !== JSON.stringify(existingPubKeys.sort());

			if (hasChanged && newAggregatedPubKeys.length > 0) {
				console.log(`[ROOM] Membership perspectives changed, re-manifesting room ${roomId}`);
				manifestRoom(roomId, newAggregatedPubKeys);
			}
		}
	});
}

/**
 * Join a room by subscribing to its info and setting up message subscriptions
 * @param roomId The room to join
 * @param hostId The public key of the room host
 */
export function joinRoom(roomId: string, hostId: string): void {
	console.log(`[ROOM] Joining room ${roomId} hosted by ${hostId}`);

	// Subscribe to room info from host
	gun
		.get(`~${hostId}`)
		.get('rooms')
		.get(roomId)
		.on((roomData: any) => {
			if (!roomData) {
				console.log(`[ROOM] No room data found for ${roomId}`);
				return;
			}

			console.log(`[ROOM] Received room info for ${roomId}:`, roomData);

			const roomInfo: RoomInfo = {
				id: roomData.id,
				pubKeys: roomData.pubKeys || [],
				hostId: roomData.hostId,
				createdAt: roomData.createdAt
			};

			currentRoom.set(roomInfo);

			// Set up message subscriptions for all participants using flexible manifest
			manifestRoom(roomId, roomInfo.pubKeys);
		});
}

/**
 * Post a message to a room in our user space
 * @param roomId The room to post to
 * @param content The message content
 */
export function postMessageToRoom(roomId: string, content: string): Promise<void> {
	return new Promise((resolve, reject) => {
		const ourId = user.is?.pub;
		if (!ourId) {
			reject(new Error('Cannot post message - not authenticated'));
			return;
		}

		const message: RoomMessage = {
			id: `${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
			content,
			authorId: ourId,
			timestamp: Date.now(),
			roomId
		};

		console.log(`[ROOM] Posting message to room ${roomId}:`, message);

		// Store message in our user space under rooms/roomId/messages
		gun
			.get(`~${ourId}`)
			.get('rooms')
			.get(roomId)
			.get('messages')
			.get(message.id)
			.put(message, (ack: any) => {
				if (ack.err) {
					console.error(`[ROOM] Error posting message to room ${roomId}:`, ack.err);
					reject(new Error(ack.err));
				} else {
					console.log(`[ROOM] Successfully posted message to room ${roomId}`);

					// Update our local store
					myRoomMessages.update((current) => {
						const roomMessages = current[roomId] || [];
						return {
							...current,
							[roomId]: [...roomMessages, message]
						};
					});

					resolve();
				}
			});
	});
}

/**
 * Leave the current room and clean up subscriptions
 */
export function leaveRoom(): void {
	const room = get(currentRoom);
	if (!room) {
		console.log('[ROOM] No room to leave');
		return;
	}

	console.log(`[ROOM] Leaving room ${room.id}`);

	// Clear subscriptions
	activeSubscriptions.forEach((key) => {
		if (key.startsWith(`${room.id}:`)) {
			activeSubscriptions.delete(key);
		}
	});

	// Clear room state
	currentRoom.set(null);

	// Clear room messages
	roomMessages.update((current) => {
		const { [room.id]: _, ...rest } = current;
		return rest;
	});
}

/**
 * Leave a specific room by ID (flexible approach)
 */
export function leaveRoomById(roomId: string): void {
	console.log(`[ROOM] Leaving room ${roomId}`);

	// Clear subscriptions for this room
	activeSubscriptions.forEach((key) => {
		if (key.startsWith(`${roomId}:`)) {
			activeSubscriptions.delete(key);
		}
	});

	// Clear room messages
	roomMessages.update((current) => {
		const { [roomId]: _, ...rest } = current;
		return rest;
	});

	// Clear current room if it matches
	const currentRoomState = get(currentRoom);
	if (currentRoomState?.id === roomId) {
		currentRoom.set(null);
	}
}

/**
 * Get all rooms we've created or joined
 */
export function getMyRooms(): Promise<RoomInfo[]> {
	return new Promise((resolve, reject) => {
		const ourId = user.is?.pub;
		if (!ourId) {
			reject(new Error('Cannot get rooms - not authenticated'));
			return;
		}

		const rooms: RoomInfo[] = [];

		gun
			.get(`~${ourId}`)
			.get('rooms')
			.map()
			.on((roomData: any, roomId: string) => {
				if (!roomData) return;

				const roomInfo: RoomInfo = {
					id: roomId,
					pubKeys: roomData.pubKeys || [],
					hostId: roomData.hostId,
					createdAt: roomData.createdAt
				};

				// Add to list if not already present
				const existingIndex = rooms.findIndex((r) => r.id === roomId);
				if (existingIndex >= 0) {
					rooms[existingIndex] = roomInfo;
				} else {
					rooms.push(roomInfo);
				}
			});

		// Return rooms after a brief delay to allow data to load
		setTimeout(() => resolve(rooms), 500);
	});
}
