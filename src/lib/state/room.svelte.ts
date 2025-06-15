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

// Stores
export const currentRoom: Writable<RoomInfo | null> = writable(null);
export const roomMessages: Writable<Record<string, RoomMessage[]>> = writable({});
export const myRoomMessages: Writable<Record<string, RoomMessage[]>> = writable({});

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

			// Set up message subscriptions for all participants
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
 * Manifest a room by setting up subscriptions to messages from specified public keys
 * @param roomId The room to manifest
 * @param pubKeys Array of public keys to subscribe to for messages
 */
export function manifestRoom(roomId: string, pubKeys: string[]): void {
	console.log(`[ROOM] Manifesting room ${roomId} with ${pubKeys.length} participants`);

	// Clear existing subscriptions for this room
	const existingKey = `${roomId}:*`;
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
