import { writable, derived, get } from 'svelte/store';
import type { Writable, Readable } from 'svelte/store';
import { gun, user } from './gun.svelte';

// Types
export interface RoomMessage {
	id: string;
	content: string;
	authorId: string;
	timestamp: number;
	roomId: string;
}

export interface Room {
	id: string;
	members: string[];
	createdAt?: number;
}

// Core stores
export const rooms = writable<Record<string, Room>>({});
export const messages = writable<Record<string, RoomMessage[]>>({});
export const activeRoom = writable<string | null>(null);

// Elegant derived stores
export const currentRoom = derived([activeRoom, rooms], ([roomId, allRooms]) =>
	roomId ? allRooms[roomId] : null
);

export const currentChat = derived([activeRoom, messages], ([roomId, allMessages]) =>
	roomId ? (allMessages[roomId] || []).sort((a, b) => a.timestamp - b.timestamp) : []
);

// Subscription management
const subscriptions = new Map<string, any>();

// Elegant membership strategies
export const MembershipStrategy = {
	union: (perspectives: string[][]): string[] => [...new Set(perspectives.flat())],
	intersection: (perspectives: string[][]): string[] => {
		if (!perspectives.length) return [];
		return perspectives.reduce((acc, curr) => acc.filter((id) => curr.includes(id)));
	},
	majority: (perspectives: string[][]): string[] => {
		const counts = new Map<string, number>();
		perspectives.forEach((p) => p.forEach((id) => counts.set(id, (counts.get(id) || 0) + 1)));
		const threshold = Math.ceil(perspectives.length / 2);
		return [...counts.entries()].filter(([_, count]) => count >= threshold).map(([id]) => id);
	},
	latest: (perspectives: string[][]): string[] => perspectives[perspectives.length - 1] || []
} as const;

// Elegant room builder
export class RoomBuilder {
	private config: { id: string; members: string[]; strategy?: keyof typeof MembershipStrategy } = {
		id: '',
		members: []
	};

	static create(id: string) {
		return new RoomBuilder().withId(id);
	}

	private withId(id: string) {
		this.config.id = id;
		return this;
	}

	withMembers(members: string[]) {
		this.config.members = members;
		return this;
	}

	withMembershipFrom(users: string[], strategy: keyof typeof MembershipStrategy = 'union') {
		this.config.strategy = strategy;
		return this.fetchMembership(users, strategy);
	}

	private fetchMembership(users: string[], strategy: keyof typeof MembershipStrategy) {
		const perspectives = users.map((userId) => getMembershipPerspective(this.config.id, userId));
		this.config.members = MembershipStrategy[strategy](perspectives.filter((p) => p.length > 0));
		return this;
	}

	manifest() {
		return manifestRoom(this.config.id, this.config.members);
	}

	build() {
		const room: Room = {
			id: this.config.id,
			members: this.config.members,
			createdAt: Date.now()
		};

		// Store room configuration
		if (user.is?.pub) {
			storeRoom(room);
		}

		// Update store and manifest
		rooms.update((r) => ({ ...r, [room.id]: room }));
		manifestRoom(room.id, room.members);

		return room;
	}
}

// Core elegant functions
export const Room = {
	// Create room with members
	create: (id: string) => RoomBuilder.create(id),

	// Join room by ID - discover members from any participant
	join: (roomId: string, seedUserId?: string) => {
		let roomData: Room | null = null;

		if (seedUserId) {
			// Try to get room info from seed user
			gun
				.get(`~${seedUserId}`)
				.get('rooms')
				.get(roomId)
				.once((data: any) => {
					if (data) {
						roomData = {
							id: data.id || roomId,
							members: data.members || data.pubKeys || [],
							createdAt: data.createdAt
						};
					}
				});
		}

		// If no seed user or no data found, create minimal room
		if (!roomData) {
			roomData = {
				id: roomId,
				members: seedUserId ? [seedUserId] : [],
				createdAt: Date.now()
			};
		}

		rooms.update((r) => ({ ...r, [roomId]: roomData! }));
		manifestRoom(roomId, roomData.members);
		activeRoom.set(roomId);

		return roomData;
	},

	// Core operations
	enter: (roomId: string) => activeRoom.set(roomId),
	leave: () => {
		const roomId = get(activeRoom);
		if (roomId) {
			cleanup(roomId);
			activeRoom.set(null);
		}
	},

	// Messaging
	send: (content: string, roomId?: string) => {
		const targetRoom = roomId || get(activeRoom);
		if (!targetRoom || !user.is?.pub) throw new Error('No room or user');

		const message: RoomMessage = {
			id: `${Date.now()}_${Math.random().toString(36).substr(2, 9)}`,
			content,
			authorId: user.is.pub,
			timestamp: Date.now(),
			roomId: targetRoom
		};

		gun
			.get(`~${user.is.pub}`)
			.get('rooms')
			.get(targetRoom)
			.get('messages')
			.get(message.id)
			.put(message);
	},

	// Utilities
	getMessages: (roomId: string) => get(messages)[roomId] || [],
	getCurrentRoom: () => get(currentRoom),
	getCurrentChat: () => get(currentChat),

	// Room discovery - find rooms by scanning known users
	discover: (userIds: string[]) => {
		const discoveredRooms: Record<string, Room> = {};

		userIds.forEach((userId) => {
			gun
				.get(`~${userId}`)
				.get('rooms')
				.map()
				.once((roomData: any, roomId: string) => {
					if (roomData && roomId) {
						discoveredRooms[roomId] = {
							id: roomId,
							members: roomData.members || roomData.pubKeys || [],
							createdAt: roomData.createdAt
						};
					}
				});
		});

		return discoveredRooms;
	}
};

// Elegant internal functions
function storeRoom(room: Room) {
	if (!user.is?.pub) return;
	gun.get(`~${user.is.pub}`).get('rooms').get(room.id).put({
		id: room.id,
		members: room.members,
		createdAt: room.createdAt
	});
}

function getMembershipPerspective(roomId: string, userId: string): string[] {
	let perspective: string[] = [];

	gun
		.get(`~${userId}`)
		.get('roomMembership')
		.get(roomId)
		.once((data: any) => {
			if (data) {
				perspective = data.members || data.pubKeys || [];
			}
		});

	return perspective;
}

function manifestRoom(roomId: string, members: string[]): void {
	console.log(`[ROOM] Manifesting ${roomId} with ${members.length} members`);

	// Cleanup existing
	cleanup(roomId);

	// Subscribe to all members
	members.forEach((memberId) => {
		const key = `${roomId}:${memberId}`;

		// Gun subscriptions are stored as the gun chain reference
		const subscription = gun
			.get(`~${memberId}`)
			.get('rooms')
			.get(roomId)
			.get('messages')
			.map()
			.on((messageData: any, messageId: string) => {
				if (!messageData?.content) return;

				const message: RoomMessage = {
					id: messageId,
					content: messageData.content,
					authorId: messageData.authorId,
					timestamp: messageData.timestamp,
					roomId: messageData.roomId || roomId
				};

				messages.update((m) => {
					const roomMessages = m[roomId] || [];
					if (roomMessages.some((msg) => msg.id === message.id)) return m;
					return { ...m, [roomId]: [...roomMessages, message] };
				});
			});

		// Store the subscription reference for cleanup
		subscriptions.set(key, subscription);
	});
}

function cleanup(roomId: string): void {
	// Remove subscriptions
	for (const [key, subscription] of subscriptions) {
		if (key.startsWith(`${roomId}:`)) {
			subscription.off();
			subscriptions.delete(key);
		}
	}

	// Clear messages
	messages.update((m) => {
		const { [roomId]: _, ...rest } = m;
		return rest;
	});
}

// Elegant membership management
export const Membership = {
	update: (roomId: string, members: string[]) => {
		if (!user.is?.pub) return;

		gun.get(`~${user.is.pub}`).get('roomMembership').get(roomId).put({
			userId: user.is.pub,
			roomId,
			members,
			lastUpdated: Date.now()
		});

		// Update local room
		rooms.update((r) => {
			if (r[roomId]) {
				return { ...r, [roomId]: { ...r[roomId], members } };
			}
			return r;
		});
	},

	subscribe: (
		roomId: string,
		userIds: string[],
		strategy: keyof typeof MembershipStrategy = 'union'
	) => {
		const perspectives: string[][] = [];

		userIds.forEach((userId, index) => {
			gun
				.get(`~${userId}`)
				.get('roomMembership')
				.get(roomId)
				.on((data: any) => {
					if (!data) return;

					const members = data.members || data.pubKeys || [];
					perspectives[index] = members;

					// Re-aggregate and re-manifest
					const aggregated = MembershipStrategy[strategy](
						perspectives.filter((p) => p && p.length > 0)
					);
					if (aggregated.length > 0) {
						rooms.update((r) => ({
							...r,
							[roomId]: {
								id: roomId,
								members: aggregated,
								createdAt: r[roomId]?.createdAt || Date.now()
							}
						}));
						manifestRoom(roomId, aggregated);
					}
				});
		});
	},

	// Get aggregated membership from multiple perspectives
	aggregate: (
		roomId: string,
		userIds: string[],
		strategy: keyof typeof MembershipStrategy = 'union'
	) => {
		const perspectives = userIds.map((userId) => getMembershipPerspective(roomId, userId));
		return MembershipStrategy[strategy](perspectives.filter((p) => p.length > 0));
	}
};

// Export elegant API
export default Room;
