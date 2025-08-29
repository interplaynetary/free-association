import { writable, derived, get } from 'svelte/store';
import { gun } from '../gun.svelte';
import JSOG from 'jsog';
import { v4 as uuidv4 } from 'uuid';
import {
	BoardSchema,
	RoomSchema,
	NoteSchema,
	PlacementSchema,
	UnconferenceUserSchema,
	RoomsMapSchema,
	NotesMapSchema,
	PlacementsMapSchema,
	UsersMapSchema,
	type Board,
	type Room,
	type Note,
	type Placement,
	type UnconferenceUser,
	type RoomsMap,
	type NotesMap,
	type PlacementsMap,
	type UsersMap
} from '$lib/schema/unconference';

// Step 2: Create Writeables for Data Sources

// User Data (Writeables) - data the current user owns
export const userBoard = writable<Board>({
	id: '',
	title: 'Unconference Schedule',
	gridStartTime: new Date().setHours(9, 0, 0, 0),
	gridEndTime: new Date().setHours(17, 0, 0, 0),
	slotDurationMinutes: 30
});

export const userRooms = writable<RoomsMap>({});
export const userNotes = writable<NotesMap>({});
export const userPlacements = writable<PlacementsMap>({});

// Network Data (Writeables) - data from other participants
export const networkRooms = writable<RoomsMap>({});
export const networkNotes = writable<NotesMap>({});
export const networkPlacements = writable<PlacementsMap>({});
export const networkUsers = writable<UsersMap>({});

// Step 6: Loading States for Race Condition Prevention
export const isLoadingBoard = writable(false);
export const isLoadingRooms = writable(false);
export const isLoadingNotes = writable(false);
export const isLoadingPlacements = writable(false);
export const isLoadingUsers = writable(false);

// Track active subscriptions for cleanup
let subscriptions: Array<() => void> = [];

// User identity
const userId = (() => {
	const stored = localStorage.getItem('unconferenceUserId');
	if (stored) return stored;
	const newId = uuidv4();
	localStorage.setItem('unconferenceUserId', newId);
	return newId;
})();

const userName = (() => {
	const stored = localStorage.getItem('unconferenceUserName');
	if (stored) return stored;
	const name = `User ${userId.slice(0, 8)}`;
	localStorage.setItem('unconferenceUserName', name);
	return name;
})();

// Current user object
export const currentUser = writable<UnconferenceUser>({
	id: userId,
	name: userName,
	cursorColor: generateUserColor(),
	isOrganizer: false,
	joinedAt: Date.now()
});

// Step 4: Create Derived Stores from Writeables

// Merge all rooms (both user and network)
export const allRooms = derived(
	[userRooms, networkRooms],
	([$userRooms, $networkRooms]) => {
		// Merge with network rooms taking precedence for conflicts
		const merged = { ...$userRooms };
		Object.entries($networkRooms).forEach(([id, room]) => {
			merged[id] = room;
		});
		return merged;
	}
);

// Merge all notes (both user and network)
export const allNotes = derived(
	[userNotes, networkNotes],
	([$userNotes, $networkNotes]) => {
		// Merge with network notes taking precedence for conflicts
		const merged = { ...$userNotes };
		Object.entries($networkNotes).forEach(([id, note]) => {
			merged[id] = note;
		});
		return merged;
	}
);

// Merge all placements with conflict resolution
export const allPlacements = derived(
	[userPlacements, networkPlacements],
	([$userPlacements, $networkPlacements]) => {
		// Merge with network placements taking precedence
		const merged = { ...$userPlacements };
		Object.entries($networkPlacements).forEach(([id, placement]) => {
			merged[id] = placement;
		});
		return merged;
	}
);

// Get unplaced notes
export const unplacedNotes = derived(
	[allNotes, allPlacements],
	([$allNotes, $allPlacements]) => {
		const placed = new Set(Object.values($allPlacements).map(p => p.noteId));
		return Object.values($allNotes)
			.filter(note => !placed.has(note.id))
			.sort((a, b) => b.createdAt - a.createdAt);
	}
);

// Time slots calculation
export const timeSlots = derived(userBoard, ($board) => {
	const slots = [];
	const start = $board.gridStartTime;
	const end = $board.gridEndTime;
	const duration = $board.slotDurationMinutes * 60 * 1000;
	
	for (let time = start; time < end; time += duration) {
		slots.push(new Date(time));
	}
	
	return slots;
});

// Sorted rooms
export const sortedRooms = derived(allRooms, ($rooms) => {
	return Object.values($rooms).sort((a, b) => a.order - b.order);
});

// Step 3: Set Up Gun-Path Subscriptions
let boardRef: any = null;

// Cleanup function for subscriptions
export function cleanup() {
	subscriptions.forEach(unsub => unsub());
	subscriptions = [];
}

export function initializeBoard(boardId: string) {
	// Cleanup any existing subscriptions
	cleanup();
	
	// Update board ID
	userBoard.update(b => ({ ...b, id: boardId }));
	
	// Get board reference
	boardRef = gun.get('unconference').get(boardId);
	
	// Subscribe to board data
	const boardSub = boardRef.get('board').on((data: any) => {
		console.log('Board data received:', data);
		
		if (!data || data === 'null' || typeof data !== 'object') {
			return;
		}
		
		try {
			// Handle both encoded and plain objects
			const decoded = typeof data === 'string' ? JSOG.decode(JSON.parse(data)) : data;
			const validated = BoardSchema.parse(decoded);
			
			userBoard.set(validated);
		} catch (error) {
			console.error('Invalid board data:', error, data);
		}
	});
	
	// Subscribe to rooms with proper map handling
	const roomsSub = boardRef.get('rooms').map().on((data: any, id: string) => {
		console.log('Room data received:', id, data);
		
		if (!data || data === 'null') {
			// Remove deleted room
			networkRooms.update(rooms => {
				const updated = { ...rooms };
				delete updated[id];
				return updated;
			});
			userRooms.update(rooms => {
				const updated = { ...rooms };
				delete updated[id];
				return updated;
			});
			return;
		}
		
		try {
			// Handle both encoded and plain objects
			const decoded = typeof data === 'string' ? JSOG.decode(JSON.parse(data)) : data;
			const validated = RoomSchema.parse(decoded);
			
			// Update the appropriate store based on ownership
			if (decoded.createdBy === userId) {
				userRooms.update(rooms => ({
					...rooms,
					[id]: validated
				}));
			} else {
				networkRooms.update(rooms => ({
					...rooms,
					[id]: validated
				}));
			}
		} catch (error) {
			console.error('Invalid room data:', error, data);
		}
	});
	
	// Subscribe to notes
	const notesSub = boardRef.get('notes').map().on((data: any, id: string) => {
		console.log('Note data received:', id, data);
		
		if (!data || data === 'null') {
			// Remove deleted note
			networkNotes.update(notes => {
				const updated = { ...notes };
				delete updated[id];
				return updated;
			});
			userNotes.update(notes => {
				const updated = { ...notes };
				delete updated[id];
				return updated;
			});
			return;
		}
		
		try {
			// Handle both encoded and plain objects
			const decoded = typeof data === 'string' ? JSOG.decode(JSON.parse(data)) : data;
			const validated = NoteSchema.parse(decoded);
			
			// Update the appropriate store based on ownership
			if (validated.authorId === userId) {
				userNotes.update(notes => ({
					...notes,
					[id]: validated
				}));
			} else {
				networkNotes.update(notes => ({
					...notes,
					[id]: validated
				}));
			}
		} catch (error) {
			console.error('Invalid note data:', error, data);
		}
	});
	
	// Subscribe to placements
	const placementsSub = boardRef.get('placements').map().on((data: any, id: string) => {
		console.log('Placement data received:', id, data);
		
		if (!data || data === 'null') {
			// Remove deleted placement
			networkPlacements.update(placements => {
				const updated = { ...placements };
				delete updated[id];
				return updated;
			});
			userPlacements.update(placements => {
				const updated = { ...placements };
				delete updated[id];
				return updated;
			});
			return;
		}
		
		try {
			// Handle both encoded and plain objects
			const decoded = typeof data === 'string' ? JSOG.decode(JSON.parse(data)) : data;
			const validated = PlacementSchema.parse(decoded);
			
			// Update the appropriate store based on ownership
			if (validated.placedBy === userId) {
				userPlacements.update(placements => ({
					...placements,
					[id]: validated
				}));
			} else {
				networkPlacements.update(placements => ({
					...placements,
					[id]: validated
				}));
			}
		} catch (error) {
			console.error('Invalid placement data:', error, data);
		}
	});
	
	// Subscribe to users
	const usersSub = boardRef.get('users').map().on((data: any, id: string) => {
		console.log('User data received:', id, data);
		
		if (!data || data === 'null') {
			networkUsers.update(users => {
				const updated = { ...users };
				delete updated[id];
				return updated;
			});
			return;
		}
		
		try {
			// Handle both encoded and plain objects
			const decoded = typeof data === 'string' ? JSOG.decode(JSON.parse(data)) : data;
			const validated = UnconferenceUserSchema.parse(decoded);
			
			networkUsers.update(users => ({
				...users,
				[id]: validated
			}));
		} catch (error) {
			console.error('Invalid user data:', error, data);
		}
	});
	
	// Register current user
	setTimeout(() => registerUser(), 100);
	
	// Initialize default rooms if none exist after a delay
	setTimeout(() => {
		const rooms = get(allRooms);
		if (Object.keys(rooms).length === 0) {
			initializeDefaultRooms();
		}
	}, 2000);
}

// Step 5: Determine Network Persistence Points

export function persistBoard(board: Board) {
	if (!boardRef) return;
	
	// Don't use JSOG.encode, just put the plain object
	boardRef.get('board').put(board);
}

export function addRoom(room: Room) {
	if (!boardRef) return;
	
	// Add createdBy field for ownership tracking
	const roomWithOwner = { ...room, createdBy: userId };
	boardRef.get('rooms').get(room.id).put(roomWithOwner);
}

export function removeRoom(roomId: string) {
	if (!boardRef) return;
	
	boardRef.get('rooms').get(roomId).put(null);
	
	// Remove all placements in this room
	const placements = get(allPlacements);
	Object.entries(placements).forEach(([id, placement]) => {
		if (placement.roomId === roomId) {
			removePlacement(id);
		}
	});
}

export function createNote(content: string, color: string): string {
	if (!boardRef) return '';
	
	const note: Note = {
		id: uuidv4(),
		content,
		color,
		authorId: userId,
		authorName: userName,
		createdAt: Date.now(),
		updatedAt: Date.now()
	};
	
	// Put the plain object, not encoded
	boardRef.get('notes').get(note.id).put(note);
	
	return note.id;
}

export function updateNote(noteId: string, content: string) {
	if (!boardRef) return;
	
	const notes = get(allNotes);
	const note = notes[noteId];
	if (!note) return;
	
	const updated: Note = {
		...note,
		content,
		updatedAt: Date.now()
	};
	
	boardRef.get('notes').get(noteId).put(updated);
}

export function deleteNote(noteId: string) {
	if (!boardRef) return;
	
	boardRef.get('notes').get(noteId).put(null);
	
	// Remove placement if exists
	const placements = get(allPlacements);
	Object.entries(placements).forEach(([id, placement]) => {
		if (placement.noteId === noteId) {
			removePlacement(id);
		}
	});
}

export function placeNote(noteId: string, roomId: string, slotIndex: number, spanSlots: number = 1): boolean {
	if (!boardRef) return false;
	
	// Check for conflicts
	if (hasConflict(roomId, slotIndex, spanSlots, noteId)) {
		return false;
	}
	
	// Remove existing placement for this note
	const placements = get(allPlacements);
	Object.entries(placements).forEach(([id, placement]) => {
		if (placement.noteId === noteId) {
			removePlacement(id);
		}
	});
	
	const placement: Placement = {
		noteId,
		roomId,
		slotIndex,
		spanSlots,
		placedBy: userId,
		placedAt: Date.now()
	};
	
	const placementId = `${noteId}_${roomId}_${slotIndex}`;
	boardRef.get('placements').get(placementId).put(placement);
	
	return true;
}

export function removePlacement(placementId: string) {
	if (!boardRef) return;
	
	boardRef.get('placements').get(placementId).put(null);
}

function registerUser() {
	if (!boardRef) return;
	
	const user = get(currentUser);
	boardRef.get('users').get(userId).put(user);
}

// Helper functions

function generateUserColor(): string {
	const colors = [
		'#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4',
		'#FFEAA7', '#DDA0DD', '#98D8C8', '#FFB6C1'
	];
	return colors[Math.floor(Math.random() * colors.length)];
}

function initializeDefaultRooms() {
	const defaultRooms = [
		'Main Hall',
		'Room A', 
		'Room B',
		'Workshop Space',
		'Breakout Room'
	];
	
	defaultRooms.forEach((name, index) => {
		const room: Room = {
			id: uuidv4(),
			name,
			capacity: 30,
			order: index
		};
		addRoom(room);
	});
}

export function hasConflict(roomId: string, slotIndex: number, spanSlots: number, excludeNoteId?: string): boolean {
	const placements = get(allPlacements);
	
	for (const placement of Object.values(placements)) {
		if (placement.roomId !== roomId) continue;
		if (excludeNoteId && placement.noteId === excludeNoteId) continue;
		
		const placementStart = placement.slotIndex;
		const placementEnd = placement.slotIndex + placement.spanSlots;
		const newStart = slotIndex;
		const newEnd = slotIndex + spanSlots;
		
		if (newStart < placementEnd && newEnd > placementStart) {
			return true;
		}
	}
	
	return false;
}

export function getPlacementAt(roomId: string, slotIndex: number): Placement | null {
	const placements = get(allPlacements);
	
	for (const placement of Object.values(placements)) {
		if (placement.roomId === roomId) {
			const start = placement.slotIndex;
			const end = placement.slotIndex + placement.spanSlots;
			if (slotIndex >= start && slotIndex < end) {
				return placement;
			}
		}
	}
	return null;
}

export function formatTime(date: Date): string {
	return date.toLocaleTimeString('en-US', {
		hour: 'numeric',
		minute: '2-digit',
		hour12: true
	});
}

export function updateUserName(name: string) {
	localStorage.setItem('unconferenceUserName', name);
	currentUser.update(user => ({ ...user, name }));
	registerUser();
}