import { gun } from './gun.svelte';
import { v4 as uuidv4 } from 'uuid';
import {
	BoardSchema,
	RoomSchema,
	NoteSchema,
	PlacementSchema,
	UnconferenceUserSchema,
	type Board,
	type Room,
	type Note,
	type Placement,
	type UnconferenceUser
} from '$lib/schema/unconference';

// Create a reactive state class using Svelte 5 $state runes
class UnconferenceState {
	// Board reference
	private boardRef: any = null;
	private subscriptions: any[] = [];
	
	// User identity
	readonly userId = (() => {
		const stored = localStorage.getItem('unconferenceUserId');
		if (stored) return stored;
		const newId = uuidv4();
		localStorage.setItem('unconferenceUserId', newId);
		return newId;
	})();
	
	readonly userName = (() => {
		const stored = localStorage.getItem('unconferenceUserName');
		if (stored) return stored;
		const name = `User ${this.userId.slice(0, 8)}`;
		localStorage.setItem('unconferenceUserName', name);
		return name;
	})();
	
	// Reactive state using $state
	board = $state<Board>({
		id: '',
		title: 'Unconference Schedule',
		gridStartTime: new Date().setHours(9, 0, 0, 0),
		gridEndTime: new Date().setHours(17, 0, 0, 0),
		slotDurationMinutes: 30
	});
	
	rooms = $state<Map<string, Room>>(new Map());
	notes = $state<Map<string, Note>>(new Map());
	placements = $state<Map<string, Placement>>(new Map());
	users = $state<Map<string, UnconferenceUser>>(new Map());
	
	// Current user
	currentUser = $state<UnconferenceUser>({
		id: this.userId,
		name: this.userName,
		cursorColor: this.generateUserColor(),
		isOrganizer: false,
		joinedAt: Date.now()
	});
	
	// Derived reactive values using $derived
	timeSlots = $derived.by(() => {
		const slots = [];
		const start = this.board.gridStartTime;
		const end = this.board.gridEndTime;
		const duration = this.board.slotDurationMinutes * 60 * 1000;
		
		for (let time = start; time < end; time += duration) {
			slots.push(new Date(time));
		}
		
		return slots;
	});
	
	sortedRooms = $derived.by(() => {
		return Array.from(this.rooms.values()).sort((a, b) => a.order - b.order);
	});
	
	unplacedNotes = $derived.by(() => {
		const placed = new Set(Array.from(this.placements.values()).map(p => p.noteId));
		return Array.from(this.notes.values())
			.filter(note => !placed.has(note.id))
			.sort((a, b) => b.createdAt - a.createdAt);
	});
	
	placedNotesCount = $derived.by(() => {
		return this.placements.size;
	});
	
	totalNotesCount = $derived.by(() => {
		return this.notes.size;
	});
	
	// Initialize board and set up subscriptions
	initializeBoard(boardId: string) {
		console.log('Initializing board:', boardId);
		
		// Clean up existing subscriptions
		this.cleanup();
		
		// Update board ID
		this.board.id = boardId;
		
		// Get board reference
		this.boardRef = gun.get('unconference').get(boardId);
		
		// Subscribe to board data
		this.boardRef.get('board').on((data: any) => {
			if (!data || data === 'null' || typeof data !== 'object') return;
			
			try {
				const validated = BoardSchema.parse(data);
				console.log('Board update:', validated);
				// Update individual properties to maintain reactivity
				this.board.title = validated.title;
				this.board.gridStartTime = validated.gridStartTime;
				this.board.gridEndTime = validated.gridEndTime;
				this.board.slotDurationMinutes = validated.slotDurationMinutes;
			} catch (error) {
				console.error('Invalid board data:', error);
			}
		});
		
		// Subscribe to rooms
		this.boardRef.get('rooms').map().on((data: any, id: string) => {
			if (!data || data === 'null') {
				this.rooms.delete(id);
				// Trigger reactivity by reassigning
				this.rooms = new Map(this.rooms);
				return;
			}
			
			try {
				const validated = RoomSchema.parse(data);
				console.log('Room update:', id, validated);
				this.rooms.set(id, validated);
				// Trigger reactivity by reassigning
				this.rooms = new Map(this.rooms);
			} catch (error) {
				console.error('Invalid room data:', error);
			}
		});
		
		// Subscribe to notes
		this.boardRef.get('notes').map().on((data: any, id: string) => {
			if (!data || data === 'null') {
				this.notes.delete(id);
				// Trigger reactivity by reassigning
				this.notes = new Map(this.notes);
				return;
			}
			
			try {
				const validated = NoteSchema.parse(data);
				console.log('Note update:', id, validated);
				this.notes.set(id, validated);
				// Trigger reactivity by reassigning
				this.notes = new Map(this.notes);
			} catch (error) {
				console.error('Invalid note data:', error);
			}
		});
		
		// Subscribe to placements
		this.boardRef.get('placements').map().on((data: any, id: string) => {
			if (!data || data === 'null') {
				this.placements.delete(id);
				// Trigger reactivity by reassigning
				this.placements = new Map(this.placements);
				return;
			}
			
			try {
				const validated = PlacementSchema.parse(data);
				console.log('Placement update:', id, validated);
				this.placements.set(id, validated);
				// Trigger reactivity by reassigning
				this.placements = new Map(this.placements);
			} catch (error) {
				console.error('Invalid placement data:', error);
			}
		});
		
		// Subscribe to users
		this.boardRef.get('users').map().on((data: any, id: string) => {
			if (!data || data === 'null') {
				this.users.delete(id);
				// Trigger reactivity by reassigning
				this.users = new Map(this.users);
				return;
			}
			
			try {
				const validated = UnconferenceUserSchema.parse(data);
				console.log('User update:', id, validated);
				this.users.set(id, validated);
				// Trigger reactivity by reassigning
				this.users = new Map(this.users);
			} catch (error) {
				console.error('Invalid user data:', error);
			}
		});
		
		// Register current user
		setTimeout(() => this.registerUser(), 100);
		
		// Initialize default rooms if none exist
		setTimeout(() => {
			if (this.rooms.size === 0) {
				this.initializeDefaultRooms();
			}
		}, 2000);
	}
	
	// Clean up subscriptions
	cleanup() {
		// Gun.js doesn't provide a clean way to unsubscribe from .on()
		// Best practice is to null out the reference
		this.boardRef = null;
	}
	
	// User registration
	private registerUser() {
		if (!this.boardRef) return;
		this.boardRef.get('users').get(this.userId).put(this.currentUser);
	}
	
	// Initialize default rooms
	private initializeDefaultRooms() {
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
			this.addRoom(room);
		});
	}
	
	// CRUD operations
	addRoom(room: Room) {
		if (!this.boardRef) return;
		this.boardRef.get('rooms').get(room.id).put(room);
	}
	
	removeRoom(roomId: string) {
		if (!this.boardRef) return;
		this.boardRef.get('rooms').get(roomId).put(null);
		
		// Remove all placements in this room
		this.placements.forEach((placement, id) => {
			if (placement.roomId === roomId) {
				this.removePlacement(id);
			}
		});
	}
	
	createNote(content: string, color: string): string {
		if (!this.boardRef) return '';
		
		const note: Note = {
			id: uuidv4(),
			content,
			color,
			authorId: this.userId,
			authorName: this.userName,
			createdAt: Date.now(),
			updatedAt: Date.now()
		};
		
		this.boardRef.get('notes').get(note.id).put(note);
		return note.id;
	}
	
	updateNote(noteId: string, content: string) {
		if (!this.boardRef) return;
		
		const note = this.notes.get(noteId);
		if (!note) return;
		
		const updated: Note = {
			...note,
			content,
			updatedAt: Date.now()
		};
		
		this.boardRef.get('notes').get(noteId).put(updated);
	}
	
	deleteNote(noteId: string) {
		if (!this.boardRef) return;
		this.boardRef.get('notes').get(noteId).put(null);
		
		// Remove placement if exists
		this.placements.forEach((placement, id) => {
			if (placement.noteId === noteId) {
				this.removePlacement(id);
			}
		});
	}
	
	placeNote(noteId: string, roomId: string, slotIndex: number, spanSlots: number = 1): boolean {
		if (!this.boardRef) return false;
		
		// Check for conflicts
		if (this.hasConflict(roomId, slotIndex, spanSlots, noteId)) {
			return false;
		}
		
		// Remove existing placement for this note
		this.placements.forEach((placement, id) => {
			if (placement.noteId === noteId) {
				this.removePlacement(id);
			}
		});
		
		const placement: Placement = {
			noteId,
			roomId,
			slotIndex,
			spanSlots,
			placedBy: this.userId,
			placedAt: Date.now()
		};
		
		const placementId = `${noteId}_${roomId}_${slotIndex}`;
		this.boardRef.get('placements').get(placementId).put(placement);
		
		return true;
	}
	
	removePlacement(placementId: string) {
		if (!this.boardRef) return;
		this.boardRef.get('placements').get(placementId).put(null);
	}
	
	// Helper functions
	hasConflict(roomId: string, slotIndex: number, spanSlots: number, excludeNoteId?: string): boolean {
		for (const placement of this.placements.values()) {
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
	
	getPlacementAt(roomId: string, slotIndex: number): Placement | null {
		for (const placement of this.placements.values()) {
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
	
	formatTime(date: Date): string {
		return date.toLocaleTimeString('en-US', {
			hour: 'numeric',
			minute: '2-digit',
			hour12: true
		});
	}
	
	updateUserName(name: string) {
		localStorage.setItem('unconferenceUserName', name);
		this.currentUser.name = name;
		this.registerUser();
	}
	
	private generateUserColor(): string {
		const colors = [
			'#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4',
			'#FFEAA7', '#DDA0DD', '#98D8C8', '#FFB6C1'
		];
		return colors[Math.floor(Math.random() * colors.length)];
	}
}

// Export singleton instance
export const unconferenceState = new UnconferenceState();