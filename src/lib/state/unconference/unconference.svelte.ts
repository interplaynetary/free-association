import { gun } from '../gun.svelte';
import { v4 as uuidv4 } from 'uuid';

interface Room {
	id: string;
	name: string;
	capacity?: number;
	order: number;
}

interface Note {
	id: string;
	content: string;
	color: string;
	authorId: string;
	authorName?: string;
	createdAt: number;
	updatedAt: number;
}

interface Placement {
	noteId: string;
	roomId: string;
	slotIndex: number;
	spanSlots: number;
	placedBy: string;
	placedAt: number;
}

interface Board {
	id: string;
	title: string;
	gridStartTime: number;
	gridEndTime: number;
	slotDurationMinutes: number;
}

interface User {
	id: string;
	name: string;
	cursorColor: string;
	isOrganizer: boolean;
	joinedAt: number;
}

class UnconferenceState {
	private boardRef: any = null;
	private userId: string;
	private userName: string;

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
	users = $state<Map<string, User>>(new Map());

	draggedNote = $state<Note | null>(null);
	hoveredCell = $state<{ roomId: string; slotIndex: number } | null>(null);

	constructor() {
		// Get or create user ID
		const storedUserId = localStorage.getItem('unconferenceUserId');
		if (storedUserId) {
			this.userId = storedUserId;
		} else {
			this.userId = uuidv4();
			localStorage.setItem('unconferenceUserId', this.userId);
		}

		// Get or set user name
		const storedUserName = localStorage.getItem('unconferenceUserName');
		if (storedUserName) {
			this.userName = storedUserName;
		} else {
			this.userName = `User ${this.userId.slice(0, 8)}`;
			localStorage.setItem('unconferenceUserName', this.userName);
		}
	}

	initializeBoard(boardId: string) {
		this.board.id = boardId;
		this.boardRef = gun.get('unconference').get(boardId);

		// Subscribe to board data
		this.boardRef.get('board').on((data: any) => {
			if (data && data !== 'null') {
				this.board = { ...this.board, ...data };
			}
		});

		// Subscribe to rooms
		this.boardRef
			.get('rooms')
			.map()
			.on((data: any, id: string) => {
				if (data && data !== 'null') {
					this.rooms.set(id, data);
				} else {
					this.rooms.delete(id);
				}
			});

		// Subscribe to notes
		this.boardRef
			.get('notes')
			.map()
			.on((data: any, id: string) => {
				if (data && data !== 'null') {
					this.notes.set(id, data);
				} else {
					this.notes.delete(id);
				}
			});

		// Subscribe to placements
		this.boardRef
			.get('placements')
			.map()
			.on((data: any, id: string) => {
				if (data && data !== 'null') {
					this.placements.set(id, data);
				} else {
					this.placements.delete(id);
				}
			});

		// Subscribe to users
		this.boardRef
			.get('users')
			.map()
			.on((data: any, id: string) => {
				if (data && data !== 'null') {
					this.users.set(id, data);
				} else {
					this.users.delete(id);
				}
			});

		// Register current user
		this.registerUser();

		// Initialize default rooms if none exist
		setTimeout(() => {
			if (this.rooms.size === 0) {
				this.initializeDefaultRooms();
			}
		}, 1000);
	}

	private registerUser() {
		const user: User = {
			id: this.userId,
			name: this.userName,
			cursorColor: this.generateUserColor(),
			isOrganizer: false,
			joinedAt: Date.now()
		};

		this.boardRef.get('users').get(this.userId).put(user);
	}

	private generateUserColor(): string {
		const colors = [
			'#FF6B6B',
			'#4ECDC4',
			'#45B7D1',
			'#96CEB4',
			'#FFEAA7',
			'#DDA0DD',
			'#98D8C8',
			'#FFB6C1'
		];
		return colors[Math.floor(Math.random() * colors.length)];
	}

	private initializeDefaultRooms() {
		const defaultRooms = ['Main Hall', 'Room A', 'Room B', 'Workshop Space', 'Breakout Room'];

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

	addRoom(room: Room) {
		this.boardRef.get('rooms').get(room.id).put(room);
	}

	removeRoom(roomId: string) {
		this.boardRef.get('rooms').get(roomId).put(null);

		// Remove all placements in this room
		this.placements.forEach((placement, id) => {
			if (placement.roomId === roomId) {
				this.removePlacement(id);
			}
		});
	}

	createNote(content: string, color: string) {
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
		const note = this.notes.get(noteId);
		if (note) {
			const updated = {
				...note,
				content,
				updatedAt: Date.now()
			};
			this.boardRef.get('notes').get(noteId).put(updated);
		}
	}

	deleteNote(noteId: string) {
		this.boardRef.get('notes').get(noteId).put(null);

		// Remove placement if exists
		this.placements.forEach((placement, id) => {
			if (placement.noteId === noteId) {
				this.removePlacement(id);
			}
		});
	}

	placeNote(noteId: string, roomId: string, slotIndex: number, spanSlots: number = 1) {
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
		this.boardRef.get('placements').get(placementId).put(null);
	}

	hasConflict(
		roomId: string,
		slotIndex: number,
		spanSlots: number,
		excludeNoteId?: string
	): boolean {
		for (const [_, placement] of this.placements) {
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
		for (const [_, placement] of this.placements) {
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

	startDragging(note: Note) {
		this.draggedNote = note;
	}

	stopDragging() {
		this.draggedNote = null;
		this.hoveredCell = null;
	}

	setHoveredCell(roomId: string | null, slotIndex: number | null) {
		if (roomId !== null && slotIndex !== null) {
			this.hoveredCell = { roomId, slotIndex };
		} else {
			this.hoveredCell = null;
		}
	}

	get timeSlots() {
		const slots = [];
		const start = this.board.gridStartTime;
		const end = this.board.gridEndTime;
		const duration = this.board.slotDurationMinutes * 60 * 1000;

		for (let time = start; time < end; time += duration) {
			slots.push(new Date(time));
		}

		return slots;
	}

	formatTime(date: Date): string {
		return date.toLocaleTimeString('en-US', {
			hour: 'numeric',
			minute: '2-digit',
			hour12: true
		});
	}

	updateBoardSettings(title: string, startTime: number, endTime: number, slotDuration: number) {
		const board = {
			...this.board,
			title,
			gridStartTime: startTime,
			gridEndTime: endTime,
			slotDurationMinutes: slotDuration
		};
		this.board = board;
		this.boardRef.get('board').put(board);
	}

	updateUserName(name: string) {
		this.userName = name;
		localStorage.setItem('unconferenceUserName', name);

		const user = this.users.get(this.userId);
		if (user) {
			const updated = { ...user, name };
			this.boardRef.get('users').get(this.userId).put(updated);
		}
	}
}

export const unconferenceState = new UnconferenceState();
