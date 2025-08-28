import { z } from 'zod';

// Step 1: Define Data Structures with Zod Schemas

export const RoomSchema = z.object({
	id: z.string(),
	name: z.string(),
	capacity: z.number().optional(),
	order: z.number()
});

export const NoteSchema = z.object({
	id: z.string(),
	content: z.string(),
	color: z.string(),
	authorId: z.string(),
	authorName: z.string().optional(),
	createdAt: z.number(),
	updatedAt: z.number()
});

export const PlacementSchema = z.object({
	noteId: z.string(),
	roomId: z.string(),
	slotIndex: z.number(),
	spanSlots: z.number(),
	placedBy: z.string(),
	placedAt: z.number()
});

export const BoardSchema = z.object({
	id: z.string(),
	title: z.string(),
	gridStartTime: z.number(),
	gridEndTime: z.number(),
	slotDurationMinutes: z.number()
});

export const UnconferenceUserSchema = z.object({
	id: z.string(),
	name: z.string(),
	cursorColor: z.string(),
	isOrganizer: z.boolean(),
	joinedAt: z.number()
});

// Type exports from schemas
export type Room = z.infer<typeof RoomSchema>;
export type Note = z.infer<typeof NoteSchema>;
export type Placement = z.infer<typeof PlacementSchema>;
export type Board = z.infer<typeof BoardSchema>;
export type UnconferenceUser = z.infer<typeof UnconferenceUserSchema>;

// Collection schemas
export const RoomsMapSchema = z.record(z.string(), RoomSchema);
export const NotesMapSchema = z.record(z.string(), NoteSchema);
export const PlacementsMapSchema = z.record(z.string(), PlacementSchema);
export const UsersMapSchema = z.record(z.string(), UnconferenceUserSchema);

export type RoomsMap = z.infer<typeof RoomsMapSchema>;
export type NotesMap = z.infer<typeof NotesMapSchema>;
export type PlacementsMap = z.infer<typeof PlacementsMapSchema>;
export type UsersMap = z.infer<typeof UsersMapSchema>;