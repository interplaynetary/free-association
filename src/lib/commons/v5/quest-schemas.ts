/**
 * Quest System Schemas
 * Separated from main schemas for better organization
 */

import * as z from 'zod';
import { ITCStampSchema, RootNodeSchema, AvailabilitySlotSchema, NeedSlotSchema } from './schemas';

/**
 * Quest Origin - Tracks who/what created this quest
 */
export const QuestOriginSchema = z.discriminatedUnion('type', [
	z.object({
		type: z.literal('ai'),
		model: z.string(), // e.g., "anthropic/claude-3-opus"
		flow: z.string().optional(), // e.g., "quest-generation"
		generatedAt: z.number() // timestamp
	}),
	z.object({
		type: z.literal('peer'),
		pub: z.string(), // Peer's public key
		alias: z.string().optional(), // Peer's alias/username
		sharedAt: z.number() // timestamp
	}),
	z.object({
		type: z.literal('manual'),
		createdAt: z.number() // timestamp
	})
]);

export type QuestOrigin = z.infer<typeof QuestOriginSchema>;

/**
 * Quest Scale - Indicates whether quest is individual or social/global
 * Inferred by AI from recognition tree structure
 */
export const QuestScaleSchema = z.enum(['local', 'community', 'regional', 'global']);
export type QuestScale = z.infer<typeof QuestScaleSchema>;

/**
 * Quest Type - Classification of quest
 */
export const QuestTypeSchema = z.enum(['main', 'side', 'archived']);
export type QuestType = z.infer<typeof QuestTypeSchema>;

/**
 * Quest Difficulty
 */
export const QuestDifficultySchema = z.enum(['easy', 'medium', 'hard', 'epic']);
export type QuestDifficulty = z.infer<typeof QuestDifficultySchema>;

/**
 * Quest Location Context - Where quest is relevant
 */
export const QuestLocationSchema = z.object({
	city: z.string().optional(),
	state_province: z.string().optional(),
	country: z.string().optional(),
	latitude: z.number().optional(),
	longitude: z.number().optional(),
	online: z.boolean().optional()
});

export type QuestLocation = z.infer<typeof QuestLocationSchema>;

/**
 * Quest Reward - What user gains from completing quest
 */
export const QuestRewardSchema = z.object({
	description: z.string(),
	points: z.number().optional(),
	unlocks: z.array(z.string()).optional() // IDs of things unlocked
});

export type QuestReward = z.infer<typeof QuestRewardSchema>;

/**
 * Quest Completion Status
 */
export const QuestCompletionSchema = z.object({
	completed: z.boolean().default(false),
	completedAt: z.number().optional(),
	progress: z.number().min(0).max(1).optional(), // 0-1 for partial progress
	notes: z.string().optional()
});

export type QuestCompletion = z.infer<typeof QuestCompletionSchema>;

/**
 * Quest Schema - Core quest data structure
 */
export const QuestSchema = z.object({
	id: z.string().min(1),
	title: z.string().min(1),
	description: z.string(),
	
	// Classification
	type: QuestTypeSchema.default('side'),
	difficulty: QuestDifficultySchema,
	scale: QuestScaleSchema,
	
	// Origin tracking
	origin: QuestOriginSchema,
	
	// Location context
	location: QuestLocationSchema.optional(),
	
	// Rewards
	rewards: z.array(QuestRewardSchema).optional(),
	
	// Completion tracking
	completion: QuestCompletionSchema.default({ completed: false }),
	
	// Additional metadata
	tags: z.array(z.string()).optional(),
	relatedCapacities: z.array(z.string()).optional(), // IDs of relevant capacities
	relatedNeeds: z.array(z.string()).optional(), // IDs of relevant needs
	relatedTreeNodes: z.array(z.string()).optional(), // IDs of relevant tree nodes
	
	// P2P sync
	stamp: ITCStampSchema.optional(), // ITC causality tracking
	_updatedAt: z.number().optional() // Holster timestamp
});

export type Quest = z.infer<typeof QuestSchema>;

/**
 * Quest Collection - Array of quests
 */
export const QuestCollectionSchema = z.array(QuestSchema);
export type QuestCollection = z.infer<typeof QuestCollectionSchema>;

/**
 * Quest Sharing Settings
 */
export const QuestSharingSettingsSchema = z.object({
	enabled: z.boolean().default(false),
	shareWith: z.enum(['all', 'mutual-contributors', 'specific']).default('mutual-contributors'),
	specificPubs: z.array(z.string()).optional(), // If shareWith === 'specific'
	_updatedAt: z.number().optional()
});

export type QuestSharingSettings = z.infer<typeof QuestSharingSettingsSchema>;

/**
 * Quest Generation Request Data
 */
export const QuestGenerationRequestSchema = z.object({
	recognitionTree: RootNodeSchema,
	capacities: z.array(AvailabilitySlotSchema).optional(),
	needs: z.array(NeedSlotSchema).optional(),
	locations: z.array(QuestLocationSchema).optional(),
	peerQuests: QuestCollectionSchema.optional(),
	maxQuests: z.number().int().positive().optional().default(5),
	preferredTypes: z.array(QuestTypeSchema).optional()
});

export type QuestGenerationRequest = z.infer<typeof QuestGenerationRequestSchema>;

