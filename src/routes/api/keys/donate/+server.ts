import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { addKey, getPoolStatus } from '$lib/server/key-pool/manager';
import { checkGeneralRateLimit } from '$lib/server/middleware/rate-limit';
import * as z from 'zod';

/**
 * Donation Schema
 */
const DonationSchema = z.object({
	apiKey: z.string().min(20).max(200), // OpenRouter keys are typically long
	donorName: z.string().min(1).max(100).optional(),
	isAnonymous: z.boolean().default(false),
	// Optional: user's Holster pub key for tracking
	donorPub: z.string().optional()
});

/**
 * POST /api/keys/donate - Donate an OpenRouter API key to the pool
 * 
 * Allows users to contribute their API keys to support the community.
 * Keys are validated before being added to the pool.
 */
export const POST: RequestHandler = async ({ request }) => {
	try {
		// Rate limiting to prevent abuse
		checkGeneralRateLimit(request);
		
		// Parse and validate donation
		const body = await request.json();
		const parsed = DonationSchema.safeParse(body);
		
		if (!parsed.success) {
			throw error(400, 'Invalid donation: ' + JSON.stringify(parsed.error.format()));
		}
		
		const { apiKey, donorName, isAnonymous, donorPub } = parsed.data;
		
		// Validate the key format (basic check)
		if (!apiKey.startsWith('sk-or-')) {
			throw error(400, 'Invalid OpenRouter API key format. Keys should start with "sk-or-"');
		}
		
		// Test the key by making a minimal API call
		const testResponse = await fetch('https://openrouter.ai/api/v1/models', {
			headers: {
				'Authorization': `Bearer ${apiKey}`
			}
		});
		
		if (!testResponse.ok) {
			const errorData = await testResponse.json().catch(() => ({}));
			throw error(400, 'Invalid or expired API key: ' + (errorData.error?.message || 'Failed validation'));
		}
		
		// Add to pool with donor info
		const displayName = isAnonymous ? undefined : donorName;
		const success = addKey(apiKey, donorPub, displayName);
		
		if (!success) {
			throw error(400, 'This key is already in the pool');
		}
		
		// Get updated pool status
		const poolStatus = getPoolStatus();
		
		console.log(`🎁 New key donated${displayName ? ` by ${displayName}` : ' (anonymous)'}`);
		
		return json({
			success: true,
			message: 'Thank you for your contribution! Your key has been added to the pool.',
			poolStatus: {
				totalKeys: poolStatus.totalKeys,
				healthyKeys: poolStatus.health.healthy
			}
		});
		
	} catch (err: any) {
		console.error('[KEY-DONATE] Error:', err);
		
		if (err.status) {
			throw err;
		}
		
		throw error(500, 'Failed to process donation: ' + err.message);
	}
};

/**
 * GET /api/keys/donate - Get donation statistics (for display)
 */
export const GET: RequestHandler = async () => {
	try {
		const poolStatus = getPoolStatus();
		
		// Return public stats without exposing actual keys
		return json({
			totalKeys: poolStatus.totalKeys,
			healthyKeys: poolStatus.health.healthy,
			degradedKeys: poolStatus.health.degraded,
			failedKeys: poolStatus.health.failed,
			stats: poolStatus.stats
		});
		
	} catch (err: any) {
		console.error('[KEY-DONATE] Error fetching stats:', err);
		throw error(500, 'Failed to fetch donation stats');
	}
};

