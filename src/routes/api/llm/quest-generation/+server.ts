import { json, error } from '@sveltejs/kit';
import type { RequestHandler } from '@sveltejs/kit';
import { QuestGenerationRequestSchema, QuestSchema, type Quest } from '$lib/commons/v5/quest-schemas';
import { checkGeneralRateLimit, checkAiRateLimit } from '$lib/server/middleware/rate-limit';

// Import env vars for AI authentication
let MASTER_API_KEY: string | undefined;

try {
	const env = await import('$env/static/private');
	MASTER_API_KEY = env.MASTER_API_KEY;
} catch (e) {
	MASTER_API_KEY = undefined;
}

/**
 * POST /api/llm/quest-generation - Generate personalized quests
 * 
 * Elegant Integration with LLM Flow System:
 * 1. Validates quest-specific schema
 * 2. Calls /api/ai/completion with requestType: 'quest-generation'
 * 3. Flow system handles: routing → prompt generation → execution → health reporting
 * 4. Post-processes: extracts and validates quest JSON
 * 
 * No auth required: Users authenticate via P2P (Holster/Gun).
 * Server uses MASTER_API_KEY for AI service calls.
 */
export const POST: RequestHandler = async ({ request, fetch }) => {
	try {
		// Rate limiting
		checkGeneralRateLimit(request);
		checkAiRateLimit(request);
		
		// Validate quest-specific schema
		const body = await request.json();
		const parsed = QuestGenerationRequestSchema.safeParse(body);
		
		if (!parsed.success) {
			throw error(400, 'Invalid request: ' + JSON.stringify(parsed.error.format()));
		}
		
		const requestData = parsed.data;
		
		console.log('[QUEST-API] Generating quests for user...');
		console.log('[QUEST-API] Recognition tree:', requestData.recognitionTree?.name);
		console.log('[QUEST-API] Capacities:', requestData.capacities?.length || 0);
		console.log('[QUEST-API] Needs:', requestData.needs?.length || 0);
		console.log('[QUEST-API] Locations:', requestData.locations?.length || 0);
		console.log('[QUEST-API] Peer quests:', requestData.peerQuests?.length || 0);
		
		// Call unified AI completion endpoint
		// The flow system handles everything: routing, prompts, execution, health
		const masterKey = MASTER_API_KEY || 'dev-key-12345-change-me';
		const aiResponse = await fetch('/api/ai/completion', {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
				'X-API-Key': masterKey
			},
			body: JSON.stringify({
				requestType: 'quest-generation',
				...requestData
			})
		});
		
		if (!aiResponse.ok) {
			const errorData = await aiResponse.json().catch(() => ({ message: 'Unknown error' }));
			console.error('[QUEST-API] AI completion failed:', errorData);
			throw error(aiResponse.status, 'Quest generation failed: ' + (errorData.message || 'Unknown error'));
		}
		
		const aiResult = await aiResponse.json();
		
		// Extract and parse quest JSON from AI response
		let quests: any[] = [];
		
		if (aiResult.choices?.[0]?.message?.content) {
			const content = aiResult.choices[0].message.content;
			try {
				// Extract JSON array using the flow's postProcess pattern
				const jsonMatch = content.match(/\[[\s\S]*\]/);
				if (jsonMatch) {
					quests = JSON.parse(jsonMatch[0]);
				} else {
					console.error('[QUEST-API] No JSON array found in response');
					throw error(500, 'AI did not return valid quest JSON array');
				}
			} catch (err: any) {
				console.error('[QUEST-API] Failed to parse quest JSON:', err);
				throw error(500, 'Failed to parse quest response: ' + err.message);
			}
		} else {
			throw error(500, 'AI response missing content');
		}
		
		if (!Array.isArray(quests) || quests.length === 0) {
			throw error(500, 'AI did not generate any valid quests');
		}
		
		// Validate each quest
		const validatedQuests: Quest[] = [];
		for (const quest of quests) {
			// Add origin metadata
			const questWithOrigin = {
				...quest,
				origin: {
					type: 'ai',
					model: aiResult._routing?.model || 'unknown',
					flow: 'quest-generation',
					generatedAt: Date.now()
				},
				completion: quest.completion || { completed: false },
				_updatedAt: Date.now()
			};
			
			const validated = QuestSchema.safeParse(questWithOrigin);
			if (validated.success) {
				validatedQuests.push(validated.data);
			} else {
				console.warn('[QUEST-API] Invalid quest generated:', validated.error);
			}
		}
		
		if (validatedQuests.length === 0) {
			throw error(500, 'No valid quests were generated');
		}
		
		console.log(`[QUEST-API] ✅ Generated ${validatedQuests.length} valid quests`);
		
		// Return the quests with routing metadata from completion response
		return json({
			success: true,
			quests: validatedQuests,
			count: validatedQuests.length,
			_routing: aiResult._routing // Pass through routing info from completion
		});
		
	} catch (err: any) {
		console.error('[QUEST-API] Error:', err);
		
		// If it's already a SvelteKit error, rethrow it
		if (err.status) {
			throw err;
		}
		
		throw error(500, 'Quest generation failed: ' + err.message);
	}
};

