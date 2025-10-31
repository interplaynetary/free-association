import type { ExtendedCompletionRequest } from '../schemas/completion';

/**
 * Flow definition structure
 */
export interface FlowDefinition {
  name: string;
  requestType: string;
  description: string;
  preferredModels: string[]; // Ordered list of preferred models
  promptTemplate: (request: any) => PromptConfig;
  postProcess?: (response: any) => any;
}

export interface PromptConfig {
  system?: string;
  user: string;
  temperature?: number;
  maxTokens?: number;
}

/**
 * Recognition Analysis Flow
 * Analyzes mutual recognition networks and capacity distribution
 */
export const recognitionAnalysisFlow: FlowDefinition = {
  name: 'Recognition Analysis',
  requestType: 'recognition-analysis',
  description: 'Analyzes free-association recognition networks and capacity flows',
  preferredModels: ['anthropic/claude-3-opus', 'openai/gpt-4', 'openai/gpt-4-turbo'],
  
  promptTemplate: (request: any) => {
    const playerNames = request.players.map((p: any) => p.name).join(', ');
    
    let systemPrompt = `You are an expert in analyzing free-association networks, mutual recognition, and capacity distribution.

Free-association principles:
- Mutual Recognition = minimum(A's recognition of B, B's recognition of A)
- Capacity shares flow according to mutual recognition proportions
- Total recognition = 100% (zero-sum within each person)
- Recognition is non-transferable and dynamically adjustable

Your analysis should be clear, actionable, and mathematically sound.`;

    let userPrompt = '';
    
    if (request.analysisType === 'mutual-recognition') {
      userPrompt = `Analyze the mutual recognition network for these players: ${playerNames}

Players and their recognitions:
${request.players.map((p: any) => `
${p.name} (ID: ${p.id}):
  Recognizes: ${Object.entries(p.recognitions).map(([id, pct]) => `${id}: ${pct}%`).join(', ')}
`).join('\n')}

Calculate:
1. All mutual recognition values (min of bidirectional recognitions)
2. Each player's total mutual recognition
3. Each player's general shares in others' capacities
4. Identify strongest/weakest mutual bonds
5. Suggest optimizations for better network health`;
    } else if (request.analysisType === 'capacity-distribution') {
      userPrompt = `Analyze capacity distribution for: ${playerNames}

${request.players.map((p: any) => `${p.name}: ${JSON.stringify(p.recognitions)}`).join('\n')}

Analyze:
1. How surplus capacity would flow through this network
2. Who receives what percentage of each player's surplus
3. Identify potential bottlenecks or imbalances
4. Suggest recognition adjustments for more equitable distribution`;
    } else {
      userPrompt = `Assess network health for: ${playerNames}

Recognition data:
${JSON.stringify(request.players, null, 2)}

Evaluate:
1. Overall network connectivity
2. Recognition reciprocity levels
3. Potential free-riders or under-recognized contributors
4. Network resilience and redundancy
5. Recommendations for strengthening the network`;
    }

    return {
      system: systemPrompt,
      user: userPrompt,
      temperature: 0.3, // Lower temp for analytical tasks
      maxTokens: request.maxTokens || 1000
    };
  },
  
  postProcess: (response: any) => {
    // Could extract structured data, calculate metrics, etc.
    return response;
  }
};

/**
 * Capacity Recommendation Flow
 * Recommends how to distribute surplus capacity
 */
export const capacityRecommendationFlow: FlowDefinition = {
  name: 'Capacity Recommendation',
  requestType: 'capacity-recommendation',
  description: 'Recommends optimal capacity distribution based on mutual recognition',
  preferredModels: ['anthropic/claude-3-sonnet', 'openai/gpt-4', 'anthropic/claude-3-opus'],
  
  promptTemplate: (request: any) => ({
    system: `You are a free-association capacity distribution advisor. 

Your role is to help people understand how their surplus capacity (time, space, resources) would flow through their mutual recognition network, and suggest optimal distributions.

Principles:
- Capacity shares flow to direct mutual contributors
- Also flows transitively to indirect contributors
- Mutual fulfillment: both parties must express desire
- Proportional to mutual recognition strength`,

    user: `${request.player.name} has the following surplus capacities:
${request.player.surplusCapacities.map((c: any) => `- ${c.type}: ${c.quantity} (${c.description})`).join('\n')}

Their mutual contributors:
${request.contributors.map((c: any) => `
${c.name}:
  - Mutual Recognition: ${c.mutualRecognition}%
  - Needs: ${c.needs.join(', ')}
`).join('\n')}

Recommend:
1. How to optimally distribute each capacity type
2. Which contributors would benefit most from each capacity
3. Potential matches between surplus and needs
4. Suggestions for strengthening mutually beneficial relationships
5. Any gaps or unmet needs in the network`,

    temperature: 0.5,
    maxTokens: request.maxTokens || 800
  })
};

/**
 * Code Generation Flow
 * Generates code with proper context and formatting
 */
export const codeGenerationFlow: FlowDefinition = {
  name: 'Code Generation',
  requestType: 'code-generation',
  description: 'Generates code with context and best practices',
  preferredModels: ['openai/gpt-4', 'anthropic/claude-3-sonnet', 'openai/gpt-4-turbo'],
  
  promptTemplate: (request: any) => ({
    system: `You are an expert ${request.language} developer. Write clean, well-documented, production-quality code.

Follow best practices:
- Clear variable names
- Proper error handling
- Type safety where applicable
- Security considerations
- Performance optimization`,

    user: `Language: ${request.language}

Task: ${request.task}

${request.context ? `Context:\n${request.context}\n` : ''}

Generate the code with:
1. Clear comments explaining the logic
2. Error handling
3. Example usage
4. Any necessary imports/dependencies`,

    temperature: 0.2, // Low temp for deterministic code
    maxTokens: request.maxTokens || 2000
  })
};

/**
 * Data Analysis Flow
 * Analyzes data and provides insights
 */
export const dataAnalysisFlow: FlowDefinition = {
  name: 'Data Analysis',
  requestType: 'data-analysis',
  description: 'Analyzes data and provides structured insights',
  preferredModels: ['openai/gpt-4', 'anthropic/claude-3-sonnet', 'openai/gpt-4-turbo'],
  
  promptTemplate: (request: any) => {
    let analysisPrompt = '';
    
    switch (request.analysisType) {
      case 'summary':
        analysisPrompt = 'Provide a comprehensive summary with key statistics and insights';
        break;
      case 'trend':
        analysisPrompt = 'Identify trends, patterns, and trajectories in the data';
        break;
      case 'anomaly':
        analysisPrompt = 'Detect anomalies, outliers, and unusual patterns';
        break;
      case 'comparison':
        analysisPrompt = 'Compare different segments and highlight key differences';
        break;
    }

    return {
      system: `You are a data analyst. Provide clear, actionable insights backed by the data.

Format your response with:
- Key findings (bullet points)
- Detailed analysis
- Visualizations recommendations
- Actionable recommendations`,

      user: `Data:
\`\`\`json
${JSON.stringify(request.data, null, 2)}
\`\`\`

Question: ${request.question}

Analysis type: ${request.analysisType}

${analysisPrompt}`,

      temperature: 0.3,
      maxTokens: request.maxTokens || 1500
    };
  }
};

/**
 * General Chat Flow (fallback)
 */
export const chatFlow: FlowDefinition = {
  name: 'General Chat',
  requestType: 'chat',
  description: 'General conversational AI',
  preferredModels: ['openai/gpt-3.5-turbo', 'anthropic/claude-3-haiku', 'mistralai/mistral-medium'],
  
  promptTemplate: (request: any) => ({
    user: request.messages.map((m: any) => `${m.role}: ${m.content}`).join('\n\n'),
    temperature: request.temperature || 0.7,
    maxTokens: request.maxTokens || 1024
  })
};

/**
 * Quest Generation Flow
 * Generates personalized quests based on user's recognition tree, capacities, needs, and location
 */
export const questGenerationFlow: FlowDefinition = {
  name: 'Quest Generation',
  requestType: 'quest-generation',
  description: 'Generates personalized quests based on user values, capacities, and needs',
  preferredModels: ['anthropic/claude-3-opus', 'openai/gpt-4', 'anthropic/claude-3-sonnet'],
  
  promptTemplate: (request: any) => {
    const tree = request.recognitionTree;
    const capacities = request.capacities || [];
    const needs = request.needs || [];
    const locations = request.locations || [];
    const peerQuests = request.peerQuests || [];
    const maxQuests = request.maxQuests || 5;
    
    // Extract tree structure for context
    const treeContext = tree ? `
Recognition Tree Structure:
Root: ${tree.name}
Children: ${tree.children?.length || 0} value categories
Total Points: ${tree.points}

Value Categories:
${tree.children?.map((child: any) => `- ${child.name} (${child.points} points)`).join('\n') || 'None'}
` : 'No recognition tree provided';

    // Extract capacity context
    const capacityContext = capacities.length > 0 ? `
Available Capacities:
${capacities.map((c: any) => `- ${c.name} (${c.quantity} ${c.unit || 'units'}): ${c.description || 'No description'}`).join('\n')}
` : 'No capacities declared yet';

    // Extract need context
    const needContext = needs.length > 0 ? `
Stated Needs:
${needs.map((n: any) => `- ${n.name} (${n.quantity} ${n.unit || 'units'}): ${n.description || 'No description'}`).join('\n')}
` : 'No needs declared yet';

    // Extract location context
    const locationContext = locations.length > 0 ? `
Locations:
${locations.map((l: any) => `- ${l.city || 'Unknown city'}, ${l.state_province || ''} ${l.country || ''}`).join('\n')}
` : 'No location data available';

    // Peer quest context
    const peerQuestContext = peerQuests.length > 0 ? `
Other Players' Quests (for inspiration and collaboration):
${peerQuests.slice(0, 5).map((q: any) => `- ${q.title} (${q.scale} scale, ${q.difficulty} difficulty)`).join('\n')}
` : 'No peer quests available';

    const systemPrompt = `You are an AI Quest Generator for the Free Association platform - a system where people coordinate based on mutual recognition and shared values.

Your role is to analyze a user's recognition tree (their value hierarchy), capacities (what they can offer), needs (what they need), and location to generate personalized, actionable quests.

KEY PRINCIPLES:
1. **Scale Inference**: Analyze the recognition tree node names to determine if the user values local or global change:
   - Personal/individual language ("my housing", "personal health", "my education") → local scale
   - Community/social language ("community housing", "public health", "education for all") → community/regional/global scale
   
2. **Quest Alignment**: Generate quests that:
   - Align with their highest-valued recognition tree nodes
   - Leverage their existing capacities
   - Help fulfill their stated needs
   - Are actionable in their geographic location(s)
   - Build on "the more capacities you can build from the more wild the quests"
   
3. **Quest Diversity**: Mix of:
   - Main quests: Major, transformative actions (1-2)
   - Side quests: Smaller, achievable actions (3-4)
   
4. **Difficulty Scaling**: 
   - Easy: Simple, immediate actions
   - Medium: Requires planning and coordination
   - Hard: Significant commitment and resources
   - Epic: Transformative, long-term projects

5. **Collaboration**: Reference peer quests where collaboration makes sense

RECOGNITION TREE CONTEXT:
Free Association uses recognition trees where:
- Nodes represent values/priorities
- Points indicate relative importance
- Contributors on nodes show who helps fulfill each value
- The tree structure reveals whether someone values personal vs collective change`;

    const userPrompt = `Generate ${maxQuests} personalized quests for this user:

${treeContext}

${capacityContext}

${needContext}

${locationContext}

${peerQuestContext}

INSTRUCTIONS:
1. Analyze the recognition tree to infer the user's scale preference (local vs global)
2. Generate ${maxQuests} quests as a JSON array
3. Each quest must have this exact structure:
{
  "id": "unique-id",
  "title": "Quest Title",
  "description": "Detailed description of what the user should do and why it matters",
  "type": "main" or "side",
  "difficulty": "easy" | "medium" | "hard" | "epic",
  "scale": "local" | "community" | "regional" | "global",
  "location": {
    "city": "City Name",
    "country": "Country",
    "online": true/false
  },
  "rewards": [
    {
      "description": "What the user gains from completing this"
    }
  ],
  "tags": ["relevant", "tags"],
  "relatedCapacities": ["capacity-ids"],
  "relatedNeeds": ["need-ids"],
  "relatedTreeNodes": ["tree-node-ids"]
}

RESPOND WITH ONLY THE JSON ARRAY, NO OTHER TEXT.`;

    return {
      system: systemPrompt,
      user: userPrompt,
      temperature: 0.7, // Creative but focused
      maxTokens: request.maxTokens || 2000
    };
  },
  
  postProcess: (response: any) => {
    // Extract JSON from response
    try {
      const content = response.choices?.[0]?.message?.content || response.content || '';
      
      // Try to extract JSON array from response
      const jsonMatch = content.match(/\[[\s\S]*\]/);
      if (jsonMatch) {
        const quests = JSON.parse(jsonMatch[0]);
        return { quests };
      }
      
      // If no JSON found, return error
      return { 
        error: 'Failed to parse quest JSON from AI response',
        rawResponse: content
      };
    } catch (err: any) {
      return {
        error: 'Failed to process quest generation response: ' + err.message,
        rawResponse: response
      };
    }
  }
};

/**
 * Flow Registry - maps request types to flow definitions
 */
export const flowRegistry: Record<string, FlowDefinition> = {
  'recognition-analysis': recognitionAnalysisFlow,
  'capacity-recommendation': capacityRecommendationFlow,
  'code-generation': codeGenerationFlow,
  'data-analysis': dataAnalysisFlow,
  'quest-generation': questGenerationFlow,
  'chat': chatFlow
};

/**
 * Get flow for a request type
 */
export function getFlow(requestType?: string): FlowDefinition {
  if (!requestType || !flowRegistry[requestType]) {
    return chatFlow; // Default fallback
  }
  return flowRegistry[requestType];
}

/**
 * List all available flows
 */
export function listFlows(): FlowDefinition[] {
  return Object.values(flowRegistry);
}

