# Quest Maker Documentation

## Overview

This document describes how to design custom prompts for quest generation in the Free Association platform. As a quest maker, you can create prompts that will be used instead of the default system prompts when generating personalized quests for users.

## How It Works

1. **Input Data**: When a user requests quest generation, the system gathers:
   - Their recognition tree (value hierarchy)
   - Their capacities (what they can offer)
   - Their needs (what they need)
   - Their locations
   - Peer quests (if available)

2. **Prompt Processing**: Your custom prompts are processed with template variables replaced with actual user data

3. **Quest Generation**: The AI uses your prompts to generate personalized quests

## Input Data Structure

When designing prompts, you have access to the following data:

### `recognitionTree` (RootNodeSchema)

The user's recognition tree - their value hierarchy:

```typescript
{
  id: string;
  name: string;              // Root node name
  points: number;             // Total points allocated
  children?: TreeNode[];      // Child value categories
  
  // Each child has:
  {
    id: string;
    name: string;             // Value category name
    points: number;           // Points allocated to this category
    children?: TreeNode[];    // Further nested values
    contributors?: Contributor[]; // People who help fulfill this value
  }
}
```

**Example Access:**
- `{{recognitionTree.name}}` → "Personal Growth"
- `{{recognitionTree.points}}` → 100
- `{{recognitionTree.children.length}}` → 3

### `capacities` (AvailabilitySlotSchema[])

Array of what the user can offer:

```typescript
[
  {
    id: string;
    name: string;             // e.g., "Web Development"
    quantity: number;         // Amount available
    unit?: string;            // e.g., "hours", "items"
    description?: string;     // Detailed description
    availability?: {
      start: number;          // Timestamp
      end: number;            // Timestamp
    };
    location?: {
      city?: string;
      country?: string;
      online?: boolean;
    };
  }
]
```

**Example Access:**
- Use `{{#each capacities}}...{{/each}}` to loop through capacities
- Within loop: `{{name}}`, `{{quantity}}`, `{{unit}}`, `{{description}}`

### `needs` (NeedSlotSchema[])

Array of what the user needs:

```typescript
[
  {
    id: string;
    name: string;             // e.g., "Workspace"
    quantity: number;         // Amount needed
    unit?: string;            // e.g., "hours", "items"
    description?: string;     // Detailed description
    deadline?: number;        // Timestamp
    location?: {
      city?: string;
      country?: string;
      online?: boolean;
    };
  }
]
```

**Example Access:**
- Use `{{#each needs}}...{{/each}}` to loop through needs
- Within loop: `{{name}}`, `{{quantity}}`, `{{unit}}`, `{{description}}`

### `locations` (QuestLocation[])

Array of user locations:

```typescript
[
  {
    city?: string;
    state_province?: string;
    country?: string;
    latitude?: number;
    longitude?: number;
    online?: boolean;
  }
]
```

**Example Access:**
- Use `{{#each locations}}...{{/each}}` to loop through locations
- Within loop: `{{city}}`, `{{country}}`, `{{online}}`

### `peerQuests` (Quest[])

Array of quests from other users (for inspiration):

```typescript
[
  {
    id: string;
    title: string;
    description: string;
    type: "main" | "side" | "archived";
    difficulty: "easy" | "medium" | "hard" | "epic";
    scale: "local" | "community" | "regional" | "global";
    location?: QuestLocation;
    rewards?: QuestReward[];
    tags?: string[];
  }
]
```

**Example Access:**
- Use `{{#each peerQuests}}...{{/each}}` to loop through peer quests
- Within loop: `{{title}}`, `{{scale}}`, `{{difficulty}}`

### `maxQuests` (number)

Maximum number of quests to generate (default: 5)

**Example Access:**
- `{{maxQuests}}` → 5

## Template Syntax

### Simple Variables

```
{{variableName}}
```

Example: `{{maxQuests}}` → `5`

### Nested Properties

```
{{object.property.subproperty}}
```

Example: `{{recognitionTree.name}}` → `"Personal Growth"`

### Optional Chaining

```
{{object?.property?.subproperty}}
```

Returns empty string if any part is null/undefined.

### Loops

```
{{#each arrayName}}
  - {{propertyName}}
{{/each}}
```

Example:
```
{{#each capacities}}
- {{name}} ({{quantity}} {{unit}})
{{/each}}
```

This will output:
```
- Web Development (10 hours)
- Design Skills (5 projects)
```

### Conditional Values

You can use JavaScript-like expressions in template logic, but for simplicity, handle missing values in your prompt text:

```
{{property || 'default value'}}
```

## Prompt Configuration File

Custom prompts are stored in: `config/quest-prompt-config.json`

### Structure

```json
{
  "active": true,
  "systemPrompt": "Your system prompt here...",
  "userPrompt": "Your user prompt here...",
  "temperature": 0.7,
  "maxTokens": 2000,
  "metadata": {
    "createdBy": "Quest Maker Name",
    "createdAt": 1234567890,
    "updatedBy": "Quest Maker Name",
    "updatedAt": 1234567890,
    "version": "1.0",
    "description": "Brief description of this prompt design"
  }
}
```

### Fields

- **active** (boolean): Set to `true` to use custom prompts, `false` to use defaults
- **systemPrompt** (string): Defines the AI's role and behavior
- **userPrompt** (string): The actual request with user data
- **temperature** (number, 0-2): Controls creativity (default: 0.7)
- **maxTokens** (number): Maximum response length (default: 2000)
- **metadata** (object): Optional tracking information

## Output Format

The AI must return quests as a JSON array matching this structure:

```json
[
  {
    "id": "unique-id",
    "title": "Quest Title",
    "description": "Detailed description...",
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
        "description": "What the user gains..."
      }
    ],
    "tags": ["relevant", "tags"],
    "relatedCapacities": ["capacity-id-1"],
    "relatedNeeds": ["need-id-1"],
    "relatedTreeNodes": ["tree-node-id-1"]
  }
]
```

**IMPORTANT**: Your prompt must instruct the AI to return ONLY the JSON array, with no additional text.

## Example Custom Prompt

### System Prompt

```
You are a creative quest designer for the Free Association platform. Your role is to inspire users with meaningful, actionable quests that align with their values and capacities.

Design principles:
- Quests should feel personal and meaningful
- Leverage the user's existing capacities
- Connect quests to their highest-valued recognition tree nodes
- Consider geographic context
- Encourage collaboration when peer quests are available
```

### User Prompt

```
Generate {{maxQuests}} personalized quests for this user based on their values and capacities.

Their Values:
{{#each recognitionTree.children}}
- {{name}} ({{points}} points)
{{/each}}

What They Can Offer:
{{#each capacities}}
- {{name}}: {{description}}
{{/each}}

What They Need:
{{#each needs}}
- {{name}}: {{description}}
{{/each}}

Their Location:
{{#each locations}}
- {{city}}, {{country}}
{{/each}}

Generate quests as a JSON array matching this structure:
{
  "id": "unique-id",
  "title": "Quest Title",
  "description": "Detailed description",
  "type": "main" or "side",
  "difficulty": "easy" | "medium" | "hard" | "epic",
  "scale": "local" | "community" | "regional" | "global",
  "location": {"city": "...", "country": "...", "online": false},
  "rewards": [{"description": "..."}],
  "tags": [],
  "relatedCapacities": [],
  "relatedNeeds": [],
  "relatedTreeNodes": []
}

RESPOND WITH ONLY THE JSON ARRAY, NO OTHER TEXT.
```

## Best Practices

1. **Be Specific**: Use the available data to create context-rich prompts
2. **Structure Output**: Always require JSON array output format
3. **Test Thoroughly**: Test your prompts with various input data
4. **Document Changes**: Update metadata when modifying prompts
5. **Version Control**: Keep track of prompt versions
6. **Iterate**: Refine prompts based on generated quest quality

## Managing Custom Prompts

### Using the API (if available)

You can create an API endpoint to manage prompts, or edit the JSON file directly.

### File Location

The prompt configuration file is located at:
```
config/quest-prompt-config.json
```

### Activating Custom Prompts

1. Create/edit `config/quest-prompt-config.json`
2. Set `"active": true`
3. Provide `systemPrompt` and `userPrompt`
4. Restart the server (or the system will reload on next request)

### Deactivating Custom Prompts

Set `"active": false` in the config file to use default prompts.

## Questions?

If you need help designing prompts or understanding the data structure, contact the development team or refer to:
- `src/lib/modules/quests/quest-schemas.ts` - Quest schema definitions
- `src/lib/protocol/schemas.ts` - Recognition tree and capacity schemas
- `src/lib/server/llm/quest-prompt-config.ts` - Prompt configuration code

