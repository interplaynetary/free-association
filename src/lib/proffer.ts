import { z } from 'zod/v4';

/*
From Pekko Koskinen:

## **Slot**

#### 

#### **Definition of input:**

* Strict:  
  * Type  
    * Options (set of types)  
  * Quantity  
    * Fixed amount  
    * Array of options, fixed range (choose within the range).  
* Descriptive (definition):  
  * Categorical (Image, text descriptions etc.)  
* Combination:  
  * Combining strict and descriptive definitions (descriptive text with a proposal of a token amount, for example)

#### **Logic upon input:**

* Automatic acceptance:  
  * If input conditions are met, the slot is filled  
* "Governed" (rights-based) acceptance  
  * Right of: Offeror(s), other

## **Proffer**

#### 

#### **Executing the Pattern of composition:**

* Description:  
  * Templated:  
    * Strict  
      * Required  
        * Amount (of words, etc)  
    * Lazy  
      * Description with no checking

#### **Managing of the logic of Slots:**

* Monitor conditions set by the Proffer:  
  * Required Slots  
    * Logic: Fill the slots, fill the Proffer  
  * Optional Slots  
* If Slot conditions change, update the state of the Proffer  
  * Progress updates  
    * Slot fulfilled  
  * Proffer completed  
    * Required Slot conditions fulfilled

*/

// Base types for input definitions
const StrictTypeSchema = z.object({
	type: z.literal('strict_type'),
	options: z.array(z.string()) // set of type options
});

const StrictQuantitySchema = z.object({
	type: z.literal('strict_quantity'),
	amount: z.union([
		z.number(), // fixed amount
		z.object({
			min: z.number(),
			max: z.number()
		}) // range of options
	])
});

const DescriptiveSchema = z.object({
	type: z.literal('descriptive'),
	category: z.enum(['image', 'text', 'other']),
	description: z.string()
});

const CombinationSchema = z.object({
	type: z.literal('combination'),
	descriptive: DescriptiveSchema.omit({ type: true }),
	strict: z.union([
		StrictTypeSchema.omit({ type: true }),
		StrictQuantitySchema.omit({ type: true })
	])
});

// ID-based Proffer reference (JSON serializable)
const ProfferReferenceSchema = z.object({
	type: z.literal('proffer'),
	profferTemplateId: z.string(), // ID reference instead of full object
	completionRequirement: z.enum(['draft', 'active', 'completed']).default('completed'),
	allowPartialCompletion: z.boolean().default(false),
	minimumProgress: z.number().min(0).max(100).optional()
});

// Input definition union (now JSON serializable)
const InputDefinitionSchema = z.union([
	StrictTypeSchema,
	StrictQuantitySchema,
	DescriptiveSchema,
	CombinationSchema,
	ProfferReferenceSchema
]);

// Logic upon input types
const AutomaticAcceptanceSchema = z.object({
	type: z.literal('automatic'),
	conditions: z.array(z.string()) // conditions that must be met
});

const GovernedAcceptanceSchema = z.object({
	type: z.literal('governed'),
	rightHolder: z.enum(['offeror', 'other']),
	rightHolderIds: z.array(z.string()).optional()
});

const AcceptanceLogicSchema = z.union([AutomaticAcceptanceSchema, GovernedAcceptanceSchema]);

// Slot status enum
const SlotStatusSchema = z.enum(['empty', 'pending', 'filled']);

// Slot schema (JSON serializable)
const SlotSchema = z.object({
	id: z.string(),
	inputDefinition: InputDefinitionSchema,
	acceptanceLogic: AcceptanceLogicSchema,
	status: SlotStatusSchema,
	currentInput: z.any().optional(), // the actual input data when filled
	nestedProfferId: z.string().optional(), // ID reference to nested Proffer instance
	createdAt: z.date(),
	updatedAt: z.date()
});

// Proffer description types
const TemplatedStrictDescriptionSchema = z.object({
	type: z.literal('templated_strict'),
	requirements: z.object({
		wordCount: z.number().optional(),
		characterCount: z.number().optional(),
		format: z.string().optional()
	}),
	template: z.string()
});

const TemplatedLazyDescriptionSchema = z.object({
	type: z.literal('templated_lazy'),
	description: z.string(),
	template: z.string()
});

const ProfferDescriptionSchema = z.union([
	TemplatedStrictDescriptionSchema,
	TemplatedLazyDescriptionSchema
]);

// Proffer status enum
const ProfferStatusSchema = z.enum(['draft', 'active', 'completed', 'cancelled']);

// Progress tracking
const ProgressSchema = z.object({
	requiredSlotsFilled: z.number(),
	totalRequiredSlots: z.number(),
	optionalSlotsFilled: z.number(),
	totalOptionalSlots: z.number(),
	completionPercentage: z.number().min(0).max(100)
});

// Proffer schema (JSON serializable)
const ProfferSchema = z.object({
	id: z.string(),
	description: ProfferDescriptionSchema,
	requiredSlots: z.array(SlotSchema),
	optionalSlots: z.array(SlotSchema),
	status: ProfferStatusSchema,
	progress: ProgressSchema,
	createdAt: z.date(),
	updatedAt: z.date(),
	completedAt: z.date().optional()
});

// Proffer Registry Schema
const ProfferRegistrySchema = z.object({
	proffers: z.record(z.string(), ProfferSchema), // Map of ID -> Proffer
	metadata: z
		.object({
			version: z.string().default('1.0.0'),
			createdAt: z.date(),
			updatedAt: z.date()
		})
		.optional()
});

// Export types
export type Slot = z.infer<typeof SlotSchema>;
export type Proffer = z.infer<typeof ProfferSchema>;
export type InputDefinition = z.infer<typeof InputDefinitionSchema>;
export type AcceptanceLogic = z.infer<typeof AcceptanceLogicSchema>;
export type ProfferDescription = z.infer<typeof ProfferDescriptionSchema>;
export type Progress = z.infer<typeof ProgressSchema>;
export type ProfferReference = z.infer<typeof ProfferReferenceSchema>;
export type ProfferRegistry = z.infer<typeof ProfferRegistrySchema>;

// Resolved types (with object graph reconstructed)
export type ResolvedSlot = Omit<Slot, 'nestedProfferId'> & {
	nestedProffer?: ResolvedProffer;
};

export type ResolvedProffer = Omit<Proffer, 'requiredSlots' | 'optionalSlots'> & {
	requiredSlots: ResolvedSlot[];
	optionalSlots: ResolvedSlot[];
};

export type ResolvedInputDefinition =
	| Exclude<InputDefinition, ProfferReference>
	| (Omit<ProfferReference, 'profferTemplateId'> & {
			profferTemplate: ResolvedProffer;
	  });

// Export schemas for validation
export {
	SlotSchema,
	ProfferSchema,
	InputDefinitionSchema,
	AcceptanceLogicSchema,
	ProfferDescriptionSchema,
	ProgressSchema,
	SlotStatusSchema,
	ProfferStatusSchema,
	ProfferReferenceSchema,
	ProfferRegistrySchema
};

// Registry Management
export class ProfferRegistryManager {
	private registry: Map<string, Proffer> = new Map();

	// Add a Proffer to the registry
	addProffer(proffer: Proffer): void {
		this.registry.set(proffer.id, proffer);
	}

	// Get a Proffer by ID
	getProffer(id: string): Proffer | undefined {
		return this.registry.get(id);
	}

	// Get all Proffers
	getAllProffers(): Proffer[] {
		return Array.from(this.registry.values());
	}

	// Remove a Proffer
	removeProffer(id: string): boolean {
		return this.registry.delete(id);
	}

	// Clear all Proffers
	clear(): void {
		this.registry.clear();
	}

	// Export registry to JSON-serializable format
	toJSON(): ProfferRegistry {
		const proffers: Record<string, Proffer> = {};
		this.registry.forEach((proffer, id) => {
			proffers[id] = proffer;
		});

		return {
			proffers,
			metadata: {
				version: '1.0.0',
				createdAt: new Date(),
				updatedAt: new Date()
			}
		};
	}

	// Import registry from JSON
	fromJSON(registryData: ProfferRegistry): void {
		this.clear();
		Object.entries(registryData.proffers).forEach(([id, proffer]) => {
			this.registry.set(id, proffer);
		});
	}

	// Resolve a Proffer with its dependencies
	resolveProffer(id: string): ResolvedProffer | undefined {
		const proffer = this.getProffer(id);
		if (!proffer) return undefined;

		return this.resolveProffer_internal(proffer);
	}

	private resolveProffer_internal(proffer: Proffer): ResolvedProffer {
		const resolveSlot = (slot: Slot): ResolvedSlot => {
			const resolved: ResolvedSlot = { ...slot };

			// Resolve nested Proffer if present
			if (slot.nestedProfferId) {
				const nestedProffer = this.getProffer(slot.nestedProfferId);
				if (nestedProffer) {
					resolved.nestedProffer = this.resolveProffer_internal(nestedProffer);
				}
			}

			return resolved;
		};

		return {
			...proffer,
			requiredSlots: proffer.requiredSlots.map(resolveSlot),
			optionalSlots: proffer.optionalSlots.map(resolveSlot)
		};
	}

	// Validate DAG structure across all Proffers in registry
	validateAllDAGs(): { isValid: boolean; errors: string[] } {
		const errors: string[] = [];

		this.registry.forEach((proffer, id) => {
			const validation = this.validateProfferDAG(id);
			if (!validation.isValid) {
				errors.push(`Proffer ${id}: ${validation.cyclePath?.join(' → ')}`);
			}
		});

		return {
			isValid: errors.length === 0,
			errors
		};
	}

	// DAG validation using registry resolution
	private validateProfferDAG(
		profferId: string,
		visited: Set<string> = new Set(),
		visiting: Set<string> = new Set(),
		path: string[] = []
	): { isValid: boolean; cyclePath?: string[] } {
		const currentPath = [...path, profferId];

		// Cycle detected
		if (visiting.has(profferId)) {
			const cycleStart = currentPath.indexOf(profferId);
			return {
				isValid: false,
				cyclePath: currentPath.slice(cycleStart)
			};
		}

		// Already validated
		if (visited.has(profferId)) {
			return { isValid: true };
		}

		const proffer = this.getProffer(profferId);
		if (!proffer) {
			return { isValid: false, cyclePath: [`Unknown Proffer: ${profferId}`] };
		}

		visiting.add(profferId);

		// Check all nested Proffer references
		const allSlots = [...proffer.requiredSlots, ...proffer.optionalSlots];

		for (const slot of allSlots) {
			if (slot.inputDefinition.type === 'proffer') {
				const nestedId = slot.inputDefinition.profferTemplateId;
				const validation = this.validateProfferDAG(nestedId, visited, visiting, currentPath);

				if (!validation.isValid) {
					return validation;
				}
			}
		}

		visiting.delete(profferId);
		visited.add(profferId);

		return { isValid: true };
	}

	// Get dependency chain for a Proffer
	getDependencies(profferId: string): string[] {
		const proffer = this.getProffer(profferId);
		if (!proffer) return [];

		const dependencies: string[] = [];
		const allSlots = [...proffer.requiredSlots, ...proffer.optionalSlots];

		for (const slot of allSlots) {
			if (slot.inputDefinition.type === 'proffer') {
				dependencies.push(slot.inputDefinition.profferTemplateId);
			}
		}

		return dependencies;
	}

	// Get topological order of all Proffers
	getExecutionOrder(): string[] {
		const allIds = Array.from(this.registry.keys());
		const inDegree = new Map<string, number>();
		const graph = new Map<string, string[]>();

		// Initialize
		allIds.forEach((id) => {
			inDegree.set(id, 0);
			graph.set(id, []);
		});

		// Build dependency graph
		allIds.forEach((id) => {
			const dependencies = this.getDependencies(id);
			dependencies.forEach((dep) => {
				if (graph.has(dep)) {
					graph.get(dep)!.push(id);
					inDegree.set(id, (inDegree.get(id) || 0) + 1);
				}
			});
		});

		// Kahn's algorithm
		const queue: string[] = [];
		const result: string[] = [];

		inDegree.forEach((degree, id) => {
			if (degree === 0) {
				queue.push(id);
			}
		});

		while (queue.length > 0) {
			const current = queue.shift()!;
			result.push(current);

			const neighbors = graph.get(current) || [];
			neighbors.forEach((neighbor) => {
				const newDegree = (inDegree.get(neighbor) || 0) - 1;
				inDegree.set(neighbor, newDegree);

				if (newDegree === 0) {
					queue.push(neighbor);
				}
			});
		}

		return result;
	}
}

// Global registry instance
export const globalProfferRegistry = new ProfferRegistryManager();

// Utility functions for working with JSON-serializable Proffers
export const serializeProfferRegistry = (registry: ProfferRegistryManager): string => {
	return JSON.stringify(registry.toJSON(), null, 2);
};

export const deserializeProfferRegistry = (jsonString: string): ProfferRegistryManager => {
	const registryData = JSON.parse(jsonString) as ProfferRegistry;
	const registry = new ProfferRegistryManager();
	registry.fromJSON(registryData);
	return registry;
};

// Enhanced Proffer creation with registry integration
export const createProfferWithValidation = (
	profferData: Omit<Proffer, 'createdAt' | 'updatedAt'>,
	registry: ProfferRegistryManager = globalProfferRegistry,
	maxDepth: number = 10
): { proffer?: Proffer; errors: string[] } => {
	const errors: string[] = [];

	try {
		// Create the proffer
		const proffer: Proffer = {
			...profferData,
			createdAt: new Date(),
			updatedAt: new Date()
		};

		// Validate schema
		ProfferSchema.parse(proffer);

		// Add to registry temporarily for validation
		const hadExisting = registry.getProffer(proffer.id) !== undefined;
		registry.addProffer(proffer);

		// Validate DAG
		const dagValidation = registry.validateAllDAGs();
		if (!dagValidation.isValid) {
			// Remove from registry if validation fails
			if (!hadExisting) {
				registry.removeProffer(proffer.id);
			}
			errors.push(...dagValidation.errors);
		}

		if (errors.length === 0) {
			return { proffer, errors: [] };
		}
	} catch (zodError) {
		errors.push(
			`Schema validation failed: ${zodError instanceof Error ? zodError.message : String(zodError)}`
		);
	}

	return { errors };
};

// Legacy compatibility functions (for migration)
export const validateNestedProfferSlot = (
	slot: Slot,
	registry: ProfferRegistryManager = globalProfferRegistry
): boolean => {
	if (slot.inputDefinition.type !== 'proffer' || !slot.nestedProfferId) {
		return false;
	}

	const nestedProffer = registry.getProffer(slot.nestedProfferId);
	if (!nestedProffer) return false;

	const requirement = slot.inputDefinition.completionRequirement;

	switch (requirement) {
		case 'draft':
			return nestedProffer.status !== 'cancelled';
		case 'active':
			return ['active', 'completed'].includes(nestedProffer.status);
		case 'completed':
			return nestedProffer.status === 'completed';
		default:
			return false;
	}
};

export const calculateNestedProgress = (
	proffer: Proffer,
	registry: ProfferRegistryManager = globalProfferRegistry
): Progress => {
	const allSlots = [...proffer.requiredSlots, ...proffer.optionalSlots];
	let totalNestedProgress = 0;
	let nestedSlotCount = 0;
	let requiredFilled = 0;
	let optionalFilled = 0;

	// Calculate progress including nested Proffers
	allSlots.forEach((slot) => {
		const isFilled = slot.status === 'filled';
		const isRequired = proffer.requiredSlots.includes(slot);

		if (isRequired && isFilled) requiredFilled++;
		if (!isRequired && isFilled) optionalFilled++;

		if (slot.inputDefinition.type === 'proffer' && slot.nestedProfferId) {
			const nestedProffer = registry.getProffer(slot.nestedProfferId);
			if (nestedProffer) {
				nestedSlotCount++;
				totalNestedProgress += nestedProffer.progress.completionPercentage;
			}
		}
	});

	const baseCompletionPercentage =
		proffer.requiredSlots.length > 0 ? (requiredFilled / proffer.requiredSlots.length) * 100 : 100;

	// Weight nested progress into overall completion
	let finalCompletionPercentage = baseCompletionPercentage;
	if (nestedSlotCount > 0) {
		const averageNestedProgress = totalNestedProgress / nestedSlotCount;
		finalCompletionPercentage = (baseCompletionPercentage + averageNestedProgress) / 2;
	}

	return {
		requiredSlotsFilled: requiredFilled,
		totalRequiredSlots: proffer.requiredSlots.length,
		optionalSlotsFilled: optionalFilled,
		totalOptionalSlots: proffer.optionalSlots.length,
		completionPercentage: Math.round(finalCompletionPercentage)
	};
};
