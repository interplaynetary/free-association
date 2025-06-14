import { z } from 'zod/v4';

/*
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

// Input definition union
const InputDefinitionSchema = z.union([
	StrictTypeSchema,
	StrictQuantitySchema,
	DescriptiveSchema,
	CombinationSchema
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

// Slot schema
const SlotSchema = z.object({
	id: z.string(),
	inputDefinition: InputDefinitionSchema,
	acceptanceLogic: AcceptanceLogicSchema,
	status: SlotStatusSchema,
	currentInput: z.any().optional(), // the actual input data when filled
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

// Proffer schema
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

// Export types
export type Slot = z.infer<typeof SlotSchema>;
export type Proffer = z.infer<typeof ProfferSchema>;
export type InputDefinition = z.infer<typeof InputDefinitionSchema>;
export type AcceptanceLogic = z.infer<typeof AcceptanceLogicSchema>;
export type ProfferDescription = z.infer<typeof ProfferDescriptionSchema>;
export type Progress = z.infer<typeof ProgressSchema>;

// Export schemas for validation
export {
	SlotSchema,
	ProfferSchema,
	InputDefinitionSchema,
	AcceptanceLogicSchema,
	ProfferDescriptionSchema,
	ProgressSchema,
	SlotStatusSchema,
	ProfferStatusSchema
};
