import { z } from 'zod/v4';

// VSM Theoretical Foundations
const RegulatoryAphorismsSchema = z.object({
	black_box_function_understanding: z
		.literal(true)
		.describe(
			'It is not necessary to enter the black box to understand the nature of the function it performs'
		),
	black_box_variety_calculation: z
		.literal(true)
		.describe(
			'It is not necessary to enter the black box to calculate the variety that it potentially may generate'
		)
});

const PrinciplesOfOrganizationSchema = z.object({
	variety_equalization: z.object({
		managerial_variety: z.number().min(0),
		operational_variety: z.number().min(0),
		environmental_variety: z.number().min(0),
		designed_to_equate: z
			.boolean()
			.describe('Varieties should be designed to equate with minimum damage to people and cost')
	}),
	channel_capacity: z
		.object({
			information_channels: z.array(
				z.object({
					channel_id: z.string(),
					transmission_capacity: z.number().min(0),
					originating_variety: z.number().min(0),
					meets_requirement: z
						.boolean()
						.describe(
							"Channel capacity must exceed originating subsystem's variety generation rate"
						)
				})
			)
		})
		.refine(
			(data) =>
				data.information_channels.every((ch) => ch.transmission_capacity > ch.originating_variety),
			{ message: 'All channels must have higher capacity than originating variety' }
		),
	transduction_variety: z
		.object({
			transducers: z.array(
				z.object({
					transducer_id: z.string(),
					channel_variety: z.number().min(0),
					transducer_variety: z.number().min(0),
					adequate: z
						.boolean()
						.describe('Transducer variety must be at least equivalent to channel variety')
				})
			)
		})
		.refine((data) => data.transducers.every((t) => t.transducer_variety >= t.channel_variety), {
			message: 'All transducers must have adequate variety'
		}),
	cyclical_maintenance: z
		.object({
			cycle_time: z.number().min(0).describe('Time for complete cycle operation'),
			maximum_delay: z.number().min(0).describe('Maximum acceptable delay'),
			maintained_without_delays: z
				.boolean()
				.describe('First three principles must be cyclically maintained without delays')
		})
		.refine((data) => data.cycle_time <= data.maximum_delay, {
			message: 'Cycle time must not exceed maximum allowable delay'
		})
});

const AxiomsOfManagementSchema = z.object({
	first_axiom: z
		.object({
			horizontal_variety: z
				.number()
				.min(0)
				.describe('Sum of variety disposed by n operational elements (System 1s)'),
			vertical_variety: z
				.number()
				.min(0)
				.describe('Sum of variety disposed by six vertical components'),
			varieties_equal: z.boolean().describe('Horizontal variety equals vertical variety')
		})
		.refine((data) => Math.abs(data.horizontal_variety - data.vertical_variety) < 0.01, {
			message: 'First Axiom: Horizontal and vertical varieties must be equal'
		}),
	second_axiom: z
		.object({
			system3_variety: z.number().min(0).describe('Variety disposed by System 3'),
			system4_variety: z.number().min(0).describe('Variety disposed by System 4'),
			varieties_equal: z.boolean().describe('System 3 and 4 varieties must be equal')
		})
		.refine((data) => Math.abs(data.system3_variety - data.system4_variety) < 0.01, {
			message: 'Second Axiom: System 3 and 4 varieties must be equal'
		}),
	third_axiom: z
		.object({
			system5_variety: z.number().min(0).describe('Variety disposed by System 5'),
			residual_variety: z.number().min(0).describe('Residual variety from Second Axiom operation'),
			varieties_equal: z.boolean().describe('System 5 variety equals residual variety')
		})
		.refine((data) => Math.abs(data.system5_variety - data.residual_variety) < 0.01, {
			message: 'Third Axiom: System 5 variety must equal residual variety'
		})
});

const LawOfCohesionSchema = z.object({
	recursion_pairs: z
		.array(
			z.object({
				recursion_x: z.number().int().min(0),
				recursion_y: z.number().int().min(0),
				system1_variety_accessible_to_system3_x: z.number().min(0),
				sum_metasystem_variety_y: z.number().min(0),
				cohesion_maintained: z
					.boolean()
					.describe(
						'System 1 variety accessible to System 3 of recursion x equals sum of metasystems of recursion y'
					)
			})
		)
		.refine(
			(data) =>
				data.every(
					(pair) =>
						Math.abs(pair.system1_variety_accessible_to_system3_x - pair.sum_metasystem_variety_y) <
						0.01
				),
			{ message: 'Law of Cohesion must be maintained for all recursive pairs' }
		)
});

// Enhanced Variety Management with Requisite Variety
const RequisiteVarietySchema = z.object({
	ashbys_law_compliance: z
		.object({
			controller_variety: z.number().min(0).describe('Variety available to controller'),
			disturbance_variety: z.number().min(0).describe('Variety of disturbances to be regulated'),
			requisite_variety_satisfied: z
				.boolean()
				.describe('Controller variety must match or exceed disturbance variety')
		})
		.refine((data) => data.controller_variety >= data.disturbance_variety, {
			message: "Ashby's Law: Controller variety must be at least as great as disturbance variety"
		}),
	variety_amplifiers: z.array(
		z.object({
			id: z.string(),
			input_variety: z.number().min(0),
			output_variety: z.number().min(0),
			amplification_factor: z.number().min(1)
		})
	),
	variety_attenuators: z.array(
		z.object({
			id: z.string(),
			input_variety: z.number().min(0),
			output_variety: z.number().min(0),
			attenuation_factor: z.number().min(0).max(1)
		})
	)
});

// Enhanced Metalanguage Stack
const MetalanguageStackSchema = z.object({
	current_level: z.number().int().min(0),
	undecidability_resolution: z.object({
		undecidable_problems: z.array(
			z.object({
				problem_id: z.string(),
				description: z.string(),
				current_level_adequate: z.boolean(),
				escalation_required: z.boolean(),
				target_level: z.number().int().min(0).optional()
			})
		),
		resolution_capacity: z
			.number()
			.min(0)
			.describe('Variety available for resolving undecidability')
	}),
	chaitin_algorithmic_information: z.object({
		information_content: z.number().min(0).describe('Algorithmic information theory measure'),
		complexity_measure: z.number().min(0).describe('Kolmogorov complexity measure'),
		randomness_coefficient: z.number().min(0).max(1).describe('Measure of randomness in the system')
	})
});

// Core performance metrics from Beer's triple vector
const PerformanceMetricsSchema = z
	.object({
		actuality: z
			.number()
			.min(0)
			.describe(
				'What we are managing to do now, with existing resources, under existing constraints'
			),
		capability: z
			.number()
			.min(0)
			.describe(
				'What we could be doing (still right now) with existing resources, under existing constraints, if we really worked at it'
			),
		potentiality: z
			.number()
			.min(0)
			.describe('What we ought to be doing by developing our resources and removing constraints')
	})
	.refine((data) => data.actuality <= data.capability && data.capability <= data.potentiality, {
		message: 'Performance metrics must satisfy: actuality ≤ capability ≤ potentiality'
	});

// Derived performance ratios
const PerformanceRatiosSchema = z.object({
	productivity: z.number().min(0).max(1).describe('Ratio of actuality to capability'),
	latency: z.number().min(0).max(1).describe('Ratio of capability to potentiality'),
	performance: z
		.number()
		.min(0)
		.max(1)
		.describe('Ratio of actuality to potentiality (also productivity × latency)')
});

// Enhanced Variety measures for requisite variety management
const VarietySchema = z.object({
	states: z.number().int().min(1).describe('Number of possible states'),
	capacity: z.number().min(0).describe('Information transmission capacity'),
	complexity: z.number().min(0).describe('Measure of system complexity'),
	requisite_variety: RequisiteVarietySchema
});

// Algedonic alerts (pain/pleasure signals)
const AlgedonicAlertSchema = z.object({
	id: z.string().uuid(),
	type: z
		.enum(['pain', 'pleasure'])
		.describe('Pain for performance deviation, pleasure for improvement'),
	severity: z.enum(['low', 'medium', 'high', 'critical']),
	deviation: z.number().describe('Statistical deviation from capability'),
	timestamp: z.date(),
	escalated: z.boolean().default(false),
	resolved: z.boolean().default(false),
	source_system: z.enum(['system1', 'system2', 'system3', 'system4', 'system5']),
	message: z.string().optional(),
	corrective_actions: z.array(z.string()).default([]),
	escalation_path: z
		.array(
			z.object({
				level: z.number().int().min(0),
				timestamp: z.date(),
				action_taken: z.string()
			})
		)
		.default([])
});

// Enhanced Information channels for System 2
const InformationChannelSchema = z.object({
	id: z.string(),
	from_system: z.string(),
	to_system: z.string(),
	capacity: z.number().min(0).describe('Information transmission capacity'),
	current_load: z.number().min(0).describe('Current information load'),
	transduction_variety: z.number().min(0).describe('Variety of the transducer'),
	active: z.boolean().default(true),
	channel_principles_compliance: z.object({
		capacity_exceeds_originating_variety: z.boolean(),
		transduction_adequate: z.boolean(),
		cyclical_maintenance: z.boolean()
	})
});

// Resource allocation for System 3
const ResourceSchema = z
	.object({
		id: z.string(),
		type: z.enum(['human', 'financial', 'technological', 'informational', 'physical']),
		name: z.string(),
		quantity: z.number().min(0),
		allocated: z.number().min(0),
		reserved: z.number().min(0).default(0),
		constraints: z.array(z.string()).default([])
	})
	.refine((data) => data.allocated + data.reserved <= data.quantity, {
		message: 'Allocated and reserved resources cannot exceed total quantity'
	});

// Environmental factors for System 4
const EnvironmentalFactorSchema = z.object({
	id: z.string(),
	category: z.enum(['technological', 'economic', 'political', 'social', 'ecological', 'legal']),
	name: z.string(),
	impact_level: z.enum(['low', 'medium', 'high', 'critical']),
	trend: z.enum(['declining', 'stable', 'growing', 'volatile']),
	monitoring_frequency: z.enum(['continuous', 'daily', 'weekly', 'monthly', 'quarterly']),
	adaptation_required: z.boolean().default(false),
	last_assessment: z.date(),
	description: z.string().optional()
});

// Policy directive for System 5
const PolicyDirectiveSchema = z.object({
	id: z.string(),
	title: z.string(),
	description: z.string(),
	priority: z.enum(['low', 'medium', 'high', 'critical']),
	affects_systems: z.array(z.enum(['system1', 'system2', 'system3', 'system4', 'system5'])),
	created_date: z.date(),
	effective_date: z.date(),
	review_date: z.date().optional(),
	status: z.enum(['draft', 'active', 'suspended', 'archived']),
	success_criteria: z.array(z.string()),
	resource_requirements: z.array(z.string()).default([])
});

// System 1: Primary Activities (Operational Level)
const System1Schema = z.object({
	id: z.string(),
	name: z.string(),
	function: z.string().describe('Key transformation performed by this primary activity'),
	performance_metrics: PerformanceMetricsSchema,
	performance_ratios: PerformanceRatiosSchema,
	variety: VarietySchema,
	resources: z.array(ResourceSchema),
	sub_systems: z
		.array(z.lazy(() => ViableSystemSchema))
		.default([])
		.describe('Recursive viable systems within this System 1'),
	active: z.boolean().default(true),
	last_audit: z.date().optional()
});

// System 2: Information and Coordination
const System2Schema = z.object({
	information_channels: z.array(InformationChannelSchema),
	shared_resources: z.array(ResourceSchema),
	scheduling_functions: z.array(
		z.object({
			id: z.string(),
			resource_id: z.string(),
			allocated_to: z.string(),
			start_time: z.date(),
			end_time: z.date(),
			priority: z.enum(['low', 'medium', 'high', 'urgent'])
		})
	),
	coordination_protocols: z.array(z.string()).default([]),
	variety: VarietySchema,
	principles_compliance: PrinciplesOfOrganizationSchema
});

// Enhanced System 3: Control and Coordination
const System3Schema = z.object({
	rules: z.array(z.string()),
	resources: z.array(ResourceSchema),
	rights: z.array(z.string()),
	responsibilities: z.array(z.string()),
	audit_functions: z.array(
		z.object({
			id: z.string(),
			target_system: z.string(),
			audit_type: z.enum(['performance', 'compliance', 'resource_usage', 'quality']),
			frequency: z.enum(['continuous', 'daily', 'weekly', 'monthly', 'quarterly']),
			last_audit: z.date(),
			next_audit: z.date(),
			findings: z.array(z.string()).default([])
		})
	),
	system3_star: z.object({
		sporadic_audits: z
			.array(
				z.object({
					id: z.string(),
					target_system1: z.string(),
					audit_date: z.date(),
					good_times_baseline: z.object({
						performance_metrics: PerformanceMetricsSchema,
						date_established: z.date()
					}),
					current_performance: PerformanceMetricsSchema,
					comparison_results: z.object({
						performance_deviation: z.number(),
						requires_attention: z.boolean(),
						recommended_actions: z.array(z.string())
					})
				})
			)
			.describe('System 3* sporadic audit functions')
	}),
	variety: VarietySchema,
	metasystem_interface: z.object({
		system4_requests: z.array(z.string()).default([]),
		system5_directives: z.array(z.string()).default([])
	})
});

// System 4: Intelligence and Adaptation
const System4Schema = z.object({
	environmental_monitoring: z.array(EnvironmentalFactorSchema),
	research_activities: z.array(
		z.object({
			id: z.string(),
			title: z.string(),
			focus_area: z.string(),
			status: z.enum(['proposed', 'active', 'completed', 'cancelled']),
			findings: z.array(z.string()).default([]),
			recommendations: z.array(z.string()).default([])
		})
	),
	development_initiatives: z.array(
		z.object({
			id: z.string(),
			title: z.string(),
			objective: z.string(),
			target_potential: z.number().min(0),
			current_progress: z.number().min(0).max(100),
			resources_required: z.array(z.string()),
			timeline: z.object({
				start_date: z.date(),
				end_date: z.date(),
				milestones: z.array(
					z.object({
						name: z.string(),
						date: z.date(),
						completed: z.boolean().default(false)
					})
				)
			})
		})
	),
	variety: VarietySchema,
	innovation_capacity: z.number().min(0).describe('Capacity to realize potential')
});

// System 5: Policy and Identity
const System5Schema = z.object({
	policy_directives: z.array(PolicyDirectiveSchema),
	identity: z.object({
		mission: z.string(),
		vision: z.string(),
		values: z.array(z.string()),
		purpose: z.string()
	}),
	governance_structure: z.object({
		decision_making_process: z.string(),
		authority_levels: z.array(
			z.object({
				level: z.string(),
				scope: z.array(z.string()),
				constraints: z.array(z.string()).default([])
			})
		),
		accountability_mechanisms: z.array(z.string())
	}),
	balancing_mechanisms: z.array(
		z.object({
			id: z.string(),
			name: z.string(),
			competing_demands: z.array(z.string()),
			resolution_criteria: z.array(z.string()),
			active: z.boolean().default(true)
		})
	),
	variety: VarietySchema,
	metalanguage_level: z
		.number()
		.int()
		.min(0)
		.describe('Level of recursion in the metalanguage stack'),
	metalanguage_stack: MetalanguageStackSchema
});

// Environment schema
const EnvironmentSchema = z.object({
	id: z.string(),
	name: z.string(),
	domain_of_action: z.string().describe('The context that grounds internal interactions'),
	external_factors: z.array(EnvironmentalFactorSchema),
	stakeholders: z.array(
		z.object({
			id: z.string(),
			name: z.string(),
			type: z.enum(['customer', 'supplier', 'regulator', 'partner', 'competitor', 'community']),
			influence_level: z.enum(['low', 'medium', 'high', 'critical']),
			relationship_quality: z.enum(['poor', 'fair', 'good', 'excellent']),
			interests: z.array(z.string())
		})
	),
	market_conditions: z.object({
		volatility: z.enum(['low', 'medium', 'high']),
		growth_rate: z.number(),
		competition_level: z.enum(['low', 'medium', 'high', 'intense']),
		regulatory_stability: z.enum(['stable', 'changing', 'volatile'])
	}),
	variety: VarietySchema
});

// Main Viable System schema with theoretical foundations
const ViableSystemSchema: z.ZodSchema = z.object({
	id: z.string(),
	name: z.string(),
	description: z.string().optional(),
	recursion_level: z.number().int().min(0).describe('Level in the recursive hierarchy'),

	// VSM Theoretical Foundations
	regulatory_aphorisms: RegulatoryAphorismsSchema,
	principles_of_organization: PrinciplesOfOrganizationSchema,
	axioms_of_management: AxiomsOfManagementSchema,
	law_of_cohesion: LawOfCohesionSchema,

	// The five subsystems
	system1: z
		.array(System1Schema)
		.min(1)
		.describe('Primary activities - each is itself a viable system'),
	system2: System2Schema.describe('Information channels and coordination'),
	system3: System3Schema.describe(
		'Structures, controls, rules, resources, rights and responsibilities'
	),
	system4: System4Schema.describe('Environmental monitoring and adaptation'),
	system5: System5Schema.describe('Policy decisions and organizational steering'),

	// Environment
	environment: EnvironmentSchema,

	// Algedonic alerts
	algedonic_alerts: z.array(AlgedonicAlertSchema).default([]),

	// Meta-system relationships
	parent_system: z.string().optional().describe('ID of containing viable system'),
	child_systems: z.array(z.string()).default([]).describe('IDs of contained viable systems'),

	// Overall system health
	viability_status: z.enum(['thriving', 'viable', 'at_risk', 'failing']),
	last_assessment: z.date(),

	// Enhanced Variety management
	total_variety: VarietySchema.describe('Aggregate variety of the entire system'),
	variety_balance: z.object({
		operational_variety: z.number().min(0),
		managerial_variety: z.number().min(0),
		environmental_variety: z.number().min(0),
		balanced: z.boolean().describe('Whether requisite variety condition is satisfied'),
		ashbys_law_compliance: z
			.boolean()
			.describe("Whether Ashby's Law of Requisite Variety is satisfied")
	})
});

// Enhanced calculation functions
const calculatePerformanceRatios = (
	metrics: z.infer<typeof PerformanceMetricsSchema>
): z.infer<typeof PerformanceRatiosSchema> => {
	const productivity = metrics.capability > 0 ? metrics.actuality / metrics.capability : 0;
	const latency = metrics.potentiality > 0 ? metrics.capability / metrics.potentiality : 0;
	const performance = metrics.potentiality > 0 ? metrics.actuality / metrics.potentiality : 0;

	return { productivity, latency, performance };
};

// Variety calculations with theoretical validation
const calculateTotalVariety = (systems: any): z.infer<typeof VarietySchema> => {
	const totalStates = systems.system1.reduce((acc: number, s1: any) => acc + s1.variety.states, 0);
	const totalCapacity =
		systems.system2.variety.capacity +
		systems.system3.variety.capacity +
		systems.system4.variety.capacity +
		systems.system5.variety.capacity;
	const avgComplexity =
		(systems.system2.variety.complexity +
			systems.system3.variety.complexity +
			systems.system4.variety.complexity +
			systems.system5.variety.complexity) /
		4;

	return {
		states: totalStates,
		capacity: totalCapacity,
		complexity: avgComplexity,
		requisite_variety: {
			ashbys_law_compliance: {
				controller_variety: totalCapacity,
				disturbance_variety: systems.environment.variety.complexity,
				requisite_variety_satisfied: totalCapacity >= systems.environment.variety.complexity
			},
			variety_amplifiers: [],
			variety_attenuators: []
		}
	};
};

// Export schemas and types
export {
	ViableSystemSchema,
	System1Schema,
	System2Schema,
	System3Schema,
	System4Schema,
	System5Schema,
	EnvironmentSchema,
	PerformanceMetricsSchema,
	PerformanceRatiosSchema,
	VarietySchema,
	AlgedonicAlertSchema,
	InformationChannelSchema,
	ResourceSchema,
	EnvironmentalFactorSchema,
	PolicyDirectiveSchema,
	RegulatoryAphorismsSchema,
	PrinciplesOfOrganizationSchema,
	AxiomsOfManagementSchema,
	LawOfCohesionSchema,
	RequisiteVarietySchema,
	MetalanguageStackSchema,
	calculatePerformanceRatios,
	calculateTotalVariety
};

export type ViableSystem = z.infer<typeof ViableSystemSchema>;
export type System1 = z.infer<typeof System1Schema>;
export type System2 = z.infer<typeof System2Schema>;
export type System3 = z.infer<typeof System3Schema>;
export type System4 = z.infer<typeof System4Schema>;
export type System5 = z.infer<typeof System5Schema>;
export type Environment = z.infer<typeof EnvironmentSchema>;
export type PerformanceMetrics = z.infer<typeof PerformanceMetricsSchema>;
export type PerformanceRatios = z.infer<typeof PerformanceRatiosSchema>;
export type Variety = z.infer<typeof VarietySchema>;
export type AlgedonicAlert = z.infer<typeof AlgedonicAlertSchema>;
export type InformationChannel = z.infer<typeof InformationChannelSchema>;
export type Resource = z.infer<typeof ResourceSchema>;
export type EnvironmentalFactor = z.infer<typeof EnvironmentalFactorSchema>;
export type PolicyDirective = z.infer<typeof PolicyDirectiveSchema>;
export type RegulatoryAphorisms = z.infer<typeof RegulatoryAphorismsSchema>;
export type PrinciplesOfOrganization = z.infer<typeof PrinciplesOfOrganizationSchema>;
export type AxiomsOfManagement = z.infer<typeof AxiomsOfManagementSchema>;
export type LawOfCohesion = z.infer<typeof LawOfCohesionSchema>;
export type RequisiteVariety = z.infer<typeof RequisiteVarietySchema>;
export type MetalanguageStack = z.infer<typeof MetalanguageStackSchema>;
