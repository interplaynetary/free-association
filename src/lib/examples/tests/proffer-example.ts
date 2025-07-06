import {
	type Proffer,
	type Slot,
	ProfferRegistryManager,
	createProfferWithValidation,
	globalProfferRegistry,
	serializeProfferRegistry,
	deserializeProfferRegistry,
	calculateNestedProgress
} from '../experiments/proffer';

// Helper function to create a slot
function createSlot(
	id: string,
	inputType: 'text' | 'number' | 'proffer',
	required: boolean = true,
	profferTemplateId?: string
): Slot {
	const baseSlot = {
		id,
		status: 'empty' as const,
		createdAt: new Date(),
		updatedAt: new Date()
	};

	if (inputType === 'text') {
		return {
			...baseSlot,
			inputDefinition: {
				type: 'descriptive' as const,
				category: 'text' as const,
				description: `Text input for ${id}`
			},
			acceptanceLogic: {
				type: 'automatic' as const,
				conditions: ['non-empty-text']
			}
		};
	}

	if (inputType === 'number') {
		return {
			...baseSlot,
			inputDefinition: {
				type: 'strict_quantity' as const,
				amount: { min: 1, max: 100 }
			},
			acceptanceLogic: {
				type: 'automatic' as const,
				conditions: ['valid-number-range']
			}
		};
	}

	if (inputType === 'proffer' && profferTemplateId) {
		return {
			...baseSlot,
			inputDefinition: {
				type: 'proffer' as const,
				profferTemplateId,
				completionRequirement: 'completed' as const,
				allowPartialCompletion: false
			},
			acceptanceLogic: {
				type: 'automatic' as const,
				conditions: ['proffer-completed']
			}
		};
	}

	throw new Error('Invalid slot configuration');
}

// Helper function to create a basic proffer
function createBasicProffer(
	id: string,
	title: string,
	requiredSlots: Slot[],
	optionalSlots: Slot[] = []
): Omit<Proffer, 'createdAt' | 'updatedAt'> {
	return {
		id,
		description: {
			type: 'templated_lazy',
			description: title,
			template: `Complete the proffer: ${title}`
		},
		requiredSlots,
		optionalSlots,
		status: 'draft',
		progress: {
			requiredSlotsFilled: 0,
			totalRequiredSlots: requiredSlots.length,
			optionalSlotsFilled: 0,
			totalOptionalSlots: optionalSlots.length,
			completionPercentage: 0
		}
	};
}

// Function to simulate filling a slot
function fillSlot(proffer: Proffer, slotId: string, input: any): void {
	const slot = [...proffer.requiredSlots, ...proffer.optionalSlots].find((s) => s.id === slotId);
	if (slot) {
		slot.status = 'filled';
		slot.currentInput = input;
		slot.updatedAt = new Date();

		// Update proffer progress
		const requiredFilled = proffer.requiredSlots.filter((s) => s.status === 'filled').length;
		const optionalFilled = proffer.optionalSlots.filter((s) => s.status === 'filled').length;

		proffer.progress = {
			requiredSlotsFilled: requiredFilled,
			totalRequiredSlots: proffer.requiredSlots.length,
			optionalSlotsFilled: optionalFilled,
			totalOptionalSlots: proffer.optionalSlots.length,
			completionPercentage:
				proffer.requiredSlots.length > 0
					? Math.round((requiredFilled / proffer.requiredSlots.length) * 100)
					: 100
		};

		// Update status if all required slots are filled
		if (requiredFilled === proffer.requiredSlots.length) {
			proffer.status = 'completed';
			proffer.completedAt = new Date();
		} else if (requiredFilled > 0) {
			proffer.status = 'active';
		}

		proffer.updatedAt = new Date();
	}
}

async function runProfferExample() {
	console.log('🚀 Starting Proffer System Example\n');

	// Create a new registry for this example
	const registry = new ProfferRegistryManager();

	// Example 1: Simple Task Proffer
	console.log('📝 Example 1: Creating a simple task proffer');

	const taskSlots = [
		createSlot('task-title', 'text'),
		createSlot('task-description', 'text'),
		createSlot('estimated-hours', 'number')
	];

	const taskProfferData = createBasicProffer('task-001', 'Create a new task', taskSlots);

	const taskResult = createProfferWithValidation(taskProfferData, registry);
	if (taskResult.proffer) {
		console.log(`✅ Created proffer: ${taskResult.proffer.id}`);
		console.log(`   Status: ${taskResult.proffer.status}`);
		console.log(`   Required slots: ${taskResult.proffer.requiredSlots.length}`);
	} else {
		console.log('❌ Failed to create task proffer:', taskResult.errors);
		return;
	}

	// Example 2: Project Proffer (depends on task proffer)
	console.log('\n📋 Example 2: Creating a project proffer that depends on tasks');

	const projectSlots = [
		createSlot('project-name', 'text'),
		createSlot('task-1', 'proffer', true, 'task-001'),
		createSlot('task-2', 'proffer', true, 'task-001'), // Reusing the task template
		createSlot('budget', 'number')
	];

	const projectProfferData = createBasicProffer(
		'project-001',
		'Complete a project with multiple tasks',
		projectSlots
	);

	const projectResult = createProfferWithValidation(projectProfferData, registry);
	if (projectResult.proffer) {
		console.log(`✅ Created proffer: ${projectResult.proffer.id}`);
		console.log(
			`   Dependencies: ${registry.getDependencies(projectResult.proffer.id).join(', ')}`
		);
	} else {
		console.log('❌ Failed to create project proffer:', projectResult.errors);
		return;
	}

	// Example 3: Validate DAG structure
	console.log('\n🔍 Example 3: Validating dependency structure');

	const dagValidation = registry.validateAllDAGs();
	if (dagValidation.isValid) {
		console.log('✅ All proffers form a valid DAG (no circular dependencies)');
	} else {
		console.log('❌ Circular dependencies detected:', dagValidation.errors);
	}

	// Example 4: Get execution order
	console.log('\n📊 Example 4: Getting execution order');

	const executionOrder = registry.getExecutionOrder();
	console.log('Execution order:', executionOrder);

	// Example 5: Fill slots and track progress
	console.log('\n⚡ Example 5: Filling slots and tracking progress');

	const taskProffer = registry.getProffer('task-001')!;

	console.log('Initial progress:', taskProffer.progress);

	// Fill the task proffer slots
	fillSlot(taskProffer, 'task-title', 'Implement user authentication');
	fillSlot(taskProffer, 'task-description', 'Add login/logout functionality with JWT tokens');
	fillSlot(taskProffer, 'estimated-hours', 8);

	console.log('After filling all slots:');
	console.log('  Status:', taskProffer.status);
	console.log('  Progress:', taskProffer.progress);

	// Example 6: Nested progress calculation
	console.log('\n📈 Example 6: Calculating nested progress');

	const projectProffer = registry.getProffer('project-001')!;

	// Fill project slots
	fillSlot(projectProffer, 'project-name', 'User Management System');
	fillSlot(projectProffer, 'budget', 10000);

	// Create and fill nested task instances
	const task1Data = createBasicProffer('task-instance-1', 'Task 1 Instance', [
		createSlot('task-title', 'text'),
		createSlot('task-description', 'text'),
		createSlot('estimated-hours', 'number')
	]);

	const task1Result = createProfferWithValidation(task1Data, registry);
	if (task1Result.proffer) {
		// Link the task instance to the project slot
		const task1Slot = projectProffer.requiredSlots.find((s) => s.id === 'task-1');
		if (task1Slot) {
			task1Slot.nestedProfferId = task1Result.proffer.id;
			fillSlot(task1Result.proffer, 'task-title', 'User Registration');
			fillSlot(task1Result.proffer, 'task-description', 'Create registration form and validation');
			fillSlot(task1Result.proffer, 'estimated-hours', 6);
		}
	}

	const nestedProgress = calculateNestedProgress(projectProffer, registry);
	console.log('Project progress with nested tasks:', nestedProgress);

	// Example 7: Serialization and deserialization
	console.log('\n💾 Example 7: Serialization and deserialization');

	const serialized = serializeProfferRegistry(registry);
	console.log(
		'Registry serialized to JSON (first 200 chars):',
		serialized.substring(0, 200) + '...'
	);

	const newRegistry = deserializeProfferRegistry(serialized);
	const deserializedProffer = newRegistry.getProffer('task-001');
	console.log('Deserialized proffer status:', deserializedProffer?.status);

	// Example 8: Resolve proffer with dependencies
	console.log('\n🔗 Example 8: Resolving proffer with dependencies');

	const resolvedProject = registry.resolveProffer('project-001');
	if (resolvedProject) {
		console.log('Resolved project proffer:');
		console.log('  ID:', resolvedProject.id);
		console.log('  Required slots with nested proffers:');
		resolvedProject.requiredSlots.forEach((slot) => {
			if (slot.nestedProffer) {
				console.log(
					`    - ${slot.id}: nested proffer "${slot.nestedProffer.id}" (${slot.nestedProffer.status})`
				);
			} else {
				console.log(`    - ${slot.id}: ${slot.inputDefinition.type} input`);
			}
		});
	}

	// Example 9: Query registry
	console.log('\n🔍 Example 9: Querying the registry');

	const allProffers = registry.getAllProffers();
	console.log('All proffers in registry:');
	allProffers.forEach((proffer) => {
		console.log(
			`  - ${proffer.id}: ${proffer.status} (${proffer.progress.completionPercentage}% complete)`
		);
	});

	console.log('\n✨ Proffer system example completed successfully!');
}

// Run the example
if (import.meta.main) {
	runProfferExample().catch(console.error);
}

export { runProfferExample };
