// VSM Enacted: Elegant State Management for Viable Systems
// Transforms Beer's cybernetic model into living, breathing organizational intelligence

import { writable, derived, type Writable, type Readable } from 'svelte/store';
import type {
	ViableSystem,
	System1,
	PerformanceMetrics,
	AlgedonicAlert,
	InformationChannel,
	Resource
} from './vsm.js';

// Core VSM State: The Nervous System of Organization
interface VSMState {
	// Identity & Purpose (System 5)
	identity: Writable<{
		mission: string;
		vision: string;
		values: string[];
		purpose: string;
	}>;

	// Environmental Intelligence (System 4)
	environment: Writable<{
		threats: string[];
		opportunities: string[];
		adaptations_required: string[];
		innovation_capacity: number;
	}>;

	// Operational Control (System 3)
	operations: Writable<{
		resources: Resource[];
		performance_standards: Record<string, PerformanceMetrics>;
		audit_findings: string[];
	}>;

	// Coordination Networks (System 2)
	coordination: Writable<{
		information_flows: InformationChannel[];
		resource_sharing: Record<string, number>;
		scheduling_conflicts: string[];
	}>;

	// Primary Activities (System 1)
	activities: Writable<System1[]>;

	// Nervous System: Algedonic Signals
	alerts: Writable<AlgedonicAlert[]>;
}

// VSM Intelligence: Self-Organizing Cybernetic Functions
class VSMIntelligence {
	private state: VSMState;

	// Derived Intelligence: Real-time System Health
	public readonly systemHealth: Readable<'thriving' | 'viable' | 'at_risk' | 'failing'>;
	public readonly varietyBalance: Readable<boolean>;
	public readonly adaptationPressure: Readable<number>;

	constructor() {
		this.state = this.initializeNervousSystem();

		// System Health: Emergent from all subsystem interactions
		this.systemHealth = derived(
			[this.state.activities, this.state.alerts, this.state.operations],
			([$activities, $alerts, $operations]) => {
				const criticalAlerts = $alerts.filter((a) => a.severity === 'critical').length;
				const avgPerformance = this.calculateAveragePerformance($activities);

				if (criticalAlerts > 3 || avgPerformance < 0.3) return 'failing';
				if (criticalAlerts > 1 || avgPerformance < 0.6) return 'at_risk';
				if (avgPerformance > 0.8) return 'thriving';
				return 'viable';
			}
		);

		// Variety Balance: Ashby's Law in Action
		this.varietyBalance = derived(
			[this.state.environment, this.state.coordination],
			([$env, $coord]) => {
				const environmentalVariety = $env.threats.length + $env.opportunities.length;
				const systemVariety = $coord.information_flows.length * 2; // Simplified
				return systemVariety >= environmentalVariety; // Requisite Variety
			}
		);

		// Adaptation Pressure: How urgently change is needed
		this.adaptationPressure = derived(
			[this.state.environment, this.state.alerts],
			([$env, $alerts]) => {
				const threatLevel = $env.threats.length / 10;
				const alertPressure = $alerts.filter((a) => a.type === 'pain').length / 5;
				return Math.min(threatLevel + alertPressure, 1.0);
			}
		);
	}

	// Initialize: Create the organizational nervous system
	private initializeNervousSystem(): VSMState {
		return {
			identity: writable({
				mission: '',
				vision: '',
				values: [],
				purpose: ''
			}),
			environment: writable({
				threats: [],
				opportunities: [],
				adaptations_required: [],
				innovation_capacity: 0.5
			}),
			operations: writable({
				resources: [],
				performance_standards: {},
				audit_findings: []
			}),
			coordination: writable({
				information_flows: [],
				resource_sharing: {},
				scheduling_conflicts: []
			}),
			activities: writable([]),
			alerts: writable([])
		};
	}

	// System 5: Policy & Identity Formation
	// setIdentity: (identity: VSMState['identity']) => void
	setIdentity(mission: string, vision: string, values: string[], purpose: string) {
		this.state.identity.set({ mission, vision, values, purpose });
		this.generateAlgedonicSignal('pleasure', 'Identity crystallized', 'system5');
	}

	// System 4: Environmental Adaptation
	// scanEnvironment: (threats: string[], opportunities: string[]) => Promise<string[]>
	async scanEnvironment(threats: string[], opportunities: string[]) {
		const adaptations = this.calculateRequiredAdaptations(threats, opportunities);

		this.state.environment.update((env) => ({
			...env,
			threats,
			opportunities,
			adaptations_required: adaptations
		}));

		if (threats.length > 3) {
			this.generateAlgedonicSignal('pain', 'High environmental threat level', 'system4');
		}

		return adaptations;
	}

	// System 3: Resource Management & Control
	// allocateResources: (allocation: Record<string, number>) => boolean
	allocateResources(allocation: Record<string, number>): boolean {
		const totalAllocated = Object.values(allocation).reduce((sum, amt) => sum + amt, 0);

		this.state.operations.update((ops) => ({
			...ops,
			resource_sharing: allocation
		}));

		// Audit for over-allocation
		if (totalAllocated > 1.0) {
			this.generateAlgedonicSignal('pain', 'Resource over-allocation detected', 'system3');
			return false;
		}

		return true;
	}

	// System 2: Information Flow Management
	// establishChannel: (from: string, to: string, capacity: number) => InformationChannel
	establishChannel(from: string, to: string, capacity: number): InformationChannel {
		const channel: InformationChannel = {
			id: crypto.randomUUID(),
			from_system: from,
			to_system: to,
			capacity,
			current_load: 0,
			transduction_variety: capacity * 0.8, // 80% efficiency
			active: true,
			channel_principles_compliance: {
				capacity_exceeds_originating_variety: true,
				transduction_adequate: true,
				cyclical_maintenance: true
			}
		};

		this.state.coordination.update((coord) => ({
			...coord,
			information_flows: [...coord.information_flows, channel]
		}));

		return channel;
	}

	// System 1: Primary Activity Management
	// createActivity: (name: string, function: string) => System1
	createActivity(name: string, functionDescription: string): System1 {
		const activity: System1 = {
			id: crypto.randomUUID(),
			name,
			function: functionDescription,
			performance_metrics: {
				actuality: 0,
				capability: 1,
				potentiality: 1
			},
			performance_ratios: {
				productivity: 0,
				latency: 1,
				performance: 0
			},
			variety: {
				states: 1,
				capacity: 1,
				complexity: 1,
				requisite_variety: {
					ashbys_law_compliance: {
						controller_variety: 1,
						disturbance_variety: 1,
						requisite_variety_satisfied: true
					},
					variety_amplifiers: [],
					variety_attenuators: []
				}
			},
			resources: [],
			sub_systems: [],
			active: true
		};

		this.state.activities.update((activities) => [...activities, activity]);
		return activity;
	}

	// Algedonic Signaling: Pain/Pleasure Nervous System
	// generateAlgedonicSignal: (type: 'pain' | 'pleasure', message: string, source: string) => void
	private generateAlgedonicSignal(
		type: 'pain' | 'pleasure',
		message: string,
		source: 'system1' | 'system2' | 'system3' | 'system4' | 'system5'
	) {
		const alert: AlgedonicAlert = {
			id: crypto.randomUUID(),
			type,
			severity: type === 'pain' ? 'medium' : 'low',
			deviation: type === 'pain' ? -0.2 : 0.2,
			timestamp: new Date(),
			escalated: false,
			resolved: false,
			source_system: source,
			message,
			corrective_actions: [],
			escalation_path: []
		};

		this.state.alerts.update((alerts) => [alert, ...alerts].slice(0, 100)); // Keep last 100
	}

	// Performance Analytics: Real-time System Intelligence
	// updatePerformance: (activityId: string, metrics: PerformanceMetrics) => void
	updatePerformance(activityId: string, metrics: PerformanceMetrics) {
		this.state.activities.update((activities) =>
			activities.map((activity) => {
				if (activity.id === activityId) {
					const productivity = metrics.capability > 0 ? metrics.actuality / metrics.capability : 0;
					const latency = metrics.potentiality > 0 ? metrics.capability / metrics.potentiality : 0;
					const performance =
						metrics.potentiality > 0 ? metrics.actuality / metrics.potentiality : 0;

					// Generate algedonic signals based on performance
					if (productivity < 0.5) {
						this.generateAlgedonicSignal('pain', `Low productivity in ${activity.name}`, 'system1');
					} else if (productivity > 0.9) {
						this.generateAlgedonicSignal(
							'pleasure',
							`Excellent productivity in ${activity.name}`,
							'system1'
						);
					}

					return {
						...activity,
						performance_metrics: metrics,
						performance_ratios: { productivity, latency, performance }
					};
				}
				return activity;
			})
		);
	}

	// Homeostatic Regulation: Self-Correcting Loops
	// autoRegulate: () => void - Automatic system maintenance
	autoRegulate() {
		// 3-2-1 Homeostatic Loop: Operations ↔ Coordination ↔ Activities
		this.state.activities.subscribe((activities) => {
			const underperforming = activities.filter((a) => a.performance_ratios.productivity < 0.6);

			if (underperforming.length > 0) {
				// System 3 response: Audit and resource reallocation
				this.state.operations.update((ops) => ({
					...ops,
					audit_findings: [`${underperforming.length} activities underperforming`]
				}));

				// System 2 response: Increase coordination
				underperforming.forEach((activity) => {
					this.establishChannel('system3', activity.id, 2.0);
				});
			}
		});
	}

	// Recursive Viability: Fractally Nested Systems
	// createSubsystem: (parentId: string, subsystemSpec: Partial<ViableSystem>) => ViableSystem
	createSubsystem(parentId: string, name: string): VSMIntelligence {
		const subsystem = new VSMIntelligence();

		// Link subsystem to parent through variety flow
		this.state.activities.subscribe((activities) => {
			const parent = activities.find((a) => a.id === parentId);
			if (parent) {
				// Subsystem inherits bounded variety from parent
				subsystem.setIdentity(
					`${parent.name} Subsystem: ${name}`,
					'Autonomous operation within parent constraints',
					['autonomy', 'alignment', 'adaptation'],
					`Specialized function: ${name}`
				);
			}
		});

		return subsystem;
	}

	// Utility Functions
	private calculateAveragePerformance(activities: System1[]): number {
		if (activities.length === 0) return 0;
		const total = activities.reduce((sum, a) => sum + a.performance_ratios.performance, 0);
		return total / activities.length;
	}

	private calculateRequiredAdaptations(threats: string[], opportunities: string[]): string[] {
		// Simple heuristic: each threat/opportunity suggests an adaptation
		return [...threats.map((t) => `Mitigate: ${t}`), ...opportunities.map((o) => `Exploit: ${o}`)];
	}

	// State Access: Read-only views into the organizational nervous system
	get currentState(): {
		identity: Readable<VSMState['identity']['_value']>;
		environment: Readable<VSMState['environment']['_value']>;
		operations: Readable<VSMState['operations']['_value']>;
		coordination: Readable<VSMState['coordination']['_value']>;
		activities: Readable<System1[]>;
		alerts: Readable<AlgedonicAlert[]>;
	} {
		return {
			identity: { subscribe: this.state.identity.subscribe },
			environment: { subscribe: this.state.environment.subscribe },
			operations: { subscribe: this.state.operations.subscribe },
			coordination: { subscribe: this.state.coordination.subscribe },
			activities: { subscribe: this.state.activities.subscribe },
			alerts: { subscribe: this.state.alerts.subscribe }
		};
	}
}

// Factory: Create Organizational Intelligence
// createViableSystem: (name: string) => VSMIntelligence
export function createViableSystem(name: string): VSMIntelligence {
	const vsm = new VSMIntelligence();

	// Initialize with basic identity
	vsm.setIdentity(
		`Mission: ${name}`,
		`Vision: Thriving ${name}`,
		['viability', 'adaptation', 'recursion'],
		`Purpose: Self-organizing ${name} intelligence`
	);

	// Start homeostatic regulation
	vsm.autoRegulate();

	return vsm;
}

// Elegant Composability: VSM Networks
// linkSystems: (systems: VSMIntelligence[], topology: 'hierarchy' | 'network' | 'federation') => void
export function linkSystems(
	systems: VSMIntelligence[],
	topology: 'hierarchy' | 'network' | 'federation' = 'network'
): void {
	systems.forEach((system, i) => {
		systems.forEach((other, j) => {
			if (i !== j) {
				// Establish information channels between systems
				system.establishChannel(`system_${i}`, `system_${j}`, 1.0);

				// Create resource sharing based on topology
				if (topology === 'hierarchy' && j > i) {
					system.allocateResources({ [`system_${j}`]: 0.1 });
				} else if (topology === 'network') {
					system.allocateResources({ [`system_${j}`]: 0.05 });
				}
			}
		});
	});
}

export { VSMIntelligence };
export type { VSMState };
