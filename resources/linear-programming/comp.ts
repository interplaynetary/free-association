
// Compositional Capacity Matrix

```typescript
/**
 * Compositional Capacity Matrix (CCM) - TypeScript Definition
 * Maps share combinations to emergent capacities through contextual composition functions
 */

// Input Share Vector (S^n)
interface ShareVector {
	shares: Record<
		string,
		{
			quantity: number;
			percentage: number;
			type: string;
			source_id: string;
		}
	>;
}

// Composition Operators (C)
type CompositionOperator =
	| 'linear' // Simple addition: output = sum(inputs)
	| 'synergistic' // Multiplicative: output = product(inputs) * synergy_factor
	| 'catalytic' // One input amplifies others: output = catalyst * sum(others)
	| 'threshold' // Minimum required: output = inputs > threshold ? function(inputs) : 0
	| 'network' // Network effects: output = inputs * sqrt(network_size)
	| 'complementary'; // Requires specific combinations: output = min(input_ratios) * effectiveness

// Context Parameters (Θ)
interface ContextParameters {
	temporal: {
		timestamp: number;
		time_of_day?: string;
		day_of_week?: string;
		season?: string;
	};
	social: {
		relationship_strength: Record<string, number>; // 0-1 trust levels
		group_cohesion: number;
		recognition_cache: Record<string, { ourShare: number; theirShare: number }>;
	};
	environmental: {
		location?: { longitude: number; latitude: number };
		availability_constraints?: Record<string, any>;
		external_factors?: Record<string, number>;
	};
}

// Output Capacity Vector (R^m)
interface CapacityOutput {
	capacity_id: string;
	name: string;
	quantity: number;
	unit: string;
	quality_metrics?: Record<string, number>;
	availability_windows?: Array<{
		start: string;
		end: string;
		max_utilization: number;
	}>;
}

// Efficiency Coefficient (E)
interface EfficiencyCoefficient {
	yield_ratio: number; // output_quantity / input_quantity
	relationship_cost: number; // social overhead
	sustainability_factor: number; // long-term viability
	discovery_bonus: number; // reward for novel compositions
}

// Matrix Result
interface CompositionResult {
	outputs: CapacityOutput[];
	efficiency: EfficiencyCoefficient;
	confidence: number; // 0-1, how certain we are about this composition
	alternatives?: CompositionResult[]; // other viable compositions
}

// The Compositional Capacity Matrix Function
type CompositionFunction = (
	shareVector: ShareVector,
	operator: CompositionOperator,
	context: ContextParameters
) => CompositionResult | null;

// Matrix Interface
interface CompositionMatrix {
	// The core composition function
	compose: CompositionFunction;

	// Matrix search optimization
	findOptimalComposition: (
		availableShares: ShareVector,
		desiredOutputs: Partial<CapacityOutput>[],
		context: ContextParameters,
		objectives?: {
			maximize_quantity?: number;
			maximize_efficiency?: number;
			maximize_relationships?: number;
			maximize_sustainability?: number;
		}
	) => CompositionResult[];

	// Discovery functions
	discoverEmergentCapacities: (
		shareVector: ShareVector,
		context: ContextParameters
	) => CapacityOutput[];

	// Learning and adaptation
	updateFromObservation: (
		inputs: ShareVector,
		actualOutputs: CapacityOutput[],
		context: ContextParameters
	) => void;
}

// Example Implementation Structure
class SparseCompositionMatrix implements CompositionMatrix {
	private compositions: Map<string, CompositionFunction> = new Map();
	private learningRate = 0.1;

	compose: CompositionFunction = (shareVector, operator, context) => {
		const key = this.generateCompositionKey(shareVector, operator, context);
		const compositionFn = this.compositions.get(key);

		if (!compositionFn) {
			// Matrix is sparse - most combinations yield null
			return null;
		}

		return compositionFn(shareVector, operator, context);
	};

	findOptimalComposition = (
		availableShares: ShareVector,
		desiredOutputs: Partial<CapacityOutput>[],
		context: ContextParameters,
		objectives = { maximize_quantity: 1 }
	): CompositionResult[] => {
		const results: CompositionResult[] = [];
		const operators: CompositionOperator[] = [
			'linear',
			'synergistic',
			'catalytic',
			'threshold',
			'network',
			'complementary'
		];

		// Search across all operators and share combinations
		for (const operator of operators) {
			const result = this.compose(availableShares, operator, context);
			if (result && this.matchesDesiredOutputs(result.outputs, desiredOutputs)) {
				results.push(result);
			}
		}

		// Sort by multi-objective optimization
		return results.sort(
			(a, b) =>
				this.calculateObjectiveScore(b, objectives) - this.calculateObjectiveScore(a, objectives)
		);
	};

	discoverEmergentCapacities = (
		shareVector: ShareVector,
		context: ContextParameters
	): CapacityOutput[] => {
		// This is where the magic happens - discovering new capacity types
		// that emerge from unexpected share combinations
		const emergentCapacities: CapacityOutput[] = [];

		// Try all composition operators to see what emerges
		const operators: CompositionOperator[] = [
			'synergistic',
			'catalytic',
			'network',
			'complementary'
		];

		for (const operator of operators) {
			const result = this.compose(shareVector, operator, context);
			if (result && result.confidence > 0.7) {
				emergentCapacities.push(...result.outputs);
			}
		}

		return this.deduplicateCapacities(emergentCapacities);
	};

	updateFromObservation = (
		inputs: ShareVector,
		actualOutputs: CapacityOutput[],
		context: ContextParameters
	): void => {
		// Machine learning component - update matrix based on observed results
		// This allows the matrix to learn and improve over time
	};

	private generateCompositionKey(
		shareVector: ShareVector,
		operator: CompositionOperator,
		context: ContextParameters
	): string {
		// Generate a sparse key for the composition lookup
		return `${JSON.stringify(shareVector)}-${operator}-${JSON.stringify(context)}`;
	}

	private matchesDesiredOutputs(
		outputs: CapacityOutput[],
		desired: Partial<CapacityOutput>[]
	): boolean {
		// Check if composition outputs match what was desired
		return desired.every((d) =>
			outputs.some(
				(o) => (!d.name || o.name === d.name) && (!d.quantity || o.quantity >= d.quantity)
			)
		);
	}

	private calculateObjectiveScore(
		result: CompositionResult,
		objectives: Record<string, number>
	): number {
		return (
			(objectives.maximize_quantity || 0) * result.outputs.reduce((sum, o) => sum + o.quantity, 0) +
			(objectives.maximize_efficiency || 0) * result.efficiency.yield_ratio +
			(objectives.maximize_relationships || 0) * (1 - result.efficiency.relationship_cost) +
			(objectives.maximize_sustainability || 0) * result.efficiency.sustainability_factor
		);
	}

	private deduplicateCapacities(capacities: CapacityOutput[]): CapacityOutput[] {
		// Remove duplicate emergent capacities
		const seen = new Set<string>();
		return capacities.filter((c) => {
			const key = `${c.name}-${c.unit}`;
			if (seen.has(key)) return false;
			seen.add(key);
			return true;
		});
	}
}

// Usage Example
const matrix = new SparseCompositionMatrix();

// Find optimal composition for a housing + coordination capacity
const optimalCompositions = matrix.findOptimalComposition(
	{
		shares: {
			'housing-space': { quantity: 100, percentage: 0.8, type: 'physical', source_id: 'user1' },
			coordination: { quantity: 50, percentage: 0.6, type: 'social', source_id: 'user2' },
			trust: { quantity: 30, percentage: 0.9, type: 'relational', source_id: 'user1-user2' }
		}
	},
	[{ name: 'Housing Network', quantity: 80, unit: 'nights' }],
	{
		temporal: { timestamp: Date.now() },
		social: {
			relationship_strength: { 'user1-user2': 0.8 },
			group_cohesion: 0.7,
			recognition_cache: { user1: { ourShare: 0.6, theirShare: 0.4 } }
		},
		environmental: { location: { longitude: -122.4194, latitude: 37.7749 } }
	},
	{ maximize_quantity: 0.4, maximize_efficiency: 0.3, maximize_relationships: 0.3 }
);
```
