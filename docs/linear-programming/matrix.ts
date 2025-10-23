// Simple Composition Matrix Definition
type CompositionMatrix = {
	[inputComposition: string]: {
		[outputType: string]: {
			ratio: number;
			quantity: number;
		};
	};
};

// Input composition key format: "input1:qty1,input2:qty2,..."
// Example: "housing:100,coordination:50,trust:30"

// Usage example:
const matrix: CompositionMatrix = {
	'housing:100,coordination:50': {
		housing_network: { ratio: 0.8, quantity: 80 },
		community_space: { ratio: 0.6, quantity: 60 }
	},
	'coordination:50,trust:30': {
		facilitation: { ratio: 1.2, quantity: 36 },
		mediation: { ratio: 0.9, quantity: 27 }
	}
};

// Search function
function searchMatrix(
	inputs: Record<string, number>,
	matrix: CompositionMatrix
): Array<{ outputType: string; ratio: number; quantity: number }> {
	const inputKey = Object.entries(inputs)
		.map(([type, qty]) => `${type}:${qty}`)
		.sort()
		.join(',');

	const outputs = matrix[inputKey];
	if (!outputs) return [];

	return Object.entries(outputs).map(([outputType, result]) => ({
		outputType,
		...result
	}));
}

// should be time-indexed