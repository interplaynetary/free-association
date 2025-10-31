/**
 * Allocation Display Component
 * 
 * Beautiful display of allocation results
 */

import React from 'react';
import { Box, Text } from 'ink';
import type { AllocationResult } from '../free';

interface AllocationDisplayProps {
	result: AllocationResult;
	myName?: string;
}

export const AllocationDisplay: React.FC<AllocationDisplayProps> = ({ result, myName = 'You' }) => {
	const { allocations, convergence, totalsByTypeAndRecipient } = result;

	return (
		<Box flexDirection="column" padding={1} borderStyle="round" borderColor="green">
			<Box marginBottom={1}>
				<Text bold color="green">
					✓ Allocation Complete
				</Text>
			</Box>

			{/* Summary */}
			<Box flexDirection="column" marginBottom={1}>
				<Text dimColor>Total Allocations: </Text>
				<Text color="cyan">{allocations.length} slots distributed</Text>
			</Box>

			{/* Allocations by Recipient */}
			<Box flexDirection="column" marginBottom={1}>
				<Text bold underline>
					Distribution:
				</Text>
				{Object.entries(totalsByTypeAndRecipient).map(([typeId, recipients]) => (
					<Box key={typeId} flexDirection="column" marginLeft={2}>
						<Text color="yellow">📦 {typeId}</Text>
						{Object.entries(recipients as Record<string, number>).map(([recipientId, quantity]) => (
							<Box key={recipientId} marginLeft={2}>
								<Text>
									→ <Text color="cyan">{recipientId.substring(0, 8)}...</Text>: {quantity.toFixed(2)} units
								</Text>
							</Box>
						))}
					</Box>
				))}
			</Box>

			{/* Convergence Metrics */}
			<Box flexDirection="column" borderStyle="single" borderColor="blue" padding={1}>
				<Text bold color="blue">
					📊 Convergence Metrics
				</Text>
				<Box marginTop={1} flexDirection="column" gap={0}>
					<Text>
						Need Magnitude: <Text color="magenta">{convergence.totalNeedMagnitude.toFixed(2)}</Text>
					</Text>
					<Text>
						Contraction Rate: <Text color={convergence.contractionRate < 1 ? 'green' : 'red'}>
							{convergence.contractionRate.toFixed(2)}
						</Text>
					</Text>
					{convergence.percentNeedReduction !== undefined && (
						<Text>
							Need Reduction: <Text color="cyan">{convergence.percentNeedReduction.toFixed(1)}%</Text>
							<Text dimColor> (allocation progress)</Text>
						</Text>
					)}
					<Text>
						People Satisfied: <Text color="cyan">{convergence.percentNeedsMet.toFixed(1)}%</Text>
						<Text dimColor> (fully met)</Text>
					</Text>
					{convergence.iterationsToConvergence !== null && (
						<Text>
							Iterations to Convergence: <Text color="yellow">{convergence.iterationsToConvergence}</Text>
						</Text>
					)}
					{convergence.universalSatisfaction && (
						<Text color="green" bold>
							🎉 Universal Satisfaction Achieved!
						</Text>
					)}
				</Box>
			</Box>
		</Box>
	);
};

