import React from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import type { AvailabilitySlot, NeedSlot } from '../../schemas';

interface DashboardProps {
	myName: string;
	capacities: AvailabilitySlot[];
	needs: NeedSlot[];
	onNavigate: (screen: string) => void;
}

export const Dashboard: React.FC<DashboardProps> = ({
	myName,
	capacities,
	needs,
	onNavigate
}) => {
	useInput((input, key) => {
		const lower = input.toLowerCase();
		if (lower === 'c') {
			onNavigate('capacities');
		} else if (lower === 'n') {
			onNavigate('needs');
		} else if (lower === 'a') {
			onNavigate('allocate');
		} else if (lower === 'q' || key.escape) {
			onNavigate('menu');
		}
	});

	// Calculate stats
	const totalCapacities = capacities.length;
	const totalNeeds = needs.length;
	const avgCapacitySatisfaction = 0; // Mock
	const avgNeedSatisfaction = 0; // Mock

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="double" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">🌐 Playnet Dashboard</Text>
			</Box>

			<Box flexDirection="column" paddingX={2}>
				{/* Profile */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold>Your Profile: <Text color="cyan">{myName}</Text></Text>
					<Text dimColor>Status: 🟢 Active | Last sync: Just now</Text>
				</Box>

				{/* Quick Stats */}
				<Box
					borderStyle="single"
					borderColor="gray"
					padding={1}
					flexDirection="column"
					marginBottom={2}
				>
					<Text bold underline>Quick Stats</Text>
					<Box marginTop={1} flexDirection="column">
						<Text>Capacities: {totalCapacities} active</Text>
						<Text>Needs: {totalNeeds} active</Text>
						<Text dimColor>Network: 1 participant (you)</Text>
					</Box>
				</Box>

				{/* Your Capacities */}
				<Box
					borderStyle="single"
					borderColor="cyan"
					padding={1}
					flexDirection="column"
					marginBottom={2}
				>
					<Text bold color="cyan">📦 Your Capacities</Text>
					<Box marginTop={1} flexDirection="column">
						{capacities.length === 0 ? (
							<Text dimColor>No capacities yet</Text>
						) : (
							capacities.slice(0, 3).map((cap) => (
								<Box key={cap.id}>
									<Text>
										{cap.emoji} {cap.name}
									</Text>
									<Text dimColor>
										{' '}({cap.quantity} {cap.unit})
									</Text>
								</Box>
							))
						)}
						{capacities.length > 3 && (
							<Text dimColor>...and {capacities.length - 3} more</Text>
						)}
					</Box>
				</Box>

				{/* Your Needs */}
				<Box
					borderStyle="single"
					borderColor="magenta"
					padding={1}
					flexDirection="column"
					marginBottom={2}
				>
					<Text bold color="magenta">🎯 Your Needs</Text>
					<Box marginTop={1} flexDirection="column">
						{needs.length === 0 ? (
							<Text dimColor>No needs yet</Text>
						) : (
							needs.slice(0, 3).map((need) => (
								<Box key={need.id}>
									<Text>
										{need.emoji} {need.name}
									</Text>
									<Text dimColor>
										{' '}({need.quantity} {need.unit})
									</Text>
								</Box>
							))
						)}
						{needs.length > 3 && (
							<Text dimColor>...and {needs.length - 3} more</Text>
						)}
					</Box>
				</Box>

				{/* Actions */}
				<Box
					borderStyle="single"
					borderColor="yellow"
					padding={1}
					flexDirection="column"
				>
					<Text bold>Quick Actions:</Text>
					<Box marginTop={1} flexDirection="column">
						<Text><Text color="cyan">C</Text> Manage capacities</Text>
						<Text><Text color="magenta">N</Text> Manage needs</Text>
						<Text><Text color="green">A</Text> Run allocation</Text>
						<Text><Text color="yellow">Q/Esc</Text> Back to menu</Text>
					</Box>
				</Box>
			</Box>
		</Box>
	);
};

