import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import type { AvailabilitySlot } from '../../schemas';

interface CapacityListProps {
	capacities: AvailabilitySlot[];
	onSelect: (capacityId: string) => void;
	onAdd: () => void;
	onBack: () => void;
}

export const CapacityList: React.FC<CapacityListProps> = ({
	capacities,
	onSelect,
	onAdd,
	onBack
}) => {
	const [selectedIndex, setSelectedIndex] = useState(0);

	useInput((input, key) => {
		if (key.upArrow) {
			setSelectedIndex(Math.max(0, selectedIndex - 1));
		} else if (key.downArrow) {
			setSelectedIndex(Math.min(capacities.length, selectedIndex + 1));
		} else if (key.return) {
			if (selectedIndex === capacities.length) {
				onAdd();
			} else if (capacities[selectedIndex]) {
				onSelect(capacities[selectedIndex].id);
			}
		} else if (key.escape || input === 'q') {
			onBack();
		} else if (input === 'a') {
			onAdd();
		}
	});

	// Group by need type
	const byType = capacities.reduce((acc, cap) => {
		if (!acc[cap.need_type_id]) {
			acc[cap.need_type_id] = [];
		}
		acc[cap.need_type_id].push(cap);
		return {};
	}, {} as Record<string, AvailabilitySlot[]>);

	return (
		<Box flexDirection="column">
			<Box borderStyle="round" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">📦 Your Capacities</Text>
			</Box>

			{capacities.length === 0 ? (
				<Box flexDirection="column" paddingX={2}>
					<Text dimColor>No capacities yet. Add one to get started!</Text>
					<Box marginTop={1}>
						<Text>
							{selectedIndex === 0 ? '▶ ' : '  '}
							<Text color={selectedIndex === 0 ? 'cyan' : undefined}>
								➕ Add capacity
							</Text>
						</Text>
					</Box>
				</Box>
			) : (
				<Box flexDirection="column" paddingX={2}>
					{capacities.map((capacity, index) => {
						const isSelected = index === selectedIndex;
						// Calculate allocation percentage (mock for now)
						const allocated = 0;
						const percentage = (allocated / capacity.quantity) * 100;

						return (
							<Box key={capacity.id} flexDirection="column" marginBottom={1}>
								<Box>
									<Text color={isSelected ? 'cyan' : undefined}>
										{isSelected ? '▶ ' : '  '}
										{capacity.emoji || '📦'} {capacity.name}
									</Text>
								</Box>
								<Box marginLeft={3}>
									<Text dimColor>
										{capacity.quantity} {capacity.unit || 'units'}
										{' • '}
										{capacity.recurrence || 'One-time'}
										{capacity.location_type && ` • ${capacity.location_type}`}
									</Text>
								</Box>
								<Box marginLeft={3}>
									<StatusBadge percentage={percentage} />
									<Text dimColor> allocated</Text>
								</Box>
							</Box>
						);
					})}
					
					<Box marginTop={1}>
						<Text>
							{selectedIndex === capacities.length ? '▶ ' : '  '}
							<Text color={selectedIndex === capacities.length ? 'cyan' : undefined}>
								➕ Add capacity
							</Text>
						</Text>
					</Box>
				</Box>
			)}

			<Box marginTop={1} paddingX={2}>
				<Text dimColor>
					↑↓ Navigate | Enter Select | A Add | Q/Esc Back
				</Text>
			</Box>
		</Box>
	);
};

