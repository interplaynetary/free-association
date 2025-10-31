import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import type { NeedSlot } from '../../schemas';

interface NeedListProps {
	needs: NeedSlot[];
	onSelect: (needId: string) => void;
	onAdd: () => void;
	onBack: () => void;
}

export const NeedList: React.FC<NeedListProps> = ({
	needs,
	onSelect,
	onAdd,
	onBack
}) => {
	const [selectedIndex, setSelectedIndex] = useState(0);

	useInput((input, key) => {
		if (key.upArrow) {
			setSelectedIndex(Math.max(0, selectedIndex - 1));
		} else if (key.downArrow) {
			setSelectedIndex(Math.min(needs.length, selectedIndex + 1));
		} else if (key.return) {
			if (selectedIndex === needs.length) {
				onAdd();
			} else if (needs[selectedIndex]) {
				onSelect(needs[selectedIndex].id);
			}
		} else if (key.escape || input === 'q') {
			onBack();
		} else if (input === 'a') {
			onAdd();
		}
	});

	return (
		<Box flexDirection="column">
			<Box borderStyle="round" borderColor="magenta" padding={1} marginBottom={1}>
				<Text bold color="magenta">🎯 Your Needs</Text>
			</Box>

			{needs.length === 0 ? (
				<Box flexDirection="column" paddingX={2}>
					<Text dimColor>No needs yet. Add one to get started!</Text>
					<Box marginTop={1}>
						<Text>
							{selectedIndex === 0 ? '▶ ' : '  '}
							<Text color={selectedIndex === 0 ? 'magenta' : undefined}>
								➕ Add need
							</Text>
						</Text>
					</Box>
				</Box>
			) : (
				<Box flexDirection="column" paddingX={2}>
					{needs.map((need, index) => {
						const isSelected = index === selectedIndex;
						// Calculate satisfaction percentage (mock for now)
						const received = 0;
						const percentage = (received / need.quantity) * 100;

						return (
							<Box key={need.id} flexDirection="column" marginBottom={1}>
								<Box>
									<Text color={isSelected ? 'magenta' : undefined}>
										{isSelected ? '▶ ' : '  '}
										{need.emoji || '🎯'} {need.name}
									</Text>
								</Box>
								<Box marginLeft={3}>
									<Text dimColor>
										{need.quantity} {need.unit || 'units'}
										{' • '}
										{need.recurrence || 'One-time'}
										{need.location_type && ` • ${need.location_type}`}
									</Text>
								</Box>
								<Box marginLeft={3}>
									<StatusBadge percentage={percentage} />
									<Text dimColor> satisfied</Text>
								</Box>
							</Box>
						);
					})}
					
					<Box marginTop={1}>
						<Text>
							{selectedIndex === needs.length ? '▶ ' : '  '}
							<Text color={selectedIndex === needs.length ? 'magenta' : undefined}>
								➕ Add need
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

