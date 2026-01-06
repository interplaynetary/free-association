/**
 * Interactive Menu Component
 * 
 * Main navigation menu for the CLI
 */

import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

export interface MenuItem {
	id: string;
	label: string;
	description: string;
}

interface MenuProps {
	items: MenuItem[];
	onSelect: (itemId: string) => void;
}

export const Menu: React.FC<MenuProps> = ({ items, onSelect }) => {
	const [selectedIndex, setSelectedIndex] = useState(0);

	useInput((input, key) => {
		if (key.upArrow) {
			setSelectedIndex((prev) => (prev > 0 ? prev - 1 : items.length - 1));
		} else if (key.downArrow) {
			setSelectedIndex((prev) => (prev < items.length - 1 ? prev + 1 : 0));
		} else if (key.return) {
			onSelect(items[selectedIndex].id);
		}
	});

	return (
		<Box flexDirection="column" borderStyle="round" borderColor="cyan" padding={1}>
			<Box marginBottom={1}>
				<Text bold color="cyan">
					Select an option (↑↓ to navigate, Enter to select):
				</Text>
			</Box>
			{items.map((item, index) => {
				const isSelected = index === selectedIndex;
				return (
					<Box key={item.id} marginLeft={2}>
						<Text color={isSelected ? 'green' : 'white'} bold={isSelected}>
							{isSelected ? '► ' : '  '}
							{item.label}
							<Text dimColor> - {item.description}</Text>
						</Text>
					</Box>
				);
			})}
		</Box>
	);
};

