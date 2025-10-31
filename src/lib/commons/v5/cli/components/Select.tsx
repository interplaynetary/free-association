import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

export interface SelectOption {
	label: string;
	value: string;
	description?: string;
	emoji?: string;
}

interface SelectProps {
	label: string;
	options: SelectOption[];
	defaultValue?: string;
	onSelect: (value: string) => void;
	onCancel?: () => void;
}

export const Select: React.FC<SelectProps> = ({
	label,
	options,
	defaultValue,
	onSelect,
	onCancel
}) => {
	// Find index of default value if provided
	const defaultIndex = defaultValue 
		? options.findIndex(opt => opt.value === defaultValue)
		: 0;
	const [selectedIndex, setSelectedIndex] = useState(defaultIndex >= 0 ? defaultIndex : 0);

	useInput((input, key) => {
		if (key.upArrow) {
			setSelectedIndex(Math.max(0, selectedIndex - 1));
		} else if (key.downArrow) {
			setSelectedIndex(Math.min(options.length - 1, selectedIndex + 1));
		} else if (key.return) {
			onSelect(options[selectedIndex].value);
		} else if (key.escape) {
			onCancel?.();
		}
	});

	return (
		<Box flexDirection="column">
			<Text bold>{label}:</Text>
			<Box flexDirection="column" marginTop={1}>
				{options.map((option, index) => {
					const isSelected = index === selectedIndex;
					return (
						<Box key={option.value} marginLeft={1}>
							<Text color={isSelected ? 'cyan' : undefined}>
								{isSelected ? '▶ ' : '  '}
								{option.emoji ? `${option.emoji} ` : ''}
								{option.label}
							</Text>
							{option.description && isSelected && (
								<Text dimColor> - {option.description}</Text>
							)}
						</Box>
					);
				})}
			</Box>
			<Box marginTop={1} marginLeft={1}>
				<Text dimColor>
					Use ↑↓ to navigate | Enter to select{onCancel ? ' | Esc to cancel' : ''}
				</Text>
			</Box>
		</Box>
	);
};

