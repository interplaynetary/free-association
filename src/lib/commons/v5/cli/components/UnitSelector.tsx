import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

interface UnitSelectorProps {
	label: string;
	defaultUnit?: string;
	commonUnits?: string[];
	onSelect: (unit: string) => void;
	onCancel?: () => void;
}

const DEFAULT_COMMON_UNITS = [
	'hours',
	'units',
	'kg',
	'sessions',
	'meals',
	'days',
	'people',
	'items',
	'liters',
	'pieces'
];

export const UnitSelector: React.FC<UnitSelectorProps> = ({
	label,
	defaultUnit = 'units',
	commonUnits = DEFAULT_COMMON_UNITS,
	onSelect,
	onCancel
}) => {
	// Find initial index of default unit
	const initialIndex = commonUnits.indexOf(defaultUnit);
	const [selectedIndex, setSelectedIndex] = useState(initialIndex >= 0 ? initialIndex : 0);
	const [customUnit, setCustomUnit] = useState('');
	const [isTyping, setIsTyping] = useState(false);

	useInput((input, key) => {
		if (key.return) {
			// Submit either custom unit or selected unit
			const finalUnit = isTyping && customUnit ? customUnit : commonUnits[selectedIndex];
			onSelect(finalUnit);
		} else if (key.escape) {
			onCancel?.();
		} else if (key.upArrow && !isTyping) {
			// Cycle up through common units
			setSelectedIndex((prev) => (prev - 1 + commonUnits.length) % commonUnits.length);
		} else if (key.downArrow && !isTyping) {
			// Cycle down through common units
			setSelectedIndex((prev) => (prev + 1) % commonUnits.length);
		} else if (key.backspace || key.delete) {
			// Start/continue typing custom unit
			setIsTyping(true);
			setCustomUnit(customUnit.slice(0, -1));
		} else if (!key.ctrl && !key.meta && input && input.length === 1) {
			// Start/continue typing custom unit (any letter/number)
			setIsTyping(true);
			setCustomUnit(customUnit + input);
		}
	});

	const displayUnit = isTyping && customUnit ? customUnit : commonUnits[selectedIndex];

	return (
		<Box flexDirection="column">
			<Text bold>{label}:</Text>
			<Box marginTop={1} marginLeft={1}>
				<Text>
					<Text color="cyan">{displayUnit}</Text>
					<Text dimColor>█</Text>
				</Text>
			</Box>
			<Box marginTop={1} marginLeft={1}>
				<Text dimColor>
					{isTyping 
						? '(typing custom unit | Enter to confirm | Esc to cancel)'
						: '(↑↓ to cycle units | type to customize | Enter to confirm | Esc to cancel)'
					}
				</Text>
			</Box>
			{!isTyping && (
				<Box marginTop={1} marginLeft={1} flexDirection="column">
					<Text dimColor>Common units: {commonUnits.join(', ')}</Text>
				</Box>
			)}
		</Box>
	);
};

