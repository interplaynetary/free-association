import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';

interface InputProps {
	label: string;
	placeholder?: string;
	defaultValue?: string;
	onSubmit: (value: string) => void;
	onCancel?: () => void;
	type?: 'text' | 'number';
	validate?: (value: string) => boolean | string;
}

export const Input: React.FC<InputProps> = ({
	label,
	placeholder = '',
	defaultValue = '',
	onSubmit,
	onCancel,
	type = 'text',
	validate
}) => {
	// Initialize with defaultValue so Enter works immediately
	const [value, setValue] = useState(defaultValue);
	const [error, setError] = useState<string | null>(null);
	const [hasUserInput, setHasUserInput] = useState(false);

	useInput((input, key) => {
		if (key.return) {
			// Use current value (which starts as defaultValue)
			const finalValue = value;
			
			// Don't submit if empty
			if (!finalValue) {
				setError('Please enter a value');
				return;
			}
			
			// Validate
			if (validate) {
				const result = validate(finalValue);
				if (result !== true) {
					setError(typeof result === 'string' ? result : 'Invalid input');
					return;
				}
			}
			setError(null);
			onSubmit(finalValue);
		} else if (key.escape) {
			onCancel?.();
		} else if (key.backspace || key.delete) {
			setHasUserInput(true);
			setValue(value.slice(0, -1));
			setError(null);
		} else if (!key.ctrl && !key.meta && input) {
			setHasUserInput(true);
			// For number type, only allow digits and decimal point
			if (type === 'number') {
				if (/^[0-9.]$/.test(input)) {
					setValue(value + input);
					setError(null);
				}
			} else {
				setValue(value + input);
				setError(null);
			}
		}
	});

	return (
		<Box flexDirection="column">
			<Box>
				<Text bold>{label}: </Text>
				{value ? (
					<Text color="cyan">{value}</Text>
				) : (
					<Text dimColor>{placeholder}</Text>
				)}
				<Text dimColor>█</Text>
			</Box>
			{error && (
				<Box marginLeft={2}>
					<Text color="red">⚠ {error}</Text>
				</Box>
			)}
			<Box marginLeft={2} marginTop={1}>
				<Text dimColor>
					Press Enter to submit{onCancel ? ' | Esc to cancel' : ''}
				</Text>
			</Box>
		</Box>
	);
};

