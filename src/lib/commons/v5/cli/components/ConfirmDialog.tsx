import React from 'react';
import { Box, Text, useInput } from 'ink';

interface ConfirmDialogProps {
	title: string;
	message: string;
	onConfirm: () => void;
	onCancel: () => void;
}

export const ConfirmDialog: React.FC<ConfirmDialogProps> = ({
	title,
	message,
	onConfirm,
	onCancel
}) => {
	useInput((input, key) => {
		const lower = input.toLowerCase();
		if (lower === 'y' || key.return) {
			onConfirm();
		} else if (lower === 'n' || key.escape) {
			onCancel();
		}
	});

	return (
		<Box flexDirection="column" borderStyle="round" borderColor="yellow" padding={1}>
			<Text bold color="yellow">{title}</Text>
			<Box marginTop={1}>
				<Text>{message}</Text>
			</Box>
			<Box marginTop={2}>
				<Text>
					Press <Text color="green">Y</Text> to confirm, <Text color="red">N/Esc</Text> to cancel
				</Text>
			</Box>
		</Box>
	);
};

