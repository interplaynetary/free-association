import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import { ConfirmDialog } from '../components/ConfirmDialog';
import { formatScheduleForDisplay } from '../utils/formatSchedule';
import type { NeedSlot } from '../../schemas';

interface NeedDetailProps {
	need: NeedSlot;
	onBack: () => void;
	onEdit: () => void;
	onDelete: () => void;
}

export const NeedDetail: React.FC<NeedDetailProps> = ({
	need,
	onBack,
	onEdit,
	onDelete
}) => {
	const [showDeleteConfirm, setShowDeleteConfirm] = useState(false);
	
	useInput((input, key) => {
		if (showDeleteConfirm) return; // Let ConfirmDialog handle input
		
		const lower = input.toLowerCase();
		if (key.escape || lower === 'q') {
			onBack();
		} else if (lower === 'e') {
			onEdit();
		} else if (lower === 'd') {
			setShowDeleteConfirm(true);
		}
	});
	
	if (showDeleteConfirm) {
		return (
			<ConfirmDialog
				title="⚠️  Delete Need?"
				message={`Delete "${need.name}" (${need.quantity} ${need.unit})?\n\nThis cannot be undone.`}
				onConfirm={() => {
					setShowDeleteConfirm(false);
					onDelete();
				}}
				onCancel={() => setShowDeleteConfirm(false)}
			/>
		);
	}

	// Mock fulfillment data
	const fulfilled = 0;
	const percentage = (fulfilled / need.quantity) * 100;

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="round" borderColor="magenta" padding={1} marginBottom={1}>
				<Text bold color="magenta">
					🎯 Need: {need.emoji} {need.name}
				</Text>
			</Box>

			<Box flexDirection="column" paddingX={2}>
				{/* Basic Info */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Basic Info:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>ID: <Text dimColor>{need.id}</Text></Text>
						<Text>Type: {need.emoji} {need.need_type_id}</Text>
						<Text>
							Quantity: <Text color="magenta">{need.quantity} {need.unit || 'units'}</Text>
							{' '}
							<Text dimColor>
								({fulfilled} fulfilled, {need.quantity - fulfilled} remaining)
							</Text>
						</Text>
						<Box>
							<Text>Status: </Text>
							<StatusBadge percentage={percentage} />
							<Text dimColor> fulfilled</Text>
						</Box>
					</Box>
				</Box>

			{/* Schedule */}
			{need.recurrence && (
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Schedule:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>Pattern: {need.recurrence}</Text>
						{formatScheduleForDisplay(need.availability_window).map((line, i) => (
							<Box key={i}>
								<Text>{line}</Text>
							</Box>
						))}
						{need.start_date && (
							<Text>Start: {need.start_date}</Text>
						)}
						{need.end_date && (
							<Text>End: {need.end_date}</Text>
						)}
						{need.time_zone && (
							<Text>Timezone: {need.time_zone}</Text>
						)}
					</Box>
				</Box>
			)}

				{/* Location */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Location:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>Type: {need.location_type || 'Not specified'}</Text>
						{need.city && (
							<Text>City: {need.city}</Text>
						)}
						{need.state_province && (
							<Text>State: {need.state_province}</Text>
						)}
						{need.country && (
							<Text>Country: {need.country}</Text>
						)}
						{need.online_link && (
							<Text>Link: {need.online_link}</Text>
						)}
					</Box>
				</Box>

				{/* Fulfillment */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Fulfillment:</Text>
					<Box marginLeft={2}>
						{fulfilled === 0 ? (
							<Text dimColor>No fulfillment yet</Text>
						) : (
							<Text>TODO: Show fulfillment here</Text>
						)}
					</Box>
				</Box>

				{/* Actions */}
				<Box marginTop={2} borderStyle="single" borderColor="gray" padding={1}>
					<Text>
						<Text color="magenta">E</Text> Edit{' | '}
						<Text color="red">D</Text> Delete{' | '}
						<Text color="yellow">Q/Esc</Text> Back
					</Text>
				</Box>
			</Box>
		</Box>
	);
};

