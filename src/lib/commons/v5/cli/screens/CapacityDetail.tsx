import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import { ConfirmDialog } from '../components/ConfirmDialog';
import { formatScheduleForDisplay } from '../utils/formatSchedule';
import type { AvailabilitySlot } from '../../schemas';

interface CapacityDetailProps {
	capacity: AvailabilitySlot;
	onBack: () => void;
	onEdit: () => void;
	onDelete: () => void;
}

export const CapacityDetail: React.FC<CapacityDetailProps> = ({
	capacity,
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
				title="⚠️  Delete Capacity?"
				message={`Delete "${capacity.name}" (${capacity.quantity} ${capacity.unit})?\n\nThis cannot be undone.`}
				onConfirm={() => {
					setShowDeleteConfirm(false);
					onDelete();
				}}
				onCancel={() => setShowDeleteConfirm(false)}
			/>
		);
	}

	// Mock allocation data
	const allocated = 0;
	const percentage = (allocated / capacity.quantity) * 100;

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="round" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">
					📦 Capacity: {capacity.emoji} {capacity.name}
				</Text>
			</Box>

			<Box flexDirection="column" paddingX={2}>
				{/* Basic Info */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Basic Info:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>ID: <Text dimColor>{capacity.id}</Text></Text>
						<Text>Type: {capacity.emoji} {capacity.need_type_id}</Text>
						<Text>
							Quantity: <Text color="cyan">{capacity.quantity} {capacity.unit || 'units'}</Text>
							{' '}
							<Text dimColor>
								({allocated} allocated, {capacity.quantity - allocated} available)
							</Text>
						</Text>
						<Box>
							<Text>Status: </Text>
							<StatusBadge percentage={percentage} />
							<Text dimColor> allocated</Text>
						</Box>
					</Box>
				</Box>

			{/* Schedule */}
			{capacity.recurrence && (
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Schedule:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>Pattern: {capacity.recurrence}</Text>
						{formatScheduleForDisplay(capacity.availability_window).map((line, i) => (
							<Box key={i}>
								<Text>{line}</Text>
							</Box>
						))}
						{capacity.start_date && (
							<Text>Start: {capacity.start_date}</Text>
						)}
						{capacity.end_date && (
							<Text>End: {capacity.end_date}</Text>
						)}
						{capacity.time_zone && (
							<Text>Timezone: {capacity.time_zone}</Text>
						)}
					</Box>
				</Box>
			)}

				{/* Location */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Location:</Text>
					<Box marginLeft={2} flexDirection="column">
						<Text>Type: {capacity.location_type || 'Not specified'}</Text>
						{capacity.city && (
							<Text>City: {capacity.city}</Text>
						)}
						{capacity.state_province && (
							<Text>State: {capacity.state_province}</Text>
						)}
						{capacity.country && (
							<Text>Country: {capacity.country}</Text>
						)}
						{capacity.online_link && (
							<Text>Link: {capacity.online_link}</Text>
						)}
					</Box>
				</Box>

				{/* Allocations */}
				<Box flexDirection="column" marginBottom={2}>
					<Text bold underline>Allocations:</Text>
					<Box marginLeft={2}>
						{allocated === 0 ? (
							<Text dimColor>No allocations yet</Text>
						) : (
							<Text>TODO: Show allocations here</Text>
						)}
					</Box>
				</Box>

				{/* Actions */}
				<Box marginTop={2} borderStyle="single" borderColor="gray" padding={1}>
					<Text>
						<Text color="cyan">E</Text> Edit{' | '}
						<Text color="red">D</Text> Delete{' | '}
						<Text color="yellow">Q/Esc</Text> Back
					</Text>
				</Box>
			</Box>
		</Box>
	);
};

