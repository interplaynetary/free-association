import React from 'react';
import { Box, Text } from 'ink';
import { TreeZipper, TreeNode } from '../utils/TreeZipper';
import { TimeScheduleBuilderV2 } from '../components/TimeScheduleBuilderV2';
import { formatScheduleOneLine } from '../utils/formatSchedule';
import type { AvailabilitySlot, AvailabilityWindow } from '../../schemas';

interface CapacityAddProps {
	existingCapacity?: AvailabilitySlot;
	onSave: (capacity: AvailabilitySlot) => void;
	onCancel: () => void;
}

interface CapacityState {
	id: string;
	need_type_id: string;
	name: string;
	emoji?: string;
	quantity: number;
	unit: string;
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	availability_window?: AvailabilityWindow;
	location_type: string;
}

const NEED_TYPE_OPTIONS = [
	{ value: 'food', label: '🍎 Food' },
	{ value: 'tutoring', label: '📚 Tutoring' },
	{ value: 'housing', label: '🏠 Housing' },
	{ value: 'healthcare', label: '🏥 Healthcare' },
	{ value: 'transportation', label: '🚗 Transportation' },
	{ value: 'childcare', label: '👶 Childcare' },
	{ value: 'other', label: '📦 Other' }
];

const RECURRENCE_OPTIONS = [
	{ value: 'none', label: 'One-time event' },
	{ value: 'daily', label: 'Daily' },
	{ value: 'weekly', label: 'Weekly' },
	{ value: 'monthly', label: 'Monthly' },
	{ value: 'yearly', label: 'Yearly' }
];

const LOCATION_OPTIONS = [
	{ value: 'online', label: 'Online' },
	{ value: 'specific', label: 'Specific location' },
	{ value: 'flexible', label: 'Flexible/Any location' }
];

const COMMON_UNITS = ['hours', 'units', 'kg', 'sessions', 'meals', 'days', 'people', 'items'];

export const CapacityAddV2: React.FC<CapacityAddProps> = ({ existingCapacity, onSave, onCancel }) => {
	const isEditMode = !!existingCapacity;

	// Build the wizard tree
	const buildTree = (): TreeNode<CapacityState> => {
		return {
			id: 'root',
			type: 'branch',
			children: [
				{
					id: 'type',
					type: 'select',
					label: 'What can you offer?',
					options: NEED_TYPE_OPTIONS,
					onExit: (state, value: string) => {
						const option = NEED_TYPE_OPTIONS.find(o => o.value === value);
						const emoji = option?.label?.match(/^(\p{Emoji})/u)?.[1];
						const label = option?.label?.replace(/^(\p{Emoji})\s*/u, '') || value;
						return {
							...state,
							need_type_id: value,
							name: label,
							emoji
						};
					}
				},
				{
					id: 'quantity',
					type: 'input',
					inputType: 'number',
					label: 'Quantity',
					placeholder: '50',
					defaultValue: (state) => state.quantity > 0 ? state.quantity.toString() : '',
					validate: (value) => {
						const num = parseFloat(value);
						if (isNaN(num) || num <= 0) return 'Please enter a positive number';
						return true;
					},
					onExit: (state, value: string) => ({
						...state,
						quantity: parseFloat(value)
					})
				},
				{
					id: 'unit',
					type: 'select',
					label: 'Unit of measurement',
					options: COMMON_UNITS.map(u => ({ value: u, label: u })),
					onExit: (state, value: string) => ({
						...state,
						unit: value || 'units'
					})
				},
				{
					id: 'recurrence',
					type: 'select',
					label: 'When are you available?',
					options: RECURRENCE_OPTIONS,
					onExit: (state, value: string) => ({
						...state,
						recurrence: value === 'none' ? null : value as 'daily' | 'weekly' | 'monthly' | 'yearly'
					})
				},
				// Conditional schedule node (only if recurrence is not null)
				{
					id: 'schedule-branch',
					type: 'branch',
					children: (state) => {
						// Only include schedule node if recurrence exists
						if (!state.recurrence) return [];
						return [{
							id: 'schedule',
							type: 'custom',
							component: TimeScheduleBuilderV2,
							componentProps: () => ({
								recurrence: state.recurrence,
								existingSchedule: state.availability_window
							}),
							onExit: (s, value: AvailabilityWindow) => ({
								...s,
								availability_window: value
							})
						}];
					}
				},
				{
					id: 'location',
					type: 'select',
					label: 'Where?',
					options: LOCATION_OPTIONS,
					onExit: (state, value: string) => ({
						...state,
						location_type: value
					})
				},
				{
					id: 'confirm',
					type: 'custom',
					component: ConfirmCapacity,
					componentProps: (state) => ({
						capacity: state,
						isEditMode
					}),
					autoAdvance: false,  // Don't auto-advance after confirm
					onExit: (state) => state
				}
			]
		};
	};

	// Initial state
	const initialState: CapacityState = existingCapacity ? {
		id: existingCapacity.id,
		need_type_id: existingCapacity.need_type_id,
		name: existingCapacity.name,
		emoji: existingCapacity.emoji,
		quantity: existingCapacity.quantity,
		unit: existingCapacity.unit || 'units',
		recurrence: existingCapacity.recurrence || null,
		availability_window: existingCapacity.availability_window,
		location_type: existingCapacity.location_type || 'flexible'
	} : {
		id: `cap-${Date.now()}`,
		need_type_id: '',
		name: '',
		quantity: 0,
		unit: 'units',
		recurrence: null,
		location_type: 'flexible'
	};

	const tree = buildTree();

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="round" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">{isEditMode ? '✏️  Edit Capacity' : '➕ Add Capacity'}</Text>
			</Box>

			<Box flexDirection="column" paddingX={2}>
				<TreeZipper
					tree={tree}
					initialState={initialState}
					onComplete={(state) => {
						onSave({
							...state,
							max_natural_div: 1,
							max_percentage_div: 0.01
						} as AvailabilitySlot);
					}}
					onCancel={onCancel}
				/>
			</Box>
		</Box>
	);
};

// Confirm component
interface ConfirmCapacityProps {
	capacity: CapacityState;
	isEditMode: boolean;
	onSubmit: () => void;
	onCancel: () => void;
}

const ConfirmCapacity: React.FC<ConfirmCapacityProps> = ({ capacity, isEditMode, onSubmit, onCancel }) => {
	const scheduleDisplay = formatScheduleOneLine(capacity.availability_window, capacity.recurrence);
	
	return (
		<Box flexDirection="column">
			<Text bold color="green">✓ Review your capacity{isEditMode ? ' (editing)' : ''}:</Text>
			<Box marginTop={1} flexDirection="column" marginLeft={2}>
				<Text>Type: {capacity.emoji} {capacity.name}</Text>
				<Text>Quantity: {capacity.quantity} {capacity.unit}</Text>
				<Text>Schedule: {scheduleDisplay}</Text>
				<Text>Location: {capacity.location_type}</Text>
			</Box>
			<Box marginTop={2}>
				<Text dimColor>← to go back | Enter to save{isEditMode ? ' changes' : ''} | Esc to cancel</Text>
			</Box>
		</Box>
	);
};

