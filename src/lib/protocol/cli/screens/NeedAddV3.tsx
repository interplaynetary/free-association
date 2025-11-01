import React from 'react';
import { Box, Text } from 'ink';
import { TreeZipper } from '../utils/TreeZipper';
import { input, select, branch, custom, when, wizard } from '../utils/TreeBuilder';
import { TimeScheduleBuilderV3 } from '../components/TimeScheduleBuilderV3';
import { formatScheduleOneLine } from '../utils/formatSchedule';
import type { Need, AvailabilityWindow } from '../../schemas';

interface NeedAddProps {
	existingNeed?: Need;
	onSave: (need: Need) => void;
	onCancel: () => void;
}

interface NeedState {
	id: string;
	need_type_id: string;
	name: string;
	emoji?: string;
	quantity: number;
	unit: string;
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	availability_window?: AvailabilityWindow;
	location_type: string;
	description?: string;
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

export const NeedAddV3: React.FC<NeedAddProps> = ({ existingNeed, onSave, onCancel }) => {
	const isEditMode = !!existingNeed;

	// 🎨 ELEGANT TREE DEFINITION using fluent builders
	const tree = wizard<NeedState>('need', [
		// Type selection with emoji extraction
		select<NeedState>('type')
			.label('What do you need?')
			.options(NEED_TYPE_OPTIONS)
			.update((state, value) => {
				const option = NEED_TYPE_OPTIONS.find(o => o.value === value);
				const emoji = option?.label?.match(/^(\p{Emoji})/u)?.[1];
				const label = option?.label?.replace(/^(\p{Emoji})\s*/u, '') || value;
				return {
					...state,
					need_type_id: value,
					name: label,
					emoji
				};
			})
			.build(),

		// Quantity with validation
		input<NeedState>('quantity')
			.label('How much do you need?')
			.placeholder('50')
			.number(0)  // Auto-validates number > 0
			.path('quantity')  // Auto-updates state.quantity
			.build(),

		// Unit selection
		select<NeedState>('unit')
			.label('Unit of measurement')
			.options(COMMON_UNITS.map(u => ({ value: u, label: u })))
			.path('unit')
			.build(),

		// Recurrence
		select<NeedState>('recurrence')
			.label('When do you need it?')
			.options(RECURRENCE_OPTIONS)
			.update((state, value) => ({
				...state,
				recurrence: value === 'none' ? null : value as 'daily' | 'weekly' | 'monthly' | 'yearly'
			}))
			.build(),

		// ✨ Conditional schedule (only if recurrence exists)
		when<NeedState>(s => s.recurrence !== null).then([
			custom<NeedState>('schedule')
				.component(TimeScheduleBuilderV3)
				.props((state) => ({
					recurrence: state.recurrence,
					existingSchedule: state.availability_window
				}))
				.path('availability_window')
				.build()
		]),

		// Location
		select<NeedState>('location')
			.label('Where?')
			.options(LOCATION_OPTIONS)
			.path('location_type')
			.build(),

		// Optional description
		input<NeedState>('description')
			.label('Additional details (optional)')
			.placeholder('Any specific requirements...')
			.default(state => state.description || '')
			.path('description')
			.build(),

		// Confirmation
		custom<NeedState>('confirm')
			.component(ConfirmNeed)
			.props((state) => ({
				need: state,
				isEditMode
			}))
			.build()
	]);

	// Initial state
	const initialState: NeedState = existingNeed ? {
		id: existingNeed.id,
		need_type_id: existingNeed.need_type_id,
		name: existingNeed.name,
		emoji: existingNeed.emoji,
		quantity: existingNeed.quantity,
		unit: existingNeed.unit || 'units',
		recurrence: existingNeed.recurrence || null,
		availability_window: existingNeed.availability_window,
		location_type: existingNeed.location_type || 'flexible',
		description: existingNeed.description
	} : {
		id: `need-${Date.now()}`,
		need_type_id: '',
		name: '',
		quantity: 0,
		unit: 'units',
		recurrence: null,
		location_type: 'flexible'
	};

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="round" borderColor="yellow" padding={1} marginBottom={1}>
				<Text bold color="yellow">{isEditMode ? '✏️  Edit Need' : '➕ Add Need'}</Text>
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
						} as Need);
					}}
					onCancel={onCancel}
				/>
			</Box>
		</Box>
	);
};

// Confirm component
interface ConfirmNeedProps {
	need: NeedState;
	isEditMode: boolean;
	onSubmit: () => void;
	onCancel: () => void;
}

const ConfirmNeed: React.FC<ConfirmNeedProps> = ({ need, isEditMode, onSubmit, onCancel }) => {
	const scheduleDisplay = formatScheduleOneLine(need.availability_window, need.recurrence);
	
	return (
		<Box flexDirection="column">
			<Text bold color="green">✓ Review your need{isEditMode ? ' (editing)' : ''}:</Text>
			<Box marginTop={1} flexDirection="column" marginLeft={2}>
				<Text>Type: {need.emoji} {need.name}</Text>
				<Text>Quantity: {need.quantity} {need.unit}</Text>
				<Text>Schedule: {scheduleDisplay}</Text>
				<Text>Location: {need.location_type}</Text>
				{need.description && <Text>Details: {need.description}</Text>}
			</Box>
			<Box marginTop={2}>
				<Text dimColor>← to go back | Enter to save{isEditMode ? ' changes' : ''} | Esc to cancel</Text>
			</Box>
		</Box>
	);
};

