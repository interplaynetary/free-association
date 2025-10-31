import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { Input } from '../components/Input';
import { Select, type SelectOption } from '../components/Select';
import { UnitSelector } from '../components/UnitSelector';
import { TimeScheduleBuilder } from '../components/TimeScheduleBuilder';
import { formatScheduleOneLine } from '../utils/formatSchedule';
import type { AvailabilitySlot, AvailabilityWindow } from '../../schemas';

interface ConfirmStepProps {
	capacity: Partial<AvailabilitySlot>;
	isEditMode: boolean;
	onConfirm: () => void;
	onCancel: () => void;
	onGoBack: () => void;
}

const ConfirmStep: React.FC<ConfirmStepProps> = ({ capacity, isEditMode, onConfirm, onCancel, onGoBack }) => {
	useInput((input, key) => {
		if (key.return) {
			onConfirm();
		} else if (key.escape) {
			onCancel();
		} else if (key.leftArrow) {
			onGoBack();
		}
	});

	const scheduleDisplay = formatScheduleOneLine(capacity.availability_window, capacity.recurrence || null);
	
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

interface CapacityAddProps {
	existingCapacity?: AvailabilitySlot;
	onSave: (capacity: AvailabilitySlot) => void;
	onCancel: () => void;
}

type Step = 'type' | 'name' | 'quantity' | 'unit' | 'recurrence' | 'schedule' | 'location' | 'confirm';

const NEED_TYPE_OPTIONS: SelectOption[] = [
	{ value: 'food', label: 'Food', emoji: '🍎' },
	{ value: 'tutoring', label: 'Tutoring', emoji: '📚' },
	{ value: 'housing', label: 'Housing', emoji: '🏠' },
	{ value: 'healthcare', label: 'Healthcare', emoji: '🏥' },
	{ value: 'transportation', label: 'Transportation', emoji: '🚗' },
	{ value: 'childcare', label: 'Childcare', emoji: '👶' },
	{ value: 'other', label: 'Other', emoji: '📦' }
];

const RECURRENCE_OPTIONS: SelectOption[] = [
	{ value: 'none', label: 'One-time event' },
	{ value: 'daily', label: 'Daily' },
	{ value: 'weekly', label: 'Weekly' },
	{ value: 'monthly', label: 'Monthly' },
	{ value: 'yearly', label: 'Yearly' }
];

const LOCATION_OPTIONS: SelectOption[] = [
	{ value: 'online', label: 'Online' },
	{ value: 'specific', label: 'Specific location' },
	{ value: 'flexible', label: 'Flexible/Any location' }
];

export const CapacityAdd: React.FC<CapacityAddProps> = ({ existingCapacity, onSave, onCancel }) => {
	const isEditMode = !!existingCapacity;
	const [step, setStep] = useState<Step>('type');  // Always start at type, even in edit mode
	const [capacity, setCapacity] = useState<Partial<AvailabilitySlot>>(existingCapacity || {
		id: `cap-${Date.now()}`,
		quantity: 0,
		need_type_id: '',
		name: '',
		unit: 'units'
	});

	// Calculate available steps dynamically
	const getAvailableSteps = (): Step[] => {
		const steps: Step[] = ['type', 'quantity', 'unit', 'recurrence'];
		// Only add schedule step if recurrence is not 'none'
		if (capacity.recurrence && capacity.recurrence !== 'none') {
			steps.push('schedule');
		}
		steps.push('location', 'confirm');
		return steps;
	};

	const availableSteps = getAvailableSteps();
	const currentStepIndex = availableSteps.indexOf(step);

	// Navigation helpers
	const goToPreviousStep = () => {
		if (currentStepIndex > 0) {
			setStep(availableSteps[currentStepIndex - 1]);
		}
	};

	const goToNextStep = () => {
		if (currentStepIndex < availableSteps.length - 1) {
			setStep(availableSteps[currentStepIndex + 1]);
		}
	};

	// Global navigation with arrow keys (but NOT when in schedule builder)
	useInput((input, key) => {
		// Don't intercept if we're in the schedule builder - it has its own navigation
		if (step === 'schedule' || step === 'confirm') return;

		if (key.leftArrow) {
			goToPreviousStep();
		} else if (key.rightArrow) {
			goToNextStep();
		}
	});

	const handleTypeSelect = (typeId: string) => {
		const option = NEED_TYPE_OPTIONS.find(o => o.value === typeId);
		setCapacity({
			...capacity,
			need_type_id: typeId,
			name: option?.label || typeId,
			emoji: option?.emoji
		});
		goToNextStep();
	};

	const handleQuantitySubmit = (value: string) => {
		setCapacity({ ...capacity, quantity: parseFloat(value) });
		goToNextStep();
	};

	const handleUnitSubmit = (value: string) => {
		setCapacity({ ...capacity, unit: value || 'units' });
		goToNextStep();
	};

	const handleRecurrenceSelect = (value: string) => {
		const rec = value === 'none' ? null : value as any;
		setCapacity({
			...capacity,
			recurrence: rec
		});
		goToNextStep();
	};

	const handleScheduleComplete = (schedule: AvailabilityWindow) => {
		setCapacity({
			...capacity,
			availability_window: schedule
		});
		goToNextStep();
	};

	const handleLocationSelect = (value: string) => {
		setCapacity({ ...capacity, location_type: value });
		goToNextStep();
	};

	const handleConfirm = () => {
		onSave(capacity as AvailabilitySlot);
	};

	// Breadcrumb display
	const renderBreadcrumb = () => {
		const stepLabels: Record<Step, string> = {
			'type': 'Type',
			'name': 'Name',
			'quantity': 'Qty',
			'unit': 'Unit',
			'recurrence': 'When',
			'schedule': 'Schedule',
			'location': 'Where',
			'confirm': 'Review'
		};

		return (
			<Box marginBottom={1}>
				<Text dimColor>
					{availableSteps.map((s, i) => {
						const isCurrent = s === step;
						const isPast = i < currentStepIndex;
						return (
							<Text key={s} color={isCurrent ? 'cyan' : isPast ? 'green' : 'gray'}>
								{i > 0 && ' → '}
								{isCurrent && '[ '}
								{stepLabels[s]}
								{isCurrent && ' ]'}
							</Text>
						);
					})}
				</Text>
				{step !== 'schedule' && step !== 'confirm' && (
					<Box marginLeft={2}>
						<Text dimColor>(← → to navigate | Enter to continue | Esc to cancel)</Text>
					</Box>
				)}
			</Box>
		);
	};

	return (
		<Box flexDirection="column" padding={1}>
			<Box borderStyle="round" borderColor="cyan" padding={1} marginBottom={1}>
				<Text bold color="cyan">{isEditMode ? '✏️  Edit Capacity' : '➕ Add Capacity'}</Text>
			</Box>

			<Box flexDirection="column" paddingX={2}>
				{renderBreadcrumb()}
				
			{step === 'type' && (
				<Select
					label="What can you offer?"
					options={NEED_TYPE_OPTIONS}
					defaultValue={capacity.need_type_id}
					onSelect={handleTypeSelect}
					onCancel={onCancel}
				/>
			)}

			{step === 'quantity' && (
				<Input
					label="Quantity"
					type="number"
					placeholder="50"
					defaultValue={capacity.quantity ? capacity.quantity.toString() : ''}
					onSubmit={handleQuantitySubmit}
					onCancel={onCancel}
					validate={(value) => {
						const num = parseFloat(value);
						if (isNaN(num) || num <= 0) {
							return 'Please enter a positive number';
						}
						return true;
					}}
				/>
			)}

			{step === 'unit' && (
				<UnitSelector
					label="Unit of measurement"
					defaultUnit={capacity.unit || 'units'}
					commonUnits={['hours', 'units', 'kg', 'sessions', 'meals', 'days', 'people', 'items']}
					onSelect={handleUnitSubmit}
					onCancel={onCancel}
				/>
			)}

		{step === 'recurrence' && (
			<Select
				label="When are you available?"
				options={RECURRENCE_OPTIONS}
				defaultValue={capacity.recurrence || 'none'}
				onSelect={handleRecurrenceSelect}
				onCancel={onCancel}
			/>
		)}

		{step === 'schedule' && capacity.recurrence && (
			<TimeScheduleBuilder
				recurrence={capacity.recurrence as any}
				existingSchedule={capacity.availability_window}
				onComplete={handleScheduleComplete}
				onCancel={onCancel}
			/>
		)}

		{step === 'location' && (
				<Select
					label="Where?"
					options={LOCATION_OPTIONS}
					defaultValue={capacity.location_type}
					onSelect={handleLocationSelect}
					onCancel={onCancel}
				/>
			)}

			{step === 'confirm' && (
				<ConfirmStep
					capacity={capacity}
					isEditMode={isEditMode}
					onConfirm={handleConfirm}
					onCancel={onCancel}
					onGoBack={goToPreviousStep}
				/>
			)}
			</Box>
		</Box>
	);
};

