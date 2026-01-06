import React from 'react';
import { Box } from 'ink';
import { TreeZipper } from '../utils/TreeZipper';
import { input, multiSelect, branch, forEach, wizard } from '../utils/TreeBuilder';
import type { AvailabilityWindow, DayOfWeek } from '../../schemas';

interface TimeScheduleBuilderProps {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	existingSchedule?: AvailabilityWindow;
	onComplete: (schedule: AvailabilityWindow) => void;
	onCancel: () => void;
}

interface TimeScheduleState {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	selectedMonths: number[];
	selectedWeeks: number[];
	selectedDays: DayOfWeek[];
	timeRangesByDay: Record<DayOfWeek, { start_time: string; end_time: string }>;
}

const DAY_OPTIONS = [
	{ value: 'monday', label: 'Monday' },
	{ value: 'tuesday', label: 'Tuesday' },
	{ value: 'wednesday', label: 'Wednesday' },
	{ value: 'thursday', label: 'Thursday' },
	{ value: 'friday', label: 'Friday' },
	{ value: 'saturday', label: 'Saturday' },
	{ value: 'sunday', label: 'Sunday' }
];

const MONTH_OPTIONS = [
	{ value: '1', label: 'January' },
	{ value: '2', label: 'February' },
	{ value: '3', label: 'March' },
	{ value: '4', label: 'April' },
	{ value: '5', label: 'May' },
	{ value: '6', label: 'June' },
	{ value: '7', label: 'July' },
	{ value: '8', label: 'August' },
	{ value: '9', label: 'September' },
	{ value: '10', label: 'October' },
	{ value: '11', label: 'November' },
	{ value: '12', label: 'December' }
];

const WEEK_OPTIONS = [
	{ value: '1', label: 'First week' },
	{ value: '2', label: 'Second week' },
	{ value: '3', label: 'Third week' },
	{ value: '4', label: 'Fourth week' },
	{ value: '5', label: 'Fifth week' }
];

export const TimeScheduleBuilderV3: React.FC<TimeScheduleBuilderProps> = ({
	recurrence,
	existingSchedule,
	onComplete,
	onCancel
}) => {
	// 🎨 ULTRA-ELEGANT tree definition
	const buildTree = () => {
		const nodes = [];

		// Months (yearly only)
		if (recurrence === 'yearly') {
			nodes.push(
				multiSelect<TimeScheduleState>('months')
					.label('Which months?')
					.options(MONTH_OPTIONS)
					.update((state, value: string[]) => ({
						...state,
						selectedMonths: value.map(v => parseInt(v)).sort((a, b) => a - b)
					}))
					.build()
			);
		}

		// Weeks (monthly/yearly)
		if (recurrence === 'monthly' || recurrence === 'yearly') {
			nodes.push(
				multiSelect<TimeScheduleState>('weeks')
					.label('Which weeks? (leave empty for all)')
					.options(WEEK_OPTIONS)
					.allowEmpty()
					.update((state, value: string[]) => ({
						...state,
						selectedWeeks: value.map(v => parseInt(v)).sort((a, b) => a - b)
					}))
					.build()
			);
		}

		// Days (weekly/monthly/yearly)
		if (recurrence !== 'daily') {
			nodes.push(
				multiSelect<TimeScheduleState>('days')
					.label('Which days?')
					.options(DAY_OPTIONS)
					.update((state, value: string[]) => ({
						...state,
						selectedDays: value as DayOfWeek[]
					}))
					.build()
			);
		}

		// ✨ PER-DAY TIME EDITING - The ultimate elegance!
		nodes.push(
			forEach<TimeScheduleState, DayOfWeek>(state => 
				recurrence === 'daily'
					? ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as DayOfWeek[]
					: state.selectedDays
			).map((day) => [
				input<TimeScheduleState>(`${day}-start`)
					.label(`${day.charAt(0).toUpperCase() + day.slice(1)} - Start time (HH:MM)`)
					.placeholder('09:00')
					.default(state => state.timeRangesByDay[day]?.start_time || '09:00')
					.validate(value => /^\d{2}:\d{2}$/.test(value) || 'Use HH:MM format (e.g., 09:00)')
					.path(`timeRangesByDay.${day}.start_time`)  // 🎯 Nested path update!
					.build(),
				
				input<TimeScheduleState>(`${day}-end`)
					.label(`${day.charAt(0).toUpperCase() + day.slice(1)} - End time (HH:MM)`)
					.placeholder('17:00')
					.default(state => state.timeRangesByDay[day]?.end_time || '17:00')
					.validate(value => /^\d{2}:\d{2}$/.test(value) || 'Use HH:MM format (e.g., 17:00)')
					.path(`timeRangesByDay.${day}.end_time`)  // 🎯 Nested path update!
					.build()
			])
		);

		return wizard<TimeScheduleState>('time-schedule', nodes);
	};

	// Build schedule from state
	const buildSchedule = (state: TimeScheduleState): AvailabilityWindow => {
		const daysToUse = recurrence === 'daily'
			? (['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as DayOfWeek[])
			: state.selectedDays;
		
		// Group days by time range
		const groupedByTime: Record<string, DayOfWeek[]> = {};
		for (const day of daysToUse) {
			const range = state.timeRangesByDay[day] || { start_time: '09:00', end_time: '17:00' };
			const key = `${range.start_time}-${range.end_time}`;
			if (!groupedByTime[key]) groupedByTime[key] = [];
			groupedByTime[key].push(day);
		}
		
		const daySchedules = Object.entries(groupedByTime).map(([timeKey, days]) => {
			const [start, end] = timeKey.split('-');
			return {
				days,
				time_ranges: [{ start_time: start, end_time: end }]
			};
		});
		
		// Build based on recurrence
		if (recurrence === 'daily') {
			return daySchedules.length === 1 
				? { time_ranges: daySchedules[0].time_ranges }
				: { day_schedules: daySchedules };
		} else if (recurrence === 'weekly') {
			return { day_schedules: daySchedules };
		} else if (recurrence === 'monthly') {
			return state.selectedWeeks.length > 0
				? { week_schedules: [{ weeks: state.selectedWeeks, day_schedules: daySchedules }] }
				: { day_schedules: daySchedules };
		} else if (recurrence === 'yearly') {
			return {
				month_schedules: state.selectedMonths.map(month => ({
					month,
					...(state.selectedWeeks.length > 0
						? { week_schedules: [{ weeks: state.selectedWeeks, day_schedules: daySchedules }] }
						: { day_schedules: daySchedules })
				}))
			};
		}
		
		return {};
	};

	const tree = buildTree();
	const initialState: TimeScheduleState = {
		recurrence,
		selectedMonths: [],
		selectedWeeks: [],
		selectedDays: [],
		timeRangesByDay: {} as Record<DayOfWeek, { start_time: string; end_time: string }>
	};

	return (
		<Box flexDirection="column">
			<TreeZipper
				tree={tree}
				initialState={initialState}
				onComplete={(state) => onComplete(buildSchedule(state))}
				onCancel={onCancel}
			/>
		</Box>
	);
};


