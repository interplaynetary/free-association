import React from 'react';
import { TreeZipper, TreeNode } from '../utils/TreeZipper';
import type { AvailabilityWindow, DayOfWeek } from '../../schemas';

interface TimeScheduleState {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	selectedMonths: number[];
	selectedWeeks: number[];
	selectedDays: DayOfWeek[];
	timeRangesByDay: Record<DayOfWeek, { start_time: string; end_time: string }>;
}

interface TimeScheduleBuilderProps {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	existingSchedule?: AvailabilityWindow;
	onComplete: (schedule: AvailabilityWindow) => void;
	onCancel: () => void;
}

const DAY_OPTIONS = [
	{ value: 'monday', label: 'Monday' },
	{ value: 'tuesday', label: 'Tuesday' },
	{ value: 'wednesday', label: 'Wednesday' },
	{ value: 'thursday', label: 'Thursday' },
	{ value: 'friday', label: 'Friday' },
	{ value: 'saturday', label: 'Saturday' },
	{ value: 'sunday', label: 'Sunday' }
] as const;

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
] as const;

const WEEK_OPTIONS = [
	{ value: '1', label: 'First week' },
	{ value: '2', label: 'Second week' },
	{ value: '3', label: 'Third week' },
	{ value: '4', label: 'Fourth week' },
	{ value: '5', label: 'Fifth week' }
] as const;

export const TimeScheduleBuilderV2: React.FC<TimeScheduleBuilderProps> = ({
	recurrence,
	existingSchedule,
	onComplete,
	onCancel
}) => {
	// Build tree structure based on recurrence type
	const buildTree = (): TreeNode<TimeScheduleState> => {
		const children: TreeNode<TimeScheduleState>[] = [];

		// Add months node for yearly
		if (recurrence === 'yearly') {
			children.push({
				id: 'months',
				type: 'multi-select',
				label: 'Which months?',
				options: MONTH_OPTIONS,
				onExit: (state, value: string[]) => ({
					...state,
					selectedMonths: value.map(v => parseInt(v)).sort((a, b) => a - b)
				})
			});
		}

		// Add weeks node for monthly/yearly
		if (recurrence === 'monthly' || recurrence === 'yearly') {
			children.push({
				id: 'weeks',
				type: 'multi-select',
				label: 'Which weeks? (leave empty for all weeks)',
				options: WEEK_OPTIONS,
				allowEmpty: true,
				onExit: (state, value: string[]) => ({
					...state,
					selectedWeeks: value.map(v => parseInt(v)).sort((a, b) => a - b)
				})
			});
		}

		// Add days node for weekly/monthly/yearly
		if (recurrence !== 'daily') {
			children.push({
				id: 'days',
				type: 'multi-select',
				label: 'Which days?',
				options: DAY_OPTIONS,
				onExit: (state, value: string[]) => ({
					...state,
					selectedDays: value as DayOfWeek[]
				})
			});
		}

		// Add time nodes (dynamic per day)
		children.push({
			id: 'times',
			type: 'branch',
			label: 'Set times for each day',
			children: (state) => {
				const daysToEdit = recurrence === 'daily'
					? (['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as DayOfWeek[])
					: state.selectedDays;

				// Create start/end nodes for each day
				const timeNodes: TreeNode<TimeScheduleState>[] = [];
				
				for (const day of daysToEdit) {
					timeNodes.push(
						{
							id: `${day}-start`,
							type: 'input',
							inputType: 'time',
							label: `${day.charAt(0).toUpperCase() + day.slice(1)} - Start time (HH:MM)`,
							placeholder: '09:00',
							defaultValue: (state) => state.timeRangesByDay[day]?.start_time,
							validate: (value) => {
								if (!/^\d{2}:\d{2}$/.test(value)) {
									return 'Please use HH:MM format (e.g., 09:00)';
								}
								return true;
							},
							onExit: (state, value: string) => ({
								...state,
								timeRangesByDay: {
									...state.timeRangesByDay,
									[day]: {
										...state.timeRangesByDay[day],
										start_time: value
									}
								}
							})
						},
						{
							id: `${day}-end`,
							type: 'input',
							inputType: 'time',
							label: `${day.charAt(0).toUpperCase() + day.slice(1)} - End time (HH:MM)`,
							placeholder: '17:00',
							defaultValue: (state) => state.timeRangesByDay[day]?.end_time,
							validate: (value) => {
								if (!/^\d{2}:\d{2}$/.test(value)) {
									return 'Please use HH:MM format (e.g., 17:00)';
								}
								return true;
							},
							onExit: (state, value: string) => ({
								...state,
								timeRangesByDay: {
									...state.timeRangesByDay,
									[day]: {
										...state.timeRangesByDay[day],
										end_time: value
									}
								}
							})
						}
					);
				}
				
				return timeNodes;
			}
		});

		return {
			id: 'root',
			type: 'branch',
			children
		};
	};

	// Parse existing schedule to initial state
	const parseExisting = (): TimeScheduleState => {
		// ... (similar parsing logic as before)
		return {
			recurrence,
			selectedMonths: [],
			selectedWeeks: [],
			selectedDays: [],
			timeRangesByDay: {} as Record<DayOfWeek, { start_time: string; end_time: string }>
		};
	};

	// Build final schedule from state
	const buildSchedule = (state: TimeScheduleState): AvailabilityWindow => {
		// Group days by time range
		const groupedByTime: Record<string, DayOfWeek[]> = {};
		
		const daysToUse = recurrence === 'daily'
			? (['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'] as DayOfWeek[])
			: state.selectedDays;
		
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
		let schedule: AvailabilityWindow = {};
		
		if (recurrence === 'daily') {
			schedule = daySchedules.length === 1 
				? { time_ranges: daySchedules[0].time_ranges }
				: { day_schedules: daySchedules };
		} else if (recurrence === 'weekly') {
			schedule = { day_schedules: daySchedules };
		} else if (recurrence === 'monthly') {
			schedule = state.selectedWeeks.length > 0
				? { week_schedules: [{ weeks: state.selectedWeeks, day_schedules: daySchedules }] }
				: { day_schedules: daySchedules };
		} else if (recurrence === 'yearly') {
			schedule = {
				month_schedules: state.selectedMonths.map(month => ({
					month,
					...(state.selectedWeeks.length > 0
						? { week_schedules: [{ weeks: state.selectedWeeks, day_schedules: daySchedules }] }
						: { day_schedules: daySchedules })
				}))
			};
		}
		
		return schedule;
	};

	const tree = buildTree();
	const initialState = parseExisting();

	return (
		<TreeZipper
			tree={tree}
			initialState={initialState}
			onComplete={(state) => onComplete(buildSchedule(state))}
			onCancel={onCancel}
		/>
	);
};

