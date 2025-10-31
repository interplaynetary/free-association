import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { Input } from './Input';
import type { AvailabilityWindow, DayOfWeek, TimeRange } from '../../schemas';

interface TimeScheduleBuilderProps {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	existingSchedule?: AvailabilityWindow;
	onComplete: (schedule: AvailabilityWindow) => void;
	onCancel: () => void;
}

type Step = 
	| 'months'      // yearly only
	| 'weeks'       // monthly/yearly optional
	| 'days'        // weekly/monthly/yearly
	| 'time-edit'   // Per-day time editing
	| 'confirm';

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
	{ value: '5', label: 'Fifth week (if applicable)' }
] as const;

export const TimeScheduleBuilder: React.FC<TimeScheduleBuilderProps> = ({
	recurrence,
	existingSchedule,
	onComplete,
	onCancel
}) => {
	// Parse existing schedule if provided
	const parseExistingSchedule = () => {
		if (!existingSchedule) {
			return { 
				months: [], 
				weeks: [], 
				days: [], 
				timeRangesByDay: {} as Record<DayOfWeek, TimeRange>
			};
		}
		
		let months: number[] = [];
		let weeks: number[] = [];
		let days: DayOfWeek[] = [];
		let timeRangesByDay: Record<DayOfWeek, TimeRange> = {} as Record<DayOfWeek, TimeRange>;
		
		// Helper to extract time ranges from day_schedules array
		const extractFromDaySchedules = (daySchedules: any[]) => {
			const allDays: DayOfWeek[] = [];
			const timeMap: Record<DayOfWeek, TimeRange> = {} as Record<DayOfWeek, TimeRange>;
			
			for (const ds of daySchedules) {
				if (ds.days && ds.time_ranges && ds.time_ranges.length > 0) {
					const range = ds.time_ranges[0];
					for (const day of ds.days) {
						allDays.push(day);
						timeMap[day] = { start_time: range.start_time, end_time: range.end_time };
					}
				}
			}
			
			return { days: allDays, timeRangesByDay: timeMap };
		};
		
		// Extract from month_schedules
		if (existingSchedule.month_schedules && existingSchedule.month_schedules.length > 0) {
			months = existingSchedule.month_schedules.map(ms => ms.month);
			const firstMonth = existingSchedule.month_schedules[0];
			
			if (firstMonth.week_schedules && firstMonth.week_schedules.length > 0) {
				weeks = firstMonth.week_schedules[0].weeks;
				if (firstMonth.week_schedules[0].day_schedules) {
					const extracted = extractFromDaySchedules(firstMonth.week_schedules[0].day_schedules);
					days = extracted.days;
					timeRangesByDay = extracted.timeRangesByDay;
				}
			} else if (firstMonth.day_schedules) {
				const extracted = extractFromDaySchedules(firstMonth.day_schedules);
				days = extracted.days;
				timeRangesByDay = extracted.timeRangesByDay;
			}
		}
		// Extract from week_schedules
		else if (existingSchedule.week_schedules && existingSchedule.week_schedules.length > 0) {
			weeks = existingSchedule.week_schedules[0].weeks;
			if (existingSchedule.week_schedules[0].day_schedules) {
				const extracted = extractFromDaySchedules(existingSchedule.week_schedules[0].day_schedules);
				days = extracted.days;
				timeRangesByDay = extracted.timeRangesByDay;
			}
		}
		// Extract from day_schedules
		else if (existingSchedule.day_schedules) {
			const extracted = extractFromDaySchedules(existingSchedule.day_schedules);
			days = extracted.days;
			timeRangesByDay = extracted.timeRangesByDay;
		}
		// Extract from time_ranges (daily - same time for all days)
		else if (existingSchedule.time_ranges && existingSchedule.time_ranges.length > 0) {
			const range = existingSchedule.time_ranges[0];
			// For daily, we'll handle this separately
			timeRangesByDay = {} as Record<DayOfWeek, TimeRange>;
		}
		
		return { months, weeks, days, timeRangesByDay };
	};
	
	const existing = parseExistingSchedule();
	
	// Start directly in appropriate step based on recurrence
	const initialStep: Step = 
		recurrence === 'yearly' ? 'months' :
		recurrence === 'monthly' ? 'weeks' :
		recurrence === 'weekly' ? 'days' :
		'time-edit';
	
	const [step, setStep] = useState<Step>(initialStep);
	const [selectedIndex, setSelectedIndex] = useState(0);
	
	const [selectedMonths, setSelectedMonths] = useState<number[]>(existing.months);
	const [selectedWeeks, setSelectedWeeks] = useState<number[]>(existing.weeks);
	const [selectedDays, setSelectedDays] = useState<DayOfWeek[]>(existing.days);
	
	// NEW: Per-day time ranges
	const [timeRangesByDay, setTimeRangesByDay] = useState<Record<DayOfWeek, TimeRange>>(existing.timeRangesByDay);
	
	// NEW: Track which day we're currently editing
	const [currentDayIndex, setCurrentDayIndex] = useState(0);
	const [currentField, setCurrentField] = useState<'start' | 'end'>('start');

	// Get available steps for current recurrence
	const getSteps = (): Step[] => {
		if (recurrence === 'yearly') return ['months', 'weeks', 'days', 'time-edit', 'confirm'];
		if (recurrence === 'monthly') return ['weeks', 'days', 'time-edit', 'confirm'];
		if (recurrence === 'weekly') return ['days', 'time-edit', 'confirm'];
		return ['time-edit', 'confirm'];
	};

	const steps = getSteps();
	const currentStepIndex = steps.indexOf(step);

	// Get days to edit (for recurrence types that need it)
	const getDaysToEdit = (): DayOfWeek[] => {
		if (recurrence === 'daily') {
			// For daily, all days
			return ['monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'sunday'];
		}
		// For weekly/monthly/yearly, use selected days
		return selectedDays;
	};

	const daysToEdit = getDaysToEdit();
	const currentDay = daysToEdit[currentDayIndex];

	// Navigation helpers
	const goToPreviousStep = () => {
		if (step === 'time-edit') {
			// Use functional updates to avoid closure issues
			setCurrentField(prevField => {
				if (prevField === 'end') {
					// end → start (same day)
					return 'start';
				} else {
					// start → check if we can go to previous day
					setCurrentDayIndex(prevIndex => {
						if (prevIndex > 0) {
							// Go to previous day's end
							return prevIndex - 1;
						} else {
							// First day's start → go to previous step
							if (currentStepIndex > 0) {
								setSelectedIndex(0);
								setStep(steps[currentStepIndex - 1]);
							}
							return prevIndex; // Stay on same index
						}
					});
					return 'end'; // Reset to end for previous day
				}
			});
		} else {
			if (currentStepIndex > 0) {
				setSelectedIndex(0);
				setStep(steps[currentStepIndex - 1]);
			}
		}
	};

	const goToNextStep = () => {
		if (step === 'time-edit') {
			// Use functional updates to avoid closure issues
			setCurrentField(prevField => {
				if (prevField === 'start') {
					// start → end (same day)
					return 'end';
				} else {
					// end → check if we can move to next day
					setCurrentDayIndex(prevIndex => {
						if (prevIndex < daysToEdit.length - 1) {
							// Move to next day's start
							return prevIndex + 1;
						} else {
							// Last day's end → go to next step
							if (currentStepIndex < steps.length - 1) {
								setSelectedIndex(0);
								setStep(steps[currentStepIndex + 1]);
							}
							return prevIndex; // Stay on same index
						}
					});
					return 'start'; // Reset to start for next day
				}
			});
		} else {
			if (currentStepIndex < steps.length - 1) {
				setSelectedIndex(0);
				setStep(steps[currentStepIndex + 1]);
			}
		}
	};

	// Single useInput hook (fixes Rules of Hooks violation)
	useInput((input, key) => {
		// Special handling for time-edit navigation (avoid closure issues)
		if (step === 'time-edit' && (key.leftArrow || key.rightArrow)) {
			if (key.rightArrow) {
				// Navigate forward
				if (currentField === 'start') {
					setCurrentField('end');
				} else if (currentDayIndex < daysToEdit.length - 1) {
					setCurrentDayIndex(currentDayIndex + 1);
					setCurrentField('start');
				} else {
					// Last field - go to confirm
					if (currentStepIndex < steps.length - 1) {
						setSelectedIndex(0);
						setStep(steps[currentStepIndex + 1]);
					}
				}
			} else if (key.leftArrow) {
				// Navigate backward
				if (currentField === 'end') {
					setCurrentField('start');
				} else if (currentDayIndex > 0) {
					setCurrentDayIndex(currentDayIndex - 1);
					setCurrentField('end');
				} else {
					// First field - go back to days
					if (currentStepIndex > 0) {
						setSelectedIndex(0);
						setStep(steps[currentStepIndex - 1]);
					}
				}
			}
			return;
		}
		
		// Global navigation for other steps
		if (key.leftArrow) {
			goToPreviousStep();
			return;
		} else if (key.rightArrow) {
			goToNextStep();
			return;
		}

		// Handle months selection
		if (step === 'months') {
			if (key.upArrow) {
				setSelectedIndex(Math.max(0, selectedIndex - 1));
			} else if (key.downArrow) {
				setSelectedIndex(Math.min(MONTH_OPTIONS.length - 1, selectedIndex + 1));
			} else if (input === ' ') {  // Space key
				const monthNum = parseInt(MONTH_OPTIONS[selectedIndex].value);
				if (selectedMonths.includes(monthNum)) {
					setSelectedMonths(selectedMonths.filter(m => m !== monthNum));
				} else {
					setSelectedMonths([...selectedMonths, monthNum].sort((a, b) => a - b));
				}
			} else if (key.return) {
				// Allow proceeding even with no selection
				setSelectedIndex(0);
				goToNextStep();
			} else if (key.escape) {
				onCancel();
			}
			return;
		}

		// Handle weeks selection
		if (step === 'weeks') {
			if (key.upArrow) {
				setSelectedIndex(Math.max(0, selectedIndex - 1));
			} else if (key.downArrow) {
				setSelectedIndex(Math.min(WEEK_OPTIONS.length - 1, selectedIndex + 1));
			} else if (input === ' ') {  // Space key
				const weekNum = parseInt(WEEK_OPTIONS[selectedIndex].value);
				if (selectedWeeks.includes(weekNum)) {
					setSelectedWeeks(selectedWeeks.filter(w => w !== weekNum));
				} else {
					setSelectedWeeks([...selectedWeeks, weekNum].sort((a, b) => a - b));
				}
			} else if (key.return) {
				// Allow skipping (empty = all weeks)
				setSelectedIndex(0);
				goToNextStep();
			} else if (key.escape) {
				onCancel();
			}
			return;
		}

		// Handle days selection
		if (step === 'days') {
			if (key.upArrow) {
				setSelectedIndex(Math.max(0, selectedIndex - 1));
			} else if (key.downArrow) {
				setSelectedIndex(Math.min(DAY_OPTIONS.length - 1, selectedIndex + 1));
			} else if (input === ' ') {  // Space key
				const day = DAY_OPTIONS[selectedIndex].value as DayOfWeek;
				if (selectedDays.includes(day)) {
					setSelectedDays(selectedDays.filter(d => d !== day));
				} else {
					setSelectedDays([...selectedDays, day]);
				}
			} else if (key.return) {
				// Allow proceeding even with no selection
				setSelectedIndex(0);
				goToNextStep();
			} else if (key.escape) {
				onCancel();
			}
			return;
		}

		// Handle other steps
		if (key.escape) {
			onCancel();
		}
	});

	// Handle time input for current day
	const handleTimeInput = (time: string) => {
		if (!currentDay) return;
		
		const existingRange = timeRangesByDay[currentDay] || { start_time: '09:00', end_time: '17:00' };
		
		if (currentField === 'start') {
			setTimeRangesByDay({
				...timeRangesByDay,
				[currentDay]: { ...existingRange, start_time: time }
			});
		} else {
			setTimeRangesByDay({
				...timeRangesByDay,
				[currentDay]: { ...existingRange, end_time: time }
			});
		}
		// Don't auto-advance - let user navigate with ← →
	};

	const handleConfirm = () => {
		// Group days by their time ranges (schema optimization)
		const groupedByTime: Record<string, DayOfWeek[]> = {};
		
		for (const day of daysToEdit) {
			const range = timeRangesByDay[day] || { start_time: '09:00', end_time: '17:00' };
			const key = `${range.start_time}-${range.end_time}`;
			if (!groupedByTime[key]) {
				groupedByTime[key] = [];
			}
			groupedByTime[key].push(day);
		}
		
		// Build day_schedules array from grouped days
		const daySchedules = Object.entries(groupedByTime).map(([timeKey, days]) => {
			const [start, end] = timeKey.split('-');
			return {
				days,
				time_ranges: [{ start_time: start, end_time: end }]
			};
		});
		
		// Build the availability window based on recurrence
		let schedule: AvailabilityWindow = {};

		if (recurrence === 'daily') {
			// For daily, use time_ranges if all days have same time, otherwise day_schedules
			if (daySchedules.length === 1) {
				schedule = { time_ranges: daySchedules[0].time_ranges };
			} else {
				schedule = { day_schedules: daySchedules };
			}
		} else if (recurrence === 'weekly') {
			schedule = { day_schedules: daySchedules };
		} else if (recurrence === 'monthly') {
			if (selectedWeeks.length > 0) {
				schedule = {
					week_schedules: [{
						weeks: selectedWeeks,
						day_schedules: daySchedules
					}]
				};
			} else {
				schedule = { day_schedules: daySchedules };
			}
		} else if (recurrence === 'yearly') {
			schedule = {
				month_schedules: selectedMonths.map(month => ({
					month,
					...(selectedWeeks.length > 0 ? {
						week_schedules: [{
							weeks: selectedWeeks,
							day_schedules: daySchedules
						}]
					} : {
						day_schedules: daySchedules
					})
				}))
			};
		}

		onComplete(schedule);
	};

	// Breadcrumb display
	const renderBreadcrumb = () => {
		const breadcrumbLabels: Record<Step, string> = {
			'months': 'Months',
			'weeks': 'Weeks',
			'days': 'Days',
			'time-edit': 'Times',
			'confirm': 'Confirm'
		};

		const breadcrumbText = steps.map((s, i) => {
			const isCurrent = s === step;
			const isPast = i < currentStepIndex;
			return `${i > 0 ? ' → ' : ''}${isCurrent ? '[ ' : ''}${breadcrumbLabels[s]}${isCurrent ? ' ]' : ''}`;
		}).join('');

		return (
			<Box marginBottom={1} flexDirection="column">
				<Text dimColor>{breadcrumbText}</Text>
				<Text dimColor>(← → to navigate between steps)</Text>
			</Box>
		);
	};

	// Render based on step
	if (step === 'months') {
		return (
			<Box flexDirection="column">
				{renderBreadcrumb()}
				<Text bold>Which months? <Text dimColor>(Space to toggle, Enter/→ to continue)</Text></Text>
				<Box flexDirection="column" marginTop={1}>
					{MONTH_OPTIONS.map((option, index) => {
						const isSelected = selectedIndex === index;
						const isChecked = selectedMonths.includes(parseInt(option.value));
						return (
							<Box key={option.value}>
								<Text color={isSelected ? 'cyan' : undefined}>
									{isSelected ? '▶ ' : '  '}
									{isChecked ? '[✓] ' : '[ ] '}
									{option.label}
								</Text>
							</Box>
						);
					})}
				</Box>
				<Box marginTop={1}>
					<Text dimColor>
						Selected: {selectedMonths.length > 0 ? 
							selectedMonths.map(m => MONTH_OPTIONS[m-1].label).join(', ') : 
							'none'}
					</Text>
				</Box>
			</Box>
		);
	}

	if (step === 'weeks') {
		return (
			<Box flexDirection="column">
				{renderBreadcrumb()}
				<Text bold>Which weeks? <Text dimColor>(Space to toggle, Enter/→ to continue, ← to go back)</Text></Text>
				<Box flexDirection="column" marginTop={1}>
					{WEEK_OPTIONS.map((option, index) => {
						const isSelected = selectedIndex === index;
						const isChecked = selectedWeeks.includes(parseInt(option.value));
						return (
							<Box key={option.value}>
								<Text color={isSelected ? 'cyan' : undefined}>
									{isSelected ? '▶ ' : '  '}
									{isChecked ? '[✓] ' : '[ ] '}
									{option.label}
								</Text>
							</Box>
						);
					})}
				</Box>
				<Box marginTop={1}>
					<Text dimColor>
						Selected: {selectedWeeks.length > 0 ? 
							selectedWeeks.map(w => WEEK_OPTIONS[w-1].label).join(', ') : 
							'all weeks'}
					</Text>
				</Box>
			</Box>
		);
	}

	if (step === 'days') {
		return (
			<Box flexDirection="column">
				{renderBreadcrumb()}
				<Text bold>Which days? <Text dimColor>(Space to toggle, Enter/→ to continue, ← to go back)</Text></Text>
				<Box flexDirection="column" marginTop={1}>
					{DAY_OPTIONS.map((option, index) => {
						const isSelected = selectedIndex === index;
						const isChecked = selectedDays.includes(option.value as DayOfWeek);
						return (
							<Box key={option.value}>
								<Text color={isSelected ? 'cyan' : undefined}>
									{isSelected ? '▶ ' : '  '}
									{isChecked ? '[✓] ' : '[ ] '}
									{option.label}
								</Text>
							</Box>
						);
					})}
				</Box>
				<Box marginTop={1}>
					<Text dimColor>
						Selected: {selectedDays.length > 0 ? 
							selectedDays.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ') : 
							'none'}
					</Text>
				</Box>
			</Box>
		);
	}

	if (step === 'time-edit') {
		if (!currentDay) {
			return (
				<Box flexDirection="column">
					{renderBreadcrumb()}
					<Text color="yellow">⚠️  No days selected. Please go back and select days.</Text>
				</Box>
			);
		}

		const dayLabel = currentDay.charAt(0).toUpperCase() + currentDay.slice(1);
		const existingRange = timeRangesByDay[currentDay] || { start_time: '09:00', end_time: '17:00' };
		const currentValue = currentField === 'start' ? existingRange.start_time : existingRange.end_time;
		
		// Progress indicator
		const totalFields = daysToEdit.length * 2; // start + end for each day
		const currentFieldNum = (currentDayIndex * 2) + (currentField === 'start' ? 1 : 2);
		
		// Debug: Show all stored times AND navigation state
		const debugInfo = daysToEdit.map(day => {
			const range = timeRangesByDay[day];
			return `${day}: ${range ? `${range.start_time}-${range.end_time}` : 'unset'}`;
		}).join(' | ');
		
		const navDebug = `DayIdx: ${currentDayIndex}/${daysToEdit.length - 1}, Field: ${currentField}, CurrentDay: ${currentDay}`;
		
		return (
			<Box flexDirection="column">
				{renderBreadcrumb()}
				<Box marginBottom={1}>
					<Text>
						<Text bold color="cyan">{dayLabel}</Text>
						{' '}({currentFieldNum}/{totalFields})
					</Text>
				</Box>
				<Box marginBottom={1}>
					<Text dimColor>TIMES: {debugInfo}</Text>
				</Box>
				<Box marginBottom={1}>
					<Text dimColor>NAV: {navDebug}</Text>
				</Box>
				<Input
					key={`${currentDay}-${currentField}`}
					label={currentField === 'start' ? 'Start time (HH:MM, 24-hour format)' : 'End time (HH:MM, 24-hour format)'}
					placeholder={currentField === 'start' ? '09:00' : '17:00'}
					defaultValue={currentValue}
					onSubmit={handleTimeInput}
					onCancel={onCancel}
					validate={(value) => {
						if (!/^\d{2}:\d{2}$/.test(value)) {
							return 'Please use HH:MM format (e.g., 09:00)';
						}
						return true;
					}}
				/>
				<Box marginTop={1}>
					<Text dimColor>← → to navigate days/times | Enter to save | Esc to cancel</Text>
				</Box>
				<Box marginTop={1}>
					<Text dimColor>
						{currentDayIndex > 0 || currentField === 'end' ? '← Previous ' : ''}
						{currentDayIndex < daysToEdit.length - 1 || currentField === 'start' ? '→ Next' : ''}
					</Text>
				</Box>
			</Box>
		);
	}

	if (step === 'confirm') {
		return (
			<Box flexDirection="column">
				{renderBreadcrumb()}
				<ConfirmSchedule
					recurrence={recurrence}
					months={selectedMonths}
					weeks={selectedWeeks}
					days={daysToEdit}
					timeRangesByDay={timeRangesByDay}
					onConfirm={handleConfirm}
					onCancel={onCancel}
				/>
			</Box>
		);
	}

	return null;
};

interface ConfirmScheduleProps {
	recurrence: 'daily' | 'weekly' | 'monthly' | 'yearly' | null;
	months: number[];
	weeks: number[];
	days: DayOfWeek[];
	timeRangesByDay: Record<DayOfWeek, TimeRange>;
	onConfirm: () => void;
	onCancel: () => void;
}

const ConfirmSchedule: React.FC<ConfirmScheduleProps> = ({
	recurrence,
	months,
	weeks,
	days,
	timeRangesByDay,
	onConfirm,
	onCancel
}) => {
	useInput((input, key) => {
		if (key.return) {
			onConfirm();
		} else if (key.escape) {
			onCancel();
		}
	});

	// Group days by time range for display
	const grouped: Record<string, DayOfWeek[]> = {};
	for (const day of days) {
		const range = timeRangesByDay[day] || { start_time: '09:00', end_time: '17:00' };
		const key = `${range.start_time}-${range.end_time}`;
		if (!grouped[key]) grouped[key] = [];
		grouped[key].push(day);
	}

	return (
		<Box flexDirection="column">
			<Text bold color="green">✓ Review your schedule:</Text>
			<Box marginTop={1} flexDirection="column" marginLeft={2}>
				<Text>Pattern: {recurrence}</Text>
				{months.length > 0 && (
					<Text>Months: {months.map(m => MONTH_OPTIONS[m-1].label).join(', ')}</Text>
				)}
				{weeks.length > 0 && (
					<Text>Weeks: {weeks.map(w => WEEK_OPTIONS[w-1].label).join(', ')}</Text>
				)}
				
				{/* Show per-day times */}
				<Box marginTop={1} flexDirection="column">
					<Text bold>Schedule:</Text>
					{Object.entries(grouped).map(([timeKey, groupedDays]) => {
						const [start, end] = timeKey.split('-');
						const dayLabels = groupedDays.map(d => d.charAt(0).toUpperCase() + d.slice(1)).join(', ');
						return (
							<Box key={timeKey} marginLeft={2}>
								<Text>
									<Text color="cyan">{dayLabels}</Text>: {start} - {end}
								</Text>
							</Box>
						);
					})}
				</Box>
			</Box>
			<Box marginTop={2}>
				<Text dimColor>← to edit | Enter to confirm | Esc to cancel</Text>
			</Box>
		</Box>
	);
};
