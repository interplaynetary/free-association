/**
 * Playnet CLI - Main Application
 * 
 * Beautiful interactive CLI for the Free Association Protocol
 */

import React, { useState } from 'react';
import { Box, Text, useApp, useInput } from 'ink';
import { Logo } from './Logo.js';
import { Menu, MenuItem } from './Menu.js';
import { AllocationDisplay } from './AllocationDisplay.js';
import { StepByStepDisplay } from './StepByStepDisplay.js';
import { runAliceIteration } from '../examples/terminal-example.js';
import type { Commitment } from '../schemas';

type Screen = 'menu' | 'running' | 'results' | 'stepByStep' | 'exit';

export const App: React.FC = () => {
	const { exit } = useApp();
	const [screen, setScreen] = useState<Screen>('menu');
	const [result, setResult] = useState<any>(null);
	const [loading, setLoading] = useState(false);

	// Example commitments for step-by-step mode
	const exampleCommitments: Record<string, Commitment> = {
		'alice-pub-key': {
			capacity_slots: [{
				id: 'alice-food-1',
				need_type_id: 'food',
				quantity: 50,
				name: 'Food',
				availability_window: { time_ranges: [{ start_time: '09:00', end_time: '17:00' }] },
				location: { city: 'San Francisco', country: 'USA' }
			}],
			need_slots: [{
				id: 'alice-tutoring-1',
				need_type_id: 'tutoring',
				quantity: 10,
				name: 'Tutoring',
				availability_window: { time_ranges: [{ start_time: '18:00', end_time: '20:00' }] },
				location: { city: 'San Francisco', country: 'USA' }
			}],
			global_recognition_weights: {
				'bob-pub-key': 0.6,
				'carol-pub-key': 0.4
			},
			itcStamp: { id: 1, event: 0 },
			timestamp: Date.now()
		},
		'bob-pub-key': {
			capacity_slots: [{
				id: 'bob-tutoring-1',
				need_type_id: 'tutoring',
				quantity: 20,
				name: 'Tutoring',
				availability_window: { time_ranges: [{ start_time: '18:00', end_time: '21:00' }] },
				location: { city: 'San Francisco', country: 'USA' }
			}],
			need_slots: [{
				id: 'bob-food-1',
				need_type_id: 'food',
				quantity: 30,
				name: 'Food',
				availability_window: { time_ranges: [{ start_time: '09:00', end_time: '17:00' }] },
				location: { city: 'San Francisco', country: 'USA' }
			}],
			global_recognition_weights: {
				'alice-pub-key': 0.7,
				'carol-pub-key': 0.3
			},
			itcStamp: { id: 1, event: 0 },
			timestamp: Date.now()
		},
		'carol-pub-key': {
			capacity_slots: [
				{
					id: 'carol-food-1',
					need_type_id: 'food',
					quantity: 40,
					name: 'Food',
					availability_window: { time_ranges: [{ start_time: '10:00', end_time: '16:00' }] },
					location: { city: 'San Francisco', country: 'USA' }
				},
				{
					id: 'carol-tutoring-1',
					need_type_id: 'tutoring',
					quantity: 15,
					name: 'Tutoring',
					availability_window: { time_ranges: [{ start_time: '19:00', end_time: '21:00' }] },
					location: { city: 'San Francisco', country: 'USA' }
				}
			],
			need_slots: [
				{
					id: 'carol-food-2',
					need_type_id: 'food',
					quantity: 10,
					name: 'Food',
					availability_window: { time_ranges: [{ start_time: '12:00', end_time: '14:00' }] },
					location: { city: 'San Francisco', country: 'USA' }
				},
				{
					id: 'carol-tutoring-2',
					need_type_id: 'tutoring',
					quantity: 5,
					name: 'Tutoring',
					availability_window: { time_ranges: [{ start_time: '18:00', end_time: '19:00' }] },
					location: { city: 'San Francisco', country: 'USA' }
				}
			],
			global_recognition_weights: {
				'alice-pub-key': 0.5,
				'bob-pub-key': 0.5
			},
			itcStamp: { id: 1, event: 0 },
			timestamp: Date.now()
		}
	};

	const menuItems: MenuItem[] = [
		{
			id: 'stepByStep',
			label: '▶️  Step-by-Step Iteration',
			description: 'Watch convergence happen one iteration at a time (RECOMMENDED!)'
		},
		{
			id: 'example',
			label: '⚡ Quick Example',
			description: 'Run a single iteration instantly'
		},
		{
			id: 'exit',
			label: '🚪 Exit',
			description: 'Quit the application'
		}
	];

	const handleMenuSelect = async (itemId: string) => {
		if (itemId === 'exit') {
			setScreen('exit');
			setTimeout(() => exit(), 500);
			return;
		}

		if (itemId === 'stepByStep') {
			setScreen('stepByStep');
			return;
		}

		if (itemId === 'example') {
			setScreen('running');
			setLoading(true);

			// Simulate async operation
			setTimeout(() => {
				try {
					const allocationResult = runAliceIteration();
					setResult(allocationResult);
					setScreen('results');
				} catch (error) {
					console.error('Error running example:', error);
					setScreen('menu');
				} finally {
					setLoading(false);
				}
			}, 1000);
		}
	};

	const handleBack = () => {
		setScreen('menu');
		setResult(null);
	};

	// Handle key press when on results screen
	useInput((input, key) => {
		if (screen === 'results') {
			handleBack();
		}
	});

	return (
		<Box flexDirection="column" padding={2}>
			<Logo />

			{screen === 'menu' && (
				<Menu items={menuItems} onSelect={handleMenuSelect} />
			)}

			{screen === 'running' && (
				<Box borderStyle="round" borderColor="yellow" padding={2}>
					<Text color="yellow">⏳ Running allocation algorithm...</Text>
				</Box>
			)}

			{screen === 'stepByStep' && (
				<StepByStepDisplay
					commitments={exampleCommitments}
					myPubKey="alice-pub-key"
					onComplete={handleBack}
				/>
			)}

			{screen === 'results' && result && (
				<Box flexDirection="column">
					<AllocationDisplay result={result} myName="Alice" />
					<Box marginTop={1} justifyContent="center">
						<Text dimColor>Press any key to return to menu...</Text>
					</Box>
				</Box>
			)}

			{screen === 'exit' && (
				<Box borderStyle="round" borderColor="magenta" padding={2}>
					<Text color="magenta">👋 Goodbye! Thanks for playing!</Text>
				</Box>
			)}

			{screen === 'results' && (
				<Box marginTop={1} justifyContent="center">
					<Text dimColor>
						Tip: Use Step-by-Step mode to watch convergence!
					</Text>
				</Box>
			)}
		</Box>
	);
};

