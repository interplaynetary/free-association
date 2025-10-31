import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { StatusBadge } from '../components/StatusBadge';
import type { Commitment, AvailabilitySlot, NeedSlot } from '../../schemas';

interface AllocationResultsProps {
	myPubKey: string;
	allCommitments: Record<string, Commitment>;
	onBack: () => void;
}

type ViewMode = 'overview' | 'capacities' | 'needs' | 'network';

export const AllocationResults: React.FC<AllocationResultsProps> = ({
	myPubKey,
	allCommitments,
	onBack
}) => {
	const [viewMode, setViewMode] = useState<ViewMode>('overview');

	useInput((input, key) => {
		if (key.escape || input.toLowerCase() === 'q') {
			onBack();
		} else if (input.toLowerCase() === 'o') {
			setViewMode('overview');
		} else if (input.toLowerCase() === 'c') {
			setViewMode('capacities');
		} else if (input.toLowerCase() === 'n') {
			setViewMode('needs');
		} else if (input.toLowerCase() === 'w') {
			setViewMode('network');
		}
	});

	const myCommitment = allCommitments[myPubKey];
	const myCapacities = myCommitment?.availability_slots || [];
	const myNeeds = myCommitment?.need_slots || [];
	const participantCount = Object.keys(allCommitments).length;

	// Overview mode
	if (viewMode === 'overview') {
		const totalCapacityQuantity = myCapacities.reduce((sum, c) => sum + c.quantity, 0);
		const totalNeedQuantity = myNeeds.reduce((sum, n) => sum + n.quantity, 0);

		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">📊 Allocation Overview</Text>

				<Box marginTop={1} flexDirection="column">
					<Text bold underline>Your Profile:</Text>
					<Box marginLeft={2} marginTop={1}>
						<Text>Public Key: {myPubKey.slice(0, 30)}...</Text>
						<Text>Status: <StatusBadge status="success" text="Active" /></Text>
					</Box>
				</Box>

				<Box marginTop={1} flexDirection="column">
					<Text bold underline>Your Capacities:</Text>
					<Box marginLeft={2} marginTop={1}>
						<Text>Count: {myCapacities.length}</Text>
						<Text>Total Quantity Offered: {totalCapacityQuantity.toFixed(2)}</Text>
					</Box>
				</Box>

				<Box marginTop={1} flexDirection="column">
					<Text bold underline>Your Needs:</Text>
					<Box marginLeft={2} marginTop={1}>
						<Text>Count: {myNeeds.length}</Text>
						<Text>Total Quantity Needed: {totalNeedQuantity.toFixed(2)}</Text>
					</Box>
				</Box>

				<Box marginTop={1} flexDirection="column">
					<Text bold underline>Network:</Text>
					<Box marginLeft={2} marginTop={1}>
						<Text>Total Participants: {participantCount}</Text>
					</Box>
				</Box>

				<Box marginTop={2} borderStyle="round" borderColor="yellow" padding={1}>
					<Text color="yellow">
						💡 To see actual allocation results, use:
					</Text>
					<Box marginTop={1} marginLeft={2}>
						<Text>▶️  Step-by-Step Allocation - Watch convergence in action</Text>
					</Box>
				</Box>

				<Box marginTop={2}>
					<Text dimColor>
						C Capacities | N Needs | W Network | Q/Esc Back
					</Text>
				</Box>
			</Box>
		);
	}

	// Capacities mode
	if (viewMode === 'capacities') {
		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">📊 Your Capacities</Text>

				{myCapacities.length === 0 ? (
					<Box marginTop={2}>
						<Text color="yellow">No capacities yet. Add some from the main menu!</Text>
					</Box>
				) : (
					<Box marginTop={1} flexDirection="column">
						{myCapacities.map(cap => (
							<Box key={cap.id} marginTop={1} flexDirection="column">
								<Text bold>
									{cap.emoji} {cap.name}
								</Text>
								<Box marginLeft={2}>
									<Text>
										Quantity: {cap.quantity} {cap.unit}
									</Text>
									<Text>Type: {cap.need_type_id}</Text>
									<Text>Location: {cap.location_type || 'any'}</Text>
									{cap.recurrence && (
										<Text>Recurrence: {cap.recurrence}</Text>
									)}
								</Box>
							</Box>
						))}
					</Box>
				)}

				<Box marginTop={2}>
					<Text dimColor>
						O Overview | N Needs | W Network | Q/Esc Back
					</Text>
				</Box>
			</Box>
		);
	}

	// Needs mode
	if (viewMode === 'needs') {
		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">📊 Your Needs</Text>

				{myNeeds.length === 0 ? (
					<Box marginTop={2}>
						<Text color="yellow">No needs yet. Add some from the main menu!</Text>
					</Box>
				) : (
					<Box marginTop={1} flexDirection="column">
						{myNeeds.map(need => (
							<Box key={need.id} marginTop={1} flexDirection="column">
								<Text bold>
									{need.emoji} {need.name}
								</Text>
								<Box marginLeft={2}>
									<Text>
										Quantity: {need.quantity} {need.unit}
									</Text>
									<Text>Type: {need.need_type_id}</Text>
									<Text>Location: {need.location_type || 'any'}</Text>
									{need.recurrence && (
										<Text>Recurrence: {need.recurrence}</Text>
									)}
								</Box>
							</Box>
						))}
					</Box>
				)}

				<Box marginTop={2}>
					<Text dimColor>
						O Overview | C Capacities | W Network | Q/Esc Back
					</Text>
				</Box>
			</Box>
		);
	}

	// Network mode
	if (viewMode === 'network') {
		const participants = Object.keys(allCommitments);
		
		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">📊 Network Overview</Text>

				<Box marginTop={1}>
					<Text>Total Participants: {participants.length}</Text>
				</Box>

				{participants.map(pubKey => {
					const isMe = pubKey === myPubKey;
					const commitment = allCommitments[pubKey];
					const capacities = commitment?.availability_slots || [];
					const needs = commitment?.need_slots || [];
					
					const totalOffered = capacities.reduce((sum, c) => sum + c.quantity, 0);
					const totalNeeded = needs.reduce((sum, n) => sum + n.quantity, 0);

					return (
						<Box key={pubKey} marginTop={1} flexDirection="column">
							<Text bold color={isMe ? 'green' : undefined}>
								{pubKey.slice(0, 30)}... {isMe ? '(you)' : ''}
							</Text>
							<Box marginLeft={2}>
								<Text>
									Capacities: {capacities.length} (total: {totalOffered.toFixed(1)})
								</Text>
								<Text>
									Needs: {needs.length} (total: {totalNeeded.toFixed(1)})
								</Text>
							</Box>
						</Box>
					);
				})}

				<Box marginTop={2}>
					<Text dimColor>
						O Overview | C Capacities | N Needs | Q/Esc Back
					</Text>
				</Box>
			</Box>
		);
	}

	return null;
};
