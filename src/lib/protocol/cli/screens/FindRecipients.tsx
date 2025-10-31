import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { Table } from '../components/Table';
import * as storage from '../data/storage';
import type { NeedSlot, AvailabilitySlot, Commitment } from '../../schemas';

interface FindRecipientsProps {
	myPubKey: string;
	myCapacities: AvailabilitySlot[];
	allCommitments: Record<string, Commitment>;
	onBack: () => void;
	onSelectCapacity: (capacityId: string) => void;
}

export const FindRecipients: React.FC<FindRecipientsProps> = ({
	myPubKey,
	myCapacities,
	allCommitments,
	onBack,
	onSelectCapacity
}) => {
	const [selectedCapacityIndex, setSelectedCapacityIndex] = useState(0);

	useInput((input, key) => {
		const lower = input.toLowerCase();
		if (key.escape || lower === 'q') {
			onBack();
		} else if (key.upArrow) {
			setSelectedCapacityIndex(Math.max(0, selectedCapacityIndex - 1));
		} else if (key.downArrow) {
			setSelectedCapacityIndex(Math.min(myCapacities.length - 1, selectedCapacityIndex + 1));
		} else if (key.return && myCapacities.length > 0) {
			onSelectCapacity(myCapacities[selectedCapacityIndex].id);
		}
	});

	if (myCapacities.length === 0) {
		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">🔍 Find Recipients for Your Capacities</Text>
				<Box marginTop={2}>
					<Text color="yellow">⚠️  You don't have any capacities yet</Text>
				</Box>
				<Box marginTop={1}>
					<Text dimColor>Add a capacity first, then come back to find recipients.</Text>
				</Box>
				<Box marginTop={2}>
					<Text dimColor>Press Esc to go back</Text>
				</Box>
			</Box>
		);
	}

	// Find potential recipients for each capacity
	const capacitiesWithRecipients = myCapacities.map(capacity => {
		const recipients: Array<{
			pubKey: string;
			need: NeedSlot;
			compatibility: number;
		}> = [];

		// Look through all other participants
		Object.entries(allCommitments).forEach(([pubKey, commitment]) => {
			if (pubKey === myPubKey) return; // Skip self

			// Check their needs
			commitment.need_slots?.forEach(need => {
				// Match by need type
				if (need.need_type_id === capacity.need_type_id) {
					// Calculate basic compatibility score (0-100)
					let compatibility = 50; // Base score for type match

					// Bonus for matching location type
					if (need.location_type === capacity.location_type) {
						compatibility += 20;
					}

					// Bonus for same unit
					if (need.unit === capacity.unit) {
						compatibility += 10;
					}

					// Bonus if we have enough to fulfill their need
					if (capacity.quantity >= need.quantity) {
						compatibility += 20;
					}

					recipients.push({
						pubKey,
						need,
						compatibility: Math.min(100, compatibility)
					});
				}
			});
		});

		// Sort by compatibility
		recipients.sort((a, b) => b.compatibility - a.compatibility);

		return {
			capacity,
			recipients
		};
	});

	const selectedCapacity = capacitiesWithRecipients[selectedCapacityIndex];

	return (
		<Box flexDirection="column" padding={1}>
			<Text bold color="cyan">🔍 Find Recipients for Your Capacities</Text>

			<Box marginTop={1} flexDirection="column">
				<Text bold>Your Capacities:</Text>
				{capacitiesWithRecipients.map((item, index) => {
					const isSelected = index === selectedCapacityIndex;
					const recipientCount = item.recipients.length;
					return (
						<Box key={item.capacity.id} marginLeft={2}>
							<Text color={isSelected ? 'green' : undefined} bold={isSelected}>
								{isSelected ? '▶ ' : '  '}
								{item.capacity.emoji} {item.capacity.name} ({item.capacity.quantity} {item.capacity.unit})
								<Text dimColor> - {recipientCount} potential recipient{recipientCount !== 1 ? 's' : ''}</Text>
							</Text>
						</Box>
					);
				})}
			</Box>

			{selectedCapacity && (
				<Box marginTop={2} flexDirection="column">
					<Text bold underline>
						Potential Recipients for "{selectedCapacity.capacity.name}":
					</Text>

					{selectedCapacity.recipients.length === 0 ? (
						<Box marginTop={1} marginLeft={2}>
							<Text color="yellow">No recipients found in the network yet.</Text>
						</Box>
					) : (
						<Box marginTop={1}>
							<Table
								headers={['Recipient', 'Need', 'Quantity', 'Location', 'Match']}
								rows={selectedCapacity.recipients.slice(0, 10).map(r => [
									r.pubKey.slice(0, 12) + '...',
									r.need.name,
									`${r.need.quantity} ${r.need.unit}`,
									r.need.location_type || 'any',
									`${r.compatibility}%`
								])}
							/>
						</Box>
					)}

					{selectedCapacity.recipients.length > 0 && (
						<Box marginTop={1} marginLeft={2}>
							<Text dimColor>
								💡 Showing top {Math.min(10, selectedCapacity.recipients.length)} matches
							</Text>
						</Box>
					)}
				</Box>
			)}

			<Box marginTop={2}>
				<Text dimColor>↑↓ Navigate | Enter to see details | Q/Esc Back</Text>
			</Box>
		</Box>
	);
};

