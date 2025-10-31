import React, { useState } from 'react';
import { Box, Text, useInput } from 'ink';
import { Table } from '../components/Table';
import * as storage from '../data/storage';
import type { NeedSlot, AvailabilitySlot, Commitment } from '../../schemas';

interface FindProvidersProps {
	myPubKey: string;
	myNeeds: NeedSlot[];
	allCommitments: Record<string, Commitment>;
	onBack: () => void;
	onSelectNeed: (needId: string) => void;
}

export const FindProviders: React.FC<FindProvidersProps> = ({
	myPubKey,
	myNeeds,
	allCommitments,
	onBack,
	onSelectNeed
}) => {
	const [selectedNeedIndex, setSelectedNeedIndex] = useState(0);

	useInput((input, key) => {
		const lower = input.toLowerCase();
		if (key.escape || lower === 'q') {
			onBack();
		} else if (key.upArrow) {
			setSelectedNeedIndex(Math.max(0, selectedNeedIndex - 1));
		} else if (key.downArrow) {
			setSelectedNeedIndex(Math.min(myNeeds.length - 1, selectedNeedIndex + 1));
		} else if (key.return && myNeeds.length > 0) {
			onSelectNeed(myNeeds[selectedNeedIndex].id);
		}
	});

	if (myNeeds.length === 0) {
		return (
			<Box flexDirection="column" padding={1}>
				<Text bold color="cyan">🔍 Find Providers for Your Needs</Text>
				<Box marginTop={2}>
					<Text color="yellow">⚠️  You don't have any needs yet</Text>
				</Box>
				<Box marginTop={1}>
					<Text dimColor>Add a need first, then come back to find providers.</Text>
				</Box>
				<Box marginTop={2}>
					<Text dimColor>Press Esc to go back</Text>
				</Box>
			</Box>
		);
	}

	// Find potential providers for each need
	const needsWithProviders = myNeeds.map(need => {
		const providers: Array<{
			pubKey: string;
			capacity: AvailabilitySlot;
			compatibility: number;
		}> = [];

		// Look through all other participants
		Object.entries(allCommitments).forEach(([pubKey, commitment]) => {
			if (pubKey === myPubKey) return; // Skip self

			// Check their capacities
			commitment.availability_slots?.forEach(capacity => {
				// Match by need type
				if (capacity.need_type_id === need.need_type_id) {
					// Calculate basic compatibility score (0-100)
					let compatibility = 50; // Base score for type match

					// Bonus for matching location type
					if (capacity.location_type === need.location_type) {
						compatibility += 20;
					}

					// Bonus for same unit
					if (capacity.unit === need.unit) {
						compatibility += 10;
					}

					// Bonus if they have enough quantity
					if (capacity.quantity >= need.quantity) {
						compatibility += 20;
					}

					providers.push({
						pubKey,
						capacity,
						compatibility: Math.min(100, compatibility)
					});
				}
			});
		});

		// Sort by compatibility
		providers.sort((a, b) => b.compatibility - a.compatibility);

		return {
			need,
			providers
		};
	});

	const selectedNeed = needsWithProviders[selectedNeedIndex];

	return (
		<Box flexDirection="column" padding={1}>
			<Text bold color="cyan">🔍 Find Providers for Your Needs</Text>

			<Box marginTop={1} flexDirection="column">
				<Text bold>Your Needs:</Text>
				{needsWithProviders.map((item, index) => {
					const isSelected = index === selectedNeedIndex;
					const providerCount = item.providers.length;
					return (
						<Box key={item.need.id} marginLeft={2}>
							<Text color={isSelected ? 'green' : undefined} bold={isSelected}>
								{isSelected ? '▶ ' : '  '}
								{item.need.emoji} {item.need.name} ({item.need.quantity} {item.need.unit})
								<Text dimColor> - {providerCount} potential provider{providerCount !== 1 ? 's' : ''}</Text>
							</Text>
						</Box>
					);
				})}
			</Box>

			{selectedNeed && (
				<Box marginTop={2} flexDirection="column">
					<Text bold underline>
						Potential Providers for "{selectedNeed.need.name}":
					</Text>

					{selectedNeed.providers.length === 0 ? (
						<Box marginTop={1} marginLeft={2}>
							<Text color="yellow">No providers found in the network yet.</Text>
						</Box>
					) : (
						<Box marginTop={1}>
							<Table
								headers={['Provider', 'Capacity', 'Quantity', 'Location', 'Match']}
								rows={selectedNeed.providers.slice(0, 10).map(p => [
									p.pubKey.slice(0, 12) + '...',
									p.capacity.name,
									`${p.capacity.quantity} ${p.capacity.unit}`,
									p.capacity.location_type || 'any',
									`${p.compatibility}%`
								])}
							/>
						</Box>
					)}

					{selectedNeed.providers.length > 0 && (
						<Box marginTop={1} marginLeft={2}>
							<Text dimColor>
								💡 Showing top {Math.min(10, selectedNeed.providers.length)} matches
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

