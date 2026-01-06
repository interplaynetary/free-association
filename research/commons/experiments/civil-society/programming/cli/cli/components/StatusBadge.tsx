import React from 'react';
import { Text } from 'ink';

interface StatusBadgeProps {
	percentage: number;
	showPercentage?: boolean;
}

export const StatusBadge: React.FC<StatusBadgeProps> = ({
	percentage,
	showPercentage = true
}) => {
	let emoji: string;
	let color: 'green' | 'yellow' | 'red';
	
	if (percentage >= 90) {
		emoji = '🟢';
		color = 'green';
	} else if (percentage >= 50) {
		emoji = '🟡';
		color = 'yellow';
	} else {
		emoji = '🔴';
		color = 'red';
	}
	
	return (
		<Text>
			{emoji} {showPercentage && percentage !== undefined && (
				<Text color={color}>{percentage.toFixed(0)}%</Text>
			)}
		</Text>
	);
};

export const MatchIndicator: React.FC<{
	time: boolean;
	location: boolean;
	recognition?: number;
}> = ({ time, location, recognition }) => {
	return (
		<Text>
			⏰ {time ? <Text color="green">✓</Text> : <Text color="red">✗</Text>}
			{' '}
			📍 {location ? <Text color="green">✓</Text> : <Text color="red">✗</Text>}
			{recognition !== undefined && (
				<>
					{' '}
					🤝 <Text color="cyan">{(recognition * 100).toFixed(0)}%</Text>
				</>
			)}
		</Text>
	);
};

