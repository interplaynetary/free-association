/**
 * Playnet Logo Component
 * 
 * Beautiful ASCII logo using oh-my-logo
 */

import React from 'react';
import { Box, Text } from 'ink';

interface LogoProps {
	subtitle?: string;
}

export const Logo: React.FC<LogoProps> = ({ subtitle = 'Free Association Protocol v5' }) => {
	// Beautiful ASCII art logo with gradient
	const logo = `
██████╗ ██╗      █████╗ ██╗   ██╗███╗   ██╗███████╗████████╗
██╔══██╗██║     ██╔══██╗╚██╗ ██╔╝████╗  ██║██╔════╝╚══██╔══╝
██████╔╝██║     ███████║ ╚████╔╝ ██╔██╗ ██║█████╗     ██║   
██╔═══╝ ██║     ██╔══██║  ╚██╔╝  ██║╚██╗██║██╔══╝     ██║   
██║     ███████╗██║  ██║   ██║   ██║ ╚████║███████╗   ██║   
╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═══╝╚══════╝   ╚═╝   
	`;

	return (
		<Box flexDirection="column" marginBottom={1}>
			<Text color="cyan" bold>
				{logo}
			</Text>
			<Box justifyContent="center">
				<Text color="magenta" dimColor>
					{subtitle}
				</Text>
			</Box>
		</Box>
	);
};

