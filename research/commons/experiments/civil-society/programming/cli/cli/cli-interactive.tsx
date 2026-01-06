#!/usr/bin/env node
/**
 * Interactive Ink-based CLI for Playnet
 * 
 * Usage: playnet interactive
 */

import React from 'react';
import { render } from 'ink';
import { CLIApp } from './CLIApp';

// Render the app
const { waitUntilExit } = render(<CLIApp />);

// Wait for exit
waitUntilExit().then(() => {
	process.exit(0);
});

