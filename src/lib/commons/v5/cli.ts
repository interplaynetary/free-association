#!/usr/bin/env node
/**
 * Playnet CLI - Legacy Text-Based Interface
 * 
 * Simple command-line interface for running the pure protocol.
 * For the new beautiful Ink-based UI, use: bun run src/lib/commons/v5/cli/index.tsx
 * 
 * Usage:
 *   bun run src/lib/commons/v5/cli.ts example
 *   bun run src/lib/commons/v5/cli.ts simulate --iterations=10
 *   bun run src/lib/commons/v5/cli.ts interactive  # NEW: Launch Ink UI
 */

import { runAliceIteration, runMultipleIterations, runStepByStepIteration } from './examples/terminal-example';
import { execSync } from 'child_process';

function printHelp() {
	console.log(`
╔════════════════════════════════════════════════════════╗
║          Playnet - Free Association Protocol          ║
║                      CLI Tools                          ║
╚════════════════════════════════════════════════════════╝

Usage:
  cli.ts <command> [options]

Commands:
  interactive          Launch beautiful interactive UI (NEW!)
  step                 Step through iterations one at a time (Press Enter)
  example              Run a single iteration example
  simulate [--iterations=N]  Run multiple iterations (default: 5)
  help                 Show this help message

Examples:
  bun run src/lib/commons/v5/cli.ts interactive
  bun run src/lib/commons/v5/cli.ts step
  bun run src/lib/commons/v5/cli.ts example
  bun run src/lib/commons/v5/cli.ts simulate --iterations=10

Tip: Use 'step' to watch convergence happen iteration by iteration!
	`);
}

async function main() {
	const args = process.argv.slice(2);
	
	if (args.length === 0) {
		printHelp();
		process.exit(0);
	}
	
	const command = args[0];
	
	switch (command) {
	case 'interactive':
	case 'ui':
		console.log('🚀 Launching Playnet Interactive UI...\n');
		try {
			execSync('bun run src/lib/commons/v5/cli/cli-interactive.tsx', { stdio: 'inherit' });
		} catch (error) {
			console.error('Error launching interactive UI:', error);
			process.exit(1);
		}
		break;
			
		case 'step':
		case 'iterate':
			await runStepByStepIteration();
			break;
			
		case 'example':
			runAliceIteration();
			break;
			
		case 'simulate': {
			const iterationsArg = args.find(arg => arg.startsWith('--iterations='));
			const iterations = iterationsArg 
				? parseInt(iterationsArg.split('=')[1], 10)
				: 5;
			
			if (isNaN(iterations) || iterations < 1) {
				console.error('Error: --iterations must be a positive number');
				process.exit(1);
			}
			
			runMultipleIterations(iterations);
			break;
		}
		
		case 'help':
		case '--help':
		case '-h':
			printHelp();
			break;
			
		default:
			console.error(`Error: Unknown command '${command}'`);
			printHelp();
			process.exit(1);
	}
}

main();

