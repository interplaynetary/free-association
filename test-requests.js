/**
 * Test file to demonstrate the capacity request system
 * Run with: node test-requests.js
 */

// Import the demonstration function
import { demonstrateCapacityRequests } from './src/lib/protocol.js';

// Run the demonstration
try {
	demonstrateCapacityRequests();
} catch (error) {
	console.error('Error running demonstration:', error);
}
