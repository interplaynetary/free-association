/**
 * @module time
 * Elegant time utilities for the Decider system
 */

/**
 * Format milliseconds into human-readable time string
 * 
 * @example
 * formatTime(125000) // "2m 5s"
 * formatTime(3665000) // "1h 1m"
 * formatTime(500) // "0s"
 */
export function formatTime(ms: number): string {
	const seconds = Math.floor(ms / 1000);
	const minutes = Math.floor(seconds / 60);
	const hours = Math.floor(minutes / 60);
	
	if (hours > 0) {
		const remainingMinutes = minutes % 60;
		return remainingMinutes > 0 ? `${hours}h ${remainingMinutes}m` : `${hours}h`;
	}
	
	if (minutes > 0) {
		const remainingSeconds = seconds % 60;
		return remainingSeconds > 0 ? `${minutes}m ${remainingSeconds}s` : `${minutes}m`;
	}
	
	return `${seconds}s`;
}

/**
 * Calculate progress percentage (0-100)
 */
export function calculateProgress(elapsed: number, total: number): number {
	if (total <= 0) return 0;
	return Math.min(100, (elapsed / total) * 100);
}

/**
 * Calculate time remaining
 */
export function calculateRemaining(startTime: number, duration: number): number {
	const elapsed = Date.now() - startTime;
	return Math.max(0, duration - elapsed);
}

/**
 * Check if time is in urgent state (< 10% remaining)
 */
export function isUrgent(remaining: number, total: number): boolean {
	return remaining > 0 && remaining < total * 0.1;
}

/**
 * Check if time has expired
 */
export function isExpired(remaining: number): boolean {
	return remaining === 0;
}

/**
 * Parse time string to milliseconds
 * 
 * @example
 * parseTime('30s') // 30000
 * parseTime('2m') // 120000
 * parseTime('1h') // 3600000
 * parseTime('1h 30m') // 5400000
 */
export function parseTime(timeStr: string): number {
	const parts = timeStr.trim().toLowerCase().match(/(\d+)\s*([hms])/g);
	if (!parts) return 0;
	
	let total = 0;
	for (const part of parts) {
		const match = part.match(/(\d+)\s*([hms])/);
		if (!match) continue;
		
		const value = parseInt(match[1]);
		const unit = match[2];
		
		switch (unit) {
			case 'h': total += value * 3600000; break;
			case 'm': total += value * 60000; break;
			case 's': total += value * 1000; break;
		}
	}
	
	return total;
}

/**
 * Time state type for UI components
 */
export interface TimeState {
	remaining: number;
	progress: number;
	isUrgent: boolean;
	isExpired: boolean;
	formatted: string;
}

/**
 * Calculate complete time state in one call
 */
export function getTimeState(startTime: number, duration: number): TimeState {
	const remaining = calculateRemaining(startTime, duration);
	const elapsed = Date.now() - startTime;
	const progress = calculateProgress(elapsed, duration);
	
	return {
		remaining,
		progress,
		isUrgent: isUrgent(remaining, duration),
		isExpired: isExpired(remaining),
		formatted: formatTime(remaining)
	};
}

