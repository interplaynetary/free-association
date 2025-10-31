// Debounce helper for subscription updates
export function debounce<T extends (...args: any[]) => void>(func: T, delay: number): T {
	let timeoutId: any;
	return ((...args: any[]) => {
		clearTimeout(timeoutId);
		timeoutId = setTimeout(() => func(...args), delay);
	}) as T;
}
