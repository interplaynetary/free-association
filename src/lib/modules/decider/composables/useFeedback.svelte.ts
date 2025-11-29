/**
 * @module useFeedback
 * Composable for managing user feedback (toasts, messages)
 * Extracts feedback logic from DeciderWidget
 */

export interface FeedbackMessage {
	type: 'success' | 'error' | 'info' | 'warning';
	text: string;
}

export interface FeedbackOptions {
	duration?: number;
	maxQueue?: number;
}

export function useFeedback(options: FeedbackOptions = {}) {
	const { duration = 3000, maxQueue = 3 } = options;
	
	let messages = $state<FeedbackMessage[]>([]);
	let timeouts: Map<number, ReturnType<typeof setTimeout>> = new Map();
	let messageId = 0;
	
	function show(type: FeedbackMessage['type'], text: string) {
		const id = messageId++;
		const message: FeedbackMessage = { type, text };
		
		// Add to queue (limit size)
		if (messages.length >= maxQueue) {
			const firstId = messages.length - maxQueue;
			clearTimeout(timeouts.get(firstId));
			timeouts.delete(firstId);
			messages.shift();
		}
		
		messages.push(message);
		
		// Auto-dismiss after duration
		const timeout = setTimeout(() => {
			dismiss(id);
		}, duration);
		
		timeouts.set(id, timeout);
	}
	
	function dismiss(id: number) {
		const timeout = timeouts.get(id);
		if (timeout) {
			clearTimeout(timeout);
			timeouts.delete(id);
		}
		
		const index = id - (messageId - messages.length);
		if (index >= 0 && index < messages.length) {
			messages.splice(index, 1);
		}
	}
	
	function clear() {
		timeouts.forEach(timeout => clearTimeout(timeout));
		timeouts.clear();
		messages = [];
	}
	
	function success(text: string) {
		show('success', text);
	}
	
	function error(text: string) {
		show('error', text);
	}
	
	function info(text: string) {
		show('info', text);
	}
	
	function warning(text: string) {
		show('warning', text);
	}
	
	return {
		get messages() { return messages; },
		show,
		success,
		error,
		info,
		warning,
		dismiss,
		clear
	};
}

/**
 * Validation helper for feedback integration
 */
export function validateContent(
	content: string,
	showError: (text: string) => void,
	maxLength = 5000
): boolean {
	if (!content || content.trim().length === 0) {
		showError('Content cannot be empty');
		return false;
	}
	if (content.length > maxLength) {
		showError(`Content exceeds maximum length of ${maxLength} characters`);
		return false;
	}
	return true;
}



