/**
 * @module useModalState
 * Composable for managing modal states elegantly
 * Extracts modal management from DeciderWidget
 */

export type ModalType = 'challenge' | 'comment' | 'support' | 'config' | 'expanded' | null;

export interface ModalState {
	type: ModalType;
	data?: any;
}

export function useModalState() {
	let current = $state<ModalState>({ type: null });
	
	function open(type: ModalType, data?: any) {
		current = { type, data };
	}
	
	function close() {
		current = { type: null, data: undefined };
	}
	
	function isOpen(type: ModalType): boolean {
		return current.type === type;
	}
	
	// Specific modal openers for better DX
	function openChallenge(proposalPub: string) {
		open('challenge', { proposalPub });
	}
	
	function openComment(proposalPub: string) {
		open('comment', { proposalPub });
	}
	
	function openSupport(proposalPub: string) {
		open('support', { proposalPub });
	}
	
	function openConfig() {
		open('config');
	}
	
	function openExpanded(proposalPub: string) {
		open('expanded', { proposalPub });
	}
	
	return {
		get current() { return current; },
		get isAnyOpen() { return current.type !== null; },
		open,
		close,
		isOpen,
		openChallenge,
		openComment,
		openSupport,
		openConfig,
		openExpanded
	};
}

/**
 * Enhanced modal controller with loading/submitting states
 */
export function useModalController() {
	const modal = useModalState();
	let isSubmitting = $state(false);
	
	async function submit(handler: () => Promise<void>) {
		isSubmitting = true;
		try {
			await handler();
			modal.close();
		} catch (e) {
			// Error handling should be done in the handler
			console.error('Modal submit error:', e);
		} finally {
			isSubmitting = false;
		}
	}
	
	return {
		modal,
		get isSubmitting() { return isSubmitting; },
		submit,
		close: () => modal.close()
	};
}




