export type ToastType = 'success' | 'error' | 'warning' | 'info';

export interface Toast {
	id: string;
	message: string;
	type: ToastType;
}

class ToastStore {
	toasts = $state<Toast[]>([]);
	
	private nextId = 0;

	show(message: string, type: ToastType = 'info', duration = 3000) {
		const id = `toast-${this.nextId++}`;
		const toast: Toast = { id, message, type };
		
		this.toasts.push(toast);
		
		// Auto-remove after duration
		setTimeout(() => {
			this.remove(id);
		}, duration);
		
		return id;
	}

	success(message: string, duration = 3000) {
		return this.show(message, 'success', duration);
	}

	error(message: string, duration = 4000) {
		return this.show(message, 'error', duration);
	}

	warning(message: string, duration = 3500) {
		return this.show(message, 'warning', duration);
	}

	info(message: string, duration = 3000) {
		return this.show(message, 'info', duration);
	}

	remove(id: string) {
		this.toasts = this.toasts.filter(t => t.id !== id);
	}

	clear() {
		this.toasts = [];
	}
}

export const toastStore = new ToastStore();

// Export a compatible API for the old svelte-french-toast
export const toast = {
	success: (message: string) => toastStore.success(message),
	error: (message: string) => toastStore.error(message),
	warning: (message: string) => toastStore.warning(message),
	info: (message: string) => toastStore.info(message),
	custom: (message: string) => toastStore.info(message),
};

