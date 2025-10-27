import type { Action } from 'svelte/action';

export const emojiPicker: Action<HTMLElement, { isVisible: boolean, onClick: (emoji: string) => void,  onVisibilityChange: (isVisible: boolean) => void}> = (node, { isVisible, onClick, onVisibilityChange }) => {
	let picker: HTMLElement | null = null;

	let clickHandler = onClick;
	let handleVisibilityChange = onVisibilityChange;

	try {
		picker = document.createElement('emoji-picker');
		picker.style.position = 'absolute';
		picker.style.zIndex = '1000';
		picker.style.boxShadow = '0 10px 25px rgba(0, 0, 0, 0.2)';
		picker.style.border = '1px solid #e5e7eb';
		picker.style.borderRadius = '8px';
		picker.style.width = '320px';
		picker.style.height = '400px';

		// Listen for emoji selection
		picker.addEventListener('emoji-click', (event: any) => {
			clickHandler(event.detail.unicode)
			handleVisibilityChange(false);
		});

		if (isVisible) {
			node.appendChild(picker);
		}
	} catch (error) {
		console.warn('Failed to create emoji picker:', error);
	}

	function handleDocumentClick(event: MouseEvent) {
		if (!node.contains(event.target as Node)) {
			handleVisibilityChange(false);
		}
	}
	document.addEventListener('click', handleDocumentClick);

	return {
		update({ isVisible, onClick, onVisibilityChange }) {
			clickHandler = onClick
			handleVisibilityChange = onVisibilityChange;
			if (!picker) return;
			if (isVisible) {
				node.appendChild(picker);
			} else {
				node.removeChild(picker);
			}
		},
		destroy() {
			document.removeEventListener('click', handleDocumentClick);
		}
	}
};
