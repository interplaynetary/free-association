import type { Action } from 'svelte/action';

export const emojiPicker: Action<HTMLElement, { onClick: (emoji: string) => void }> = (
	node,
	{ onClick }
) => {
	import('emoji-picker-element').catch((error) =>
		console.warn('Failed to load emoji picker:', error)
	);

	let clickHandler = onClick;
	let picker: HTMLElement | null = null;

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
			clickHandler(event.detail.unicode);
		});

		node.appendChild(picker);
	} catch (error) {
		console.warn('Failed to create emoji picker:', error);
	}

	return {
		update({ onClick }) {
			clickHandler = onClick;
		}
	};
};
