import type { Action } from 'svelte/action';

export const outsideClick: Action<HTMLElement, () => void> = (node, callback) => {
	let callbackRef = callback;

	const handleClick = (event: MouseEvent) => {
		if (!node.contains(event.target as Node)) {
			callbackRef();
		}
	};

	document.addEventListener('click', handleClick);

	return {
		update(callback) {
			callbackRef = callback;
		},
		destroy() {
			document.removeEventListener('click', handleClick);
		}
	};
};