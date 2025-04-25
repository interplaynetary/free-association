import { browser } from '$app/environment';

export function load({ url }: { url: URL }) {
	return {
		// Navigation information
		navigation: [
			{ id: 'node', path: '/', label: 'Home' },
			{ id: 'inventory', path: '/inventory', label: 'Inventory' },
			{ id: 'login', path: '/login', label: 'Login' },
			{ id: 'charts', path: '/charts', label: 'Charts' }
		],

		// Current route information
		currentPath: url.pathname,
		isNode: url.pathname === '/',
		isInventory: url.pathname === '/inventory',
		isLogin: url.pathname === '/login',
		isCharts: url.pathname === '/charts'
	};
}

export const prerender = true;
