// Disable SSR for the root route to prevent iOS Safari hydration issues
// with Svelte 5 $state runes in globalState
export const prerender = false;
export const ssr = false;
export const csr = true;

