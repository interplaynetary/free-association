// Explicitly disable SSR for the main page to ensure consistency
// Even though layout already has ssr=false, being explicit helps with iOS Safari
export const ssr = false;
export const prerender = true;

