import { browser } from '$app/environment';
import { loadTranslations, defaultLocale, locale } from '$lib/translations';

// Disable SSR - this is a client-only app (Holster/Gun requires browser environment)
export const prerender = false;
export const ssr = false;
export const csr = true;

/** @type {import('./$types').LayoutLoad} */
export const load = async ({ url }) => {
  if (browser) {
    // Try to get the locale from localStorage
    let storedLocale = localStorage.getItem('lang') || '';

    // If no stored locale, try to get from browser language
    if (!storedLocale) {
      const browserLang = navigator.language.toLowerCase();
      storedLocale = browserLang.startsWith('pt') ? 'pt' :
                     browserLang.startsWith('tr') ? 'tr' : 
                     defaultLocale;
      // Save the detected language to localStorage
      localStorage.setItem('lang', storedLocale);
    }

    // Load translations without pathname to avoid TDZ error on iOS Safari
    // The route will be set automatically by sveltekit-i18n after routing context is ready
    await loadTranslations(storedLocale);
  }

  return {
    locale: browser ? locale : defaultLocale,
  };
};
