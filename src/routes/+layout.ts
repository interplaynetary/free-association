import { browser } from '$app/environment';
import { loadTranslations, defaultLocale, locale } from '$lib/translations';

// Disable SSR - this is a client-only app (Holster/Gun requires browser environment)
export const prerender = true;
export const trailingSlash = 'always';
export const ssr = false;
export const csr = true;

export const load = async ({ url }) => {
  const { pathname } = url;
  const initLocale = getInitialLocale();

  if (browser) {
    console.log('[LAYOUT] Loading translations for:', initLocale, 'route:', pathname);
    try {
      await loadTranslations(initLocale, pathname);
      console.log('[LAYOUT] Translations loaded successfully');
    } catch (err) {
      console.error('[LAYOUT] Error loading translations:', err);
      if (err instanceof Error) {
        console.error('[LAYOUT] Error stack:', err.stack);
      }
    }
  }

  return {
    locale: initLocale,
    route: pathname
  };
};

function getInitialLocale(): string {
  if (browser) {
    // Try to get the locale from localStorage
    const storedLocale = localStorage.getItem('lang');
    if (storedLocale) return storedLocale;

    // If no stored locale, try to get from browser language
    const browserLang = navigator.language.toLowerCase();
    const detected = browserLang.startsWith('pt') ? 'pt' :
      browserLang.startsWith('tr') ? 'tr' :
        defaultLocale;

    // Save the detected language to localStorage
    localStorage.setItem('lang', detected);
    return detected;
  }

  return defaultLocale;
}
