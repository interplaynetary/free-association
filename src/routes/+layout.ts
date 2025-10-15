import { browser } from '$app/environment';
import { loadTranslations, defaultLocale, locale } from '$lib/translations';

// Enable prerendering for static site generation
export const prerender = true;
export const ssr = false;

// Enable client-side routing
export const csr = true;

/** @type {import('./$types').LayoutLoad} */
export const load = async ({ url }) => {
  const { pathname } = url;

  if (browser) {
    // Try to get the locale from localStorage
    let storedLocale = localStorage.getItem('lang') || '';

    // If no stored locale, try to get from browser language
    if (!storedLocale) {
      const browserLang = navigator.language.toLowerCase();
      storedLocale = browserLang.startsWith('pt') ? 'pt' :
                     browserLang.startsWith('tr') ? 'tr' : 
                     defaultLocale;
    }

    await loadTranslations(storedLocale, pathname);
  }

  return {
    locale: browser ? locale : defaultLocale,
    route: pathname
  };
};
