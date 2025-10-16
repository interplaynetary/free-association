import i18n from 'sveltekit-i18n';
import { dev } from '$app/environment';
import lang from './lang.json';

export const defaultLocale = 'en';

/** @type {import('sveltekit-i18n').Config} */
export const config = {
  log: {
    level: (dev ? 'warn' : 'error') as 'warn' | 'error',
  },
  translations: {
    en: { lang },
    pt: { lang },
  },
  loaders: [
    // English - Global (loaded for all routes)
    {
      locale: 'en',
      key: 'common',
      routes: [/.*/],
      loader: async () => (await import('./en/common.json')).default,
    },
    {
      locale: 'en',
      key: 'auth',
      routes: [/.*/],
      loader: async () => (await import('./en/auth.json')).default,
    },
    {
      locale: 'en',
      key: 'toolbar',
      routes: [/.*/],
      loader: async () => (await import('./en/toolbar.json')).default,
    },
    {
      locale: 'en',
      key: 'notifications',
      routes: [/.*/],
      loader: async () => (await import('./en/notifications.json')).default,
    },
    {
      locale: 'en',
      key: 'errors',
      routes: [/.*/],
      loader: async () => (await import('./en/errors.json')).default,
    },
    // English - Route-specific
    {
      locale: 'en',
      key: 'home',
      loader: async () => (await import('./en/home.json')).default,
    },
    {
      locale: 'en',
      key: 'collective',
      loader: async () => (await import('./en/collective.json')).default,
    },
    {
      locale: 'en',
      key: 'inventory',
      loader: async () => (await import('./en/inventory.json')).default,
    },
    {
      locale: 'en',
      key: 'map',
      loader: async () => (await import('./en/map.json')).default,
    },
    {
      locale: 'en',
      key: 'tree',
      loader: async () => (await import('./en/tree.json')).default,
    },
    {
      locale: 'en',
      key: 'contacts',
      loader: async () => (await import('./en/contacts.json')).default,
    },
    // Portuguese - Global (loaded for all routes)
    {
      locale: 'pt',
      key: 'common',
      routes: [/.*/],
      loader: async () => (await import('./pt/common.json')).default,
    },
    {
      locale: 'pt',
      key: 'auth',
      routes: [/.*/],
      loader: async () => (await import('./pt/auth.json')).default,
    },
    {
      locale: 'pt',
      key: 'toolbar',
      routes: [/.*/],
      loader: async () => (await import('./pt/toolbar.json')).default,
    },
    {
      locale: 'pt',
      key: 'notifications',
      routes: [/.*/],
      loader: async () => (await import('./pt/notifications.json')).default,
    },
    {
      locale: 'pt',
      key: 'errors',
      routes: [/.*/],
      loader: async () => (await import('./pt/errors.json')).default,
    },
    // Portuguese - Route-specific
    {
      locale: 'pt',
      key: 'home',
      loader: async () => (await import('./pt/home.json')).default,
    },
    {
      locale: 'pt',
      key: 'collective',
      loader: async () => (await import('./pt/collective.json')).default,
    },
    {
      locale: 'pt',
      key: 'inventory',
      loader: async () => (await import('./pt/inventory.json')).default,
    },
    {
      locale: 'pt',
      key: 'map',
      loader: async () => (await import('./pt/map.json')).default,
    },
    {
      locale: 'pt',
      key: 'tree',
      loader: async () => (await import('./pt/tree.json')).default,
    },
    {
      locale: 'pt',
      key: 'contacts',
      loader: async () => (await import('./pt/contacts.json')).default,
    },
  ],
};

export const { t, loading, locales, locale, translations, loadTranslations, addTranslations, setLocale, setRoute } = new i18n(config);

loading.subscribe(($loading) => $loading && console.log('Loading translations...'));

