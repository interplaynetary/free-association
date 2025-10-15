# Internationalization (i18n) Guide

This guide explains how to use the multilingual features in the Free Association project.

## Overview

The project uses `sveltekit-i18n` for internationalization support. Currently, three languages are configured:
- **English (en)** - Default language
- **Türkçe (tr)** - Turkish
- **Português (pt)** - Portuguese (Brazilian & European)

## Quick Start

### Using Translations in Components

```svelte
<script lang="ts">
  import { t } from '$lib/translations';
</script>

<h1>{$t('home.title')}</h1>
<p>{$t('home.subtitle')}</p>
```

### Using Translations with Variables

```svelte
<script lang="ts">
  import { t } from '$lib/translations';
  
  const userName = 'John';
</script>

<p>{$t('welcome.greeting', { name: userName })}</p>
```

### Using HTML in Translations

For translations that contain HTML:

```svelte
<p>{@html $t('home.text', { link: 'https://example.com' })}</p>
```

## Adding New Languages

1. Add the language code and name to `src/lib/translations/lang.json`:

```json
{
  "en": "English",
  "tr": "Türkçe",
  "pt": "Português",
  "de": "Deutsch"
}
```

2. Create a new folder for the language: `src/lib/translations/de/`

3. Add translation files (e.g., `common.json`, `home.json`, `collective.json`)

4. Register the loaders in `src/lib/translations/index.ts`:

```typescript
{
  locale: 'de',
  key: 'common',
  loader: async () => (await import('./de/common.json')).default,
},
{
  locale: 'de',
  key: 'home',
  routes: ['/'],
  loader: async () => (await import('./de/home.json')).default,
},
```

5. Update the browser language detection in `src/routes/+layout.ts` if needed:

```typescript
storedLocale = browserLang.startsWith('pt') ? 'pt' :
               browserLang.startsWith('de') ? 'de' : 
               browserLang.startsWith('tr') ? 'tr' : 
               defaultLocale;
```

## Translation File Structure

Translation files are organized by:
- **Language**: Each language has its own folder (e.g., `en/`, `tr/`)
- **Module**: Each module/feature has its own JSON file (e.g., `home.json`, `collective.json`)

### Example: `src/lib/translations/en/common.json`

```json
{
  "language": "Language",
  "loading": "Loading...",
  "save": "Save",
  "cancel": "Cancel"
}
```

### Example: `src/lib/translations/en/home.json`

```json
{
  "title": "Welcome to Free Association",
  "subtitle": "A decentralized platform for collective organization"
}
```

## Route-Specific Translations

**Note:** Route-specific loading is disabled for production reliability. All translations are loaded upfront.

Previously, translations could be loaded based on routes, but this doesn't work reliably with static site generation (`prerender: true`) and client-side rendering (`ssr: false`):

```typescript
// ❌ Don't use routes in production builds
{
  locale: 'en',
  key: 'collective',
  routes: ['/collective'],  // Can cause issues in production
  loader: async () => (await import('./en/collective.json')).default,
}

// ✅ Load all translations upfront
{
  locale: 'en',
  key: 'collective',
  loader: async () => (await import('./en/collective.json')).default,
}
```

## Language Switcher

The `LanguageSwitcher` component is already added to the Header. Users can change languages using the dropdown in the top right corner.

The selected language is stored in `localStorage` and persists across sessions.

## Checking Current Locale

```svelte
<script lang="ts">
  import { locale } from '$lib/translations';
</script>

<p>Current language: {$locale}</p>
```

## Available Translation Stores

- `t` - Translation function (use with `$t('key')`)
- `locale` - Current locale code (use with `$locale`)
- `locales` - Available locales (use with `$locales`)
- `loading` - Loading state (use with `$loading`)
- `setLocale(locale)` - Function to change locale
- `loadTranslations(locale, route)` - Function to load translations

## Best Practices

1. **Keep translation keys descriptive**: Use dot notation for hierarchy (e.g., `home.title`, `home.subtitle`)
2. **Organize by feature**: Create separate JSON files for different features/pages
3. **Use common.json**: Put shared translations (buttons, labels) in `common.json`
4. **Test all languages**: Make sure to test the UI with all supported languages
5. **Keep translations in sync**: When adding new keys, add them to all language files

## Development Tips

- Translations are loaded asynchronously based on routes
- The `loading` store indicates when translations are being loaded
- Missing translation keys will be displayed as-is in development mode
- Use the browser's language setting as the default if no preference is stored

## Reference

Based on the guide: [Creating a Multi-Language Website with SvelteKit](https://raktive.com/blog/creating-a-multi-language-website-with-sveltekit)

